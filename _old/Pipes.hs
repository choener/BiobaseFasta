
module Biobase.Fasta.Pipes where

import Control.Lens
import Control.Monad (join, unless, forM_)
import Pipes
import Data.ByteString (ByteString)
import Data.ByteString as BS
import Data.ByteString.Char8 as BS8
import Pipes.ByteString as PBS
import Pipes.Group as PG
import Data.Monoid
import Pipes as P
import Data.Semigroup
import qualified Data.List as L

import qualified Pipes.Prelude as PP
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq)

import Biobase.Fasta.Types



-- | Improper lens that splits off the first @FASTA@ entry. Newlines are still intact.

fastaEntry ∷ Monad m ⇒ Lens' (Producer ByteString m x) (Producer ByteString m (Producer ByteString m x))
fastaEntry k p0 = fmap join (k (go 1 p0))
  where
    -- If @hdrCount == 1@, then we have a first line where we assume to have a
    -- @>@ symbol and *don't* want to split before that symbol.
    go hdrCount p = do
      x ← lift (next p)
      case x of
        Left r → return (return r)
        Right (bs, p')
          | BS.null bs → go hdrCount p'
          | h `BS8.elem` hdrChar →
              case BS8.findIndex (`BS8.elem` hdrChar) (BS8.drop hdrCount bs)
                   -- no header char found, yield characters and continue on
              of   Nothing  → yield bs >> go 0 p'
                   -- already have seen header char, only yield 
                   (Just z) → let (this,that) = BS.splitAt (z+hdrCount) bs
                              in  yield this >> return (yield that >> p')
          where h = BS8.head bs

{-# Inlinable fastaEntry #-}

hdrChar = BS8.pack ">;"



-- ** Test stuff

test1 :: Monad m ⇒ Producer ByteString m (Producer ByteString m ())
test1 = view fastaEntry (fromLazy $ BSL8.pack ">a\naaa\n>b\nbbb")

test2 = PP.toList (void test1)



-- TODO list for reading fasta.
--
-- Skip lines until first line starting with ">" discovered. Note (in the info
-- structure to be returned) the number of skipped header lines.
--
-- Each fasta entry should start with ">". This is the title line.
--
-- Following lines should have all '\n' stripped. And concatenated to become
-- the fasta entry. Remove internal '\r' parts.

-- Keep track of some FASTA information.

data TrackedFasta
  -- | A new header has shown up.
  = TrackedHeader
    { _trackedLine    ∷ !Int
    -- ^ which line are we in
    , _trackedHeader  ∷ !ByteString
    -- ^ keep the header
    }
  -- | Fasta entries.
  | TrackedFasta
    { _trackedLine      ∷ !Int
    -- ^ which line are we in
    , _trackedPosition  ∷ !Int
    -- ^ position of first character in this FASTA entry.
    -- TODO use typed things that shows if we are 1- or 0-based.
    , _trackedData      ∷ !ByteString
    -- ^ the payload line
    , _trackedHeader    ∷ !ByteString
    -- ^ keep the header
    }
makeLenses ''TrackedFasta

data TF = TF
  { _tfLine ∷ !Int
  , _tfPos  ∷ !Int
  , _tfHdr  ∷ !ByteString
  }
makeLenses ''TF

-- | Transforms bytestrings into tracked fasta entries. This is a
-- pre-transform: header entries can be split over multiple @TrackedHeader@
-- constructors and @TrackedFasta@ constructors hold different line lengths and
-- their corresponding header is not yet set.

tracked ∷ Monad m ⇒ Producer ByteString (Producer TrackedFasta m) x → Producer TrackedFasta m x
tracked p = next p >>= goPrepare (TF 1 1 BS.empty)
  where
    -- Remove all lines before the first line starting with @'>'@.
    goPrepare tf = \case
      Left l → return l
      Right (r',p') → let r = lineprep r' in case BS8.uncons r of
        -- not a single character?
        Nothing    → next p' >>= goPrepare tf
        -- start of a FASTA header
        Just (h,_) | h=='>'
                   → goHdr tf $ Right (r,p')
        -- remove first line and check the beginning of the next line
                   | Just k ← BS8.findIndex (=='\n') r
                   → goPrepare (over tfLine (+1) tf) $ Right (BS8.drop k r, p')
        -- we are somewhere in the first line of crap and need to pull more
        -- stuff
                   | otherwise
                   → next p' >>= goPrepare tf
    -- Start emitting @TrackedHeader@. Does not do an additional check!
    goHdr tf = \case
      Left l → return l
      Right (r',p') → let r = lineprep r' in case BS8.findIndex (=='\n') r of
        -- header continues into the next fragment.
        Nothing → do yield $ TrackedHeader (tf^.tfLine) r
                     next p' >>= goHdr tf
        -- one header line, goData has to do the check on what the next line
        -- is.
        Just k  → do yield $ TrackedHeader (tf^.tfLine) (BS8.take k r)
                     -- goData should be at the first character of the next
                     -- line now.
                     goData (over tfLine (+1) tf) $ Right (BS8.drop (k+1) r, p')
    -- Start emitting @TrackedFasta@, but check each line for being a header.
    -- This will emit @TrackedFasta@ of (at most) line length elements, but
    -- sometimes (whenever @next@ is not at line boundaries) yield the same
    -- line with different positions.
    --
    -- TODO need to remove all @\n@ and @\r@.
    goData tf = \case
      Left l       → return l
      Right (r',p') → let r = lineprep r' in case BS8.uncons r of
        -- need to get more data first.
        Nothing → next p' >>= goData tf
        Just (h,_) | h=='>'
                   -- this is a header that needs to be handled.
                   → goHdr tf $ Right (r,p')
                   -- we have at least two lines, emit the first and handle the
                   -- remaining bytes next.
                   -- the fasta header will be set by a combining function later.
                   | Just k ← BS8.findIndex (=='\n') r
                   → do yield $ TrackedFasta (tf^.tfLine) (tf^.tfPos) (BS8.take k r) BS.empty
                        goData (set tfPos 1 $ over tfLine (+1) tf) $ Right (BS8.drop (k+1) r, p')
                   -- we have only the beginning of a line.
                   | otherwise
                   → do yield $ TrackedFasta (tf^.tfLine) (tf^.tfPos) r BS.empty
                        next p' >>= goData (over tfPos (+ BS.length r) tf)
    lineprep = BS8.filter (/='\r')

data RetrackOptions = RetrackOptions
  { _maxHeaderLength  ∷ Maybe Int
  -- ^ if @Nothing@, any size is fine, just collapse, if @Just@, then cut off
  -- after these many bytes.
  , _windowLength     ∷ Maybe Int
  -- ^ if @Nothing@ then do not re-arrange, if @Just@ then set into windows of
  -- this size.
  }
makeLenses ''RetrackOptions

-- | This function collapses header objects, sets the header in each window,
-- and sets window sizes if requested.
--
-- This traverses the collected elements twice.

--retracked ∷ Monad m ⇒ RetrackOptions → Producer TrackedFasta (Producer TrackedFasta m) x → Producer TrackedFasta m x
retracked o p = next p >>= goHdr Seq.empty
  where
    goHdr (hs ∷ Seq TrackedFasta) = \case
      Left l → do unless (Seq.null hs) $ yield $ buildHeader hs
                  return l
      Right (r,p')
        | TrackedHeader{} ← r → next p' >>= goHdr (hs Seq.|> clampHdr hs r)
        | otherwise → do
            let hdr = buildHeader hs
            yield hdr
            goData hdr Seq.empty $ Right (r,p')
    goData (hdr ∷ TrackedFasta) (ds ∷ Seq TrackedFasta) = \case
      Left l → do case Seq.viewr ds of
                    Seq.EmptyR → return ()
                    (is Seq.:> i) → buildData True hdr is i >> return l
      Right (r,p')
        | TrackedHeader{} ← r → goHdr Seq.empty $ Right (r,p')
        | otherwise → do ds' ← buildData False hdr ds r
                         next p' >>= goData hdr ds'
    -- clamp all headers to maximal length.
    -- TODO this is @O(n)@ time for each call.
    clampHdr ∷ Seq TrackedFasta → TrackedFasta → TrackedFasta
    clampHdr hs h = let Sum t = hs^.traverse.trackedHeader.to BS8.length._Unwrapped
                    in  maybe h (\l →  over trackedHeader (BS8.take (t-l)) h) $ o^.maxHeaderLength
    -- builds up a header entry
    buildHeader ∷ Seq TrackedFasta → TrackedFasta
    buildHeader hs = let Min t = hs^.traverse.trackedLine._Unwrapped
                     in  TrackedHeader t (hs^.traverse.trackedHeader)
    -- builds data and attaches line and position information where necessary.
    buildData flush hdr ds d
      -- do nothing with the payload
      | Nothing ← o^.windowLength
      = if Seq.null ds then yield (set trackedHeader (hdr^.trackedHeader) d)
                            >> return Seq.empty
                       else error "buildData: invariant not valid"
      -- flush last element.
      | flush = do let y = ds Seq.|> d
                       (h Seq.:< _) = Seq.viewl y
                   yield $ TrackedFasta
                     { _trackedLine     = h ^. trackedLine
                     , _trackedPosition = _trackedPosition h
                     , _trackedData     = y^.traverse.trackedData
                     , _trackedHeader   = hdr^.trackedHeader
                     }
                   return Seq.empty
      | Just k  ← o^.windowLength
      -- TODO what if @k@ is smaller than the line length, this does not work like this...
      = do let es = ds Seq.|> d
               Sum l = es^.traverse.trackedData.to BS8.length._Unwrapped
               h Seq.:< _ = Seq.viewl es
           if | k > l → return es
              | k > undefined → undefined
              | otherwise → do
                 let bs = es^.traverse.trackedData
                     (this,that) = BS8.splitAt k bs
                     y = TrackedFasta
                          { _trackedLine     = h^.trackedLine
                          , _trackedPosition = _trackedPosition h
                          , _trackedData     = this
                          , _trackedHeader   = hdr^.trackedHeader
                          }
                     r = TrackedFasta
                          { _trackedLine     = d^.trackedLine
                          , _trackedPosition = 1 + BS8.length (d^.trackedData) - BS8.length that
                          , _trackedData     = that
                          , _trackedHeader   = hdr^.trackedHeader
                          }
                 yield y
                 return (Seq.singleton r)

ups = PP.toList . goncats . go . view PG.groups $ P.each "122333455"
  where
    goncats f = do
      lift (runFreeT f) >>= \case
        Pure r → return r
        Free p → do
          f' ← p
          goncats f'
    go ∷ Monad m ⇒ FreeT (Producer a m) m x → FreeT (Producer a m) m x
    go f = FreeT $ do
      g ← runFreeT f
      case g of
        Pure _ → return g
        Free h → do
          h' ← P.runEffect $ P.for h P.discard
          runFreeT $ go h'


