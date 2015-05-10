[![Build Status](https://travis-ci.org/choener/BiobaseFasta.svg?branch=master)](https://travis-ci.org/choener/BiobaseFasta)

# BiobaseFasta

## A Haskell library for FASTA-file handling.

This re-written version (0.0.1.0) is based on the conduit library and includes
some preliminary abstraction mechanism over zero- and one-based indices.




## Old 0.0.0.x text:

Enumeratees for FASTA-handling and convenience functions. In a
typical application, the user should write an enumeratee to
extract information to allow for efficient low-memory handling
of queries.

Facilities for fast, efficient linear scans are provided. These
functions are not yet stable and could move to another library,
if more data sources require efficient scans.

The library is, in general, in a "preview" state. In cases
where you need to scan large FASTA files fast and with low
memory overhead, the enumeratees should, however, already be
useable enough.



#### Contact

Christian Hoener zu Siederdissen  
Leipzig University, Leipzig, Germany  
choener@bioinf.uni-leipzig.de  
http://www.bioinf.uni-leipzig.de/~choener/  

