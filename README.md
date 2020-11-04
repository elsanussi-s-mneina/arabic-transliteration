# Introduction
This is a project for transliterating Arabic to Latin written by Elsanussi Mneina. It is not ready for use.

I do consider the implementation of the Buckwalter transliteration system complete.

The implementation of ISO233-2 is incomplete, and of doubtful use because I did not purchase access to the official standard.

For people not interested in Arabic or transliteration, you may be interested this as an example project showing how to accept command line arguments using Haskell.

The Buckwalter transliteration system was found at
the following website:
-- http://www.qamus.org/transliteration.htm


The ISO233-2 was found in 
-- https://fr.wikipedia.org/wiki/ISO_233-2



# Example usages from the command line

Using stack:

`stack run -- -l test/arabic_test.txt`

`stack run -- -i test/arabic_test.txt`