Fireball
========

A *very hacky*, incomplete proof of concept that visualises a
subexpression-by-subexpression execution trace of (selected modules) from a
Haskell program using javascript + html.

Samples of output:

  A simple Loop program:
  http://www.doc.ic.ac.uk/~tora/fireball/Loop.html

  Not quicksort:
  http://www.doc.ic.ac.uk/~tora/fireball/QuickSort.html

  A subset of an ICFP 2012 entry.  Note the program running is pretty exotic,
  using a full set of GHC extensions, including being linked with Foreign C
  code.
  http://www.doc.ic.ac.uk/~tora/fireball/icfp2012/Hack.html

The principle is that you have a GHC compiled Haskell binary, where the modules
you care about have been compiled with -fhpc.  We then use gdb + valgrind at
runtime to watch every write to the hpc tickboxes arrays, and store a trace of
*every single write*.  Diffing this lets us see which subexpression was just
entered (for a single threaded program).


Fireball will run gdb and valgrind on a program to produce a hfb
(haskell-fireball) file, FireballRender can then turn that into a .html and .js
file to visualise.  Look at gen.sh for a full example of how to invoke this.
