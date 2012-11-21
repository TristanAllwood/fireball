#!/bin/bash

# rm -rf Main Main.o Main.hi .hpc
rm -rf Main.tix

# ghc -fhpc --make Main

gdb -batch -x gdbscript
