#!/bin/bash

make || exit "Build failed"
mkdir -p outputs

ln -s ../style.css outputs/
ln -s ../jquery-1.8.3.js outputs/
ln -s ../code.js outputs/

rm -rf samples/.hpc

for i in Loop QuickSort ; do
  (cd samples ; ghc -fforce-recomp -main-is $i -fhpc -O2 $i.hs ; ../Fireball "./$i" $i ; ../FireballRender $i ; cp $i.{html,js} ../outputs)
done

