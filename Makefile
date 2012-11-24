all:	Fireball FireballRender Main

FireballRender: FireballRender.hs
	ghc -threaded --make -main-is FireballRender FireballRender.hs


Fireball: Fireball.hs
	ghc -threaded --make -main-is Fireball Fireball.hs

Main: Main.hs
	ghc -O2 -fhpc --make Main.hs

run: Main
	./gen.sh

clean:
	rm -rf .hpc *.tix *.hi *.o *.hfb

.phony: clean all
