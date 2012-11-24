all:	Fireball Main

Fireball: Fireball.hs
	ghc -threaded --make -main-is Fireball Fireball.hs

Main: Main.hs
	ghc -O2 -fhpc --make Main.hs

run: Main
	./gen.sh

clean:
	rm -rf .hpc Main.tix Main.hi Main.o

.phony: clean all
