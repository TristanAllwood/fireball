Main: Main.hs
	ghc -O2 -fhpc --make Main.hs

run: Main
	./gen.sh

clean:
	rm -rf .hpc Main.tix Main.hi Main.o

.phony: clean
