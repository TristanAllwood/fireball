all:	Fireball FireballRender

FireballRender: FireballRender.hs
	ghc -threaded --make -main-is FireballRender FireballRender.hs


Fireball: Fireball.hs
	ghc -threaded --make -main-is Fireball Fireball.hs

clean:
	rm -rf .hpc *.tix *.hi *.o *.hfb *.html
	rm -f Fireball FireballRender
	rm -rf outputs

.phony: clean all
