all: src/Main.hs src/Options.hs src/Perceptron.hs
	mkdir -p bin
#	cd src && ghc -O -o ../bin/perc Main.hs
#	The following yields ~1.18 (MacBook) to ~1.3 (Linux) times faster code
	cd src && ghc -O2 -fspec-constr-count=2 -o ../bin/perc Main.hs

profile: src/Main.hs src/Options.hs src/Perceptron.hs
	mkdir -p bin
	cd src && ghc -O -o ../bin/perc Main.hs -prof -rtsopts -auto-all

clean:
	rm -rf bin
	rm -f src/*.hi src/*.o

test:
	cd src && runhaskell Tests.hs
