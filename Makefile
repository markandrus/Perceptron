all: src/Main.hs src/Options.hs src/Perceptron.hs
	mkdir -p bin
	cd src && ghc -O -o ../bin/perc Main.hs

clean:
	rm -rf bin
	rm -f src/*.hi src/*.o

test:
	cd src && runhaskell Tests.hs
