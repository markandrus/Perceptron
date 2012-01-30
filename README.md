Linear & Kernel Perceptron
==========================
By _Mark Roberts_ andrus@uchicago.edu

This project implements a linear and kernel perceptron.

Usage
-----

	$ ./bin/perc -h
	perc
	  -i FILE       --vectors=FILE    Input vectors
	  -l FILE       --labels=FILE     Input labels
	  -p lp|lp2|gp  --perc=lp|lp2|gp  Perceptron algorithm
	  -s FILE       --sigma=FILE      Sigma value to use with Guassian perceptron
	  -o FILE       --out=FILE        Output path
	  -v            --verbose         Enable verbose messages
	  -h            --help            Show help

Kernels
-------
Kernels may be specified with the `-p` or `-perc` flags. The flags and their corresponding kernels
are listed below:

Flag  | Kernel          | Perceptron
------|-----------------|------------
`lp`  |                 | Linear
`lp2` | Dot product     | Kernel
`gp`  | Gaussian kernel | Kernel

Tests
-----
You can run `make test` from the `Makefile`--this executes `runhaskell Tests.hs`. The test confirms
that

1. sorting the `shuffle` of a list is equal to the sort of the list (no elements lost)
2. `lp` and `lp2` produce the same results
3. `lpredict` and `lpredict2` produce the same results (linear and kernel prediction)
