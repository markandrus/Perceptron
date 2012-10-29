Linear & Kernel Perceptron
==========================

This project implements a linear and kernel perceptron.

Usage
-----

	$ ./bin/perc -h
	perc
		-t FILE             --in-train-vecs=FILE        Input training vectors
		-l FILE             --in-train-labels=FILE      Input training labels
		-i FILE             --in-vecs=FILE              Input vectors (to classify)
		-c [lp | lp2 | gp]  --class=[lp | lp2 | gp]     Classifier
		-p [lp | lp2 | gp]  --pred=[lp | lp2 | gp]      Predictor
		-s DOUBLE           --sigma=DOUBLE              Sigma value for Guassian kernel (gp)
		-w FILE             --out-weight=FILE           Output weight path
		-o FILE             --out-preds=FILE            Output predictions path
		-v                  --verbose                   Enable verbose messages
		-h                  --help                      Show help
		-C                  --cross-validate            Cross validate from 90%/10% to 10%/90%
		-N INT              --cross-validate-count=INT  Number of tests to run before averaging

Typical execution looks like `$ ./bin/perc -t data/train2k.databw.35 -l data/train2k.label.35 -i data/test200.databw.35 -c gp -p gp -s 0.9 -v -o out/test200.label.kernel.35`.

Classifiers/Predictors
----------------------
Kernels may be specified with the `-c`/`-class` and `-p`/`--pred` flags. The flags and their
corresponding kernels are listed below:

Flag  | Kernel          | Perceptron
------|-----------------|------------
`lp`  |                 | Linear
`lp2` | Dot product     | Kernel
`gp`  | Gaussian kernel | Kernel

Tests
-----
### Cross Validation
The `-C`/`--cross-validate` triggers the cross validation suite. Given a number of times to
average randomized shuffles of the input data, `-N`/`--cross-validate-count`, the cross validator
will execute the perceptron using first 10% of the training data as test data, up to 90% of the 
training data as test data.

### QuickCheck
You can run `make test` from the `Makefile`--this executes `runhaskell Tests.hs`. The test runs
QuickCheck to confirm that

1. sorting the `shuffle` of a list is equal to the sort of the list (no elements lost)
2. `lp` and `lp2` produce the same results
3. `lpredict` and `lpredict2` produce the same results (linear and kernel prediction)