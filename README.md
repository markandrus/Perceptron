Linear & Kernel Perceptron
==========================
By _Mark Roberts_ andrus@uchicago.edu

This project implements a linear and kernel perceptron.

Kernels
-------
Kernels may be specified with the `-p` or `-perc` flags. The flags and their corresponding kernels
are listed below:

Flag  | Perceptron/Kernel
------|--------------------------
`lp`  | linear perceptron
`lp2` | dot product/linear kernel
`gp`  | Gaussian kernel

Tests
-----
You can run `make test` from the `Makefile`--this executes `runhaskell Tests.hs`. The test confirms
that

1. `lp` and `lp2` produce the same results
