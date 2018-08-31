# Chisel Fastfood

This repository contains Chisel HDL code for the paper titled `FPGA Fastfood: A High Speed Systolic Implementation of a Large Scale Online Kernel Method` given [here](http://phwl.org/papers/ff_fpga18.pdf).

## Quick Start
To generate the verilog, run:

```
make run-ff N=4096 D=512 P=128
```
This will create a verilog file called `Fastfood.v` in the `./verilog` directory. The command line arguments must be chosen with reference to the paper, eg. `N=Number of dictionary entries`, `D=Input dimensionality` and `P=Number of PEs`.


