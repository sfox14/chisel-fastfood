SBT ?= sbt
SBT_FLAGS ?= -Dsbt.log.noformat=true -language:postfixOps
CHISEL_FLAGS := --wio
staging_dir := ~/.sbt/0.13/staging

top_srcdir ?= .
srcdir ?= src/main/scala/*
top_file := src/main/scala/top.scala
source_files := $(wildcard $(srcdir)/*.scala)

# Fastfood parameters
top_name ?= Fastfood
top_src ?= ./verilog/Fastfood.v
compile_src = ./src/main/tcl/compile.tcl
N ?= 2048
D ?= 2048
P ?= 256
config ?= $(N):$(D):$(P):$(E)
device ?= KU035
# E - controls compile script. E=0 Fastfood. E=1 DummyArray pblock. E=2 DummyArray floorplan
E ?= 0

# DummyArray Parameters
Q ?= $(N)
K ?= $(D)


# Clean-up
clean:
	$(SBT) $(SBT_FLAGS) +clean
	-rm -f *.h *.hex *.flo *.cpp *.o *.out *.v *.vcd *~
	-rm -rf .emulator verilog benchmark
	sbt clean
	-rm -rf project/target/ target/

cleanall: clean
	-rm -rf $(staging_dir)/*


# Run tests and generate verilog:
# Select main
run:
	set -e pipefail; $(SBT) $(SBT_FLAGS) "run --genHarness --backend v $(CHISEL_FLAGS)"

# Generate Fastfood verilog
run-ff:
	set -e pipefail; $(SBT) $(SBT_FLAGS) "run-main hierarchical.fastfoodVerilog $(N) $(P) $(D)"

# Generate DummyArray verilog
run-ex1:
	set -e pipefail; $(SBT) $(SBT_FLAGS) "run-main practise.dummyArrayVerilog $(Q) $(K)" 

# Place and route Fastfood
impl:	run-ff
	mkdir -p outputs/ && vivado -mode batch -source $(compile_src) -log ./outputs/vivado.log -journal ./outputs/vivado.jou -tclargs $(top_name) $(top_src) $(config) $(device)

# Place and route DummyArray
impl-ex1: run-ex1
	mkdir -p outputs/ && vivado -mode batch -source $(compile_src) -log ./outputs/vivado.log -journal ./outputs/vivado.jou -tclargs $(top_name) $(top_src) $(config) $(device)

# Explore differet Fastfood designs
explore:
	$(MAKE) impl N=4096 D=4096 P=512
	$(MAKE) impl N=8192 D=4096  P=512
	$(MAKE) impl N=16384 D=4096 P=512
	$(MAKE) impl N=32768 D=4096 P=512
	(MAKE) impl N=65536 D=4096 P=512

# Explore different DummyArray designs
explore-ex1:
	$(MAKE) impl-ex1 N=37 D=36 top_src="./verilog/DummyArray.v" top_name="DummyArray" E=1
	$(MAKE) impl-ex1 N=37 D=25 top_src="./verilog/DummyArray.v" top_name="DummyArray" E=1
	$(MAKE) impl-ex1 N=37 D=16 top_src="./verilog/DummyArray.v" top_name="DummyArray" E=1
	$(MAKE) impl-ex1 N=37 D=8 top_src="./verilog/DummyArray.v" top_name="DummyArray" E=1
	$(MAKE) impl-ex1 N=37 D=4 top_src="./verilog/DummyArray.v" top_name="DummyArray" E=1
	$(MAKE) impl-ex1 N=37 D=2 top_src="./verilog/DummyArray.v" top_name="DummyArray" E=1

	$(MAKE) impl-ex1 N=37 D=36 top_src="./verilog/DummyArray.v" top_name="DummyArray" E=2
	$(MAKE) impl-ex1 N=37 D=25 top_src="./verilog/DummyArray.v" top_name="DummyArray" E=2
	$(MAKE) impl-ex1 N=37 D=16 top_src="./verilog/DummyArray.v" top_name="DummyArray" E=2
	$(MAKE) impl-ex1 N=37 D=8 top_src="./verilog/DummyArray.v" top_name="DummyArray" E=2
	$(MAKE) impl-ex1 N=37 D=4 top_src="./verilog/DummyArray.v" top_name="DummyArray" E=2
	$(MAKE) impl-ex1 N=37 D=2 top_src="./verilog/DummyArray.v" top_name="DummyArray" E=2

explore-all:
	$(MAKE) explore-ex1
	$(MAKE) explore


# refernece - http://www.scala-sbt.org/0.13/docs/Command-Line-Reference.html