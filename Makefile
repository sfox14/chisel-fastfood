SBT ?= sbt
SBT_FLAGS ?= -Dsbt.log.noformat=true
CHISEL_FLAGS := --wio
staging_dir := ~/.sbt/0.13/staging

top_srcdir ?= .
srcdir ?= src/main/scala/*
top_file := src/main/scala/top.scala
source_files := $(wildcard $(srcdir)/*.scala)

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

run: $(top_file)
	set -e pipefail; $(SBT) $(SBT_FLAGS) "run --genHarness --backend v $(CHISEL_FLAGS)"



# refernece - http://www.scala-sbt.org/0.13/docs/Command-Line-Reference.html
