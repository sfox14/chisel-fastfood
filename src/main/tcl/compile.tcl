#
# Compile script for Synthesis and Implementation 
#	- called from bash script
#   	- run on Kintex Ultrascale 
#

set srcDir "./src/main/verilog/"
set ::outputDir "./outputs"
file mkdir $::outputDir

set topMod   		[lindex $argv 0] ;		# Top module name
set verilogSource 	[lindex $argv 1] ;		# Verilog source
set config 			[lindex $argv 2] ;		# Configuration parameters
set device 			[lindex $argv 3] ; 		# Device

set tclSource "./src/main/tcl/setBram.tcl"
puts stdout $topMod
puts stdout $verilogSource
puts stdout $tclSource
puts stdout $config
puts stdout $device

# unpack config params
if {[llength [split $config :]] == 4} {
	foreach {N D P E} [split $config :] {}
	if {$E == 0} {
		set K [ expr $N/$P ]
		set H [ expr $N/$D ]
		set B [ expr $D/$K ]
	}
} else {
	puts "ERROR: Missing configuration parameters, N:D:P"
}

# get the part number
if {$device == "KU035"} {
	set DEVICE "xcku035-fbva676-2-e"
} elseif {$device == "V707"} {
	set DEVICE "xc7vx485tffg1761-2"
}

if {$E == 0} {
	set outname "k${::K}b${::B}h${::H}"
} else {
	if {$E == 1} {
		set outname "n${::N}k${::D}"
	} else {
		set outname "n${::N}k${::D}fp"
	}
	
}

puts $outname

create_project -in_memory -part $DEVICE
set_property target_language Verilog [current_project]

# import source .tcl files
source ./src/main/tcl/reportTiming.tcl
source $tclSource

set ipcore_files [list \
  "/home/sean/projects/vivado/roofline/DummyPE/DummyPE.srcs/sources_1/ip/mult_gen_0/mult_gen_0.xci"
]

if {[llength $::verilogSource] != 0} {
	read_verilog $::verilogSource
	read_verilog "${::srcDir}/DualPortBram.v"
	read_verilog "${::srcDir}/DualPortLutRam.v"
	read_ip -verbose $ipcore_files
} else {
	puts "ERROR: Verilog source file not found"
}

#synth_design -top $topMod -part $DEVICE -include_dirs $srcDir
synth_design -top $topMod -part $DEVICE -no_lc -keep_equivalent_registers -resource_sharing off -flatten_hierarchy none -include_dirs $srcDir
reportTiming "${::outputDir}/post_synth_critpath_${::outname}.rpt" 

# Read xdc file and run pre-place .tcl script
if {$E == 0} {
	read_xdc "./src/main/resources/timing.xdc"
} else {
	read_xdc "./src/main/resources/timing-ex1.xdc"
}

if {$E == 0} {
	setBram $B $H
} else {
	if {$E == 1} {
		fpblock
	} else {
		fplan $N
	}
	
}

#setTimingPaths

#opt_design
place_design
#phys_opt_design
route_design -directive Explore

# checkpoint and reports
write_checkpoint -force "${::outputDir}/post_route_${::outname}.dcp"
#reportTiming "${outputDir}/post_route_critpath_${::outname}.rpt"
#report_route_status -file "${::outputDir}/post_route_status_${::outname}.rpt"

#report_timing -sort_by "slack" -delay_type "max" -max_paths 10 -nworst 1 -file "${outputDir}/post_route_timing_${::outname}.rpt" -rpx "${outputDir}/timing_${::outname}.rpx"
#report_timing -sort_by "slack" -delay_type "min" -max_paths 10 -nworst 1 -file "${outputDir}/post_route_hold_${::outname}.rpt"
#report_utilization -file "${::outputDir}/post_route_utilisation_${::outname}.rpt"

if {$E == 0} {
	getPaths "./outputs/paths/paths_${::outname}.rpt"
} else {
	getPathsEx1 "./outputs/paths/paths_${::outname}.rpt"
}
