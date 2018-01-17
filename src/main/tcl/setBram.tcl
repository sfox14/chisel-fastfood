proc setBram { B H } {

	set_property DOA_REG 1 [get_cells HadBlock/PE/dataMem/DualPortBRAM/mem_reg_bram_0] 
	for {set i 1} {$i < $B} {incr i} {
		set_property DOA_REG 1 [get_cells "HadBlock/PE_$i/dataMem/DualPortBRAM/mem_reg_bram_0"]
	}
	for {set j 1} {$j < $H} {incr j} {
		set_property DOA_REG 1 [get_cells "HadBlock_$j/PE/dataMem/DualPortBRAM/mem_reg_bram_0"]
		for {set i 1} {$i < $B} {incr i} {
			set_property DOA_REG 1 [get_cells "HadBlock_$j/PE_$i/dataMem/DualPortBRAM/mem_reg_bram_0"]
		}
	}

}

# Remove all elements of a list from another list
proc lremove {args} {
	if {[llength $args] < 2} {
		puts stderr {Wrong # args: should be "lremove ?-all? list pattern"}
	}
	set list [lindex $args end-1]
	set elements [lindex $args end]
	if [string match -all [lindex $args 0]] {
		foreach element $elements {
			set list [lsearch -all -inline -not -exact $list $element]
		}
	} else {
	# Using lreplace to truncate the list saves having to calculate
	# ranges or offsets from the indexed element. The trimming is
	# necessary in cases where the first or last element is the
	# indexed element.
		foreach element $elements {
			set idx [lsearch $list $element]
			set list [string trim \
			"[lreplace $list $idx end] [lreplace $list 0 $idx]"]
		}
	}
	return $list
}

# Script to set false timing paths. This crashes for hierarchical/Fastfood
# because there's too many. 
proc setTimingPaths {} {
	set allPinsA [get_pins -of [get_cells -hier -regexp .*HadBlock.*]]
	set allPins [get_pins *]
	set cxPins [get_pins {HadBlock_*/PE_*/io*}]
	set falsePaths [lremove -all $allPins $cxPins]
	set falsePathsA [lremove -all $allPinsA $cxPins]

	#puts $falsePaths
	set_false_path -through $falsePaths
	set_false_path -through $falsePathsA

}

# Rectangular Pblock
proc fpblock {} {
	create_pblock dummy
	add_cells_to_pblock [get_pblocks dummy] -top
	resize_pblock [get_pblocks dummy] -add {SLICE_X36Y0:SLICE_X46Y299}
	set_property CONTAIN_ROUTING true [get_pblocks dummy]
}

# Floorplan:
# 	- linear array of PEs (practise/DummyPE)
# 	- place top down
proc fplan { N } {

	# entity names
	set pbId "pb"
	set peId "DummyPE"

	# start position
	set W 11
	set H 8
	set JX 0
	set JY 0
	set varX 36
	set varY 292

	# top right corner of PE
	set varXoff [ expr $varX + $W - 1]
	set varYoff [ expr $varY + $H - 1]

	# distance between PEs (when moving up/dwn use dY, else when right/left use dX)
	set dX [ expr $W+$JX ]
	set dY [ expr $H+$JY ]

	set idx 0
	set sgn -1

	# Move Up
	for {set i 0} {$i < $N} {incr i} {
		# offset
		set varXoff [ expr $varX + $W - 1]
		set varYoff [ expr $varY + $H - 1]

		set slicexy [format {SLICE_X%sY%s:SLICE_X%sY%s} $varX $varY $varXoff $varYoff]
		set pbId [format {pb%s} $idx]
		set peId [ expr { $i eq 0 ? "DummyPE" : "DummyPE_$idx" } ]
		
		create_pblock $pbId
		add_cells_to_pblock $pbId [get_cells [list $peId]]
		resize_pblock $pbId -add $slicexy
		set_property CONTAIN_ROUTING true [get_pblocks $pbId]

		# increment
		set idx [ expr $idx + 1 ] 
		# increment varY (except on last PE of column)
		set varY [ expr { $i eq ($N-1) ? $varY : ($varY + $dY*$sgn) } ]
	}


}

# hierarchical/Fastfood I/0 paths
proc getPaths { FNAME } {
	set cxPins [get_pins {HadBlock_*/PE_*/dat_out_reg[*]/C HadBlock/PE_*/dat_out_reg[*]/C} ]
	report_timing -through $cxPins -setup -path_type short -max_paths 600 -file $FNAME
}

# practise/DummyPE I/O paths
proc getPathsEx1 { FNAME } {
	set cxPins [get_pins {DummyPE_*/x_parr_reg[*]/C DummyPE/x_parr_reg[*]/C DummyPE_*/dat_out_reg[*]/C DummyPE/dat_out_reg[*]/C DummyPE_*/kin_reg[*]/D DummyPE/kin_reg[*]/D} ]
	report_timing -through $cxPins -name timing_1 -setup -path_type short -max_paths 600 -file $FNAME 
}