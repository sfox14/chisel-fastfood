# TCL function for sizing pblock

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

set allPinsA [get_pins -of [get_cells -hier -regexp .*HadBlock.*]]
set allPins [get_pins *]
set cxPins [get_pins {HadBlock_*/PE_*/io*}]
set falsePaths [lremove -all $allPins $cxPins]
set falsePathsA [lremove -all $allPinsA $cxPins]

#puts $falsePaths
set_false_path -through $falsePaths
set_false_path -through $falsePathsA






