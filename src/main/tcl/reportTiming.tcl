#------------------------------------------------------------------------
# reportTiming.tcl
#------------------------------------------------------------------------
# This function generates a report of the 10 worst Setup/Max violations  
#------------------------------------------------------------------------
proc reportTiming { fileName } {

	# Open output file in write mode
	set FH [open $fileName w]
	# Write header
	puts $FH "Startpoint,Endpoint,DelayType,Slack,#Levels,#LUTs,StartRoot,EndRoot,Skew,Logic,Net,Arr,Req"
	set delayType max
	# Collect details from the 50 worst timing paths for the current analysis
	foreach path [get_timing_paths -delay_type $delayType -max_paths 10 -nworst 1] {
		# Get information for timing paths
		set luts [get_cells -filter {REF_NAME =~ LUT*} -of_object $path]
		set startpoint [get_property STARTPOINT_PIN $path]
		set endpoint [get_property ENDPOINT_PIN $path]
		set slack [get_property SLACK $path]
		set levels [get_property LOGIC_LEVELS $path]
		set startroot [get_property STARTPOINT_CLOCK_ROOT $path]
		set endroot [get_property ENDPOINT_CLOCK_ROOT $path]
		set skew [get_property SKEW $path]
		set logic [get_property DATAPATH_LOGIC_DELAY $path]
		set net [get_property DATAPATH_NET_DELAY $path]
		set arrival [get_property ARRIVAL_TIME $path]
		set required [get_property REQUIRED_TIME $path]
		# write the path details
		puts $FH "$startpoint,$endpoint,$delayType,$slack,$levels,[llength $luts],$startroot,$endroot,$skew,$logic,$net,$arrival,$required"
	}
	# Close the output file
	close $FH
	puts "CSV file $fileName has been created.\n"
	return 0
};
