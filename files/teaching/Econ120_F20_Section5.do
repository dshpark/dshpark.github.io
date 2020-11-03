* ---------------------------- *
* Econ120 (J. Robinson, F2020) *
* Section 5 (Nov. 2, 2020)	   * NOTE: all the path directories are based on my own computer, 
* TA: David Sungho Park        *       so you should change them according to yours.
* ---------------------------- *

clear all
set more off

cd "/Users/DSP/Google Drive/UCSC/Teaching/Econ120_F20/SectionNotes/Section5/"

use "farming.dta", clear

// Generating a variable that's a sum of multiple variables
gen total_time_blue = total_time_plowing_blue + total_time_planting_blue + total_time_plantfert_blue + total_time_weeding_blue + total_time_tdfert_blue + total_time_manure_blue + total_time_maint_blue

	// Another way (Note: be careful to put the "missing" option at the end!)
	egen total_time_blue_v2 = rowtotal(total_time_plowing_blue total_time_planting_blue total_time_plantfert_blue total_time_weeding_blue total_time_tdfert_blue total_time_manure_blue total_time_maint_blue), missing
		
	// check how it sums up "unconditionally" when missing is not specified
	egen total_time_blue_v3 = rowtotal(total_time_plowing_blue total_time_planting_blue total_time_plantfert_blue total_time_weeding_blue total_time_tdfert_blue total_time_manure_blue total_time_maint_blue)

// Correcting for acreage difference (i.e. making the denominator the same -- "number of hours per XX acre")
egen total_time_rof = rowtotal(total_time_plowing_rof total_time_planting_rof total_time_plantfert_rof total_time_weeding_rof total_time_tdfert_rof total_time_manure_rof total_time_maint_rof), missing
gen total_time_rof_per_acre = (total_time_rof/acres_rof)		// per acre
gen total_time_rof_per_qtr_acre = total_time_rof_per_acre*0.25	// per 0.25 acre

// Checking for special codes 
su facilitator_blue facilitator_unmarked facilitator_rof
tab facilitator_rof

// Replacing special codes with a missing value
replace facilitator_rof=. if facilitator_rof==99

// Mean difference tests for more than two variables (remember this only works with pairs!)
ttest facilitator_blue		= facilitator_unmarked 
ttest facilitator_unmarked	= facilitator_rof
ttest facilitator_rof		= facilitator_blue

// Regressions
reg NGO_kick_out_blue_does_badly training   

	// Other ways to check the difference in means
	su NGO_kick_out_blue_does_badly if training==1 // for treatment group
	su NGO_kick_out_blue_does_badly if training==0 // for control group
	ttest NGO_kick_out_blue_does_badly, by(training) // mean difference test between treatment and control groups


exit

