* ---------------------------- *
* Econ120 (J. Robinson, F2020) *
* Section 3 (Oct. 19, 2020)	   * NOTE: all the path directories are based on my own computer, 
* TA: David Sungho Park        *       so you should change them according to yours.
* ---------------------------- *

clear all
set more off

cd "/Users/DSP/Google Drive/UCSC/Teaching/Econ120_F20/SectionNotes/Section3/" // Note: I changed this from previous do-files for Sections 1 & 2

//log using "Econ120_F20_Section3.smcl", replace // you can uncomment this line if you want to keep a Stata log file

//ssc install estout // you need to install this package only once

use "farming.dta", clear

// Getting sumstats 
su kg_tdfert_blue kg_plantfert_blue packets_hyb_blue
su kg_tdfert_unmarked kg_plantfert_unmarked packets_hyb_unmarked

// Testing for mean differences
gen kg_tdfert_diff 		= kg_tdfert_blue 	- kg_tdfert_unmarked
gen kg_plantfert_diff 	= kg_plantfert_blue - kg_plantfert_unmarked
gen packets_hyb_diff 	= packets_hyb_blue 	- packets_hyb_unmarked

ttest kg_tdfert_diff	= 0
	
	// equivalent to this
	ttest kg_tdfert_blue = kg_tdfert_unmarked 

ttest kg_plantfert_diff	= 0
ttest packets_hyb_diff	= 0

// Constructing mean-difference table (similar to what you've done for Econ104)
preserve
	// First step: we need to reshape the dataset from "wide" format to "long" format
	keep respondent_id kg_tdfert_unmarked kg_plantfert_unmarked packets_hyb_unmarked kg_tdfert_blue kg_plantfert_blue packets_hyb_blue 
	reshape long kg_tdfert kg_plantfert packets_hyb, i(respondent_id) j(plot,string)

	// Step 2: exporting into single table (similar to what you've done for Econ104)
	est clear 
	eststo unmarked: 	estpost summarize 	kg_tdfert kg_plantfert packets_hyb if plot=="_unmarked"
	eststo blue: 		estpost summarize 	kg_tdfert kg_plantfert packets_hyb if plot=="_blue"
	eststo difference: 	estpost ttest 		kg_tdfert kg_plantfert packets_hyb, by(plot) 

	esttab unmarked blue difference using "Section3_table1_raw.csv", replace  cells("mean(pattern(1 1 0) fmt(%9.2f)) b(pattern(0 0 1) fmt(%9.2f) star)" "sd(pattern(1 1 0) fmt(%9.2f) par) se(pattern(0 0 1) fmt(%9.2f) par)") starlevels(* 0.10 ** 0.05 *** 0.01)  plain 
restore

// Calculating monetary cost of inputs
gen input_ksh_blue = kg_tdfert_blue*65 + kg_plantfert_blue*70 + packets_hyb_blue*230
gen input_ksh_unmarked = kg_tdfert_unmarked*65 + kg_plantfert_unmarked*70 + packets_hyb_unmarked*230

ttest input_ksh_blue = input_ksh_unmarked

	// FYI: To export in nice-looking table format, you can do something similar to above
	preserve
		// Step 1: eshaping the dataset from "wide" to "long" 
		keep respondent_id input_ksh_blue input_ksh_unmarked
		reshape long input_ksh, i(respondent_id) j(plot,string)

		// Step 2: exporting into single table (similar to what you've done for Econ104)
		est clear 
		eststo unmarked: 	estpost summarize 	input_ksh if plot=="_unmarked"
		eststo blue: 		estpost summarize 	input_ksh if plot=="_blue"
		eststo difference: 	estpost ttest 		input_ksh, by(plot) unequal

		esttab unmarked blue difference using "Section3_table2_raw.csv", replace  cells("mean(pattern(1 1 0) fmt(%9.2f)) b(pattern(0 0 1) fmt(%9.2f) star)" "sd(pattern(1 1 0) fmt(%9.2f) par) se(pattern(0 0 1) fmt(%9.2f) par)") starlevels(* 0.10 ** 0.05 *** 0.01)  plain 
	restore

// Calculating rate of return
gen rate_of_return = ((revenue_blue - revenue_unmarked) - (input_ksh_blue - input_ksh_unmarked)) / (input_ksh_blue - input_ksh_unmarked)
su rate_of_return, d

// Histograms
histogram revenue_blue, saving(histogram_revenue_blue, replace) xtitle("revenue (Ksh)") ytitle("percentage of farmers") title("Distribution of revenue on blue plot")
graph export "histogram_revenue_blue.pdf", replace // this line is necessary to save in PDF format

kdensity revenue_blue, saving(histogram_revenue_blue, replace) xtitle("revenue (Ksh)") ytitle("percentage of farmers") title("Distribution of revenue on blue plot")
graph export "kdensity_revenue_blue.pdf", replace // this line is necessary to save in PDF format

// Putting multiple plots into a single graph
twoway ///
	(kdensity revenue_blue) ///
	(kdensity revenue_unmarked, lpattern("-")), ///
	saving(kdensity_both, replace) ///
	xtitle("revenue (Ksh)") ytitle("Percentage of farmers") ///
	legend(order(1 "Blue Plot" 2 "Unmarked plot"))
graph export "both_revenue_blue.pdf", replace // this line is necessary to save in PDF format

// Dealing with outliers
su rate_of_return, d
di `r(p5)'  // 5th-%tile value
di `r(p95)' // 95th-%tile value
	
	// Saving the values for later reference
	gen rate_of_return_p5 = `r(p5)'
	gen rate_of_return_p95 = `r(p95)'

kdensity rate_of_return if rate_of_return>rate_of_return_p5 & rate_of_return<rate_of_return_p95, xtitle("revenue (Ksh)") ytitle("percentage of farmers") title("Rate of return - excluding top and bottom 5%")
graph export "kdensity_return.pdf", replace // this line is necessary to save in PDF format

// Bonus
twoway ///
	(kdensity rate_of_return if rate_of_return>rate_of_return_p5 & rate_of_return<rate_of_return_p95 & training==1, lpattern(solid)) ///
	(kdensity rate_of_return if rate_of_return>rate_of_return_p5 & rate_of_return<rate_of_return_p95 & training==0, lpattern(dash)), ///
	xtitle("revenue (Ksh)") ytitle("Percentage of farmers") legend(order(1 "Training group" 2 "Control group"))
graph export "kdensity_return_training.pdf", replace 

// REVIEW: exporting regression tables
local i = 1
foreach YYY in revenue_blue revenue_unmarked rate_of_return {
	eststo s3_reg_`i': reg `YYY' training 
	su `YYY' if training==0 
	estadd scalar controlmean = r(mean)
	estadd scalar controlsd = r(sd)
	local i = `i' + 1
}

estout s3_reg_* using "Section3_table2_raw.csv", replace style(tab) delimiter(",") mlabel(,depvars) collabel(none) label ///
	drop(_cons) stats(controlmean controlsd N, labels("Control mean" "Control SD" "Obs") fmt(%9.2f %9.2f %9.0fc)) starlevels(* 0.10 ** 0.05 *** 0.01) ///
	cells("b(fmt(%9.2f) star)" "se(fmt(%9.2f) par)") 
	
	
//log close // Again, you can just un-comment this line if you want to keep a Stata log file

exit



