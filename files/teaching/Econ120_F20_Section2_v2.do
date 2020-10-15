* ---------------------------- *
* Econ120 (J. Robinson, F2020) *
* Section 2 (Oct. 12, 2020)	   * NOTE: all the path directories are based on my own computer, 
* TA: David Sungho Park        *       so you should change them according to yours.
* ---------------------------- *

clear all
set more off

cap noisily cd "/Users/DSP/Google Drive/UCSC/Teaching/Econ120_F20/Analysis/"

log using "doFiles/Econ120_F20_Section2.smcl"

use "dtaFiles/farming.dta", clear

// Exporting (nice-looking) sumstat table
ssc install estout // you need to install this package only once
eststo s2_tab1: estpost su age gender edu_years married, d
esttab s2_tab1 using "output/Section2_table1_raw.csv", replace cells("mean(fmt(%9.2f)) sd(fmt(%9.2f)) count(fmt(%9.0fc))") nonumber nomtitle noobs plain 

// Using Stata as calculator 
su revenue_blue revenue_unmarked, d
display (7631-6257)/6257

// Mean difference test (t-test)
ttest revenue_blue = revenue_unmarked

// Referring to (non)missing observations for a variable
count if missing(training)
count if !mi(training)

	// Another way
	count if training==.
	count if training!=.

// Generating a binary variable equal to 1 if satisfying a condition
gen dropped = .
replace dropped = 1 if !mi(training) & harvest!=1 
replace dropped = 0 if !mi(training) & harvest==1

	// A more streamlined way
	gen dropped2 = (harvest!=1) if !mi(training)
	compare dropped dropped2 // just to check whether the 2 vars are identical

// Testing for differences
reg ever_anyfert dropped 

// Doing this for multiple variables 
local i = 1
foreach YYY in age gender edu_years married {
	eststo s2_reg_`i': reg `YYY' dropped 
	su `YYY' if dropped==0 
	estadd scalar nondroppedmean = r(mean)
	local i = `i' + 1
}

estout s2_reg_* using "output/Section2_table2_raw.csv", replace style(tab) delimiter(",") mlabel(,depvars) collabel(none) label ///
	drop(_cons) stats(nondroppedmean N, labels("Non-dropped mean" "Obs") fmt(%9.2f %9.0fc)) starlevels(* 0.10 ** 0.05 *** 0.01) ///
	cells("b(fmt(%9.2f) star)" "se(fmt(%9.2f) par)") 

// Checking attrition balance
reg dropped training

log close

exit



