* ---------------------------- *
* Econ120 (J. Robinson, F2020) *
* Section 6 (Nov. 10, 2020)	   * NOTE: all the path directories are based on my own computer, 
* TA: David Sungho Park        *       so you should change them according to yours.
* ---------------------------- *

clear all
set more off

cd "/Users/DSP/Google Drive/UCSC/Teaching/Econ120_F20/SectionNotes/Section6/"

use "Uganda_baseline.dta", clear

// Cleaning response codes by examining the survey documentation
	// You can also check this wit the "fre" command (instead of using the "tabulate" command)
	*ssc install fre
	tab sa1
	fre sa1

gen savings_yn =.
replace savings_yn=1 if sa1==1 // 1 - Yes
replace savings_yn=0 if sa1==2 // 2 - No

gen female_literate_yn=.
replace female_literate_yn=1 if fh4==1
replace female_literate_yn=0 if fh4==2

gen male_literate_yn=.
replace male_literate_yn=1 if mh4==1
replace male_literate_yn=0 if mh4==2

// Generating a binary (0/1) variable from a categorical variable
gen female_head=.
replace female_head=1 if bi13==1 | bi13==3 | bi13==4
replace female_head=0 if bi13==2
	
	// Another way
	*gen female_head=(bi13==1 | bi13==3 | bi13==4) if !missing(bi13)

// Generating an indicator variable (with a specific condition to be satisfied)
gen cashround_yn=.
replace cashround_yn=1 if mr0>0
replace cashround_yn=0 if mr0==0
	
	// Another (more parsimonious) way
	*gen cashround_yn = (mr0>0) if !missing(mr0)

// Exporting regression results
	// Dependent variable: whether the household owns a savings account 
	reg savings_yn female_head
	reg savings_yn female_literate_yn
	reg savings_yn male_literate_yn
	
	local i = 1
	foreach xvar in female_head female_literate_yn male_literate_yn {
		eststo s6_reg1_`i': reg savings_yn `xvar'  
		su savings_yn if `xvar'==0 
		estadd scalar controlmean = r(mean)
		estadd scalar controlsd = r(sd)
		local i = `i' + 1
	}
		
		// FYI, the foreach loop above is mechanically running the folloinwing 12 lines of code. 
		/*
		eststo s6_reg1_1: reg savings_yn female_head  
		su savings_yn if female_head==0 
		estadd scalar controlmean = r(mean)
		estadd scalar controlsd = r(sd)
		
		eststo s6_reg1_2: reg savings_yn female_literate_yn  
		su savings_yn if female_literate_yn==0 
		estadd scalar controlmean = r(mean)
		estadd scalar controlsd = r(sd)	
		
		eststo s6_reg1_3: reg savings_yn male_literate_yn  
		su savings_yn if male_literate_yn==0 
		estadd scalar controlmean = r(mean)
		estadd scalar controlsd = r(sd)
		*/
		
	estout s6_reg1_* using "Section6_table1_raw.csv", replace style(tab) delimiter(",") mlabel(,depvars) collabel(none) label ///
		drop(_cons) stats(controlmean controlsd N, labels("Control mean" "Control SD" "Obs") fmt(%9.2f %9.2f %9.0f)) starlevels(* 0.10 ** 0.05 *** 0.01) ///
		cells("b(fmt(%9.2f) star)" "se(fmt(%9.2f) par)") 

	// Dependent variable: whether household took out a loan from formal source	
	gen loan_formal=cr12

	local i = 1
	foreach XXX in female_head female_literate_yn male_literate_yn {
		eststo s6_reg2_`i': reg loan_formal `XXX'  
		su loan_formal if `XXX'==0 
		estadd scalar controlmean = r(mean)
		estadd scalar controlsd = r(sd)
		local i = `i' + 1
	}

	estout s6_reg2_* using "Section6_table2_raw.csv", replace style(tab) delimiter(",") mlabel(,depvars) collabel(none) label ///
		drop(_cons) stats(controlmean controlsd N, labels("Control mean" "Control SD" "Obs") fmt(%9.2f %9.2f %9.0f)) starlevels(* 0.10 ** 0.05 *** 0.01) ///
		cells("b(fmt(%9.2f) star)" "se(fmt(%9.2f) par)") 
		
		
// Merging two datasets
use "Uganda_baseline.dta", clear

merge 1:1 hhid using "Uganda_followup.dta"
tab _merge
gen in_followup = (_merge==3 | _merge==2) if !mi(_merge)
drop _merge

merge 1:1 hhid using "Uganda_bankinfo.dta"	
tab _merge
gen in_bankinfo = (_merge==3 | _merge==2) if !mi(_merge)
drop _merge

// Attrition
gen attrition = 1 - in_followup
reg attrition treatment	


exit


