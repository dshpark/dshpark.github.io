* ---------------------------- *
* Econ120 (J. Robinson, F2020) *
* Section 1 (Oct. 5, 2020)	   * NOTE: all the path directories are based on my own computer, 
* TA: David Sungho Park        *       so you should change them according to yours.
* ---------------------------- *
clear all
set more off

cd "/Users/DSP/Google Drive/UCSC/Teaching/Econ120_F20/Analysis/"

log using "doFiles/Econ120_F20_Section1.smcl"

use "dtaFiles/WDI.dta", clear

// Counting number of distinct observations
codebook countrycode // often "code" is preferrable to "name" as there could be name duplicates
distinct countrycode
*ssc install distinct // if "distinct" package is not already installed

// Summary statistics of variable(s)
summarize GDP_pc
su GDP_pc, detail // usually to get median or to detect outliers

// To get a rough distribution of a variable (in addition to comparing the mean vs. median)
histogram GDP_pc
kdensity GDP_pc

// Graphing scatter plots
scatter mort_rate_under5 GDP_pc  
graph export "output/scatter_under5_gdp.pdf", replace

// Tabulating summary statistics by group
su GDP_pc if region=="East Asia & Pacific"
su GDP_pc if region=="Europe & Central Asia"

tabstat GDP_pc, by(region)

// Generating new variables
generate lit_gender_gap_adult = lit_rate_fem_adult - lit_rate_male_adult

// Sorting observations by certain variable
browse countryname GDP_pc
	
	// "+" sign for ascending order
	sort GDP_pc
	gsort +GDP_pc
	gsort GDP_pc
	
	br countryname GDP_pc
	br countryname GDP_pc if _n<=50
	br countryname GDP_pc lit_rate_fem_adult mort_rate_under5 if _n<=50
	su lit_rate_fem_adult mort_rate_under5 if _n<=50
	
	// "-" for descending order
	gsort -GDP_pc

// Running regressions
regress lit_rate_fem_adult GDP_pc

gen GDP_pc_1000 = GDP_pc/1000
regress lit_rate_fem_adult GDP_pc_1000

// Reporting regression lines in graphs
	// Option 1
	twoway (scatter mort_rate_under5 GDP_pc) (lfit mort_rate_under5 GDP_pc)
	graph export "output/scatter_under5_gdp_lfit.pdf", replace
	
	// Option 2 (more flexible)
	reg mort_rate_under5 GDP_pc 
	predict mort_rate_under5_yhat
	twoway (scatter mort_rate_under5 GDP_pc) (line mort_rate_under5_yhat GDP_pc)
	graph export "output/scatter_under5_gdp_pred.pdf", replace
	
		// Example: squared terms
		gen GDP_pc_2 = GDP_pc^2
		reg mort_rate_under5 GDP_pc GDP_pc_2
		predict mort_rate_under5_yhat2
		twoway (scatter mort_rate_under5 GDP_pc) (line mort_rate_under5_yhat2 GDP_pc)
		graph export "output/scatter_under5_gdp_pred2.pdf", replace
	
log close
	
exit
exit
exit
exit
exit
