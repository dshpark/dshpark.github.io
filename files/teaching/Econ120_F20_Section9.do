* ---------------------------- *
* Econ120 (J. Robinson, F2020) *
* Section 9 (Nov. 30, 2020)	   * NOTE: all the path directories are based on my own computer, 
* TA: David Sungho Park        *       so you should change them according to yours.
* ---------------------------- *

clear all
set more off

cd "/Users/DSP/Google Drive/UCSC/Teaching/Econ120_F20/SectionNotes/Section9/"

// Merging *panel* datasets
use "WDI_series_working.dta", clear
merge 1:1 countryname year using "Polity_IV.dta"
	
	// Visually investigating unmatched country names
	tab countryname if _m==1
	tab countryname if _m==2
	
		// ... and making manual fixes. For example, 
		use "WDI_series_working.dta", clear
		replace countryname = "East Timor" if countryname=="Timor-Leste"
	
	// FYI (with caution): a Stata package for working with country names/codes
	ssc install kountry
	
		// Matching to WDI's country coding
		use "Polity_IV.dta", clear
		kountry scode, from(cowc) to(iso3c)
		rename _ISO3C_ countrycode
		drop if mi(countrycode)
		
		merge 1:1 countrycode year using "WDI_series_working.dta"
		
		// Alternatively, matching to Polity IV's country coding
		use "WDI_series_working.dta", clear
		kountry countrycode, from(iso3c) to(cowc) 
		rename _COWC_ scode
		drop if mi(scode)
		
		merge 1:1 scode year using "Polity_IV.dta"

		// ... but you still need to visually investigate and make manual fixes
		tab countryname if _m==1
		tab countryname if _m==2
		
		use "Polity_IV.dta", clear
		replace countryname="Timor-Leste" if countryname=="East Timor"

	
// anyways, for now let's just be satisfied with the following
use "WDI_series_working.dta", clear
merge 1:1 countryname year using "Polity_IV.dta"
	
// Subgroup mean
replace democ=. if democ<0
egen democ_countrymean = mean(democ), by(countrycode)	

// Visual fitting with a locally weighted regression 
lowess gdp_per_capita democ if year==2013
	
	// Similar but "smoother" than this:
	twoway (scatter gdp_per_capita democ) (lfit gdp_per_capita democ)  if year==2013

// Regressions with clustered standard errors
reg gdp_per_capita democ
reg gdp_per_capita democ, cluster(countrycode)

// Controlling for categorical variables (aka. fixed effects)
xi: reg gdp_per_capita democ i.income_group, cluster(countrycode)

	// Or this way:
	egen income_group_id = group(income_group)
	reg gdp_per_capita democ i.income_group_id, cluster(countrycode)
	
	
exit	



