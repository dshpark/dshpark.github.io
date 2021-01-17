* ---------------------------- *
* Econ217 (J. Li, W2021)       *
* Section 2 (Jan. 13-15, 2021) * NOTE: all the path directories are based on my own computer, 
* TA: David Sungho Park        *       so you should change them according to yours.
* ---------------------------- *
clear all
set more off

cd "/Users/DSP/Google Drive/UCSC/Teaching/Econ217_W21/SectionMaterial/"

use "Data Sets/BWGHT.dta", clear

gen bwght_verylow = bwght<50
gen bwght_low = (bwght>=50 & bwght<100)
gen bwght_normal = bwght>=100 

su bwght, d
gen bwght_abovemed = bwght>`r(p50)'

// Logit regression (taking binary var <bwght_low> as dep. var)
	// Basic model
	logit bwght_low cigs faminc 

	// Marginal effects at mean (MEM)
	margins, dydx(*) at((mean) cigs faminc)
	margins, dydx(*) atmeans
	
	// Average marginal effects (AME)
	margins, dydx(*)

// Probit regression	
	// Basic model
	probit bwght_low cigs faminc 	

	// MEM
	margins, dydx(*) at((mean) cigs faminc)
	
	// AME
	margins, dydx(*)


// Multinomial logit 
use "Data Sets/keane.dta", clear

mlogit choice educ exper, baseoutcome(1)
	
	// AME
	margins, dydx(*) 

	margins, dydx(*) predict(outcome(1))
	margins, dydx(*) predict(outcome(2))

exit


