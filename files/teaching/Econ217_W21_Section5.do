* ---------------------------- *
* Econ217 (J. Li, W2021)       *
* Section 5 (Feb. 3-5, 2021)   * NOTE: all the path directories are based on my own computer, 
* TA: David Sungho Park        *       so you should change them according to yours.
* ---------------------------- *
clear all
set more off

cd "/Users/DSP/Google Drive/UCSC/Teaching/Econ217_W21/SectionMaterial/"
global raw_folder "tex_raw"

// Spline interpolation
webuse ipolxmpl2, clear
*ssc install csipolate // you need to do this only once
keep if magazine=="mag0" // to keep it simple
browse

csipolate circ year, gen(csicirc) 	// cubic spline interpolation
ipolate circ year, gen(icirc)		// linear spline interpolation

	// investigating differences b/w cubic & linear
	gen diff = csicirc-icirc
	gen diff_perc = (csicirc-icirc)/icirc
	su diff_perc if mi(circ), d
	kdensity diff_perc if mi(circ)


// Tobit Type 1 (Note:  specifications below are different from the assignment)
use "Data Sets/APPLE.dta", clear
gen lecoprc = log(ecolbs)
tobit ecolbs lecoprc faminc, ll(0)
margins, dydx(*) predict(ystar(0,.))


// Tobit Type 2 (Heckman)
webuse womenwk, clear
heckman wage educ age, select(married children educ age) 
margins, dydx(*) predict(yexpected)

	// To be more explicit 
	gen wage_observed = !missing(wage)
	heckman wage educ age, select(wage_observed = married children educ age) 


// Nonparametric local linear regression (runs in Stata 15 or above)
use "Data Sets/APPLE.dta", clear
 
npregress kernel ecolbs ecoprc, reps(500) seed(10)
margins, dydx(*) atmeans reps(500) seed(10)


// Plotting estimates from kernel local linear regression
winsor2 ecolbs, cuts(0 99) replace
lpoly ecolbs ecoprc, degree(1) ci


// Kernel density plot
use "Data Sets/APPLE.dta", clear

kdensity ecolbs
kdensity ecolbs, kernel(gaussian)
kdensity ecolbs, kernel(epanechnikov) bwidth(3) 


// Spline regression 
use "Data Sets/APPLE.dta", clear

	// Linear spline
	mkspline agesp_l 5 = age, displayknots // equally-spaced
	reg faminc educ agesp_l1 agesp_l2 agesp_l3 agesp_l4 agesp_l5
		// reg faminc educ agesp_l? 
	
		// to have coeffs represent change in slope from preceding interval 
		mkspline agesp_lm 5 = age, marginal displayknots 
		reg faminc educ agesp_lm?

	// Natural cubic spline
	mkspline agesp_c = age, cubic nknots(5) displayknots // using Stata's default spacing method
	reg faminc educ agesp_c*
	
	
exit

	
