* Change working directory
cd "/Users/DSP/Dropbox/UCSC (2016- )/1stYear_2Q/211B/Homeworks/HW5"

* NHIS data: CSV to DTA (+ misc. cleansing)
import delimited "NHIS Data.csv", clear
gen age = 21 + days_21/365
gen MLDA = 0
replace MLDA = 1 if age >= 21
save "211b_hw5_NHIS_data.dta", replace

* In addition, drop extreme observations
use "211b_hw5_NHIS_data", clear
drop if age>30 | age<18
save "211b_hw5_NHIS_data_trimmed.dta", replace

* CSV to DTA: mortality data (+ misc. cleansing)
import delimited "mortality data.csv", clear
gen MLDA = 0
replace MLDA = 1 if age >= 21
save "211b_hw5_mortality_data.dta", replace


*************************************** Problem 1 ***************************************
	
* Bin size: (consider only 1,2,3,4,6,8,12-twelfth of an year)
local list = "1 2 3 4 6 8 12"
foreach i of local list {
	use "211b_hw5_NHIS_data_trimmed", clear
	gen age_cell_`i' = autocode(age,14*12/`i',17,31)
	replace age_cell_`i' = age_cell_`i' - 1/24*`i'
	collapse (mean) drinks_alcohol, by(age_cell_`i')
	twoway (scatter drinks_alcohol age_cell_`i',msize(small)), xline(21) ytitle(Avg. Prob. of drinking alcohol) xtitle(Age) xscale(range(17 31)) xlabel(17(1)31) yscale(range(0 1)) ylabel(0(0.1)1) graphregion(fcolor(white) lcolor(white))
	graph export age_cell_`i'.png, replace
	}

	
*************************************** Problem 2 ***************************************

* Estimation with different order polynomials + superimposing fitted values to scatter plot
set more off
use "211b_hw5_NHIS_data_trimmed", clear
mat T = J(5,3,.)
forval i = 1/5 {
	quietly rdrobust drinks_alcohol age, c(21) kernel(tri) deriv(0) p(`i') bwselect(mserd)
	mat T[`i',1] = e(tau_cl)
	mat T[`i',2] = e(se_tau_cl) 
	mat T[`i',3] = e(h_l)
	rdplot drinks_alcohol age, c(21) kernel(tri) deriv(0) p(`i') nbins(9 27) graph_options(title("") legend(off) xline(21) ytitle(Avg. Prob. of drinking alcohol) xtitle(Age) xscale(range(17 31)) xlabel(17(1)31) yscale(range(0 1)) ylabel(0(0.1)1) graphregion(fcolor(white) lcolor(white)))
	graph export 211b_hw5_p2_rdplot_`i'_t.png, replace
	}
mat rownames T = "1st order" "2nd order" "3rd order" "4th order" "5th order"
mat colnames T = "coefficient" "standard error" "bandwidth" 
frmttable using "211b_hw5_p2_t.tex",statmat(T) tex fr nocenter sdec(2,2,2,3) replace 


*************************************** Problem 3 ***************************************

* Robustness check of bandwidth selection (2nd polynomial)
set more off
use "211b_hw5_NHIS_data_trimmed", clear
gen bandwidth=.
gen rd_est = .
gen rd_ci_l = .
gen rd_ci_h = .
local i = 1
forval h = 0.1(0.1)3 {
	quietly rdrobust drinks_alcohol age, c(21) kernel(tri) deriv(0) p(2) h(`h')
	replace bandwidth = `h' in `i'
	replace rd_est = e(tau_cl) in `i'
	replace rd_ci_l = e(tau_cl) - 1.96 * e(se_tau_cl) in `i'
	replace rd_ci_h = e(tau_cl) + 1.96 * e(se_tau_cl) in `i'
	local i = `i' + 1
	}
twoway (scatter rd_est bandwidth, msize(small)) (rcap rd_ci_h rd_ci_l bandwidth), ytitle(RD estimate) ylabel(none) yscale(range(0 0.5)) xtitle(bandwidth) xscale(range(0 3)) xlabel(0(0.5)3) ylabel(0(0.1)0.5) legend(off) graphregion(fcolor(white) lcolor(white))
graph export 211b_hw5_p3_bandwidth.png, replace
 

*************************************** Problem 4 ***************************************

* Visual inspection via plots
set more off
use "211b_hw5_NHIS_data_trimmed", replace
local x_list = "hs_diploma hispanic white black uninsured employed married working_lw going_school male"
local list = "4"
foreach x of local x_list{
	rdplot `x' age, c(21) kernel(tri) deriv(0) p(2) nbins(9 27) graph_options(title("") legend(off) xline(21) ytitle(`x') xtitle(Age) xscale(range(17 31)) xlabel(17(1)31) yscale(range(0 1)) ylabel(0(0.1)1) graphregion(fcolor(white) lcolor(white)))
	graph export 211b_hw5_p4_`x'.png, replace
	}
	
* Making table
set more off
use "211b_hw5_NHIS_data_trimmed", replace
mat T = J(10,3,.)
local i = 1
local x_list = "hs_diploma hispanic white black uninsured employed married working_lw going_school male"
foreach x of local x_list {
	quietly rdrobust `x' age, c(21) kernel(tri) deriv(0) p(2) bwselect(mserd)
	mat T[`i',1] = e(tau_cl)
	mat T[`i',2] = e(se_tau_cl)
	mat T[`i',3] = ttail(e(N), abs(e(tau_cl)/e(se_tau_cl)))*2
	local i = `i' + 1
 	}
mat rownames T = hs_diploma hispanic white black uninsured employed married working_lw going_school male
mat colnames T = "RD estimate" "standard error" "p-value"
frmttable using "211b_hw5_p4_cov.tex",statmat(T) tex fr nocenter sdec(2,2,2,3) replace 


*************************************** Problem 5 ***************************************

* Density histogram
use "211b_hw5_NHIS_data_trimmed", replace
twoway (histogram age, width(0.5))(scatteri 0 21 .1 21, c(l) m(i) lcolor(maroon)),ytitle(Density) xtitle(Age) xscale(range(17 31)) xlabel(17(1)31) legend(off) graphregion(fcolor(white) lcolor(white))
graph export 211b_hw5_p5_histogram.png, replace


*************************************** Problem 7 ***************************************

* Scatter plot of deaths due to all causes
use "211b_hw5_mortality_data", replace
twoway (scatter all age,msize(small)), xline(21) ytitle("Death rate per 100,000 person years") xtitle(Age) xscale(range(19 23)) xlabel(19(0.5)23) graphregion(fcolor(white) lcolor(white))
graph export death_all.png, replace

* RD estimation
set more off
use "211b_hw5_mortality_data", replace
mat T = J(2,4,.)
forval i = 1/2 {
	quietly rdrobust all age, c(21) kernel(tri) deriv(0) p(`i') bwselect(mserd)
	mat T[`i',1] = e(tau_cl)
	mat T[`i',2] = e(se_tau_cl) 
	mat T[`i',3] = ttail(e(N), abs(e(tau_cl)/e(se_tau_cl)))*2
	mat T[`i',4] = e(h_l)
	rdplot all age, c(21) kernel(tri) deriv(0) p(`i') nbins(24 24) graph_options(title("") legend(off) xline(21) ytitle(Avg. Prob. of drinking alcohol) xtitle(Age) xscale(range(19 23)) xlabel(19(0.5)23) graphregion(fcolor(white) lcolor(white)))
	graph export 211b_hw5_p7_rdplot_all_`i'.png, replace
	}
mat rownames T = "1st order" "2nd order"
mat colnames T = "RD estimate" "standard error" "p-value" "bandwidth"
frmttable using "211b_hw5_p7_all.tex",statmat(T) tex fr nocenter sdec(2,2,2,3) replace 


*************************************** Problem 8 ***************************************

* Scatter plot of deaths due to MVA causes
use "211b_hw5_mortality_data", clear
twoway (scatter mva age,msize(small)), xline(21) ytitle("Death rate per 100,000 person years") xtitle(Age) xscale(range(19 23)) xlabel(19(0.5)23) graphregion(fcolor(white) lcolor(white))
graph export death_mva.png, replace

* RD estimation
set more off
use "211b_hw5_mortality_data", clear
mat T = J(2,4,.)
forval i = 1/2 {
	quietly rdrobust mva age, c(21) kernel(tri) deriv(0) p(`i') bwselect(mserd)
	mat T[`i',1] = e(tau_cl)
	mat T[`i',2] = e(se_tau_cl) 
	mat T[`i',3] = ttail(e(N), abs(e(tau_cl)/e(se_tau_cl)))*2
	mat T[`i',4] = e(h_l)
	rdplot mva age, c(21) kernel(tri) deriv(0) p(`i') nbins(24 24) graph_options(title("") legend(off) xline(21) ytitle(Avg. Prob. of drinking alcohol) xtitle(Age) xscale(range(19 23)) xlabel(19(0.5)23) graphregion(fcolor(white) lcolor(white)))
	graph export 211b_hw5_p8_rdplot_mva_`i'.png, replace
	}
mat rownames T = "1st order" "2nd order"
mat colnames T = "RD estimate" "standard error" "p-value" "bandwidth"
frmttable using "211b_hw5_p7_all.tex",statmat(T) tex fr nocenter sdec(2,2,2,3) replace 

*************************************** Problem 9 ***************************************

* Robustness check of bandwidth selection for ALL (2nd polynomial)
set more off
use "211b_hw5_mortality_data", replace
gen bandwidth=.
gen rd_est = .
gen rd_ci_l = .
gen rd_ci_h = .
local i = 1
forval h = 0.4(0.1)3 {
	quietly rdrobust all age, c(21) kernel(tri) deriv(0) p(2) h(`h')
	replace bandwidth = `h' in `i'
	replace rd_est = e(tau_cl) in `i'
	replace rd_ci_l = e(tau_cl) - 1.96 * e(se_tau_cl) in `i'
	replace rd_ci_h = e(tau_cl) + 1.96 * e(se_tau_cl) in `i'
	local i = `i' + 1
	}
twoway (scatter rd_est bandwidth, msize(small)) (rcap rd_ci_h rd_ci_l bandwidth), ytitle(RD estimate) xtitle(bandwidth) xscale(range(0 2)) xlabel(0(0.5)3) legend(off) graphregion(fcolor(white) lcolor(white))
graph export 211b_hw5_p9_bandwidth_all.png, replace

* Robustness check of bandwidth selection for MVA (2nd polynomial)
set more off
use "211b_hw5_mortality_data", replace
gen bandwidth=.
gen rd_est = .
gen rd_ci_l = .
gen rd_ci_h = .
local i = 1
forval h = 0.4(0.1)3 {
	quietly rdrobust mva age, c(21) kernel(tri) deriv(0) p(2) h(`h')
	replace bandwidth = `h' in `i'
	replace rd_est = e(tau_cl) in `i'
	replace rd_ci_l = e(tau_cl) - 1.96 * e(se_tau_cl) in `i'
	replace rd_ci_h = e(tau_cl) + 1.96 * e(se_tau_cl) in `i'
	local i = `i' + 1
	}
twoway (scatter rd_est bandwidth, msize(small)) (rcap rd_ci_h rd_ci_l bandwidth), ytitle(RD estimate) xtitle(bandwidth) xscale(range(0 2)) xlabel(0(0.5)3) legend(off) graphregion(fcolor(white) lcolor(white))
graph export 211b_hw5_p9_bandwidth_mva.png, replace

*************************************** Problem 10 ***************************************

* Settings for output matrix
mat T = J(2,5,.)
mat colnames T = "coefficient" "standard error" "p-value" "observations" "R-squared"
mat rownames T = "all causes" "MVA" 

* MLDA on Deaths (All causes)
use "211b_hw5_mortality_data", replace
gen age_c = age - 21
reg all MLDA age_c c.age_c#c.age_c c.age_c#i.MLDA c.age_c#c.age_c#i.MLDA if abs(age_c) <= 1.39
mat T[1,1] = _b[MLDA]
mat T[1,2] = _se[MLDA] 
mat T[1,3] = ttail(e(N), abs(_b[MLDA]/_se[MLDA]))*2
mat T[1,4] = e(N)
mat T[1,5] = e(r2)

* MLDA on Deaths (MVA)
use "211b_hw5_mortality_data", replace
gen age_c = age - 21
reg mva MLDA age_c c.age_c#c.age_c c.age_c#i.MLDA c.age_c#c.age_c#i.MLDA if abs(age_c) <= 1.39
mat T[2,1] = _b[MLDA]
mat T[2,2] = _se[MLDA] 
mat T[2,3] = ttail(e(N), abs(_b[MLDA]/_se[MLDA]))*2
mat T[2,4] = e(N)
mat T[2,5] = e(r2)

* Export table to LaTeX
frmttable using "211b_hw5_p10.tex",statmat(T) tex fr nocenter sdec(3,3,3,0,3) replace 


*************************************** Problem 11 ***************************************

* Settings for output matrix
mat T = J(5,3,.)
mat rownames T = "Coefficient" "standard error" "p-value" "observations" "R-squared"
mat colnames T = "First stage" "Reduced form" "2SLS"

****** using 2SLS ******

* First stage
use "211b_hw5_NHIS_data_trimmed", replace
gen age_c = age - 21
reg drinks_alcohol MLDA age_c c.age_c#c.age_c c.age_c#i.MLDA c.age_c#c.age_c#i.MLDA if abs(age_c) <= 1.39
mat T[1,1] = _b[MLDA]
mat T[2,1] = _se[MLDA] 
mat T[3,1] = ttail(e(N), abs(_b[MLDA]/_se[MLDA]))*2
mat T[4,1] = e(N)
mat T[5,1] = e(r2)

* Reduced form
use "211b_hw5_mortality_data", replace
gen age_c = age - 21
reg mva MLDA age_c c.age_c#c.age_c c.age_c#i.MLDA c.age_c#c.age_c#i.MLDA if abs(age_c) <= 1.39
mat T[1,2] = _b[MLDA]
mat T[2,2] = _se[MLDA] 
mat T[3,2] = ttail(e(N), abs(_b[MLDA]/_se[MLDA]))*2
mat T[4,2] = e(N)
mat T[5,2] = e(r2)

* 2SLS = RF/FS
mat T[1,3] = T[1,2] / T[1,1]

* SE for 2SLS estimator (Delta method)
mat T[2,3] = sqrt(1/(T[1,1])^2 *(T[2,2])^2 + (T[1,2])^2/(T[1,1])^4 *(T[2,1])^2)

* Export output matrix to LaTeX
frmttable using "211b_hw5_p11_2sls_2.tex",statmat(T) tex fr nocenter sdec(3\3\3\0\3) replace 


****** using Fuzzy RD ******

* Adjustments for merging
use "211b_hw5_NHIS_data_trimmed", clear
replace age = round(age, 0.001)
collapse (mean) drinks_alcohol, by(age)
save "211b_hw5_p11_FS", replace

use "211b_hw5_mortality_data", clear
replace age = round(age, 0.001)
save "211b_hw5_p11_RF", replace

* Merge dataset for FRD
use "211b_hw5_p11_FS", clear
merge 1:1 age using "211b_hw5_p11_RF.dta"
keep if _merge == 3 
save "211b_hw5_p11_FRD", replace

* Fuzzy RD
use "211b_hw5_p11_FRD", clear
rdrobust mva age, c(21) kernel(tri) deriv(0) p(2) bwselect(mserd) fuzzy(drinks_alcohol)
mat T[1,4] = e(tau_cl)



