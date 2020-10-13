* Using semicolon as delimiter ("#delimit cr" to cancel)
#delimit

* Change working directory
cd "/Users/DSP/Dropbox/UCSC (2016- )/1stYear_2Q/211B/Homeworks/HW2"

* Import CSV file
import delimited "NSW_PSID_Lalonde.csv"

* Save as DTA file
save "211b_hw2_NSW_PSID.dta", replace

*************************************** Problem 1 ***************************************
use "211b_hw2_NSW_PSID.dta", replace
keep if data_id=="Lalonde Sample"

* Creating an 8x4 matrix
mat T = J(8,4,.)

* Filling in the matrix
local i = 1
local list "age education black hispanic married nodegree re75 re78"
foreach x of local list {
	ttest `x', by(treat)
	mat T[`i',1] = r(mu_2)
	mat T[`i',2] = r(mu_1)
	mat T[`i',3] = r(mu_2) - r(mu_1)
	mat T[`i',4] = r(p)
	local i = `i' + 1
 	}

* Naming rows/columns
mat rownames T = age education black hispanic married nodegree re75 re78 
mat colnames T = "Treatment group" "Control group" "Difference" "p-value"

* Exporting to LaTeX
frmttable using "mean_diff.tex",statmat(T) tex fr nocenter sdec(2,2,2,3) replace 


*************************************** Problem 2 ***************************************
use "211b_hw2_NSW_PSID.dta", replace
keep if data_id=="Lalonde Sample"

* Raw difference
reg re78 treat
outreg2 using "reg1.tex", tex(fr) replace se bdec(1) sdec(1) noaster nonote ctitle(" ")

* Adding estimation results sequentially
local list "age education black hispanic married nodegree"
foreach x of local list {
	reg re78 treat-`x'
	outreg2 using "reg1.tex", tex(fr) append se bdec(1) sdec(1) noaster nonote ctitle(" ")
	}

* Diff-in-Diff (DD)
gen diff = re78 - re75
reg diff treat
outreg2 using "reg1.tex", tex(fr) append se bdec(1) sdec(1) noaster nonote ctitle("DD")

* Alternative: DD estimation (by def) 
su re78 if treat==1
gen re78_T = r(mean)
su re75 if treat==1
gen re75_T = r(mean)
su re78 if treat==0
gen re78_C = r(mean)
su re75 if treat==0
gen re75_C = r(mean)
gen DD = (re78_T - re75_T)-(re78_C-re75_C)
di DD


*************************************** Problem 4 ***************************************
use "211b_hw2_NSW_PSID.dta", replace
keep if data_id=="Lalonde Sample"

* Kernel density plots
gen earnings_1978_t = treat*re78
gen earnings_1978_c = (1-treat)*re78
replace earnings_1978_t =. if earnings_1978_t ==0
replace earnings_1978_c =. if earnings_1978_c ==0

twoway (kdensity earnings_1978_t, kernel(triangle))(kdensity earnings_1978_c, kernel(triangle)), legend(on order(1 "Treatment group" 2 "Control group"))
graph export "kernel_density_.png", replace


*************************************** Problem 5 ***************************************
use "211b_hw2_NSW_PSID.dta", replace
drop if data_id=="Lalonde Sample" & treat==0

* Creating an 8x4 matrix
mat T = J(8,4,.)

* Filling in the matrix
local i = 1
local list "age education black hispanic married nodegree re75 re78"
foreach x of local list {
	ttest `x', by(treat)
	mat T[`i',1] = r(mu_2)
	mat T[`i',2] = r(mu_1)
	mat T[`i',3] = r(mu_2) - r(mu_1)
	mat T[`i',4] = r(p)
	local i = `i' + 1
 	}

* Naming rows/columns
mat rownames T = age education black hispanic married nodegree re75 re78
mat colnames T = "Treatment (NSW)" "Comparison (PSID)" "Difference" "p-value"

* Exporting to LaTeX
frmttable using "mean_diff_PSID.tex",statmat(T) tex fr nocenter sdec(2,2,2,3) replace 


*************************************** Problem 6 ***************************************
use "211b_hw2_NSW_PSID.dta", replace
drop if data_id=="Lalonde Sample" & treat==0

* Raw difference
reg re78 treat
outreg2 using "reg2.tex", tex(fr) replace se bdec(1) sdec(1) noaster nonote ctitle(" ")

* Adding estimation results sequentially
local list "age education black hispanic married nodegree"
foreach x of local list {
	reg re78 treat-`x'
	outreg2 using "reg2.tex", tex(fr) append se bdec(1) sdec(1) noaster nonote ctitle(" ")
	}

* Diff-in-Diff (DD)
gen diff = re78 - re75
reg diff treat
outreg2 using "reg2.tex", tex(fr) append se bdec(1) sdec(1) noaster nonote ctitle("DD")


*************************************** Problem 8 ***************************************
use "211b_hw2_NSW_PSID.dta", replace
drop if data_id=="Lalonde Sample" & treat==0

* Estimating p-score using logit and polynomials and interaction terms
logit treat age-nodegree c.age#(c.age c.education black hispanic married nodegree) c.education#(c.education black hispanic married nodegree) black#(hispanic married nodegree) hispanic#(married nodegree) married#nodegree
predict pscore, pr

* Plotting histograms
histogram pscore if treat==1, fcolor(bluishgray) lcolor(black)
graph export "pscore_PSID_t.png", replace

histogram pscore if treat==0, fcolor(bluishgray) lcolor(black)
graph export "pscore_PSID_c.png", replace

* Plotting overlap (before trimming)
twoway (histogram pscore if treat==0, color(cranberry))(histogram pscore if treat==1, fcolor(none) lcolor(black)), legend(order(1 "Comparison" 2 "Treatment"))
graph export "pscore_PSID_overlap.png", replace

* Trimming by "0.1 Rule"
keep if pscore > 0.1 & pscore < 0.9

twoway (histogram pscore if treat==0, color(cranberry))(histogram pscore if treat==1, fcolor(none) lcolor(black)), legend(order(1 "Comparison" 2 "Treatment"))
graph export "pscore_overlap_PSID_1-9.png", replace

mat T = J(8,4,.)
local i = 1
local list "age education black hispanic married nodegree re75 re78"
foreach x of local list {
	ttest `x', by(treat)
	mat T[`i',1] = r(mu_2)
	mat T[`i',2] = r(mu_1)
	mat T[`i',3] = r(mu_2) - r(mu_1)
	mat T[`i',4] = r(p)
	local i = `i' + 1
 	}
mat rownames T = age education black hispanic married nodegree re75 re78
mat colnames T = "Treatment (NSW)" "Comparison (PSID)" "Difference" "p-value"
mat list T
frmttable using "mean_diff_PSID_1-9.tex",statmat(T) tex fr nocenter sdec(2,2,2,3) replace 

* Trimming by "0.2 Rule"
keep if pscore > 0.2 & pscore < 0.8

twoway (histogram pscore if treat==0, color(cranberry))(histogram pscore if treat==1, fcolor(none) lcolor(black)), legend(order(1 "Comparison" 2 "Treatment"))
graph export "pscore_overlap_PSID_2-8.png", replace

mat T = J(8,4,.)
local i = 1
local list "age education black hispanic married nodegree re75 re78"
foreach x of local list {
	ttest `x', by(treat)
	mat T[`i',1] = r(mu_2)
	mat T[`i',2] = r(mu_1)
	mat T[`i',3] = r(mu_2) - r(mu_1)
	mat T[`i',4] = r(p)
	local i = `i' + 1
 	}
mat rownames T = age education black hispanic married nodegree re75 re78
mat colnames T = "Treatment (NSW)" "Comparison (PSID)" "Difference" "p-value"
frmttable using "mean_diff_PSID_2-8.tex",statmat(T) tex fr nocenter sdec(2,2,2,3) replace 


*************************************** Problem 9 ***************************************
use "211b_hw2_NSW_PSID.dta", replace
drop if data_id=="Lalonde Sample" & treat==0
logit treat age-nodegree c.age#(c.age c.education black hispanic married nodegree) c.education#(c.education black hispanic married nodegree) black#(hispanic married nodegree) hispanic#(married nodegree) married#nodegree
predict pscore, pr
keep if pscore > 0.2 & pscore < 0.8

* Estimation
reg re78 treat
outreg2 using "reg3.tex", tex(fr) replace se bdec(1) sdec(1) noaster nonote ctitle(" ")

local list "age education black hispanic married nodegree"
foreach x of local list {
	reg re78 treat-`x'
	outreg2 using "reg3.tex", tex(fr) append se bdec(1) sdec(1) noaster nonote ctitle(" ")
	}

gen diff = re78 - re75
reg diff treat
outreg2 using "reg3.tex", tex(fr) append se bdec(1) sdec(1) noaster nonote ctitle("DD")



