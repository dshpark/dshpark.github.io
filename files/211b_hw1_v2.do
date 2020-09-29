*using semicolon as delimiter ("#delimit cr" to cancel)
#delimit

*opening file
use "/Users/DSP/Dropbox/UCSC (2016- )/1stYear_2Q/211B/Homeworks/cepr_org_2014_hw1.dta", replace

*Applying restrictions
keep if female==0 & age >=30 & age <=40 & month==1

*overview of education variable
tab educ92

*A 98% Winsorization
sum wage4, detail 
replace wage4 = r(p99) if wage4 > r(p99)
replace wage4 = r(p1) if wage4 < r(p1)

*Plotting
scatter wage4 educ92, jitter(7) msize(small)
graph export "/Users/DSP/Dropbox/UCSC (2016- )/1stYear_2Q/211B/Homeworks/211b_hw1_fig1_scatter.png"

*checking the label values of educ92
label list educ92

*generating dummy variables
forval i=1/16 {
	gen educ_`i' = 0
	replace educ_`i' = 1 if educ92==`i' 
	}
exit

*Estimating CEF via OLS
reg wage4 educ_*, noconstant

*Saving the fitted values
predict fitted

*Exporting to tex
outreg2 using reg1_1, tex(fr)

*Plotting the fitted values
twoway (scatter wage4 educ92, jitter(7) msize(small))(scatter fitted educ92, msize(small) mcolor(cranberry))
graph export "/Users/DSP/Dropbox/UCSC (2016- )/1stYear_2Q/211B/Homeworks/211b_hw1_fig2_fitted.png"


