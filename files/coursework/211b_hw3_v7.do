* Change working directory
cd "/Users/DSP/Dropbox/UCSC (2016- )/1stYear_2Q/211B/Homeworks/HW3"

* Open original DTA file
use "meth_otc", replace

* Generate date variables
gen MMM = date(event_date,"DMY")
gen NNN = date(any_law,"MDY")
drop event_date any_law

* Calculate days in month
gen dd = day(MMM)
gen mm = month(MMM)
gen yyyy = year(MMM)
gen mm1=mm+1 if mm<12
replace mm1=1 if mm==12
replace yyyy=yyyy+1 if mm==12
gen dpm = day(mdy(mm1,1,yyyy)-1)
drop dd mm mm1 yyyy 

* Generate OTC variable
gen OTC = 0
replace OTC = (MMM - NNN)/dpm if mofd(MMM)==mofd(NNN) & MMM > NNN
replace OTC = 1 if mofd(MMM)!= mofd(NNN) & MMM > NNN
drop dpm

* Clean up date variables
gen event_date = mofd(MMM)
gen any_law = mofd(NNN)
format %tm event_date any_law
drop MMM NNN

* Arrange ID variable for panel
encode state_ab, gen(state)
drop state_ab

* Declare panel dataset
xtset state event_date, monthly

* Clean up dummies
gen cap_9_oz_or_more = cap_9_oz_1_lb + cap_2_9_lb + cap_10_19_lb + cap_20_lb_or_greater
drop cap_9_oz_1_lb cap_2_9_lb cap_10_19_lb cap_20_lb_or_greater

* Order variables (for convenience)
order state event_date any_law tot_labs cap_under_2_oz cap_2_8_oz cap_9_oz_or_more OTC, first

* Save file
save "211b_hw3_meth_otc.dta", replace


*************************************** Problem 1 ***************************************
use "211b_hw3_meth_otc", replace

* First difference variables
local list "tot_labs OTC cov_unemp_rate cov_food__st_person"
foreach x of local list {
	gen `x'_d = `x' - `x'[_n-1]
 	}

* Time period of our interest
global ttt = 552

* Within estimator
xtreg tot_labs OTC cov_unemp_rate cov_food__st_person if event_date==$ttt-1 | event_date==$ttt, fe
outreg2 using "hw3_reg1.tex", tex(fr) replace se bdec(3) sdec(1) noaster nonote ctitle("Within")

* First difference estimator
reg tot_labs_d OTC_d cov_unemp_rate_d cov_food__st_person_d if event_date==$ttt, noconstant
outreg2 using "hw3_reg1.tex", tex(fr) append se bdec(3) sdec(1) noaster nonote ctitle("First Difference")


*************************************** Problem 2 ***************************************
use "211b_hw3_meth_otc", replace

* Generate time-series of proportion of population in states with OTC
gen OTC_pop = OTC * pop_all_fitted
collapse (sum) OTC_pop (sum) pop_all_fitted (sum) cap_under_2_oz cap_2_8_oz cap_9_oz_or_more, by(event_date)
gen Fraction = OTC_pop/pop_all_fitted
save "211b_hw3_p2.dta", replace

* Plot
#delimit ;
twoway (connected cap_under_2_oz event_date, msize(vsmall) msymbol(square) yaxis(1)) 
(connected cap_2_8_oz event_date, msize(vsmall) msymbol(triangle) yaxis(1)) 
(connected cap_9_oz_or_more event_date, msize(vsmall) msymbol(circle) yaxis(1)) 
(line Fraction event_date, yaxis(2)), 
legend(on order(1 "Under 2 oz." 2 "2-8 oz." 3 "Over 8 oz." 4 "OTC law coverage")); 
#delimit cr
ytitle(No. of Labs) xtitle("")

graph export "time_series_plot.png", replace


*************************************** Problem 3 ***************************************
use "211b_hw3_meth_otc", replace

* Center time at 0
gen time = event_date - any_law

* Average number of labs
collapse (mean) cap_under_2_oz cap_2_8_oz cap_9_oz_or_more, by(time)

* Plot
#delimit ;
twoway (connected cap_under_2_oz time, msize(vsmall) msymbol(square)) 
(connected cap_2_8_oz time, msize(vsmall) msymbol(triangle)) 
(connected cap_9_oz_or_more time, msize(vsmall) msymbol(circle)), 
legend(on order(1 "Under 2 oz." 2 "2-8 oz." 3 "Over 8 oz.")) 
ytitle(Avg. No. of Labs) xtitle(time) xscale(range(-80 60)) xlabel(-50(50)50);
#delimit cr
graph export "avg_labs_plot.png", replace


*************************************** Problem 4 ***************************************
use "211b_hw3_meth_otc", replace

* Rename to table-friendly variable names
rename (cov_unemp_rate) (unemployment)
rename (cov_food__st_person) (food_stamp)

* FE estimation (normal s.e.)
order cap_9_oz_or_more cap_2_8_oz cap_under_2_oz tot_labs, before(OTC)
local y_list "tot_labs cap_under_2_oz cap_2_8_oz cap_9_oz_or_more"
local x_list "OTC unemployment food_stamp"
foreach y of local y_list {
	foreach x of local x_list{
	xtreg `y'-`x' i.state i.event_date
	outreg2 using "hw3_reg2_FE.tex", tex(fr) append se bdec(2) sdec(2) noaster nonote ctitle(" ") keep(OTC unemployment food_stamp) nocons
	}
	order `y', after(any_law)
 	}
	
* FE estimation (clustered s.e.)
order cap_9_oz_or_more cap_2_8_oz cap_under_2_oz tot_labs, before(OTC)
local y_list "tot_labs cap_under_2_oz cap_2_8_oz cap_9_oz_or_more"
local x_list "OTC unemployment food_stamp"
foreach y of local y_list {
	foreach x of local x_list{
	xtreg `y'-`x' i.state i.event_date, vce(cluster state)
	outreg2 using "hw3_reg3_FE_cluster.tex", tex(fr) append se bdec(2) sdec(2) noaster nonote ctitle(" ") keep(OTC unemployment food_stamp) nocons
	}
	order `y', after(any_law)
 	}
	
*************************************** Problem 7 ***************************************
use "211b_hw3_meth_otc", replace

* Center time at 0
gen time = event_date - any_law

* Generate event time dummies (positive)
forval i= 0/24 {
	gen pi_`i' = 0 
	replace pi_`i' = 1 if time==`i'
}

* Generate event time dummies (negative)
forval i= 1/12 {
	gen pi_n`i' = 0 
	replace pi_n`i' = 1 if time==-`i'
	order pi_n`i', after(time)
}

* Save file
save "211b_hw3_prob7.dta", replace


* Estimation (total labs)
use "211b_hw3_prob7", replace
reg tot_labs i.state i.event_date pi_*
regsave pi_*, ci
gen id = 0
forval i=1/37 {
	replace id = -13 + `i' in `i'
}
twoway (connected coef id, msize(small)) (rcap ci_upper ci_lower id), ytitle(No. of labs) ylabel(none) yscale(range(0 20)) xtitle(months relative to law enactment) xscale(range(-12 24)) xlabel(-12(6)24) ylabel(0(5)18) legend(off)
graph export "CI_plot_total.png", replace

* Estimation (under 2 oz)
use "211b_hw3_prob7", replace
reg cap_under_2_oz i.state i.event_date pi_*
regsave pi_*, ci
gen id = 0
forval i=1/37 {
	replace id = -13 + `i' in `i'
}
twoway (connected coef id, msize(small)) (rcap ci_upper ci_lower id), ytitle(No. of labs) ylabel(none) xtitle(months relative to law enactment) xscale(range(-12 24)) xlabel(-12(6)24) ylabel(0(5)15) yscale(range(0 15)) legend(off)
graph export "CI_plot_under2.png", replace

* Estimation (under 2-8 oz)
use "211b_hw3_prob7", replace
reg cap_2_8_oz i.state i.event_date pi_*
regsave pi_*, ci
gen id = 0
forval i=1/37 {
	replace id = -13 + `i' in `i'
}
twoway (connected coef id, msize(small)) (rcap ci_upper ci_lower id), ytitle(No. of labs) ylabel(none) xtitle(months relative to law enactment) xscale(range(-12 24)) xlabel(-12(6)24) ylabel(0(5)5) yscale(range(0 5)) legend(off)
graph export "CI_plot_2-8.png", replace

* Estimation (under over 9 oz)
use "211b_hw3_prob7", replace
reg cap_9_oz_or_more i.state i.event_date pi_*
regsave pi_*, ci
gen id = 0
forval i=1/37 {
	replace id = -13 + `i' in `i'
}
twoway (connected coef id, msize(small)) (rcap ci_upper ci_lower id), ytitle(No. of labs) ylabel(none) xtitle(months relative to law enactment) xscale(range(-12 24)) xlabel(-12(6)24) ylabel(0(1)2) yscale(range(0 2)) legend(off)
graph export "CI_plot_over9.png", replace
