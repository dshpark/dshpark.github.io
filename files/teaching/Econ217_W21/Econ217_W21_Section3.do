* ---------------------------- *
* Econ217 (J. Li, W2021)       *
* Section 3 (Jan. 20-22, 2021) * NOTE: all the path directories are based on my own computer, 
* TA: David Sungho Park        *       so you should change them according to yours.
* ---------------------------- *
clear all
set more off

cd "/Users/DSP/Google Drive/UCSC/Teaching/Econ217_W21/SectionMaterial/"
global raw_folder "tex_raw"

/*********************
 Logit/probit results
*********************/
use "Data Sets/BWGHT.dta", clear
	
// Generating a binary variable
su bwght, detail	
gen bwght_abovemed = (bwght>`r(p50)')

// Option 1: copy-paste raw Stata output
logit bwght_abovemed cigs faminc 	// raw model
margins, dydx(*)					// average marginal effects (AME)
margins, dydx(*) atmeans			// marginal effects at means (MEM)

// Option 2: export to LaTeX
	// setting tex filename 
	local texname example_logit

	est clear
	eststo raw: logit bwght_abovemed cigs faminc 		// storing results for raw model as "raw"
	logit bwght_abovemed cigs faminc 
		eststo ame: estpost margins, dydx(*)			// storing AME results as "ame"
	logit bwght_abovemed cigs faminc 
		eststo mem: estpost margins, dydx(*) atmeans 	// storing MEM results as "mem"

	// Setting indep variables' labels to appear in LaTeX tables
	lab var cigs 	"Number of cigarettes/day while pregnant"	
	lab var faminc	"Family income (USD 1000's)"
	
	local i = 3 // number of columns in the table
	local i_plus1 = `i'+1

	#delimit ;
	estout raw ame mem using "${raw_folder}/`texname'.tex", replace style(tex) eqlabel(none) mlabel(none) collabel(none) number label 
		drop(_cons) starlevels(* 0.10 ** 0.05 *** 0.01)
		stats(N, labels("Observations") fmt(%9.0fc))  
		prehead("\begin{tabular}{l *{`i'}{c}}" "\toprule")
		posthead(
			" & \multicolumn{`i'}{c}{=1 if birthweight is above median} \\"		
			"\cmidrule(lr){2-`i_plus1'}"
			" & Raw model & Avg. ME & ME at means \\"
			"\midrule") 
		cells("b(fmt(%9.3f) star)" "se(fmt(%9.3f) par)") 
		prefoot("\\")
		postfoot("\bottomrule" "\end{tabular}")
		;
	#d cr

/********************************
 Multinomial logit/probit results
********************************/
use "Data Sets/BWGHT.dta", clear

// Generating a categorical variable
gen bwght_class = .
replace bwght_class = 1 if bwght<50
replace bwght_class = 2 if bwght>=50 & bwght<100
replace bwght_class = 3 if bwght>=100 

label define bwght_class_str 1 "very low" 2 "low" 3 "normal"
label values bwght_class bwght_class_str

// Export to LaTeX 
local texname example_mlogit


	mlogit bwght_class cigs faminc, baseoutcome(3)
	eststo ame_1: estpost margins, dydx(*) predict(outcome(1))
	mlogit bwght_class cigs faminc, baseoutcome(3)
	eststo ame_2: estpost margins, dydx(*) predict(outcome(2))
	mlogit bwght_class cigs faminc, baseoutcome(3)
	eststo ame_3: estpost margins, dydx(*) predict(outcome(3))


est clear
local i = 0
foreach o in 1 2 3 {
	mlogit bwght_class cigs faminc, baseoutcome(3)
	local ++i
	eststo ame_`o': estpost margins, dydx(*) predict(outcome(`o'))
}

lab var cigs 	"Number of cigarettes/day while pregnant"	
lab var faminc	"Family income"

#d;
estout ame_* using "${raw_folder}/`texname'.tex", replace style(tex) eqlabel(none) mlabel(none) collabel(none) number label 
	starlevels(* 0.10 ** 0.05 *** 0.01)
	stats(N, labels("Observations") fmt(%9.0fc))  
	prehead("\begin{tabular}{l *{`i'}{c}}" "\toprule")
	posthead(
		" & very low & low & normal \\"
		"\midrule") 
	cells("b(fmt(%9.4f) star)" "se(fmt(%9.4f) par)") 
	prefoot("\\")
	postfoot("\bottomrule" "\end{tabular}")
	;
#d cr	

/********************************
 Ordered logit/probit results
********************************/
local texname example_ologit

est clear
local i = 0
foreach o in 1 2 3 {
	ologit bwght_class cigs faminc
	local ++i
	eststo ame_`o': estpost margins, dydx(*) predict(outcome(`o'))
}

lab var cigs 	"Number of cigarettes/day while pregnant"	
lab var faminc	"Family income"

#d;
estout ame_* using "${raw_folder}/`texname'.tex", replace style(tex) eqlabel(none) mlabel(none) collabel(none) number label 
	starlevels(* 0.10 ** 0.05 *** 0.01)
	stats(N, labels("Observations") fmt(%9.0fc))  
	prehead("\begin{tabular}{l *{`i'}{c}}" "\toprule")
	posthead(
		" & very low & low & normal \\"
		"\midrule") 
	cells("b(fmt(%9.4f) star)" "se(fmt(%9.4f) par)") 
	prefoot("\\")
	postfoot("\bottomrule" "\end{tabular}")
	;
#d cr	


exit
