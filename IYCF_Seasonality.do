* IYCF Seasonality Analysis
* Five pooled datasets from India 2005-21

* Data requirements for following analysis:  All months of the year must be represented in the data.
* Analysis with 5 datasets

* Add dependencies
* ssc install combomarginsplot

* Combomarginsplot
* help combomarginsplot
* Coefplot -  http://repec.sowi.unibe.ch/stata/coefplot/getting-started.html


* Analysis Plan
* 1 Graph of percent data collected by month by survey
* 2 Data by background variables (weighted estimates)
* 3 Tests for seasonal variation by month12
* 4 Amplitude of seasonal variation by vars
* 5 Adjusted vs unadjusted survey estimates of breastfeeding vars / complementary feeding vars
* 		significant/ relevant differences
* 6 Adjusted survey estimates by month
* 7 Trend analysis
* Excel EBF graph by 12 months by survey (centred on midpoint of data collection) without trend lines
* Excel Water graph by 12 months by survey (centred on midpoint of data collection) without trend lines


 

* Dependent variables

* General
// evbf currently_bf
* First day of life
// eibf eibf_timing ebf3d
// prelacteal_milk prelacteal_sugarwater prelacteal_water prelacteal_gripewater prelacteal_saltwater prelacteal_juice prelacteal_formula prelacteal_tea prelacteal_honey prelacteal_janamghuti prelacteal_other prelacteal_otherthanmilk prelacteal_milk_form 
* Breastfeeding and liquids
// ebf mixed_milk water juice tea other_liq milk formula broth bottle
// freq_milk freq_formula freq_other_milk
* Continued Breastfeeding

* if vars are not represented by each survey - like ebf3d, tea, freq_milk, freq_formula, They are not included

set scheme s1mono

* Include paths 
include "C:\Users\stupi\OneDrive - UNICEF\1 UNICEF Work\1 moved to ECM\IIT-B\IYCF\analysis\robert_paths.do"
// include "dnyaneshwar_paths.do"

* Load Data
use iycf_5surveys.dta, clear 

* path for graphs to apply to word doc. 
cd C:\Temp\Junk

tab int_month round, m
tab round if agemos<24, m
sum agemos



* for independent variables in analysis, we do not need to create dummies if we specify variable type in code
* for categorical vars use "i."
* for continuous vars use "c."
* for use of age and age squared use - c.age c.age#c.age

* wi and mum_educ are considered categorical variables, as agegrp is considered categorical in stata manual

local ContVars ib12.int_month i.state i.rururb i.wi i.mum_educ i.mum_work i.anc4plus ///
	i.earlyanc i.csection i.inst_birth i.bord c.age_days c.age_days#c.age_days ///
	i.sex i.cat_birth_wt i.diar i.fever i.ari i.round

tab ebf round
tab ebf_denom round
cap drop ebf_x
gen ebf_x = ebf*100 if agemos<6
tab agemos ebf_x 

version 16: table round [pw = national_wgt] if ebf_denom==1, c(mean ebf_x n ebf_x) format(%9.1f)
version 16: table round [pw = national_wgt] , c(mean ebf_x n ebf_x) format(%9.1f)

* Reported results from survey reports
// youngest child < 6 months living with mother
// NFHS-3 REPORT  EBF<6M  	46.4     5,081 -
//										   - uses age in months v008-b3  & child is alive, most recent birth & child living with mother
// RSOC   REPORT  EBF<6M  	64.9     9,281
// NFHS-4 REPORT  EBF<6M  	55.0    21,365 - 
//                                         - uses age in months v008-b3  & child is alive
//                                         - does not use most recent birth & child living with mother
// CNNS   REPORT  EBF<6M  	58.0     3,615 - Estimate from CNNS Report updated by new code
// NFHS-5 REPORT  EBF<6M  	63.7  	22,406 

* Test for inclusion of only youngest child of woman
// egen ebf_count = tag(caseid v003 ebf)
* Child is twin
// tab b0 ebf_count,m
* 103 cases from NFHS-5 of multiple births with EBF data collected


* Analysis
* Data by background variables (weighted estimates)

* Add code from Dnyaneshwar here

**********
* Table 1
**********

* Table for prevalence estimate from dependent variables

foreach var of varlist ebf mixed_milk water juice other_liq milk formula broth bottle {
	replace `var' = . if agemos>=6
}
foreach var of varlist evbf currently_bf eibf prelacteal_milk prelacteal_sugarwater prelacteal_water prelacteal_gripewater prelacteal_saltwater prelacteal_formula prelacteal_honey prelacteal_janamghuti prelacteal_other {
	replace `var' = . if agemos>=24
}
replace cont_bf =. if agemos<12 & agemos>23

putexcel set prev_table, replace
putexcel A1 = "Table 2: Prevalence of breastfeeding and giving liquids variables by survey, India Surveys 2005-2021"
putexcel A2 = "Variable"
putexcel C2 = "NFHS-3"
putexcel D2 = "RSOC"
putexcel E2 = "NFHS-4"
putexcel F2 = "CNNS"
putexcel G2 = "NFHS-5"

local RowNum = 1

foreach var of varlist evbf currently_bf eibf prelacteal_milk prelacteal_sugarwater prelacteal_water prelacteal_gripewater prelacteal_saltwater prelacteal_formula prelacteal_honey prelacteal_janamghuti prelacteal_other ebf mixed_milk water juice other_liq milk formula broth bottle cont_bf {
	local RowNum = `RowNum'+2
	putexcel A`RowNum' = "`var'"
	local ObsRowNum = `RowNum' +1
	putexcel B`ObsRowNum' = "N"

	forvalues i=1/5 {
		sum `var' [aw = national_wgt] if round==`i'
		local Cell = char(66 + `i' ) + string(`RowNum')
// 		local Prev =(cond(`r(mean)'==0,"-", string(`r(mean)'*100)))
		local Prev = string(`r(mean)'*100)
		putexcel `Cell' = `Prev', nformat(#0.0)
		local Cell = char(66 + `i') + string(`ObsRowNum')
// 		local Obs =(cond(`r(N)'<=1,"-", string(`r(N)')))
// 		putexcel `Cell' = `Obs', nformat(#,###)
		putexcel `Cell' = `r(N)', nformat(#,###)
	}
}
putexcel save

cap drop cont_bf_x
gen cont_bf_x = cont_bf*100
version 16: table round [pw = national_wgt] , c(mean cont_bf_x n cont_bf_x) format(%9.1f)

tab broth round [aw = national_wgt], col
tab bottle round [aw = national_wgt], col
sum broth if round==1
sum bottle if round==2
* attempted to insert corrected formatting of result into cell
local Prev =(cond(`r(mean)'==0,"-", string(`r(mean)'*100)))
local Obs =(cond(`r(N)'<=1,"-", string(`r(N)')))
di "`Prev'"
di "`Obs'" 
	
	
* Tables for dependent variables, max / min / amplitude / statistical significance of monthly variation

* Assumption, if there is no variation in pooled 5 survey data, then no variation in single survey dataset
* Start Min Max Table 
putexcel set min_max_table, replace
putexcel A1 = "Table 3: Annual prevalence & standard deviation with monthly estimates of minimum, maximum and amplitude of feeding variables adjusted for socio-demographic variation, India Surveys 2005-2021"
putexcel A2 = "Var"
putexcel B2 = "Prevalence"
putexcel C2 = "SD"
putexcel D2 = "Min"
putexcel E2 = "Max"
putexcel F2 = "Amp"
putexcel G2 = "N"

local DepVars = "evbf currently_bf"

local ContVars ib12.int_month i.state i.rururb i.wi i.mum_educ i.mum_work i.anc4plus ///
	i.earlyanc i.csection i.inst_birth i.bord c.age_days c.age_days#c.age_days ///
	i.sex i.cat_birth_wt i.diar i.fever i.ari i.round

local RowNum = 2

foreach var of varlist evbf currently_bf {
	di "`var'"
 	local RowNum = `RowNum' +1
	logit `var' `ContVars' [pw = national_wgt] if agemos < 24
	margins 
	* save r(table) to normal matrix
	matrix output = r(table)
	local temp = "`r(predict1_label)'"
	local temp1 = substr("`temp'",4,.)
	local var_name = subinstr("`temp1'",")","",.)
	putexcel A`RowNum' = "`var_name'"
	putexcel B`RowNum' = (output[1,1] * 100), nformat(0.0)  // mean one digit 
	* Note SD is calculated as SD = SE * sqrt(N)
	putexcel C`RowNum' = (output[2,1] * sqrt(`r(N)')), nformat(number_d2) // standard deviation two digits
	putexcel G`RowNum' = `r(N)', nformat(#,###) 

	margins int_month
	putexcel set margin_output, replace
	* Add Matrix
	putexcel A1 = matrix(r(table)'), names
	* Add varname to Matrix
	putexcel A1 = "`r(predict1_label)'"
	putexcel save
	import excel "C:\Temp\Junk\margin_output.xlsx", sheet("Sheet1") firstrow clear
	sum b, meanonly
	local min = r(min) *100
	local max = r(max) *100
	local amp = (`max'-`min')/2
	putexcel set min_max_table, modify
	putexcel D`RowNum' = `min', nformat(0.0)  		// min
	putexcel E`RowNum' = `max', nformat(0.0) 	  	// max
	putexcel F`RowNum' = `amp', nformat(number_d2)  // amplitude
	putexcel save
	use C:\Temp\Data\iycf_5surveys.dta, clear 
}

* add initiation of breastfeeding variables
local ContVars ib12.birthmonth i.state i.rururb i.wi i.mum_educ i.mum_work i.anc4plus ///
	i.earlyanc i.csection i.inst_birth i.bord c.age_days c.age_days#c.age_days ///
	i.sex i.cat_birth_wt i.diar i.fever i.ari i.round
local RowNum = 5

foreach var of varlist eibf prelacteal_milk prelacteal_sugarwater prelacteal_water prelacteal_gripewater prelacteal_saltwater prelacteal_formula prelacteal_honey prelacteal_janamghuti prelacteal_other { 
	di "`var'"
 	local RowNum = `RowNum' +1
	logit `var' `ContVars' [pw = national_wgt] if agemos < 24
	margins 
	* save r(table) to normal matrix
	matrix output = r(table)
	local temp1 = substr("`r(predict1_label)'",4,.)
	local var_name = subinstr("`temp1'",")","",.)
	putexcel A`RowNum' = "`var_name'"
	putexcel B`RowNum' = (output[1,1] * 100), nformat(0.0)  // mean one digit 
	* Note SD is calculated as SD = SE * sqrt(N)
	putexcel C`RowNum' = (output[2,1] * sqrt(`r(N)')), nformat(number_d2) // standard deviation two digits
	putexcel G`RowNum' = `r(N)', nformat(#,###) 

	margins birthmonth
	putexcel set margin_output, replace
	* Add Matrix
	putexcel A1 = matrix(r(table)'), names
	* Add varname to Matrix
	putexcel A1 = "`r(predict1_label)'"
	putexcel save
	import excel "C:\Temp\Junk\margin_output.xlsx", sheet("Sheet1") firstrow clear
	sum b, meanonly
	local min = r(min) *100
	local max = r(max) *100
	local amp = (`max'-`min')/2
	putexcel set min_max_table, modify
	putexcel D`RowNum' = `min', nformat(0.0) 		// min
	putexcel E`RowNum' = `max', nformat(0.0) 	  	// max
	putexcel F`RowNum' = `amp', nformat(number_d2)  // amplitude
	putexcel save
	use C:\Temp\Data\iycf_5surveys.dta, clear 
}

local ContVars ib12.int_month i.state i.rururb i.wi i.mum_educ i.mum_work  ///
	i.anc4plus i.earlyanc i.csection i.inst_birth i.bord c.age_days       ///
	c.age_days#c.age_days i.sex i.cat_birth_wt i.diar i.fever i.ari i.round
local RowNum = 16

foreach var of varlist ebf water mixed_milk milk juice other_liq formula broth bottle  {
	di "`var'"
 	local RowNum = `RowNum' +1
	logit `var' `ContVars' [pw = national_wgt] if agemos<6
	margins 
	* save r(table) to normal matrix
	matrix output = r(table)
	local temp1 = substr("`r(predict1_label)'",4,.)
	local var_name = subinstr("`temp1'",")","",.)
	putexcel A`RowNum' = "`var_name'"
	putexcel B`RowNum' = (output[1,1] * 100), nformat(##.0) // mean one digit 
	* Note SD is calculated as SD = SE * sqrt(N)
	putexcel C`RowNum' = (output[2,1] * sqrt(`r(N)')), nformat(number_d2) // standard deviation two digits
	putexcel G`RowNum' = `r(N)', nformat(#,###) 

	margins int_month
	putexcel set margin_output, replace
	* Add Matrix
	putexcel A1 = matrix(r(table)'), names
	* Add varname to Matrix
	putexcel A1 = "`r(predict1_label)'"
	putexcel save
	import excel "C:\Temp\Junk\margin_output.xlsx", sheet("Sheet1") firstrow clear
	sum b, meanonly
	local min = r(min) *100
	local max = r(max) *100
	local amp = (`max'-`min')/2
	putexcel set min_max_table, modify
	putexcel D`RowNum' = `min', nformat(##.0) 		// min
	putexcel E`RowNum' = `max', nformat(##.0) 	  	// max
	putexcel F`RowNum' = `amp', nformat(number_d2)  // amplitude
	putexcel save
	use C:\Temp\Data\iycf_5surveys.dta, clear 
}

*Continued BF
local ContVars ib12.int_month i.state i.rururb i.wi i.mum_educ i.mum_work  ///
	i.anc4plus i.earlyanc i.csection i.inst_birth i.bord c.age_days       ///
	c.age_days#c.age_days i.sex i.cat_birth_wt i.diar i.fever i.ari i.round
local RowNum = 26

foreach var of varlist  cont_bf {
	di "`var'"
	
 	local RowNum = `RowNum' +1
	logit `var' `ContVars' [pw = national_wgt] if agemos<12 | agemos>23
	margins 
	* save r(table) to normal matrix
	matrix output = r(table)
	local temp1 = substr("`r(predict1_label)'",4,.)
	local var_name = subinstr("`temp1'",")","",.)
	putexcel A`RowNum' = "`var_name'"
	putexcel B`RowNum' = (output[1,1] * 100), nformat(##.0) // mean one digit 
	* Note SD is calculated as SD = SE * sqrt(N)
	putexcel C`RowNum' = (output[2,1] * sqrt(`r(N)')), nformat(number_d2) // standard deviation two digits
	putexcel G`RowNum' = `r(N)', nformat(#,###) 

	margins int_month
	putexcel set margin_output, replace
	* Add Matrix
	putexcel A1 = matrix(r(table)'), names
	* Add varname to Matrix
	putexcel A1 = "`r(predict1_label)'"
	putexcel save
	import excel "C:\Temp\Junk\margin_output.xlsx", sheet("Sheet1") firstrow clear
	sum b, meanonly
	local min = r(min) *100
	local max = r(max) *100
	local amp = (`max'-`min')/2
	putexcel set min_max_table, modify
	putexcel D`RowNum' = `min', nformat(##.0) 		// min
	putexcel E`RowNum' = `max', nformat(##.0) 	  	// max
	putexcel F`RowNum' = `amp', nformat(number_d2)  // amplitude
	putexcel save
	use C:\Temp\Data\iycf_5surveys.dta, clear 
}


local ExportPath "C:/TEMP/Seasonality"
local FileName "IYCF Seasonality.docx"
di "`ExportPath'/`FileName'"

* Create word document with results
* TITLE PAGE
putdocx clear
putdocx begin, font("Calibri") 
putdocx paragraph, style(Title) halign(center) spacing(line,16 pt)
putdocx text ("Seasonality of IYCF variables ")
putdocx paragraph, style(Title) halign(center) spacing(line,14 pt)
putdocx text ("in Indian 5 Surveys ")
putdocx save "`ExportPath'/`FileName'", replace
	

	
	
local ExportPath "C:/TEMP/Seasonality"
local FileName "IYCF Seasonality.docx"
di "`ExportPath'/`FileName'"


* Exclusive Breastfeeding by Round
* add corrections for selection of correct denominators
cap drop ebf_x
gen ebf_x = ebf*100 if agemos <6
tab agemos ebf_x 
tab round ebf_x



* Plot adjusted vs unadjusted estimates onto one graph


* EBF
* Survey estimates based on one month shared by all surveys

* No controls are applied for month of data collection 
logit ebf_x i.round  [pw = national_wgt] 
margins round, saving(file1, replace)

* Adjustments are applied using control variables for month of data collection 
local ContVars ib12.int_month i.state i.rururb i.wi i.mum_educ i.mum_work  ///
	i.anc4plus i.earlyanc i.csection i.inst_birth i.bord c.age_days       ///
	c.age_days#c.age_days i.sex i.cat_birth_wt i.diar i.fever i.ari i.round

logit ebf_x `ContVars' [pw = national_wgt] 
* output by round of survey
margins round,  saving(file2, replace)

margins round, at(int_month==3) saving(file3, replace)

preserve
use file1, clear
replace _at =1
append using file2  
replace _at =2 in 6/10
append using file3  
replace _at =3 in 11/15
save ebf_round, replace 

replace _m1 = _m1-0.05 if _at==1
replace _m1 = _m1+0.05 if _at==3

twoway (scatter _margin _m1 if _at==1 , msymbol(D) ) ///
       (rcap _ci_ub _ci_lb _m1 if _at==1) (line _margin _m1 if _at==1) ///
	   (scatter _margin _m1 if _at==2 , msymbol(Oh) ) ///
       (rcap _ci_ub _ci_lb _m1 if _at==2) (line _margin _m1 if _at==2, lpattern(longdash)) ///
	   (scatter _margin _m1 if _at==3 , msymbol(Th) ) ///
       (rcap _ci_ub _ci_lb _m1 if _at==3) (line _margin _m1 if _at==3, lpattern(shortdash)) ///
	    , ///
	   title("Exclusive Breastfeeding") ///
	   xlabel(1(1)5, valuelabel ) xtitle(" ") ylabel(0.4(0.1)0.7) ///
	   ytitle(Proportion) legend(pos(5) ring(0) col(1)  region(lstyle(none)) ///
	   order(1 "Unadjusted - Midpoint" 4 "Adjusted - Midpoint" 7 "Adjusted - March")) scheme(s1mono) ///
	   graphregion(color(white)) bgcolor(white) 
graph export ebf_unadj_adj_mar.tif, as(tif) replace

restore


* WATER
* Survey estimates based on one month shared by all surveys

* Giving Water by Round
* selection of correct denominators
cap drop water_x
gen water_x = water*100 if agemos <6
tab agemos water_x 

* No adjustment 
logit water_x i.round  [pw = national_wgt] 
margins round, saving(file1, replace)

* Adjustments are applied using control variables for month of data collection 
local ContVars ib12.int_month i.state i.rururb i.wi i.mum_educ i.mum_work  ///
	i.anc4plus i.earlyanc i.csection i.inst_birth i.bord c.age_days       ///
	c.age_days#c.age_days i.sex i.cat_birth_wt i.diar i.fever i.ari i.round

logit water_x `ContVars' [pw = national_wgt] 
* Adjustment by midpoint of survey
margins round,  saving(file2, replace)
* Adjustment by month March
margins round, at(int_month==3) saving(file3, replace)

preserve
use file1, clear
replace _at =1
append using file2  
replace _at =2 in 6/10
append using file3  
replace _at =3 in 11/15
save h20_round, replace 

replace _m1 = _m1-0.05 if _at==1
replace _m1 = _m1+0.05 if _at==3

twoway (scatter _margin _m1 if _at==1 , msymbol(D) ) ///
       (rcap _ci_ub _ci_lb _m1 if _at==1) (line _margin _m1 if _at==1) ///
	   (scatter _margin _m1 if _at==2 , msymbol(Oh) ) ///
       (rcap _ci_ub _ci_lb _m1 if _at==2) (line _margin _m1 if _at==2, lpattern(longdash)) ///
	   (scatter _margin _m1 if _at==3 , msymbol(Th) ) ///
       (rcap _ci_ub _ci_lb _m1 if _at==3) (line _margin _m1 if _at==3, lpattern(shortdash)) ///
	    , ///
	   title("Giving Water") ///
	   xlabel(1(1)5, valuelabel ) xtitle(" ") ylabel(0.1(0.1)0.5) ///
	   ytitle(Proportion) legend(pos(5) ring(0) col(1)  region(lstyle(none)) ///
	   order(1 "Unadjusted - Midpoint" 4 "Adjusted - Midpoint" 7 "Adjusted - March")) scheme(s1mono) ///
	   graphregion(color(white)) bgcolor(white) 
graph export water_unadj_adj_mar.tif, as(tif) replace

restore

* Merge graphs together
graph combine ebf_unadj_adj_mar water_unadj_adj_mar , xsize(6.5) ysize(2.7) iscale(.8) name(comb, replace)
graph close ebf_unadj_adj_mar water_unadj_adj_mar 
graph export "EBF and giving water unadjusted, adjusted and set to March as month of data collection.png", width(6000) replace

*OLD 
// * No controls are applied for month of data collection 
// logit ebf_x i.round [pw = national_wgt] 
// margins round, saving(file1, replace)
//
// * Adjustments are applied using control variables for month of data collection 
// local ContVars ib12.int_month i.state i.rururb i.wi i.mum_educ i.mum_work  ///
// 	i.anc4plus i.earlyanc i.csection i.inst_birth i.bord c.age_days       ///
// 	c.age_days#c.age_days i.sex i.cat_birth_wt i.diar i.fever i.ari i.round
//
// logit ebf_x `ContVars' [pw = national_wgt] 
// * output by round of survey
// margins round, saving(file2, replace)
//
// combomarginsplot file1 file2, labels( "Unadjusted" "Adjusted" ) ///
// 	file1opts(pstyle(p1)) file2opts(pstyle(p2)) lplot1(mfcolor(white)) ///
// 	title("Exclusive breastfeeding by survey") ytitle("Proportion") ///
// 	legend(pos(6) ring(0) col(2) region(lstyle(none))) offset
// graph export ebf_adj_unadj.tif, as(tif) replace


* Add combomarginsplot to word file
putdocx begin, font("Calibri") 
putdocx pagebreak
putdocx paragraph, halign(left)
putdocx image ebf_unadj_adj_mar.tif, linebreak(1)
putdocx image water_unadj_adj_mar.tif, linebreak(1)

putdocx save "`ExportPath'/`FileName'", append

* attention to include all socio-economic vars i.wi i.mum_educ i.mum_work

* Plot adjusted EBF estimates by month from pooled data in one graph
local ContVars ib12.int_month i.state i.rururb i.wi i.mum_educ i.mum_work  ///
	i.anc4plus i.earlyanc i.csection i.inst_birth i.bord c.age_days       ///
	c.age_days#c.age_days i.sex i.cat_birth_wt i.diar i.fever i.ari i.round
	
logit ebf_x `ContVars' [pw = national_wgt] 
margins int_month, saving(file1, replace)
marginsplot, title("Exclusive breastfeeding by month of data collection") ///
	ytitle("Proportion") ylab(0.5(.1)0.7) yscale(range(0.5 0.7)) ///
	name(month_ebf, replace)
graph export ebf_month.tif, as(tif) replace

putdocx pagebreak
putdocx paragraph, halign(left)
putdocx image ebf_month.tif, linebreak(1)


* Plot adjusted WATER estimates by month from pooled data in one graph
local ContVars ib12.int_month i.state i.rururb i.wi i.mum_educ i.mum_work  ///
	i.anc4plus i.earlyanc i.csection i.inst_birth i.bord c.age_days       ///
	c.age_days#c.age_days i.sex i.cat_birth_wt i.diar i.fever i.ari i.round
	
logit water_x `ContVars' [pw = national_wgt] 
margins int_month, saving(file1, replace)
marginsplot, title("Giving water by month of data collection") ///
	ytitle("Proportion") ylab(0.2(.1)0.4) yscale(range(0.2 0.4)) ///
	name(month_water, replace)
graph export h20_month.tif, as(tif) replace

* Add combomarginsplot to word file
putdocx pagebreak
putdocx paragraph, halign(left)
putdocx image h20_month.tif, linebreak(1)


* Merge graphs together
graph combine month_ebf month_water , xsize(6.5) ysize(2.7) iscale(.8) name(comb, replace)
graph close month_ebf month_water 
graph export "Feeding variables by month of data collection.png", width(6000) replace

putdocx begin, font("Calibri") 

* Add combomarginsplot to word file
putdocx pagebreak
putdocx paragraph, halign(left)
putdocx image "Feeding variables by month of data collection.png", linebreak(1)

putdocx save "`ExportPath'/`FileName'", append


local ExportPath "C:/TEMP/Seasonality"
local FileName "IYCF Seasonality.docx"


* Exclusive breastfeeding	
* Plot the predicted values of the dependent variable for each survey in five graphs for five surveys
* Combomarginsplot - joining five graphs onto one background

* Analysis of interaction between month and round
local ContVars ib12.int_month##i.round  i.state i.rururb i.wi i.mum_educ i.mum_work  ///
	i.anc4plus i.earlyanc i.csection i.inst_birth i.bord c.age_days ///
	c.age_days#c.age_days i.sex i.cat_birth_wt i.diar i.fever i.ari 

* Code below uses data available from dummy==x
forval x = 1/5 {
	logit ebf_x `ContVars' [pw = national_wgt] if round==`x' 
	margins int_month#round, saving(ebf_round`x', replace)
	local RoundValueLabel : value label round
	local GraphLabel: label `RoundValueLabel' `x'
	marginsplot, title("`GraphLabel'") ytitle("Proportion") name(file`x', replace) ///
		ylab(0.2(.1)0.8) yscale(range(0.2 0.8))
}

* Code uses all entire regression estimation sample to calculate the margins, with the value of dummy temporarily reset to x in every observation.
// logit ebf_x `ContVars' [pw = national_wgt] 
// forval x = 1/5 {
// 	margins int_month, at(round==`x') saving(ebf_round`x', replace)
// 	local RoundValueLabel : value label round
// 	local GraphLabel: label `RoundValueLabel' `x'
// 	marginsplot, title("`GraphLabel'") ytitle("Proportion") name(file`x', replace) ///
// 		ylab(0.2(.1)0.8) yscale(range(0.2 0.8))
// }
// graph combine file1 file2 file3 file4 file5, xsize(6.5) ysize(2.7) iscale(.8) name(comb, replace)
// graph close file1 file2 file3 file4 file5
graph export "EBF by month by survey.png", width(6000) replace

* Add EBF survey/month to word file
putdocx begin, font("Calibri") 
putdocx pagebreak
putdocx paragraph, halign(left)
putdocx text ("EBF by month by survey")
putdocx image "EBF by month by survey.png", linebreak(1)


* WATER
* Plot the predicted values of the dependent variable in five graphs for five surveys
* Combomarginsplot - joining five graphs onto one background

local ContVars i.state i.rururb i.wi i.mum_educ i.mum_work  ///
	i.anc4plus i.earlyanc i.csection i.inst_birth i.bord c.age_days ///
	c.age_days#c.age_days i.sex i.cat_birth_wt i.diar i.fever i.ari 

* this only uses the round data not pooled data for calculations
forval x = 1/5 {
	logit water_x ib12.int_month##i.round `ContVars' [pw = national_wgt] if round==`x' 
	margins int_month#round, saving(h20_round`x', replace)
	local RoundValueLabel : value label round
	local GraphLabel: label `RoundValueLabel' `x'
	marginsplot, title("`GraphLabel'")  ytitle("Proportion") name(file`x', replace) ///
		ylab(0.1(.1)0.8) yscale(range(0.1 0.8))
}
graph combine file1 file2 file3 file4 file5, xsize(6.5) ysize(2.7) iscale(.8) name(comb, replace)
graph close file1 file2 file3 file4 file5
graph export "Water by month by survey.png", width(6000) replace

* Add EBF survey/month to word file

putdocx pagebreak
putdocx paragraph, halign(left)
putdocx text ("Water by month by survey")
putdocx image "Water by month by survey.png", linebreak(1)

putdocx save "`ExportPath'/`FileName'", append
	
* For Excel Graph
* Use estimates that assume that all data was collected in April
* see exported data in ebf_roundX
* NFHS-5
* start 6 2019, mid   5 2020, end   5 2021


* FOR ANNEXES

* REGION
tab region round, m
scatter regional_wgt round

* Verify that ebf_x is for children under 6 months
tab agemos ebf_x
version 16: table region [pw = regional_wgt] , c(mean ebf_x n ebf_x) format(%9.1f)


* Plot adjusted EBF estimates by month by region from pooled data in one graph
* Region interaction with month of data collection
// local ContVars ib12.int_month##i.region i.state i.rururb i.wi i.mum_educ i.mum_work  ///
// 	i.anc4plus i.earlyanc i.csection i.inst_birth i.bord c.age_days       ///
// 	c.age_days#c.age_days i.sex i.cat_birth_wt i.diar i.fever i.ari i.round

// local ContVars ib12.int_month##i.region 

* Here state is replaced with region in the ContVars
* Graph below presents all regions on one graph overlapping (unreadable)
// local ContVars ib12.int_month##i.region i.rururb i.wi i.mum_educ i.mum_work  ///
// 	i.anc4plus i.earlyanc i.csection i.inst_birth i.bord c.age_days       ///
// 	c.age_days#c.age_days i.sex i.cat_birth_wt i.diar i.fever i.ari i.round
//	
// logit ebf_x `ContVars' [pw = regional_wgt] 
// margins int_month#region, saving(file_1, replace)
// marginsplot, title("Exclusive breastfeeding by month of data collection by region") ///
// 		ytitle("Proportion") ylab(0(.1)0.8) yscale(range(0.0 0.8)) 

		
* Margins with region==`x'

local ContVars ib12.int_month##i.region i.rururb i.wi i.mum_educ i.mum_work  ///
	i.anc4plus i.earlyanc i.csection i.inst_birth i.bord c.age_days       ///
	c.age_days#c.age_days i.sex i.cat_birth_wt i.diar i.fever i.ari i.round

logit ebf_x `ContVars' [pw = regional_wgt] 
	
forval x = 1/3 {
	margins int_month, at(region==`x') saving(file_`x', replace)
	marginsplot, title("Exclusive breastfeeding by month by region-"`x') ///
		ytitle("Proportion") ylab(0(.1)0.8) yscale(range(0.0 0.8)) 
	graph export ebf_region_`x'.tif, as(tif) replace
}

combomarginsplot file_1 file_2 file_3, labels( "North" "Central" "East" ) ///
	file1opts(pstyle(p1)) file2opts(pstyle(p2)) file3opts(pstyle(p3)) ///
	lplot1(mfcolor(white)) ///
	title("Exclusive Breastfeeding by North, Central & East regions") ///
		ytitle("Proportion") ylab(0(.1)0.8) yscale(range(0.0 0.8)) ///
	legend(pos(6) ring(0) col(3) region(lstyle(none))) offset

combomarginsplot file_4 file_5 file_6, labels( "NorthEast" "West" "South" ) ///
	lplot1(mfcolor(white)) ///
	title("Exclusive Breastfeeding by NorthEast, West & South regions")  ///
	ytitle("Proportion") ylab(0(.1)0.8) yscale(range(0.0 0.8)) ///
	legend(pos(6) ring(0) col(3) region(lstyle(none))) offset
	
* Add combomarginsplot to word file
putdocx pagebreak
putdocx paragraph, halign(left)
putdocx image ebf_month.tif, linebreak(1)

	
// combomarginsplot file_4 file_5 file_6, labels( "NorthEast" "West" "South" ) ///
// 	file4opts(pstyle(p1)) file5opts(pstyle(p2)) file6opts(pstyle(p3)) ///
// 	lplot1(mfcolor(white)) ///
// 	title("Exclusive breastfeeding by NorthEast, West &South regions")  ///
// 	ytitle("Proportion") ylab(0(.1)0.8) yscale(range(0.0 0.8)) ///
// 	legend(pos(6) ring(0) col(3) region(lstyle(none))) offset
	
* REGION
	
*Region 
*region 1  North    Delhi(25), Haryana(12), HP(13), J&K(14), Punjab(28), Rajasthan(29), Uttarakhand(34) Chandigarh(6) Ladakh(37)			 							   
*region 2  Central	Chhattisgarh(7), Madhya Pradesh(19), Uttar Pradesh(33)				
*region 3  East		Bihar(5), West Bengal(35), Jharkhand(15), Odisha(26)	 							
*region 4  NorthEast  Arunachal Pradesh(3), Sikkim(30), Tripura(32),  Meghalaya(22), Assam(4), Nagaland(24), Manipur(21), Mizoram(23)
*region 5  West     Gujarat(11), Maharshtra (20), Goa(10), Dadra & Nagar Haveli (8), Daman and Diu (9) 
*region 6  South    Andhra Pradesh(2),  Karnataka(16),  Kerala(17),  Tamil Nadu(31),  Telangana(36)  A&N islands (1) Puducherry (27) Lakshadweep (18)

* RESIDENCE
local ContVars ib12.int_month##i.rururb i.state i.wi i.mum_educ i.mum_work  ///
	i.anc4plus i.earlyanc i.csection i.inst_birth i.bord c.age_days       ///
	c.age_days#c.age_days i.sex i.cat_birth_wt i.diar i.fever i.ari i.round

logit ebf_x `ContVars' [pw = national_wgt] 
	
forval x = 1/2 {
	margins int_month, at(rururb==`x') saving(file_`x', replace)
}
combomarginsplot file_1 file_2, labels( "Urban" "Rural" ) ///
	file1opts(pstyle(p1)) file2opts(pstyle(p2)) plot1(mfcolor(white)) ///
	title("Exclusive Breastfeeding by Rural/Urban Residence") ///
		ytitle("Proportion") ylab(0(.1)0.8) yscale(range(0.0 0.8)) ///
	legend(pos(6) ring(0) col(3) region(lstyle(none))) offset
* Add combomarginsplot to word file
putdocx pagebreak
putdocx paragraph, halign(left)
putdocx image ebf_month.tif, linebreak(1)

* Mother's Employment
tab mum_work round, m 
local ContVars ib12.int_month##i.mum_work  i.state i.rururb i.wi i.mum_educ   ///
	i.anc4plus i.earlyanc i.csection i.inst_birth i.bord c.age_days       ///
	c.age_days#c.age_days i.sex i.cat_birth_wt i.diar i.fever i.ari i.round

logit ebf_x `ContVars' [pw = national_wgt] 
	
forval x = 0/1 {
	margins int_month, at(mum_work==`x') saving(file_`x', replace)
}
combomarginsplot file_0 file_1, labels( "No" "Yes" ) ///
	file1opts(pstyle(p1)) file2opts(pstyle(p2)) plot1(mfcolor(white)) ///
	title("Exclusive Breastfeeding by Mother's Employment") ///
		ytitle("Proportion") ylab(0(.1)0.8) yscale(range(0.0 0.8)) ///
	legend(pos(6) ring(0) col(3) region(lstyle(none))) offset
* Add combomarginsplot to word file
putdocx pagebreak
putdocx paragraph, halign(left)
putdocx image ebf_month.tif, linebreak(1)

* Socio-Economic Status
local ContVars ib12.int_month##i.wi i.state i.rururb i.mum_educ i.mum_work  ///
	i.anc4plus i.earlyanc i.csection i.inst_birth i.bord c.age_days       ///
	c.age_days#c.age_days i.sex i.cat_birth_wt i.diar i.fever i.ari i.round

logit ebf_x `ContVars' [pw = national_wgt] 
	
forval x = 1/5 {
	margins int_month, at(wi==`x') saving(file_`x', replace)
}
combomarginsplot file_1 file_2 file_3 file_4 file_5,  ///
	labels( "Poorest" "Poorer" "Middle" "Richer" "Richest") ///
	file1opts(pstyle(p1)) file2opts(pstyle(p2)) file3opts(pstyle(p3)) ///
	file4opts(pstyle(p4)) file5opts(pstyle(p5)) plot1(mfcolor(white)) ///
	title("Exclusive Breastfeeding by Socio-Economic Status") ///
		ytitle("Proportion") ylab(0.3(.1)0.7) yscale(range(0.3 0.7)) ///
	legend(pos(7) ring(0) col(1) region(lstyle(none))) offset
* Add combomarginsplot to word file
putdocx pagebreak
putdocx paragraph, halign(left)
putdocx image ebf_month.tif, linebreak(1)


* AGE IN MONTHS
* removed age in days from ContVars
local ContVars ib12.int_month##i.agemos i.state i.rururb i.wi i.mum_educ i.mum_work  ///
	i.anc4plus i.earlyanc i.csection i.inst_birth i.bord      ///
	i.sex i.cat_birth_wt i.diar i.fever i.ari i.round

logit ebf_x `ContVars' [pw = national_wgt] 
	
forval x = 0/5 {
	margins int_month, at(agemos==`x') saving(file_`x', replace)
}
combomarginsplot file_0 file_1 file_2 file_3 file_4 file_5,  ///
	labels( "0" "1" "2" "3" "4" "5") ///
	 file1opts(pstyle(p1)) file2opts(pstyle(p2)) file3opts(pstyle(p3)) ///
	file4opts(pstyle(p4)) file5opts(pstyle(p5)) file6opts(pstyle(p7)) plot1(mfcolor(white)) ///
	title("Exclusive Breastfeeding by Age in Months") ///
		ytitle("Proportion") ylab(0.1(.1)0.8) yscale(range(0.1 0.8)) ///
	legend(pos(7) ring(0) col(1) region(lstyle(none))) offset
* Add combomarginsplot to word file
putdocx pagebreak
putdocx paragraph, halign(left)
putdocx image ebf_month.tif, linebreak(1)



putdocx begin, font("Calibri") 

* Giving liquids and continued breastfeeding
* Seasonality of depvar01
local depvar01 mixed_milk milk juice other_liq formula broth bottle cont_bf 
	
* Variables that represent data from date of data collection
local ContVars i.int_month i.state i.rururb i.wi i.mum_educ i.mum_work  ///
	i.anc4plus i.earlyanc i.csection i.inst_birth i.bord c.age_days ///
	c.age_days#c.age_days i.sex i.cat_birth_wt i.diar i.fever i.ari i.round

foreach var in `depvar01' {
	di `depvar01'
		
	logit `var' `ContVars' [pw = national_wgt] if agemos<6
	margins int_month
	marginsplot, title(`var' by month of data collection) ytitle("Proportion") 
	graph export `var'.tif, as(tif) replace

	putdocx paragraph, halign(center)
	putdocx image "`var'.tif"
}
local ExportPath "C:/TEMP/Seasonality"
local FileName "IYCF Seasonality.docx"
putdocx save "`ExportPath'/`FileName'", append

putdocx begin, font("Calibri") 

* Continued breastfeeding
	
* Variables that represent data from date of data collection
local ContVars i.int_month i.state i.rururb i.wi i.mum_educ i.mum_work  ///
	i.anc4plus i.earlyanc i.csection i.inst_birth i.bord c.age_days ///
	c.age_days#c.age_days i.sex i.cat_birth_wt i.diar i.fever i.ari i.round
		
logit cont_bf `ContVars' [pw = national_wgt] if agemos>=12 & agemos<24
margins int_month
marginsplot, title(`var' by month of data collection) ytitle("Proportion") 
graph export `var'.tif, as(tif) replace

putdocx paragraph, halign(center)
putdocx image "`var'.tif"

local ExportPath "C:/TEMP/Seasonality"
local FileName "IYCF Seasonality.docx"
putdocx save "`ExportPath'/`FileName'", append


putdocx begin, font("Calibri") 

* Ever, current and early initation of breastfeeding
* Seasonality of depvar01
local depvar01 evbf currently_bf eibf prelacteal_milk prelacteal_sugarwater ///
	prelacteal_water prelacteal_gripewater prelacteal_saltwater ///
	prelacteal_formula prelacteal_honey prelacteal_janamghuti prelacteal_other 
* Variables that represent data from date of birth
local ContVars i.birthmonth i.state i.rururb i.wi i.mum_educ i.mum_work  ///
	i.anc4plus i.earlyanc i.csection i.inst_birth i.bord c.age_days ///
	c.age_days#c.age_days i.sex i.cat_birth_wt i.diar i.fever i.ari i.round

foreach var in `depvar01' {
	di `depvar01'
		
	logit `var' `ContVars' [pw = national_wgt] if agemos<24
	margins int_month
	marginsplot, title(`var' by month of data collection) ytitle("Proportion") 
	graph export `var'.tif, as(tif) replace

	putdocx paragraph, halign(center)
	putdocx image "`var'.tif"
}
putdocx save "`ExportPath'/`FileName'", append



* END

* dependent variables
* ebf_x water_x mixed_milk milk juice other_liq formula broth bottle

* Variables that represent data from date of data collection
local ContVars i.int_month i.state i.rururb i.wi i.mum_educ i.mum_work  ///
	i.anc4plus i.earlyanc i.csection i.inst_birth i.bord c.age_days ///
	c.age_days#c.age_days i.sex i.cat_birth_wt i.diar i.fever i.ari i.round

logit bottle `ContVars' [pw = national_wgt] if agemos<6
margins int_month
pwcompare int_month, effects sort mcompare(sidak)


// ------------------------------------------------------------------------------
//              |                              Sidak                Sidak
//              |   Contrast   Std. err.      z    P>|z|     [95% conf. interval]
// -------------+----------------------------------------------------------------
* ebf 			
 Feb vs Jan  |  -.0888641   .0456016    -1.95   0.969    -.2421268    .0643986
 Mar vs Jan  |  -.2018539   .0487748    -4.14   0.002    -.3657815   -.0379263
 Apr vs Jan  |  -.6187814   .0567247   -10.91   0.000    -.8094278   -.4281349
 May vs Jan  |   -.898198   .0652538   -13.76   0.000     -1.11751   -.6788861
 Jun vs Jan  |  -.9199704   .0646985   -14.22   0.000    -1.137416   -.7025246
 Jul vs Jan  |  -.8173911   .0648307   -12.61   0.000    -1.035281   -.5995013
 Aug vs Jan  |  -.6198681   .0780346    -7.94   0.000    -.8821353   -.3576009
 Sep vs Jan  |  -.6104374   .0742461    -8.22   0.000    -.8599717   -.3609031
 Oct vs Jan  |  -.4944614   .0857995    -5.76   0.000    -.7828257    -.206097
 Nov vs Jan  |  -.4373159   .0954717    -4.58   0.000    -.7581875   -.1164444
 Dec vs Jan  |  -.2706061     .06268    -4.32   0.001    -.4812676   -.0599445
* water 	
 Feb vs Jan  |   .1069185   .0524809     2.04   0.940    -.0694651    .2833021
 Mar vs Jan  |   .3723018   .0550947     6.76   0.000     .1871337    .5574699
 Apr vs Jan  |   .8731455   .0618512    14.12   0.000     .6652694    1.081022
 May vs Jan  |   1.269315    .069188    18.35   0.000      1.03678    1.501849
 Jun vs Jan  |   1.276918    .069395    18.40   0.000     1.043688    1.510148
 Jul vs Jan  |   1.141542   .0693083    16.47   0.000     .9086033    1.374481
 Aug vs Jan  |    .860796   .0834211    10.32   0.000     .5804254    1.141167
 Sep vs Jan  |   .8467247   .0803237    10.54   0.000     .5767641    1.116685
 Oct vs Jan  |   .7037217   .0932105     7.55   0.000       .39045    1.016993
 Nov vs Jan  |   .5335744   .1060221     5.03   0.000      .177244    .8899047
 Dec vs Jan  |   .2824641   .0697743     4.05   0.003     .0479591    .5169692
* mixed_milk 	- No
* milk 			- Yes  
Dec vs Mar  |   .2690234   .0793058     3.39   0.045     .0024839    .5355629
Jul vs Mar  |    .304376   .0798837     3.81   0.009     .0358943    .5728577
* juice 		- Yes 
May vs Jan  |   .5424226   .1601235     3.39   0.045     .0042625    1.080583
* other_liq 	- Yes  
 Dec vs Sep  |   .5769729   .1638516     3.52   0.028      .026283    1.127663
 Dec vs Oct  |   .8795346    .201338     4.37   0.001     .2028564    1.556213
 Dec vs Nov  |   .6881156   .1949298     3.53   0.027     .0329746    1.343257
* formula 		- No
* broth			- No
* Bottle 		- Yes 
 Jul vs Jan  |   .330760   .0974981     3.39   0.045     .0030784    .6584425

local depvar01 mixed_milk milk juice other_liq formula broth bottle cont_bf 
	
* Variables that represent data from date of data collection
local ContVars i.int_month i.state i.rururb i.wi i.mum_educ i.mum_work  ///
	i.anc4plus i.earlyanc i.csection i.inst_birth i.bord c.age_days ///
	c.age_days#c.age_days i.sex i.cat_birth_wt i.diar i.fever i.ari i.round

foreach var in `depvar01' {
	di `depvar01'
		
	logit `var' `ContVars' [pw = national_wgt] if agemos<6
	margins int_month
	marginsplot, title(`var' by month of data collection) ytitle("Proportion") 
	graph export `var'.tif, as(tif) replace

	putdocx paragraph, halign(center)
	putdocx image "`var'.tif"
}
local ExportPath "C:/TEMP/Seasonality"
local FileName "IYCF Seasonality.docx"
putdocx save "`ExportPath'/`FileName'", append

putdocx begin, font("Calibri") 





