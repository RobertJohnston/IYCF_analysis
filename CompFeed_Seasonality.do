* Complementary Feeding Seasonality Analysis
* Five pooled datasets from India 2005-21

* Data requirements for following analysis:  All months of the year must be represented in the data.
* Analysis with 5 datasets

* Combomarginsplot
* help combomarginsplot
* Coefplot -  http://repec.sowi.unibe.ch/stata/coefplot/getting-started.html


* Analysis Plan
* 1 Graph of percent data collected by month of data collection by survey
* 2 Data by background variables (weighted estimates)
*	Dependent variables 
*	Covariates (independent variables)
* 3 Tests for seasonal variation by month12
* 4 Amplitude of seasonal variation by vars
* 5 Adjusted vs unadjusted survey estimates of complementary feeding vars
* 		significant/ relevant differences
* 6 Adjusted survey estimates by month
* 7 Trend analysis on seasonal affected indicators
 
/*
* Dependent variables

Complementary feeding indicators 
2.1. Introduction of solid, semi-solid or soft foods 6–8 months (ISSSF) 
any_solid_semi_food 

2.2. Minimum dietary diversity 6–23 months (MDD) 
currently_bf
carb 
leg_nut 
dairy 
all_meat 
egg 
vita_fruit_veg 
fruit_veg 
fortified_food 
meat 
bread 
potato 
vita_veg 
leafy_green 
vita_fruit 
organ 
fish 
yogurt 
semisolid 

* Variables that are not available across all surveys
* Gruel, Poultry
// version 16: table round [pw = national_wgt] , c(mean gruel n gruel) format(%9.1f)
// version 16: table round [pw = national_wgt] , c(mean poultry n  poultry) format(%9.1f)

* Variables harmonized to use across all surveys
* BREAD
* In all surveys, gruel and fortified_food is added to bread. 
* MEAT
* in all surveys, if poultry consumed then added to variable meat. 


2.3. Minimum meal frequency 6–23 months (MMF) 
	Number of meals given
	
2.4. Minimum milk feeding frequency for non-breastfed children 6–23 months (MMFF) 
	Number of milk feeds given
	
2.5. Minimum acceptable diet 6–23 months (MAD) 
	Number of dietary items given

2.6. Egg and/or flesh food consumption 6–23 months (EFF) 
2.7. Sweet beverage consumption 6–23 months (SwB) 
2.8. Unhealthy food consumption 6–23 months (UFC) 
2.9. Zero vegetable or fruit consumption 6–23 months (ZVF) 

* WHO/UNICEF IYCF food groups
1. breast milk;
2. grains, roots, tubers and plantains;
No specific question asked on plantain
3. pulses (beans, peas, lentils), nuts and seeds;
4. dairy products (milk, infant formula, yogurt, cheese);
5. flesh foods (meat, fish, poultry, organ meats);
6. eggs;
7. vitamin-A rich fruits and vegetables; and
8. other fruits and vegetables.

*/

* if vars are not represented by each survey - like X Y Z, They are not included

set scheme s1mono

* Include paths 
// include "C:\Users\stupi\OneDrive - UNICEF\1 UNICEF Work\1 moved to ECM\IIT-B\IYCF\analysis\robert_paths.do"
include "C:\Robert\IYCF Analysis Samsung\robert_paths.do"

* Load Data
use iycf_5surveys.dta, clear 

* path for graphs to apply to word doc. 
cd C:\Temp\Junk

tab int_month round, m
tab round if agemos>=6 & agemos<24, m
sum agemos

* Figure 1 Percent distribution of data by month of data collection and survey round (NFHS-3, RSOC, NFHS-4, CNNS and NFHS-5)
tab  int_month round if agemos>=6 & agemos<24, m

sum sumfoodgrp freq_solids milk_feeds feeds if agemos>=6 & agemos<24

gen sumfoodgrp_nm = sumfoodgrp if agemos>=6 & agemos<24
tab agemos sumfoodgrp_nm
tab sumfoodgrp_nm round, col

* Data cleaning
gen freq_solids_nm = freq_solids if freq_solids<8
tab freq_solids_nm if agemos>=6 & agemos<24, m 
tab  freq_solids_nm round if agemos>=6 & agemos<24, col

* indicators with extreme small sample for monthly estimates

local DepVars = "isssf mdd mmf_bf mmf_nobf min_milk_freq_nbf mmf_all mad_all egg_meat zero_fv currently_bf carb leg_nut dairy all_meat egg vita_fruit_veg fruit_veg bread leafy_green potato vita_veg fortified_food yogurt vita_fruit meat organ fish"

foreach var of varlist `DepVars' {
	forvalues num_round =1/5 {
		forvalues num_month =1/12 {
			sum `var' if round==`num_round' & int_month==`num_month'
			di "Round-`num_round' and Month-`num_month'" 
			replace `var' =. if r(N)<30 & round==`num_round' & int_month==`num_month'
		}
	}
}

* Double check corrections
local DepVars = "isssf mdd mmf_bf mmf_nobf min_milk_freq_nbf mmf_all mad_all egg_meat zero_fv currently_bf carb leg_nut dairy all_meat egg vita_fruit_veg fruit_veg bread leafy_green potato vita_veg fortified_food yogurt vita_fruit meat organ fish"
foreach var of varlist `DepVars' {
	version 16: table int_month round, c(n `var')
} 

cap drop agegrp_3
gen agegrp_3 = floor(agemos/6) if agemos>=6 & agemos<24
la def agegrp 1 "6-11 M" 2 "12-17 M" 3 "18-23 M"
la val agegrp_3 agegrp
tab agemos agegrp_3, m 
graph bar (count) one if agemos>=4 & agemos<28, over(agemos) 
* slight evidence of age preference for whole numbers 

* CLEANED Data
save iycf_5surveys_cleaned.dta, replace 
* Open if not opened already
use iycf_5surveys_cleaned.dta, clear 







* for independent variables in analysis, we do not need to create dummies if we specify variable type in code
* for categorical vars use "i."
* for continuous vars use "c."
* for use of age and age squared use - c.age c.age#c.age

* wi and mum_educ are considered categorical variables, as agegrp is considered categorical in stata manual

local ContVars ib12.int_month i.state i.rururb i.wi i.mum_educ i.mum_work i.anc4plus ///
	i.earlyanc i.csection i.inst_birth i.bord c.age_days c.age_days#c.age_days ///
	i.sex i.cat_birth_wt i.diar i.fever i.ari i.round

// 2.1. Introduction of solid, semi-solid or soft foods 6–8 months (ISSSF) 
// 2.2. Minimum dietary diversity 6–23 months (MDD) 
// 2.3. Minimum meal frequency 6–23 months   (MMF) 
// 2.5. Minimum acceptable diet 6–23 months (MAD) 

foreach var of varlist isssf mdd sumfoodgrp mmf_bf mmf_nobf mmf_all freq_solids milk_feeds feeds mad_all  {
	tab agemos `var'
	gen `var'_x = `var' *100
	version 16: table round [pw = national_wgt], c(mean `var'_x n `var'_x) format(%9.1f)
	drop `var'_x
} 

// Dietary diversity: sum of food groups consumed in past 24 hours
//  Food frequency: sum of number of solid/semi-solid feedings in past 24 hours
// 	Sum of number of milk feedings in past 24 hours
// 	Sum of number of solid/semi-solid  & milk feedings in past 24 hours 

foreach var of varlist sumfoodgrp freq_solids milk_feeds feeds {
	version 16: table agemos [pw = national_wgt] if agemos>=6 & agemos<=23, c(mean `var' n `var') format(%9.1f)
	version 16: table round [pw = national_wgt]if agemos>=6 & agemos<=23, c(mean `var' n `var') format(%9.1f)
} 




* are there differences between estimates from data and survey reports? 
* Reported results from survey reports

// NFHS-3 REPORT  VAR 	
// RSOC   REPORT  
// NFHS-4 REPORT  
// CNNS   REPORT  
// NFHS-5 REPORT 

* Test for inclusion of only youngest child of woman
// egen ebf_count = tag(caseid v003 ebf)
* Child is twin
// tab b0 ebf_count,m
* 103 cases from NFHS-5 of multiple births with EBF data collected


* Analysis



* Table 1
* Table for prevalence estimate from complementary feeding (dependent) variables

local DepVars = "isssf mdd mmf_bf mmf_nobf min_milk_freq_nbf mmf_all mad_all egg_meat zero_fv currently_bf carb leg_nut dairy all_meat egg vita_fruit_veg fruit_veg bread leafy_green potato vita_veg fortified_food yogurt vita_fruit meat organ fish"
* DepVars not including sumfoodgrp feeds

putexcel set CF_prev_table, replace
putexcel A1 = "Table 2: Prevalence of complementary feeding variables by survey, India Surveys 2005-2021"
putexcel A2 = "Variable"
putexcel C2 = "NFHS-3"
putexcel D2 = "RSOC"
putexcel E2 = "NFHS-4"
putexcel F2 = "CNNS"
putexcel G2 = "NFHS-5"

local RowNum = 1

foreach var of varlist `DepVars' {
	local RowNum = `RowNum'+2
	putexcel A`RowNum' = "`var'"
	local ObsRowNum = `RowNum' +1
	putexcel B`ObsRowNum' = "N"

	forvalues i=1/5 {
		sum `var' [aw = national_wgt] if round==`i' & agemos>=6 & agemos<24
		local Cell = char(66 + `i' ) + string(`RowNum')
		// local Prev = cond(missing(`r(mean)'),"-",string(`r(mean)'))
 		if  "`r(mean)'" == "" {
			local Prev  = "-"
		}
		else {
			local Prev  = round((`r(mean)'*100), .1)
		}
		putexcel `Cell' = "`Prev'"
		local Cell = char(66 + `i') + string(`ObsRowNum')
		local Obs =(cond(`r(N)'<=1,0, `r(N)'))
		putexcel `Cell' = `Obs', nformat(#,###)
	}
}
putexcel save


* Analysis
* What are the most appropriate covariates? 

local ContVars ib12.int_month i.state i.rururb i.wi i.mum_educ i.mum_work i.anc4plus i.inst_birth ///
	i.bord c.age_days c.age_days#c.age_days i.sex i.cat_birth_wt i.diar i.fever i.ari i.round
logit mad_all `ContVars' [pw = national_wgt] 
* Pseudo R2 = 0.07


* Table 2
* Data by background variables (weighted estimates)

local ContVars  mum_educ wi caste rururb anc4plus cat_birth_wt sex bord agegrp_3 diar fever ari mum_work 

putexcel set CF_background_vars, replace
putexcel A1 = "Table 2: Percent distribution of background variables by survey, India Surveys 2005-2021"
putexcel A2 = "Variable"
putexcel C2 = "NFHS-3"
putexcel D2 = "RSOC"
putexcel E2 = "NFHS-4"
putexcel F2 = "CNNS"
putexcel G2 = "NFHS-5"

local RowNum = 4
	
foreach var of varlist `ContVars' {
	tabulate `var' round [aw = national_wgt] if agemos>=6 & agemos<24, col matcell(cell) matrow(row)
	putexcel C`RowNum' = matrix(cell), nformat(##.0)
	local RowNum = `RowNum' + r(r) +1
	di `RowNum'
	}


* Table 3 
* Tables for dependent variables, max / min / amplitude / statistical significance of monthly variation
* Removed EXCEL FILES as temp calculation files.  Now only putexcel

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
putexcel G2 = "Proport- ional change"
putexcel H2 = "Stat sig variation"
putexcel I2 = "N"

local DepVars = "isssf mdd mmf_bf  mmf_nobf min_milk_freq_nbf mmf_all mad_all egg_meat zero_fv currently_bf carb leg_nut dairy all_meat egg vita_fruit_veg fruit_veg bread leafy_green potato vita_veg fortified_food yogurt vita_fruit meat organ fish"

local ContVars ib12.int_month i.state i.rururb i.wi i.mum_educ i.mum_work i.anc4plus  ///
	i.inst_birth i.bord c.age_days c.age_days#c.age_days ///
	i.sex i.cat_birth_wt i.diar i.fever i.ari i.round

local RowNum = 2

foreach var of varlist `DepVars' {
	di "`var'"
 	local RowNum = `RowNum' +1
	logit `var' `ContVars' [pw = national_wgt] if agemos>=6 & agemos<24
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
	putexcel I`RowNum' = `r(N)', nformat(#,###) // weighted N from logit

	margins int_month, saving(min_max_file, replace)
	preserve
	use min_max_file
	* Add varname to min_max_file
	gen varname = "`r(predict1_label)'"
	sum _margin, meanonly
	local min = r(min) *100
	local max = r(max) *100
	local amp = (`max'-`min')/2
	putexcel set min_max_table, modify
	putexcel D`RowNum' = `min', nformat(0.0)  		// min
	putexcel E`RowNum' = `max', nformat(0.0) 	  	// max
	putexcel F`RowNum' = `amp', nformat(number_d2)  // amplitude
	putexcel save
	restore
}






local ExportPath "C:/TEMP/Seasonality"
local FileName "Comp Feed Seasonality.docx"
di "`ExportPath'/`FileName'"

* Create word document with results
* TITLE PAGE
putdocx clear
putdocx begin, font("Calibri") 
putdocx paragraph, style(Title) halign(center) spacing(line,16 pt)
putdocx text ("Seasonality of Complementary Feedings ")
putdocx paragraph, style(Title) halign(center) spacing(line,14 pt)
putdocx text ("variables in Indian 5 Survey ")
putdocx save "`ExportPath'/`FileName'", replace
	

	
	
local ExportPath "C:/TEMP/Seasonality"
local FileName "Comp Feed Seasonality.docx"
di "`ExportPath'/`FileName'"


* DepVars



* Plot adjusted vs unadjusted estimates onto one graph

putdocx begin, font("Calibri") 

* ISSSF, mdd, mmf_all, egg_meat, dairy vita_fruit leafy_green
* mmf_all does not have NFHS-3 so need to create different code


local DepVars = "isssf mdd mmf_bf egg_meat dairy leg_nut vita_fruit leafy_green"
local DepVars = "mmf_bf leg_nut"


foreach var of varlist `DepVars' {

	* No controls are applied for month of data collection 
	logit `var' i.round  [pw = national_wgt] if agemos>=6 & agemos<24
	margins round, saving(file1, replace)

	* Adjustments are applied using control variables for month of data collection 
	local ContVars ib12.int_month i.state i.rururb i.wi i.mum_educ i.mum_work i.anc4plus  ///
		i.inst_birth i.bord c.age_days c.age_days#c.age_days ///
		i.sex i.cat_birth_wt i.diar i.fever i.ari i.round

	logit `var' `ContVars' [pw = national_wgt] 
	* output by round of survey
	margins round,  saving(file2, replace)

	preserve
	use file1, clear
	replace _at=1
	append using file2 
	replace _at=2 if _at==.
	save `var'_round, replace

 	replace _m1 = _m1-0.05 if _at==1  // offset plotting on graph

 	twoway (scatter _margin _m1 if _at==1 , msymbol(D) ) ///
 		   (rcap _ci_ub _ci_lb _m1 if _at==1) (line _margin _m1 if _at==1) ///
 		   (scatter _margin _m1 if _at==2 , msymbol(Oh) ) ///
 		   (rcap _ci_ub _ci_lb _m1 if _at==2) (line _margin _m1 if _at==2, lpattern(longdash)) ///
 			, ///
 		   title("`var'") ///
 		   xlabel(1(1)5, valuelabel ) xtitle(" ")  /// ylabel(0.4(0.1)0.7)
 		   ytitle(Proportion) legend(pos(3) ring(0) col(1)  region(lstyle(none)) ///
 		   order(1 "Unadjusted - Midpoint" 4 "Adjusted - Midpoint" )) scheme(s1mono) ///
 		   graphregion(color(white)) bgcolor(white) 
 	graph export `var'.tif, as(tif) replace
	putdocx paragraph, halign(left)
	putdocx image `var'.tif, linebreak(1)

	restore
}

* Graph for mmf_all created and saved separately - don't forget to add. 

local ExportPath "C:/TEMP/Seasonality"
local FileName "Comp Feed Seasonality.docx"
putdocx save "`ExportPath'/`FileName'", append
	


* GRAPHS
* ONE Plot adjusted estimates by month from pooled data in one graph
* TWO Plot dependent variable for each survey in five graphs for each survey

local DepVars = "isssf mdd egg_meat leg_nut dairy leafy_green vita_fruit"
// local DepVars = "mmf_all"
// forval x = 2/5 {  if using mmf_all

local ContVars ib12.int_month i.state i.rururb i.wi i.mum_educ i.mum_work  ///
	i.anc4plus i.earlyanc i.csection i.inst_birth i.bord c.age_days       ///
	c.age_days#c.age_days i.sex i.cat_birth_wt i.diar i.fever i.ari i.round

* attention to include all socio-economic vars i.wi i.mum_educ i.mum_work ***

foreach var of varlist `DepVars' {
	logit `var' `ContVars' [pw = national_wgt] if agemos>=6 & agemos<24
	margins int_month, saving(`var'_month, replace)
	marginsplot, title("`var'") ///
		ytitle("Proportion")  /// ylab(0.5(.1)0.7) yscale(range(0.5 0.7))
		name(`var', replace)
	graph export `var'_month.tif, as(tif) replace
	
	putdocx begin, font("Calibri") 
	putdocx pagebreak
	putdocx paragraph, halign(left)
	putdocx image `var'_month.tif, linebreak(1)
	
	* Put variables of interest onto one page A4 size
	// graph combine isssf mdd mmf_all  dairy vita_fruit leafy_green, xsize(11.75) ysize(7) iscale(.5) name(comb, replace)
	// graph close isssf mdd mmf_all  dairy vita_fruit leafy_green
	// graph export "CF indicators by month of data collection (pooled).png", width(6000) replace
	//
	// local ExportPath "C:/TEMP/Seasonality"
	// local FileName "Comp Feed Seasonality.docx"
	// putdocx save "`ExportPath'/`FileName'", append

	
	* Code below uses data available from dummy==x
	forval x = 1/5 {
	// forval x = 2/5 {  // if using mmf_all
		logit `var' `ContVars' [pw = national_wgt] if round==`x' & agemos>=6 & agemos<24
		margins int_month#round, saving(`var'_round`x', replace)
		local RoundValueLabel : value label round
		local GraphLabel: label `RoundValueLabel' `x'
		marginsplot, title("`GraphLabel'") ytitle("Proportion") name(file`x', replace) 
	}

	graph combine file1 file2 file3 file4 file5, xsize(6.5) ysize(2.7) iscale(.8) name(comb, replace)
	graph close file1 file2 file3 file4 file5
	graph export "`var' by month by survey.png", width(6000) replace
	
	putdocx paragraph, halign(left)
	putdocx text ("`var' by month by survey")
	putdocx image "`var' by month by survey.png", linebreak(1)

	local ExportPath "C:/TEMP/Seasonality"
	local FileName "Comp Feed Seasonality.docx"
	putdocx save "`ExportPath'/`FileName'", append
}





* Plot the predicted values of the dependent variable for each survey in five graphs for five surveys
* Combomarginsplot - joining five graphs onto one background

* Analysis of interaction between month and round
local ContVars ib12.int_month i.state i.rururb i.wi i.mum_educ i.mum_work  ///
	i.anc4plus i.earlyanc i.csection i.inst_birth i.bord c.age_days       ///
	c.age_days#c.age_days i.sex i.cat_birth_wt i.diar i.fever i.ari i.round

// local DepVars = "mdd mmf_bf mmf_nobf min_milk_freq_nbf mmf_all egg_meat carb leg_nut bread leafy_green vita_fruit"
local DepVars = "isssf mdd mmf_bf egg_meat carb leg_nut bread leafy_green vita_fruit"

foreach var of varlist `DepVars' {
	
	* Code below uses data available from dummy==x
	forval x = 1/5 {
		logit `var' `ContVars' [pw = national_wgt] if round==`x' & agemos>=6 & agemos<24
		margins int_month#round, saving(`var'_round`x', replace)
		local RoundValueLabel : value label round
		local GraphLabel: label `RoundValueLabel' `x'
		marginsplot, title("`GraphLabel'") ytitle("Proportion") name(file`x', replace) 
	}
// 	* Code uses all entire regression estimation sample to calculate the margins, with the value of dummy temporarily reset to x in every observation.
// 	* Do we want to use the pooled data or the exclusive survey 
// 	* calculates one trend and applies variation for each round
// 	logit `var' `ContVars' [pw = national_wgt] 
// 	forval x = 1/5 {
// 			margins int_month, at(round==`x') saving(`var'_round`x', replace)
// 			local RoundValueLabel : value label round
// 			local GraphLabel: label `RoundValueLabel' `x'
// 			marginsplot, title("`GraphLabel'") ytitle("Proportion") name(file`x', replace) 
// 		}
//	
	graph combine file1 file2 file3 file4 file5, xsize(6.5) ysize(2.7) iscale(.8) name(comb, replace)
	graph close file1 file2 file3 file4 file5
	graph export "`var' by month by survey.png", width(6000) replace
	
	putdocx begin, font("Calibri") 
	putdocx pagebreak
	putdocx paragraph, halign(left)
	putdocx text ("`var' by month by survey")
	putdocx image "`var' by month by survey.png", linebreak(1)

	local ExportPath "C:/TEMP/Seasonality"
	local FileName "Comp Feed Seasonality.docx"
	putdocx save "`ExportPath'/`FileName'", append
}

* Some vars do not show clear seasonal trend but sig differences by month by survey


* By REGION
* Analysis of interaction between month and round
local ContVars ib12.int_month i.region i.rururb i.wi i.mum_educ i.mum_work  ///
	i.anc4plus i.earlyanc i.csection i.inst_birth i.bord c.age_days       ///
	c.age_days#c.age_days i.sex i.cat_birth_wt i.diar i.fever i.ari i.round

local DepVars = "vita_fruit"

foreach var of varlist `DepVars' {
	
	* Code below uses data available from dummy==x
	forval x = 1/5 {
		logit `var' `ContVars' [pw = national_wgt] if round==`x' & agemos>=6 & agemos<24
		margins int_month#round, saving(`var'_round`x', replace)
		local RoundValueLabel : value label round
		local GraphLabel: label `RoundValueLabel' `x'
		marginsplot, title("`GraphLabel'") ytitle("Proportion") name(file`x', replace) 
	}

	graph combine file1 file2 file3 file4 file5, xsize(6.5) ysize(2.7) iscale(.8) name(comb, replace)
	graph close file1 file2 file3 file4 file5
	graph export "`var' by month by survey.png", width(6000) replace
	
	putdocx begin, font("Calibri") 
	putdocx pagebreak
	putdocx paragraph, halign(left)
	putdocx text ("`var' by month by survey")
	putdocx image "`var' by month by survey.png", linebreak(1)

	local ExportPath "C:/TEMP/Seasonality"
	local FileName "Comp Feed Seasonality.docx"
	putdocx save "`ExportPath'/`FileName'", append
}






* Trend of vita_fruit evident in NFHS-3 weighted & unadjusted.  
tab  int_month vita_fruit [aw = national_wgt] if round==1, row
gen vita_fruit_x = vita_fruit*100
graph bar (mean) vita_fruit_x [aw = national_wgt] if round==1, over(int_month) 

* With Margins adjustment for month of data collection - same trend found
logit vita_fruit ib12.int_month [pw = national_wgt] if round==1 & agemos>=6 & agemos<24
margins int_month
marginsplot

* The state covariate in the control variables is reversing the trend in NFHS-3.
* replacement of state with region shows seasonal trend
local ContVars ib12.int_month i.region i.rururb i.wi i.mum_educ i.mum_work  ///
	i.anc4plus i.earlyanc i.csection i.inst_birth i.bord c.age_days       ///
	c.age_days#c.age_days i.sex i.cat_birth_wt i.diar i.fever i.ari i.round
logit vita_fruit `ContVars' [pw = national_wgt] if round==1 & agemos>=6 & agemos<24
margins int_month
marginsplot

* What is difference between state in NFHS-3 and 4 & 5 ?
tabulate state int_month if round==3
cap drop counts
// bysort state int_month: egen counts =count(one) if round==1
bysort state int_month: egen counts =count(one) if round==3
scatter state int_month [w=counts*0.5] , msymbol(circle_hollow)
* District level representation demands more sample in larger states and less impact of weights


* For data to include in Excel Trend Graph
* see exported data in var_roundX

* NFHS-5
* start 6 2019, mid   5 2020, end   5 202



* dependent variables
* isssf mdd mmf_bf mmf_nobf min_milk_freq_nbf mmf_all egg_meat carb leg_nut bread leafy_green vita_fruit


* Variables that represent data from date of data collection
local ContVars ib12.int_month i.state i.rururb i.wi i.mum_educ i.mum_work  ///
	i.anc4plus i.earlyanc i.csection i.inst_birth i.bord c.age_days       ///
	c.age_days#c.age_days i.sex i.cat_birth_wt i.diar i.fever i.ari i.round

logit mdd `ContVars' [pw = national_wgt] if agemos>=6 & agemos<24
margins int_month
pwcompare int_month, effects sort mcompare(sidak)


// ------------------------------------------------------------------------------
//              |                              Sidak                Sidak
//              |   Contrast   Std. err.      z    P>|z|     [95% conf. interval]
// -------------+----------------------------------------------------------------
* isssf
 Oct vs Aug  |  -.4742758   .1302143    -3.64   0.018    -.9119139   -.0366376
 Nov vs Aug  |  -.7076406   .1536165    -4.61   0.000    -1.223931   -.1913498
 Dec vs Aug  |  -.5580483   .1260824    -4.43   0.001    -.9817994   -.1342972

* mdd
 Mar vs Jan  |   -.163935   .0368515    -4.45   0.001    -.2877894   -.0400805
 Apr vs Jan  |  -.1986808   .0438985    -4.53   0.000    -.3462197    -.051142
 Jun vs Jan  |   -.246336    .046475    -5.30   0.000    -.4025341    -.090138
 Aug vs Jan  |  -.2095673   .0522344    -4.01   0.004    -.3851223   -.0340124
 Sep vs Jan  |  -.1993607   .0502256    -3.97   0.005    -.3681645    -.030557
 Oct vs Jan  |  -.2895812   .0552258    -5.24   0.000    -.4751899   -.1039725
 Oct vs Feb  |  -.1779109   .0526923    -3.38   0.047    -.3550047    -.000817 
 Jul vs Jun  |   .1765761   .0458799     3.85   0.008      .022378    .3307741
 Oct vs Jul  |  -.2198212    .054595    -4.03   0.004      -.40331   -.0363325
 Dec vs Oct  |   .2040973   .0605449     3.37   0.048     .0006115    .4075831

* mmf_bf
 Oct vs Jan  |   .1947537   .0524793     3.71   0.014     .0183755    .3711318
 Apr vs Feb  |   .1427144   .0321866     4.43   0.001     .0345382    .2508907
 Jun vs Feb  |   .1445441   .0355784     4.06   0.003     .0249684    .2641198
 Jul vs Feb  |   .1588616   .0363639     4.37   0.001     .0366461    .2810772
 May vs Feb  |   .1712972   .0358517     4.78   0.000     .0508029    .2917916
 Aug vs Feb  |   .1903868   .0452434     4.21   0.002      .038328    .3424456
 Sep vs Feb  |   .2305752   .0437099     5.28   0.000     .0836702    .3774802
 Oct vs Feb  |   .2830762   .0502495     5.63   0.000     .1141924      .45196 
 May vs Mar  |   .1326993    .035095     3.78   0.010     .0147484    .2506502
 Sep vs Mar  |   .1919773   .0441941     4.34   0.001     .0434448    .3405097
 Oct vs Mar  |   .2444782   .0506671     4.83   0.000     .0741909    .4147656

* mmf_nobf
None
* min_milk_freq_nbf
None
* mmf_all
* driven by mmf_bf

* egg_meat
 Jun vs Jan  |   -.325737   .0523798    -6.22   0.000    -.5017807   -.1496932
 Aug vs Jan  |  -.2157781   .0568781    -3.79   0.010    -.4069402    -.024616
 Sep vs Jan  |  -.2067607    .054472    -3.80   0.010    -.3898359   -.0236855  
 Oct vs Jan  |   -.211089   .0577469    -3.66   0.017    -.4051708   -.0170071
 
 Jun vs Feb  |  -.2473868   .0481652    -5.14   0.000    -.4092656    -.085508
 Dec vs Feb  |   .1879838    .050331     3.73   0.012     .0188259    .3571417
 
 Jun vs Mar  |  -.2072324   .0469079    -4.42   0.001    -.3648854   -.0495794
 Dec vs Mar  |   .2281381   .0523361     4.36   0.001     .0522414    .4040348
 
 Dec vs Apr  |   .2761251   .0599982     4.60   0.000     .0744767    .4777734
 Jun vs May  |  -.1975543   .0504816    -3.91   0.006    -.3672184   -.0278903
 Dec vs May  |   .2378162   .0597742     3.98   0.005     .0369206    .4387119
 
 Jul vs Jun  |   .1915589   .0505238     3.79   0.010     .0217531    .3613646
 Nov vs Jun  |   .2905348   .0721064     4.03   0.004     .0481918    .5328777
 Dec vs Jun  |   .4353706   .0598583     7.27   0.000     .2341922    .6365489
 
 Dec vs Jul  |   .2438117   .0562997     4.33   0.001     .0545936    .4330298
 Dec vs Aug  |   .3254117    .062037     5.25   0.000      .116911    .5339124
 Dec vs Sep  |   .3163943   .0599104     5.28   0.000      .115041    .5177476
 Dec vs Oct  |   .3207225   .0631358     5.08   0.000      .108529     .532916
 
*carb 
* driven by bread

*leg_nut 
 Feb vs Jan  |  -.1973276   .0310362    -6.36   0.000    -.3016374   -.0930178
 Mar vs Jan  |  -.2194427   .0346774    -6.33   0.000    -.3359903   -.1028952
 Apr vs Jan  |  -.2740063   .0432736    -6.33   0.000    -.4194448   -.1285677
 May vs Jan  |  -.2566528   .0477654    -5.37   0.000     -.417188   -.0961176
 Jun vs Jan  |  -.3444443   .0470733    -7.32   0.000    -.5026534   -.1862352

*bread 
Driven by one outlier in August

*leafy_green 
 Jun vs Jan  |  -.2134515   .0381972    -5.59   0.000    -.3418285   -.0850744
 Jul vs Jan  |  -.2743396   .0388525    -7.06   0.000    -.4049192   -.1437601
 Aug vs Jan  |   -.266855   .0457171    -5.84   0.000    -.4205059   -.1132041
 Sep vs Jan  |  -.2271553   .0435624    -5.21   0.000    -.3735646   -.0807461 
 Oct vs Jan  |  -.2824013   .0488954    -5.78   0.000    -.4467342   -.1180685

*vita_fruit
 Mar vs Jan  |   .2553908   .0403868     6.32   0.000     .1196544    .3911272
 Apr vs Jan  |   .3890403   .0464735     8.37   0.000     .2328472    .5452333
 May vs Jan  |   .7736428   .0484042    15.98   0.000     .6109608    .9363247
 Jun vs Jan  |   1.093174    .045551    24.00   0.000     .9400811    1.246266
 Jul vs Jan  |    .892022   .0457121    19.51   0.000      .738388    1.045656 
 Aug vs Jan  |   .3158437   .0571951     5.52   0.000     .1236162    .5080712
 Sep vs Jan  |   .1951492   .0552251     3.53   0.027     .0095428    .3807555
 

 
 



 
* FOR ANNEXES


* EXPLORE # of Dietary Diversity and Number of Feedings
* no strong evidence of seasonal variation
putdocx begin, font("Calibri") 

local DepVars = "sumfoodgrp_nm freq_solids_nm"

local ContVars ib12.int_month i.state i.rururb i.wi i.mum_educ i.mum_work  ///
	i.anc4plus i.earlyanc i.csection i.inst_birth i.bord c.age_days       ///
	c.age_days#c.age_days i.sex i.cat_birth_wt i.diar i.fever i.ari i.round

* attention to include all socio-economic vars i.wi i.mum_educ i.mum_work ***

foreach var of varlist `DepVars' {
	reg `var' `ContVars' [pw = national_wgt] if agemos>=6 & agemos<24
	margins int_month, saving(`var'_month, replace)
	marginsplot, title("`var' by month of data collection (pooled)") ///
		ytitle("Proportion")  /// ylab(0.5(.1)0.7) yscale(range(0.5 0.7))
		name(`var', replace)
	graph export `var'_month.tif, as(tif) replace

	putdocx pagebreak
	putdocx paragraph, halign(left)
	putdocx image ebf_month.tif, linebreak(1)

}

* A4 size
graph combine sumfoodgrp_nm freq_solids_nm , xsize(6.5) ysize(2.7) iscale(.8) name(comb, replace)
graph close sumfoodgrp_nm freq_solids_nm
graph export "Dietary diversity and food frequency indicators by month by survey.png", width(6000) replace

local ExportPath "C:/TEMP/Seasonality"
local FileName "Comp Feed Seasonality.docx"
putdocx save "`ExportPath'/`FileName'", append




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



* First three regions		
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

logit ebf_x `ContVars' [pw = national_wgt]  if agemos>=6 & agemos<24
	
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

logit ebf_x `ContVars' [pw = national_wgt] if agemos>=6 & agemos<24
	
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

logit ebf_x `ContVars' [pw = national_wgt] if agemos>=6 & agemos<24
	
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

logit ebf_x `ContVars' [pw = national_wgt] if agemos>=6 & agemos<24
	
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











* ANNEXES 

* Complementary feeding indicators by month of data collection
putdocx begin, font("Calibri") 

local ExportPath "C:/TEMP/Seasonality"
local FileName "Comp Feed Seasonality.docx"

* Seasonality of depvar01
local depvar01 carb leg_nut dairy all_meat egg vita_fruit_veg fruit_veg ///
	fortified_food meat bread potato vita_veg leafy_green vita_fruit ///
	organ fish yogurt semisolid any_solid_semi_food mdd mmf_bf mmf_nobf /// 
	min_milk_freq_nbf mmf_all mad_all egg_meat zero_fv
	
* Variables that represent data from date of data collection
local ContVars ib12.int_month i.state i.rururb i.wi i.mum_educ i.mum_work i.anc4plus i.inst_birth ///
	i.bord c.age_days c.age_days#c.age_days i.sex i.cat_birth_wt i.diar i.fever i.ari i.round

foreach var in `depvar01' {
	di "`var'"
		
	logit `var' `ContVars' [pw = national_wgt] 
	margins int_month
	marginsplot, title(`var' by month of data collection) ytitle("Proportion") 
	graph export `var'.tif, as(tif) replace
	
	putdocx begin, font("Calibri") 
	putdocx paragraph, halign(center)
	putdocx image "`var'.tif"
	putdocx save "`ExportPath'/`FileName'", append
}


local ExportPath "C:/TEMP/Seasonality"
local FileName "Comp Feed Seasonality.docx"

* Seasonality of depvar_100
local depvar_100 sumfoodgrp freq_solids milk_feeds feeds
	
* Variables that represent data from date of data collection
local ContVars ib12.int_month i.state i.rururb i.wi i.mum_educ i.mum_work i.anc4plus i.inst_birth ///
	i.bord c.age_days c.age_days#c.age_days i.sex i.cat_birth_wt i.diar i.fever i.ari i.round

foreach var in `depvar_100' {
	di "`var'"

	reg `var' `ContVars' [pw = national_wgt] if agemos>=6 & agemos<24
	margins int_month
	marginsplot, title(`var' by month of data collection) ytitle("Proportion") 
	graph export `var'.tif, as(tif) replace
	
	putdocx begin, font("Calibri") 
	putdocx paragraph, halign(center)
	putdocx image "`var'.tif"
	putdocx save "`ExportPath'/`FileName'", append
}


local ExportPath "C:/TEMP/Seasonality"
local FileName "Comp Feed Seasonality.docx"
putdocx save "`ExportPath'/`FileName'", append










* Continued breastfeeding
putdocx begin, font("Calibri") 	

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
local FileName "Comp Feed Seasonality.docx"
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



END




* Test
 sum poultry if round==1
 * attempted to insert corrected formatting of result into cell
 di "`r(mean)'"

// local Prev = cond(missing(`r(mean)'),"-",string("`r(mean)'"))
//  if  "`r(mean)'" == "" {
//      local Prev  = "-"
//  }
//  else {
//      local Prev  = round(`r(mean)', .1)
//  }
 di "`Prev'"

 local Obs =cond(`r(N)'<=1,"-", string(`r(N)'))
 di "`Obs'" 




* Merge graphs together
graph combine graph1 graph2 , xsize(6.5) ysize(2.7) iscale(.8) name(comb, replace)
graph close ebf_unadj_adj_mar water_unadj_adj_mar 
graph export "X&Y.png", width(6000) replace

* Add combomarginsplot to word file
putdocx begin, font("Calibri") 
putdocx pagebreak
putdocx paragraph, halign(left)
putdocx image X.tif, linebreak(1)
putdocx image Y.tif, linebreak(1)

putdocx save "`ExportPath'/`FileName'", append


