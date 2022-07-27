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
include "C:\Users\stupi\OneDrive - UNICEF\1 UNICEF Work\1 moved to ECM\IIT-B\IYCF\analysis\robert_paths.do"

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
replace isssf = . if int_month==5 & round ==5  	// 1 case
replace mdd =. if int_month==11 & round ==2 	// 2 cases
replace mdd =. if int_month==6 & round ==2		// 3 cases
replace dairy =. if int_month==6 & round ==2	// 3 cases
replace dairy =. if int_month==11 & round ==2	// 4 cases
replace leafy_green =. if int_month==6 & round ==2	// 3 cases
replace leafy_green =. if int_month==11 & round ==2	// 4 cases
cap drop agegrp_3
gen agegrp_3 = floor(agemos/6) if agemos>=6 & agemos<24
la def agegrp 1 "6-11 M" 2 "12-17 M" 3 "18-23 M"
la val agegrp_3 agegrp
tab agemos agegrp_3, m 
graph bar (count) one if agemos>=4 & agemos<28, over(agemos) 
* evidence of age preference for whole numbers 

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

local DepVars currently_bf carb leg_nut dairy all_meat egg vita_fruit_veg fruit_veg  fortified_food meat bread potato vita_veg leafy_green vita_fruit organ fish yogurt semisolid any_solid_semi_food mdd mmf_bf  mmf_nobf min_milk_freq_nbf mmf_all mad_all egg_meat zero_fv
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
	tabulate `var' round [aw = national_wgt] if agemos>=6 & agemos<=23, col matcell(cell) matrow(row)
	putexcel C`RowNum' = matrix(cell), nformat(##.0)
	local RowNum = `RowNum' + r(r) +1
	di `RowNum'
	}

















* Table 3
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

local DepVars = "isssf mdd mmf_bf  mmf_nobf min_milk_freq_nbf mmf_all mad_all egg_meat zero_fv currently_bf carb leg_nut dairy all_meat egg vita_fruit_veg fruit_veg bread leafy_green potato vita_veg fortified_food yogurt vita_fruit meat organ fish"
// local DepVars = "isssf"


local ContVars ib12.int_month i.state i.rururb i.wi i.mum_educ i.mum_work i.anc4plus  ///
	i.inst_birth i.bord c.age_days c.age_days#c.age_days ///
	i.sex i.cat_birth_wt i.diar i.fever i.ari i.round

local RowNum = 2

foreach var of varlist `DepVars' {
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




* For each change in age groups. 



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
* add corrections for selection of correct denominators
// cap drop ebf_x
// gen ebf_x = ebf*100 if agemos <6
// tab agemos ebf_x 
// tab round ebf_x


* Plot adjusted vs unadjusted estimates onto one graph

putdocx begin, font("Calibri") 

* ISSSF, mdd, mmf_all, egg_meat, dairy vita_fruit leafy_green
* mmf_all does not have NFHS-3 so need to create different code


local DepVars = "isssf mdd egg_meat dairy vita_fruit leafy_green"
// local DepVars = "mmf_all"

foreach var of varlist `DepVars' {

	* No controls are applied for month of data collection 
	logit `var' i.round  [pw = national_wgt] 
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
 		   ytitle(Proportion) legend(pos(5) ring(0) col(1)  region(lstyle(none)) ///
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
putdocx save "`ExportPath'/`FileName'", replace
	



* Merge graphs together
graph combine graph1 graph2 , xsize(6.5) ysize(2.7) iscale(.8) name(comb, replace)
graph close ebf_unadj_adj_mar water_unadj_adj_mar 
graph export "EBF and giving water unadjusted, adjusted and set to March as month of data collection.png", width(6000) replace

* Add combomarginsplot to word file
putdocx begin, font("Calibri") 
putdocx pagebreak
putdocx paragraph, halign(left)
putdocx image ebf_unadj_adj_mar.tif, linebreak(1)
putdocx image water_unadj_adj_mar.tif, linebreak(1)

putdocx save "`ExportPath'/`FileName'", append





* Plot adjusted estimates by month from pooled data in one graph

putdocx begin, font("Calibri") 

local DepVars = "isssf mdd mmf_all dairy vita_fruit leafy_green"
// local DepVars = "mmf_all"

local ContVars ib12.int_month i.state i.rururb i.wi i.mum_educ i.mum_work  ///
	i.anc4plus i.earlyanc i.csection i.inst_birth i.bord c.age_days       ///
	c.age_days#c.age_days i.sex i.cat_birth_wt i.diar i.fever i.ari i.round

* attention to include all socio-economic vars i.wi i.mum_educ i.mum_work ***

foreach var of varlist `DepVars' {
	logit `var' `ContVars' [pw = national_wgt] 
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
graph combine isssf mdd mmf_all  dairy vita_fruit leafy_green, xsize(11.75) ysize(7) iscale(.5) name(comb, replace)
graph close isssf mdd mmf_all  dairy vita_fruit leafy_green
graph export "CF indicators by month by survey.png", width(6000) replace

local ExportPath "C:/TEMP/Seasonality"
local FileName "Comp Feed Seasonality.docx"
putdocx save "`ExportPath'/`FileName'", append




* Plot the predicted values of the dependent variable for each survey in five graphs for five surveys
* Combomarginsplot - joining five graphs onto one background

* Analysis of interaction between month and round
local ContVars ib12.int_month i.state i.rururb i.wi i.mum_educ i.mum_work  ///
	i.anc4plus i.earlyanc i.csection i.inst_birth i.bord c.age_days       ///
	c.age_days#c.age_days i.sex i.cat_birth_wt i.diar i.fever i.ari i.round

local DepVars = "isssf mdd egg_meat dairy vita_fruit leafy_green"
local DepVars = "egg_meat dairy vita_fruit leafy_green"
* mmf_all

foreach var of varlist `DepVars' {
	
	* Code below uses data available from dummy==x
	forval x = 1/5 {
		logit `var' `ContVars' [pw = national_wgt] if round==`x' 
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


* Trend of vita_fruit evident in NFHS-3 unweighted.  Check weighted and adjusted








// marginsplot yaxis
// 	ylab(0.08(.02)0.22) yscale(range(0.08 0.22))

* For Excel Graph
* see exported data in ebf_roundX

* NFHS-5
* start 6 2019, mid   5 2020, end   5 202



* Feeding Egg & Meat - driven by early survey? - put in Annex

	
1


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
	reg `var' `ContVars' [pw = national_wgt] 
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



