* Make IYCF Variables for CNNS data

version 16

// check currently BF
// carb = fortified_food - double check WHO guidance
// Introduction to the semi_solid, solid, soft_food in children from 6-8 months of age - check official WHO definition
* LBW
* Mothers education 
 
include "C:\Users\stupi\OneDrive - UNICEF\1 UNICEF Work\1 moved to ECM\IIT-B\IYCF\analysis\robert_paths.do"
// include "dnyaneshwar_paths.do"

use "CNNS_04_regional_wgts", clear
rename caseid case_id
save C:\Temp\Data\reg_wgt_temp, replace

* Open CNNS
use `CNNS', clear

* * * 
*DROP ALL PREVIOUSLY CONSTRUCTED IYCF VARS
drop everbf breast1h colstrum breast24h cbf compfed minmealfbf minmealf ///
minmealfnbf group1_cdd group2_cdd group3_cdd group4_cdd group5_cdd group6_cdd ///
group7_cdd childdd childdd_nbf minaccdiet minaccdietbf minaccdietnbf ironfood food_con_n age_c
* * * 

* make IYCF vars.do

gen one=1
lab define no_yes 0 "No" 1 "Yes"

gen psu = psu_no_ov
gen hh_num = hh_no

* CHECK FINAL N of CNNS 
tab result
drop if result !=1
* Complete N  38,060  

* Change from age in months to age in days
* One year = 365.25 days
* Ages in month by day ranges
* 6 - 23 M  (183 - 730)  // 
* Under 24 M
* 6 - 8 M   (183 - 243)
* 9 -23 M   (243 - 730)
* 6-11 M    (183 - 335)
* 12 - 23 M (335 - 730)
* Underfive (< 1825)


gen birthday =  q103d

replace birthday = 15 if q103d > 31
tab birthday
* By following WHO recommendations on anthro data, we have created heaping on 15th of month

gen birthmonth = q103m
gen birthyear = q103y

* Age in days
gen int_date = mdy(int_m , int_d , int_y)
format int_date %td

gen dateofbirth = mdy(q103m , q103d , q103y)
format dateofbirth %td
gen age_days = int_date - dateofbirth 
replace age_days =. if age_days>1825
tab age_days,m 

* If children are less than 1 month old, double check 15th of month as setting for day of birth. 
list age_days dateofbirth int_date if age_days<0


// cap drop count
// bysort age_days: egen count = count(age_days)
// line count age_days
// scatter count age_days
gen agemos = floor(age_days/30.4375)
tab agemos, m 

* Ever breastfed (children born in past 24 months)
gen evbf = 0
replace evbf=1 if q301==1
replace evbf=. if age_days>=730 
la var evbf "Ever breastfed (children born in past 24 months)"
tab evbf q301, m

*Early initiation of Breastfeeding (children born in last 24 months breastfed within 1hr)
gen eibf = 0
replace eibf = 1 if (q302n <=1 & q302u == 1) | (q302u==0)   //here q302u=0 indicates the response as "immediately"

* Harmonize to NFHS
// replace eibf = 1 if m34 == 0 | m34==100  ///  0 -immediately, 100 - within 30min to 1hr 
//  101 =  one hour and more

replace eibf =. if age_days>=730 // age in days
tab  q302n eibf, m
tab  q302u eibf, m

*Timing of initiation of Breastfeeding 
gen eibf_timing =. 
replace eibf_timing = 0 if q302u==0
replace eibf_timing = q302n if q302u==1
replace eibf_timing = q302n*24 if q302u==2
replace eibf_timing = 48 if eibf_timing>48
replace eibf_timing =.  if age_days>=730
la var eibf_timing "Timing of start of breastfeeding (in hours)"
la def eibf_timing 48 "48 hours or more"
la val eibf_timing eibf_timing
tab eibf_timing
tab  q302n eibf_timing, m
tab  q302u eibf_timing, m

* Exclusively breastfed for the first two days after birth
* Percentage of children born in the last 24 months who were fed exclusively with breast milk for the first two days after birth
* q303 Was [NAME] given anything to drink other than breast milk within the first three days after delivery?
* The CNNS did not ask the question in the best method, so we are trying to edit to represent 2 days. 

tab q303
la list q303
recode q303 (2 = 1)(1=0)(8=.), gen(ebf3d)
la var ebf3d "Given nothing in 1st 3 days but BM"
tab ebf3d q303, m

cap drop ebf2d
gen ebf2d =0
replace ebf2d=1 if q303!=1 // q303!=1 means no other drink was given to the child in 1st three days
replace ebf2d =. if age_days >2
* Question was not asked correctly so variable will only be valid at national level 
* 82 children in sample <= 2 days
tab ebf2d q303, m 

* Currently Breastfeeding (Child was breastfed yesterday)
* Pay attention to order of assignment of currently BF
// 1. ever breastfed
// 2. still breastfeeding
// 3. breastfed yesterday
tab q301
tab q306
tab q307

cap drop currently_bf
gen currently_bf = .
replace currently_bf = 0 if q301 !=1
replace currently_bf = 0 if q306 ==2
replace currently_bf = 1 if q307 ==1
replace currently_bf = 0 if q307 >=2

la var currently_bf "Currently breastfeeding"
tab currently_bf, m 
tab currently_bf q307, m 
tab currently_bf q306, m 
tab currently_bf q301, m 


* Prelacteal feeds
* What was [NAME] given to drink? within first three days after delivery
tab q303, m 
cap drop prelacteal*
gen prelacteal_milk       = cond(strpos(q304,"A"), 1, 0)
gen prelacteal_water      = cond(strpos(q304,"B"), 1, 0)
gen prelacteal_sugarwater = cond(strpos(q304,"C"), 1, 0)
gen prelacteal_gripewater = cond(strpos(q304,"D"), 1, 0)
gen prelacteal_saltwater  = cond(strpos(q304,"E"), 1, 0)
gen prelacteal_fruitjuice = cond(strpos(q304,"F"), 1, 0)
gen prelacteal_formula    = cond(strpos(q304,"G"), 1, 0)
gen prelacteal_honey      = cond(strpos(q304,"H"), 1, 0)
gen prelacteal_janamghuti = cond(strpos(q304,"I"), 1, 0)
gen prelacteal_tea        = cond(strpos(q304,"J"), 1, 0) 
gen prelacteal_other      = cond(strpos(q304,"X"), 1, 0)



cap drop prelacteal_otherthanmilk
gen prelacteal_otherthanmilk =0
local prelacteal = "prelacteal_water prelacteal_sugarwater prelacteal_gripewater prelacteal_saltwater prelacteal_fruitjuice prelacteal_formula prelacteal_honey prelacteal_janamghuti prelacteal_tea prelacteal_other"
foreach var in `prelacteal' { 
	replace prelacteal_otherthanmilk = 1 if  `var'==1
}
local prelacteal_feeds = "prelacteal_milk prelacteal_water prelacteal_sugarwater prelacteal_gripewater prelacteal_saltwater prelacteal_fruitjuice prelacteal_formula prelacteal_honey prelacteal_janamghuti prelacteal_tea prelacteal_other"
foreach var in `prelacteal_feeds' { 
	replace `var' = . if  age_days>=730
}

tab  q304 prelacteal_milk
tab  prelacteal_milk
tab prelacteal_otherthanmilk
tab  q304 prelacteal_otherthanmilk

*Colostrum Feeding
gen colostrum = 0
replace colostrum = 1 if q305 == 1 
replace colostrum=. if age_days>=730
tab  q305 colostrum, m

* bottle feeding (did child drink anything from bottle)
gen bottle =0
replace bottle =1 if q308 == 1
replace bottle=. if age_days>=730
tab  q308 bottle, m


/*
                     value
variable name        label      variable label
--------------------------------------------------------------------------------------------
q301                 q301       q301: whether index child ever breastfed
q302u                q302u      q302u: time after birth to put index child to breast first time-unit
q302n                           q302n: time after birth to put index child to breast first time-number
q303                 q303     * q303: whether index child given anything other than breast milk in first three days
q304     str244                 q304: drink given
q304o    str244                 q304o: drink given- other
q305                 q305       q305: mother fed colostrum/ khees
q306                 q306       q306: whether still breasfeeding the index child
q307                 q307       q307: whether index child breastfed previous day
q308                 q308     * q308: did [name] drink anything from a bottle, with a nipple yesterday during da
q309                 q309       q309: for how many months did the mother exclusively breastfeed to [name]?
q310a                q310a      q310a: index child consumed: plain water
q310b                q310b      q310b: index child consumed: juice/drinks
q310c                q310c      q310c: index child consumed: clear broth
q310d1               q310d1   * q310d1: index child consumed: milk such as tinned, powdered, or fresh animal mil
q310d2                          q310d2. if yes: how many times did (name) drink milk?
q310e1               q310e1     q310e1: index child consumed: infant formula
q310e2                          q310e2. if yes: how many times did (name) drink infant formula?
q310f                q310f      q310f: index child consumed: any other liquid
q310g1               q310g1     q310g1: index child consumed: any yogurt/curd
q310g2                          q310g2. if yes: how many times did (name) eat yogurt?
q310h                q310h      q310h. any commercially fortified food
q310i                q310i    * q310i. any bread, roti, chapati, rice, noodles, biscuits, idli, porridge or any
q310j                q310j      q310j. any pumpkin, carrot, squash or sweet potato that is yellow or orange?
q310k                q310k      q310k. any white potato, white yam, cassava, or other food made from roots?
q310l                q310l      q310l. any dark green leafy vegetables?
q310m                q310m      q310m. any ripe mango, papaya, cantaloupe or jackfruit?
q310n                q310n      q310n. any other fruits or vegetables?
q310o                q310o      q310o. any liver, kidney, heart or other organ meat?
q310p                q310p      q310p. any chicken, duck, or other birds?
q310q                q310q      q310q. any other meat?
q310r                q310r      q310r. any eggs?
q310s                q310s      q310s. any fresh or dried fish or shellfish?
q310t                q310t      q310t. any foods made from beans, peas, lentils, or nuts?
q310u                q310u      q310u. any cheese or other food made from milk?
q310v                q310v      q310v. any oil, ghee/butter?
q310w                q310w      q310w. any other solid, semi-solid, or soft food?
q311                 q311     * q311: did [name] eat any solid, semisolid, or soft foods yesterday during the da
q312                 q312       q312: number of times index child ate solid, semi solid, soft foods yesterday
q313                 q313       q313. was [name] given any multi-vitamin tablets/syrup during the last 1 month?
q314                 q314     * q314. how frequently in last one month, did [name] consume multi-vitamin tablets
q317                 q317       q317. has [name] received vitamin ?a? dose in the last six months?
q319                 q319       q319. was a dose of deworming given to [name] in the last six months?
*/

********************************************************************************
* Food groups (liquids and solids)
********************************************************************************

* recode foods into food groups
* 	yes = 1, 
* 	no and don't know = 0
* following global guidance on IYCF analysis, this allows for maximium children to be included in indicator 

// list all variables excluding frequencies put in varlist
// local varlist = 

foreach var of varlist q310a- q310w {
	recode `var' (1=1) (2 8=0) , gen(`var'_rec)
	lab val `var' no_yes
}
* LIQUIDS
clonevar water			=q310a_rec
clonevar juice			=q310b_rec
clonevar broth			=q310c_rec
clonevar milk			=q310d1_rec
clonevar formula 		=q310e1_rec
clonevar other_liq 		=q310f_rec
* SOLIDS SEMISOLIDS
clonevar yogurt			=q310g1_rec
clonevar fortified_food =q310h_rec  // any commercially fortified food
clonevar bread 			=q310i_rec  // any bread, roti, chapati, rice, noodles, biscuits, idli, porridge 
clonevar vita_veg		=q310j_rec
clonevar potato 		=q310k_rec  // any white potato, white yam, cassava or other food made from roots
clonevar leafy_green	=q310l_rec
clonevar vita_fruit		=q310m_rec
clonevar fruit_veg		=q310n_rec
clonevar organ			=q310o_rec
clonevar poultry		=q310p_rec
clonevar meat			=q310q_rec
clonevar egg			=q310r_rec
clonevar fish			=q310s_rec
clonevar leg_nut		=q310t_rec  // beans, peas, lentils, nuts
clonevar cheese			=q310u_rec  // any cheese or other food made from milk?
*clonevar fat			=q310v_rec  // oil, ghee, butter
clonevar semisolid		=q310w_rec  // any other solid, semi-solid or soft food

lab var bread "Bread, rice, biscuits, idli, porridge"
lab val bread bread

* Define eight food groups following WHO recommended IYCF indicators
gen carb = 0
replace carb = 1 if bread ==1 | potato==1 | fortified_food==1
lab var carb "1: Bread, rice, grains, roots and tubers"
// lab val carb carb1

// WHO guidance
// thin porridge
// Porridge, bread, rice, noodles, or other foods made from grains
// White potatoes, white yams, manioc, cassava, or any other foods made from roots

lab var leg_nut "2: Legumes and nuts"
// lab val leg_nut leg_nut1

gen dairy = 0
replace dairy = 1 if formula ==1 | yogurt==1 | cheese==1 | milk==1
lab var dairy "3: Dairy - milk, formula, yogurt, cheese"
// lab val dairy dairy1
 
gen all_meat = 0
replace all_meat = 1 if organ ==1 | poultry ==1 | meat ==1 | fish ==1  
lab var all_meat "4: Flesh foods(meat, fish, poultry and liver/organ meats)"
// lab val all_meat all_meat

lab var egg "5: Eggs"
// lab val egg egg1

gen vita_fruit_veg = 0
replace vita_fruit_veg = 1 if vita_fruit ==1 | vita_veg==1 | leafy_green==1
lab var vita_fruit_veg "6: Vitamin A rich fruits and vegetables"
// lab val vita_fruit_veg vita_fruit_veg1

lab var fruit_veg "7: Other fruits and vegetables"
// lab val fruit_veg fruit_veg1

foreach var of varlist carb dairy all_meat vita_fruit_veg {
	lab val `var' no_yes
}

* Test all 8 groups including breastmilk as one of the 8 groups
* DO NOT USE breast_milk as variable.  Use currently_bf

foreach var of varlist carb dairy all_meat egg vita_fruit_veg fruit_veg currently_bf {
	tab `var' , m
}

* Age groups in blocks of 6 months
gen agegroup = floor(age_days/183 +1)   // agemons/6 +1
lab def agegroup 1 "0-5m" 2 "6-11m" 3 "12=17m" 4 "18-23m" 5 "24-29m" 6 "30-35m" 7 "36-41m" 8 "42-47m" 9 "48-53m" 10 "54-59m"
lab val agegroup agegroup

* Number of food groups out of eight - includes currently breastfeeding
cap drop sumfoodgrp
egen sumfoodgrp = rowtotal (carb leg_nut dairy all_meat egg vita_fruit_veg fruit_veg currently_bf)
tabulate sumfoodgrp, generate(fg)
rename (fg1 fg2 fg3 fg4 fg5 fg6 fg7 fg8 fg9) ///
	   (fg0 fg1 fg2 fg3 fg4 fg5 fg6 fg7 fg8)		

* Any solid/semi-solid food consumption -  Does NOT include currently breastfeeding
cap drop any_solid_semi_food
egen any_solid_semi_food = rowtotal (carb leg_nut dairy all_meat egg vita_fruit_veg fruit_veg semisolid)
replace any_solid_semi_food = 1 if any_solid_semi_food >1
tab any_solid_semi_food, m 


*Introduction to semi_solid, solid, soft_food in children from 6-8 months of age
* based on 
* q311: did [name] eat any solid, semisolid, or soft foods yesterday during the day
* q312: number of |times index child ate solid, semi solid, soft foods yesterday 
* any_solid_semi_food

tab q312 q311, m 
* no inconsistency between q311 and q312
tab any_solid_semi_food q311, m 
* inconsistency between q311 and any_solid_semi_food  - 12 cases
replace any_solid_semi_food = 1 if q311==1

gen intro_compfood = 0
replace intro_compfood = 1 if q311 == 1 | any_solid_semi_food ==1 
replace intro_compfood =. if age_days<=183 | age_days>=243
la var intro_compfood "Intro to complementary food 6-8 months of age"
tab intro_compfood
// this indicator is always 6-8 m 



*EXCLUSIVE BREASTFEEDING
*Exclusive breastfeeding is defined as breastfeeding with no other food or drink, not even water.
*Using the WHO guideline for defining ebf variable - create a condition variable based on 
*if the child received any other food items (liquid/solids/semi-solids) on previous day

cap drop ebf
* Create ebf variable - 1 yes 0 no
// no liquids besides breastmilk
// no food groups consumed - any_solid_semi_food==0 
gen ebf=0 
replace ebf =1 if currently_bf ==1
replace ebf =0 if water      ==1 | ///
                  juice      ==1 | ///
				  broth      ==1 | ///			
                  milk       ==1 | ///
                  formula    ==1 | ///
                  other_liq  ==1 | ///
                  any_solid_semi_food ==1 
replace ebf =. if age_days >730
la var ebf "Exclusive breasfeeding"
tab ebf

* Validate correct estimate of EBF between current calculations and final report
* EXBF is the variable used in final report
la drop exbf1
la var exbf "EBF - CNNS"
tab agemos ebf 
tab agemos exbf 
* many differences in EBF by age months between official and recalculated EBF indicator
foreach var of varlist ebf exbf {
	foreach var_2 of varlist water juice broth milk formula yogurt other_liq fortified_food bread vita_veg potato leafy_green vita_fruit fruit_veg organ poultry meat egg fish leg_nut currently_bf {
		tab `var_2' `var' , m
	}
}

cap drop ebf_denom
gen ebf_denom=1 if agemos<6
gen exbf_x = exbf*100
// version 16: table one [pw = iw_s_pool] if ebf_denom==1, c(mean exbf_x n exbf_x) format(%9.1f)
// CNNS   REPORT  EBF<6M  	58.0     3,615
cap drop exbf_x



* MEDIAN duration of exclusive breastfeeding
cap drop age_ebf
gen age_ebf = round(age_days/30.4375, 0.01)   //exact age in months round of to 2 digits after decimal
replace age_ebf = . if age_days >730
*set agemos_ebf to missing if exbf=no
replace age_ebf=. if ebf==0
la var age_ebf "Median age of exclusive breasfeeding in months"
sum age_ebf [aw=iw_s_pool] , d

* MEDIAN duration of continued breastfeeding
gen age_cbf = round(age_days/30.4375, 0.01)   //exact age in months round of to 2 digits after decimal
replace age_cbf=. if currently_bf !=1
la var age_cbf "Median age of continued breasfeeding in months"
sum age_cbf [aw=iw_s_pool] , d

*Continued breastfeeding 12-23 months 
gen cont_bf = 0
replace cont_bf = 1 if q307 == 1 
gen cont_bf_12_23 = cont_bf if age_days>335 & age_days<730 
tab cont_bf_12_23, m



* Minimum Dietary Diversity- code for new indicator definition 
* currently_bf - identifies children still breastfeeding
* Following new definition MDD is met if child fed 5 out 8 food groups regardless of breastfeeding status *

gen mdd=0
replace mdd=1 if sumfoodgrp >=5 
replace mdd=. if age_days<=183 | age_days>=730
la var mdd "Minimum Dietary Diversity (2020)"
tab mdd, m 


*Minimum Meal Frequency (MMF) 
*For currently breastfeeding children: MMF is met if child 6-8 months of age receives solid, semi-solid or soft foods at least 2 times and a child 9-23 months of age receives solid, semi-solid or soft foods at least 3 times*******
* Variable fed_solids identifies the number of times a child was fed solid, semi-solid or soft foods*

* For this variable please remember to set response "don't knows" to zero
* IN NFHS (DHS) replace fed_solids=0 if fed_solids is don't know  - In this case don't know has been assigned the value 8
tab q311
tab q312
la list q312
tab q312 q311, m 

// gen freq_solids=q312
// replace freq_solids =0 if q311==2 & sumfoodgrp==0  // Child did not consume any solid semi-solid foods yesterday
//
// * CNNS is different from NFHS - It records # of feeds per day instead of 1-7. 
// replace freq_solids =7 if q312 >=7 & q312 <50
// replace freq_solids =0 if q312 ==98 

cap drop freq_solids
gen freq_solids =0 if q311==2  // zero freq_feeds
replace freq_solids = q312 if q312 <=7 
replace freq_solids = 7 if q312 >7 & q312 <=44 

* Number of freq_solids, don't know and missing
replace freq_solids =9 if freq_solids>7 & q311==.  // missing
replace freq_solids =9 if freq_solids==.  // don't know
replace freq_solids =8 if q312==98  // don't know

tab freq_solids, m

* Cannot give any # of freq_feeds to those yes any_solid_semi_food cannot be coded. 
* There 992 cases of yes any_solid_semi_food but 0 freq_feeds
// replace freq_solids =9 if q199_1==. & any_solid_semi_food==1 // missing frequency and 1+ food groups
tab freq_solids any_solid_semi_food, m 

la def freq_solids 0 none, add 
la def freq_solids 7 "7+", add 
la def freq_solids 8 "don't know", add 
la def freq_solids 9 missing, add 
la val freq_solids freq_solids

tab freq_solids q311 ,m
tab  q312 freq_solids ,m
tab freq_solids any_solid_semi_food, m 

* Quality of freq solids indicators
clonevar qual_freq_solids = freq_solids
replace qual_freq_solids =10 if freq_solids>=0 & freq_solids<=7
replace qual_freq_solids =99 if any_solid_semi_food==1 & freq_solids==0
la def freq_solids 10 "from 0 to 7x", add
la def freq_solids 99 "missing freq & yes semi-solids", add
la val qual_freq_solids freq_solids
tab qual_freq_solids,m
tab qual_freq_solids



*Minimum Meal Frequency (MMF) Breastfeeding
gen mmf_bf=0
replace mmf_bf=1 if freq_solids>=2 & currently_bf==1 & age_days>183 & age_days<243 
replace mmf_bf=2 if freq_solids>=3 & currently_bf==1 & age_days>=243 & age_days<730 
replace mmf =. if age_days<=183 | age_days>=730
la def mmf  0 "Inadequate MMF" 1 "Adequate freq(2) and BF 6-8M" 2 "Adequate freq(3) and BF 9-23M"
la val mmf mmf
tab mmf, m 

*For currently non-breastfed children: MMF is met if children 6-23 months of age receive solid, semi-solid or soft foods or milk feeds at least 4 times during the previous day and at least one of the feeds is a solid, semi-solid or soft feed*******
* Variable fed_milk refers to number of times a child received milk other than breastmilk i.e. other animal milk*
* Variable fed_formula refers to number of times a child received infant formula*

* Generates the total number of times a non-breastfed child received solid, semi-solid or soft foods or milk feeds*
tab milk, m
tab q310d2, m // times drank milk

gen freq_milk = q310d2
replace freq_milk=freq_milk*-1 if q310d2<0  // negatives converted to positive
replace freq_milk = 7 if q310d2 >=7 & q310d2 <50
replace freq_milk = 0 if milk ==0  | q310d2==98
tab q310d2 freq_milk , m 

tab formula, m
tab q310e2, m 
gen freq_formula=q310e2
replace freq_formula = 7 if q310e2 >=7 & q310e2 <98
replace freq_formula = 0 if formula ==0  | q310e2==98
tab q310e2 freq_formula , m 

tab yogurt, m 
tab q310g2, m
gen freq_yogurt = q310g2
replace freq_yogurt = 7 if q310g2 >=7 & q310g2 <98
replace freq_yogurt = 0 if yogurt ==0  | q310g2==98
tab q310e2 freq_yogurt , m 

* Frequency of Milk and non solid semi-solid dairy feeds in children 0-23 months

//  Milk feeds include any formula (e.g. infant formula, follow-on formula, "toddler milk") or
// any animal milk other than human breast milk, (e.g. cow milk, goat milk, evaporated milk
// or reconstituted powdered milk) as well as semi-solid and fluid/drinkable yogurt and other
// fluid/drinkable fermented products made with animal milk.
// Page 9 WHO IYCF 2020

gen milk_feeds= freq_milk + freq_formula 
replace milk_feeds = 7 if milk_feeds>=7 & milk_feeds !=.
la val milk_feeds feeds
replace milk_feeds =. if age_days<0 | age_days>=730
tab milk_feeds, m 

// Do not double count yogurt
// it is included in solids

* combines number of times fed milk, infant formula and yogurt. 
* Check code from Richard in NY
* all missings have been set to 0 to allow summation across row. 

* OVERALL FREQUENCY FEEDS
gen feeds= milk_feeds + freq_solids	
* use milk feeds for all liquid milk feeds
* use freq_solids for all semi-solid feeds
replace feeds = 7 if feeds>=7 & feeds !=.
la def feeds 7 "7+ feeds"
la val feeds feeds
la var feeds  "Frequency of solid, semi-solid, all milk feeds"
tab feeds, m 	
* please ensure there are no missings(.) and all missings have been set to 0 in the age group to allow summation across row. 
* STATA will mark feeds as missings if any of them are missing.




*Minimum Meal Frequency (MMF) NON Breastfeeding
*  For currently non-breastfed children: Children 6-23 months of age who received solid, semi-solid or soft foods or milk feeds at least 4 times 
* during the previous day and at least one of the feeds was a solid, semi-solid or soft feed.
gen mmf_nobf=0
replace mmf_nobf=1 if feeds>=4 & freq_solids>=1 & currently_bf!=1
replace mmf_nobf =. if age_days<=183 | age_days>=730 
tab mmf_nobf, m 


* Minimum Milk / Dairy frequency for Non-Breastfed Child
* include milk, formula and yogurt
gen min_milk_freq_nbf =0
replace min_milk_freq_nbf =1 if milk_feeds >=2 & currently_bf!=1 
replace min_milk_freq_nbf =. if age_days<=183 | age_days>=730
la var min_milk_freq_nbf "Minimum Milk Frequency for Non-Breastfed Child"
tab min_milk_freq_nbf, m 
// graph bar (mean) min_milk_freq_nbf currently_bf if agemons < 24, over(agemons) 


*MMF among all children 6-23 months
gen mmf_all=0
replace mmf_all=1 if mmf_bf==1 | mmf_bf==2 | mmf_nobf==1
replace mmf_all =. if age_days<=183 | age_days>=730
la var mmf_all "Minimum meal frequency for all children 6-23M"
tab mmf_all, m 

**Minimum Acceptable Diet (MAD) 
*** Generates milk feeds for non-breastfed infants ***
*** Milk feeds include milk other than breastmilk, infant formula and yogurt*
* Variable fed_yogurt refers to number of times a child received yogurt*

* For each one please remember to set don't know to zero
* for NFHS (DHS) replace fed_yogurt=0 if fed_yogurt>7  - In this case don't knows has been assigned the value 8

* Mixed milk feeding 0-5M
* Mixed milk feeding (<6 months): Percentage of infants 0–5 months of age who 
* were fed formula and/or animal milk in addition to breast milk during the previous day
gen mixed_milk = 0 
replace mixed_milk=1 if (currently_bf==1 & formula==1) | (currently_bf==1 & milk==1) 
replace mixed_milk =. if age_days<0 | age_days>=183 
tab mixed_milk, m 


**** MAD among all infants 6-23 months of age ****	
gen mad_all=0
replace mad_all=1 if (mdd==1 & mmf_all==1) & (currently_bf==1 | min_milk_freq_nbf==1) 
replace mad_all=. if age_days<=183 | age_days>=730 
tab mad_all, m 

*Egg and/or Flesh food consumption - % of children 6-23months of age who consumed egg and/or flesh food during the previous day*
gen egg_meat=0
replace egg_meat=1 if all_meat ==1 | egg==1            //& agemons>=6 & agemons<=23
replace egg_meat =. if age_days<=183 | age_days>=730
tab egg_meat, m 

*Zero fruit or veg consumption - % of children 6-23months of age who did not consume any fruits or vegetables during the previous day**
gen zero_fv=0
replace zero_fv =1 if vita_fruit_veg==0  & fruit_veg ==0
replace zero_fv =. if age_days<=183 | age_days>=730
tab zero_fv, m 


*Unhealthy food consumption
*consumption of sugar sweetened beverages by child agemons 6 to 23

gen sugar_bev = .
gen unhealthy_food = .
* Need clear definition of unhealthy foods - from WHO guidance. 
* Low nutrient density liquids
* juice broth other_liqs

* LBW

// cap drop lbw
// gen birthweight = q645wght if q645wght < 8
// tab birthweight
// // kdensity birthweight
// recode birthweight (0/2.499999=1)(2.5/8=0), gen(lbw)
// tab lbw, m



* LBW              (low birth weight)
tab q644  //  q644. was [name] weighed at birth? 
tab q645wght
cap drop birth_weight
gen birth_weight = q645wght
replace birth_weight = .  if birth_weight >9

label var birth_weight "Birth weight"
sum birth_weight

recode birth_weight (0/0.249=6)(0.25/1.499=1)(1.5/2.499=2)(2.5/3.999=3)(4/10.999=4)(. = 7), gen(cat_birth_wt)
replace cat_birth_wt =  6 if birth_weight ==9.998
replace cat_birth_wt =  5 if q644 ==2
replace cat_birth_wt =  6 if q644 ==8
label def cat_birth_wt 1 "Very low <1.5kg" 2 "Low <2.5kg" 3 "Average" 4 "High >4kg" 5 "Not weighed" 6 "Don't know" 7 "Missing"

* SHOULD RECODE TO 8 = don't know 9 = Missing

* Should always set birth_weight to grams 

label val cat_birth_wt cat_birth_wt
label var cat_birth_wt "Birth weight category"
tab cat_birth_wt, m 

cap drop lbw
replace birth_weight = birth_weight*1000
gen lbw = . 
replace lbw = 1 if birth_weight <2500
replace lbw = 0 if birth_weight >=2500 
replace lbw = . if cat_birth_wt >=5
tab birth_weight lbw, m
tab lbw







* early ANC  <=3 months first trimester
tab q614, m 
tab q617 q614, m 
gen earlyanc = 0
replace earlyanc =1 if q617 <=3
replace earlyanc =. if age_days>=730
tab earlyanc,m
tab q617 earlyanc

* ANC 4+ 
gen anc4plus = 0
replace anc4plus = 1 if q618 >=4 & q618 <=9
replace anc4plus =. if age_days>=730
tab anc4plus,m
tab q618 anc4plus

* C-section
gen csection = 0
replace csection = 1 if q639==3
tab q639 csection, m 

* Institutional births (public, private, home)
tab q637
la list q637
tab q637o
recode q637 (10/13=3)(20/27=1)(30/42=2)(96/99 . =4), gen(inst_birth)
* double checked val codes against questionnaire
la def inst_birth 1 public 2 private 3 home 4 "other-missing"
la val inst_birth inst_birth
la var inst_birth "Institutional Birth"
tab q637 inst_birth, m 


* Birth order
la list q106
tab q106 // how many births prior to birth of index child
gen bord = q106+1
replace bord = . if q106==98
tab bord q106, m 

* mother's work status*
tab q119
la list q119
recode q119 (2=0)(1=1)(3 .=0),gen(mum_work)
la var mum_work "Mother working"
tab q119 mum_work, m 
tab q119 mum_work if agemos <24, m 

tab q120



* mothers education
* Recode of q116/117
tab  q116, m
tab  q117, m
tab  q117 q116, m
// recode q117 (0=0)(1/4=1)(5/9=2)(10/11=3)(12/25=4)(26/max=99), gen(mum_educ)
// * no education if mother did not attend school or don't know
// replace mum_educ=0 if q116==2 | q116==8
//
// lab var mum_educ "Maternal Education"
// la def mum_educ 0 "No Education" 1 "5 years completed" 2 "5-9 years completed" ///
// 	3 "10-11 years completed" 4  "12+ years completed" 99 "Missing"
// la val mum_educ mum_educ
// tab mum_educ,m
// tab q117 mum_educ, m 

cap drop mum_educ
cap drop mum_educ_years
gen mum_educ_years=.
replace mum_educ_years = q117 
replace mum_educ_years = 0 if q116==2
tab mum_educ_years, m 
replace mum_educ_years=. if q117==98
 * Only 34 cases of don't know in mum_educ - set missing to . 

recode mum_educ_years (0=1)(1/4=2)(5/9=3)(10/11=4)(12/25=5)(26/max=99), gen(mum_educ)
replace mum_educ = 99 if mum_educ_years==.
lab var mum_educ "Maternal Education"
la def mum_educ 1 "No Education" 2 "<5 years completed" 3 "5-9 years completed" ///
	4 "10-11 years completed" 5  "12+ years completed" 99 "Missing"
la val mum_educ mum_educ
tab mum_educ_years mum_educ, m

tab wi mum_educ, m 


* caste
cap drop caste
gen caste = 0 if q113!=.    //caste =0 if religion is missing
replace caste = 1 if q115 ==1 
replace caste = 2 if q115 ==2 
replace caste = 3 if q115 ==3
replace caste = 4 if q115 ==4
replace caste = 4 if q114 ==993 
replace caste = 4 if q115==8
lab define caste 1 "Scheduled caste" 2"Scheduled tribe" 3 "OBC"  4 "Other"  
lab val caste caste
lab var caste "Caste"
tab caste, m 

* Residence Rural Urban
recode area (2=1) (1=2), gen(rururb)
replace rururb = . if q113==.
lab define rururb 1 "Urban" 2 "Rural"
lab val rururb rururb   
lab var rururb "Residence"
tab rururb, m 

* Wealth index National level
tab wi, m

* Wealth index State level
tab wi_s,m



* Sex 
* 1= male / 2= female 
gen sex = q102
tab sex // double check
tab q102

	
* Child Illness
* Diarrhea
recode q501 (1=1)(2 8=0), gen(diar)
tab diar q501
* Fever
recode q510 (1=1)(2 8=0), gen(fever)
tab fever q510	
* Cough with rapid breathing excluding those with only nasal breathing problems
recode q512 (1=1)(2 8=0), gen(ari)
lab val diar fever ari no_yes
tab q512 ari
replace ari=0 if q513!=1
replace ari=0 if q514==2 | q514==8 | q514==9
tab ari q513
tab ari q514
tab ari, m 

* add region variable in master.do
* UTs are MISSING IN THE REGIONS


* CNNS state variable has to be harmonized with all survey state identification variables
clonevar state_cnns = state
* state_cnns is the original non modified var.  state is the new harmonized var. 

// 1 "A&N islands"
replace state =2   if  state_cnns ==28
replace state =3   if  state_cnns ==12
replace state =4   if  state_cnns ==18
replace state =5   if  state_cnns ==10
// 6 Chandigarh
replace state =7   if  state_cnns ==22
// 8 "Dadra and Nagar Haveli"
// 9 "Daman and Diu"
replace state =10  if  state_cnns ==30
replace state =11  if  state_cnns ==24
replace state =12  if  state_cnns ==6
replace state =13  if  state_cnns ==2
replace state =14  if  state_cnns ==1
replace state =15  if  state_cnns ==20
replace state =16  if  state_cnns ==29
replace state =17  if  state_cnns ==32
// 18 Lakshadweep
replace state =19  if  state_cnns ==23
replace state =20  if  state_cnns ==27
replace state =21  if  state_cnns ==14
replace state =22  if  state_cnns ==17
replace state =23  if  state_cnns ==15
replace state =24  if  state_cnns ==13
replace state =25  if  state_cnns ==7
replace state =26  if  state_cnns ==21
// 27 Puducherry
replace state =28  if  state_cnns ==3
replace state =29  if  state_cnns ==8
replace state =30  if  state_cnns ==11
replace state =31  if  state_cnns ==33
replace state =32  if  state_cnns ==16
replace state =33  if  state_cnns ==9
replace state =34  if  state_cnns ==5
replace state =35  if  state_cnns ==19
replace state =36  if  state_cnns ==99


cap la drop state_name
la def state_name			   1 "A&N islands"
la def state_name			   2 "Andhra Pradesh", add
la def state_name			   3 "Arunachal Pradesh" , add
la def state_name			   4 Assam , add
la def state_name			   5 Bihar , add
la def state_name			   6 Chandigarh, add
la def state_name			   7 Chattisgarh, add
la def state_name			   8 "Dadra and Nagar Haveli", add
la def state_name			   9 "Daman and Diu", add
la def state_name			  10 Goa, add
la def state_name			  11 Gujarat, add
la def state_name			  12 Haryana, add
la def state_name			  13 "Himachal Pradesh", add
la def state_name			  14 "Jammu and Kashmir", add
la def state_name			  15 Jharkhand, add
la def state_name			  16 Karnataka, add
la def state_name			  17 Kerala, add
la def state_name			  18 Lakshadweep, add
la def state_name			  19 "Madhya Pradesh", add
la def state_name			  20 Maharashtra, add
la def state_name			  21 Manipur, add
la def state_name			  22 Meghalaya, add
la def state_name			  23 Mizoram, add
la def state_name			  24 Nagaland, add
la def state_name			  25 Delhi, add
la def state_name			  26 Odisha, add
la def state_name			  27 Puducherry, add
la def state_name			  28 Punjab, add
la def state_name			  29 Rajasthan, add
la def state_name			  30 Sikkim, add
la def state_name			  31 "Tamil Nadu", add
la def state_name			  32 Tripura, add
la def state_name			  33 "Uttar Pradesh", add
la def state_name			  34 Uttarakhand, add
la def state_name			  35 "West Bengal", add
la def state_name			  36 Telangana, add
la val state state_name

tab state, m 
tab state state_cnns, m 

* Survey Weights
gen national_wgt =iw_s_pool   

* Regional weights to add
merge 1:1 case_id using C:\Temp\Data\reg_wgt_temp
gen regional_wgt = reg_weight_survey
gen regional_bio_wgt = reg_weight_bio

* 852 cases of data collection not completed
* reg_weight_survey reg_weight_bio

gen state_wgt =iweight_s   


gen round=4

* if doing analysis on only IYCF then drop all other vars. 

keep psu hh_num one int_date birthday birthmonth birthyear dateofbirth age_days agemos ///
	evbf eibf eibf_timing ebf2d ebf3d ebf age_cbf age_ebf prelacteal_milk ///
	prelacteal_water prelacteal_sugarwater prelacteal_gripewater /// 
	prelacteal_saltwater prelacteal_formula prelacteal_honey ///
	prelacteal_janamghuti prelacteal_other bottle water juice milk ///
	formula other_liq juice broth yogurt fortified_food bread vita_veg ///
	potato leafy_green any_solid_semi_food vita_fruit fruit_veg organ meat ///
	egg fish cont_bf semisolid carb leg_nut dairy all_meat vita_fruit_veg ///
	agegroup sumfoodgrp diar fever ari cont_bf_12_23 ///
	intro_compfood mdd currently_bf freq_solids mmf_bf freq_milk ///
	freq_formula freq_yogurt milk_feeds feeds mmf_nobf min_milk_freq_nbf ///
	mmf_all mixed_milk mad_all egg_meat zero_fv sugar_bev unhealthy_food ///
	lbw cat_birth_wt anc4plus csection earlyanc mum_educ caste rururb wi wi_s state ///
	sex national_wgt state_wgt round ebf_denom mum_work inst_birth bord regional_wgt regional_bio_wgt

// 	regional_wgt regional_bio_wgt 

* Save data with name of survey
save iycf_cnns, replace 


// end

