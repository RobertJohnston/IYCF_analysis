* Make IYCF Variables for CNNS data

* Robert updated 9 12 2021 review for EBF 

cd "C:/Temp"
use "C:\Temp\CNNS_04_Cleaned_3JUNE_final.dta", clear
*use "C:\Users\roypu\OneDrive\Documents\UNICEF 2019-20\CNNS IYCF Analysis\CNNS_04_version_1.1.dta"

// cd "C:\Users\dnyan\OneDrive\Documents\UNICEF FELLOWSHIP\CNNS\NSSO_CNNS"
// use "C:\Users\dnyan\OneDrive\Documents\UNICEF FELLOWSHIP\CNNS\NSSO_CNNS\CNNS_04_version_1.1.dta", clear

* make IYCF vars.do

gen one=1

tab result
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




* Age in days
gen int_date = mdy(int_m , int_d , int_y)
format int_date %td
gen dob_date = mdy(q103m , q103d , q103y)
format dob_date %td
gen age_days = int_date - dob_date 
replace age_days =. if age_days>1825
tab age_days,m 
// cap drop count
// bysort age_days: egen count = count(age_days)
// line count age_days
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

cap drop ebf2d
gen ebf2d =0
replace ebf2d=1 if q303!=1 // q303!=1 means no other drink was given to the child in 1st three days
replace ebf2d =. if age_days >2
* Question was not asked correctly so variable will only be valid at national level 
* 82 children in sample <= 2 days
tab ebf2d q303, m 

* Currently Breastfeeding
* Pay attention to order of assignment of currently BF
// 1. ever breastfed
// 2. still breastfeeding
// 3. breastfed yesterday
cap drop currently_bf
gen currently_bf = .
replace currently_bf = 0 if q301 !=1
replace currently_bf = 0 if q306 ==2
replace currently_bf = 1 if q307 ==1
replace currently_bf = 0 if q307 >=2
// recode q307 (1=1)(2 8=0)(missing=.), gen(currently_bf)
la var currently_bf "Currently breastfeeding"
tab currently_bf, m 
tab currently_bf q307, m 
tab currently_bf q306, m 
tab currently_bf q301, m 

* Prelacteal feeds
* What was [NAME] given to drink? within first three days after delivery
tab q304, m 
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

local prelacteal = "prelacteal_milk prelacteal_water prelacteal_sugarwater prelacteal_gripewater prelacteal_saltwater prelacteal_fruitjuice prelacteal_formula prelacteal_honey prelacteal_janamghuti prelacteal_tea prelacteal_other"
foreach var in `prelacteal' { 
	replace `var' = . if  age_days>=730
}

tab  q304 prelacteal_milk
tab  prelacteal_milk
tab  q304 prelacteal_other

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

lab define no_yes 0 "No" 1 "Yes"

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
replace carb = 1 if bread ==1 | potato==1 
lab var carb "1: Bread, rice, grains, roots and tubers"
// lab val carb carb1

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
replace ebf =. if age_days >182
la var ebf "Exclusive breasfeeding"
tab ebf
tab ebf agemos


* We have already reviewed and converted the vars below into new vars.  Use new vars for definition
// cap drop condition 					   
// gen condition = 0	if age_days<183				   
// replace condition = 1 if q310a==2 & q310b==2 & q310c==2 & q310d1==2  & q310e1==2  & q310f==2 & q310g1==2  & q310h==2 ///
//                        & q310i==2 & q310j==2 & q310k==2 & q310l==2 & q310m==2 & q310n==2 & q310o==2 & q310p==2 & q310q==2 & q310r==2 & q310s==2 & q310t==2 ///
// 					   & q310u==2 & q310v==2 & q310w==2 & q311==2
// tab condition, m

// cap drop exbf
// gen exbf = 0 if age_days<183
// * cont_bf is wrong here
// replace exbf = 1 if condition == 1 & cont_bf==1 & age_days<183 
// tab exbf


* MEDIAN duration of exclusive breastfeeding
cap drop age_ebf
gen age_ebf = round(age_days/30.4375, 0.01)   //exact age in months round of to 2 digits after decimal
replace age_ebf = . if age_days >183
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

tab q312
gen freq_solids=q312
replace freq_solids =0 if q311==2 & sumfoodgrp==0  // Child did not consume any solid semi-solid foods yesterday

* CNNS is different from NFHS - It records # of feeds per day instead of 1-7. 
replace freq_solids =7 if q312 >=7 & q312 <50
replace freq_solids =0 if q312 ==98 
tab q312 freq_solids, m 
tab sumfoodgrp freq_solids, m 
* In CNNS there are 902 cases of children who consumed foods yesterday but we don't know frequency - recode as 1 feed
replace freq_solids=1 if freq_solids==. & sumfoodgrp >=1 & sumfoodgrp <=8
tab sumfoodgrp freq_solids, m 
* In CNNS there 102 cases of eating 0 food groups but consumed semi-solid 1+ times yesterday, - recode as 1 food group. 
replace sumfoodgrp=1 if sumfoodgrp==0 & freq_solids>=1 & freq_solids<=7
tab sumfoodgrp freq_solids, m 

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
tab q310d2, m 

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

* DOUBLE CHECK THAT YOGURT INCLUDED IN MILK FEEDS  / See NY guidance and WHO docs

gen milk_feeds= freq_milk + freq_formula + freq_yogurt	
replace milk_feeds = 7 if milk_feeds>=7 & milk_feeds !=.
la val milk_feeds feeds
replace milk_feeds =. if age_days<0 | age_days>=730
tab milk_feeds, m 
* combines number of times fed milk, infant formula and yogurt. Please ensure there are no missings(.) 
* all missings have been set to 0 to allow summation across row. 

* OVERALL FREQUENCY FEEDS
gen feeds= freq_milk + freq_formula + freq_solids	
replace feeds = 7 if feeds>=7 & feeds !=.
la def feeds 7 "7+ feeds"
la val feeds feeds
la var feeds  "Frequency of solid, semi-solid, milk and formula feeds"
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
* graph bar (mean) min_milk_freq_nbf currently_bf if agemons < 24, over(agemons) 


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
* juice broth other_liq

* LBW
* MISSING
gen lbw = . 

* early ANC  <=3 months first trimester
gen earlyanc = 0
replace earlyanc =1 if q617 <=3
replace earlyanc =. if age_days>=730
tab q617 earlyanc

* ANC 4+ 
gen anc4plus = 0
replace anc4plus = 1 if q618 >=4 & q618 <=9
replace anc4plus =. if age_days>=730
tab q618 anc4plus

* C-section
gen csection = 0
replace csection = 1 if q639==3
tab q639 csection, m 

* mothers education
* Recode of q116/117
tab  q117 q116, m
recode q117 (0=0)(1/4=1)(5/9=2)(10/11=3)(12/25=4)(26/max=99), gen(mum_educ)
* no education if mother did not attend school or don't know
replace mum_educ=0 if q116==2 | q116==8

lab var mum_educ "Maternal Education"
la def mum_educ 0 "No Education" 1 "5 years completed" 2 "5-9 years completed" ///
	3 "10-11 years completed" 4  "12+ years completed" 99 "Missing"
la val mum_educ mum_educ
tab q117 mum_educ, m 

 * Only 34 cases of missing in mum_educ - set missing to . 
replace mum_educ=. if mum_educ==99

* caste
gen caste = 0 if q113!=.
replace caste = 1 if q115 ==1 
replace caste = 2 if q115 ==2 
replace caste = 3 if q115 ==3
replace caste = 4 if q115 ==4
replace caste = 4 if q114 ==993 
replace caste = 4 if q115==8
lab define caste 1 "Scheduled caste" 2"Scheduled tribe" 3"OBC"  4"Others"  
lab val caste caste
lab var caste "Caste"
tab caste, m 

* Residence Rural Urban
recode area (2=1) (1=2), gen(rururb)
replace rururb = . if q113==.
lab define rururb 1 "Urban" 2"Rural"
lab val rururb rururb   
lab var rururb "Residence"
tab rururb, m 

* socio-economic status
* IS THIS CORRECT NAME ?
tab wi, m


	
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



* Generate 'region' variable from the CNNS state variable
gen double region:region=0
* double is not really needed here. 
replace region=1 if state==7  |  state==6  | state==2  | state==1  | state==3  | state==8 | state==5
replace region=2 if state==22 |  state==23 | state==9
replace region=3 if state==10 |  state==20 | state==21 | state==19
replace region=4 if state==12 |  state==18 | state==14 | state==17 | state==15 | state==13 | state==11 | state==16
replace region=5 if state==30 |  state==24 | state==27
replace region=6 if state==28 |  state==29 | state==32 | state==33 | state==99
replace region=. if iw_s_pool==.
lab define region 1 "North" 2 "Central" 3 "East" 4 "Northeast" 5 "West" 6 "South"
lab var region "Region" 
lab val region region


/*
------------------------------------------------------------------------------------------------------------------------------------------------
region       Region Name       states included in the region
-------------------------------------------------------------------------------------------------------------------------------------------------
region 1       North           NCT of Delhi(7), Haryana(6), Himachal Pradesh(2), Jammu and Kashmir(1), Punjab(3), Rajasthan(8), Uttarakhand(5)
region 2	   Central		   Chhattisgarh(22), Madhya Pradesh(23), Uttar Pradesh(9)
region 3	   East			   Bihar(10), West Bengal(19), Jharkhand(20), Odisha(21)	 							
region 4       NorthEast       Arunachal Pradesh(12), Sikkim(11), Tripura(16),  Meghalaya(17), Assam(18), Nagaland(13), Manipur(14), Mizoram(15)
region 5       West            Gujarat(24), Maharshtra (27), Goa(30)
region 6       South           Andhra Pradesh(28),  Karnataka(29),  Kerala(32),  Tamil Nadu(33),  Telangana(99) 
--------------------------------------------------------------------------------------------------------------------------------------------------
*/
* UTs are MISSING IN THE REGIONS


* CNNS state variable has to be harmonized with all survey state identification variables
gen state_cnns = state
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

* Survey Weights
gen nat_wgt =iw_s_pool    //national women's sample weight (6 decimals)
gen reg_wgt = reg_weight_survey
gen state_wgt =iweight_s    // 	state women's sample weight (6 decimals)

* male / female

* Save data with name of survey
save iycf_cnns, replace 


