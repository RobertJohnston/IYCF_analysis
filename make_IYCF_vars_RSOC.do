* Make IYCF Variables for RSOC data
* create standardized data following WHO 2020 IYCF indicator definitions
* Robert 

version 16 



***************************
* DOUBLE CHECK IF YOGURT IS INCLUDED HERE
*replace min_milk_freq_nbf =1 if yogurt_freq >=2 & currently_bf!=1 
**********************************


// use "C:\Temp\IYCF\RSOC\EMW_INDIA_RSOC.dta", clear  // complete data at individual level
* EMW - ever married women
// use "C:\Temp\IYCF\RSOC\Household_India_RSOC.dta", clear
* Please note household data does not include IYCF


include "C:\Users\stupi\OneDrive - UNICEF\1 UNICEF Work\1 moved to ECM\IIT-B\IYCF\analysis\robert_paths.do"
// include "dnyaneshwar_paths.do"

* Open RSOC
use `RSOC', clear


* 29 states included 
tab q1a

destring q5_1, gen(psu)
// gen psu = q5_1
// scatter q1a q5_1

gen hh_num = q11_1
gen one=1

* identify number of living children under five
* A2.1 number of live births
tab q134, m 
// Live Birth           |     53,987       98.95       98.95
* Complete Underfive N  53,987   ???

* ever breastfed
tab q184
* Children under 3 years of age who were asked about breastfeeding = 35,035 
 
* Birth date
tab q136a_1, m 
tab q136b_1, m 
tab q136c_1, m



gen birthday =  q136a_1
gen birthmonth = q136b_1
gen birthyear = q136c_1
replace birthyear = . if q136c_1<2010 | q136c_1>2014 
gen dateofbirth = mdy(birthmonth , birthday , birthyear)
format dateofbirth %td
// kdensity dateofbirth


* date of interview - Data recorded as string
tab q106_1

* Convert string to number
destring q106_1, gen(temp)

gen int_y = (temp/100) - floor(temp/100) 
gen int_m = (temp/10000) - floor(temp/10000) 
replace int_m = int_m - int_y/100

gen int_d = floor(temp/10000)
replace int_m = int_m*100
replace int_y = 2000 + (int_y*100)

replace int_d=int(int_d)
replace int_m=int(int_m)
replace int_y=int(int_y)

* Corrections to records of interview date
replace int_y = 2013 if int_m ==11 | int_m ==12
replace int_y = 2014 if int_m >=1 & int_m <=6

gen int_date = mdy(int_m , int_d , int_y)
format int_date %td

// order q106_1 int_d int_m int_y , first
list q106_1 int_d int_m int_y int_date in 1/20 

* Change from age in months to age in days
* Ages in month by day ranges
* 6 - 23 M  (183 - 730)
* Under 24 M
* 6 - 8 M   (183 - 243)
* 9 -23 M   (243 - 730)
* 6-11 M    (183 - 335)
* 12 - 23 M (335 - 730)
* Underfive (< 1825)

* Age in days
gen age_days = int_date - dateofbirth 
tab age_days,m 
list q106_1 int_d int_m int_y int_date dateofbirth q136a_1 q136b_1 q136c_1 if age_days<0
replace age_days =. if age_days<0 | age_days>1825

gen agemos = floor(age_days/30.42)
tab agemos, m 

cap drop ageyears
gen ageyears = floor(agemos / 12)
tab q1a ageyears

* IYCF data (under 3 years of age) only collected from following states
// Jammu & Kashmir
// Himachal Pradesh
// Punjab
// Uttarakhand
// Haryana
// Delhi
// Rajasthan
// Uttar Pradesh
// Sikkim
// Jharkhand
// Chhattisgarh
// Madhya Pradesh
// Gujarat
// Maharashtra
// Andhra Pradesh
// Karnataka
// Goa
// Kerala
// Tamil Nadu


* IYCF data collected only for children under 3 years of age
drop if age_days> 1095  // 3 years = 1096 days 

tab birthday, m
tab birthmonth, m
tab birthyear, m
* U3 Children with birth dates -   34,217

// graph bar (count) one, over(agemos)



* Ever breastfed (children born in past 24 months)
tab q184, m 
gen evbf = 0
replace evbf=1 if q184==1
replace evbf=. if age_days>=730 
la var evbf "Ever breastfed (children born in past 24 months)"
tab evbf q184, m

*Early initiation of Breastfeeding (children born in last 24 months breastfed within 1hr)
tab q185a1_1
tab q185a1_2 
tab q185a1_3

* Early initiation of breastfeeding (under two years of age)
gen eibf = 0
replace eibf = 1 if (q185a1_2 <=1 & q185a1_2 == 1) | (q185a1_1==0)   //here response as "immediately"

* Harmonize to NFHS 
// replace eibf = 1 if m34 == 0 | m34==100  ///  0 -immediately, 100 - within 30min to 1hr 
// // 101 =  one hour and more

replace eibf =. if age_days>=730 // age in days
tab  eibf, m
tab  q185a1_1 eibf, m
tab  q185a1_2 eibf, m

*Timing of initiation of Breastfeeding
cap drop eibf_timing
gen eibf_timing =. 
replace eibf_timing = q185a1_3*24 if q185a1_3>=1
replace eibf_timing = q185a1_2 if q185a1_2>=1
replace eibf_timing = 0 if q185a1_1==0

replace eibf_timing = 48 if eibf_timing>48
replace eibf_timing =.  if age_days>=730
la var eibf_timing "Timing of start of breastfeeding (in hours)"
la def eibf_timing 48 "48 hours or more"
la val eibf_timing eibf_timing
tab eibf_timing, m 
// scatter  q185a1_2 eibf_timing
// scatter  q185a1_3 eibf_timing

* Exclusively breastfed for the first two days after birth
* Percentage of children born in the last 24 months who were fed exclusively with breast milk for the first two days after birth
* q187 Was [NAME] given anything to drink other than breast milk within the first three days after delivery?
* The RSOC did not ask the question in the best method, so we are trying to edit to represent 2 days. 

tab q187 // In the first 3 days after delivery, was child given something other than breastmilk
// la list q187
//            1 Yes
//            2 No
//            9 Missing

cap drop ebf2d 
* only breastfed in first two days
gen ebf2d =0
replace ebf2d=1 if q187!=1 //  no other drink was given to the child in 1st three days
replace ebf2d =. if age_days >2
* Question was not asked correctly so variable will only be valid at national level 
* 30 children in sample <= 2 days
tab ebf2d q187, m 

* only breastfed in first three days
cap drop ebf3d 
gen ebf3d =0
replace ebf3d=1 if q187!=1 //  no other drink was given to the child in 1st three days
* Question was not asked according to new standards so variable will only be valid at national level 
tab ebf3d q187, m 

* Currently Breastfeeding
* Pay attention to order of assignment of currently BF
// 1. ever breastfed
// 2. still breastfeeding
// 3. breastfed yesterday
// missing and dont know are considered as No --WHO guidelines

tab q184, m // ever breastfed
tab q190, m // currently breastfed 
tab q192, m // breastfed yesterday during the day or at night

cap drop currently_bf
gen currently_bf = .
//Ever breastfed
replace currently_bf = 0 if q184 !=1    
replace currently_bf = 1 if q184 ==1    
//Are you still breastfeeding
replace currently_bf = 0 if q190 !=1    
replace currently_bf = 1 if q190 ==1
//Was breastfed yesterday during the day or at night?
replace currently_bf = 1 if q192 ==1   
replace currently_bf = 0 if q192 ==2 | q192==98
la var currently_bf "Currently breastfeeding"
tab currently_bf, m 


* Prelacteal feeds
* What was [NAME] given to drink? within first three days after delivery
* q188_1 q188_2 q188_3 q188_4 q188_5 q188_6 q188_7 q188_8 q188_9 q188_96

cap drop prelacteal*
gen prelacteal_milk       = cond(q188_1==1, 1, 0)
gen prelacteal_water      = cond(q188_2==1, 1, 0)
gen prelacteal_sugarwater = cond(q188_3==1, 1, 0)
gen prelacteal_gripewater = cond(q188_4==1, 1, 0)
gen prelacteal_saltwater  = cond(q188_5==1, 1, 0)
gen prelacteal_fruitjuice = cond(q188_6==1, 1, 0)
gen prelacteal_formula    = cond(q188_7==1, 1, 0)
gen prelacteal_honey      = cond(q188_8==1, 1, 0)
gen prelacteal_janamghuti = cond(q188_9==1, 1, 0)
gen prelacteal_other      = cond(q188_96==1, 1, 0)

local prelacteal_feeds = "prelacteal_milk prelacteal_water prelacteal_sugarwater prelacteal_gripewater prelacteal_saltwater prelacteal_fruitjuice prelacteal_formula prelacteal_honey prelacteal_janamghuti prelacteal_other"
foreach var in `prelacteal_feeds' { 
	replace `var' = . if  age_days>=730
}

cap drop prelacteal_otherthanmilk
gen prelacteal_otherthanmilk =0
local prelacteal = "prelacteal_water prelacteal_sugarwater prelacteal_gripewater prelacteal_saltwater prelacteal_fruitjuice prelacteal_formula prelacteal_honey prelacteal_janamghuti prelacteal_other"
foreach var in `prelacteal' { 
	replace prelacteal_otherthanmilk = 1 if  `var'==1
}

tab  q187 prelacteal_milk
tab  prelacteal_milk
tab  prelacteal_otherthanmilk
tab  q187 prelacteal_other

*Colostrum Feeding
tab q186
gen colostrum = 0
replace colostrum=1 if q186==1
replace colostrum=. if age_days>=730
tab colostrum q186, m 

* bottle feeding (did child drink anything from bottle)
gen bottle =. 


/*
variable name       value label      variable label
--------------------------------------------------------------------------------------------

q184                           A5.2. Did you ever breastfeed <NAME>
q185a1_1                       A5.3. How long after the birth did first put to the breast - IMMEDIATELY /WITHIN
q185a1_2                       A5.3. How long after the birth did first put to the breast - HOURS
q185a1_3                       A5.3. How long after the birth did first put to the breast - DAYS
q186                           A5.4. Did you feed first yellow thick breast milk (Colostrum)/Khees to the baby?
q187                           A5.5. In the first 3 days after delivery, was (NAME) given anything to drink other than breast milk?             
q188_1                         A5.6. Milk other than breast milk
q188_2                         A5.6. Plain Water
q188_3                         A5.6. Sugar or Glucose water
q188_4                         A5.6. Gripe Water
q188_5                         A5.6. Sugar- Salt-Water Solution
q188_6                         A5.6. Fruit Juice
q188_7                         A5.6. Infant Formula
q188_8                         A5.6. Honey
q188_9                         A5.6. Janamghuti
q188_96                         A5.6. Other
q189                           A5.7. CHECK
q190                           A5.8. Are you still breastfeeding this child?
q191_1                         DAYS (A5.9. How many days/months did you breastfeed the child?)  
q191_2                         MONTHS (A5.9. How many days/months did you breastfeed the child?)  
q192                           A5.10. Was breastfed yesterday during the day or at night?
q193                           A5.11. Was given any vitamin drops or other medicines as drops yesterday during
q194                           A5.12. Was given ORS yesterday during the day or at night?
q195a1_1                       A5.13. a) PLAIN WATER    
q195a1_2                       A5.13. b) WATERY ITEMS  SUCH AS DAL/RICE WATER
q195a1_3                       A5.13. c) POWDER MILK/FORMULA
q195a1_4                       A5.13. d) COWS/BUFFALOS/GOATS/ OTHER ANIMAL MILK
q195a1_5                       A5.13. e) JUICE OR JUICE DRINKS (MANGO, ORANGE, APPLE,  LEMON ETC)
q195a1_6                       A5.13. f) BUTTER MILK/BEATEN CURD
q195a1_7                       A5.13. g) ANY OTHER LIQUIDS
q196a1_3                       A5.14. POWDER MILK/FORMULA (NUMBER OF TIMES)
q196a1_4                       A5.14. COWS/BUFFALOS/GOATS/OTHER ANIMAL MILK (NUMBER OF TIMES)
q196a1_6                       A5.14. BUTTER MILK/BEATEN CURD (NUMBER OF TIMES)

q197a1_1                       A5.15. ANY COMMERCIALLY FORTIFIED BABY FOOD SUCH AS CERELAC ETC.
q197a1_2                       A5.15. BREAD, ROTI, CHAPATI, RICE, KITCHDI, NOODLES, PORRIDGE,  BISCUITS, IDLI, 
q197a1_3                       A5.15. PULSES/LENTILS/BEANS OR FOOD PREPARED WITH MIXING PULSES/LENTILS/LEGUMES
q197a1_4                       A5.15. PUMPKIN, CARROTS, OR SWEET POTATOES THAT ARE YELLOW OR ORANGE INSIDE?
q197a1_5                       A5.15. POTATOES, WHITE YAMS, OR ANY OTHER FOODS MADE FROM ROOTS?
q197a1_6                       A5.15. ANY DARK GREEN, LEAFY VEGETABLES?
q197a1_7                       A5.15. RIPE MANGOES, PAPAYAS, CANTALOUPE, OR JACKFRUIT?
q197a1_8                       A5.15. ANY OTHER FRUITS OR VEGETABLES?
q197a1_9                       A5.15. LIVER, KIDNEY, HEART OR OTHER ORGAN MEATS?
q197a1_10                      A5.15. ANY MEAT SUCH AS CHICKEN, BEEF, PORK, LAMB, GOAT OR DUCK?
q197a1_11                      A5.15. EGGS?
q197a1_12                      A5.15. FRESH OR DRIED FISH OR SHELLFISH?
q197a1_13                      A5.15. ANY FOODS MADE FROM NUTS SUCH AS PEANUTS, CASHEW NUTS, ALMOND ETC.?
q197a1_14                      A5.15. CHEESE, PANEER, DAHI OR OTHER FOOD MADE FROM MILK?
q197a1_15                      A5.15. ANY OTHER SOLID OR SEMI-SOLID FOOD?
q198                           A5.16. Did eat any solid, semi-solid, or soft foods yesterday during the day or
q199_1                         NUMBER OF TIMES (A5.17. How many times did eat solid, semi-solid, or soft foods   
q199dk_98                      A5.17. Do not know     
q200                           A6.1. Do you have a Mother Child Protection (MCP) card for this child  where a
*/

********************************************************************************
* Food groups (liquids and solids)
********************************************************************************

* recode foods into food groups
* 	yes = 1, 
* 	no and don't know = 0
* following global guidance on IYCF analysis, this allows for maximium children to be included in indicator 

* Liquids in past 24 hours
foreach var of varlist q195a1_1 - q195a1_7 {
	recode `var' (1=1) (2 8 98 .=0) , gen(`var'_rec)
	lab val `var' no_yes
}
* q196 - number of times consumed dairy

* Solids in past 24 hours
foreach var of varlist q197a1_1 -  q197a1_15 {
	recode `var' (1=1) (2 8 98 . =0) , gen(`var'_rec)
	lab val `var' no_yes
}

* LIQUIDS
clonevar water			=q195a1_1_rec
tab water q195a1_1_rec

clonevar juice			=q195a1_5_rec
clonevar broth			=q195a1_2_rec
clonevar other_liq 		=q195a1_7_rec

* Milk represents any milk consumption (animal milk, powder milk/formula and buttermilk/curd)
clonevar milk = 		 q195a1_4_rec     //  animal milk, powder milk/formula and buttermilk/curd
clonevar formula = 		 q195a1_3_rec
clonevar other_milk = 	 q195a1_6_rec     // BUTTER MILK/BEATEN CURD

clonevar freq_milk = q196a1_4
clonevar freq_formula = q196a1_3
clonevar freq_other_milk = q196a1_6

* If consumed (=1) & freq=0 then freq=1
* If not consumed (=0) & freq>0 then freq=0
replace freq_milk = 1 if milk==1 & q196a1_4==0
replace freq_milk = 0 if milk==0 & q196a1_4>0

replace freq_formula = 1 if formula==1 & q196a1_3==0
replace freq_formula = 0 if formula==0 & q196a1_3>0
replace freq_other_milk = 1 if other_milk==1 & q196a1_6==0
replace freq_other_milk = 0 if other_milk==0 & q196a1_6>0

lab var water "water"
lab var juice "juice"
lab var broth "broth"
lab var milk "milk"
lab var formula "formula"
lab var other_milk "buttermilk/curd"
lab var other_liq "other_liq"

tab freq_formula formula, m 
tab freq_milk milk, m 
tab freq_other_milk other_milk, m 

* Recode milk to include all animal milk, powder milk/formula and buttermilk/curd
replace milk =1 if       formula==1  //  powder milk/formula 
replace milk =1 if       other_milk ==1  //  buttermilk/curd




* SOLIDS SEMISOLIDS
clonevar yogurt			=q197a1_14_rec // any cheese, yogurt or other food made from milk?
clonevar fortified_food =q197a1_1_rec  // any commercially fortified food
clonevar bread 			=q197a1_2_rec  // any bread, roti, chapati, rice, noodles, biscuits, idli, porridge 
replace  bread =1 if 	 q197a1_1_rec==1 // any commercially fortified food
clonevar legume			=q197a1_3_rec  // pulses, lentils beans legumes or foods containing legumes
clonevar vita_veg		=q197a1_4_rec  // oranged fleshed vegetables
clonevar potato 		=q197a1_5_rec  // any white potato, white yam, cassava or other food made from roots
clonevar leafy_green	=q197a1_6_rec
clonevar vita_fruit		=q197a1_7_rec
clonevar fruit_veg		=q197a1_8_rec
clonevar organ			=q197a1_9_rec
gen poultry =. 
clonevar meat			=q197a1_10_rec  // includes poultry
clonevar egg			=q197a1_11_rec
clonevar fish			=q197a1_12_rec
clonevar nut			=q197a1_13_rec  // ANY FOODS MADE FROM NUTS SUCH AS PEANUTS, CASHEW NUTS, ALMOND ETC.?
*clonevar cheese		=q197a1_14_rec  included as yogurt above
*gen fat = 0                            // no data collected on oil, ghee, butter consumption in RSOC
gen cheese =. 
gen fat =. 


cap drop semisolid
clonevar semisolid		=q197a1_15_rec  // any other solid, semi-solid or soft food
replace semisolid = 1 if q199_1 >0 & q199_1 <25  // same code as freq_solids
lab var semisolid "other semi-solids"  // not included in list of foods

// tab q198, m // A5.16. Did eat any solid, semi-solid, or soft foods yesterday during the day or night
tab q197a1_15_rec, m 
tab q197a1_15_rec semisolid, m 
tab q199_1 semisolid, m 

// all 8 food groups 
// 1: Bread, rice, grains, roots and tubers
// 2: Legumes and nuts
// 3: Dairy - milk, formula, yogurt, cheese
// 4: Flesh foods(meat, fish, poultry and liver/organ meats)
// 5: Eggs
// 6: Vitamin A rich fruits and vegetables
// 7: Other fruits and vegetables
// 8: Breastmilk

lab var bread "Bread, rice, biscuits, idli, porridge"
lab val bread bread

* Define eight food groups following WHO recommended IYCF indicators
gen carb = 0
replace carb = 1 if bread ==1 | potato==1 | fortified_food==1
lab var carb "1: Bread, rice, grains, roots and tubers"
// WHO guidance
// thin porridge
// Porridge, bread, rice, noodles, or other foods made from grains
// White potatoes, white yams, manioc, cassava, or any other foods made from roots

gen leg_nut = 0
replace leg_nut = 1 if legume ==1 | nut ==1 
lab var leg_nut "2: Legumes and nuts"

gen dairy = 0
replace dairy = 1 if formula ==1 | yogurt==1 | milk==1 | other_milk==1
lab var dairy "3: Dairy - milk, buttermilk, formula, yogurt, cheese"
 
 
gen all_meat = 0
replace all_meat = 1 if organ ==1 | meat ==1 | fish ==1  
lab var all_meat "4: Flesh foods(meat, fish, poultry and liver/organ meats)"

lab var egg "5: Eggs"

gen vita_fruit_veg = 0
replace vita_fruit_veg = 1 if vita_fruit ==1 | vita_veg==1 | leafy_green==1
lab var vita_fruit_veg "6: Vitamin A rich fruits and vegetables"

lab var fruit_veg "7: Other fruits and vegetables"

foreach var of varlist water juice  broth milk formula other_liq carb leg_nut dairy all_meat egg vita_fruit_veg fruit_veg currently_bf {
	lab val `var' no_yes
}
* Test all liquids
foreach var of varlist water juice  broth milk  formula other_milk other_liq {
	tab `var' , m
}
* Test all 8 food groups 
foreach var of varlist carb leg_nut dairy all_meat egg vita_fruit_veg fruit_veg currently_bf {
	tab `var' , m
}


* Age groups in blocks of 6 months
gen agegroup = floor(age_days/183 +1)   //agemons/6 +1
lab def agegroup 1 "0-5m" 2 "6-11m" 3 "12=17m" 4 "18-23m" 5 "24-29m" 6 "30-35m" 7 "36-41m" 8 "42-47m" 9 "48-53m" 10 "54-59m"
lab val agegroup agegroup


* Number of food groups out of eight
cap drop sumfoodgrp
egen sumfoodgrp = rowtotal (carb leg_nut dairy all_meat egg vita_fruit_veg fruit_veg currently_bf)
tabulate sumfoodgrp, generate(fg)
rename (fg1 fg2 fg3 fg4 fg5 fg6 fg7 fg8 ) ///
	   (fg0 fg1 fg2 fg3 fg4 fg5 fg6 fg7 )		

* Any solid/semi-solid food consumption -  includes all food groups plus semisolid
* Does NOT include currently breastfeeding
cap drop any_solid_semi_food
egen any_solid_semi_food = rowtotal (carb leg_nut dairy all_meat egg vita_fruit_veg fruit_veg semisolid)
replace any_solid_semi_food = 1 if any_solid_semi_food >1
replace any_solid_semi_food = 1 if q199_1 >0 & q199_1 <22 // frequency of feeding
tab any_solid_semi_food, m 
cap drop any_solid_semi_food_x
gen any_solid_semi_food_x = any_solid_semi_food*100
// graph bar (mean) any_solid_semi_food_x if agemos<24, over(agemos)
cap drop any_solid_semi_food_x

*Introduction to the semi_solid, solid, soft_food in children from 6-8 months of age
* based on 
* q197a1_15_rec :ANY OTHER SOLID OR SEMI-SOLID FOOD?
* q198: did [name] eat any solid, semisolid, or soft foods yesterday during the day
* q199_1: number of |times index child ate solid, semi solid, soft foods yesterday 
* sumfoodgrp - number of food groups eaten yesterday
* any_solid_semi_food- any semi-solids eaten yesterday

* Questionnaire flow
* if consumption of any semi-solids yesterday
* filter - yes / no
* number of times consumed  semi-solids yesterday

tab q197a1_15_rec // Did child eat any other semi-solid foods
tab q198 		  // Did child eat any semi-solid in past 24 hours
tab q199_1        // How many times did child eat  semi-solid in past 24 hours
tab q199_1 q198

*Introduction to the semi_solid, solid, soft_food in children from 6-8 months of age
* Error - see WHO Guidance. New indicator not based on v414s: gave child solid, semi solid, soft foods yesterday 
cap drop intro_compfood
cap drop isssf
gen isssf = 0
replace isssf = . if any_solid_semi_food == .
replace isssf = 1 if  any_solid_semi_food>=1 
replace isssf = . if age_days<=183 | age_days>=273
*Corrected error in age range
la var isssf "Intro to semi_solid, solid, soft_food 6-8 months of age"
tab  q199_1 isssf, m 
tab  any_solid_semi_food isssf, m 




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
tab ebf agemos

gen ebf_denom = 1 
replace ebf_denom = . if agemos>=6

gen ebf_x = ebf*100
version 16: table one [pw =  N_WT] if ebf_denom==1, c(mean ebf_x n ebf_x) format(%9.1f)
// RSOC   REPORT  EBF<6M  	64.9     9,281


*Breastfeeding area graph
* breastfeeding status
// gen diet=1
// replace diet=2 if (v409>=1 & v409<=7) 					// water
//
// foreach xvar of varlist v409a v410 v410a v412c v413*{ 	// other liquids
// 	replace diet=3 if `xvar'>=1 & `xvar'<=7
// }
// foreach xvar of varlist v411 v411a {  					// other milks
// 	replace diet=4  if `xvar'>=1 & `xvar'<=7
// }
// foreach xvar of varlist v414* { 						// solids
// 	replace diet=5 if `xvar'>=1 & `xvar'<=7
// }
// replace diet=5 if v412a==1 | v412b==1 | m39a==1
// replace diet=0 if m4!=95
* add missing 




* MEDIAN duration of exclusive breastfeeding
cap drop age_ebf
gen age_ebf = round(age_days/30.4375, 0.01)   //exact age in months round of to 2 digits after decimal
replace age_ebf = . if age_days >730
*set agemos_ebf to missing if exbf=no
replace age_ebf=. if ebf==0
la var age_ebf "Median age of exclusive breasfeeding in months"
sum age_ebf [aw=N_WT] , d

* MEDIAN duration of continued breastfeeding
gen age_cbf = round(age_days/30.4375, 0.01)   //exact age in months round of to 2 digits after decimal
replace age_cbf=. if currently_bf !=1
la var age_cbf "Median age of continued breasfeeding in months"
sum age_cbf [aw=N_WT] , d

*Continued breastfeeding
tab q190
gen cont_bf = 0
replace cont_bf = 1 if q190 == 1 
tab cont_bf q190

gen cont_bf_12_23 = cont_bf if age_days>335 &age_days<730 
tab cont_bf_12_23, m


*Minimum Dietary Diversity- code for new indicator definition 
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

tab q197a1_15  , m 
tab q198   , m 
tab q199_1 , m 
tab q199dk_98  , m 

tab q199_1 q198, m 

cap drop freq_solids
gen freq_solids =0 if q198==2  // zero freq_feeds
replace freq_solids = q199_1 if q199_1 <=7 
replace freq_solids = 7 if q199_1 >7 & q199_1 <=22 

* Number of freq_solids, don't know and missing
replace freq_solids =9 if freq_solids>7 & q198==.  // missing
replace freq_solids =9 if freq_solids==.  // don't know
replace freq_solids =8 if q198==3  // don't know
replace freq_solids =8 if q199dk_98 ==1  // don't know

tab freq_solids, m

* Cannot give any # of freq_feeds to those yes any_solid_semi_food cannot be coded. 
* There 1251 cases of yes any_solid_semi_food but 0 freq_feeds
// replace freq_solids =9 if q199_1==. & any_solid_semi_food==1 // missing frequency and 1+ food groups
tab freq_solids any_solid_semi_food, m 

la def freq_solids 0 none, add 
la def freq_solids 7 "7+", add 
la def freq_solids 8 "don't know", add 
la def freq_solids 9 missing, add 
la val freq_solids freq_solids

tab q199_1 freq_solids,m
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

*Minimum Meal Frequency (MMF) for Breastfeeding Children
gen mmf_bf=0
replace mmf_bf=1 if freq_solids>=2 & currently_bf==1 & age_days>183 & age_days<243 
replace mmf_bf=1 if freq_solids>=3 & currently_bf==1 & age_days>=243 & age_days<730 
replace mmf_bf =. if age_days<=183 | age_days>=730
replace mmf_bf =. if currently_bf!=1
la val mmf_bf no_yes
tab mmf_bf, m 

*For currently non-breastfed children: MMF is met if children 6-23 months of age receive solid, semi-solid or soft foods or milk feeds at least 4 times during the previous day and at least one of the feeds is a solid, semi-solid or soft feed*******
* Variable fed_milk refers to number of times a child received milk other than breastmilk i.e. other animal milk*
* Variable fed_formula refers to number of times a child received infant formula*

* Generates the total number of times a non-breastfed child received solid, semi-solid or soft foods or milk feeds*
tab milk, m

* Frequency of milk consumption in code above
tab freq_milk
tab freq_formula
tab freq_other_milk

* Frequency of milk and non solid semi-solid dairy feeds in children 0-23 months
gen milk_feeds= freq_milk + freq_formula + freq_other_milk	
replace milk_feeds = 7 if milk_feeds>=7 & milk_feeds !=.
la val milk_feeds feeds
replace milk_feeds =. if age_days<0 | age_days>=730
tab milk_feeds, m 
* combines number of times fed milk, buttermilk, infant formula and yogurt. Please ensure there are no missings(.) 
* all missings have been set to 0 to allow summation across row. 

* Do not double count yogurt
* it is included in solids


* OVERALL FREQUENCY FEEDS
gen feeds= milk_feeds + freq_solids	
* use milk feeds for all liquid milk feeds
* use freq_solids for all semi-solid feeds
replace feeds = 7 if feeds>=7 & feeds !=.
la def feeds 7 "7+ feeds"
la val feeds feeds
la var feeds  "Frequency of solid, semi-solid, all milk feeds"
tab feeds, m 	
* Ensure there are no missings(.) and all missings have been set to 0 in the age group to allow summation across row. 
* STATA will mark feeds as missings if any of them are missing.

*Minimum Meal Frequency (MMF) NON Breastfeeding
*  For currently non-breastfed children: Children 6-23 months of age who received solid, semi-solid or soft foods or milk feeds at least 4 times 
* during the previous day and at least one of the feeds was a solid, semi-solid or soft feed.
gen mmf_nobf=0
replace mmf_nobf=1 if feeds>=4 & freq_solids>=1 & currently_bf!=1
replace mmf_nobf =. if age_days<=183 | age_days>=730 
replace mmf_nobf =. if currently_bf==1
tab mmf_nobf, m 


* Minimum Milk / Dairy frequency for Non-Breastfed Child
* include milk, formula and yogurt
gen min_milk_freq_nbf =0
replace min_milk_freq_nbf =1 if milk_feeds >=2 & currently_bf!=1 

***************************
* DOUBLE CHECK IF YOGURT IS INCLUDED HERE
*replace min_milk_freq_nbf =1 if yogurt_freq >=2 & currently_bf!=1 
**********************************

replace min_milk_freq_nbf =. if age_days<=183 | age_days>=730
replace min_milk_freq_nbf =. if currently_bf==1
la var min_milk_freq_nbf "Minimum Milk Frequency for Non-Breastfed Child"
tab min_milk_freq_nbf, m 
// graph bar (mean) min_milk_freq_nbf currently_bf if agemos < 24, over(agemos) 

*MMF among all children 6-23 months
gen mmf_all=0
replace mmf_all=1 if mmf_bf==1 | mmf_nobf==1
replace mmf_all =. if age_days<=183 | age_days>=730
la var mmf_all "Minimum meal frequency for all children 6-23M"
tab mmf_all, m 

**Minimum Acceptable Diet (MAD) 
*** Generates milk feeds for non-breastfed infants ***
*** Milk feeds include milk other than breastmilk, infant formula and yogurt*

* Variable freq_yogurt refers to number of times a child received yogurt*

* For each one please remember to set don't know to zero
* for NFHS (DHS) replace fed_yogurt=0 if fed_yogurt>7  - don't know=8

* Mixed milk feeding 0-5M
* Mixed milk feeding (<6 months): Percentage of infants 0–5 months of age who 
* were fed formula and/or animal milk in addition to breast milk during the previous day
gen mixed_milk = 0 
replace mixed_milk=1 if (currently_bf==1 & milk==1)
replace mixed_milk=1 if (currently_bf==1 & formula==1)
replace mixed_milk=1 if (currently_bf==1 & other_milk==1)
replace mixed_milk =. if age_days<0 | age_days>=183 
tab mixed_milk, m 

tab mdd
tab mmf_all
tab currently_bf
tab min_milk_freq_nbf

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



*SOCIO-ECONOMIC-DEMOGRAPHIC VARS


* LBW              (low birth weight)

gen birth_weight = (q181_1*1000) + q181_2 
replace birth_weight = 9999 if birth_weight == .
label def bw 9999 "Missing", replace
label val birth_weight bw
label var birth_weight "Birth weight"
replace birth_weight = birth_weight/1000 if birth_weight != 9999


recode birth_weight (0/0.249=6)(0.25/1.499=1)(1.5/2.499=2)(2.5/3.999=3)(4/10.999=4)(11/10000=7), gen(cat_birth_wt)
// egen c_birth_wt = cut(birth_weight), at(0.25,1.5,2.5,4,9001,9997,9999,10000) icodes
label def cat_birth_wt 1 "Very low <1.5kg" 2 "Low <2.5kg" 3 "Average" 4 "High >4kg" 5 "Not weighed" 6 "Don't know" 7 "Missing"
label val cat_birth_wt cat_birth_wt
label var cat_birth_wt "Birth weight category"
tab cat_birth_wt, m 

cap drop lbw
gen lbw = . 
replace lbw = 1 if birth_weight <2.500
replace lbw = 0 if birth_weight >=2.500 
replace lbw = . if cat_birth_wt >=5
tab birth_weight lbw, m
tab lbw

* early ANC <=3 months first trimester
tab q144
tab q145_1
gen earlyanc = 0
replace earlyanc =1 if q145_1 <=3
replace earlyanc =. if age_days>=730
tab q145_1 earlyanc, m

* ANC 4+ 
tab q146_1
gen anc4plus = 0
replace anc4plus = 1 if q146_1 >=4 & q146_1 <=9
replace anc4plus =. if age_days>=730
tab q146_1 anc4plus, m

* C-section
tab q163
gen csection = 0
replace csection = 1 if q163==2
tab q163 csection, m 

* Institutional Birth
// inst_birth 
tab q159
la list q159
recode q159 (1=1)(2=2)(3=3)(96 99=4), gen(inst_birth)
la def inst_birth 1 public 2 private 3 home 4 "other-missing"
la val inst_birth inst_birth
la var inst_birth "Institutional Birth"
tab q159 inst_birth, m 

* Birth Order		  
// bord 
gen bord = q137_1

* mum_work 
tab q115 q114, m 
la list q114
cap drop mum_work
recode q114 (2=0)(1=1)(9=.),gen(mum_work)
la var mum_work "Mother working"
tab q114 mum_work, m 

* mothers education
tab q113_1, m 
recode q113_1 (0=1)(1/4=2)(5/9=3)(10/11=4)(12/25=5)(26/max=99), gen(mum_educ)
la def mum_educ 1 "No Education" 2 "<5 years completed" 3 "5-9 years completed" ///
	4 "10-11 years completed" 5 "12+ years completed" 99 "Missing"
label val mum_educ mum_educ
label var mum_educ "Mother's education"
tab   q113_1 mum_educ



* caste
gen caste = q45
replace caste = 5 if q45 ==98
replace caste = 6 if q45 ==.
lab define caste 1 "Scheduled caste" 2"Scheduled tribe" 3"OBC"  4"Others" 5 "Missing/don't know" 
label val caste caste
label var caste "Caste"
tab q45 caste, m

* Residence Rural Urban
*Residence
tab q4, m 
recode q4 (2=1) (1=2), gen(rururb)
lab define rururb 1 "Urban" 2"Rural"
lab val rururb rururb   
lab var rururb "Residence"
tab rururb q4, m 




* socio-economic status
clonevar wi = wealth_index
// wealth_index:
//            1 Lowest
//            2 Second
//            3 Middle
//            4 Fourth
//            5 Highest
//also available, wealth index factor score
tab wi
gen wi_s=.



* Sex of child 
* 1 = male / 2 = female
tab q135_1 // pregnancy number
gen sex=.
replace sex = q129_1 if q135_1==1
replace sex = q129_2 if q135_1==2
replace sex = q129_3 if q135_1==3
replace sex = q129_4 if q135_1==4
replace sex = q129_5 if q135_1==5
replace sex = q129_6 if q135_1==6

// q129_1 q129_2 q129_3 q129_4 q129_5 q129_6
la val sex q280
tab sex, m 
tab sex q280, m  // q280 is only for section B2.0 vaccination /child ilness

* Survey Weights
gen national_wgt = N_WT 
* Regional weights 
gen regional_wgt = S_WT

gen state_wgt =S_WT


* Child Illness
* Diarrhea
tab q257
recode q257 (1=1)(2 8=0), gen(diar)
tab diar q257
* Fever
 tab q263
recode q263 (1=1)(2 8 98=0), gen(fever)
tab fever q263	
* Cough with rapid breathing excluding those with only nasal breathing problems
* q266 q267 q268
recode q266 (1=1)(2 8=0), gen(ari)
lab val diar fever ari no_yes
tab q267 ari
replace ari=0 if q267!=1
replace ari=0 if q268==2 | q268==96 | q268==98
tab ari q267
tab ari q268
tab ari, m 







* Include state, district and other identification variables
clonevar state_rsoc = q1a 
tab q1a
* Only 19 states. 

gen state = .
// 1 "A&N islands"
replace state =2  if state_rsoc ==28
replace state =3  if state_rsoc ==12
replace state =4  if state_rsoc ==18
replace state =5  if state_rsoc ==10
// 6 Chandigarh
replace state =7  if state_rsoc ==22
// 8 "Dadra and Nagar Haveli"
// 9 "Daman and Div"
replace state =10  if state_rsoc ==30
replace state =11  if state_rsoc==24
replace state =12  if state_rsoc==6
replace state =13  if state_rsoc==2
replace state =14  if state_rsoc==1
replace state =15  if state_rsoc==20
replace state =16  if state_rsoc==29
replace state =17  if state_rsoc==32
// 18 Lakshadweep
replace state =19  if state_rsoc==23
replace state =20  if state_rsoc==27
replace state =21  if state_rsoc==14
replace state =22  if state_rsoc==17
replace state =23  if state_rsoc==15
replace state =24  if state_rsoc==13
replace state =25  if state_rsoc==7
replace state =26  if state_rsoc==21
// 27 Puducherry
replace state =28  if state_rsoc==3
replace state =29  if state_rsoc==8
replace state =30  if state_rsoc==11
replace state =31  if state_rsoc==33
replace state =32  if state_rsoc==16
replace state =33  if state_rsoc==9
replace state =34  if state_rsoc==5
replace state =35  if state_rsoc==19


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
tab state state_rsoc, m 

gen round =2

drop if state==30 & round ==2


keep psu hh_num one int_date birthday birthmonth birthyear dateofbirth age_days agemos  ///
	evbf eibf eibf_timing ebf2d ebf3d ebf age_cbf age_ebf prelacteal_milk ///
	prelacteal_water prelacteal_sugarwater prelacteal_gripewater /// 
	prelacteal_saltwater prelacteal_formula prelacteal_honey ///
	prelacteal_janamghuti prelacteal_other bottle water juice milk ///
	formula other_liq juice broth yogurt fortified_food bread vita_veg ///
	potato leafy_green any_solid_semi_food vita_fruit fruit_veg organ meat ///
	egg fish cont_bf semisolid carb leg_nut dairy all_meat vita_fruit_veg ///
	agegroup sumfoodgrp diar fever ari cont_bf cont_bf_12_23 ///
	isssf mdd currently_bf freq_solids mmf_bf freq_milk ///
	freq_formula freq_other_milk milk_feeds feeds mmf_nobf min_milk_freq_nbf ///
	mmf_all mixed_milk mad_all egg_meat zero_fv sugar_bev unhealthy_food ///
	lbw cat_birth_wt anc4plus csection earlyanc mum_educ caste rururb wi wi_s state ///
	sex national_wgt regional_wgt state_wgt round ebf_denom mum_work inst_birth bord 


* Save data with name of survey
save iycf_rsoc, replace 

*END










