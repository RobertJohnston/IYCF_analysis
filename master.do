* Creates IYCF database from most recent 4 survey India (NFHS3 RSOC NFHS4 CNNS)
* IYCF Variables for multi-survey analysis

version 16

* To change

// does bread include gruel? no
// carb includes gruel

// colostrum
// RSOC state
// RSOC region

// birthday, month year	




// broth NFHS
// bread
// water CNNS
// juice CNNS
// milk CNNS
// potato CNNS
// leafy_green CNNS
// vita_fruit CNNS
// fruit_veg CNNS
// egg CNNS
// semisolid CNNS
// carb CNNS
// leg_nut CNNS
// fever RSOC
// LBW
// mum_educ
// caste
// region, national, state weights


// cd "C:\Users\rober\OneDrive - UNICEF\1 UNICEF Work\1 moved to ECM\IIT-B\IYCF\analysis"
// cd "C:\Users\dnyan\OneDrive\Documents\UNICEF FELLOWSHIP\CNNS\Merged"


* NFHS-3
clear 
cd "C:\Users\stupi\OneDrive - UNICEF\1 UNICEF Work\1 moved to ECM\IIT-B\IYCF\analysis"
run make_IYCF_vars_NFHS3.do

* NFHS4
clear 
cd "C:\Users\stupi\OneDrive - UNICEF\1 UNICEF Work\1 moved to ECM\IIT-B\IYCF\analysis"
run make_IYCF_vars_NFHS4.do

*RSOC
clear 
cd "C:\Users\stupi\OneDrive - UNICEF\1 UNICEF Work\1 moved to ECM\IIT-B\IYCF\analysis"
run make_IYCF_vars_RSOC.do

*CNNS
clear 
cd "C:\Users\stupi\OneDrive - UNICEF\1 UNICEF Work\1 moved to ECM\IIT-B\IYCF\analysis"
run make_IYCF_vars_CNNS.do

* NFHS5
clear  
cd "C:\Users\stupi\OneDrive - UNICEF\1 UNICEF Work\1 moved to ECM\IIT-B\IYCF\analysis"
run make_IYCF_vars_NFHS5.do


*MERGE ALL DATASETS

cd "C:/Temp/Data"

use  "iycf_NFHS5" , clear
tab round, m 
append using "iycf_NFHS4"
tab round, m 
append using "iycf_NFHS3"
tab round, m 
append using "iycf_rsoc"
tab round, m 
append using "iycf_cnns"

la def round 1 "NFHS-3" 2 RSOC 3 "NFHS-4" 4 CNNS 5 "NFHS-5"
la val round round


* Generate 'region' variable
gen region= .
replace region=1 if state==25 |  state==12 | state==13 | state==14 | state==28 | state==29 | state==34 | state==6 | state==37
replace region=2 if state==7 |  state==19 | state==33
replace region=3 if state==5 |  state==35 | state==15 | state==26
replace region=4 if state==3 |  state==30 | state==32 | state==22 | state==4 | state==24 | state==21 | state==23
replace region=5 if state==11 |  state==20 | state==10 | state==8 | state==9
replace region=6 if state==2 |  state==16 | state==17 | state==31 | state==36 | state==1 | state==27 | state==18

// list state state_num if region==.


/*
------------------------------------------------------------------------------------------------------------------------------------------------
region       Region Name       states included in the region (state)
-------------------------------------------------------------------------------------------------------------------------------------------------
region 1       North           NCT of Delhi(25), Haryana(12), Himachal Pradesh(13), Jammu and Kashmir(14), Punjab(28), Rajasthan(29), Uttarakhand(34)
				        	   Chandigarh (6) Ladakh (37)			 							   
region 2	   Central		   Chhattisgarh(7), Madhya Pradesh(19), Uttar Pradesh(33)
									
region 3	   East			   Bihar(5), West Bengal(35), Jharkhand(15), Odisha(26)	 							

region 4       NorthEast       Arunachal Pradesh(3), Sikkim(30), Tripura(32),  Meghalaya(22), Assam(4), Nagaland(24), Manipur(21), Mizoram(23)

region 5       West            Gujarat(11), Maharshtra (20), Goa(10), Dadra & Nagar Haveli (8), Daman and Diu (9) 


region 6       South           Andhra Pradesh(2),  Karnataka(16),  Kerala(17),  Tamil Nadu(31),  Telangana(36)  A&N islands (1) Puducherry (27) Lakshadweep (18)
--------------------------------------------------------------------------------------------------------------------------------------------------
*/

* In NFHS use national weights for region
lab define region 1 "North" 2 "Central" 3 "East" 4 "Northeast" 5 "West" 6 "South"
lab var region "Region" 
lab val region region

* Add regional weights



*Month of survey
 gen int_month = month(int_date)
label define m12 1 "Jan" 2 "Feb" 3 "Mar" 4 "Apr" 5 "May" 6 "Jun" 7 "Jul" 8 "Aug" 9 "Sep" 10 "Oct" 11 "Nov" 12 "Dec"
label val int_month m12 
label var int_month "Month"


* add bottle to RSOC
sort round agemos
* set one case to be present in following
* add bottle to RSOC
replace bottle = 0 if _n == 51556
* add broth to NFHS-3
replace broth = 0 if _n == 1

* Birth order
recode bord (5/20=5)
la def bord 5 "5+"
la val bord bord

* Number of hh members
replace hh_mem = 10 if hh_mem>10 & hh_mem!=.
replace hh_mem = 2 if hh_mem<2
la def hh_mem  10 "10+"
la val hh_mem  hh_mem
tab hh_mem round, col
version 16: table round, c(mean hh_mem)
* test sample size for IYCF ages 
tab hh_mem round if agemos>=6 & agemos<24

* introduction of complementary foods
tab agemos any_solid_semi_food
// 2.1. Introduction of solid, semi-solid or soft foods 6–8 months (ISSSF) 
tab agemos isssf


save "C:\Temp\Data\iycf_5surveys.dta", replace



/*
*The IYCF vars in the dataset - 

*Var_name                                        Description 
------------------------------------------------------------------------------------------
birthday                                         day of birth of child
birthmonth                                       month of birth of child
birthyear                                        year of birth of child
int_y                                            interview year
int_m                                            interview month
int_d                                            interview day
int_date                                         interview date 
age_days                                         age of child in days 
agemos                                           age of child in months
---------------------------------------------------------------------------------------------
evbf                                             children below 24 months ever breastfed
eibf                                             children below 24 months early initiation of breastfeeding
eibf_timing                                      timing from birth of child to start breastfeeding
ebf2d                                            exclusive breastfeeding for first two days
--------------------------------------------------------------------------------------------
          PRELACTEAL FEED (GIVEN SOMETHING TO EAT WITHIN 1st 3 DAYS of BIRTH)
prelacteal_milk                                  given child milk within 3 days after birth
prelacteal_water                                 given child water within 3 days after birth
prelacteal_sugarwater                            given child sugarwater within 3 days after birth
prelacteal_gripewater                            given child gripewater within 3 days after birth
prelacteal_saltwater                             given child saltwater within 3 days after birth
prelacteal_formula                               given child formula within 3 days after birth
prelacteal_honey                                 given child honey within 3 days after birth
prelacteal_janamghuti                            given child janamghuti within 3 days after birth
prelacteal_other                                 given child other than milk drinks (including above) within 3 days after birth

colostrum                                        Weather child Child recieved colostrum (data for NFHS 3 and NFHS 4 is not available) 
bottle                                           Percentage of children 0–23 months of age who were fed from a bottle with a nipple during the previous day
--------------------------------------------------------------------------------------------------
Following are the Food groups, which child has consumed a day before interview
--------------------------------------------------------------------------------------------------
                    LIQUIDS
water                                            
juice 
broth                                         **Not available in NFHS data 
milk 
formula 
other_liq 
                   SOLID / SEMI-SOLIDS
yogurt 
fortified_food 
bread                                         any bread, roti, chapati, rice, noodles, biscuits, idli, porridge
legume 
vita_veg                                      Any pumpkin, carrots, squash or sweet potatoes that are yellow or orange inside
potato                                        any white potato, white yam, cassava or other food made from roots
leafy_green                                   Any dark green, leafy vegetables
vita_fruit                                    Any ripe mango, papaya, cantaloupe or jackfruit
fruit_veg                                     other fruits and vegetables
organ                                         Any liver, kidney, heart or other organ meat
meat                                          Any other meat
egg  
fish 
semisolid                                     Any other solid, semi-solid, or soft food
------------------------------------------------------------------------------------------------------
                  8 Food groups 

carb                                          grains, white/pale starchy roots, tubers and plantains (Bread, potato, gruel, etc)
leg_nut                                       beans, peas, lentils, nuts, legumes
dairy                                         Milk, Formula, Yogurt, curd, etc. dairy products
all_meat                                      flesh foods (meat, fish, poultry, organ meats)
vita_fruit_veg                                vitamin A-rich fruits and vegetables
fruit_veg                                     other fruits and vegetables
cont_bf                                       here for breastmilk we have used a indicator "cont_bf" where response is still breastfeeding
egg                                           
------------------------------------------------------------------------------------------------------
sumfoodgrp                                    Sum of food groups (from 8 food groups) consumed by a child 
------------------------------------------------------------------------------------------------------
cont_bf                                       Continued breastfeeding
cont_bf_12_23                                 percentage of children 12–23 months of age who were fed breast milk during the previous day.
intro_compfood                                percentage of infants 6–8 months of age who consumed solid, semisolid or soft foods during the previous day
mdd                                           percentage of children 6–23 months of age who consumed least five out of eight food groups during the previous day                         
currently_bf                                  
freq_solids                                   Number of times child consumed any solid/semi-solid food on day before interview
mmf_bf                                        Minimum meal frequency for breastfed children
freq_milk                                     Number of times child consumed a milk other than BM on day before interview
freq_formula                                  Number of times child consumed formula on day before interview
freq_yogurt                                   Number of times child consumed yogurt on day before interview
milk_feeds                                    frequency of (milk+formula+yogurt)
feeds                                         Frequency of solid, semi-solid, milk and formula feeds 
mmf_nobf                                      Minimum Meal Frequency (MMF) for NON Breastfeeding
min_milk_freq_nbf                             Minimum milk Frequency (MMF) NON Breastfeeding
mmf_all                                       MMF for all children (BF+NBF) 
mixed_milk                                    Percentage of infants 0–5 months of age who were fed formula and/or animal milk in addition to breast milk during the previous day
mad_all                                       Minimum Adequacy of Diet among all infants 6-23 months of age
egg_meat                                      % of children 6-23months of age who consumed egg and/or flesh food during the previous day
zero_fv                                       % of children 6-23months of age who did not consume any fruits or vegetables during the previous day
sugar_bev                   NA                  **consumption of sugar sweetened beverages by child agemons 6 to 23** (no relevant question in the dataset)
unhealthy_food              NA                  **Need clear definition of unhealthy foods - from WHO guidance

agegroup                                      Age groups in blocks of 6 months   
-------------------------------------------------------------------------------------------------------------
diar                                         % children had diarrhoea in last 2 weeks
fever                                        % children had fever in last 2 weeks
ari                                           Cough with rapid breathing excluding those with only nasal breathing problems

lbw                                           Low birth weight
earlyamc                                      early ANC : ANC check-up in <=3 months or first trimester 
anc4plus                                      ANC 4+ (Pregnent women receiving more than 4 ANC check-ups)                                      
csection                                      child is delivered by caesarean section,
-------------------------------------------------------------------------------------------------------------
                   SOCIO-ECONOMIC VARS
mum_educ                                      Mother's education ()
caste                                         
rururb                                        rural / urban
wi                                            wealth index
state                          
region                                        region based on geography (east, west, north, south, central)
sex                                           sex/gender of child 
national_wgt                                  national sample weight
state_wgt                                     state sample weight
round                                         Round indicates the survey Round1 - NFHS3, Round 2 - RSOC, round3 - NFHS 4, Round4 - CNNS
------------------------------------------------------------------------------------------------------------------
*/





log using IYCF_5_survey_data.txt, replace text
 
* Round
tab round, m 

*Double check all vars 

tab sex round, col  

tab state, m 
tab state round,m

tab region round,m

tab birthday round,m
tab birthmonth round,m
tab birthyear round,m

tab agegroup round,m

* keep all children under three 
drop if age_days > 1095
tab agegroup round,m



* Liquid variables
tab evbf round, col 
tab evbf round, col m

tab eibf round,col 
tab eibf round,col m 

* initation of breastfeeding in hours
version 16: table round, c (mean eibf_timing) 

* recommended ebf in 1st two days
tab ebf2d round,m
* original ebg in 1st 3 days
tab ebf3d round,col 
tab ebf3d round,col m

* Not correct for NFHS5
tab ebf round, col   
tab ebf round, m 

tab cont_bf round, col 
tab cont_bf round,m 


 *Prelacteal feeds
tab prelacteal_milk round, col
tab prelacteal_milk round,m

tab prelacteal_formula round,col
tab prelacteal_formula round,m

tab prelacteal_gripewater round, col
tab prelacteal_gripewater round,m

tab prelacteal_sugarwater round, col
tab prelacteal_sugarwater round,m

tab prelacteal_saltwater round, col
tab prelacteal_saltwater round,m

tab prelacteal_water round, col 
tab prelacteal_water round,m

tab prelacteal_honey round, col
tab prelacteal_honey round,m

tab prelacteal_other round, col
tab prelacteal_other round,m

* Ayurvedic medicine which cures stomach ailments/constipation in babies 
tab prelacteal_janamghuti round, col
tab prelacteal_janamghuti round,m

// tab colostrum round,m
// * missing NFHS3, 4, 5 and CNNS

tab bottle round, col
tab bottle round,m
* missing bottle with nipple RSOC

tab water round, col     
tab water round,m       
                                    
tab juice round, col
tab juice round,m

tab broth round, col
tab broth round,m

tab milk round, col
tab milk round,m 

tab formula round, col
tab formula round,m 

tab other_liq round, col
tab other_liq round,m


tab mixed_milk round, col
tab mixed_milk round,m
* Mixed milk feeding 0-5M
* Mixed milk feeding (<6 months): Percentage of infants 0–5 months of age who 
* were fed formula and/or animal milk in addition to breast milk during the previous day
* formerly predominant breastfeeding - misnomer




foreach var of varlist v409- v414u {
 do 
}




* Food variables
tab yogurt round, col 
tab yogurt round,m 
graph bar (mean) yogurt, over(int_month)

tab fortified_food round, col 
tab fortified_food round,m 
graph bar (mean) fortified_food, over(int_month)

tab bread round, col
tab bread round,m 

tab vita_veg round, col
tab vita_veg round,m 

tab potato round, col 
tab potato round,m 

tab leafy_green round, col
tab leafy_green round,m 

tab vita_fruit round, col
tab vita_fruit round,m 

tab fruit_veg round, col
tab fruit_veg round,m 

tab organ round, col
tab organ round,m 

tab meat round, col 
tab meat round,m 

tab egg round, col
tab egg round,m 

tab fish round, col 
tab fish round,m 

tab semisolid round, col
tab semisolid round,m 
* NFHS 4 problem?

tab carb round, col
tab carb round,m 

tab leg_nut round, col 
tab leg_nut round,m 
* NFHS 4 problem?

tab dairy round, col 	 
tab dairy round,m 



tab all_meat round, col 
tab all_meat round,m 

tab egg round, col  
tab egg round,m 

tab vita_fruit_veg round, col 
tab vita_fruit_veg round, m 

tab fruit_veg round, col  
tab fruit_veg round,m 


* Composite diet variables

* Food variables
tab isssf round, col 
tab isssf round,m 
graph bar (mean) isssf, over(int_month)

tab sumfoodgrp round, col
tab sumfoodgrp round,m

tab mdd round, col
tab mdd round,m

la list mmf
replace mmf_bf = 1 if mmf_bf==2
la drop mmf
la val mmf_bf no_yes

tab mmf_bf round, col	 
tab mmf_bf round,m

tab mmf_nobf round, col
tab mmf_nobf round,m

tab mmf_all round, col
tab mmf_all round,m
* As Frequency of Milk feeds is not in NFHS 3 DATA, WE ARE UNABLE TO CREATE THE MMF for Non_BF CHILDREN  VARIABLE

tab mad_all round, col
tab mad_all round,m
* Minimum Acceptable Diet (MAD) in NFHS-3 is only for BF children
* % of non BF children in NFHS-3 = 16%
* cannot calculate for all children as do not have frequency of milk feeds in non_BF children

tab egg_meat round, col
tab egg_meat round,m
	 
tab zero_fv round, col
tab zero_fv round,m

* Health and Illness variables
tab diar round, col
tab diar round,m

tab fever round, col    
tab fever round,m        

tab ari round, col
tab ari round,m

tab lbw round, col
tab lbw round,m

tab cat_birth_wt round, col
tab cat_birth_wt round,m

tab anc4plus round, col
tab anc4plus round,m

tab earlyanc round, col
tab earlyanc round,m

tab csection round, col 
tab csection round,m

* Socio-economic variables
tab bord round, col 
tab bord round,m

tab inst_birth round, col
tab inst_birth round, m

tab mum_work round, col 
tab mum_work round,m

tab mum_educ round, col 
tab mum_educ round,m

tab caste round, col
tab caste round,m

tab rururb round, col
tab rururb round,m

tab wi round, col
tab wi round,m




tab anc_BFcounsel round, col
tab anc_BFcounsel round,m

tab pnc_child_visit round, col
tab pnc_child_visit round,m

tab pnc_assistance round, col
tab pnc_assistance round,m

* to update
log close 
end







	 
	 
	 
	 
	 