// Causal connection between early marriage and decision making power//

* sidh's path
use caseid mm1_* mmidx_* using "/Users/sidhpandit/Desktop/DHS/data/nepal/nepal 2016 ir/NPIR7HFL.DTA", clear

* test change by sidh

// Step 1: Load IR file and save a copy of sibling roster
use "NPIR7HFL.DTA", clear

// creating younger sister//
keep caseid mm1_* mmidx_*   // keep only IDs + sibling roster
save "sibling_temp.dta", replace

* Step 2: Reshape only sibling roster
reshape long mm1_ mmidx_, i(caseid) j(siborder)
* now the data is at the level of sibling

* Drop respondent herself (mmidx = 0)
drop if mmidx_ == 0
* mmidx is index to maternal mortality?

* Keep only sisters
keep if mm1_ == 2   // 2 = female

* Identify younger sisters
gen youngsis = (mmidx_ > 0)

* Collapse back to woman level
collapse (sum) youngsis, by(caseid)
save "youngsis_temp.dta", replace

* Step 3: Merge back to full IR file
use "NPIR7HFL.DTA", clear
merge 1:1 caseid using "youngsis_temp.dta"

* Replace missing with zero (women who had no younger sisters)
replace youngsis = 0 if missing(youngsis)

// brother and sister separate// 

Reshape sibling history (mm1_01, mm1_02, …) to long format
reshape long mm1 mm3, i(caseid) j(sibnum)

* mm1 = sex of sibling (1=male, 2=female)
* mm3 = age of sibling
* v012 = age of respondent

* Create flag for younger sisters
gen youngsis = (mm1==2 & mm3 < v012 & mm3 != .)

* Create flag for younger brothers (optional, in case you want to compare)
gen youngbro = (mm1==1 & mm3 < v012 & mm3 != .)

* Count number of younger sisters and younger brothers for each respondent
bysort caseid: egen n_youngsis = total(youngsis)
bysort caseid: egen n_youngbro = total(youngbro)


//////////// ever married women with younger sister/////////////////////

* Keep only ever-married women
keep if inlist(v502,1,2)

* Create binary for "has younger sister"
gen has_young_sis = (youngsis > 0)

* Combine into a single variable if needed
gen emarried_has_young_sis = has_young_sis
label define emarried_ys 0 "Ever-married, no younger sister" 1 "Ever-married, has younger sister"
label values emarried_has_young_sis emarried_ys

* Check
tab emarried_has_young_sis






//
* First make a binary for "has at least one sister"
gen has_sister = (youngsis > 0)

* Restrict to ever-married women (currently or formerly married)
gen emarried_has_sister = 0
replace emarried_has_sister = 1 if inlist(v502,1,2) & has_sister==1

asdoc tab , replace save(/Users/bipasabanerjee/Library/CloudStorage/OneDrive-TheUniversityofTexasatAustin/PHD/Semester 3, Fall 2025/Research/Nepal/Results.doc)

asdoc tab v502, save(/Users/bipasabanerjee/Library/CloudStorage/OneDrive-TheUniversityofTexasatAustin/PHD/Semester 3, Fall 2025/Research/Nepal/Results.doc) replace
asdoc tab youngsis, append
asdoc tab emarried_has_young_sis, append
asdoc tab v743a, append
asdoc tab v743b, append
asdoc tab v743c, append
v743a 
v743b 
v743c 
v743d 
v743e 
v743f 
s924b 
v739



keep if v511 < .

* Lowess smoother: Age at marriage (v511) by younger sister presence
lpoly v511 youngsis, ci degree(1) kernel(epanechnikov) ///
    title("Local Polynomial Regression: Age at Marriage vs Younger Sister") ///
    xtitle("Has younger sister (0 = No, 1 = Yes)") ///
    ytitle("Age at first marriage (years)")
	

	twoway (kdensity v511 if emarried_has_sis==0, lcolor(blue)) ///
       (kdensity v511 if emarried_has_sis==1, lcolor(red)), ///
       legend(order(1 "No sister" 2 "Has sister")) ///
       title("Distribution of Age at First Marriage by Sister Status") ///
       xtitle("Age at first marriage") ytitle("Density")
	   
//Younger sibling//	   
gen young_sib = mmc1 - mmc2

*-------------------------------------------------
* Count younger brothers and sisters (DHS sibling data)
*-------------------------------------------------

* Generate counters
gen count_ybro = 0
gen count_ysis = 0

* Loop over up to 20 siblings
forvalues i = 1/20 {
    local j : display %02.0f `i'   // makes 01, 02, 03 ...

    * Younger brothers (sex=1)
    replace count_ybro = count_ybro + 1 if ///
        (mm3_`j' < v012 & mm3_`j' != . & mm1_`j' == 1)

    * Younger sisters (sex=2)
    replace count_ysis = count_ysis + 1 if ///
        (mm3_`j' < v012 & mm3_`j' != . & mm1_`j' == 2)
}

*-------------------------------------------------
* Create a categorical variable
* 0 = no younger siblings
* 1 = only younger brothers
* 2 = only younger sisters
* 3 = both brothers and sisters
*-------------------------------------------------
gen younger_sib_type = .
replace younger_sib_type = 0 if count_ybro==0 & count_ysis==0
replace younger_sib_type = 1 if count_ybro>0 & count_ysis==0
replace younger_sib_type = 2 if count_ybro==0 & count_ysis>0
replace younger_sib_type = 3 if count_ybro>0 & count_ysis>0

label define sibtype 0 "No younger siblings" ///
                    1 "Only younger brothers" ///
                    2 "Only younger sisters" ///
                    3 "Both brothers & sisters"
label values younger_sib_type sibtype


// kernel density plot with younger sibling//

//Kernel density of age at marriage (v511) by sibling type//

twoway (kdensity v511 if younger_sib_type==0, lcolor(blue) lpattern(solid)) ///
       (kdensity v511 if younger_sib_type==1, lcolor(red)  lpattern(solid)) ///
       (kdensity v511 if younger_sib_type==2, lcolor(green) lpattern(solid)) ///
       (kdensity v511 if younger_sib_type==3, lcolor(black) lpattern(solid)), ///
       legend(order(1 "No younger siblings" ///
                    2 "Only younger brothers" ///
                    3 "Only younger sisters" ///
                    4 "Both brothers & sisters")) ///
       xtitle("Age at First Marriage (v511)") ///
       ytitle("Density") ///
       title("Kernel Density of Age at Marriage by Younger Sibling Type")

asdoc tab younger_sib_type, append

// sibling dummy//

* First, store respondent's age
gen resp_age = v012

* Create variables for next youngest sibling's age and sex
gen next_y_age = .
gen next_y_sex = .

* Loop through siblings (DHS allows up to 20 siblings)
forvalues i = 1/20 {
    local j : display %02.0f `i'   // makes 01, 02, ..., 20

    quietly replace next_y_age = mm3_`j' if mm3_`j' < resp_age ///
        & (missing(next_y_age) | mm3_`j' > next_y_age)

    quietly replace next_y_sex = mm1_`j' if mm3_`j' == next_y_age
}

* Now create dummies
gen next_youngest_sis = (next_y_sex == 2) if next_y_sex < .
gen next_youngest_bro = (next_y_sex == 1) if next_y_sex < .



*************************** TRIAL**********************************
*--- Keep original data intact
preserve

* Reshape sibling data long
reshape long mm1_ mm3_ mm9_, i(caseid) j(sibnum)

* Keep only daughters
keep if mm1_ == 2   // 2 = female

* Create daughter order within each household
bysort caseid (sibnum): gen daughter_order = sum(mm1_ == 2)

* Indicator: 1 = still lives with mother, 0 = not
gen with_mother = (mm9_ == 1)

* Collapse: fraction living with mother by age and daughter order
collapse (mean) with_mother, by(mm3_ daughter_order)
rename mm3_ age

* Plot multiple lines automatically (one for each daughter order)
twoway ///
    (line with_mother age if daughter_order==1, lcolor(blue)) ///
    (line with_mother age if daughter_order==2, lcolor(red)) ///
    (line with_mother age if daughter_order==3, lcolor(green)) ///
    (line with_mother age if daughter_order==4, lcolor(orange)) ///
    , legend(order(1 "1st daughter" 2 "2nd daughter" 3 "3rd daughter" 4 "4th daughter")) ///
    ytitle("Fraction living with mother") xtitle("Age of daughter") ///
    title("When daughters leave the household (Nepal DHS 2016)")

*--- Restore main dataset unchanged
restore

// merge household and individual rosters//
********** to do with household***********

* Step 1. Load the PR (household roster) file
use "NPPR7HFL.DTA", clear

* Keep only daughters
keep if hv104 == 2   // hv104=sex (1=male, 2=female)

* Create indicator: does she live with mother?
gen with_mother = (hv112 != 0 & hv112 != .)

* Save temporary dataset
tempfile daughters
save `daughters'
///////////////////////////////
* Load household recode
use "NPHR7HFL.DTA", clear

* Create household ID
egen hhid = group(hv001 hv002)

* Reshape wide → long
reshape long hv104_ hv105_ hv112_ hv113_, i(hhid) j(line)

* Rename variables
rename hv104_ sex
rename hv105_ age
rename hv112_ mother_line
rename hv113_ father_line

* Keep females only
keep if sex == 2

* Keep daughters (have a reported mother in household)
keep if mother_line < .

*assign birth order 

bysort hhid (age): gen birth_order = _n

* coreising with mother

gen coreside = !missing(mother_line)

* restricting with exact two daughters 
bysort hhid: gen ndaughters = _N
keep if ndaughters == 2



reg coreside i.age##i.birth_order, cluster(hhid)
margins birth_order, at(age=(5(1)25))
marginsplot
******************************************************
use "NPHR7HFL.DTA", clear   // household recode
egen hhid = group(hv001 hv002)

* Reshape household members into long format
reshape long hv104_ hv105_ hv112_, i(hhid) j(line)
rename hv104_ sex
rename hv105_ age
rename hv112_ mother_line

********************************************* TRIAL*******************************************


*************************************
// fig 1//
*************


*********  using Birth recode for the birth order *************

//////// daughters leaving home not in birth order (using the birth order recode_) ////////////
/////////////////////////////////////////////////////////////
* 1) Open the child-level file (all births to interviewed women)
use "NPBR7HFL.DTA", clear   // Nepal 2016 BR file

* 2) Keep daughters who are alive (so probability is among living daughters)
keep if b4==2 & b5==1      // b4: sex (2=female); b5: alive (1)

* 3) Daughter's age at interview (years)
gen age_daughter = floor((v008 - b3)/12)   // v008: interview date (CMC); b3: DOB (CMC)
keep if inrange(age_daughter, 0, 30)       // sensible plotting range

* 4) Lives with mother indicator
gen with_mother = (b9==0)   // b9: 0=lives with respondent (mother), 1=lives elsewhere

* 5) Survey design (recommended for DHS)
gen wt = v005/1000000
svyset v021 [pweight=wt], strata(v022)

* 6) Smooth curve with confidence bands—no collapsing needed
svy: logit with_mother c.age_daughter##c.age_daughter
margins, at(age_daughter=(0(1)30))
marginsplot, ///
  ytitle("Pr(daughter lives with mother)") ///
  xtitle("Daughter age (years)") ///
  title("Nepal DHS 2016: Coresidence with mother by daughter's age") ///
  recast(line)


// when the family has two daughters// ***observation: 3642***

* Keep daughters (alive)
keep if b4==2 & b5==1

* Daughter's age at interview
gen age_daughter = floor((v008 - b3)/12)
keep if inrange(age_daughter, 0, 30)

* Lives with mother indicator
gen with_mother = (b9==0)

* Mother ID
egen mother_id = group(v001 v002 v003)   // cluster, hh, line number of mother

// restricting to 2 daughters//
bysort mother_id: gen daughters_per_mother = _N
keep if daughters_per_mother==2

//identifying the birth order//

bysort mother_id (b3): gen daughter_rank = _n
label define daughter_rank 1 "Elder daughter" 2 "Younger daughter"
label values daughter_rank daughter_rank

//plot the line//
	
	twoway ///
  (lowess with_mother age_daughter if daughter_rank==1, bwidth(0.8) lpattern(solid)) ///
  (lowess with_mother age_daughter if daughter_rank==2, bwidth(0.8) lpattern(dash)), ///
  legend(order(1 "Elder" 2 "Younger")) ///
  xtitle("Daughter age (years)") ytitle("Pr(lives with mother)") ///
  title("Smoothed coresidence by birth rank (2-daughter families)")

// family with 3 daughters// ** observation - 2205**

use "NPBR7HFL.DTA", clear   // Nepal DHS 2016 Birth Recode

* Keep daughters who are alive
keep if b4==2 & b5==1

* Daughter's age at interview
gen age_daughter = floor((v008 - b3)/12)
keep if inrange(age_daughter, 0, 30)

* Lives with mother
gen with_mother = (b9==0)

* Mother ID
egen mother_id = group(v001 v002 v003)

// restrict with 3 daughters//

bysort mother_id: gen daughters_per_mother = _N
keep if daughters_per_mother==3

// identify the rand of the child//
bysort mother_id (b3): gen daughter_rank = _n
label define daughter_rank 1 "Eldest" 2 "Middle" 3 "Youngest"
label values daughter_rank daughter_rank

// plot the line//

twoway ///
  (lowess with_mother age_daughter if daughter_rank==1, bwidth(0.8) lpattern(solid) lcolor(red)) ///
  (lowess with_mother age_daughter if daughter_rank==2, bwidth(0.8) lpattern(solid) lcolor(blue)) ///
  (lowess with_mother age_daughter if daughter_rank==3, bwidth(0.8) lpattern(solid) lcolor(green)), ///
  legend(order(1 "Eldest" 2 "Middle" 3 "Youngest")) ///
  xtitle("Daughter age (years)") ytitle("Pr(lives with mother)") ///
  title("Smoothed coresidence by birth rank (3-daughter families)")



// family with 4 daughters// observation 1268

* keep families with exactly 4 daughters
bysort mother_id: gen daughters_per_mother = _N
keep if daughters_per_mother==4

* assign birth rank within mother
bysort mother_id (b3): gen daughter_rank = _n
label define daughter_rank 1 "Eldest" 2 "2nd" 3 "3rd" 4 "Youngest"
label values daughter_rank daughter_rank

twoway ///
  (lowess with_mother age_daughter if daughter_rank==1, bwidth(0.8) lpattern(solid) lcolor(red)) ///
  (lowess with_mother age_daughter if daughter_rank==2, bwidth(0.8) lpattern(solid) lcolor(blue)) ///
  (lowess with_mother age_daughter if daughter_rank==3, bwidth(0.8) lpattern(solid) lcolor(green)) ///
  (lowess with_mother age_daughter if daughter_rank==4, bwidth(0.8) lpattern(solid) lcolor(orange)), ///
  legend(order(1 "Eldest" 2 "2nd" 3 "3rd" 4 "Youngest")) ///
  xtitle("Daughter age (years)") ytitle("Pr(lives with mother)") ///
  title("Smoothed coresidence by birth rank (4-daughter families)")

  
 
 
******* trying to do residual plot *********
* 1. Regress age at marriage on younger sister + controls
reg v511 next_youngest_sis v130 v106 s103
predict resid_age, resid

* 2. Regress decision-making power on the same controls
reg decision_1 v130 v106 s103
predict resid_decision, resid

* 3. Make a residual-on-residual plot
twoway (scatter resid_decision resid_age, mcolor(blue) msymbol(o)) ///
       (lfit resid_decision resid_age, lcolor(red)), ///
       ytitle("Residuals of Decision-making power") ///
       xtitle("Residuals of Age at marriage") ///
       title("Residual-on-Residual Plot")





	   
// making decision making power into three categories//
**** who decices on how to spend respondent earnings***** (v739)***
**** decision_1 is for how to spend respondent earnings***

gen decision_1 = .
replace decision_1 = 1 if v739 == 1                      // respondent alone
replace decision_1 = 2 if v739 == 2                      // jointly
replace decision_1 = 3 if inlist(v739, 4, 5)          // someone else

label define decision_1_lbl 1 "Respondent alone" ///
                           2 "Jointly" ///
                           3 "Someone else"
label values decision_1 decision_1_lbl 

// binary variables one is decides or not//
gen decision_yn = .
replace decision_yn = 0 if inlist(v739, 4, 5)                      // does NOT decide
replace decision_yn = 1 if  inlist(v739, 1, 2)                    // decide

label define decision_yn_lbl 0 "does not decide" ///
                           1 "decides" ///
                           
label values decision_yn decision_yn_lbl 




// binary variable 2 decides alone or not//
gen decision_alone = .
replace decision_alone = 0 if inlist(v739, 2, 4, 5)                      //  NOT alone
replace decision_alone = 1 if  v739 == 1                    // alone

label define decision_alone_lbl 0 "not alone" ///
                           1 "alone" ///
                           
label values decision_alone decision_alone_lbl 

// running the residual on residual plot again//

* 1. Regress age at marriage on younger sister + controls
reg v511 next_youngest_sis v130 v106 s103
predict resid_age, resid

* 2. Regress decision-making power on the same controls
reg decision_yn v130 v106 s103
predict resid_decision_yn, resid

* 3. Make a residual-on-residual plot
twoway (scatter resid_decision_yn resid_age, mcolor(blue) msymbol(o)) ///
       (lfit resid_decision resid_age, lcolor(red)), ///
       ytitle("Residuals of Decision-making power_yn") ///
       xtitle("Residuals of Age at marriage") ///
       title("Residual-on-Residual Plot")

	   
	   
// without the scatter//
twoway (scatter resid_decision_yn resid_age, mcolor(none)) ///	   
	   (lfit resid_decision_yn resid_age, lcolor(red)), ///
       ytitle("Residuals of Decision-making power_yn") ///
       xtitle("Residuals of Age at marriage") ///
       title("Residual-on-Residual Fit (no scatter)")

	   
// residual on residual on whether the decision is made alone or not//

* 1. Regress age at marriage on younger sister + controls
reg v511 next_youngest_sis v130 v106 s103
predict resid_age, resid

* 2. Regress decision-making power on the same controls
reg decision_alone v130 v106 s103
predict resid_decision_alone, resid

*3.  Make a residual-on-residual plot
twoway (scatter resid_decision_alone resid_age, mcolor(none)) ///	   
	   (lfit resid_decision_yn resid_age, lcolor(red)), ///
       ytitle("Residuals of Decision-making power_alone") ///
       xtitle("Residuals of Age at marriage") ///
       title("Residual-on-Residual Fit (no scatter)")
	   
	   
	   
// residual on residual plot on whether the decision is made by the person or not on education //
* 1. Regress years of education on younger sister + controls
reg v133 next_youngest_sis v130 s103
predict resid_edu, resid

* 2. Regress decision-making power on the same controls
reg decision_yn v130 s103
predict resid_decision_yn, resid

*3.  Make a residual-on-residual plot
twoway (scatter resid_decision_yn resid_edu, mcolor(none)) ///	   
	   (lfit resid_decision_yn resid_edu, lcolor(red)), ///
       ytitle("Residuals of Decision-making power_yn") ///
       xtitle("Residuals of years of education") ///
       title("Residual-on-Residual Fit (no scatter)")
	   
	  
