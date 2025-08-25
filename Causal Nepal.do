// Causal connection between early marriage and decision making power//

Step 1: Load IR file and save a copy of sibling roster
use "NPIR7HFL.DTA", clear

// creating younger sister//
keep caseid mm1_* mmidx_*   // keep only IDs + sibling roster
save "sibling_temp.dta", replace

* Step 2: Reshape only sibling roster
reshape long mm1_ mmidx_, i(caseid) j(siborder)

* Drop respondent herself (mmidx = 0)
drop if mmidx_ == 0

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
