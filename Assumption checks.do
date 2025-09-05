
global nepal_2006_ir "/Users/sidhpandit/Desktop/DHS/data/nepal/nepal 2006 ir/NPIR51FL.dta"

global nepal_2016_ir "/Users/sidhpandit/Desktop/nepal 2016 individual /NPIR7HFL.DTA"

use caseid-mm15_20 mmc* using "${nepal_2006_ir}", clear


/*


We want to know what is the probability that a girl has left home before her older sister in the sample?
and compare that to the probability a girl has left home just given her age

For this, we first restrict the sample to families with two daughters age 15-24



Use the individual dataset - 

Is a woman in her natal home?
Does she have just one sister?
Is her sister married?

*/



* number of daughters to restrict the analysis to
local x = 2


/*

b4: sex of child
b5: child is alive
b8: age of child
b9: child lives with whom
b19: current age of child in months

*/

* prepare the varnames for reshaping
forvalues i=1/9 {
	
	foreach b in b4 b5 b8 b9 {
		
		rename `b'_0`i' `b'_`i'
	}
}

* child level dataset now
reshape long b4_ b5_ b8_ b9_ , i(caseid) j(line_number)

keep caseid b4_ b5_ b8_ b9_ v005

rename caseid motherid
rename b4_ sex 
rename b5_ alive
rename b8_ age
rename b9_ residence

* focus on daughters age 15-24
keep if inrange(age,14,24)
keep if sex==2
drop if missing(residence)

bysort motherid: egen num_daughters = count(alive)

* focus on 2 daughter families
keep if num_daughters==`x'

* predicted probabilities for coresidence by age
gen left_home = residence==4

reg left_home i.age [pw=v005]
predict phat, xb

bysort motherid (age): gen ord = _n

gen youngest = (ord==1)
gen oldest   = (ord==`x')

gen phat_old = phat if oldest
gen phat_young = phat if youngest
gen left_old = left_home if oldest
gen left_young = left_home if youngest

preserve
collapse (max) phat_old phat_young left_old left_young [pw=v005], by(motherid)


gen predicted_oop = phat_young * (1 - phat_old)
gen actual_oop    = (left_young==1 & left_old==0)

* Get weighted means
summarize predicted_oop actual_oop 



restore
