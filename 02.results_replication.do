******** 31579LSE

******** Airbnb Guest Discrimination analysis.

set more off
clear all

* ------------------------------------------------------------------------------
* 00.00 load main data set
* ------------------------------------------------------------------------------
cd "L:\99.LSE studies\01.PB4A7\99.Summative\Original Data and Do file -20211220\113691-V1"
use "main_data 4.dta"

* ------------------------------------------------------------------------------
* 01. Descriptive statistics
*-------------------------------------------------------------------------------
cd "L:\99.LSE studies\01.PB4A7\99.Summative\Original Data and Do file -20211220\113691-V1\01.descriptives"

* Local with main variables for descriptive analysis ---------------------------

local  v0101 "host_race_white host_race_black host_gender_F host_gender_M price bedrooms bathrooms number_of_reviews  multiple_listings any_black tract_listings black_proportion"

* General descriptives for hosts -----------------------------------------------

sum `v0101',d
tabstat `v0101', s(n mean median sd p25 p75) save

tabstatmat temp 
matrix temp = temp'  
mat li temp, format(%9.2f)

putexcel set "01a.desc.xlsx", sheet ("01a.desc") modify
putexcel A1= "Trait"
putexcel B1= "Observations"
putexcel C1= "Mean"
putexcel D1= "Median"
putexcel E1= "St.Dev"
putexcel F1= "25th Perc"
putexcel G1= "75th Perc"
putexcel A2= matrix(temp), rownames

tab simplified_host_response yes

* Missing values indentification -----------------------------------------------
misstable patterns yes guest_black host_race_black host_gender_M mul_list shared_property ten_reviews log_price
* Mean differences for host traits given guests race ---------------------------

file open host_table using "01b.ttest_tab.csv", w replace
file write host_table "Trait (obs), white mean, black mean, s1, s2, n1, n2, p value" _n

foreach var in `v0101'{
qui: ttest `var', by(guest_black)
		local white_mean = round(r(mu_1),0.01)
		local black_mean = round(r(mu_2),0.01)
		local s1 = round(r(sd_1),0.01)
		local s2 = round(r(sd_2),0.01)
		local n1 = round(r(N_1),0.01)
		local n2 = round(r(N_2),0.01)
		local p = round(r(p),0.01)

	file write host_table "`var' (`n'), `white_mean', `black_mean', `s1',`s2',`n1',`n2',`p'" _n
}

file close host_table

* Single frequencies for host and listing traits -------------------------------

* frequencies
tab host_race                 // host race
tab host_gender				  // host gender

* bar plots
* response - guest race
graph bar (sum) guest_black  guest_white, over(graph_bins) percentages graphregion(color(none)) plotregion(color(none)) bgcolor(none)

graph use "L:\99.LSE studies\01.PB4A7\99.Summative\Original Data and Do file -20211220\113691-V1\01.descriptives\01.bar graph.gph" 

* response - guest race - city
graph bar (sum) guest_black  guest_white if city != ".", over(city)  over(yes)  

tab city yes
 graph hbox log_price if city != "." , over(city) over(yes)

* --------------------------------------------------------
* 02. Results
*--------------------------------------------------------

* 02.01.race effects by sex -------------------------

** (1) race
reg yes i.guest_black, vce(cluster city)
estimates store reg020101

** (2) race gender
reg yes i.guest_black i.host_race_black i.host_gender_M, vce(cluster city)
estimates store reg020102

** (2.1) race gender
reg yes i.guest_black i.host_gender_M if host_race_black == 1, vce(cluster city)
estimates store reg02010201


** (3) race gender listing
reg yes i.guest_black i.host_race_black i.host_gender_M multiple_listings shared_property ten_reviews log_price, vce(cluster city)
estimates store reg020103


** (3.1) race gender listing
reg yes i.guest_black i.host_race_black i.host_gender_M i.multiple_listings i.ten_reviews number_of_reviews log_price , vce(cluster city)
estimates store reg020103

margins   i.guest_black, at(number_of_reviews=(1(10)60))
marginsplot, recast(line) recastci(rarea)

* regression stimates
esttab  reg020102 reg02010201   using "02a1.desc.rtf", b(%5.3f) se(%5.3f) r2 ar2 replace

* 02.02. race effects by sex -------------------------

** (1) general
reg yes i.guest_black i.host_race_black host_race_black##guest_black, vce(cluster city)
di e(r2_a)
margins   i.guest_black#i.host_race_black
marginsplot

estimates store reg020201

* (1.1)
reg yes i.guest_black i.host_race_black host_race_black##guest_black c.log_price, vce(cluster city)
di e(r2_a)

margins   i.host_race_black, at(log_price=(1(1)11))
marginsplot

* (1.2)
reg yes i.guest_black i.host_race_black host_race_black##guest_black c.number_of_reviews, vce(cluster city)
di e(r2_a)

margins  host_race_black#guest_black
marginsplot

** (2) male host
reg yes i.guest_black i.host_race_black host_race_black##guest_black if host_gender_M == 1, vce(cluster city)
di e(r2_a)
estimates store reg020202

** (3) female host
reg yes i.guest_black i.host_race_black i.host_race_black##i.guest_black if host_gender_F == 1, vce(cluster city)
di e(r2_a)
estimates store reg020203

** (4) other host
reg yes i.guest_black i.host_race_black i.host_race_black##i.guest_black if host_gender_F != 1  & host_gender_M != 1, vce(cluster city)
di e(r2_a)
estimates store reg020204

* regression stimates
esttab  reg020201 reg020202 reg020203 reg020204 using "02b.desc.rtf", b(%5.3f) se(%5.3f) r2 ar2 replace

* 02.03. Homophily cross tab share by guest/host race gender -------------------

** (1) general table

gen H_MW = host_gender_M*host_race_white
gen H_FW = host_gender_F*host_race_white

gen H_MA = host_gender_M*host_race_black
gen H_FA = host_gender_F*host_race_black

gen G_MW = guest_male*guest_white
gen G_FW = guest_female*guest_white

gen G_MA = guest_male*guest_black
gen G_FA = guest_female*guest_black

local hom_groupsH H_MW H_FW H_MA H_FA
local hom_groupsG G_MW G_FW G_MA G_FA

foreach aH in `hom_groupsH' {
	foreach aG in `hom_groupsG' {
	display `aH'
	capture noisily tabulate yes `aG' if `aH' == 1, col nofreq
	}
}

* 02.04. Listing characteristics -----------------------------------------------

** (1) race and shared property
reg yes i.guest_black i.shared_property i.guest_black##i.shared_property, vce(cluster name_by_city)
estimates store reg020401

** (2) race and number of listings
reg yes i.guest_black i.multiple_listings  i.guest_black##i.multiple_listings, vce(cluster name_by_city)
estimates store reg020402

** (3) race and prior reviews
reg yes i.guest_black i.ten_reviews i.guest_black##i.ten_reviews, vce(cluster name_by_city)
estimates store reg020403

** (4) race and host age
reg yes i.guest_black i.young i.guest_black##i.young, vce(cluster name_by_city)
estimates store reg020404

** (5) race and prior AA reviews
reg yes i.guest_black i.any_black i.guest_black##i.any_black, vce(cluster name_by_city)
estimates store reg020405

* regression stimates
esttab  reg020401 reg020402 reg020403 reg020404 reg020405 using "02d.desc.rtf", b(%5.3f) se(%5.3f) r2 ar2 replace

* 02.05. location characteristics ----------------------------------------------

** (1) race and price above the median
reg yes i.guest_black i.price_median i.guest_black##i.price_median, vce(cluster name_by_city)
estimates store reg020501

** (2) race and AA pop in census track
reg yes i.guest_black c.prop_black  i.guest_black##c.prop_black, vce(cluster name_by_city)
estimates store reg020502

** (3) race and Airbnb concentration in census track
reg yes i.guest_black c.tract_listings i.guest_black##c.tract_listings, vce(cluster name_by_city)
estimates store reg020503

** (4) race and prop of filling listing
reg yes i.guest_black c.pr_filled i.guest_black##c.pr_filled, vce(cluster name_by_city)
estimates store reg020504

* regression stimates
esttab  reg020501 reg020502 reg020503 reg020504 using "02e.desc.rtf", b(%5.3f) se(%5.3f) r2 ar2 replace

* 02.06. Effective response by name and host -----------------------------------

local hom_groupsG G_MW G_FW G_MA G_FA

foreach aG in `hom_groupsG' {
	tab2xl yes guest_first_name if `aG' == 1  using 02fdesc , percentage  row(1) col(1) sheet(`aG')
	}

	
* ----------------------------------------------------------------------------------------------------------------
* 03. Robustness checks
*-----------------------------------------------------------------------------------------------------------------
* 03.01. Recoding of response ------------------------------------------------------------------------------------
	 
gen no = 1 if simplified_host_response == 10 | simplified_host_response == 11
replace no = 0 if no==.
 
** (1) race interactions
reg no i.guest_black i.host_race_black host_race_black##guest_black, vce(cluster city)
di e(r2_a)
estimates store reg030101

** (2) race and shared property
reg no i.guest_black i.shared_property i.guest_black##i.shared_property, vce(cluster name_by_city)
estimates store reg030102

** (3) race and number of listings
reg no i.guest_black i.multiple_listings  i.guest_black##i.multiple_listings, vce(cluster name_by_city)
estimates store reg030103


** (4) race and AA pop in census track
reg no i.guest_black c.prop_black  i.guest_black##c.prop_black, vce(cluster name_by_city)
estimates store reg030104

* regression stimates
esttab  reg030101 reg030102 reg030103 reg030104 using "03e.desc.rtf", b(%5.3f) se(%5.3f) r2 ar2 replace

	
* ----------------------------------------------------------------------------------------------------------------
* 04. External validity 
*-----------------------------------------------------------------------------------------------------------------

* 04.01. Host gender ---------------------------------------------------------------------------------------------

** (1) All cities
egen price_bins = cut(log_price), at(1,2,3,4,5,6,7,8,9,10,11,12)

tabulate price_bins

reg yes i.guest_black i.host_race_black host_race_black##guest_black c.log_price black_proportion,  vce(cluster name_by_city)
di e(r2_a)

estimates store reg030101

margins   i.guest_black, at(log_price=(1(1)11))
marginsplot

** (2) Washington

reg yes i.guest_black i.host_race_black host_race_black##guest_black c.log_price  black_proportion if  city == "Washington", vce(cluster name_by_city)
di e(r2_a)

estimates store reg030102

** (3) Los-Angeles

reg yes i.guest_black i.host_race_black host_race_black##guest_black c.log_price black_proportion if  city == "Los-Angeles", vce(cluster name_by_city)
di e(r2_a)

estimates store reg030103

** (3) Los-Angeles and washington as indicators
gen city_main = 0
replace city_main = 1 if city == "Washington"	
replace city_main = 1 if city == "Los-Angeles"

reg yes i.guest_black i.host_race_black host_race_black##guest_black c.log_price black_proportion city_main,  vce(cluster city)
di e(r2_a)

estimates store reg030104

* regression stimates
esttab  reg030101 reg030102 reg030103 reg030104 using "03e2.desc.rtf", b(%5.3f) se(%5.3f) r2 ar2 replace

* 04.01. City distributions ---------------------------------------------------------------------------------------------

* log price on city density
gen city2 = 0
replace city2 = 1 if city == "Baltimore"
replace city2 = 2 if city == "Dallas"	
replace city2 = 3 if city == "Los-Angeles"	
replace city2 = 4 if city == "St-Louis"	
replace city2 = 5 if city == "Washington"	
	
forvalues i=1/5 {
capture drop x`i' d`i'
 kdensity log_price if city2 == `i', generate(x`i'  d`i') 
 }

gen zero = 0

twoway rarea  d1 zero x1, color("blue%50") ///
    ||  rarea d2 zero x2, color("purple%50") ///
    ||  rarea d3 zero x3, color("orange%50")  ///
    ||  rarea d4 zero x4, color("red%50") ///
	||  rarea d5 zero x5, color("green%50") ///
        title(January Temperatures by Region) ///
        ytitle("Smoothed density") ///
        legend(ring(0) pos(2) col(1) order(1 "Bal" 2 "Dal" 3 "LA" 4 "S-L" 5 "Was"))     

reg log_price black_proportion city_main tract_listings ten_reviews , vce(cluster name_by_city)

estimates store reg040101

reg log_price black_proportion city_main tract_listings ten_reviews  if host_race_black == 1, vce(cluster name_by_city)

estimates store reg040102

reg log_price black_proportion city_main tract_listings ten_reviews  if host_race_black == 0, vce(cluster name_by_city)

estimates store reg040103

* regression stimates
esttab  reg040101 reg040102 reg040103  using "03e5.desc.rtf", b(%5.3f) se(%5.3f) r2 ar2 replace

preserve
drop if log_price ==.
label define guest_black1 0 "white" 1 "black"
label values guest_black guest_black1
tab log_price
reg  yes i.guest_black host_race_black#i.guest_black c.log_price , vce(cluster name_by_city)
marhis log_price, cate(guest_black)
graph box log_price, over(city, sort(1))

restore

* probit
probit  yes i.guest_black i.host_race_black host_race_black##guest_black, vce(cluster city)
di e(r2_a)
margins guest_black, atmeans

margins, dydx(*)
outreg2 using marg_resultsprobit, word

marginsplot


* ----------------------------------------------------------------------------------------------------------------
* 05. Internal validity
*-----------------------------------------------------------------------------------------------------------------

* new response categories
gen yes_2 = 0
replace yes_2 = 1 if (simplified_host_response >= 1) & (simplified_host_response <= 6)
replace yes_2 = . if (simplified_host_response == 7) 


gen yes_3 = 0
replace yes_3 = 1 if simplified_host_response == 1
replace yes_3 = . if (simplified_host_response == 7) 

** (5.1) general yes
reg yes i.guest_black i.host_race_black , vce(cluster city)
estimates store reg050101

** (5.1) broad yes 
reg yes_2 i.guest_black i.host_race_black , vce(cluster city)
estimates store reg050102

** (5.1) extrict yes 
reg yes_3 i.guest_black i.host_race_black , vce(cluster city)
estimates store reg050103

* regression stimates
esttab  reg050101 reg050102 reg050103  using "05e2.desc.rtf", b(%5.3f) se(%5.3f) r2 ar2 replace


