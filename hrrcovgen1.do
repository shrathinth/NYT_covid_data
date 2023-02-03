******************************************************************************************************************************************************************************************************
                           ************************************************* File to convert raw county-HRR crosswalk to STATA files **************************************************
******************************************************************************************************************************************************************************************************
clear
set more off

adopath ++ "C:\Users\shrathiv\ado"

net set ado "C:\Users\shrathiv\ado"

import delimited "T:\Venky\Data\Covid\NYT_covid_data\CasesandDeathsbyHRR.csv", clear

drop v1

gen year = substr(date, 1, 4)
gen month = substr(date, 6, 2)
gen day = substr(date, 9, 2)

rename date date_str

*****Changing dates from string to long
local dates "year month day"
foreach x of local dates{
destring `x', replace
}

gen date = mdy(month, day, year) 
format date %d 

keep hrr19 date hrrpop hrrcaserate hrrdeathrate hrrcaserate100k hrrdeathrate100k hrrcases hrrdeaths casegrowthrate deathgrowthrate hrrcases_rollingaverage hrrdeaths_rollingaverage hrrcaserate_rollingaverage hrrdeathrate_rollingaverage casegrowrthrate_rollingaverage deathgrowthrate_rollingaverage newcases newdeaths

local var "casegrowthrate deathgrowthrate hrrcases_rollingaverage hrrdeaths_rollingaverage hrrcaserate_rollingaverage hrrdeathrate_rollingaverage casegrowrthrate_rollingaverage deathgrowthrate_rollingaverage newcases newdeaths"
foreach x of local var{
	replace `x' = "" if `x' == "NA"
destring `x', replace
}

gen c = newcases
gen d = newdeaths

tsset hrr19 date

bysort hrr19 (date): gen c_1 = c[_n-1]
bysort hrr19 (date): gen c_2 = c[_n-2]
bysort hrr19 (date): gen c_3 = c[_n-3]
bysort hrr19 (date): gen c_4 = c[_n-4]
bysort hrr19 (date): gen c_5 = c[_n-5]
bysort hrr19 (date): gen c_6 = c[_n-6]
bysort hrr19 (date): gen c_7 = c[_n-7]

bysort hrr19 (date): gen d_1 = d[_n-1]
bysort hrr19 (date): gen d_2 = d[_n-2]
bysort hrr19 (date): gen d_3 = d[_n-3]
bysort hrr19 (date): gen d_4 = d[_n-4]
bysort hrr19 (date): gen d_5 = d[_n-5]
bysort hrr19 (date): gen d_6 = d[_n-6]
bysort hrr19 (date): gen d_7 = d[_n-7]

egen d7 = rowmean(d d_1 d_2 d_3 d_4 d_5 d_6)
egen c7 = rowmean(c c_1 c_2 c_3 c_4 c_5 c_6)

gen d7_m = d7 if date < mdy(9, 30, 2020)
gen c7_m = c7 if date < mdy(9, 30, 2020)

egen d7_std = std(d7_m), by(hrr19)
egen c7_std = std(c7_m), by(hrr19)

drop c_* d_* d7_m c7_m

local var "c d c7 d7 c7_std d7_std"
foreach x of local var{
rename `x' `x'_hrr
}

save "T:\Venky\Data\Covid\NYT_covid_data\hrrcov.dta", replace