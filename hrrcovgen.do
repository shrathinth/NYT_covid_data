******************************************************************************************************************************************************************************************************
                           ************************************************* File to convert raw county-HRR crosswalk to STATA files **************************************************
******************************************************************************************************************************************************************************************************
clear
set more off

adopath ++ "C:\Users\shrathiv\Documents\STATA\ado"

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

save "T:\Venky\Data\Covid\NYT_covid_data\hrrcov.dta", replace