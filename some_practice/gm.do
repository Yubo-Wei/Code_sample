set more off
set varabbrev off
clear


global dir = "/Users/lee/Desktop/gm"
global output_dir="${dir}/stataoutput"

import delimited "${dir}/test_data.csv"

gen survivors = 0
replace survivors = 1 if victims == 1 | victims == 2 | victims == 3 | victims == 4

label variable hhold_num "index for household number"
label variable avg_income "monthly per person household income"
label variable year_update "year record filed"
label variable month_update "month record filed"
label variable hhead_literate "literacy of household head"
label variable hhead_age "age of household head"
label variable hhead_edu "years of education for household head"
label variable hhead_migrant "whether the household head is a migrant"
label variable hhead_foreign "whether the household head is a foreigner"
label variable kids "whether there are any children in the household"
label variable kids_under13 "children under the age of 13 in the housheold"
label variable nadults "number of adults in household"
label variable nmales "number of male adults in household"
label variable homeless_hhold "any members of the household recently homeless"
label variable ag_hhold2 "whether the household is engaged in agricultural work"
label variable victims "whether any household member survived human trafficking"
label variable yr_trafficked "year in which the survivor was rescued"
label variable sex_victim "sex of the survivor"
label variable age_victim "age of the survivor"
label variable edu_victim "years of education for survivor"
label variable homeless_victim "whether survivor has been homeless in the past"
label variable child_labor "whether any children in the household are engaged in child labor"
label variable survivors "whether the households have trafficking survivors"


estpost sum avg_income hhead_literate hhead_age hhead_edu hhead_migrant hhead_foreign kids kids_under13 nadults nmales ag_hhold2 sex_victim age_victim edu_victim homeless_victim if survivors == 1
est sto m1

estpost sum avg_income hhead_literate hhead_age hhead_edu hhead_migrant hhead_foreign kids kids_under13 nadults nmales ag_hhold2 if survivors == 0
est sto m2

estpost sum avg_income hhead_literate hhead_age hhead_edu hhead_migrant hhead_foreign kids kids_under13 nadults nmales ag_hhold2 if survivors == 1 | survivors == 0
est sto m3

esttab m1 m2 m3 using "${dir}/table/table1.tex", title("Descriptive Statistics") mtitles("with survivors" "without survivors" "Combined") label cell(mean(fmt(%9.2f)) sd(par fmt(%9.2f))) replace nonum

