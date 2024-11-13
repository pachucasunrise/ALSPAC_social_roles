*Working with Stata to impute sequences of categorical data using Brendan Halpin's method
*07.05.24

ssc install mict

*Prep a smaller dataset, but with id to link back
use "X:\data\Obj1\cohort_for_Stata.dta", clear
keep aln qlet kz021 ///
a525 b032 c600 c755 c765 c800 mz028b b663 kz030 ///
educ_* higher_ed_* employ_* fulltime_* leave_parents_* cohab_* parent_* carer_* ///
active30
drop *_yet* *_duration

save "X:\data\Obj1\subcohort_for_Stata.dta", replace
keep if kz021==4
save "X:\data\Obj1\subcohort_for_Stata_males.dta", replace

use "X:\data\Obj1\subcohort_for_Stata.dta", clear
keep if kz021==5
save "X:\data\Obj1\subcohort_for_Stata_females.dta", replace

*Loop over males and females (except higher education where in our case models don't converge), save all and append.
foreach s in males females{
	foreach t in educ employ fulltime leave_parents cohab parent carer{
		use "X:\data\Obj1\subcohort_for_Stata_`s'.dta", clear
		keep aln qlet id kz021 ///
		a525 b032 c600 c755 c765 c800 mz028b b663 kz030 ///
		`t'_* ///
		active30
		cap drop *_yet* *_duration
		
		*Drop empty indicators at beginning (i.e. indicators where we don't observe at least one 0 and one 1 - minimum requirement for mict)
		foreach v in educ_16 educ_17 educ_18 educ_19  leave_parents_16 leave_parents_17 leave_parents_18 leave_parents_19 leave_parents_20 cohab_16 cohab_17 cohab_18 cohab_19 cohab_20 parent_16 parent_17 parent_18 parent_19 carer_16 carer_17 carer_18 carer_19 carer_20 carer_21{
	cap rename `v' ig_`v'
		}
	
		mict_prep `t'_, id(id)
		mict_impute, maxgap(3) maxitgap(1) nimp(3)
		
		foreach v in educ_16 educ_17 educ_18 educ_19 higher_ed_16 higher_ed_17 higher_ed_18 higher_ed_19 leave_parents_16 leave_parents_17 leave_parents_18 leave_parents_19 leave_parents_20 cohab_16 cohab_17 cohab_18 cohab_19 cohab_20 parent_16 parent_17 parent_18 parent_19 carer_16 carer_17 carer_18 carer_19 carer_20 carer_21{
	cap rename ig_`v' `v'
		}
		save "X:\data\Obj1\\`s'_`t'_imp.dta", replace
		}
	}
