# ALSPAC_social_roles
Scripts for deriving annual social role indicators in the ALSPAC cohort

1_ALSPAC_extract_vars.R uses variables from a previous project (https://wellcomeopenresearch.org/articles/3-106: needed a lot later for auxilliary variables in multiple imputation), plus all core + additional variables listed in a spreadsheet 'AST_variables.xls'. It then uses the alspac() package to extract these variables from the ALSPAC Direct User files and saves the final dataset, 'results_lname' in 'cohort.RData'.

2_deriving_sequences.R calls four functions (ast_current_seq, ast_since_seq, ast_date_seq, and ast_age_seq) to derive annual indicators for the six social roles within the data frame 'results_lname', which is eventually saved as 'cohort_for_imp.RData'.

Note that there are two scripts for imputation, one in R and one in Stata. In the final Data Note, we used the data imputed from mict() in Stata and composite indicators imputed with standard mice() in R:

3_imputation.R takes AST.dta within 'cohort_for_imp.RData'. and carries out 1) the MICT-timing algorithm using seqimpute() R package on the individual annual indicators of social roles (although we didn't include these imputed data in the final Data Note, we leave the code in the script as may be beneficial to future users); 2) standard MICE on composite indicators of whether an individual has entered (and exited) a role by age 30. The data frames from 1) and 2) are saved as 'sequences_.RData' and 'composite_vars.RData', respectively.

3_imputation.do takes that AST.dta data frame, which has been saved as cohort_for_Stata.dta, and carries out the MICT algorithm using the mict() Stata package on the individual annual indicators of social roles. 

Please contact annie.herbert@bristol.ac.uk if you have any questions about this project or these files.
