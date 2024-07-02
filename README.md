# ALSPAC_social_roles
Scripts for deriving annual social role indicators in the ALSPAC cohort

1_ALSPAC_extract_vars.R uses variables from a previous project (https://wellcomeopenresearch.org/articles/3-106: needed a lot later for auxilliary variables in multiple imputation), plus all core + additional variables listed in a spreadsheet 'AST_variables.xls'. It then uses the alspac() package to extract these variables from the ALSPAC Direct User files and saves the final dataset, 'results_lname' in 'cohort.RData'.

2_deriving_sequences.R 'calls four functions (ast_current_seq, ast_since_seq, ast_date_seq, and ast_age_seq) to derive annual indicators for the five social roles within the data frame 'results_lname', which is eventually saved as 'cohort_for_imp.RData'.
