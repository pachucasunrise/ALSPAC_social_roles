#06.12.22: pulling out vars from ALSPAC for ASTs project.

#Calling R
cd "C:/Program Files/R/R-4.3.1/bin"
R

#Calling packages
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(version = "3.12")

packages<-c('readstata13','foreign','tidyverse','haven','Hmisc','tableone','magrittr',
          'installr','devtools','plyr','dplyr','cachem','githubinstall','remotes')

for(pkg in packages){
  if(!require(pkg,character.only=TRUE)){
    #BiocInstaller::biocLite(pkg,suppressUpdates=TRUE)
    BiocManager::install(pkg)
    library(pkg,character.only=TRUE)
  }
}

rm(pkg,packages)

rm(list=ls())

#Working directory
loc_inp<-'//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p6/147/working/data/Obj1/'

#Variables from a previous project on ACEs - we look at these to determine which variables need to be treated as factors 
#(as when the ALSPAC R package runs it fails to retain this info and we reinstate factors manually, and use 'haven' - see later - to keep hold of variable labels)
load(paste0("C:/Users/is19262/OneDrive - University of Bristol/MyFiles-Migrated/Teaching/PhDs/Mini projects/Steph Page/varnames_Steph.R"))
#load(paste0("C:/Users/Annie/OneDrive - University of Bristol/MyFiles-Migrated/Teaching/PhDs/Mini projects/Steph Page/varnames_Steph.R"))
#This produces an object, 'vars'

#Need to change the case for some variables in 'vars'
lower <- c("fjpc4000","fjpc4100","fjpc2100","fjpc2000","fjle112","fjle114","fjpc100","fjpc150","fjpc200",                
            "fjpc050", "fjal4000", "ypb8000", "ypb8001", "ypb8050","ypb8051", "ypb8100", "ypb8120", "ypb8002", "ypb8003",
            "ypb8006", "ypb8007", "ypb8052", "ypb8053", "ypb8056", "ypb8057", "ypb8030", "ypb8040", "ypb8080", "ypb8090",                
            "ypc1812", "ypc1814", "ypc1810", "ypc1811", "ypc1813", "ccxd750", "ypa5005", "ypa5007",                
            "ypa5009", "ypa5011", "ypa5013", "ypa5015", "ypa5017", "ypa5050", "ypa5004", "ypa5006", "ypa5008", "ypa5010",                
            "ypa5012", "ypa5014", "ypa5016")
for(v in lower){
  vars[vars==v]<-toupper(vars[vars==v])
  }

#25.05.22: 'mult' is is now 'mult_no'
vars[vars=="mult"]<-"mult_no"

#Removing variables that are not needed/no longer exist in the ALSPAC directory
remove <- c("withdrawn_consent_mum", "withdrawn_consent_child",  
            "sc_household_12wgest", "sc_household_18wgest", "sc_household_32wgest", "sc_household_8m", 
            "sc_household_2yr", "sc_household_3yr", "sc_household_4yr", "sc_household_8yr",    
            "matsmok_tri1", "matsmok_tri2", "matsmok_tri3_c", "matsmok_tri3_e","fa3322","fa3327")
vars <- vars[!vars %in% remove]

#Variables that Steph included in her project - some of these are already in vars, but some are not
vars_Steph <- c("f228", "f237", "g308", "g317", "h218", "h227", "j308", "j317", "k4008", "k4017", "l4008", "l4017", "p2008", "p2017", "p3003", "r5008", "r5017", "s3003",   
                 "pd228", "pd237", "pe308", "pe317", "pf5008", "pg3008", "pg3017", "ph4017", "pm2008", "pm2017", "pm3003", "pp5008", "pp5015", "pp5017", "pq3003",  "ccs2050", "f242", "g322", 
                 "h232", "j322", "k4022", "l4022", "p2022", "n3044", "n3045", "n3048", "n3049", "n3050", "n3051", "n3052", "n3053", "n3054", "n3055", "n3056", "n3057", "n3058", "n3059",
                 "pd242", "pe322", "pf5022", "pg3022", "ph4022", "pj4022", "pm2022", "pl3044", "pl3045", "pl3048", "pl3049", "pl3050", "pl3051", "pl3052", "pl3053", "pl3054", "pl3055", 
                 "pl3056", "pl3057", "pl3058", "pl3059", "pm2022", "pm5022", "r5022", "pp5022", "pc226", "pd246", "pd247", "pe326", "pe327", "pf5026", "pf5027", "pg3026", "pg3027", "ph4026", 
                 "ph4027", "pj4026", "pj4027", "pm2026", "pm2027", "pp5026", "pp5027", "fa3325", "fa3326", "f246", "f247", "g326", "g327", "h236", "h237", "j326", "j327", "k4026", "k4027", 
                 "l4026", "l4027", "p2026", "p2027", "r5026", "r5027", "t3325", "t3326", "YPB8002", "YPB8003", "YPB8006", "YPB8007", "YPC1811", "YPB8052", "YPB8053", "YPB8056", "YPB8030", 
                 "YPB8040", "YPC1813", "YPB8080", "YPB8090", "kd505a", "kf455a", "kj465", "kl475", "kn4005", "kq365", "kt5005", "b608", "f257", "f258", "g337", "g338", "h247", "h248", "j337",
                 "j338", "k4037", "k4038", "l4037", "l4038", "p2037", "p2038", "r5037", "r5038", "t3336", "t3337", "pd257", "pd258", "pf5036", "pf5037", "pc236", "pe337", "pe338", "pg3036", 
                 "pg3037", "ph4037", "ph4038", "pj4037", "pj4038", "pm2037", "pm2038", "pp5037", "pp5038", "fa3333", "fa3334", "YPB8000", "YPB8001", "YPB8050", "YPB8051", "YPC1812", "e428", 
                 "f249a", "g329", "g329a", "h329", "j329", "k4029", "l4029", "p2029", "r5029", "s5014", "pc228a", "pd249a", "pe329a", "pf5029", "pg3029", "ph4029", "pj4029", "pm2029", "pp5029", 
                 "pq5014", "e236", "e391", "f036", "f248", "f256", "f200", "g049", "g328", "g612", "h039", "h238", "h497", "g290", "j328", "h200a", "j044", "j615", "k1020", "k1044", "k4028", 
                 "l3020", "l3044", "l4028", "l6031", "p2028", "p1020", "p1054", "p3031", "n1042", "n1059", "n1060", "q4100", "q4110", "r5028", "s1020", "s3031", "r2002", "r2019", "r2020", "s4100", 
                 "s4110", "pd063", "pd248", "pc102", "pd200", "pe020", "pe064", "pe290", "pe328", "pf5028", "pg3028", "pg1034", "ph1020", "ph1044", "ph4028", "pj3020", "pj3044", "pj4028", "pj6031", 
                 "pm2028", "pm1020", "pm1054", "pm3031", "pl1042", "pl1059", "pl1060", "pm2028", "pn4100", "pn4110", "pp5028", "pq1020", "pq3031", "pp2002", "pp2019", "pp2020", "pq4100", "pq4110", 
                 "ccs6501", "ccs6502", "e192", "e213", "f061", "f067", "f069", "g047", "g053", "g056", "h037", "h043", "h046", "j042", "j048", "j051", "k1042", "k1050", "k1053", "l3042", "l3050", 
                 "l3053", "n1057", "r2017", "p1052", "p1060", "p1063", "pc266", "pd061", "pd066", "pd067", "pc276", "pe062", "pe067", "pf1032", "pf1037", "pf1039", "pg1037", "pg1039", "ph1042", 
                 "ph1050", "ph1053", "pj3053", "pj3042", "pj3050", "pj6032", "pm1052", "pm1060", "pm1063", "pl1057", "pp2017", "f527", "g613", "h498", "j616", "k1022", "l3022", "l6032", "p1022", 
                 "p3032", "n1058", "s1022", "s3032", "ph1022", "pm1022", "pm3032", "pq1022", "pq3032","mult_no")
length(vars)
#797
table(vars_Steph %in% vars)
#5 not already in vars

#Including any additional variables on top of ACEs (i.e. most of them)
#Loading Lotte's original ALSPAC ACEs dataset, where certain auxilliary variables are recoded into binary

#29.05.24: Additional variables which help us determine if still active in the cohort at age 30
active30 <- c("ypj0002","ypk0002","ypl0002","covid6yp_0001")

#Read in variables needed to derive ASTs
adv_description<-data.frame(readxl::read_excel(paste0(loc_inp,'AST_variables_20240208.xlsx'),sheet = 'ALSPAC',col_names=TRUE))

#The latter vars (after "var") relate to calculating dates
#29.05.24: updated with additional, e.g. conditional vars
vars_ASTs <-c(adv_description[,"var"],
              adv_description$ageq_mvar,
              adv_description$ageq_yvar,
              adv_description[,"doq_m"],adv_description[,"doq_y"],
              adv_description[,"doe_m"],adv_description[,"doe_y"],
              adv_description[,"start_mvals"],adv_description[,"start_yvals"],
              adv_description[,"end_mvar"],adv_description[,"end_yvar"],
              adv_description[,"other_vars"],
              adv_description$condition_var1,
              adv_description$condition_var2
            )

#All variables needed
vars <- c(vars,vars_Steph,active30,vars_ASTs,"mz024a","mz024b")
length(vars)
#n=6162

#Remove any duplicates
vars<-vars[!is.na(vars)]
length(vars)
#2789
vars <- unique(vars)
length(vars)
#n=1268

#Have to name what are NOT factors
#First those that are no factors in Steph data (as when ALSPAC R package is used it drops factor info)
#A: 
#Loading Lotte's original dataset (variables are binarised correctly to allow multiple imputation later)
load("//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p6/046/working/data/ACE data/useful_data_ACE/01Recode_the_original_variables_to_binary/alspac.table_ACE.RData")
factor_list <- list(names(alspac.table_ACE),sapply(names(alspac.table_ACE), function(x) is.factor(alspac.table_ACE[,x])))
length(factor_list[[1]])
#n=813
#List is [1] names [2] whether a factor
nfactors <- factor_list[[1]][which(factor_list[[2]]==FALSE)]
length(nfactors)
#n=84 of the 813 variables are not factors
#8 of nfactors not in the final results_lname dataset anyway, as we had removed

#Install ALSPAC package separately (https://github.com/explodecomputer/alspac)
#The first few lines are additional packages that helps the alspac package get installed...
updateR()
install_github("explodecomputer/alspac", force=TRUE)
library(alspac)

#Tell R where the drive is
setDataDir("R:/Data")
setDataDir("//ads.bris.ac.uk/Filestore/SSCM ALSPAC/Data")
#setDataDir("//ads.bris.ac.uk/folders/Health Sciences/Data")

#Use the 'Current' data in that ALSPAC drive
data(current)
#Updating dictionaries/file paths - this bit takes a while
#current <- createDictionary("Current", name="current")
#useful <- createDictionary("Useful_data", name="useful")
#So in the end the object 'current' is a ~80,000 x 18 data frame, 
#the rows being all the variables that are in the 'Current' folder 
#of ALSPAC, and the columns indicating metadata (the file used, data type, etc.).  
#That 18th column 'name' distinguishes the variable name.

#Take a subset of that data frame 'current' to only include the rows of the variables you need
#Have added some extra lines to extract factors and non-factors separately, and from useful as well as current
dim(current)
#Did !vars %in% nfactors as some variables are in neither factors nor nfactors. These will be defined as factors by default
vars_factors <- unique(vars[!vars %in% nfactors])
length(vars_factors)
#n=1192
vars_nfactors <- unique(vars[vars %in% nfactors])
length(vars_nfactors)
#n=76

x <- subset(current, name %in% vars_factors)
z <- subset(current, name %in% vars_nfactors)

#Will be > length of vars as there will be duplicates (extractVars gets rid of these)
#Now extract those particular variables for the ~15k ALSPAC participants from the individual files (e.g. child 18y clinic, etc.)
#06.06.22: Matt has now added in the option 'haven' which allows us to keep the variable labels
results1 <- extractVars(x,haven=T)
results2 <- extractVars(z,haven=F)

#Check data look as you think they should
#Number of columns should be the number of variables you'd specified 
#and about another 80 more (these vars are things like 'in_ccs' 'in_cct', 
#which are dummy variables indicating which questionnaires etc the data 
#come from).  If you want to subset the data even further to just the variables you specified 
#(though would always keep aln and qlet), can put something like:
#results <- results[,c("aln","qlet",vars)]
head(results1)
dim(results1)
#no. vars=1275
length(names(results1)[!names(results1) %in% vars_factors])
#116 vars not in the factor list but have been pulled out in results1 (along with the x that was specified above)
length(vars_factors[!vars_factors  %in% names(results1)])
#33 factor variables we've asked for but could not be pulled (e.g. no longer called that/exist)

#Remove any variables that are not aln, qlet, or factor variables that we asked for
results1 <- results1[,c("aln","qlet",names(results1)[grepl('kz021',names(results1))],vars_factors[vars_factors %in% names(results1)])]
dim(results1)
#n=1164 (so 1275-1164=111 removed, likely all consent vars e.g. in_ccs etc.)

head(results2)
dim(results2)
#no. vars=290
length(names(results2)[!names(results2) %in% vars_factors])
#282 vars not in the non-factor list but have been pulled out in results2 (along with the z that was specified above)
length(vars_nfactors[!vars_nfactors  %in% names(results2)])
#8 non-factor variables we've asked for but could not be pulled

results2 <- results2[,c(vars_nfactors[vars_nfactors %in% names(results2)])]
dim(results2)
#n=68 (so 290-68=222 variables removed)

length(vars[!vars %in% c(names(results1),names(results2))])
#41 variables that were asked for originally that are not pulled out at all (=33 factor and 8 non-factor variables described above)

#Put results1 and results2 together
results <- cbind(results1,results2)
dim(results)
#no. vars=1232
length(unique(names(results)))
#1230

#kz021 pops up multiple times so each given a weird name...
head(results[,names(results)[grepl('kz021',names(results))]])
head(sapply(results[,names(results)[grepl('kz021',names(results))]], function(x) unique(x)))
head(sapply(results[,names(results)[grepl('kz021',names(results))]], function(x) table(x, exclude=NULL)))

#Want to just keep the first one with values 1 or 2
results <- results[,names(results)[!names(results) %in% c("kz021...278","kz021...1242")]]
names(results)[names(results) %in% "kz021...208"]<-"kz021"
#Should be 1230
dim(results)

#Make sure to take any capitalised variables and put to lower case when using Lotte's scripts
results_lname <- results
names(results_lname) <- tolower(names(results_lname))

#Make sure R recognises these variables as factors, pulling those labels out
#Just using ypb8002 as an example
attributes(results_lname$ypb8002)$labels 
is.factor(results_lname$ypb8002)

factors <- names(results_lname)[!names(results_lname) %in% nfactors]

for (x in factors) {
#for (x in names(results_lname)){
  results_lname[,x] <- as_factor(results_lname[,x])
  }

# #ypb8002 should be a factor:
is.factor(results_lname$ypb8002)

# #kd058 shouldn't be a factor
is.factor(results_lname$kd058)

#For those that are not factors, minus values should be set to missing
table(results_lname$kd058, exclude=NULL)

#Now save as an R and Stata files
loc_out <- "//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p6/147/working/data/Obj1/"
save(results,results_lname,excluded,file=paste0(loc_out,"cohort.RData"))
write.dta(results_lname,file=paste0(loc_out,"cohort.dta"))