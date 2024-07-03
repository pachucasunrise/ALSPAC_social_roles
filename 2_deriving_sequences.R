#Having binary indicators that show when someone is in a role or not

#File locations, etc.
cd "C:/Program Files/R/R-4.3.1/bin"
R

rm(list=ls())

#Calling packages (you might need to install these first, e.g. install.packages(readstata13))
#The following code I stole from someone, it's a quicker way of getting all the packages installed and calling them in one go!
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(version = "3.18")

packages<-c('readxl','readstata13','foreign','tidyverse','haven','Hmisc','tableone','magrittr','seqimpute','TraMineR')

for(pkg in packages){
  if(!require(pkg,character.only=TRUE)){
    #BiocInstaller::biocLite(pkg,suppressUpdates=TRUE)
    BiocManager::install(pkg)
    library(pkg,character.only=TRUE)
  }
}
rm(pkg,packages)

#Functions to call for different algorithms: since, current, and date (also need one for age), and description
functions<-c('ast_since_seq','ast_current_seq','ast_date_seq','ast_age_seq','ast_describe','ast_yetfunc')
for(func in functions){
  #source(paste0("C:/Users/is19262/OneDrive - University of Bristol/MyFiles-Migrated/Training/R/ast/",func,".R"))
  source(paste0("X:/scripts/functions/",func,".R"))
}

loc_inp<-'//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p6/147/working/data/Obj1/'
loc_out<-loc_inp

adv_description<-data.frame(readxl::read_excel(
paste0(loc_inp,'AST_variables.xlsx'),
sheet = 'ALSPAC',col_names=TRUE))

#How many vars
#Before exclusions, core vars
length(unique(c(adv_description$var)))
#324
#After exclusions, core vars
desc<-adv_description[adv_description$exclude=="no",]
length(unique(desc[desc$exclude=="no","var"]))
#243

#Including date vars etc
length(unique(c(adv_description$var,
                adv_description$ageq_mvar,
                adv_description$ageq_yvar,
                adv_description$doq_m,
                adv_description$doq_y,
                adv_description$doe_m,
                adv_description$doe_y,
                adv_description$start_mvar,
                adv_description$start_yvar,
                adv_description$end_mvar,
                adv_description$end_yvar,
                adv_description$other_vars,
                adv_description$condition_var1,
                adv_description$condition_var2)))
#460
#460-324=64 are auxilliary vars (in the AST sense)
#After exclusions
length(unique(c(desc$var,
                desc$ageq_mvar,
                desc$ageq_yvar,
                desc$doq_m,
                desc$doq_y,
                desc$doe_m,
                desc$doe_y,
                desc$start_mvar,
                desc$start_yvar,
                desc$end_mvar,
                desc$end_yvar,
                desc$other_vars,
                desc$condition_var1,
                desc$condition_var2)))
#371
#371-243=128 aux vars
#Makes sense would be more, as fewer date vars get removed vs core as useful elsewhere

#Now load the data
load(paste0(loc_inp,"cohort.RData"))
AST.dta <- results_lname
dim(AST.dta)
#15645 1237
#According to May 2024 syntax template should be 15645 records.

#Create variable 'active' indicating responding to at least one questionnaire (of possible 4) at age 30
AST.dta$active30 <- 0
AST.dta[(AST.dta$ypj0002=="Yes" | 
				 AST.dta$ypk0002=="Yes" | 
				 AST.dta$ypl0002=="Yes" |
				 AST.dta$covid6yp_0001=="Yes"),
				 "active30"] <- 1

#Birthdate
AST.dta[AST.dta$mz024a<0 & !is.na(AST.dta$mz024a),"mz024a"]<-NA
AST.dta[AST.dta$mz024b<0 & !is.na(AST.dta$mz024b),"mz024b"]<-NA
AST.dta$birthdate <- as.Date(paste0(AST.dta$mz024b,"-",AST.dta$mz024a,"-15"),origin="1960-01-01")

#Responses that should be considered missing values
missing <- c("Consent withdrawn", 
				   ".",
				   "Triplet/Quadruplet","Triplet or quadruplet","triplet or quadruplet",
				   "Male",
				   "Questionnaire not Returned","Questionnaire not completed","Questionnaire not Completed","Not completed",
				   "Missed whole section A","Missed whole section B","Missed whole section C","Missed whole section D",
				   "Missed whole section E","Missed whole section F","Missed whole section G","Missed whole section H",
				   "Missed Whole Section I","Missed whole section I",
				   "Not selected any A6 option",
				   "unresolved","Missing","No response","No Answer","Unresolvable",
				   "Something else",
				   "NS/NA", "NS/NK",
				   "Does not currently have job","Never looked for job","YP didn't study at University",
				   "or later")

#Set minimum and maximum years for indicators, and transition vector
agemin <- 16
agemax <- 31
yetage <- 30
#exit_yetage <- 30
trans_vector <- c("educ","employ","leave_parents","cohab","parent")

#The '_yet's and exit_yets go to yetage
#Need to create these indicator and composite indicator variables first
valueset <- function(trans_vector,data,yetage,agemin,agemax){
	for (t in trans_vector){
		for (i in agemin:agemax){
			data[,paste0(t,"_",i)] <- NA
			}
		data[,c(#paste0(t,"_yet"),paste0(t,"_yet_age"),
		#paste0(t,"_exit_yet"),paste0(t,"_exit_yet_age"),
		paste0(t,"_duration"),
		paste0(t,"_yet_",yetage),paste0(t,"_yet_",yetage,"_age"),	
		paste0(t,"_exit_yet_",yetage),paste0(t,"_exit_yet_",yetage,"_age"),
		"startdate","enddate","age_start","age_end")] <- NA
		}
	return(data)	
	}
AST.dta <- valueset(trans_vector,AST.dta,yetage,agemin,agemax)

#Now use algorithms to fill in annual indicators
trans_vector1 <- c("educ","employ","leave_parents","parent")
trans_vector2 <- c("cohab") #See Data Note: 15% households include non-dependent children, 42% dependent, 43% none
#algorithms <- c("date","current","since","age")
for (t in trans_vector1){
	#Only educ (=1) at ages 16 to 28 gets updated when using restricted definition and date sequence algorithm
	AST.dta <- ast_date_seq(t,adv_description,AST.dta,agemin,agemax,missing,"restricted")
	#This one takes the longest, but still only 2 mins
	#Updates indicators(=1 and =0) for all five transitions, in the case of leave_parents and cohab, only from age 19-20
	AST.dta <- ast_current_seq(t,adv_description,AST.dta,agemin,agemax,missing,"restricted")
	#Only does anything on educ, leave_parents and parent (no since vars for employ or cohab)
	#Investigated warnings: lots of NAs introduced by coercion. This is because in some 
	#cases in adv_description, the age_start value is blank and becomes NA. Which is fine and totally expected
	AST.dta <- ast_since_seq(t,adv_description,AST.dta,agemin,agemax,missing,"restricted")
	##age_seq function only tweaks the employment vars
	#Only change here is some 0 values drop, as have become 1s
	#Medians are all as before as age_seq 
	#Investigated warnings, as with current()
	AST.dta <- ast_age_seq(t,adv_description,AST.dta,agemin,agemax,missing,"restricted")
	#Check
	}

#Only want to update broad definitions for some. Doesn't seem to work for employment, but does for cohab and being a parent
#Only one var exists broadly for leaving_parents, none for educ
#At the moment, only current and since algorithms apply for these transitions, but running all just in case
for (t in trans_vector2){
	AST.dta <- ast_date_seq(t,adv_description,AST.dta,agemin,agemax,missing,c("restricted","broad"))
	AST.dta <- ast_current_seq(t,adv_description,AST.dta,agemin,agemax,missing,c("restricted","broad"))
	AST.dta <- ast_since_seq(t,adv_description,AST.dta,agemin,agemax,missing,c("restricted","broad"))
	AST.dta <- ast_age_seq(t,adv_description,AST.dta,agemin,agemax,missing,c("restricted","broad"))
	}

#From here can output for stata
write.dta(AST.dta,file=paste0(loc_out,"cohort_for_Stata.dta"))
#write_dta(AST.dta,file.path(paste0(loc_out,"cohort_for_Stata.dta")))

#yets
#07.05.24
#criteria = "threshold" or "active", where threshold is % of non-missing values, 
#active they responded to at least one of the ~age 30 surveys
for (t in trans_vector){
	AST.dta <- yetfunc(t,AST.dta,agemin,yetage,"nothing",NA)
	print(table(AST.dta[,paste0(t,"_yet_",yetage)],AST.dta[,paste0(t,"_exit_yet_",yetage)],exclude=NULL))
	AST.dta <- yetfunc(t,AST.dta,agemin,yetage,"threshold",0.5)
	print(table(AST.dta[,paste0(t,"_yet_",yetage)],AST.dta[,paste0(t,"_exit_yet_",yetage)],exclude=NULL))
	AST.dta <- yetfunc(t,AST.dta,agemin,yetage,"active",NA)
	print(table(AST.dta[,paste0(t,"_yet_",yetage)],AST.dta[,paste0(t,"_exit_yet_",yetage)],exclude=NULL))
	}

#Table 2
indicfunc(trans_vector,AST.dta,agemin,agemax,1)
indicfunc(trans_vector,AST.dta,agemin,agemax,0)

descfunc(trans_vector,AST.dta[!is.na(AST.dta$kz021) & AST.dta$kz021=="Male",],exit_yetage,exit_yetage)
descfunc(trans_vector,AST.dta[!is.na(AST.dta$kz021) & AST.dta$kz021=="Female",],exit_yetage,exit_yetage)

#Save data for imputation
save(AST.dta,file=paste0(loc_out,"cohort_for_imp.RData"))
#ls=ls()
save(ls,file=paste0(loc_out,"quicksave.RData"))

for (i in 23:30){
  print(prop.table(table(AST.dta[,paste0("cohab_",i)]#,exclude=NULL
                         )))
  }

#Seqimpute
load(paste0(loc_out,"cohort_for_imp.RData"))

#Indicate those where not a single transition has been imputed?
AST.dta$nonmiss <- NA
for (t in trans_vector){
	for (i in 16:31){
		AST.dta[!is.na(AST.dta[,paste0(t,"_",i)]),"nonmiss"]<-1
		}
	}
table(AST.dta$nonmiss,exclude=NA)
#Half had something put in
nonmiss.dta<-AST.dta[!is.na(AST.dta$nonmiss),]
dim(nonmiss.dta)

#15.05.24
#Educ
for (t in trans_vector){
	astseq.miss<-seqdef(nonmiss.dta,grep(paste0(t,"_"),colnames(nonmiss.dta))[1:16],right=NA)
	pdf(paste0("//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p6/147/working/results/",t,"_plot.pdf")) 
	seqdplot(astseq.miss,border=NA,with.missing=TRUE)
	dev.off()
	}

#Want vector capturing positions of 'educ_16'... 'educ_31' in AST.dta
for (t in trans_vector[3:5]){
	#for (s in c("Male","Female")){
		#Temporary data makes everything easier after
		data_temp<-AST.dta

		#Pull out indices for columns needed to impute for specific transition
		vector<-NA
		for (i in agemin:agemax){
			vector<-c(vector,grep(paste0(t,"_",i),colnames(data_temp)))
			}
		vector<-vector[2:length(vector)]
		assign(paste0(t,"_vector"),vector)

		for (i in vector){
			data_temp[,i]<-as.factor(data_temp[,i])
			}

		print(Sys.time())

		data_temp <- data_temp[data_temp$kz021==s,vector]
		dim(data_temp)

		#Weirdly making things work...
		#data_temp[,"educ_16"]<-NA

		#Reset rownames which may be what causing issue...
		#rownames(data_temp) <- 1:dim(data_temp)[1]
		#Seqimpute() for specific transition and sex

		#Sorting error:
		# Error in data.frame(.imp = rep(idx, each = nrow(data$data)), .id = rep.int(1L:nrow(data$data),  :
  		# arguments imply differing number of rows: 38530, 38540
		assign(paste0(t,"_seqimp_",s),seqimpute(data_temp,var=1:length(names(data_temp))#,
			#covariates=c("a525","b032","c600","c755","c765","c800","mz028b","b663","kz030")
				))

		# assign(paste0(t,"_seqimp"),seqimpute(data_temp,var=vector#,
		# 	#covariates=c("a525","b032","c600","c755","c765","c800","mz028b","b663","kz030")
		# 		))

		#Some very weird thing happens with the index numbers but seems to be okay if i ensure the last row isn't all NAs...
		#After adding rows, etc., very sure something is happening DURING seqimpute that adds in two extra rows.

		#print(summary.seqimp(get(paste0(t,"_seqimp_",s))))
		assign(paste0(t,"_seqimp_long_",s),fromseqimp(get(paste0(t,"_seqimp_",s)), format="long",include=FALSE))
		#assign(paste0(t,"_seqimp_long"),fromseqimp(get(paste0(t,"_seqimp")), format="long"))

		print(Sys.time())
		#Each one takes ~ X
		ls=ls()
		save(ls,file=paste0(loc_out,"quicksave.RData"))
		#}
	}
#"2024-04-29 18:06:09 BST"
#Educ
#"2024-04-29 18:32:17 BST"
#Each ~5 imputations takes ~26 minutes
#Employ
#"2024-04-29 19:01:13 BST"
# The following `from` values were not present in `x`: 0, 1
# The following `from` values were not present in `x`: 0, 1
# The following `from` values were not present in `x`: 0, 1
# The following `from` values were not present in `x`: 0, 1
# The following `from` values were not present in `x`: 1
# The following `from` values were not present in `x`: 0
# The following `from` values were not present in `x`: 0
# The following `from` values were not present in `x`: 0
#Leave parents
#"2024-04-29 19:30:19 BST"
#Similar message to above re: from values
#Cohab
# "2024-04-29 19:58:49 BST"
#Parent
#"2024-04-29 20:24:49 BST"

out <- data.frame(matrix(NA, nrow = length(trans_vector), ncol = 3#15
))
colnames(out) <- c("Transition",#"n_yet_25","perc_yet_25","mean_25","median_25","lqr_25","uqr_25",
																"n_yet_30","perc_yet_30","mean_30",
																"median_30","lqr_30","uqr_30",
																#"n_exit_yet",
																"perc_exit_yet","mean_dur",
																"median_dur","lqr_dur","uqr_dur"
																)
out[,1] <- trans_vector

for (t in trans_vector){
	# for (i in agemin:agemax){
	# 	assign(paste0(t,"_seqimp_long[,'",t,"'_'",i"]"),as.numeric(as.character(get(paste0(paste0(t,"_seqimp_long[,",t,"_",i"]")))))
	# 	}
	}
# educ_seqimp_long <- yetfunc(c("educ"),educ_seqimp_long,18,yetage,exit_yetage,0.5)
# employ_seqimp_long <- yetfunc(c("employ"),employ_seqimp_long,17,yetage,exit_yetage,0.5)
# leave_parents_seqimp_long <- yetfunc(c("leave_parents"),leave_parents_seqimp_long,16,yetage,exit_yetage,0.5)
# cohab_seqimp_long <- yetfunc(c("cohab"),cohab_seqimp_long,18,yetage,exit_yetage,0.5)
# parent_seqimp_long <- yetfunc(c("parent"),parent_seqimp_long,16,yetage,exit_yetage,0.5)

list<-list(trans=trans_vector,min=c(18,17,16,18,16),threshold=c(0.5,0.25,0.1,0.25,0.5))

for (t in trans_vector){
	data_temp <- get(paste0(t,"_seqimp_long"))
	# t_vector <- paste0(t,'_',list$min[list$trans==t])
	# for (i in (list$min[list$trans==t]+1):yetage){
	# 	t_vector <-	c(t_vector,paste0(t,'_',i))
	# 	}
	#data_temp[,"rowsums"] <- rowSums(!is.na(data_temp[,t_vector]))
	#data_temp <- data_temp[data_temp$rowsums!=0,]
#	data_temp <- data_temp[data_temp$kz021=="Male",]
	for (i in agemin:agemax){
		data_temp[,paste0(t,"_",i)]<-as.numeric(as.character(data_temp[,paste0(t,"_",i)]))
		}
	data_temp[,c(
		paste0(t,"_duration"),
		paste0(t,"_yet_",exit_yetage),paste0(t,"_yet_",exit_yetage,"_age"),	
		paste0(t,"_exit_yet_",exit_yetage),paste0(t,"_exit_yet_",exit_yetage,"_age")
		)] <- NA
	data_temp <- yetfunc(t,data_temp,list$min[list$trans==t],yetage,exit_yetage,list$threshold[list$trans==t])

	#Duration
	data_temp[data_temp[,paste0(t,"_exit_yet_",exit_yetage)]==1 & data_temp[,paste0(t,"_yet_",exit_yetage)]==1 &
	!is.na(data_temp[,paste0(t,"_exit_yet_",exit_yetage)]) & !is.na(data_temp[,paste0(t,"_yet_",exit_yetage)]),paste0(t,"_duration")] <- 
			data_temp[data_temp[,paste0(t,"_exit_yet_",exit_yetage)]==1 & data_temp[,paste0(t,"_yet_",exit_yetage)]==1 &
				!is.na(data_temp[,paste0(t,"_exit_yet_",exit_yetage)]) & !is.na(data_temp[,paste0(t,"_yet_",exit_yetage)]),paste0(t,"_exit_yet_",exit_yetage,"_age")]-
			data_temp[data_temp[,paste0(t,"_exit_yet_",exit_yetage)]==1 & data_temp[,paste0(t,"_yet_",exit_yetage)]==1 &
				!is.na(data_temp[,paste0(t,"_exit_yet_",exit_yetage)]) & !is.na(data_temp[,paste0(t,"_yet_",exit_yetage)]),paste0(t,"_yet_",exit_yetage,"_age")]

	n_yet_30 <- 0
	perc_yet_30 <- 0
	mean_30 <- 0
	median_30 <- 0
	lqr_30 <- 0
	uqr_30 <- 0

	n_exit_yet <- 0
	perc_exit_yet <- 0
	mean_dur <- 0
	median_dur <- 0
	lqr_dur <- 0
	uqr_dur <- 0


	for (i in 1:5){
		n_yet_30 <- n_yet_30 + table(data_temp[data_temp[,".imp"]==i &
													  data_temp[,paste0(t,"_yet_30")]==1
																			,paste0(t,"_yet_30")])
		perc_yet_30 <- perc_yet_30 + table(data_temp[data_temp[,".imp"]==i &
															data_temp[,paste0(t,"_yet_30")]==1
															,paste0(t,"_yet_30")])/
									 length(data_temp[data_temp[,".imp"]==i
															,paste0(t,"_yet_30")])
		mean_30 <- mean_30 + trunc(mean(data_temp[data_temp[,".imp"]==i &
													  data_temp[,paste0(t,"_yet_30")]==1
															,paste0(t,"_yet_30_age")],na.rm=TRUE))
		median_30 <- median_30 + trunc(median(data_temp[data_temp[,".imp"]==i &
													  data_temp[,paste0(t,"_yet_30")]==1
															,paste0(t,"_yet_30_age")],na.rm=TRUE))
		lqr_30 <- lqr_30 + trunc(quantile(data_temp[data_temp[,".imp"]==i &
													  data_temp[,paste0(t,"_yet_30")]==1
															,paste0(t,"_yet_30_age")],0.25,na.rm=TRUE))
		uqr_30 <- uqr_30 + trunc(quantile(data_temp[data_temp[,".imp"]==i &
													  data_temp[,paste0(t,"_yet_30")]==1
															,paste0(t,"_yet_30_age")],0.75,na.rm=TRUE))
		
		n_exit_yet <- n_exit_yet + table(data_temp[data_temp[,".imp"]==i &
													  data_temp[,paste0(t,"_exit_yet_30")]==1
																			,paste0(t,"_exit_yet_30")])
		perc_exit_yet <- perc_exit_yet + table(data_temp[data_temp[,".imp"]==i &
																	data_temp[,paste0(t,"_exit_yet_30")]==1
																	,paste0(t,"_exit_yet_30")])/
											 length(data_temp[data_temp[,".imp"]==i
																	,paste0(t,"_exit_yet_30")])
		mean_dur <- mean_dur + trunc(mean(data_temp[data_temp[,".imp"]==i &
													  data_temp[,paste0(t,"_exit_yet_30")]==1
															,paste0(t,"_duration")],na.rm=TRUE))
		median_dur <- median_dur + trunc(median(data_temp[data_temp[,".imp"]==i &
													  data_temp[,paste0(t,"_exit_yet_30")]==1
															,paste0(t,"_duration")],na.rm=TRUE))
		lqr_dur <- lqr_dur + trunc(quantile(data_temp[data_temp[,".imp"]==i &
													  data_temp[,paste0(t,"_exit_yet_30")]==1
															,paste0(t,"_duration")],0.25,na.rm=TRUE))
		uqr_dur <- uqr_dur + trunc(quantile(data_temp[data_temp[,".imp"]==i &
													  data_temp[,paste0(t,"_exit_yet_30")]==1
															,paste0(t,"_duration")],0.75,na.rm=TRUE))
		}
	out[out$Transition==t,"n_yet_30"] <- round(n_yet_30/5,digits=0)
	out[out$Transition==t,"perc_yet_30"] <- round(perc_yet_30/5,digits=2)
	out[out$Transition==t,"mean_30"] <- trunc(mean_30/5)
	out[out$Transition==t,"median_30"] <- trunc(median_30/5)
	out[out$Transition==t,"lqr_30"] <- trunc(lqr_30/5)
	out[out$Transition==t,"uqr_30"] <- trunc(uqr_30/5)

	out[out$Transition==t,"n_exit_yet"] <- round(n_exit_yet/50,digits=0)
	out[out$Transition==t,"perc_exit_yet"] <- round(perc_exit_yet/50,digits=2)
	out[out$Transition==t,"mean_dur"] <- trunc(mean_dur/5)
	out[out$Transition==t,"median_dur"] <- trunc(median_dur/5)
	out[out$Transition==t,"lqr_dur"] <- trunc(lqr_dur/5)
	out[out$Transition==t,"uqr_dur"] <- trunc(uqr_dur/5)	
	}
#table2_men <- out



out <- data.frame(matrix(NA, nrow = length(trans_vector), ncol = 3#15
))
colnames(out) <- c("Transition",#"n_yet_25","perc_yet_25","mean_25","median_25","lqr_25","uqr_25",
																"perc_yet_2526",
																"earliest_50_2016"
																)
out[,1] <- trans_vector

for (t in trans_vector){
	# for (i in agemin:agemax){
	# 	assign(paste0(t,"_seqimp_long[,'",t,"'_'",i"]"),as.numeric(as.character(get(paste0(paste0(t,"_seqimp_long[,",t,"_",i"]")))))
	# 	}
	}
# educ_seqimp_long <- yetfunc(c("educ"),educ_seqimp_long,18,yetage,exit_yetage,0.5)
# employ_seqimp_long <- yetfunc(c("employ"),employ_seqimp_long,17,yetage,exit_yetage,0.5)
# leave_parents_seqimp_long <- yetfunc(c("leave_parents"),leave_parents_seqimp_long,16,yetage,exit_yetage,0.5)
# cohab_seqimp_long <- yetfunc(c("cohab"),cohab_seqimp_long,18,yetage,exit_yetage,0.5)
# parent_seqimp_long <- yetfunc(c("parent"),parent_seqimp_long,16,yetage,exit_yetage,0.5)

list<-list(trans=trans_vector,min=c(18,17,16,18,16),threshold=c(0.5,0.25,0.1,0.25,0.5))

for (t in trans_vector){
	data_temp <- get(paste0(t,"_seqimp_long"))
	#data_temp <- data_temp[,c(".imp",".id",paste0(t,"_25"),paste0(t,"_26"))]
	#data_temp[,"rowsums"] <- rowSums(!is.na(data_temp[,t_vector]))
	#data_temp <- data_temp[data_temp$rowsums!=0,]
#	data_temp <- data_temp[data_temp$kz021=="Male",]
	for (i in agemin:agemax){
		data_temp[,paste0(t,"_",i)]<-as.numeric(as.character(data_temp[,paste0(t,"_",i)]))
		}
	data_temp[,c(
		paste0(t,"_2526"),
		paste0(t,"_earliest")
		)] <- NA
	data_temp[,paste0(t,"_2526")] <- rowSums(data_temp[,c(paste0(t,"_25"),paste0(t,"_26"))],na.rm=TRUE)
	data_temp[!is.na(data_temp[,paste0(t,"_2526")]) & data_temp[,paste0(t,"_2526")]==2,paste0(t,"_2526")] <- 1

	perc_2526 <- 0
	perc_25 <- 0
	perc_26 <- 0
	earliest_50_2016 <- 0

	for (i in 1:5){

		perc_25 <- perc_25 + table(data_temp[data_temp[,".imp"]==i &
													data_temp[,paste0(t,"_25")]==1
													,paste0(t,"_25")])/
							 length(data_temp[data_temp[,".imp"]==i
													,paste0(t,"_25")])

		perc_26 <- perc_26 + table(data_temp[data_temp[,".imp"]==i &
															data_temp[,paste0(t,"_26")]==1
															,paste0(t,"_26")])/
									 length(data_temp[data_temp[,".imp"]==i
															,paste0(t,"_26")])

		#Earliest age at which >50% in role
		# data_temp2 <-data_temp[data_temp[,".imp"]==i,]
		# earliest <- NA
		# for (j in list$min[list$trans==t]:agemax){
		# 	if(table(data_temp2[data_temp2[,paste0(t,"_",j)]==1,paste0(t,"_",j)])/length(data_temp2[,paste0(t,"_",j)])>0.5){													
		# 		earliest <- j
		# 		}
		# 	}
		}

	#out[out$Transition==t,"n_yet_30"] <- round(n_yet_30/5,digits=0)
	out[out$Transition==t,"perc_yet_2526"] <- round(perc_26/5,digits=2)
	# out[out$Transition==t,"mean_30"] <- trunc(mean_30/5)
	# out[out$Transition==t,"median_30"] <- trunc(median_30/5)
	# out[out$Transition==t,"lqr_30"] <- trunc(lqr_30/5)
	# out[out$Transition==t,"uqr_30"] <- trunc(uqr_30/5)

	# out[out$Transition==t,"n_exit_yet"] <- round(n_exit_yet/50,digits=0)
	# out[out$Transition==t,"perc_exit_yet"] <- round(perc_exit_yet/50,digits=2)
	# out[out$Transition==t,"mean_dur"] <- trunc(mean_dur/5)
	# out[out$Transition==t,"median_dur"] <- trunc(median_dur/5)
	# out[out$Transition==t,"lqr_dur"] <- trunc(lqr_dur/5)
	# out[out$Transition==t,"uqr_dur"] <- trunc(uqr_dur/5)	
	}
