#Script 3: Carrying out imputation on indicators of ages at which 
#transitions occur (and '_yet' variables by mice and passive imputation)

#File locations, etc.
cd "C:/Program Files/R/R-4.3.1/bin"
R

#rm(list=ls())

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(version = "3.18")

packages<-c('TraMineR','seqimpute')

for(pkg in packages){
  if(!require(pkg,character.only=TRUE)){
    #BiocInstaller::biocLite(pkg,suppressUpdates=TRUE)
    BiocManager::install(pkg)
    library(pkg,character.only=TRUE)
  }
}
rm(pkg,packages)


loc_inp<-'//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p6/147/working/data/Obj1/'
loc_out<-loc_inp
load(paste0(loc_inp,"cohort_for_imp_R.RData"))
#Drop 'Not known'/missing sex
AST.dta <- AST.dta[AST.dta$kz021!="Not known" & !is.na(AST.dta$kz021),]
dim(AST.dta)

sex<-c("Female","Male")
agemin <- 16
agemax <- 31
trans_vector <- c("educ","employ","leave_parents","cohab","parent")

#Want to feed out ns of datasets for each combination of sex and transition
colnames <- c("Transition","sex","n")
out <- data.frame(matrix(NA, nrow = length(sex)*length(trans_vector), ncol = length(colnames)))
colnames(out) <- colnames
out[,1] <- rep(trans_vector,2)
out[,2] <- c(rep("Female",length(trans_vector)),rep("Male",length(trans_vector)))

aux <- c("a525","b032","c600","c755","c765","c800","mz028b",
			"b663","kz030b")

#Description and visualization of missing data
for (s in sex){
		for (t in trans_vector){
		data <- AST.dta
		#Trying dropping rows where transition t totally missing
		data <- data[data[,paste0(t,"_perc_nonmiss_30")]!=0, ]
		dim(data)
		#Reorder AST.dta and rename rownames in the 'second' dataset
		data <- data[order(data$kz021),]
		data <- data[data$kz021==s,]
		rownames(data) <- as.character(c(1:dim(data)[1]))
		out[out$Transition==t & out$sex==s,3] <- dim(data)[1]

		#First, pull out indices for columns needed to impute for specific transition
		#And pull out only the variables where at least some 0 and 1 values
		vector<-NA
		for (i in agemin:agemax){
			vector<-c(vector,grep(paste0(t,"_",i),colnames(data)))
			}
		vector<-vector[2:length(vector)]
		assign(paste0(t,"_vector"),vector)
		for (i in vector){
			data[,i]<-as.factor(data[,i])
			}
		#data <- data[,vector]

		colskeep <- NA
		for (j in 1:length(vector)) {
			if (dim(table(data[,vector[j]]))[1]==2) {
				colskeep <- c(colskeep,vector[j])
				}
			}
		colskeep <- colskeep[2:length(colskeep)]

		assign(paste0(t,"_seq_",s),seqdef(data,min(colskeep):max(colskeep),right=NA))
		pdf(paste0("//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p6/147/working/results/Obj1/",t,"_seqdefplot_",s,".pdf")) 
		seqdplot(get(paste0(t,"_seq_",s)),border=NA,with.missing=TRUE)
		dev.off()

		pdf(paste0("//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p6/147/working/results/Obj1/",t,"_seqmissfplot_",s,".pdf")) 
		seqmissfplot(get(paste0(t,"_seq_",s)),border=NA)
		dev.off()		

		pdf(paste0("//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p6/147/working/results/Obj1/",t,"_seqmissIplot_",s,".pdf")) 
		seqmissIplot(get(paste0(t,"_seq_",s)),border=NA,sortv="from.end")
		dev.off()		

	# 	imp <- seqmissimplic(get(paste0(t,"_seq_",s)))
	# 	pdf(paste0("//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p6/147/working/results/Obj1/",t,"_seqmissimplic_",s,".pdf")) 
	# 	plot(imp#, legend.prop=0.22, cex.axis=0.65, cex.legend=0.8, cex.lab=0.8
	# )
	# 	dev.off()
		#Error in `[<-`(`*tmp*`, i, j, value = indice) : subscript out of bounds

		set.seed(121231)
		Sys.time()
		assign(paste0(t,"_seqimp_",s),seqimpute(data, var=min(colskeep):max(colskeep), 
																							np=1, nf=1, frame.radius=0, m=3, 
																							timing=TRUE, 
																							covariates=unlist(lapply(aux, function(x) grep(x,names(data))))))
		Sys.time()
		#This part takes about 13 minutes each time

		# #Transform seqimpute object so that imputations are stacked
		# assign(paste0(t,"_stackedimp_",s),fromseqimp(get(paste0(t,"_seq_",s)), format="stacked"))
		# assign(paste0(t,"_stackedimpseq_",s),seqdef(get(paste0(t,"_stackedimp_",s)), xtstep=6))
		assign(paste0(t,"_seqimp_long_",s),fromseqimp(get(paste0(t,"_seqimp_",s)), format="long",include=FALSE))
		Sys.time()
		#N.B. From here clustering etc. performed on the stacked datasets, 
		#I don't think the function 'knows' there are copies of datasets in there, 
		#and treats as one giant dataset.

		#Dissimilarity matrix using hamming distance
		# assign(paste0(t,"_ham_",s),seqdist(get(paste0(t,"_stackedimpseq_",s)), method = "HAM"))		

		# #Hierarchical clustering
		# assign(paste0(t,"_wardCluster_",s),hclust(as.dist(get(paste0(t,"_ham_",s))), method="ward.D")

		# library("WeightedCluster")
		# assign(paste0(t,"_wardRange_",s),as.clustrange(get(paste0(t,"_wardCluster_",s)), diss=ham, ncluster=15)

		# pdf(paste0("//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p6/147/working/results/Obj1/",t,"_wardRangeplot_",s,".pdf")) 
		# plot(get(paste0(t,"_wardRange_",s)), stat=c("ASWw", "HG", "PBC", "HC"))
		# dev.off()
		}
	}
#Save all of these new objects
save(list=ls()[grep("_seqimp_",ls())],file=paste0(loc_out,"sequences_withcovs.RData"))
write.csv(out,file=paste0(loc_out,"numbers_not100miss_seqimp.csv"),row.names = F)	


#Now carry out MICE on composite variables (markers of ever having entered and exited a role, by age 30)
#Reduce dataset
load(paste0(loc_inp,"cohort_for_imp.RData"))
#Removed but can add in earlier, later (as well as ACEs): 
#"b592","pb182a","pb196a","c645","pb325"
trans_vars <- unique(c(names(AST.dta)[grepl('_yet',names(AST.dta))],
				names(AST.dta)[grepl('_age',names(AST.dta))],
				names(AST.dta)[grepl('duration',names(AST.dta))],
				names(AST.dta)[grepl('_ever',names(AST.dta))]))

keep <- c("kz021",
			trans_vars,
			aux)
keep[!keep %in% names(AST.dta)]
keep <- keep[!keep %in% keep[grep('25',keep)]]
keep <- keep[!keep %in% keep[grep('31',keep)]]
keep
keep <- keep[!keep %in% c("educ_yet","educ_yet_age","educ_exit_yet","educ_exit_yet_age",
													"employ_yet","employ_yet_age","employ_exit_yet","employ_exit_yet_age",
													"leave_parents_yet","leave_parents_yet_age","leave_parents_exit_yet","leave_parents_exit_yet_age",
													"cohab_yet","cohab_yet_age","cohab_exit_yet","cohab_exit_yet_age",
													"parent_yet","parent_yet_age","parent_exit_yet","parent_exit_yet_age"
													)]
keep

#Duration has not been calculated for any of the transition variables
for (t in trans_vector){
	AST.dta[!is.na(AST.dta[,paste0(t,"_exit_yet_30_age")]) & !is.na(AST.dta[,paste0(t,"_yet_30_age")]),paste0(t,"_duration")] <- 
			AST.dta[!is.na(AST.dta[,paste0(t,"_exit_yet_30_age")]) & !is.na(AST.dta[,paste0(t,"_yet_30_age")]),paste0(t,"_exit_yet_30_age")]-
			AST.dta[!is.na(AST.dta[,paste0(t,"_exit_yet_30_age")]) & !is.na(AST.dta[,paste0(t,"_yet_30_age")]),paste0(t,"_yet_30_age")]
	}

women.dta <- AST.dta[AST.dta$kz021=="Female",keep]
men.dta <- AST.dta[AST.dta$kz021=="Male",keep]
dim(women.dta)
dim(men.dta)

#Dry runs to update methods, etc.
library(mice)
ini <- mice(women.dta, maxit=0, pri=F)
meth <- ini$method
meth
predM <- ini$predictorMatrix
predM
exclude <- c("kz021")

meth
#leave_parents_yet_30 no 0s, cohab_yet_30 no 0s. So not imputation methods.

#Wonder if yet age and exit (and exit age) will be perfectly predicted anyway, e.g. given that they are in the observed data.
#Run imputation without these conditionals, and see if possible to have age imputed whe yet=0, or be exit if entry=0, or have exit age when exit=0
passive <- unique(c("educ_yet_30_age","employ_yet_30_age","leave_parents_yet_30_age","cohab_yet_30_age","parent_yet_30_age",
	#names(meth)[grep('age',names(meth))],
	names(meth)[grep('exit',names(meth))],
	#"educ_exit_yet_30","employ_exit_yet_30","leave_parents_exit_yet_30","cohab_exit_yet_30","parent_exit_yet_30",
	names(meth)[grep('duration',names(meth))]))
passive
#Those excluded aren't used to inform models imputing other variables
predM[,c(exclude)]<-0
#Those excluded and those passively imputed variables are not imputed by models where other covariates are (will later update passively imputed variables to be imputed by specific variables)
predM[c(exclude,passive),]<-0
meth[c(exclude,passive)]<-""
length(meth);dim(predM)

for (t in trans_vector){
	#Also don't want any yet_30_age, 'exit_yet', or 'exit_yet_age' being used to impute 'yet':
	predM[paste0(t,'_yet_30'),paste0(t,'_yet_30_age')] <- 0
	predM[paste0(t,'_yet_30'),paste0(t,'_exit_yet_30')] <- 0
	predM[paste0(t,'_yet_30'),paste0(t,'_exit_yet_30_age')] <- 0

	#Multiply by the indicator? If 1 will work, if 0 will be 0 which we convert to missing, and if missing should remain missing
	meth[grep(paste0(t,'_yet_30_age'),names(meth),value=T)] <- 
	   paste0('~ifelse(',t,'_yet_30==1,',t,'_yet_30_age,NA)') 
	meth[grep(paste0(t,'_exit_yet_30'),names(meth),value=T)] <- 
	   paste0('~ifelse(',t,'_yet_30==1,',t,'_exit_yet_30,NA)') 
	#Want to ensure duration is always positive
	meth[grep(paste0(t,'_duration'),names(meth),value=T)] <- 
	   paste0('~ifelse(',t,'_exit_yet_30==1,',t,'_duration,NA)') 
	meth[grep(paste0(t,'_exit_yet_30_age'),names(meth),value=T)] <- 
	   paste0('~I(',t,'_duration + ',t,'_yet_30_age)') 
	}

head(predM)

#Run 50 imputations, 5 iterations
m <- 50
maxit <-5
Sys.time()
#"2024-04-24 12:55:25 BST"
women.imp<- mice(women.dta, m = m,maxit=maxit,print=TRUE, method=meth,
                 predictorMatrix=predM,stringsAsFactor = TRUE, seed=140817)
Sys.time()
men.imp<- mice(men.dta, m = m,maxit=maxit,print=TRUE, method=meth,
                  predictorMatrix=predM,stringsAsFactor = TRUE, seed=140817)
Sys.time()

#Check if the conditionals happened anyway
imp_m <- NULL
imp_f <- NULL
for(i in 1:50){
  imp_f[[i]] <- complete(women.imp, action = i, inc = FALSE)
  imp_m[[i]] <- complete(men.imp, action = i, inc = FALSE)
  }
imp <- Map(rbind, imp_m, imp_f)

#Checks
table(AST.dta[,"educ_yet_30"],AST.dta[,"educ_yet_30_age"],exclude=NULL)
table(imp[[1]][,"educ_yet_30"],imp[[1]][,"educ_yet_30_age"],exclude=NULL)
table(imp[[2]][,"educ_yet_30"],imp[[2]][,"educ_yet_30_age"],exclude=NULL)

table(AST.dta[,"educ_yet_30"],AST.dta[,"educ_exit_yet_30"],exclude=NULL)
table(imp[[1]][,"educ_yet_30"],imp[[1]][,"educ_exit_yet_30"],exclude=NULL)

table(AST.dta[,"educ_exit_yet_30"],AST.dta[,"educ_exit_yet_30_age"],exclude=NULL)
table(imp[[1]][,"educ_exit_yet_30"],imp[[1]][,"educ_exit_yet_30_age"],exclude=NULL)

#Did duration run and what does that look like?
table(imp[[1]][,"educ_duration"],imp[[1]][,"educ_yet_30_age"],exclude=NULL)

#Check the proportions of things... average ages of things... for first imputation
table(imp[[1]][,"educ_yet_30"],exclude=NULL)
summary(imp[[1]][,"educ_yet_30_age"],na.rm=TRUE)
table(imp[[1]][,"educ_exit_yet_30"],exclude=NULL)
summary(imp[[1]][,"educ_exit_yet_30_age"],na.rm=TRUE)


save(imp_m,imp_f,imp,file=paste0(loc_out,"composite_vars.RData"))
