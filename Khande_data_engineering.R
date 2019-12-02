
## @knitr load_up_the_stuf

# ipak function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.


# require(haven)
# require(dplyr)
# require(ggplot2)
# require(reshape2)



#CS:  when this is read in, it's already a data frame so you don't need this line
raw_data_sas <- as.data.frame(w1)

#suppressing the variable GENDER (incomplete, we use D_GENDER that we rename just GENDER)
raw_data_sas$W1_GENDER <- NULL

#remove the "W1_" that precedes every variable name (except studyID)
for (n in 1:ncol(raw_data_sas)) {
  s<-colnames(raw_data_sas)[n]
  if( s !="STUDYID"){
    colnames(raw_data_sas)[n]<-substr(s,4,nchar(s))
  }
}

#Renaming some variables (some names have changed in the new data)
colnames(raw_data_sas)[which(colnames(raw_data_sas)=="D_RACE_SUMMARY")]<-"race"
colnames(raw_data_sas)[which(colnames(raw_data_sas)=="EDU_EDUCATION")]<-"EDUCATION"
colnames(raw_data_sas)[which(colnames(raw_data_sas)=="EDU_LONGCERT")]<-"LONGCERT"
colnames(raw_data_sas)[which(colnames(raw_data_sas)=="EDU_TRNCERT")]<-"TRNCERT"
colnames(raw_data_sas)[which(colnames(raw_data_sas)=="EDU_EDUCATION_TEXT")]<-"EDUCATION_TEXT"
colnames(raw_data_sas)[which(colnames(raw_data_sas)=="ECOG_CONCERNED_THINKING")]<-"CONCERNED_THINKING"
colnames(raw_data_sas)[which(colnames(raw_data_sas)=="NIHTLBX_depr_theta")]<-"depression_01"
colnames(raw_data_sas)[which(colnames(raw_data_sas)=="INTERVIEW_AGE")]<-"age"
colnames(raw_data_sas)[which(colnames(raw_data_sas)=="D_GENDER")]<-"GENDER"
#renaming some levels within factors
raw_data_sas[which(raw_data_sas[,"CONCERNED_THINKING"]==2),"CONCERNED_THINKING"] <- "No"
raw_data_sas[which(raw_data_sas[,"CONCERNED_THINKING"]==1),"CONCERNED_THINKING"] <- "Yes"
raw_data_sas[which(raw_data_sas[,"GENDER"]==2),"GENDER"] <- "Woman"
raw_data_sas[which(raw_data_sas[,"GENDER"]==1),"GENDER"] <- "Man"

#Centering SENAS scores 
raw_data_sas$SENAS_vrmem <- scale(raw_data_sas$SENAS_vrmem)
raw_data_sas$SENAS_wm <-scale(raw_data_sas$SENAS_wm)
raw_data_sas$SENAS_exec <- scale(raw_data_sas$SENAS_exec)


#create a clean table
raw_data_clean <- raw_data_sas


nc<-ncol(raw_data_clean)
for (col in 2:nc) {
    raw_data_clean[raw_data_clean[,col]%in% 
                     c("",99.000,"99",999.00,"999",9999.00,"9999","N/A","Na",NaN,"NaN","Refused/Don't Know"),
                   col]<- NA
    #if not age (some poeple may have age 88)
    if(colnames(raw_data_clean)[col]!="age"){
    raw_data_clean[raw_data_clean[,col] %in% 
                     c("Dk","DK","Don't know","?",88,"88",888,"888"),col]<- NA 
    }
}



##RENAMING some variables

## @knitr converting_some_variables

#The names of some variables are just composed of 2 letters and are difficult 
#to remember. the following lines rename them to more understandable names:
#(Accesorily I also use the loop to make sure the data type is "numeric")
#The corresponding long names are IN THE SAME ORDER)
#I've left some identical when they aren't usefull

short_names <-c("vm","vm_se","vmform","pa","pa_se","on","on_se","phon","phon_se",
                "wm","wm_se","cat","cat_se","vrmem","sem","exec")

#the new dataset has "SENAS_"in front of the variale names now
short_names<-paste0("SENAS_",short_names)

long_names <- c("verbal_episodic_mem","verbal_episodic_mem_se","vmform",
                "picture_assoc","picture_assoc_se","object_naming",
                "object_naming_se","fluency_phon","fluency_phon_se",
                "working_mem","working_mem_se","fluency_cat","fluency_cat_se",
                "adj_verbal_episodic_mem","semantic_memory","executive_function")
for (vn in 1:length(short_names)) {
  columnIndex <- which(colnames(raw_data_clean)==short_names[vn])
  colnames(raw_data_clean)[columnIndex]<-long_names[vn]
  raw_data_clean[,columnIndex]<-as.numeric(raw_data_clean[,columnIndex])
}


## @knitr renaming_variables(more renaming)


#changing the name of the race groups
raw_data_clean[which(raw_data_clean[,"race"]=="White"),"race"] <- "Non-Latino-White"
raw_data_clean[which(raw_data_clean[,"race"]=="LatinX"),"race"] <- "Latino"
raw_data_clean[which(raw_data_clean[,"race"]=="AfrAmer"),"race"] <- "Black"
raw_data_clean[which(raw_data_clean[,"race"]=="Native American"),"race"] <- NA #there's just one guy..
raw_data_clean[which(raw_data_clean[,"race"]=="Refused/Missing"),"race"] <- NA

#Concernerd thinking
raw_data_clean[which(raw_data_clean[,"CONCERNED_THINKING"]==1),"CONCERNED_THINKING"] <- "Yes"
raw_data_clean[which(raw_data_clean[,"CONCERNED_THINKING"]==2),"CONCERNED_THINKING"] <- "No"

#Language
raw_data_clean$language<-ifelse(raw_data_clean$LANG==2058,"SPANISH","ENGLISH")
raw_data_clean$language<-as.factor(raw_data_clean$language)
## ----
#Creating a short verion of the Ecog (based of 12 items)
Ecog_12items <- c("ECOG_MEM1","ECOG_MEM2","ECOG_LANG1","ECOG_LANG2",
                  "ECOG_VISUAL_SPATIAL2","ECOG_VISUAL_SPATIAL5",
                  "ECOG_PLANNING1","ECOG_PLANNING3",
                  "ECOG_ORGANIZATION1","ECOG_ORGANIZATION2",
                  "ECOG_DIVIDED_ATTENTION1","ECOG_DIVIDED_ATTENTION2" )


for (i in Ecog_12items){
  raw_data_clean[,i] <-as.numeric(raw_data_clean[,i])
}

# saving all to a fresh dataset
raw_data_averages <- raw_data_clean



######################################################################3
#converting EDUCATON to "years of EDUCATION"
######################################################################

#EDUCATION_TEXT contains years of educ. for those who have <13 years of education 
#and some crap coding for "missing"(99) and "refused"(88) which we recode to "NA")
# we create a variable TRUE_CERT:
# TRNCERT indicates if participant has additional training 
#(self-taught: TRNCERT=1, or formally trained: TRNCERT=2)
# LONGCERT indicates how long it took to train (1-4, with 4 meaning duration >= 6months)
# if TRNCERT=2 and LONGCERT=4, then TRUE_CERT=1 (otherwise 0)

#creating the column
raw_data_averages$yrEDUCATION <- raw_data_averages$EDUCATION

raw_data_averages$TRUECERT <- factor(
  ifelse(raw_data_averages[,"TRNCERT"]==2 &
           raw_data_averages[,"LONGCERT"]==4,
         1,0))


# number of years of education = EDUCATION_TEXT 
#when they did not attend college (EDUCATION=0)
raw_data_averages[
  which(raw_data_averages[,"EDUCATION"]==0),
  "yrEDUCATION"] <- 
  as.numeric(
    raw_data_averages[
    which(raw_data_averages[,"EDUCATION"]==0),
    "EDUCATION_TEXT"])

#for those without college, if TRUECERT=1 they get an extra year of education
raw_data_averages[
  which(
    raw_data_averages$EDUCATION==0 &
      raw_data_averages$TRUECERT==1),
  "yrEDUCATION"] <-
  as.numeric(
    raw_data_averages[
      which(
        raw_data_averages$EDUCATION==0 &
          raw_data_averages$TRUECERT==1),
      "yrEDUCATION"])+1

raw_data_averages[
  which(raw_data_averages[,"EDUCATION"]==1),
  "yrEDUCATION"] <- 13

raw_data_averages[
  which(raw_data_averages[,"EDUCATION"]==2),
  "yrEDUCATION"] <- 14

raw_data_averages[
  which(raw_data_averages[,"EDUCATION"]==3),
  "yrEDUCATION"] <- 16

raw_data_averages[
  which(raw_data_averages[,"EDUCATION"]==4),
  "yrEDUCATION"] <- 18

raw_data_averages[
  which(raw_data_averages[,"EDUCATION"]==5),
  "yrEDUCATION"] <- 20

raw_data_averages$yrEDUCATION <- as.numeric(raw_data_averages$yrEDUCATION)
table(raw_data_averages$EDUCATION,raw_data_averages$yrEDUCATION,useNA = "ifany")

################################################################################3
## Coding family history
################################################################################

## @knitr Family_history

#there are 14 variables indicating if a relative has dementia
# Originally REL_DEMENTIA_[1-14] is coded 1=yes 2=no

#converting to numeric, and converting missing to NA,
for (n in 1:14) {
  v <- paste0("REL_DEMENTIA_",n)
  raw_data_averages[,v] <- as.numeric(raw_data_averages[,v])
  raw_data_averages[,v] <- ifelse(raw_data_averages[,v]==1,1,
                                 ifelse(raw_data_averages[,v]==0,0,NA))
}

#summing the number of parents(REL_DEMENTIA 1 and 2), or siblings (3 to 14) have dementia
raw_data_averages$PARENTAL_DEMENTIA <- 
  rowSums(raw_data_averages[,c("REL_DEMENTIA_1","REL_DEMENTIA_2")],na.rm = T)
raw_data_averages$SIBLING_DEMENTIA <- 
  rowSums(raw_data_averages[,c(paste0("REL_DEMENTIA_",3:14))],na.rm = T)
#does any relative have dementia ?
raw_data_averages$RELATIVE_DEMENTIA <- 
  ifelse(rowSums(raw_data_averages[,c("PARENTAL_DEMENTIA","SIBLING_DEMENTIA")],na.rm = T)>0,1,0)
#do more than one relative have dementia?
raw_data_averages$RELATIVE_DEMENTIA2plus <- 
  ifelse(rowSums(raw_data_averages[,c("PARENTAL_DEMENTIA","SIBLING_DEMENTIA")],na.rm = T)>1,1,0)



ListOfVars <- c("semantic_memory",
                "adj_verbal_episodic_mem",
                "executive_function",
                "age",
                "GENDER",
                "race",
                "CONCERNED_THINKING",
                "EDUCATION",
                "yrEDUCATION",
                Ecog_12items,
                "RELATIVE_DEMENTIA",
                "SIBLING_DEMENTIA",
                "PARENTAL_DEMENTIA",
                "language",
                "depression_01")
DF<-raw_data_averages[,ListOfVars]

##############################################################################
#COUNTING HOW MANY ECOG ITEMS (individual questions) ARE MISSING PER PARTICIPANT
#Creating indicator variables to account for patients with missing values at the Ecog
#creating variable names:
Ecog12_indicator_variables <- paste0(Ecog_12items,"ind")

#creating the empty dataframe and fusing it to the DF 
missing_Ecog <- data.frame(matrix(ncol =12 , nrow = nrow(DF)))
colnames(missing_Ecog)<-Ecog12_indicator_variables
DF <- cbind(DF,missing_Ecog)

#Counting missing data
for (i in Ecog_12items) {
DF[,paste0(i,"ind")]   <- ifelse(is.na(DF[,i]),1,0)
}
DF$Ecog12_missing <- rowSums(DF[,Ecog12_indicator_variables])


#creating a variable that includes partial Ecog data (=with some missing items)
DF$Ecog12_including_partial_averages<-rep(NA)
DF$Ecog12_including_partial_averages=rowMeans(DF[,Ecog_12items], na.rm=TRUE)

#if all 12 items are missing(=NA) otherwise indicate how many are missing
DF$Ecog12_missing <- ifelse(DF$Ecog12_missing==12,NA,DF$Ecog12_missing)

#add those 2 new columns to the main datatable
raw_data_averages$Ecog12_including_partial_averages <- DF$Ecog12_including_partial_averages
raw_data_averages$Ecog12_missing <- DF$Ecog12_missing

##############################################################
# #creating a special table of people that report various levels of impairment (experimental)
Ecog_counts <- data.frame(lvl=factor(rep(1:4,each=12)),Nitems=factor(rep(1:12,times=4)) ,count=rep(0,48))
for(lvl in 1:4){
    binary_SCD <- ifelse(DF[,Ecog_12items]== lvl,1,0)
  for(n in 1:12){
    sumSCD<- rowSums(binary_SCD,na.rm = T)
    binSCD <- ifelse(sumSCD >= n,1,0)
    Ecog_counts[lvl*12-12+n,3]<- (sum(binSCD,na.rm = T)/1617)*100
  }
}

require(ggplot2)
EcogSeverity <-ggplot(Ecog_counts)+
  geom_line(aes(x=Ecog_counts$Nitems,y=Ecog_counts$count,
                                  group=Ecog_counts$lvl,colour=Ecog_counts$lvl),
            size=1.2)+
  xlab("Number of items")+
  ylab("Percentage of the population")+
  scale_color_discrete(name= "Perceived change in 10 years", 
                       labels=c("no change","sometimes","systematically a little worse","systematically much worse"))+
  theme(legend.justification=c(1,0), 
        legend.position=c(0.99,0.69),
        text = element_text(size=14))
# ###############################################################
################################################################################

#creating a log-transformed ECog12
DF$logEcog12 <- log(DF$Ecog12_including_partial_averages)
#DF$logEcog12 <- ifelse(DF$logEcog12=="NaN",NA,DF$logEcog12)
raw_data_averages$logEcog12 <- log(raw_data_averages$Ecog12_including_partial_averages)

#creating a centered age and a centered age in decades(because effect sizes for 1 year are too small)
raw_data_averages$Age_centered_75 <- scale(raw_data_averages$age,center = 75, scale = FALSE)
raw_data_averages$Age_centered_75_decades <- as.numeric(raw_data_averages$Age_centered_75/10)
raw_data_averages$yrEDUCATION_centered <- as.numeric(scale(raw_data_averages$yrEDUCATION,center = 12, scale = FALSE))

#Removing some missing data for key variables

DF<-DF[-which(is.na(DF$logEcog12)),]
DF<-DF[-which(is.na(DF$executive_function)),]
DF<-DF[-which(is.na(DF$adj_verbal_episodic_mem)),]
DF$race <- factor(DF$race)
DF<-DF[-which(is.na(DF$race)),]
DF$age <- as.numeric(DF$age)
DF<-DF[-which(is.na(DF[,c("yrEDUCATION")])),]
DF<-DF[-which(is.na(DF$depression_01)),]
DF<-DF[DF$Ecog12_missing<7,]

#same for raw_data_avg
 raw_data_averages<-raw_data_averages[-which(is.na(raw_data_averages$logEcog12)),]
 raw_data_averages<-raw_data_averages[-which(is.na(raw_data_averages$executive_function)),]
 raw_data_averages<-raw_data_averages[-which(is.na(raw_data_averages$adj_verbal_episodic_mem)),]
 raw_data_averages<-raw_data_averages[-which(is.na(raw_data_averages$race)),]
  raw_data_averages<-raw_data_averages[-which(is.na(raw_data_averages$yrEDUCATION)),]
 raw_data_averages<-raw_data_averages[-which(is.na(raw_data_averages$depression_01)),]
 raw_data_averages <- raw_data_averages[raw_data_averages$Ecog12_missing<7,]

## saving the final version of the data tables (OUTSIDE OF THE GIT REPO)
#write.csv(raw_data_averages, "/Users/fcorlier/Box/Fabian_ERM_Box/Khandle_data_analysis_FABIAN/R_KHANDLE_project_data/raw_data_averages.csv")

 require(tableone)
 memory_bin <- factor(cut(DF$adj_verbal_episodic_mem,c(-4,0,4)),labels = c("0 and less","more than 0"))
 executive_fn_bin <- factor(cut(DF$executive_function,c(-4,0,4)),labels = c("0 and less","more than 0"))
 Age_bin <- factor(cut(DF$age,c(65,75,85,150)),labels = c("75 and less","(75-85]","85+"))
 Edu_bin <- factor(cut(DF$yrEDUCATION,c(0,12,16,22)),labels = c("12 and less","(12-16]","16+"))
 Depressive_Smpt_bin <- factor(cut(DF$depression_01,c(-4,0,4)),labels = c("0 and less","more than 0"))
 Race_bin <- factor(DF$race, labels = c("Asian", "Black", "Latino", "Non-Latino-White"))
 fam_hist_bin <- factor(DF$RELATIVE_DEMENTIA)
 
 Tbl_bin <- cbind(memory_bin,executive_fn_bin,Age_bin,DF$GENDER,Race_bin,Edu_bin,Depressive_Smpt_bin,fam_hist_bin)
 Tbl_bin <- as.data.frame(Tbl_bin)
 Tbl_bin$memory_bin <- factor(memory_bin)
 Tbl_bin$executive_fn_bin <- factor(executive_fn_bin)
 Tbl_bin$Age_bin <- factor(Age_bin)
 Tbl_bin$Race_bin <- factor(Race_bin)
 Tbl_bin$Edu_bin <- factor(Edu_bin)
 Tbl_bin$Depressive_Smpt_bin<-factor(Depressive_Smpt_bin)
 Tbl_bin$fam_hist_bin <- factor(fam_hist_bin)
 Tbl_bin$logEcog<-DF$logEcog12
 
 colnames(Tbl_bin)<-c("Episodic memory","Executive function","Age","Gender","Race/Ethnicity","Educational attainment", "Depressive_symptoms","Family_history","logEcog")
 
 TblEcog <- list()
 for(n in dput(names(Tbl_bin))){
   TblEcog[[n]] <- CreateTableOne(vars= c("logEcog"),strata = n,data = Tbl_bin,test = F )
 }
 saveRDS(TblEcog,file = here("Ecog_table.rds"))
 
