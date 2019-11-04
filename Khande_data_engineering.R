
## @knitr load_up_the_stuf

require(haven)
require(dplyr)
require(ggplot2)
require(reshape2)
##loading the old data for comparison (for AGE, which for some reason is missing from the new dataset....)
#W0<- read_sas("C:/Users/fcorlier/Box/KANDLE_coded_data/Raw_data_tables/W1/transfer0_2019May/aaic_mariaglymour_20190507.sas7bdat")
##loading depression scores
#aaic_mariaglymour_20190821 <- read_sas("C:/Users/fcorlier/Box/KANDLE_coded_data/Raw_data_tables/W1/transfer1_2019Aug/aaic_mariaglymour_20190821.sas7bdat")
##loading the new data
w1 <- read_sas("C:/Users/fcorlier/Box/KANDLE_coded_data/Raw_data_tables/W1/transfer4_2019Oct18/khandle_baseline_20191018.sas7bdat")

raw_data_sas <- as.data.frame(w1)
raw_data <- raw_data_sas

raw_data_sas$W1_GENDER <- NULL

#print the type of each variable and remove the "W1_" that precedes every variable name (except studyID)
for (n in 1:ncol(raw_data_sas)) {
  s<-colnames(raw_data_sas)[n]
  print(paste0(s, " is detected as: ", typeof(raw_data_sas[,n])))
  if( s !="STUDYID"){
    colnames(raw_data_sas)[n]<-substr(s,4,nchar(s))
  }
}

#Renaming some variables (some names have changed in the new data)
colnames(raw_data_sas)[which(colnames(raw_data_sas)=="D_RACE_SUMMARY")]<-"race"
#colnames(raw_data_sas)[which(colnames(raw_data_sas)=="D_EDUCATION")]<-"EDUCATION" #not used
colnames(raw_data_sas)[which(colnames(raw_data_sas)=="EDU_EDUCATION")]<-"EDUCATION"
colnames(raw_data_sas)[which(colnames(raw_data_sas)=="EDU_LONGCERT")]<-"LONGCERT"
colnames(raw_data_sas)[which(colnames(raw_data_sas)=="EDU_TRNCERT")]<-"TRNCERT"
colnames(raw_data_sas)[which(colnames(raw_data_sas)=="EDU_EDUCATION_TEXT")]<-"EDUCATION_TEXT"
colnames(raw_data_sas)[which(colnames(raw_data_sas)=="ECOG_CONCERNED_THINKING")]<-"CONCERNED_THINKING"
colnames(raw_data_sas)[which(colnames(raw_data_sas)=="NIHTLBX_depr_theta")]<-"depression_01"
colnames(raw_data_sas)[which(colnames(raw_data_sas)=="INTERVIEW_AGE")]<-"age"
colnames(raw_data_sas)[which(colnames(raw_data_sas)=="D_GENDER")]<-"GENDER"
#colnames(raw_data_sas)[which(colnames(raw_data_sas)=="LANG")]<-"LANG"
raw_data_sas[which(raw_data_sas[,"CONCERNED_THINKING"]==2),"CONCERNED_THINKING"] <- "No"
raw_data_sas[which(raw_data_sas[,"CONCERNED_THINKING"]==1),"CONCERNED_THINKING"] <- "Yes"
raw_data_sas[which(raw_data_sas[,"GENDER"]==2),"GENDER"] <- "Woman"
raw_data_sas[which(raw_data_sas[,"GENDER"]==1),"GENDER"] <- "Man"


#create a clean table
raw_data_clean <- raw_data_sas


nc<-ncol(raw_data_clean)
for (col in 2:nc) {
    raw_data_clean[raw_data_clean[,col]%in% 
                     c("",99.000,"99",999.00,"999",9999.00,"9999","N/A","Na",NaN,"NaN","Refused/Don't Know"),
                   col]<- NA
    if(colnames(raw_data_clean)[col]!="age"){
    raw_data_clean[raw_data_clean[,col] %in% 
                     c("Dk","DK","Don't know","?",88,"88",888,"888"),col]<- NA 
    }
}

raw_data_sas <- raw_data_clean
######################################################################
## PLOTTING ALL THE VARIABLES FROM THE DATASET IN AN EXTERNAL PDF file 
# this is just to have a first look at the data
#(if make.plot.pdf = 0 it skips this step)
#(if make.plot.pdf = 1 the function f_FIRST_GLANCE is called; set the path the
#saved results)
make.plot.pdf <- 0

#creating the function:
f_FIRST_GLANCE <- function(path2pdf, Dataset){


pdf(file = path2pdf)

for (col in 2:nc){
  #if there's not ONLY NAs, continue
  if(sum(is.na(Dataset[,col])) !=nrow(Dataset) & typeof(Dataset[,col]) %in% c("integer","logical","double")){
    par(mfrow=c(1,3))
      if (length(unique(Dataset[,col]))>2){
        par(mfrow=c(1,3))
        print(paste0(colnames(Dataset)[col]," detected as continuous double"))
        boxplot(Dataset[,col])
        title(main = paste0(colnames(Dataset)[col]," with na's"))
        boxplot(Dataset[,col])
        title(main = colnames(Dataset)[col])
        hist(Dataset[,col], main=colnames(Dataset)[col])
      }
      if(length(unique(Dataset[,col]))<=2){
        print(paste0(colnames(Dataset)[col]," detected as binary"))
        title_string <- as.character(colnames(Dataset)[col])
        text_string <- paste0("count= ",sum(raw_data[,col],na.rm=T))
        print(ggplot(data = Dataset) + theme(axis.title.x = element_blank())+
                  geom_bar(aes(get(colnames(Dataset)[col]))) +  
                  ggtitle(paste0 (title_string, " ",text_string))+
                ylim(0,high=1592) + theme_bw()+theme(axis.title.x = element_blank(),axis.text.x=element_text(angle=90, size = 11))+
          geom_text(stat="count", aes(x= get(colnames(Dataset)[col]),label=..count..), vjust=-1))
      }
  }
  if(sum(is.na(Dataset[,col])) !=nrow(Dataset) & typeof(Dataset[,col]) %in% c("character","factor")){
    par(mfrow=c(1,1))
    titl.txt <- as.character(colnames(Dataset)[col])
    print(paste0(colnames(Dataset)[col]," is a factor"))
    if (length(unique(Dataset[,col]))>10) {
      print(ggplot(data = Dataset) + 
              geom_bar(aes(x= get(colnames(Dataset)[col])))+
              theme(axis.title.x = element_blank(),axis.text.x=element_text(angle=90,size=7)) + 
              ggtitle(titl.txt)+
              geom_text(stat="count", aes(x= get(colnames(Dataset)[col]),label=..count..), vjust=-1))
    }
    else{ print(ggplot(data = Dataset) + 
            geom_bar(aes(x= get(colnames(Dataset)[col])))+
            theme(axis.title.x = element_blank(),axis.text.x=element_text(angle=90,size=11)) + 
            ggtitle(titl.txt)+
            geom_text(stat="count", aes(x= get(colnames(Dataset)[col]),label=..count..), vjust=-1))
    }
  }
}
}

# PLEASE SET THE PATH TO THE RESULTS HERE (this file will be created later)
path2pdf <-"C:\\Users/fcorlier/Box/Fabian_ERM_Box/Khandle_data_analysis_FABIAN/
All_variables_figures.pdf"
#
# Running the function:
if(make.plot.pdf==1){
f_FIRST_GLANCE(path2pdf=path2pdf, Dataset = raw_data_sas) 
dev.off()
}
########################################################################


##RENAMING some variables

## @knitr converting_some_variables

#The names of some variables are just composed of 2 letters and are difficult 
#to remember. the following lines rename them to more understandable names:
#(Accesorily I also use the loop to make sure the data type is "numeric")
#The corresponding long names are IN THE SAME ORDER)
#I've left some identical when they aren't usefull

##TEMPORARY FIX: WE DON'T HAVE THE SENAS FOR THE NEW DATASET SO WE USE THE OLD ONES

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


## @knitr renaming_variables



#changing the name of the race groups
raw_data_clean[which(raw_data_clean[,"race"]=="White"),"race"] <- "Non-Latino-White"
raw_data_clean[which(raw_data_clean[,"race"]=="LatinX"),"race"] <- "Latino"
raw_data_clean[which(raw_data_clean[,"race"]=="AfrAmer"),"race"] <- "Black"
raw_data_clean[which(raw_data_clean[,"race"]=="Native American"),"race"] <- NA
raw_data_clean[which(raw_data_clean[,"race"]=="Refused/Missing"),"race"] <- NA





#Concernerd thinking
raw_data_clean[which(raw_data_clean[,"CONCERNED_THINKING"]==1),"CONCERNED_THINKING"] <- "Yes"
raw_data_clean[which(raw_data_clean[,"CONCERNED_THINKING"]==2),"CONCERNED_THINKING"] <- "No"

#Language
raw_data_clean$language<-ifelse(raw_data_clean$LANG==2058,"SPANISH","ENGLISH")
#raw_data_clean[which(raw_data_clean[,"language"]==1033 ),"language"] <- "English"
#raw_data_clean[which(raw_data_clean[,"language"]==2058 ),"language"] <- "Spanish"
raw_data_clean$language<-as.factor(raw_data_clean$language)
## ----
#Creating a short verion of the Ecog (based of 12 items)
Ecog_12items <- c("ECOG_MEM1","ECOG_MEM2","ECOG_LANG1","ECOG_LANG2",
                  "ECOG_VISUAL_SPATIAL2","ECOG_VISUAL_SPATIAL5",
                  "ECOG_PLANNING1","ECOG_PLANNING3",
                  "ECOG_ORGANIZATION1","ECOG_ORGANIZATION2",
                  "ECOG_DIVIDED_ATTENTION1","ECOG_DIVIDED_ATTENTION2" )

# Ecog_12items <- c("MEM1","MEM2","LANG1","LANG2","VISUAL_SPATIAL2",
#                   "VISUAL_SPATIAL5","PLANNING1","PLANNING3","ORGANIZATION1",
#                   "ORGANIZATION2","DIVIDED_ATTENTION1","DIVIDED_ATTENTION2")
for (i in Ecog_12items){
  raw_data_clean[,i] <-as.numeric(raw_data_clean[,i])
}

# saving all to a fresh dataset
raw_data_averages <- raw_data_clean

######################################################################3
#converting EDUCATON to "years of EDUCATION"
######################################################################3

#EDUCATION_TEXT contains years of educ. for those who have <13 years of education 
#and some crap coding for "missing"(99) and "refused"(88) which we recode to "NA")
# we create a variable TRUE_CERT:
# TRNCERT indicates if participant has additional training 
#(self-taught: TRNCERT=1, or formally trained: TRNCERT=2)
# LONGCERT indicates how long it took to train (1-4, with 4 meaning duration >= 6months)
# if TRNCERT=2 and LONGCERT=4, then TRUE_CERT=1 (otherwise 0)

#creating the column
 raw_data_averages$yrEDUCATION <- raw_data_averages$EDUCATION
# raw_data_averages$EDUCATION_TEXT[which(raw_data_averages$EDUCATION_TEXT==99)] <- NA
# raw_data_averages$EDUCATION_TEXT[which(raw_data_averages$EDUCATION_TEXT==88)] <- NA
raw_data_averages$TRUECERT <- factor(
  ifelse(raw_data_averages[,"TRNCERT"]==2 &
           raw_data_averages[,"LONGCERT"]==4,
         1,0))
raw_data_averages$TRUECERT[which(raw_data_averages$TRUECERT==99)] <- NA
# raw_data_averages$EDUCATION[which(raw_data_averages$EDUCATION==99)] <- NA
# raw_data_averages$EDUCATION[which(raw_data_averages$EDUCATION==88)] <- NA

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
                    
# raw_data_averages$EDUCATION[which(raw_data_averages$yrEDUCATION==99)] <- NA
# raw_data_averages$EDUCATION[which(raw_data_averages$yrEDUCATION==88)] <- NA

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
#######################################################################################

## Coding family history

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

raw_data_averages$PARENTAL_DEMENTIA <- rowSums(raw_data_averages[,c("REL_DEMENTIA_1","REL_DEMENTIA_2")],na.rm = T)
raw_data_averages$SIBLING_DEMENTIA <- rowSums(raw_data_averages[,c(paste0("REL_DEMENTIA_",3:14))],na.rm = T)
#does any relative have dementia ?
raw_data_averages$RELATIVE_DEMENTIA <- ifelse(rowSums(raw_data_averages[,c("PARENTAL_DEMENTIA","SIBLING_DEMENTIA")],na.rm = T)>0,1,0)
#do more than one relative have dementia?
raw_data_averages$RELATIVE_DEMENTIA2plus <- ifelse(rowSums(raw_data_averages[,c("PARENTAL_DEMENTIA","SIBLING_DEMENTIA")],na.rm = T)>1,1,0)


# nc<-ncol(raw_data_averages)
# for (col in 2:nc) {
#   raw_data_averages[which(as.numeric(raw_data_averages[,col])%in% c("",99,"99",999,"999",9999,"9999","N/A","Na")),col]<- NA
#   if(colnames(raw_data_averages)[col]!="collapsed_age"){
#     raw_data_averages[which(raw_data_averages[,col] %in% c("Dk","DK","Don't know","?",88,"88",888,"888")),col]<- NA 
#   }
# }

ListOfVars <- c("semantic_memory","adj_verbal_episodic_mem","executive_function","age","GENDER","race","CONCERNED_THINKING","EDUCATION","yrEDUCATION",Ecog_12items, "RELATIVE_DEMENTIA","SIBLING_DEMENTIA","PARENTAL_DEMENTIA","language","depression_01")
DF<-raw_data_averages[,ListOfVars]
# for (n in ListOfVars) {
#   DF[which(DF[,n]==99),n]<-NA
#   
# }
##############################################################################
#Creating indicator variables to account for patients with missing values at the Ecog
#creating variable names:
Ecog12_indicator_variables <- paste0(Ecog_12items,"ind")

#creating the empty dataframe
missing_Ecog <- data.frame(matrix(ncol =12 , nrow = nrow(DF)))
colnames(missing_Ecog)<-Ecog12_indicator_variables
DF <- cbind(DF,missing_Ecog)
##############################################################
# #creating a special binary variable of poeple that have SCD
# binary_SCD <- ifelse(DF[,Ecog_12items]>2,1,0)
# binary_SCD$sumSCD<- rowSums(binary_SCD,na.rm = T)
# binary_SCD$binSCD <- ifelse(binary_SCD$sumSCD>0,1,0)
# table(binary_SCD$binSCD)
# DF$binSCD <- binary_SCD$binSCD
# DF$sumSCD <- binary_SCD$sumSCD
# ###############################################################
# y<- DF$sumSCD
# x<- DF$adj_verbal_episodic_mem
# mRCS<- glm(y~rms::rcs(x,quantile(x,c(0,.05,.275,.5,.775,.95,1),include.lowest=T,na.rm = T))+DF$age + DF$yrEDUCATION)
# mLR <- glm(y~x+DF$age + DF$yrEDUCATION, family = "binomial")
# plot(x,mRCS$fitted.values,
#              col = "red",
#              xlim = c(min(x),max(x)),
#              ylim = c(min(y),max(y)))

points(x,y)
# ###############################################################




for (i in Ecog_12items) {
DF[,paste0(i,"ind")]   <- ifelse(is.na(DF[,i]),1,0)
}

DF$Ecog12_missing <- rowSums(DF[,Ecog12_indicator_variables])

#creating a variable that includes partial Ecog data (=with some missing items)
DF$Ecog12_including_partial_averages<-rep(NA)

DF$Ecog12_including_partial_averages=rowMeans(DF[,Ecog_12items], na.rm=TRUE)

#if all 12 items are missing=NA otherwise indicate how many are missing
DF$Ecog12_missing <- ifelse(DF$Ecog12_missing==12,NA,DF$Ecog12_missing)

#add those 2 new columns to the main datatable
raw_data_averages$Ecog12_including_partial_averages <- DF$Ecog12_including_partial_averages
raw_data_averages$Ecog12_missing <- DF$Ecog12_missing

################################################################################

#at some point I had 99 and 88 removed but sometimes those anoying codes re-appear
raw_data_averages[which(raw_data_averages$Ecog12_including_partial_averages==99),"Ecog12_including_partial_averages"]<-NA
raw_data_averages[which(raw_data_averages$Ecog12_including_partial_averages==88),"Ecog12_including_partial_averages"]<-NA
DF$Ecog12_including_partial_averages <- ifelse(DF$Ecog12_including_partial_averages=="NaN",NA,DF$Ecog12_including_partial_averages)

#creating a log-transformed ECog12
DF$logEcog12 <- log(DF$Ecog12_including_partial_averages)
DF$logEcog12 <- ifelse(DF$logEcog12=="NaN",NA,DF$logEcog12)
raw_data_averages$logEcog12 <- log(raw_data_averages$Ecog12_including_partial_averages)

#creating a centered age and a centered age in decades(because effect sizes for 1 year are too small)
raw_data_averages$Age_centered_75 <- scale(raw_data_averages$age,center = 75, scale = FALSE)
raw_data_averages$Age_centered_75_decades <- raw_data_averages$Age_centered_75/10
raw_data_averages$yrEDUCATION_centered <- scale(raw_data_averages$yrEDUCATION,center = 12, scale = FALSE)

#Removing some missing data for key variables
DF$race <- factor(DF$race)
DF<-DF[-which(is.na(DF$logEcog12)),]
DF<-DF[-which(is.na(DF$adj_verbal_episodic_mem)),]
DF$age <- as.numeric(DF$age)
#DF<-DF[-which(is.na(DF[,c("race")])),]
DF<-DF[-which(is.na(DF[,c("yrEDUCATION")])),]
DF<-DF[-which(is.na(DF$depression_01)),]
#DF<-DF[-which(is.na(DF[,c("GENDER")])),]

#same for raw_data_avg
 raw_data_averages<-raw_data_averages[-which(is.na(raw_data_averages$logEcog12)),]
 raw_data_averages<-raw_data_averages[-which(is.na(raw_data_averages$adj_verbal_episodic_mem)),]
# raw_data_averages<-raw_data_averages[-which(is.na(raw_data_averages$race)),]
 raw_data_averages<-raw_data_averages[-which(is.na(raw_data_averages$yrEDUCATION)),]
# raw_data_averages<-raw_data_averages[-which(is.na(raw_data_averages$GENDER)),]
 raw_data_averages<-raw_data_averages[-which(is.na(raw_data_averages$depression_01)),]


## saving the final version of the data tables
#write.csv(DF, "/Users/fcorlier/Box/Fabian_ERM_Box/Khandle_data_analysis_FABIAN/R_KHANDLE_project_data/DF.csv")
write.csv(raw_data_averages, "/Users/fcorlier/Box/Fabian_ERM_Box/Khandle_data_analysis_FABIAN/R_KHANDLE_project_data/raw_data_averages.csv")
