# here we start analysis the HKANDLE data:

require(sjPlot)
require(sjlabelled)
require(sjmisc)
require(kableExtra)
require(lspline)

raw_data_averages <- read.csv("/Users/fcorlier/Box/Fabian_ERM_Box/Khandle_data_analysis_FABIAN/R_KHANDLE_project_data/raw_data_averages.csv")[,2:694]#saving as csv always adds a column of indices in [,1] so I load from 2 to ncol+1
ListOfVars <- c("semantic_memory","adj_verbal_episodic_mem","executive_function","Age_centered_75","Age_centered_75_decades","GENDER","race","CONCERNED_THINKING","yrEDUCATION_centered",Ecog_12items, "RELATIVE_DEMENTIA","SIBLING_DEMENTIA","PARENTAL_DEMENTIA","Ecog12_including_partial_averages","depression_01","logEcog12")
short_names <- c("semantic_memory","memory","ex_function","AGE","AGE_75_d","GENDER","race","CONCERNED_THINKING","yrEDU",Ecog_12items, "F_Hist","SIBLING_DEMENTIA","PARENTAL_DEMENTIA","Ecog12","depression_01","logEcog12")
DF<-as.data.frame(raw_data_averages[,ListOfVars])


colnames(DF) <- short_names

#Little tweek to assure that the statistical analysis 
#will use the right reference values when comparing factors

#CS:  I'm not getting these as factors that can be releveled based on how raw_data_averages was created
# so I had to turn them into factors to use the following code
#---- start Crystal code ----
DF$race <- as.factor(DF$race)
DF$GENDER <- as.factor(DF$GENDER)
#---- end Crystal code ----

DF$race <- relevel(DF$race, ref = "Non-Latino-White")
DF$GENDER <- relevel(DF$GENDER, ref = "Woman")

## creating a short dataset for "memory" (Statics dataframe mem)
DFmem <- DF[,c("logEcog12","memory","AGE_75_d","race","GENDER","yrEDU","F_Hist","depression_01")]

#CS: DFmem is automatically a data frame since it is derived from DF so this code is unecessary
DFmem <- data.frame(DFmem)


# creating a list of models (mem)
# estimating the models  
#and adding them to a list called memfit
mem <- list()
memfit <- list()

BASEmem <- "logEcog12 ~ memory*AGE_75_d " #it is not necessary to specify memory or age alone (main effects) because they need to be estimated anyways to get estimates for the inteaction term
interactions <- c(BASEmem, "memory*race", "memory*GENDER","memory*yrEDU + memory*race","memory*F_Hist","memory*depression_01","memory*yrEDU")

memfit[[1]] <- lm(data = DFmem,formula = BASEmem)



mem[[1]] <-paste(BASEmem, " + ", interactions[2])
mem[[2]] <-paste(BASEmem, " + ", interactions[3])
mem[[3]] <-paste(BASEmem, " + ", interactions[4])
mem[[4]] <-paste(BASEmem, " + ", interactions[5])
mem[[5]] <-paste(BASEmem, " + ", interactions[6])
mem[[6]] <-paste(BASEmem, " + ", interactions[7])

#adding to memfit
for(n in 1:6){
memfit[[n+1]]<- lm(data = DFmem, formula = mem[[n]])
}

#creating a result table
TwoVarMem <-tab_model(memfit, title = "Relation between ECog and memory", 
          auto.label = T,
          dv.labels = c("Modifier: age","Modifier race/ethnicity","Modifier: Gender","Modifier Education (+ race))","modifier: Family History","Modifier: depression","Modifier: Education"),
          digits = 3,
          ci.hyphen = ", ",
          file = "./Rmarkdown_scripts_and_outputs/Formated_regression_tables/2variable_models_memory.html")

TwoVarMem


# ## @knitr stepwise-model-memory  
# fullModel <- paste0(interactions, collapse = " + ")
# stepmodelmem <- MASS::stepAIC(BASEmod_mem, direction = "forward",scope = list(upper = fullModel, lower = BASEmod_mem))
# summary(stepmodelmem)
##########################################################################################################################

##########################################################################################################################


##########################################################################################################################

##########################################################################################################################
## @knitr executive function
### similarily for ex_function but with a knot at ex_function = 1


DFex <-  DF[,c("logEcog12","ex_function","AGE_75_d","race","GENDER","yrEDU","F_Hist","depression_01")]

DF1<-na.omit(DFex,cols=c("ex_function","logEcog12","AGE_75_d"))
BASEex <- "logEcog12 ~ I(lspline(ex_function, knots =1))*AGE_75_d " #it is not necessary to specify memory or age alone because they need to be estimated anyways to get estimates for the inteaction term
interactionsex <- c(BASEex, "I(lspline(ex_function, knots =1))*race", "I(lspline(ex_function, knots =1))*GENDER","I(lspline(ex_function, knots =1))*yrEDU + I(lspline(ex_function, knots =1))*race","I(lspline(ex_function, knots =1))*F_Hist","I(lspline(ex_function, knots =1))*depression_01","I(lspline(ex_function, knots =1))*yrEDU")

results_execfun<-list()

results_execfun[[1]] <- lm(BASEex,data = DF1)

#make a list of model formulas
modlistex<-list()
for(t in 1:6){
  print(t)
  modlistex[[t]] <- paste0(BASEex, " + ", interactionsex[t+1])  
}


#estimate the models and store them in results_execfun
for (i in 1:length(modlistex)) {
  model<- glm(data = DFex, formula = modlistex[[i]])
  results_execfun[[i+1]] <- model
}

#creating a result table
TwoVarEx<-tab_model(results_execfun, title = "Relation between ECog and executive function",
          auto.label = T,
          dv.labels = c("Modifier: AGE","Modifier: Race/Ethnicity","Modifier: GENDER","Modifier: Education", "Modifier: Family history","Modifier: depression","Modifier: education"),
          digits = 3,
          ci.hyphen = ", ",
          file = "./Rmarkdown_scripts_and_outputs/Formated_regression_tables/2variable_models_executive.html")
TwoVarEx




#*****************************************************************************
#*****************************************************************************
#*****************************************************************************
#*****************************************************************************
#*****************************************************************************
#*****************************************************************************
#*****************************************************************************
#HERE WE RUN THE SAME MODELS WITHOUT THE KNOT AT 1 TO CHECK IF THERE IS A BIG DIFFERENCE IN INTERPRETABILTY

DFex <-  DF[,c("logEcog12","ex_function","AGE_75_d","race","GENDER","yrEDU","F_Hist","depression_01")]


BASEex_nospline <- "logEcog12 ~ ex_function*AGE_75_d " #it is not necessary to specify memory or age alone because they need to be estimated anyways to get estimates for the inteaction term
interactions_ex_nospline <- c(BASEex_nospline, "ex_function*race", "ex_function*GENDER","ex_function*yrEDU + ex_function*race","ex_function*F_Hist","ex_function*depression_01","ex_function*yrEDU")

results_execfun_nospline<-list()
results_execfun_nospline[[1]] <- lm(BASEex_nospline,data = DFex)
basetabex_nospline<-tab_model(results_execfun_nospline[[1]], title = "Relation between ECog and executive function", 
                     auto.label = T,
                     dv.labels = "Base model logEcog",
                     digits = 3,
                     ci.hyphen = ", ",
                     file = "./Rmarkdown_scripts_and_outputs/Formated_regression_tables/Base_model_executive_nospline.html")
basetabex_nospline

modlistex_nospline <- list()
for(t in 1:6){
  print(t)
  modlistex_nospline[[t]] <- paste0(BASEex_nospline, " + ", interactions_ex_nospline[t+1])  
}


#CS: Can you check the output table here and see if you get the columns mislabelled as I'm seeing?
# I'm not able to run this code because I can't install "sjPlot" =/ so I didn't know if you had 
# fixed this already and I just couldn't run the new version (though I think the
# code below isn't fixed yet based on the ordering of dv.labels)

for (i in 1:length(modlistex)) {
  model<- lm(data = DFex, formula = modlistex_nospline[[i]])
  results_execfun_nospline[[i+1]] <- model
}
TwoVarEx_NS<-tab_model(results_execfun_nospline, title = "Relation between ECog and executive function",
                    auto.label = T,
                    dv.labels = c("Base + Race/Ethnicity","Base + GENDER","Base + Education + race", "Base + Family history","Base + depression","Base + Education"),
                    digits = 3,
                    ci.hyphen = ", ",
                    file = "./Rmarkdown_scripts_and_outputs/Formated_regression_tables/2variable_models_executive_nospline.html")
TwoVarEx_NS




