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
DF$race <- relevel(DF$race, ref = "Non-Latino-White")
DF$GENDER <- relevel(DF$GENDER, ref = "Woman")

## creating a short dataset for "memory" (Statics dataframe mem)

DFmem <- DF[,c("logEcog12","memory","AGE_75_d","race","GENDER","yrEDU","F_Hist","depression_01")]
DFmem <- data.frame(DFmem)

BASEmem <- "logEcog12 ~ memory*AGE_75_d " #it is not necessary to specify memory or age alone because they need to be estimated anyways to get estimates for the inteaction term
interactions <- c(BASEmem, "memory*race", "memory*GENDER","memory*yrEDU + memory*race","memory*F_Hist","memory*depression_01","memory*yrEDU")
length(interactions)

BASEmod_mem <- lm(data = DFmem,formula = BASEmem)


baseTabmem <- tab_model(BASEmod_mem,
          title = "Relation between ECog and memory", 
          auto.label = T,
          dv.labels = "Base model logEcog",
          digits = 3,
          #pred.labels = c("(Intercept)","memory","Age_centered_75_decades","language(Spanish)","memory:Age_cent_75_decades"),
          file = "/Users/fcorlier/Box/Fabian_ERM_Box/Khandle_data_analysis_FABIAN/R_KHANDLE_project_data/Rmarkdown_scripts_and_outputs/Formated_regression_tables/Base_model_memory.html")
#save_kable(base_tbl_mem,"/Users/fcorlier/Box Sync/Khandle_data_analysis_FABIAN/R_KHANDLE_project_data/Base_model_memory.html")
baseTabmem

mem <- list()

mem[[1]] <-paste(BASEmem, " + ", interactions[2])
mem[[2]] <-paste(BASEmem, " + ", interactions[3])
mem[[3]] <-paste(BASEmem, " + ", interactions[4])
mem[[4]] <-paste(BASEmem, " + ", interactions[5])
mem[[5]] <-paste(BASEmem, " + ", interactions[6])
mem[[6]] <-paste(BASEmem, " + ", interactions[7])

# estimating the 5 models
memfit <- list()
memfit[[1]]<- lm(data = DFmem, formula = mem[[1]])
memfit[[2]]<- lm(data = DFmem, formula = mem[[2]])
memfit[[3]]<- lm(data = DFmem, formula = mem[[3]])
memfit[[4]]<- lm(data = DFmem, formula = mem[[4]])
memfit[[5]]<- lm(data = DFmem, formula = mem[[5]])
memfit[[6]]<- lm(data = DFmem, formula = mem[[6]])

TwoVarMem <-tab_model(memfit, title = "Multi model table memory", 
          auto.label = T,
          dv.labels = c("Base + race","base + Gender","Base + Education + race","Base + Family History","Base + depression","Base+Education"),
          digits = 3,
          ci.hyphen = ", ",
          # pred.labels = c("(Intercept)","memory","Age_centered_at75 (decades)","language(Spanish)",
          #                 "Asian","Black","Latino","memory:Age_centered_at75",
          #                 "memory:Asian","memory:Black","memory:Latino",
          #                 "Gender(male)","memory:Gender(male)","Education (yrs) centered at 12","memory:educaton",
          #                 "Family_history","memory:Family_history","depression","memory:depression"),
          file = "/Users/fcorlier/Box/Fabian_ERM_Box/Khandle_data_analysis_FABIAN/R_KHANDLE_project_data/Rmarkdown_scripts_and_outputs/Formated_regression_tables/2variable_models_memory.html")

TwoVarMem

fullModel <- paste0(interactions, collapse = " + ")
fullModelfit <- lm(fullModel,data = DFmem)

fullmem <- tab_model(fullModelfit, title = "Relation between ECog and memory all covariates", 
          auto.label = T,
          dv.labels = "Full model",
          digits = 3,
          ci.hyphen = ", ",
          # pred.labels = c("(Intercept)","memory","Age_centered_dec","language(Spanish)",
          #                 "Asian","Black","Latino","Gender(male)","Education_centered_at12","Family_history","depression",
          #                 "memory:Age_centered",
          #                 "memory:Asian","memory:Black","memory:Latino",
          #                 "memory:Gender(male)","memory:Educaton",
          #                 "memory:Family_history","memory:depression"),
          file = "/Users/fcorlier/Box/Fabian_ERM_Box/Khandle_data_analysis_FABIAN/R_KHANDLE_project_data/Rmarkdown_scripts_and_outputs/Formated_regression_tables/full_model_memory.html")
fullmem
# ## @knitr stepwise-model-memory  
# stepmodelmem <- MASS::stepAIC(BASEmod_mem, direction = "forward",scope = list(upper = fullModel, lower = BASEmod_mem))
# #summary(stepmodelmem)
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


BASEmod_ex <- lm(BASEex,data = DF1)
basetabex<-tab_model(BASEmod_ex, title = "Relation between ECog and executive function", 
          auto.label = T,
          dv.labels = "Base model logEcog",
          digits = 3,
          ci.hyphen = ", ",
          #pred.labels = c("(Intercept)",
          #"Executive function <1","Executive function >1",
          #"AGE_centered_75_decades","Executive function (<1):AGE","Ececutive funtion (>1):AGE"),
          file = "/Users/fcorlier/Box/Fabian_ERM_Box/Khandle_data_analysis_FABIAN/R_KHANDLE_project_data/Rmarkdown_scripts_and_outputs/Formated_regression_tables/Base_model_executive.html")
basetabex

modlistex <- list()
for(t in 1:6){
  print(t)
  modlistex[[t]] <- paste0(BASEex, " + ", interactionsex[t+1])  
}


results_execfun<-list()
for (i in 1:length(modlistex)) {
  model<- lm(data = DFex, formula = modlistex[[i]])
  results_execfun[[i]] <- model
}
TwoVarEx<-tab_model(results_execfun, title = "Relation between ECog and executive function",
          auto.label = T,
          dv.labels = c("Base + Race/Ethnicity","Base + GENDER","Base + Education + race", "Base + Family history","Base + depression","Base + Education"),
          digits = 3,
          ci.hyphen = ", ",
          # pred.labels = c("(Intercept)","Executive function <1","Executive function >1","AGE_centered at 75 by decade","Asian","Black","Latino",
          #                 "Executive function (<1):AGE","Ececutive funtion (>1):AGE",
          #                 "Executive function (<1):Asian","Ececutive funtion (>1):Asian",
          #                 "Executive function (<1):Black","Ececutive funtion (>1):Black",
          #                 "Executive function (<1):Latino","Ececutive funtion (>1):Latino",
          #                 "GENDER(Male)","Executive function (<1):GENDER(Male)","Ececutive funtion (>1):GENDER(Male)",
          #                 "YearsEDUCATION","Executive function (<1):YearsEDUCATION","Ececutive funtion (>1):YearsEDUCATION",
          #                 "FamilyHISTORY","Executive function (<1):FamilyHISTORY","Ececutive funtion (>1):FamilyHISTORY",
          #                 "depression","Executive function (<1):depression","Ececutive funtion (>1):depression"),
          file = "/Users/fcorlier/Box/Fabian_ERM_Box/Khandle_data_analysis_FABIAN/R_KHANDLE_project_data/Rmarkdown_scripts_and_outputs/Formated_regression_tables/2variable_models_executive.html")
TwoVarEx


## @knitr full-model-executive-function  
Fullmodelex <- glm(formula = paste0(interactionsex, collapse = " + "),data = DFex )
fulltabEX<-tab_model(Fullmodelex, title = "Relation between ECog and executive function",          auto.label = T,
          dv.labels = c("full Model"),
          digits = 3,
          ci.hyphen = ", ",
          # pred.labels = c("(Intercept)",
          #                 "Executive function <1",
          #                 "Executive function >1",
          #                 "AGE_centered at 75 by decade",
          #                 "language(Spanish)",
          #                 "Asian",
          #                 "Black",
          #                 "Latino",
          #                 "GENDER(Male)",
          #                 "YearsEDUCATION",
          #                 "FamilyHISTORY",
          #                 "depression",
          #                 "Executive function (<1):AGE","Ececutive funtion (>1):AGE",
          #                 "Executive function (<1):Asian","Ececutive funtion (>1):Asian",
          #                 "Executive function (<1):Black","Ececutive funtion (>1):Black",
          #                 "Executive function (<1):Latino","Ececutive funtion (>1):Latino",
          #                 "Executive function (<1):GENDER(Male)","Ececutive funtion (>1):GENDER(Male)",
          #                 "Executive function (<1):YearsEDUCATION","Ececutive funtion (>1):YearsEDUCATION",
          #                 "Executive function (<1):FamilyHISTORY","Ececutive funtion (>1):FamilyHISTORY",
          #                 "Executive function (<1):depression","Ececutive funtion (>1):depression"),
          file = "/Users/fcorlier/Box/Fabian_ERM_Box/Khandle_data_analysis_FABIAN/R_KHANDLE_project_data/Rmarkdown_scripts_and_outputs/Formated_regression_tables/full_model_executive.html")                     
fulltabEX

# ## @knitr stepwise-model-executive-function  
#  stepmodelex <- MASS::stepAIC(BASEmod_ex, direction = "forward",scope = list(upper = Fullmodelex, lower = BASEmod_ex),trace = F)
#  summary(Fullmodelex)

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


BASEmod_ex_nospline <- lm(BASEex_nospline,data = DFex)
basetabex_nospline<-tab_model(BASEmod_ex_nospline, title = "Relation between ECog and executive function", 
                     auto.label = T,
                     dv.labels = "Base model logEcog",
                     digits = 3,
                     ci.hyphen = ", ",
                     file = "/Users/fcorlier/Box/Fabian_ERM_Box/Khandle_data_analysis_FABIAN/R_KHANDLE_project_data/Rmarkdown_scripts_and_outputs/Formated_regression_tables/Base_model_executive_nospline.html")
basetabex_nospline

modlistex_nospline <- list()
for(t in 1:6){
  print(t)
  modlistex_nospline[[t]] <- paste0(BASEex_nospline, " + ", interactions_ex_nospline[t+1])  
}


results_execfun_nospline<-list()
for (i in 1:length(modlistex)) {
  model<- lm(data = DFex, formula = modlistex_nospline[[i]])
  results_execfun_nospline[[i]] <- model
}
TwoVarEx_NS<-tab_model(results_execfun_nospline, title = "Relation between ECog and executive function",
                    auto.label = T,
                    dv.labels = c("Base + Race/Ethnicity","Base + GENDER","Base + Education + race", "Base + Family history","Base + depression","Base + Education"),
                    digits = 3,
                    ci.hyphen = ", ",
                    file = "/Users/fcorlier/Box/Fabian_ERM_Box/Khandle_data_analysis_FABIAN/R_KHANDLE_project_data/Rmarkdown_scripts_and_outputs/Formated_regression_tables/2variable_models_executive_nospline.html")
TwoVarEx_NS


## @knitr full-model-executive-function  
Fullmodelex_NS <- glm(formula = paste0(interactions_ex_nospline, collapse = " + "),data = DFex )
fulltabEX_NS<-tab_model(Fullmodelex_NS, title = "Relation between ECog and executive function",          auto.label = T,
                     dv.labels = c("full Model"),
                     digits = 3,
                     ci.hyphen = ", ",
                     file = "/Users/fcorlier/Box/Fabian_ERM_Box/Khandle_data_analysis_FABIAN/R_KHANDLE_project_data/Rmarkdown_scripts_and_outputs/Formated_regression_tables/full_model_executive_nospline.html")                     
fulltabEX_NS

