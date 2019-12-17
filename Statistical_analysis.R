# here we start analysis the HKANDLE data:




#raw_data_averages <- read.csv("/Users/fcorlier/Box/Fabian_ERM_Box/Khandle_data_analysis_FABIAN/R_KHANDLE_project_data/raw_data_averages.csv")[,2:694]#saving as csv always adds a column of indices in [,1] so I load from 2 to ncol+1
ListOfVars <- c("semantic_memory","adj_verbal_episodic_mem","executive_function","Age_centered_75","Age_centered_75_decades","GENDER","race","CONCERNED_THINKING","yrEDUCATION_centered",Ecog_12items, "RELATIVE_DEMENTIA","SIBLING_DEMENTIA","PARENTAL_DEMENTIA","Ecog12_including_partial_averages","depression_01","logEcog12")
short_names <- c("semantic_memory","memory","ex_function","AGE","AGE_75_d","GENDER","race","CONCERNED_THINKING","yrEDU",Ecog_12items, "F_Hist","SIBLING_DEMENTIA","PARENTAL_DEMENTIA","Ecog12","depression_01","logEcog12")
DF<-data.frame(raw_data_averages[,ListOfVars])


colnames(DF) <- short_names

#Little tweek to assure that the statistical analysis 
#will use the right reference values when comparing factors
DF$race <- factor(DF$race)
DF$race <- relevel(DF$race, ref = "Non-Latino-White")
DF$GENDER <- factor(DF$GENDER)
DF$GENDER <- relevel(DF$GENDER, ref = "Man")

## creating a short dataset for "memory" (Statics dataframe mem)
DFmem <- DF[,c("logEcog12","memory","AGE_75_d","race","GENDER","yrEDU","F_Hist","depression_01")]
DFmem <- data.frame(DFmem)


# creating a list of models (mem)
# estimating the models  
#and adding them to a list called memfit
mem <- list()
memfit <- list()

BASEmem <- "logEcog12 ~ memory*AGE_75_d " #it is not necessary to specify memory or age alone (main effects) because they need to be estimated anyways to get estimates for the inteaction term
interactions <- c(BASEmem,  "memory*GENDER","memory*race","memory*yrEDU + race","memory*F_Hist","memory*depression_01 + GENDER","memory*yrEDU")

memfit[[1]] <- lm(data = DFmem,formula = BASEmem)



mem[[1]] <-paste(BASEmem, " + ", interactions[2])
mem[[2]] <-paste(BASEmem, " + ", interactions[3])
mem[[3]] <-paste(BASEmem, " + ", interactions[4])
mem[[4]] <-paste(BASEmem, " + ", interactions[5])
mem[[5]] <-paste(BASEmem, " + ", interactions[6])
#mem[[6]] <-paste(BASEmem, " + ", interactions[7])

#adding to memfit
for(n in 1:5){
memfit[[n+1]]<- lm(data = DFmem, formula = mem[[n]])
}


#using a simpler alternative because "sjPlot" has trouble loading consistently 
episodic_memory_table <- stargazer(memfit,type = "text",
          out = here("text_ouptput_memory.txt"),
          ci = TRUE,digits = 2,
          column.labels=c("Modifier: age","Modifier: Gender","Modifier: race/ethnicity","Modifier: Education","Modifier: Family hist.","Modifier: Depr. symptoms"),
          covariate.labels=NULL,
          no.space=TRUE,
          notes="Regression estimates by modifier, models (2-6) are all controlled for age and additionally to age, model (3) controls for race and model (6) for Gender",
          notes.append=FALSE,
          report=c("vcs"))

episodic_memory_table2 <- stargazer(memfit,
                                    type = "text",
                                   out = here("text_ouptput_memory2.txt"),
                                   ci = F,digits = 2,
                                   column.labels=c("Modifier: age","Modifier: Gender","Modifier: race/ethnicity","Modifier: Education","Modifier: Family hist.","Modifier: Depr. symptoms"),
                                   covariate.labels=NULL,
                                   no.space=TRUE,
                                   notes="Regression estimates by modifier, models (2-6) are all controlled for age and additionally to age, model (3) controls for race and model (6) for Gender",
                                   notes.append=FALSE,
                                   report=c("vcsp*"))

episodic_memory_table
episodic_memory_table2

saveRDS(memfit,file = here("regression_fit_Memory.rds")) #this saves all the regression fits

# require(sjPlot)
# #creating a result table
# TwoVarMem <-tab_model(memfit, title = "Relation between log(ECog) and memory",
#           auto.label = T,
#           dv.labels = c("Modifier: age","Modifier: Gender","Modifier: race/ethnicity","Modifier: Education","Modifier: Family hist.","Modifier: Depr. symptoms"),
#           digits = 3,
#           ci.hyphen = ", ",
#           string.ci = "95% CI",
#           show.p = T,
#           pred.labels =c("Intercept",
#                          "Memory",
#                          "Age (in decades, cetered at 75)",
#                          "Memory*Age interaction",
#                          "Female gender",
#                          "Memory*Gender interaction",
#                          "Asian",
#                          "Black",
#                          "Latino",
#                          "Memory*Asian interaction",
#                          "Memory*Black interaction",
#                          "Memory*Latino interaction",
#                          "Education (years, centered at 12)",
#                          "Memory*Education interaction",
#                          "Family history of dementia",
#                          "Memory*Family hist. interaction",
#                          "Depressive symptoms",
#                          "Memory*Depr. symptoms interaction"),
#           file = here("Rmarkdown_scripts_and_outputs","Formated_regression_tables","Formatted_model_results_memory.html"))
# 
# TwoVarMem


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
interactionsex <- c(BASEex, "I(lspline(ex_function, knots =1))*GENDER", "I(lspline(ex_function, knots =1))*race","I(lspline(ex_function, knots =1))*yrEDU + race","I(lspline(ex_function, knots =1))*F_Hist","I(lspline(ex_function, knots =1))*depression_01 + GENDER","I(lspline(ex_function, knots =1))*yrEDU")

results_execfun<-list()

results_execfun[[1]] <- lm(BASEex,data = DF1)

#make a list of model formulas
modlistex<-list()
for(t in 1:5){
  print(t)
  modlistex[[t]] <- paste0(BASEex, " + ", interactionsex[t+1])  
}


#estimate the models and store them in results_execfun
for (i in 1:length(modlistex)) {
  model<- glm(data = DFex, formula = modlistex[[i]])
  results_execfun[[i+1]] <- model
}

#same as above simpler output
executive_function_table <- stargazer(results_execfun,type = "text",
          out = here("text_ouptput_exec_fun.txt"),
          ci = T,digits = 2,
          column.labels=c("Modifier: age","Modifier: Gender","Modifier: race/ethnicity","Modifier: Education","Modifier: Family hist.","Modifier: Depr. symptoms"),
          covariate.labels=NULL,
          no.space=F,
          notes="Regression estimates by modifier, models (2-6) are all controlled for age and additionally to age, model (3) controls for race and model (6) for Gender",
          notes.append=FALSE,
          report=c("vcs"))

executive_function_table2 <- stargazer(results_execfun,type = "text",
                                      out = here("text_ouptput_exec_fun2.txt"),
                                      ci = F,digits = 2,
                                      column.labels=c("Modifier: age","Modifier: Gender","Modifier: race/ethnicity","Modifier: Education","Modifier: Family hist.","Modifier: Depr. symptoms"),
                                      covariate.labels=NULL,
                                      no.space=TRUE,
                                      notes="Regression estimates by modifier, models (2-6) are all controlled for age and additionally to age, model (3) controls for race and model (6) for Gender",
                                      notes.append=FALSE,
                                      report=c("vcsp*"))
executive_function_table
executive_function_table2
saveRDS(results_execfun,file = here("regression_fit_EXFUN.rds")) #this saves all the regression fits

#creating a result table
# TwoVarEx<-tab_model(results_execfun, title = "Relation between log(ECog) and executive function",
#           auto.label = T,
#           dv.labels = c("Modifier: AGE","Modifier: GENDER","Modifier: Race/Ethnicity","Modifier: Education", "Modifier: Family history","Modifier: depression"),
#           digits = 3,
#           ci.hyphen = ", ",
#           show.p = F,
#           string.ci = "95% CI",
#           pred.labels =c("Intercept",
#                          "Executive fun.(<1)",
#                          "Executive fun.(>1)",
#                          "Age (in decades, cetered at 75)",
#                          "Executive fun.(<1)*Age interaction",
#                          "Executive fun.(>1)*Age interaction",
#                          "Female gender",
#                          "Executive fun.(<1)*Gender interaction",
#                          "Executive fun.(>1)*Gender interaction",
#                          "Asian",
#                          "Black",
#                          "Latino",
#                          "Executive fun.(<1)*Asian interaction",
#                          "Executive fun.(>1)*Asian interaction",
#                          "Executive fun.(<1)*Black interaction",
#                          "Executive fun.(>1)*Black interaction",
#                          "Executive fun.(<1)*Latino interaction",
#                          "Executive fun.(>1)*Latino interaction",
#                          "Education (years, centered at 12)",
#                          "Executive fun.(<1)*Education interaction",
#                          "Executive fun.(>1)*Education interaction",
#                          "Family history of dementia",
#                          "Executive fun.(<1)*Family hist interaction",
#                          "Executive fun.(>1)*Family hist interaction",
#                          "Depressive symptoms",
#                          "Executive fun.(<1)*Depr. symptoms interaction",
#                          "Executive fun.(>1)*Depr. symptoms interaction"),
#           file = here("Rmarkdown_scripts_and_outputs","Formated_regression_tables","Formatted_model_results_executive.html"))
# TwoVarEx
# 



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
# basetabex_nospline<-tab_model(results_execfun_nospline[[1]], title = "Relation between ECog and executive function", 
#                      auto.label = T,
#                      dv.labels = "Base model logEcog",
#                      digits = 3,
#                      ci.hyphen = ", ",
#                      file = here("Rmarkdown_scripts_and_outputs","Formated_regression_tables","Base_model_executive_nospline.html"))
# basetabex_nospline

modlistex_nospline <- list()
for(t in 1:6){
  print(t)
  modlistex_nospline[[t]] <- paste0(BASEex_nospline, " + ", interactions_ex_nospline[t+1])  
}



for (i in 1:length(modlistex)) {
  model<- lm(data = DFex, formula = modlistex_nospline[[i]])
  results_execfun_nospline[[i+1]] <- model
}
# TwoVarEx_NS<-tab_model(results_execfun_nospline, title = "Relation between ECog and executive function",
#                     auto.label = T,
#                     dv.labels = c("Base + Race/Ethnicity","Base + GENDER","Base + Education + race", "Base + Family history","Base + depression","Base + Education"),
#                     digits = 3,
#                     ci.hyphen = ", ",
#                     file = here("Rmarkdown_scripts_and_outputs","Formated_regression_tables","2variable_models_executive_nospline.html"))
# TwoVarEx_NS




