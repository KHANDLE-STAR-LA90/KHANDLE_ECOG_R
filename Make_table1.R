## @knitr Prepare_Data
# 
#  library(Hmisc)
#  library(Gmisc) #this is the one that makes table 1

#an alternative is : https://cran.r-project.org/web/packages/table1/vignettes/table1-examples.html

 # ====> skip to line 166
  
 
#---Creating functions 
##############################FUNCTIONS#########################

##function to extract Mean(sd)
getTable1Stats <- function(x, digits = 1, plusmin_str, ...){
  getDescriptionStatsBy(x = x, 
                        by = DF$all,
                        digits = digits,
                        continuous_fn = describeMean_fabian,
                        header_count = T,percentage_sign = F,show_all_values=F,plusmin_str=T,
                        ...)
  
}
describeMean_fabian <- function (x, html = TRUE, digits = 1, digits.nonzero = NA, number_first = TRUE, 
          useNA = c("ifany", "no", "always"), useNA.digits = digits, 
          percentage_sign = TRUE, plusmin_str, language = "en",...){
  dot_args <- list(...)
  if ("show_missing_digits" %in% names(dot_args)) {
    useNA.digits <- dot_args$show_missing_digits
    dot_args$show_missing_digits <- NULL
    warning("Deprecated: show_missing_digits argument is now useNA.digits as of ver. 1.0")
  }
  if ("show_missing" %in% names(dot_args)) {
    if (missing(useNA)) {
      useNA <- convertShowMissing(dot_args$show_missing)
    }
    dot_args$show_missing <- NULL
    warning("Deprecated: show_missing argument is now useNA as of ver. 1.0")
  }
  useNA <- match.arg(useNA)
  if (missing(plusmin_str)) 
    if (html) 
      plusmin_str <- "&plusmn;"
  else plusmin_str <- "\\pm"
  ret <- c(sprintf("%s (%s%s)", txtRound(mean(x, na.rm = T), 
                                         digits = digits, digits.nonzero = digits.nonzero), "",
                   txtRound(sd(x, na.rm = T), digits = digits, digits.nonzero = digits.nonzero)))
  if (html == FALSE) 
    ret <- sprintf("$%s$", ret)
  if (useNA %in% c("ifany", "always") & sum(is.na(x)) > 
      0) {
    ret <- rbind(ret, descGetMissing(x = x, html = html, 
                                     number_first = number_first, percentage_sign = percentage_sign, 
                                     language = language, useNA.digits = useNA.digits, 
                                     digits.nonzero = digits.nonzero, dot_args = dot_args))
    rownames(ret) <- c("Mean (SD)", "Missing")
  }
  else if (useNA == "always") {
    if (percentage_sign == TRUE) 
      percentage_sign <- ifelse(html, "%", "\\%")
    else if (is.character(percentage_sign) == FALSE) 
      percentage_sign = ""
    empty <- sprintf(ifelse(number_first, "0 (0%s)", 
                            "0%s (0)"), percentage_sign)
    ret <- rbind(ret, rep(empty, times = NCOL(ret)))
    rownames(ret) <- c("Mean (SD)", "Missing")
  }
  else {
    names(ret) <- "Mean (SD)"
  }
  return(ret)
}

##same function to generate Median (min-max)
getTable1MedianStats <- function(x, digits = 1, ...){
  getDescriptionStatsBy(x = x, 
                        by = DF$all,
                        digits = digits,
                        continuous_fn = describeMedian,
                        header_count = TRUE,percentage_sign = F,
                        plusmin_str,NEJMstyle = T,
                        ...)
  
}

################SPECIAL FUNCTION TO PRINT BOTH: MEAN AND MEDIAN################

##same function to generate Median (min-max) and Mean (SD)
getTable1Stats_both <- function(x, digits = 1, ...){
  getDescriptionStatsBy(x = x, 
                        by = DF$all,
                        digits = digits,
                        continuous_fn = describeBoth,
                        header_count = TRUE,percentage_sign=F,
                        ...)
  
}


describeBoth <- function (x, iqr = F, html = TRUE, digits = 1, digits.nonzero = NA,
          number_first = TRUE, useNA = c("ifany", "no", 
                                         "always"), useNA.digits = digits, percentage_sign = TRUE, 
          language = "en", ...){
  dot_args <- list(...)
  if ("show_missing_digits" %in% names(dot_args)) {
    show_missing.digits <- dot_args$show_missing_digits
    dot_args$show_missing_digits <- NULL
    warning("Deprecated: show_missing_digits argument is now show_missing.digits as of ver. 1.0")
  }
  if ("show_missing" %in% names(dot_args)) {
    if (missing(useNA)) {
      useNA <- convertShowMissing(dot_args$show_missing)
    }
    dot_args$show_missing <- NULL
    warning("Deprecated: show_missing argument is now useNA as of ver. 1.0")
  }
  useNA <- match.arg(useNA)
  if (missing(plusmin_str)) 
    if (html) 
      plusmin_str <- "&plusmn;"
  else plusmin_str <- "\\pm"
  if (iqr) 
    range_quantiles = c(1/4, 3/4)
  else range_quantiles = c(0, 1)
  ret <- sprintf("%s (%s - %s)", txtRound(median(x, na.rm = TRUE), 
                                          digits = digits, digits.nonzero = digits.nonzero), txtRound(quantile(x, 
                                                                                                               probs = range_quantiles[1], na.rm = TRUE), digits = digits, 
                                                                                                      digits.nonzero = digits.nonzero), txtRound(quantile(x, 
                                                                                                                                                          probs = range_quantiles[2], na.rm = TRUE), digits = digits, 
                                                                                                                                                 digits.nonzero = digits.nonzero))
  
  ret <- rbind(ret, c(sprintf("%s (%s%s)", txtRound(mean(x, na.rm = T), 
                                                           digits = digits, digits.nonzero = digits.nonzero), plusmin_str, 
                                     txtRound(sd(x, na.rm = T), digits = digits, digits.nonzero = digits.nonzero))))
  
  if (useNA %in% c("ifany", "always") & sum(is.na(x)) > 
      0) {
    ret <- rbind(ret, descGetMissing(x = x, html = html, 
                                     number_first = number_first, percentage_sign = percentage_sign, 
                                     language = language, useNA.digits = useNA.digits, 
                                     digits.nonzero = digits.nonzero, dot_args = dot_args))
    rownames(ret) <- c(ifelse(iqr, "Median (IQR)", 
                              "Median (range)"),"Mean (SD)", "Missing")
  }
  else if (useNA == "always") {
    if (percentage_sign == TRUE) 
      percentage_sign <- ifelse(html, "%", "\\%")
    else if (is.character(percentage_sign) == FALSE) 
      percentage_sign = ""
    empty <- sprintf(ifelse(number_first, "0 (0%s)", 
                            "0%s (0)"), percentage_sign)
    ret <- rbind(ret, c(sprintf("%s (%s%s)", txtRound(mean(x, na.rm = T), 
                                                      digits = digits, digits.nonzero = digits.nonzero), plusmin_str, 
                                txtRound(sd(x, na.rm = T), digits = digits, digits.nonzero = digits.nonzero))))
    ret <- rbind(ret, rep(empty, times = NCOL(ret)))
    rownames(ret) <- c(ifelse(iqr, "Median (IQR)", 
                              "Median (range)"),"Mean (SD)", "Missing")
  }
  else {
    rownames(ret) <- c(ifelse(iqr, "Median (IQR)", "Median (range)"),"Mean (SD)")
  }
  return(ret)
}

####################################################################################################3
###########   END  OF FUNCTIONS  ##############################
#----

DF<-as.data.frame(DF)

DF$Age90 <- rep("FALSE",nrow(DF))#by pre-allocating this collumn with "FALSE" 
#I make sure the next test puts a "TRUE" in those of age==90 and leaves no NA
#(which otherwise get accounted as missing in the "gtTable1stats" function 
# so that you'd get 2 levels nTRUE=x,nFALSE=y,and missing=z ) when I actually just want n"TRUE"(% of the 1712 total)
DF$Age90 <- factor(DF$age>=89.9,levels = c("TRUE","FALSE"))
DF$GENDER <- factor(DF$GENDER, levels = c("Woman","Man"))
DF$RELATIVE_DEMENTIA<- factor(DF$RELATIVE_DEMENTIA,levels = c(1,0))

DF$all<-rep("Total",nrow(DF)) 
#this line is critical because the function 'sorts' by a variable, if you have 
#just one group with everyone you have to add a constant. (could be anything but
#since it gets printed in the top of your table you don't want "foo" or "boo")

#### Constructing the table1
# Here we make one line at a time in a list (called T1)
# this way we can display both description methods (Mean [sd] and Median [min-max])

## @knitr Table1

t1 <- list()

#CS:  I can't run this code from this point on because it can't find the "getDescriptionStatsBy function =/
t1[["Age"]] <-getTable1Stats(DF$age,digits = 1)
t1[["No. age 90+"]]<- getTable1Stats(DF$Age90,useNA="no")
t1[["Race/Ethnicity"]]<-getTable1Stats(DF$race)
t1[["Gender (Women)"]] <-getTable1Stats(DF$GENDER) #or getTable1Stats_both(DF$GENDER)
t1[["Years of Education"]] <- getTable1Stats(DF$yrEDUCATION,digits = 1) #or getTable1Stats_both(DF$yrEDUCATION)
t1[["Depressive symptoms"]] <- getTable1Stats(DF$depression_01) #getTable1Stats_both(DF$nihtlbx_TScore)
t1[["Family history of dementia"]]<-getTable1Stats(DF$RELATIVE_DEMENTIA)
t1[["Episodic memory score"]]<- getTable1Stats(DF$adj_verbal_episodic_mem) #getTable1Stats_both(DF$adj_verbal_episodic_mem)
t1[["Executve function score"]]<-getTable1Stats(DF$executive_function) #getTable1Stats_both(DF$executive_function)
t1[["ECog score"]] <-getTable1Stats(DF$Ecog12_including_partial_averages)
    #getTable1Stats_both(DF$Ecog12_including_partial_averages)
t1[["log(ECog) score"]]<-getTable1Stats(DF$logEcog12)

#and finally make the table1
mergeDesc(c(t1),
          htmlTable_args = list(css.rgroup = "",
                                caption  = ""))

###second table that inspects the ECOG individual items
## @knitr Table2


t2<-list()
for (i in c(Ecog_12items,"Ecog12_missing","Ecog12_including_partial_averages")) {
 t2[[i]] <-
   getTable1Stats(DF[,i])
}
mergeDesc(t2,
          htmlTable_args = list(css.rgroup = "",
                                caption  = "Detail of the Ecog scale"))

