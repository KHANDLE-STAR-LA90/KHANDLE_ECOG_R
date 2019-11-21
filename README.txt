Fabian's code README file:
The files included in this code review are:
The markdown script that sequentially calls all the other scripts:
0.	KHANDLE_ECOG_effect_modifier_project.rmd
The other scripts (called by markdown) are:
1.	Khandle_data_engineering.R-- DONE.  1 tiny comment... search for "CS:"
Imports the SAS datadase with heaven, cleans the data by removing NAs, renaming variables of 
interest and renaming levels within factors (e.g. "yes/no" instead of "1/2") for the latter I refer 
to the data dictionary provided by Kaiser's data analyst (Dil). 
@Crystal: 
       both the data and the data dictionary (.xls file) are in the box folder shared with you 
Khandle_coded_data/Raw_data_tables/W1/transfer4_2019Oct18/
       Do NOT review lines 125-200
	Next sections: 
       a. convert education to years of education
       b. Creates the variable "family history of dementia" by looking at PARENTS, SIBLIGS, ANY 
RELATIVE
       c. Counts how many of the 12 items in the ECog scale are missing per subject and 
compute averages. And makes a log-transformaion
       d. center age at 75 years and convert to decades
       e. filter missing data by, log(ECog), then adj_verbal_episodic_mem, then race, then 
education, then depressive symptoms
       f. add those derived variables to two datasets DF (which is a simplified data table with 
just the usefull variables) and raw_data_averages that contains all the variables.
       g. save only the latter data table

2.	The script "make_table1.R" basically makes the table 1-- DONE... one small comment
It basically plays with the functions in the package "Gmisc" (some of which I had to change a 
little and copied them to the script).
Real script starts at line 165.
It makes a list of variables to show in the table 1 and makes the render as html table.
3.	The script "plotting_loess.R "-- DONE; I can't get the facetted plot to run
generates plots with local averages  geomsmooth(..., 
type="loess") and repeats this by using restricted cubic splines for the 3 variables from the 
SENAS scale (episodic memory, semantic memory, executive function)
4.	The script "statistical_analysis.R" -- DONE... comment about output 3 (experimental plot).  I forgot that you told me to ignore the code, sorry!
Loads the complete dataset "raw_data_averages.csv" which contains the derived variables 
created by the first script.
It them creates a string with the formula for a base model, and for additional models 
containing the base model. The formula string are stored in a list variable.
In the next step the consecutive models are estimated and the result is stored in another list 
variable.
This list of fitted models is later used in markdown (line 279) to make diagnostic plots
and in script 5. to plot the predicted values.
These steps are run twice on two different exposure variables: episodic memory(lines 23-60) 
and on executive function(lines 79-110)
The lists of fitted models are also used to create formatted .html tables with the regression 
parameters.
@crystal: you may ignore what is after line 110, as we just experimented with another 
scenario where we eliminated the knot for executive function.
5.	The script "plotting_predicted_values.R"-- DONE
Part 1:
Creates hypothetical datasets for each of the 6 effect modifiers age, race, gender, 
education, family history of dementia, depression 
For each of these, age is set to be 0 (age is centered at 75, so 0 is the average age)
Education is also controlled for race so for education, race is set to be "non-hispanic-white"
These hypothetical data points are used to predict ECog scores at those pre-set conditions 
for the 12 different models stored in memfit() and in results_execfun() respectively for 
memory and executive function (6 models for each).
Note that the hypothetical data has k*7 points with k being the number of curves I want to 
plot
Part 2:
Graphics are generated for each of the 12 models.
This part is straightforward. Lines 108-308 make the plots for memory and lines 314-507 for 
executive function.
The plots are stored in a list called "FigureList"
IMPORTANT DETAIL: due to data sparcity for age, education and depression the plots 
voluntarily show truncated versions of the predicted values. Look for a range indices in the 
first argument to each plot. 
For example for the first plot (lines 119-148): you see in line 120 
Ba_mem <- ggplot(data = newDFageM[1:20,])+
We use only 1:20 from the predicted scores for age "newDFage" because line 21 
contains predicted values at age 85, and memory >2 SD which basically has no data 
points so that the predicted value is not reliable
Similarly the range for plot 4 is range<-c(1:5,8:21) (we left lines 6 and 7 out)
For plot 7: range<-c(1:20)
For plot 8: Ra<-ggplot(data = newDFrace[-c(14,21,28),]) (I took out the last point for 3 of 4 
groups). 
For plot 10: range<-c(1:5,8:21)
For plot 12: Dpr <- ggplot(data = newDFdepr[1:20,])

Back in script 0.
Markdown makes figures with combinations of the different plots in FigureList using the 
function ggarrange() from package "ggpubr". This creates great looking figures in a plublication 
compatible resolution.
