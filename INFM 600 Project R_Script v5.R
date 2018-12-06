#summary tools -  provides tools to neatly and quickly summarize data as well as makes R a little easier to learn and use. Function examples:
#freq() : frequency tables with proportions
#ctable() : cross-tabulations between two factors 
#descr() : descriptive (univariate) statistics for numerical data.
#dfSummary() : Extensive data frame summaries that facilitate data cleaning.

install.packages("summarytools")
library(summarytools)

library(oce)
#DescTools - collection of miscellaneous basic statistic functions and convenience wrappers for efficiently describing data. Few functions examples:
#freq() :	Frequency Table
#mean() :	Arithmetic Mean
#median() :	Median Value

install.packages("DescTools")
library(DescTools)


#pwr - This is a power analysis function. It allows us to determine the sample size required to detect an effect of a given size with a given degree of confidence. Few function are:
#pwr.p.test() :	proportion (one sample)
#pwr.r.test() :	correlation
#pwr.t.test() :	t-tests (one sample, 2 sample, paired)

install.packages("pwr")
library(pwr)
pwr.t.test(n=63, sig.level=0.05, type = c("two.sample"), d=0.5)



#car - Companion to Applied Regression. Used for regression models.
#anova() : Anova function for statistics 
#boxplot() : boxplot fucntion for representation of data
#ggplot() : Grammar of Graphics plot of data

install.packages("car")
library(car)
library(dplyr)
#reading the csv file
Data1 = read.csv("C:/Users/abhim/Downloads/survey_results_public_original.csv",stringsAsFactors=FALSE)
View(Data1)


#Cleaning the data and getting rid of columns not to be used
Data1 <- subset(Data1,select = -c( AssessJob1 : JobEmailPriorities7))
Data1 <- subset(Data1,select = -c( SelfTaughtTypes : AgreeDisagree3))
Data1 <- subset(Data1,select = -c( CheckInCode : EthicalImplications))
Data1 <- subset(Data1,select = -c( HypotheticalTools1 : Exercise))
Data1 <- subset(Data1,select = -c( Dependents : SurveyEasy))
View(Data1)

#replacing the special characters in the dataset with logical characters
data <- data.frame(lapply(Data1, function(x) {gsub("â???T", "", x)}))
data <- data.frame(lapply(data, function(x) {gsub("Im", "I am", x)}))
View(data)
View(data)

#cheking type of variables
class(data$YearsCodingProf)
#converting factor variables to character variables
data$YearsCodingProf <- as.character(data$YearsCodingProf)
data2 <- data
View(data2$YearsCodingProf)
View(data2)
data2$YearsCodingProf = ifelse(data2$YearsCodingProf=="0-2 years",1,
                               
                               ifelse(data2$YearsCodingProf=="3-5 years",4,
                               
                                ifelse(data2$YearsCodingProf=="6-8 years",7,
                                                                           
                                        ifelse(data2$YearsCodingProf=="9-11 years",10,
                                               
                                               
                                               ifelse(data2$YearsCodingProf=="12-14 years",13,
                                                                                          
                                                      ifelse(data2$YearsCodingProf=="15-17 years",16,
                                                                                                  
                                                             ifelse(data2$YearsCodingProf=="18-20 years",19,
                                                                                                         
                                                                    ifelse(data2$YearsCodingProf=="21-23 years",22,
                                                                                                                
                                                                           ifelse(data2$YearsCodingProf=="24-26 years",25,
                                                                                                                       
                                                                                  ifelse(data2$YearsCodingProf=="27-29 years",28, 
                                                                                                                              
                                                                                         ifelse(data2$YearsCodingProf=="30 or more years",30,NA)))))))))))




data4<-data2$JobSatisfaction
View(data4)
data2$JobSatisfactionNumeric<- ifelse(data4=="Extremely satisfied",1,
                              
                              ifelse(data4=="Moderately satisfied",2,
                                     
                                     ifelse(data4=="Slightly satisfied",3,
                                            
                                            ifelse(data4=="Slightly dissatisfied",4,
                                                   
                                                   
                                                       ifelse(data4=="Moderately dissatisfied",5,
                                                                 
                                                                 ifelse(data4=="Extremely dissatisfied",6,
                                                                        
                                                                        ifelse(data4=="Neither satisfied nor dissatisfied",7,NA)))))))






View(data2)

install.packages("ggpubr")
install.packages("dplyr")
library(ggplot2)

#question 1
#plots job satisfaction with salary in dollars, "las = 2" displays they x-axis labels vertically
summary(data2$JobSatisfaction)
plot(data2$JobSatisfaction,data2$ConvertedSalary, las =2)
#tests for correlation between the variables
cor.test(data2$JobSatisfactionNumeric,data2$ConvertedSalary)

#question 2
#similar to the code above. Plotting the two variables together
plot(data2$JobSatisfaction,data2$ConvertedSalary, las =2)
#testing for corrolation
cor.test(data2$JobSatisfactionNumeric,data2$ConvertedSalary)
#testing for correlation for Job satisfaction and years coding as a profession
cor.test(data2$JobSatisfactionNumeric,data2$YearsCodingProf)
#plots the two varibles
plot(data2$JobSatisfaction,data2$YearsCodingProf, las = 2)


#question 3 (under progress)
#saves column to variable 'country'
country = dataframe$Country
#saves column to variable 'langknown'
langknown = dataframe$Number_of_languages_known

#creates a new dataframe with only the two columns
df <- data.frame(country, langknown)
df

#aggregates the dataframe and cacluates mean based on the country
df2 = aggregate(.~country, data=df, mean)

#there are a lot of countries so it fills up the x-axis
#this eliminates the ones less than 9
dataframe_1 <- subset(df2,df2$langknown > 9)
dataframe_1
#use xyplot to plot the new dataframe
library(lattice)
xyplot(dataframe_1$langknown~dataframe_1$country, xlab = "Country", ylab = "Average Number of Languages Known")


#question 4
#eliminates all NA values
na.omit(data2$YearsCodingProf)

#plots Years coding against Job search status
plot(data2$YearsCodingProf ~ data2$JobSearchStatus)

#Question 5 (under more advancement process)
data2$LastNewJob<- ifelse(data2$LastNewJob=="Between 1 and 2 years ago",2,
                                      
                                      ifelse(data2$LastNewJob=="Between 2 and 4 years ago",3,
                                             
                                             ifelse(data2$LastNewJob=="I've never had a job",0,
                                                    
                                                    ifelse(data2$LastNewJob=="Less than a year ago",1,
                                                           
                                                           
                                                           ifelse(data2$LastNewJob=="More than 4 years ago",4,NA)))))
                                                                  
                                                                  
class(data2$LastNewJob)
#plot and correlation test for the last switch and number of years of coding
plot(data2$LastNewJob,data2$YearsCodingProf)
cor.test(data2$LastNewJob,data2$YearsCodingProf)

#plot and correlation test for last switch and the present salary 
plot(data2$LastNewJob,data2$ConvertedSalary)
cor.test(data2$LastNewJob,data2$ConvertedSalary)

#plot and correlation test for last switch and age 
plot(data2$LastNewJob,data2$Age)
cor.test(data2$LastNewJob,data2$Age)


#for question 6
#histogram of the binary answers for the column StackOverflowJobs
plot(data2$StackOverflowJobs)