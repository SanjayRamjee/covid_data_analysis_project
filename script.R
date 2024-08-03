#removes any previously stored variables, if any
rm(list=ls())

#importing Hmisc library
library(Hmisc)

#reading the csv file containing COVID 19 data and assigns it to a variable
data <- read.csv("~/Documents/covid_data_analysis/COVID19_line_list_data.csv")

#summarizes data
describe(data)

#cleans up death column data; when data was described 14 values were distinct, meaning the value was not 0 or 1
data$death_dummy <- as.integer(data$death != 0)

#calculates death rate; since 1 means death, each death will be counted as 1. All of them will then be summed to calculate how many died, then divided by the number of rows, which is the number of COVID 19 cases, to show on average how many infected with the virus died.
sum(data$death_dummy) / nrow(data)
#death rate calculated to be 0.058, rounded to 3 sig figs 

#analyzing whether people who die are older
dead = subset(data, death_dummy == 1)
alive = subset(data, death_dummy == 0)

#calculates mean of cases that resulted in death, removing NA values
mean(dead$age, na.rm = TRUE) #mean calculated to be roughly 69, rounded to 2 sig figs
#calculates mean of cases that did NOT result in death, removing NA values
mean(alive$age, na.rm = TRUE) #mean calculated to be roughly 48, rounded to 2 sig figs

#we can see that the mean age of those deceased is higher than the age of those who survived.
#but we need to see whether this is statistically significant or not

#for this, we will use a t distribution using a 95% confidence interval

t.test(alive$age, dead$age, alternative = "two.sided", conf.level = 0.95)
#p value was calculated to be 2.2e-16
#null hypothesis: the difference in age is equal to 0
#alternative hypothesis: the difference in age is not equal to 0

#since the p value < 0.05, we reject the null hypothesis

#this means that our results are statistically significant since p value < 0.05 and it is ~0
#furthermore, since our interval is from -24.29 to -16.74 years, this means that 
#we are 95% sure that the difference in ages can range from around ~17 years younger 
# to ~24 years younger.  


#analyzing whether gender has an effect on death from COVID 19

men = subset(data, gender == "male")
women = subset(data, gender == "female")

mean(men$death_dummy, na.rm = TRUE) #men have death rate of 8.5%
mean(women$death_dummy, na.rm = TRUE) #women have death rate of 3.7%
#running a 99% confidence interval
#null hypothesis: gender has no effect on COVID 19 deaths
t.test(men$death_dummy, women$death_dummy, alternative = "two.sided", conf.level = 0.99)
#since our p value = 0.002, and is < 0.05, our results are statistically significant
#our 99% confidence interval indicates that men are anywhere from 0.008 to 0.088% more likely to die due to COVID 19 compared to women
#therefore, we reject the null hypothesis
