
library(xtable)
library(Hmisc)
library(MASS)
library(ggplot2)
library(foreign)
library(reshape2)

#Dependent variable - Frequency
BlaBlaCar_Data_no5$frequency_bla <- as.factor(BlaBlaCar_Data_no5$frequency_bla)
BlaBlaCar_Data_no5$frequency_bla.order <- ordered(BlaBlaCar_Data_no5$frequency_bla, levels = c(5, 4, 3, 2, 1))

#Independent vars

#Alt mode
BlaBlaCar_Data_no5$alt_mode[BlaBlaCar_Data_no5$alt_mode >= 2] <- "Car+Other"
BlaBlaCar_Data_no5$alt_mode[BlaBlaCar_Data_no5$alt_mode == 1] <- "Transit"
BlaBlaCar_Data_no5$alt_mode <- as.factor(BlaBlaCar_Data_no5$alt_mode)

#App usage
BlaBlaCar_Data_no5$app_usage <- factor(BlaBlaCar_Data_no5$app_usage <= 3)
levels(BlaBlaCar_Data_no5$app_usage)[levels(BlaBlaCar_Data_no5$app_usage)=="TRUE"] <- "Yes"
levels(BlaBlaCar_Data_no5$app_usage)[levels(BlaBlaCar_Data_no5$app_usage)=="FALSE"] <- "No"

#Profession
BlaBlaCar_Data_no5$profession <- factor(BlaBlaCar_Data_no5$profession == 8)
levels(BlaBlaCar_Data_no5$profession)[levels(BlaBlaCar_Data_no5$profession)=="TRUE"] <- "Student"
levels(BlaBlaCar_Data_no5$profession)[levels(BlaBlaCar_Data_no5$profession)=="FALSE"] <- "Non-student"

## fit ordered logit model and store results 'm'
#all
#m <- polr(frequency_bla ~ distance_short + distance_long + purpose_work + purpose_leisure + frequency_other + carpool_tenure + alt_mode + vehcommute_hh + cars_hh + role_pass + role_driver + good_alt + app_usage + gender + age + profession + education + income, data = BlaBlaCar_Data_no5,Hess=TRUE)
m <- polr(frequency_bla ~ purpose_work + alt_mode + role_driver + app_usage + profession + age + income + education, data = BlaBlaCar_Data_no5,Hess=TRUE)

## view a summary of the model
summary(m)

