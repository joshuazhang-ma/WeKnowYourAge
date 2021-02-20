data1=read.table("/Users/rayray/Desktop/ST625/termlife.txt",header=T)
data1

survey = read.table("/Users/rayray/Desktop/ST625/Dataset for project/young-people-survey/responses.csv")

install.packages("readr")

resp onses = read_csv("/Users/rayray/Desktop/ST625/Dataset for project/young-people-survey/responses.csv")
summary(onses)
View(responses)
############################################################
############################################################
#first run, 149 variables,none variables deleted
#Multiple R-squared:  0.6472,	Adjusted R-squared:  0.5371
############################################################
############################################################
library(readr)
project = read_csv("/Users/rayray/Desktop/ST625/Project/surveyclean.csv")
summary(project)
reg1=lm(Age~.,data=project)
summary(reg1)
library(olsrr)
library(leaps)
stepwisereg = ols_step_both_p(reg1)
anova(reg1)
PRESS(reg1)

############################################################
############################################################
#second run, 149 variables,none variables deleted
#from linear model:
#Multiple R-squared:  0.6478,	Adjusted R-squared:  0.544 
#from stepwise model:
#Adjusted R-squared:  0.575 
############################################################
############################################################
project_change3 = read_csv("/Users/rayray/Desktop/ST625/Project/surveyclean_deleted_categorized(delete 12 var from all).csv")

reg3=lm(Age~.,data=project_change3)
summary(reg3)
stepwisereg3 = ols_step_both_p(reg3)
library(tidyverse)
PRESS(reg3)



############################################################
############################################################
#Model 3, selected cell. can just run regression
#Multiple R-squared:  0.6478,	Adjusted R-squared:  0.544 
#from stepwise model:
#Adjusted R-squared:  0.575 

############################################################
############################################################
project_model3 = read_csv("/Users/rayray/Desktop/ST625/Project/Model3.csv")
summary(project_model3)
reg_model3=lm(Age~.,data=project_model3)
reg_model3
summary(reg_model3)
PRESS(reg_model3)
anova(reg_model3)
getSSR = ols_step_both_p(reg_model3)



############################################################
############################################################
#Model 4 enter interaction terms
#Reliable X Waiting
#Questionnair x spending on gadgets
#opera X gardening
############################################################
############################################################
reliable =project_model3[,3]
waiting=project_model3[,10]
questionnair = project_model3[,4]
gadgets=project_model3[,39]
opera=project_model3[,30]
gardening=project_model3[,25]
weight=project_model3[,2]
reliable_waiting=reliable * waiting
questionnair_gadget=questionnair*gadgets
opera_gardening=opera*gardening
weight_reliable=weight * reliable

project_model3_cbindfinal = 
cbind(project_model3,reliable_waiting,questionnair_gadget,opera_gardening,weight_reliable)
names(project_model3_cbindfinal)[41] <- "reliable_waiting"
names(project_model3_cbindfinal)[42] <- "questionnair_gadget"
names(project_model3_cbindfinal)[43] <- "opera_gardening"
names(project_model3_cbindfinal)[44] <- "weight_reliable"
reg_model3_cbindfinal=lm(Age~.,data=project_model3_cbindfinal)
summary(reg_model3_cbindfinal)
anova(reg_model3_cbindfinal)
PRESS(reg_model3_cbindfinal)
############################################################
############################################################

##try to do do normalization##
age= project_model3[,40]
age
hist(age)
summary(age)
 age <- as.numeric(age) 

weight2 = project[,143]
weight
lol22=weight2 * weight2
lol22


summary(project_weight2)
summary(regweight2)
names(project_weight2)[151] <- "NewName"
############################################################
############################################################
project_change4 = read.csv("/Users/rayray/Desktop/ST625/Project/30+selected_variable.csv", header = T)
project_change4
weight_2 = project_change4[,3]
weight_2
weightsqrt = weight_2* weight_2
weightcube=weight_2* weight_2* weight_2
project_change4_cbind = cbind(project_change4,weightsqrt,weightcube)

names(project_change4_cbind)[40] <-"Weightsqrt"
names(project_change4_cbind)[41] <-"Weightcube"
summary(project_change4_cbind)

############################################################
############################################################
#Press and jackknife
library(CombMSC)

age_project_change4 = project_change4[,2]
age_project_change4
hist(age_project_change4)
var(age_project_change4)
age_clean = project[,141]
age_clean
var(age_clean)
lm_testing = lm(age_clean~.,data=project)
lm_38variable = lm(Age~.,data=project_change4)
summary(lm_38variable)
PRESS(lm_38variable)

lm_38variable_weight23 = lm(Age~.,data=project_change4_cbind)
summary(lm(lm_38variable_weight23))
PRESS(lm_38variable_weight23)

############################################################
############################################################
#scattered cloud using ggpairs
install.packages("GGally")
library(GGally)
library(ggplot2)
project_testingggpairs = read.csv("/Users/rayray/Desktop/ST625/Project/30+selected_variable_reduced.csv", header = T)
ggpairs(project_testingggpairs,mapping=ggplot2::aes())
############################################################
############################################################
#Testing AIC
extractAIC(lm_38variable)
extractAIC(lm_38variable_weight23)
############################################################
#############################################################
#Testing Leaps stepwise method
library(leaps)
null = lm(Age~1,data=project_change4)
null
full = lm(Age~.,data=project_change4)
full
step(lm_38variable,scope=list(lower=null, upper=full), data=project_change4,direction="forward")
step(null, scope=list(lower=null, upper=full), ,data=a, direction="forward")


############################################################
############################################################
#testing olsrr and give detailed information
library(olsrr)
stepwise=ols_step_both_p(lm_38variable)
stepwise

stepwise=ols_step_both_p(reg)
stepwise
plot(stepwise)

############################################################
############################################################
install.packages("readxl")
library(readxl)
reg4=lm(Age~.,data=project_change4)
summary(reg4)
stepwisereg2 = ols_step_both_p(reg4)
anova(reg4)
age_project4=project_change4[,141]
age_project4
var(Age,data=project_change4)
project_change5 = read_excel("/Users/rayray/Desktop/ST625/Project/30+selected_variable_delete12.xlsx",sheet="Sheet1")
summary(project_change5)
reg5=lm(Age~.,data=project_change5)
summary(reg5)
stepwisereg5 = ols_step_both_p(reg5)
stop


############################################################
testing higher orders
############################################################



summary(project_weight2)

reg7 = lm(Age~Weight,data=project_weight2)
summary(reg7)
reg6=lm(Age~Weight*Weight*Weight,data=project_weight2)
summary(reg6)

library(ggplot2)
install.packages("ggcorrplot")
ggcorrplot(project)




project_weight2=cbind(project,lol22)
project_weight2=
regweight2=lm(Age~.,data=project_weight2)
names(project_weight2)[151] <- "NewName"
############################################################
#1.age VS weight
#2.age VS weight^2
############################################################
ggplot(project_weight2,mapping=aes(Age,Weight*Weight*Weight))+
geom_point() +geom_rug() +geom_smooth(method='lm')	


ggplot(project_weight2,mapping=aes(Age,NewName))+
geom_point() +geom_rug() +geom_smooth(method='lm')
############################################################
############################################################


ggplot(a,aes(Caratsize,Price,colour=Certification,shape=Clarity))+
geom_point()+geom_rug()+
ggtitle("Price as a function of caratsize, separated by certification and clarity")

?AIC

