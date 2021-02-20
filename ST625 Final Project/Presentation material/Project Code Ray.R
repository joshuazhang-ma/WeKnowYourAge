data1=read.table("/Users/rayray/Desktop/ST625/termlife.txt",header=T)
data1

survey = read.table("/Users/rayray/Desktop/ST625/Dataset for project/young-people-survey/responses.csv")

install.packages("readr")
library(readr)
resp onses = read_csv("/Users/rayray/Desktop/ST625/Dataset for project/young-people-survey/responses.csv")
summary(onses)
View(responses)
############################################################
############################################################
#first run, 149 variables,none variables deleted
#Multiple R-squared:  0.6472,	Adjusted R-squared:  0.5371
############################################################
############################################################
project = read_csv("/Users/rayray/Desktop/ST625/Project/surveyclean.csv")
summary(project)
reg1=lm(Age~.,data=project)
summary(reg1)
library(olsrr)
library(leaps)
stepwisereg = ols_step_both_p(reg)



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

############################################################
############################################################
weight2 = project[,143]
weight
lol22=weight2 * weight2
lol22


summary(project_weight2)
summary(regweight2)
names(project_weight2)[151] <- "NewName"
############################################################
############################################################
project_change4 = read.table("/Users/rayray/Desktop/ST625/Project/30+selected_variable.txt", header = T)

weight_2 = project_change4[,3]
weight_2
weightsqrt = weight_2* weight_2
weightcube=weight_2* weight_2* weight_2
project_change4_cbind = cbind(project_change4,weightsqrt,weightcube)

names(project_change4_cbind)[40] <-"Weightsqrt"
names(project_change4_cbind)[41] <-"Weightcube"
summary(project_change4_cbind)
summary(lm(Age~.,data=project_change4))
summary(lm(Age~.,data=project_change4_cbind))
############################################################
############################################################

reg4=lm(Age~.,data=project_change4)
summary(reg4)
stepwisereg2 = ols_step_both_p(reg4)
anova(reg4)
age_project4=project_change4[,141]
age_project4
var(Age,data=project_change4)
project_change5 = read_excel("/Users/rayray/Desktop/ST625/Project/30+selected_variable_delete12.xlsx",sheet="Sheet1")
summary(project_change4)
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
