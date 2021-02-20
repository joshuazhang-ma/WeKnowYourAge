library(CombMSC)
library(olsrr)
library(readxl)

surveytemp <- lm(Age~., data = responses)
stepwise <- ols_step_both_p(surveytemp, detail = TRUE)
stepwise
plot(stepwise)

### All variables
setwd("~/Dropbox/Bentley/Spring 2019/ST625/Term Project/SURVEY")
all <- read_csv("responses.csv")
model_all <- lm(Age~., data = all)
summary(model_all)

### Delete variables from All
setwd("~/Downloads")
all_d <- read_csv("all_d.csv")
model_all_d <- lm(Age~.,data = all_d)
summary(model_all_d)

### Stepwise
step <- read_csv("Response Variables.csv", skip = 1)
model_step <- lm(Age~.,data = step)
summary(model_step)

### Deleting insignificant variables from Stepwise
step_d <- read_excel("Step_D.xlsx", sheet = "Sheet1")
model_step_d <- lm(Age~.,data = step_d)
summary(model_step_d)

model_step_jz <- lm(Age~Education,Weight,Reliability,'Questionnaires or polls',data = step)
