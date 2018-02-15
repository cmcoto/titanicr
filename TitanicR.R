#libraries needed
library(dplyr)
library(ggplot2)
library(mice)

#read datafiles

train <- read.csv('c:/users/EvaMaria/Downloads/train.csv', header = TRUE, sep=",")
test <- read.csv('c:/users/EvaMaria/Downloads/test.csv', header = TRUE, sep=",")

#get idea about the Data
#Structure of the Data
str(train)
#Summary of the Data
summary(train)

#Unite all the Data
test$Survived <- NA
all <- rbind(train, test)


#find NA in Variables, apparently in AGE we have 177, and appears to be the variable most affected...
# What percent does the NA's account for the dataset? Is it 5% of Variable?


#function for finding % of Na per variable...found on:
#https://datascienceplus.com/imputing-missing-data-with-r-mice-package/

pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(all,2,pMiss)
apply(all,1,pMiss)

#Use mice to find missing data with md function
md.pattern(all)



#Should I change the NA in Age with the MEAN of Age? Or should I check per case, if NAME or Title implies age?
#I get the index of the NA
#naage <- which(is.na(train$Age))

#get the names of naage to imply age?
#train$Name[naage]

#Master in name
#na_in_name <- which(grepl("Master", train$Name[naage]))
#na_in_name

#USE DPLYR to select variables to be used with MICE
dall <- select(all, Survived, Pclass, Age, SibSp, Parch, Fare)
dall

#USE MICE WITH VARIABLES 
imp <- mice(dall, exclude = "Survived", meth='pmm',seed=500)
summary(imp)

#check imputed variables
imp$imp$Age
#complete the Data
imp <- complete(imp,1)

#factorize some variables

fsurvived <- as.factor(all$Survived)
fpclass <- as.factor(all$Pclass)

 
#factor Age according to Life Cycle Stages
fage <- as.factor(all$Age) 
cfage <- cut(all$Age, c(0,12, 20, 40, 60, Inf))
#factor fage levels
#levels(fage) <- c("Childhood" = 0 <= 12, "Adolescense" = 13 <= 20, "Early Adult" = 21 <= 40, "Midlife" = 41 <= 60 "Mature" = 60 > 100)

#factor Age without the NA's imputed by mice
fimp_age <- as.factor(imp$Age) 
cfimp_age <- cut(imp$Age, c(0,12, 20, 40, 60, Inf))

#PLOTS

plot(all$Sex, fsurvived, main = "Titanic Survival According To Gender", xlab = "Gender", ylab ="Survival")

plot(cfimp_age, fsurvived, main = "Titanic Survival According To Life Cycle Stages", xlab = "Life Stage", ylab ="Survival")


#MODELS
#logistic regression

#Logistic regression
#glm.fit=glm(train$Survived~imp$Age+imp$Pclass+imp$SibSp+imp$Parch+imp$Fare,
 #           data=train,family=binomial)
#summary(glm.fit)
#Prediction
#glm.probs=predict(glm.fit,newdata=test,type="response")
#glm.pred=ifelse(glm.probs >0.5,"Lived","Died")


