library(dplyr)
library(ggplot2)

#read datafiles

train <- read.csv('c:/users/EvaMaria/Downloads/train.csv', header = TRUE, sep=",")
test <- read.csv('c:/users/EvaMaria/Downloads/test.csv', header = TRUE, sep=",")

#factorize some variables

fsurvived <- as.factor(train$Survived)
fpclass <- as.factor(train$Pclass)

#find NA in Variables

#Should I change the NA in Age with the MEAN of Age? Or should I check per case, if NAME or Title implies age?
#I get the index of the NA
naage <- which(is.na(train$Age))
#get the names of naage to imply age?
train$Name[naage]
#Master in name
na_in_name <- which(grepl("Master", train$Name[naage]))
na_in_name

#factor Age according to Life Cycle Stages
fage <- as.factor(train$Age) 
cfage <- cut(train$Age, c(0,12, 20, 40, 60, Inf))
#factor fage levels
#levels(fage) <- c("Childhood" = 0 <= 12, "Adolescense" = 13 <= 20, "Early Adult" = 21 <= 40, "Midlife" = 41 <= 60 "Mature" = 60 > 100)

plot(train$Sex, fsurvived, main = "Titanic Survival According To Gender", xlab = "Gender", ylab ="Survival")

plot(cfage, fsurvived, main = "Titanic Survival According To Life Cycle Stages", xlab = "Life Stage", ylab ="Survival")
#models