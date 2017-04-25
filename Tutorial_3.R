
#' Beginner Tutorial for Data Science using R from Kaggle
#' data set for Titanic: Machine Learning for disaster.
#' 
#'         Author : Gautam Matta
#'         Date : 3/20/2017
#'         Purpose : To learn Data Science using Kaggle data sets
#'         
#'         
#' Resource used : http://trevorstephens.com/kaggle-titanic-tutorial/r-part-1-booting-up/     
#' 
#'       

library("dplyr")
library("readr")
library("rpart")
library("rattle")
library("rpart.plot")
library("RColorBrewer")

getwd()

setwd("C:/Users/i0318/Downloads/Titanic DataSet")
train_df <- read_csv("train.csv")
test_df <-read_csv("test.csv")



length(which(train_df$Survived == 1 ))

table(train_df$Survived == 1 )["TRUE"]

prop.table(table(train_df$Survived))  

train_df$Name[1]


test_df$Survived <- NA

combi <-rbind(train_df, test_df)
str(combi)

is.factor(combi$Name)

strsplit(combi$Name[1],split='[,.]')[[1]][2]

combi$Title <- sapply(combi$Name, FUN = function(x)  {strsplit(x, split = '[,.]')[[1]][2]})

table(combi$Title)

#combi[which(c(combi$Title == " Mr")),]

combi$Title <- sub(' ','',combi$Title )

combi$Title[ combi$Title == "Jonkheer"] <- "Mr"

combi$Title[ combi$Title %in% c( 'Mlle')] <- "Miss"

combi$Title[ combi$Title %in% c( 'Mme')] <- "Mrs"

combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess')] <- 'Lady'

combi$Title <- factor(combi$Title)

combi$FamilySize <- combi$SibSp + combi$Parch + 1

combi$SurName <- sapply(combi$Name, FUN = function(x) {strsplit(x, split = '[,.]')[[1]][1]})

combi$FamilyID <- paste(as.character(combi$FamilySize) , combi$SurName, sep ="")

combi$FamilyID[combi$FamilySize <= 2] <- 'Small'

famIDs <- data.frame(table(combi$FamilyID))

famIDs <-famIDs[famIDs$Freq<=2,]

combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'

combi$FamilyID <-factor(combi$FamilyID)

table(combi$FamilyID)

#combi[combi$FamilyID == "3Johnson",]


train_final <- combi[1:891,]
#combi[grepl("Johnson", combi$FamilyID) == "TRUE",]$FamilyID
test_final <- combi[892:1309,]


fit <- rpart(Survived ~ Pclass + Sex+ Age+ SibSp + Parch+ Fare + Embarked + Title + FamilySize + FamilyID, 
              data = train_final,
             method = "class")



fancyRpartPlot(fit)



Prediction <- predict(fit, test_final, type = "class")
#submit <- data.frame(PassengerId= test_final$PassengerId, Survived = Prediction)

#write.csv(submit, file = "featureengineering.csv", row.names = FALSE)
