#' Beginner Tutorial for Data Science using R from Kaggle
#' data set for Titanic: Machine Learning for disaster.
#' 
#'         Author : Gautam Matta
#'         Date : 3/20/2017
#'         Purpose : To learn Data Science using Kaggle data sets
#'         
#'         
#' Resource used : http://trevorstephens.com/kaggle-titanic-tutorial/r-part-1-booting-up/        

library("readr")

getwd()

#Set Working Directory and import datafiles

setwd("C:/Users/i0318/Downloads/Titanic DataSet")
train_df <- read_csv("train.csv")
test_df <-read_csv("test.csv")

# glimpse(train_df)
# glimpse(test_df)
# str(train_df)
# Method 1 : Find out how many people survived using which command. 

length(which(train_df$Survived == 1 ))


# Method 2 : Find out how many people survived using table command 
# which is a very useful command when you want to view summary 
# statistics for a table.

table(train_df$Survived == 1 )["TRUE"]
table(train_df$Survived)

# To find out proportion of people survived and died, use prop.table
# on top of the above command to get the results.

prop.table(table(train_df$Survived))  
#' 0   (Died)                  1 (Survived)
#' 0.6161616                    0.3838384 
#' 
#' Since most of the people (62%) died in our training data set , 
#' let's assume that everyone died in the smaller train data set.

#'test_df$Survived <-rep(0,418)

#'submit <- data.frame(PassengerId = test_df$PassengerId, Survived = test_df$Survived)
#'write_csv(submit, "verybasic.csv")


################### Part 2 begins 

table(train_df$Sex)

prop.table(table(train_df$Sex, train_df$Survived),1)

test_df$Survived <- 0

test_df$Survived[test_df$Sex == "female"] <- 1

summary(train_df$Age)


# Create Child column 
train_df$Age[is.na (train_df$Age)] <-  mean(train_df$Age, na.rm = TRUE)

train_df$Child <-0
train_df$Child[train_df$Age<18] <- 1 
prop.table(table(train_df$Child,train_df$Survived),1)


## Total number of Gender wise , age wise people who survived
train_df %>% select(Child, Sex, Survived)%>%
group_by(Child, Sex)%>% summarise(sum(Survived))
####' or you can use below aggregate function
####' aggregate(Survived~ Child+ Sex,data =train_df, FUN = sum)




## Breakdown of Gender wise , age wise people  
train_df %>% select(Child, Sex, Survived)%>%
group_by(Child, Sex)%>% summarise(length(Survived))


## proportion of people survived based on Gender and age (child or adult)

train_df %>% select(Child, Sex, Survived)%>%
group_by(Child, Sex)%>% summarise(prop =sum(Survived)/length(Survived))



#### Lets take a look at some other features 

glimpse(train_df) # PClass, Fare

prop.table(table(train_df$Pclass,train_df$Survived),1)  # Clas 1 survived the most, then 2,3


train_df$Fare2 <- '30+'
train_df$Fare2[train_df$Fare < 30 & train_df$Fare >=20] <-'20-30'
train_df$Fare2[train_df$Fare < 20 & train_df$Fare >=10] <-'10-20'
train_df$Fare2[train_df$Fare < 10] <-'<10'

prop.table(table(train_df$Fare2, train_df$Survived),1)

train_df %>% select(Fare2,Pclass, Sex, Survived)%>%
group_by( Fare2, Pclass,Sex)%>% 
  summarise(Survived = sum(Survived)/length(Survived))%>% 
  arrange(Sex,Pclass)

# create new column in test data set 
test_df$Survived <-0 
# Update the new column with survived in 
test_df$Survived[test_df$Sex == 'female'] <- 1
test_df$Survived[test_df$Sex== 'female' & test_df$Pclass == 3 & test_df$Fare >=20] <- 0 


#'submit <- data.frame(PassengerId = test_df$PassengerId, Survived = test_df$Survived)
#'write_csv(submit, "verybasic.csv")





###########################
##### Tutorial 3 ##########
###########################


# Include rpart library which stands for "Recursive 
# Partitioning and Regression Trees" 

library(rpart)
# help(rpart)

fit <-rpart(Survived ~ Sex + Pclass + Age + SibSp+ Parch+ Fare+ Embarked,
            data = train_df,
            method = "class")

plot(fit)
text(fit)

install.packages("rattle")
install.packages("rpart.plot")
install.packages("RColorBrewer")

library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(fit)

Prediction <- predict(fit, test_df, type = "class")

submit <- data.frame(PassengerId= test_df$PassengerId, Survived = Prediction)
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)




?rpart.control


fit_ctrl <-rpart(Survived ~ Sex + Pclass + Age + SibSp+ Parch+ Fare+ Embarked,
            data = train_df,
            method = "class",
            control = rpart.control(minsplit = 2, cp = 0))


fancyRpartPlot(new.fit )

new.fit <- prp(fit, snip = TRUE)$obj