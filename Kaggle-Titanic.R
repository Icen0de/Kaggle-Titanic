#libraries
###############################
library(rpart)
library(randomForest)
library(rattle)
#library(rpart.plot)
library(RColorBrewer)
library(stringr)
###############################

#Import and Prepare Project Data
###############################
#set Working Directory
setwd("C:/Users/chris/Documents/Kaggle/Titanic")

#Import Kaggle supplied files
train <- read.table("train.csv", sep = ",", header = TRUE)
test <- read.table("test.csv", sep =",", header = TRUE)

#Normalize columns between data sets, Union the data from the two files
test$Survived <- NA 
all_data <- rbind(train,test)
###############################

#Examine Data
###############################
str(all_data)
head(all_data)
tail(all_data)
summary(all_data)

all_data[c(700,949),]
###############################

#Clean and Prepare Data for use
###############################
#Replace Fare NA values with Mediam fare value
all_data$Fare[is.na(all_data$Fare)] <- median(all_data$Fare, na.rm=TRUE)

#Replace missing Embarked location with "S" since SouthHampton was most used location
all_data$Embarked[all_data$Embarked==""] <- "S"

#Want to get rid of the NA Age values. Trying to predict age based on other data
#Several trees were created before keeping this as the final prediction formula
#Created new field FamilySize by added together number of siblings, parents/children, and the passenger
all_data$FamilySize <- all_data$SibSp + all_data$Parch + 1
Age_tree <- rpart(Age ~ Pclass + Parch + SibSp + FamilySize + Fare + Embarked, data=all_data, method="anova")
fancyRpartPlot(Age_tree)

#Replace Age NA values with predicted age value
all_data$Age[is.na(all_data$Age)] <- predict(Age_tree,all_data[is.na(all_data$Age),])

#Extract Title from Name field
all_data$Title <- factor(str_match(all_data$Name,pattern=", (.*?)\\.")[,2])

#Split Data back into Training and Test data
train <- all_data[!is.na(all_data$Survived),]
test <- all_data[is.na(all_data$Survived),]
###############################

#Prediction #1 
#using Random Forest method
#Kaggle Score: .78947
###############################
set.seed(42)
Survival_Forest1 <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + FamilySize + Title, data=train, ntree=15000, importance=TRUE)
Survival_Prediction1 <- predict(Survival_Forest1,test)
Survival_Forest1

Solution1 <- data.frame(PassengerId=test$PassengerId,Survived=Survival_Prediction1)
write.csv(Solution1,file="my_rf_prediction1.csv",row.names=FALSE)
###############################

#Prediction #2
#Using Rpart tree method 
#Kaggle Score: .77512
###############################
Survival_Tree2 <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + FamilySize + Title, data=train, method="class")
fancyRpartPlot(Survival_Tree2)
Survival_Prediction2 <- predict(Survival_Tree2,test,type="class")


Solution2 <-data.frame(PassengerId=test$PassengerId,Survived=Survival_Prediction2)
write.csv(Solution2,file="my_rpart_prediction2.csv",row.names=FALSE)
###############################
Solution2

#Want to try to improve method by testing cabin sections or deck letters
#Due to the ammount of missing data I don't expect an improvment
###############################
#Split deck data into groups and factor the data
all_data$Deck <- "U"
all_data$Deck[str_detect(all_data$Cabin,"A")] <- "AB"
all_data$Deck[str_detect(all_data$Cabin,"B")] <- "AB"
all_data$Deck[str_detect(all_data$Cabin,"C")] <- "CD"
all_data$Deck[str_detect(all_data$Cabin,"D")] <- "CD"
all_data$Deck[str_detect(all_data$Cabin,"E")] <- "EFG"
all_data$Deck[str_detect(all_data$Cabin,"F")] <- "EFG"
all_data$Deck[str_detect(all_data$Cabin,"G")] <- "EFG"
all_data$Deck <- factor(all_data$Deck)
summary(all_data)

all_data$CabNum <- as.numeric(str_extract(all_data$Cabin,pattern="[0-9]+"))
round(all_data$CabNum[!is.na(all_data$CabNum)],-1)

#Split Data back into Training and Test data
train <- all_data[!is.na(all_data$Survived),]
test <- all_data[is.na(all_data$Survived),]


#Prediction #3
#Using Rpart tree method 
#With just Deck added to formula
#Kaggle Score: .77512
###############################
#Include deck groups in our formula
Survival_Tree3 <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + FamilySize + Title + Deck, data=train, method="class")
fancyRpartPlot(Survival_Tree3)
Survival_Prediction3 <- predict(Survival_Tree3,test,type="class")

Solution3 <-data.frame(PassengerId=test$PassengerId,Survived=Survival_Prediction3)
write.csv(Solution3,file="my_rpart_prediction3.csv",row.names=FALSE)
###############################

#Prediction #4
#Using Random Forest method 
#With just Deck added to formula
#Kaggle Score: .78947
###############################
#Include deck groups in our formula
Survival_Forest4 <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + FamilySize + Title, data=train, ntree=15000, importance=TRUE)
Survival_Prediction4 <- predict(Survival_Forest4,test)
Survival_Forest4

Solution4 <- data.frame(PassengerId=test$PassengerId,Survived=Survival_Prediction4)
write.csv(Solution4,file="my_rpart_prediction4.csv",row.names=FALSE)
###############################

#Prediction #5
#Using Rpart tree method 
#With Cabin # added to formula
#Kaggle Score: .77512
###############################
#Include deck groups in our formula
Survival_Tree5 <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + FamilySize + Title + CabNum, data=train, method="class")
fancyRpartPlot(Survival_Tree5)
Survival_Prediction5 <- predict(Survival_Tree5,test,type="class")

Solution5 <-data.frame(PassengerId=test$PassengerId,Survived=Survival_Prediction5)
write.csv(Solution5,file="my_rpart_prediction5.csv",row.names=FALSE)
###############################

#Prediction #6
#Using Random Forest method 
#With Cabin group added to formula
#Kaggle Score: .78947
###############################
#Include deck groups in our formula
Survival_Forest6 <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + FamilySize + Title, data=train, ntree=15000, importance=TRUE)
Survival_Prediction6 <- predict(Survival_Forest6,test)
Survival_Forest6

Solution6 <- data.frame(PassengerId=test$PassengerId,Survived=Survival_Prediction6)
write.csv(Solution6,file="my_rpart_prediction6.csv",row.names=FALSE)
###############################

#Prediction #7
#Using Random Forest method 
#Removed SibSp and Parch leaving in only FamilySize
#Kaggle Score: .78947
###############################
#Include deck groups in our formula
Survival_Forest7 <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + Fare + Embarked + FamilySize + Title, data=train, ntree=15000, importance=TRUE)
Survival_Prediction7 <- predict(Survival_Forest7,test)
Survival_Forest7

Solution7 <- data.frame(PassengerId=test$PassengerId,Survived=Survival_Prediction7)
write.csv(Solution7,file="my_rpart_prediction7.csv",row.names=FALSE)
###############################
