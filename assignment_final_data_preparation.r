# Part 1 Data Exploration

# load the data
train <- read.csv('/home/nora/Documents/Titanic/train.csv', stringsAsFactors = F)
test  <- read.csv('/home/nora/Documents/Titanic/test.csv', stringsAsFactors = F)

# importing libraries
library('caret')
library(e1071) # model tuning
library(ROCR) # model evaluation
#install.packages("ggthemes")
library('ggthemes')
#install.packages('scales')
library('scales') 
library('dplyr') 
#install.packages("randomForest")
library('randomForest')
library(plyr)
library(reshape2)
library(ggplot2)
#install.packages("sqldf")
library(sqldf)

## Data Preperation and cleaning

#Replace missing values of ‘Cabin’ field with ‘None’
train$Cabin <- ifelse(is.na(train$Cabin), 'None', train$Cabin) 
train$Title <- gsub('(.*, )|(\\..*)', '', train$Name)

test$Cabin <- ifelse(is.na(test$Cabin), 'None', test$Cabin) 
test$Title <- gsub('(.*, )|(\\..*)', '', test$Name)
#View(test)

#2. View title values
table(train$Title)

#3. Create ‘titanic_missingAge’ table; contains only ‘Age=NA’
titanic_missingAge = train[is.na(train$Age), ]
titanic_missingAge_test = test[is.na(test$Age), ]
#View(titanic_missingAge_test)

#4.Create ‘MeanAges’ table; contains Mean ages of each title
MeanAges <- ddply(train, c("Title"), summarize, Mean_Age = round(mean(Age, na.rm = TRUE)))
#View(train)


#5.  	Join ‘titanic_missingAge’ table with ‘MeanAges’ table
MissingAge_Mean <- join(titanic_missingAge, MeanAges, by=c('Title'), type='left', match='all')
MissingAge_Mean_test <- join(titanic_missingAge_test, MeanAges, by=c('Title'), type='left', match='all')
#View(MissingAge_Mean_test)

#6.  	Replace ‘Age’ with ‘Mean_Age’ in ‘MissingAge_Mean’ table
MissingAge_Mean$Age <- MissingAge_Mean$Mean_Age
MissingAge_Mean_test$Age <- MissingAge_Mean_test$Mean_Age

#7.	Select ‘PassengerId’ and ‘Mean_Age’ from table ‘MissingAge_Mean’
#Create new table with these above selected two columns ‘MissingAge_MeanN’
MissingAge_MeanN <- sqldf('select PassengerId, Mean_Age from MissingAge_Mean')
MissingAge_MeanN_test <- sqldf('select PassengerId, Mean_Age from MissingAge_Mean_test')
#View(MissingAge_MeanN_test)

#8. Create new table ‘titanic_final’ <- Join ‘MissingAge_MeanN ‘table with ‘train’
titanic_final <- join(train, MissingAge_MeanN, by=c('PassengerId'), type='left', match='all')
titanic_final_test <- join(test, MissingAge_MeanN_test, by=c('PassengerId'), type='left', match='all')
#View(titanic_final_test)

# check if NA
#missing <- as.data.frame(sapply(titanic_final, function(x) sum(is.na(x))))
#missing_test <- as.data.frame(sapply(titanic_final_test, function(x) sum(is.na(x))))
#View(missing)

#9.  Impute ‘Age’ values with ‘Mean_Age’
titanic_final$Age <- ifelse(is.na(titanic_final$Age), titanic_final$Mean_Age, titanic_final$Age)
titanic_final_test$Age <- ifelse(is.na(titanic_final_test$Age), titanic_final_test$Mean_Age, titanic_final_test$Age)
#View(titanic_final_test)

# quick fix: Impute Fare for one missing row
titanic_final_test$Fare <- ifelse(is.na(titanic_final_test$Fare), 0 , titanic_final_test$Fare)

#Exploratory Analysis - Variables vs survived

#Age vs Survived
ggplot(titanic_final, aes(Age, fill = factor(Survived))) + geom_histogram() + theme_bw()

#Sex vs Survived
ggplot(data=titanic_final, aes(x=Survived, fill=Sex)) + geom_bar() + theme_bw()

#Pclass vs Survived
ggplot(titanic_final, aes(Pclass, fill = factor(Survived))) + geom_histogram(stat="count") + theme_bw()

#Fare vs Survived
ggplot(titanic_final, aes(Fare, fill = factor(Survived))) + geom_histogram() + theme_bw()

#Embarked vs Survived
ggplot(titanic_final, aes(Embarked, fill = factor(Survived))) + geom_histogram(stat="count") + theme_bw()

#Parch vs Survived
ggplot(titanic_final, aes(Parch, fill = factor(Survived))) + geom_histogram(stat="count") + theme_bw()

#SibSp vs Survived
ggplot(titanic_final, aes(SibSp, fill = factor(Survived))) + geom_histogram(stat="count") + theme_bw()


