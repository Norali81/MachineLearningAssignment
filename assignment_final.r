## Model creation, evaluation and 
# run file ""assignment_final_data_preparation.r" before this script, 
# to have to have data frame "titanic_final" and "titanic_final_test" loaded

# drop unneeded columns 
colnames(titanic_final)
titanic_final1 <- sqldf('select PassengerId, 
                        Survived, Pclass, Sex, Age, SibSp, Parch, Fare, 
                        Embarked, Title from titanic_final')
titanic_final1_test <- sqldf('select PassengerId, 
                             Pclass, Sex, Age, SibSp, Parch, Fare, 
                             Embarked, Title from titanic_final_test')
# str(titanic_final1_test)

# convert data types to factor, random forest package requires factors or numeric values
titanic_final1[sapply(titanic_final1, is.character)] <- lapply(titanic_final1[sapply(titanic_final1, is.character)], 
                                                               as.factor)
titanic_final1_test[sapply(titanic_final1_test, is.character)] <- lapply(titanic_final1_test[sapply(titanic_final1_test, is.character)], 
                                                                         as.factor)

# Building the random forest
#set.seed(123)


View(titanic_final1_test)

# add the level "Dona" to the titanic_final1 "title" factors 
levels(titanic_final1$Title)<-c(levels(titanic_final1$Title), "Dona")
levels(titanic_final1$Title)

# add levels not present in test that are present in train
titanic_final1_test$Embarked <- factor(titanic_final1_test$Embarked, levels=levels(titanic_final1$Embarked))
titanic_final1_test$Title<- factor(titanic_final1_test$Title, levels=levels(titanic_final1$Title))
levels(titanic_final1_test$Title)
View(titanic_final1)

## Split training set into train and validation set
# 75% sample 
cut_off <- floor(0.75 * nrow(titanic_final1))
cut_off # 668

# set seed
set.seed(123)

## Create validation data set ## 
# take random 75% of data set and store IDs in a list
train_ind  <- sample(seq_len(nrow(titanic_final1)), size = cut_off)
train_ind
# take the random 75% IDs an put them in a data frame
train_final <- titanic_final1[train_ind, ]
View(train_final)

# put remaining 25% into a data fram
validation_final <- titanic_final1[-train_ind, ]
View(validation_final)

# Labels of the validation_set
validation_survived <-validation_final[,"Survived"]
validation_survived[0:20]

# Model
# train random forest with train data set
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Embarked + Title +
                           Fare,
                         data = train_final,
                         na.action=na.roughfix)
print(rf_model)


# Get importance
## Source: https://www.kaggle.com/mrisdal/titanic/exploring-survival-on-the-titanic
importance    <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
# Source: https://www.kaggle.com/mrisdal/titanic/exploring-survival-on-the-titanic
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
## Source: https://www.kaggle.com/mrisdal/titanic/exploring-survival-on-the-titanic
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()

# Evaluation of Model on validation set
# predicting survival of validation data set based on model
prediction <- predict(rf_model, validation_final, type="response")

# create confusion matrix
confusionMatrix(data=prediction, reference=validation_survived)

# predict probabilities for ROC curve
predict_probability <- predict(rf_model, validation_final, type="prob")
View(predict_probability)
predict_probability_reduced <-predict_probability[,2]

View(predict_probability_reduced)

#make prediction object for rocr package
# arguments: 
# predictions
# A vector, matrix, list, or data frame containing the predictions.
# labels
# A vector, matrix, list, or data frame containing the true class labels. Must have the same dimensions as 'predictions'.
validation_survived[0:10]
data_for_ROC <- prediction(predict_probability_reduced, validation_survived)

# function for ROC curve. Source
# (Bali & Sarkar, 2016)
plot.roc.curve <- function(predictions, title.text){
  perf <- performance(predictions, "tpr", "fpr")
  plot(perf,col="black",lty=1, lwd=2,
       main=title.text, cex.main=0.6, cex.lab=0.8,xaxs="i", yaxs="i")
  abline(0,1, col="red")
  auc <- performance(predictions,"auc")
  auc <- unlist(slot(auc, "y.values"))
  auc <- round(auc,2)
  legend(0.4,0.4,legend=c(paste0("AUC: ",auc)),cex=0.6,bty = "n",box.col = "white")
  
}

plot.pr.curve <- function(predictions, title.text){
  perf <- performance(predictions, "prec", "rec")
  plot(perf,col="black",lty=1, lwd=2,
       main=title.text, cex.main=0.6, cex.lab=0.8, xaxs="i", yaxs="i")
}

# Source: Bali, R., & Sarkar, D. (2016). R Machine Learning By Example. Packt Publishing Ltd.
par(mfrow=c(1,2))
plot.roc.curve(data_for_ROC, title.text="RF ROC Curve")
plot.pr.curve(data_for_ROC, title.text="RF Precision/Recall Curve")
View(data_for_ROC)

# Predict using the test set
prediction <- predict(rf_model, titanic_final1_test)

# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
solution <- data.frame(PassengerID = titanic_final1_test$PassengerId, Survived = prediction)

# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
#solution <- data.frame(PassengerID = titanic_final1$PassengerId, Survived = titanic_final1$Survived, Survived = prediction)
View(solution)
# Write the solution to file
write.csv(solution, file = 'rf_mod_Solution.csv', row.names = F)

