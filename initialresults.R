# Load packages that will be used in analysis.
library(kknn)
library(brglm)
library(randomForest)
library(adabag)
library(caret)
library(klaR)
library(e1071)

# Divide the data set into 80% training data and 20% test data.
set.seed(55)
training.vector <- sample(nrow(ffm), floor(nrow(ffm)*0.8))

ffm.train <- ffm.traits[training.vector,]
ffm.test <- ffm.traits[-training.vector,]

# Multiple classification algorithms will be applied and then compared to determine
# which one will be more fully explored.

# Fit a kNN model.
set.seed(55)
ffm.knn <- kknn(gender~., train=ffm.train, test=ffm.test)
c.knn <- confusionMatrix(ffm.knn$fit, ffm.test$gender)
c.knn

# Fit a Naive Bayes model.
set.seed(55)
ffm.nb <- NaiveBayes(gender~., data=ffm.train)
ffm.nb.predictions <- predict(ffm.nb, ffm.test)
c.nb <- confusionMatrix(ffm.nb.predictions$class, ffm.test$gender)
c.nb

# Fit a logisitic regression model.
set.seed(55)
ffm.lr <- brglm(gender ~ ., data = ffm.train)
ffm.lr.predictions <- predict(ffm.lr, ffm.test, type="response")

# Produce the confusion matrix.
for (i in 1:length(ffm.lr.predictions)) {
  if (ffm.lr.predictions[i] <= 0.5) {
    ffm.lr.predictions[i] <- "Male"
  }
  else if (ffm.lr.predictions[i] > 0.5) {
    ffm.lr.predictions[i] <- "Female"
  }
}

ffm.lr.predictions <- factor(ffm.lr.predictions, c("Male", "Female"))
c.lr <- confusionMatrix(ffm.lr.predictions, ffm.test$gender)
c.lr

# Fit a random forest model.
set.seed(55)
ffm.rf <- randomForest(gender~., data = ffm.train, importance=TRUE)
ffm.rf.predictions <- predict(ffm.rf, ffm.test)
c.rf <- confusionMatrix(ffm.rf.predictions, ffm.test$gender)
c.rf

# Fit a boosting model.
set.seed(55)
ffm.boost <- boosting(gender~.,ffm)
ffm.boost.predictions <- predict(ffm.boost, ffm, type="response")
c.boost <- confusionMatrix(ffm.boost.predictions$class, ffm$gender)
c.boost

# Create and plot the ROC curves.
library(pROC)
knn.roc <- roc(ffm.test$gender ~ as.numeric(ffm.knn$fit))
nb.roc <- roc(ffm.test$gender ~ as.numeric(ffm.nb.predictions$class))
lr.roc <- roc(ffm.test$gender ~ as.numeric(ffm.lr.predictions))
rf.roc <- roc(ffm.test$gender ~ as.numeric(ffm.rf.predictions))
boost.roc <- roc(ffm$gender ~ as.numeric(factor(ffm.boost.predictions$class,levels=gender.factors)))

par(mfrow=c(1,1))
plot(knn.roc, xlab="Male", ylab="Female", main="ROC Curves")
plot(nb.roc, add=TRUE, col="red")
plot(lr.roc, add=TRUE, col="blue")
plot(rf.roc, add=TRUE, col="green")
plot(boost.roc, add=TRUE, col="purple")
legend("bottomright", 
       c(paste("kNN:", round(knn.roc$auc, 4)), paste("Naive Bayes:", round(nb.roc$auc, 4)),
         paste("Logistic Regression:", round(lr.roc$auc, 4)), paste("Random Forest:", round(rf.roc$auc, 4)),
         paste("Boost:", round(boost.roc$auc, 4))),
         pch=15, col=c("black", "red","blue", "green", "purple"), bty="n")

