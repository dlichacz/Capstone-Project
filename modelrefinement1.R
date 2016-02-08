# In an attempt to increase classification accuracy, the percentage of Likert type answers will be added to the model.

# Create new data frame by adding the Likert data. 
ffm.important.likert <- data.frame(gender=ffm$gender, ffm[,boost.importance], ffm.likert[,-1])

# Fit a model using the new data.
set.seed(55)
ffm.boost.likert <- boosting(gender~., ffm.important.likert)
ffm.boost.likert.predictions <- predict(ffm.boost.likert, ffm.important.likert, type="response")
c.boost.likert <- confusionMatrix(ffm.boost.likert.predictions$class, ffm.important.likert$gender)
c.boost.likert

# The original Likert data was calculated using all 50 features.  The same process will be repeated using the 25
# important features to calculate the Likert data.

ffm.reduced.likert <- as.data.frame(cbind(gender = factor(ffm.important$gender, labels = gender.factors),
                                          Extreme = apply(ffm.important[,2:26],1, function(x) (sum(x == 1) + sum(x == 5))/25),
                                          Moderate = apply(ffm.important[,2:26],1, function(x) (sum(x == 2) + sum(x == 4))/25),
                                          Neutral = apply(ffm.important[,2:26],1, function(x) sum(x == 3)/25)))

ffm.reduced.likert$gender <- factor(ffm.reduced.likert$gender, labels = gender.factors)

ffm.important.likert.reduced <- data.frame(gender=ffm$gender, ffm[,boost.importance], ffm.reduced.likert[,-1])
set.seed(55)
ffm.boost.likert.reduced <- boosting(gender~., ffm.important.likert.r)
ffm.boost.likert.predictions.reduced <- predict(ffm.boost.likert.reduced, ffm.important.likert.reduced, type="response")
c.boost.likert.reduced <- confusionMatrix(ffm.boost.likert.predictions.reduced$class, ffm.important.likert.reduced$gender)
c.boost.likert.reduced

# Create and plot the ROC curves.
boost.roc <- roc(ffm$gender ~ as.numeric(factor(ffm.boost.predictions$class,levels=gender.factors)))
boost.roc.likert.full <- roc(ffm.important.likert$gender ~ as.numeric(factor(ffm.boost.likert.predictions$class,levels=gender.factors)))
boost.roc.likert.reduced <- roc(ffm.important.likert.reduced$gender ~ as.numeric(factor(ffm.boost.likert.predictions.reduced$class,levels=gender.factors)))

plot(boost.roc, xlab="Male", ylab="Female", main="ROC Curves")
plot(boost.roc.likert.full, add=TRUE, col="red")
plot(boost.roc.likert.reduced, add=TRUE, col="blue")
legend("bottomright", 
       c(paste("Boost:", round(boost.roc$auc, 4)), paste("Boost with Likert:", round(boost.roc.likert.full$auc, 4)),
         paste("Boost with Predicted Likert:", round(boost.roc.likert.reduced$auc, 4))), 
         pch=15, col=c("black", "red","blue"), bty="n")

# The ROC curves in all three models appear to be almost identical with no significant difference in classification accuracy.
# Therefore, the Likert data is not adding to the accuracy and will be omitted from the final model.