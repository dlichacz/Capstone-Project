# Using importance feature of the boosting function, a loop will be run dropping the least
# important feature at each iteration and calculating the accuracy rate.

# It is hoped that a subset of the features will be identified in which there is no large
# decrease in accuracy.

# Extract the list of important features and put them in decreasing order.
boost.importance <- names(sort(ffm.boost$importance))

# Initialize a vector that will hold the accuracy of the model at each step.  For some reason the boosting function
# will not produce a result when only using one feature.  As a result, this step will be omitted.  It is not
# expected that this will change the final result.
boost.accuracy <- numeric(length=50)
boost.accuracy[1] <- NA

# Create loop that finds a model after dropping the least important feature at each step.
for (i in 50:2) {
  set.seed(55)
  df <- ffm[,c("gender",boost.importance[1:i])]
  boost.test <- boosting(gender~., df)
  c <- confusionMatrix(boost.test$class, df$gender)
  boost.accuracy[i] <- c$overall["Accuracy"]
}

# Plot the accuracy rate values.
plot(boost.accuracy, xlab="Number of Features Used", ylab="Accuracy Rate", main="Accuracy Rate by Importance")

# Based on the plot, there is some levelling off in the accuracy rate around 25 features.  For maximum accuracy, around 40
# features should be used, but in order to find a balance between accuracy and number of features, a model will be
# built using 25 features.

# Create data frame using the 25 important features.
ffm.important <- ffm[, c("gender", boost.importance[1:25])]

# Build model using these 25 features.
set.seed(55)
ffm.boost.important <- boosting(gender~., ffm.important)

# The same process will be repeated on the new model to see if any further reduction is possible.
boost.importance.2 <- names(sort(ffm.boost.important$importance))
boost.accuracy.2 <- numeric(length=25)
boost.accuracy.2[1] <- NA

for (i in 25:2) {
  set.seed(55)
  df <- ffm[,c("gender",boost.importance.2[1:i])]
  boost.test <- boosting(gender~., df)
  c <- confusionMatrix(boost.test$class, df$gender)
  boost.accuracy.2[i] <- c$overall["Accuracy"]
}

# Plot the accuracy rate values.
plot(boost.accuracy.2, xlab="Number of Features Used", ylab="Accuracy Rate", main="Accuracy Rate by Importance")

# This time the plot increases almost monotonically with no sign of levelling off.  As a result,
# no further reduction will be made.