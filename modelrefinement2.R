# Since the original data set contains more Female instances than Male, some experimenting will be done
# with different proportions of each to see how this effects results.

# Create a function that partitions the original data set into 100p% Male and 100(1-p)% Female
partition <- function (df, p) {
  # Sample x% of the total sample size of ffm from the Male population and the remaining from the Female population.
  # This will keep the overall sample size the same as the original data set
  set.seed(55)
  ffm.part <- rbind(df[sample(which(df$gender=="Male"), floor(p*nrow(df)), replace = TRUE),], 
                    df[sample(which(df$gender=="Female"), floor((1-p)*nrow(df)), replace = TRUE),])
  return(ffm.part)
}

# Create data frames that have a Male/Female ratio of 50/50, 80/20 & 20/80.
ffm.5050 <- partition(ffm.important, 0.5)
ffm.8020 <- partition(ffm.important, 0.8)
ffm.2080 <- partition(ffm.important, 0.2)

# Fit a model for the 50/50 data.
set.seed(55)
ffm.boost.5050 <- boosting(gender~., ffm.5050)
ffm.boost.5050.predictions <- predict(ffm.boost.5050, ffm.5050, type="response")
c.boost.5050 <- confusionMatrix(ffm.boost.5050$class, ffm.5050$gender)
c.boost.5050

# Fit models for the 80/20 & 20/80 split data.
set.seed(55)
ffm.boost.8020 <- boosting(gender~., ffm.8020, control = rpart.control(cp = -1))
set.seed(55)
ffm.boost.2080 <- boosting(gender~., ffm.2080, control = rpart.control(cp = -1))
ffm.boost.8020.predictions <- predict(ffm.boost.8020, ffm.8020, type="response")
ffm.boost.2080.predictions <- predict(ffm.boost.2080, ffm.2080, type="response")
c.boost.8020 <- confusionMatrix(ffm.boost.8020$class, ffm.8020$gender)
c.boost.2080 <- confusionMatrix(ffm.boost.2080$class, ffm.2080$gender)
c.boost.8020
c.boost.2080

# Display difference in importance of variables in the models.
abs(ffm.boost.8020$importance - ffm.boost.2080$importance)
abs(ffm.boost.8020$importance - ffm.boost.5050$importance)
abs(ffm.boost.5050$importance - ffm.boost.2080$importance)

# Since the accuracy rates are very high for the imbalanced models, they will be performed
# on separate training and test sets to look for overfitting due to the oversampling in the data sets.

# Create training and test sets.
ffm.train.important <- ffm.important[training.vector,]
ffm.test.important <- ffm.important[-training.vector,]

# Partition the training set. 
train.8020 <- partition(ffm.train.important, .8)
train.2080 <- partition(ffm.train.important, .2)

# Fit models.
set.seed(55)
boost.8020 <- boosting(gender~., train.8020, control = rpart.control(cp = -1))
set.seed(55)
boost.2080 <- boosting(gender~., train.8020, control = rpart.control(cp = -1))

# Use models to predict test set and produce confusion matricies.
pred.8020 <- predict(boost.8020, ffm.test.important)
pred.2080 <- predict(boost.2080, ffm.test.important)
c.8020 <- confusionMatrix(pred.8020$class, ffm.test.important$gender)
c.2080 <- confusionMatrix(pred.2080$class, ffm.test.important$gender)
c.8020
c.2080

# The accuracy using the independent test set is not similar to the original cross-validation
# and so the suspected overfitting is present and the oversampled models will not be used.

# A similar test will be performed on the balanced data to check for overfitting.

train.5050 <- partition(ffm.train.important, .5)

set.seed(55)
boost.5050 <- boosting(gender~., train.5050, control = rpart.control(cp = -1))

pred.5050 <- predict(boost.5050, ffm.test.important)
c.5050 <- confusionMatrix(pred.5050$class, ffm.test.important$gender)
c.5050

# In this case, while some overfitting on the Male side may be occurring in the original cross-validation
# due to oversampling, the results are much closer and produce the highest Male accuracy rate seen so far and
# the closest balance between sensitivity and specificity. Therefore, a model with a balanced data set will be used moving forward.