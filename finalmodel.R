# Since the previous balanced data set did not make use of all points in the original set, create one
# that consists of all Female observations and oversamples the Male observations.
ffm.over <- rbind(ffm.important[sample(which(ffm.important$gender=="Male"), table(ffm.important$gender)["Female"], replace = TRUE),], 
                  ffm.important[ffm.important$gender=="Female",])


# Use the train function to tune the parameters of the AdaBoost algorithm.
library(caret)
ffm.boost.train <- train(ffm.over[,-1], ffm.over$gender, method="AdaBoost.M1", trControl = trainControl(method = "cv"))

# The train function suggested the following parameters for the model: mfinal=150, maxdepth=3, coeflearn=Zhu

# Fit the final model using the optimal parameters.
set.seed(55)
ffm.boost.cv <- boosting.cv(gender~., ffm.over, ,mfinal=150, coeflearn="Zhu",
                            control=rpart.control(maxdepth=3))
confusionMatrix(ffm.boost.cv$class, ffm.over$gender)