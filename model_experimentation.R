#plotting a bit, to get to know dataset

library("ggplot2")

ggplot(data = train) + geom_bar(aes(houroftheday,count),stat="identity") +
  facet_grid(weather~season)

ggplot(data = train) + geom_point(aes(x = temp, y = count, colour = holiday), alpha = 0.4) +
  facet_wrap(~weather)

ggplot(data = train) + geom_density(aes(x = houroftheday)) +
  facet_grid(~weather)



# AGENDA
# 1 simple models, simple features
# 2 simple models, all features
# 3 simple models, feature engineering
# 4 complex models

performance_table <- data.frame(matrix(data = NA, nrow = 0, ncol = 4))
colnames(performance_table) <- c("model_short","description","formula_used","rsmle_at_val")

  
# ultra basic baseline: always predict mean
temp <- c("mean_of_train",
          "just predict the mean of the training set everytime",
          "-none-",
          validate_predictions(predictions = c(rep(mean(train_data$count),dim(validation_data)[1])), validation_set = validation_data))
performance_table[dim(performance_table)[1]+1,] <- temp

# 1
# try and compare a bunch of different (simple) models on the same featureset
# used features: houroftheday, season, weather, workingday
# predict: count

#############
#linear regression

# 1. Build & Train
m1.glm <- glm(formula <- count ~ houroftheday * season * weather * workingday,
              family = "gaussian",
              data = train_data)
summary(m1.glm)
m1.glm$aic

# 2. Get validation set accuracy
glm.predictions <- predict(m1.glm, newdata=validation_data, type="response", se.fit=T)

performance_table[dim(performance_table)[1]+1,] <- c("glm",
                                                     "glm with incomplete featureset",
                                                     "count ~ houroftheday * season * weather * workingday",
                                                     validate_predictions(predictions = glm.predictions$fit, validation_set = validation_data))

# 3. predict on test set and submit
# before submitting, retrain on whole training set
m1.glm <- glm(formula <- count ~ houroftheday * season * weather * workingday,
              family = "gaussian",
              data = rbind(train_data,validation_data))
glm.test.predictions <- predict(m1.glm, newdata=test_data, type="response", se.fit=T)
temp_sub$count <- glm.test.predictions$fit
write.csv(x = temp_sub, file = "glm_model_submission.csv",row.names = FALSE)


#############
# random forest 1

#1. Build & Train
library(randomForest)
m2.rf <- randomForest(formula = count ~ houroftheday * season * weather * workingday,
             data = train_data)

# 2. Get validation set accuracy
m2.predictions <- predict(object = m2.rf,newdata = validation_data,type = "response")

performance_table[dim(performance_table)[1]+1,] <- c("m2.rf",
                                                     "rf with incomplete featureset",
                                                     "count ~ houroftheday * season * weather * workingday",
                                                     validate_predictions(predictions = m2.predictions, validation_set = validation_data))



# 3. predict on test set and submit
# before submitting, retrain on whole training set
m2.rf <- randomForest(formula = count ~ houroftheday * season * weather * workingday,
                      data = rbind(train_data,validation_data))
m2.test.predictions <- predict(object = m2.rf,newdata = test_data,type = "response")
temp_sub$count <- m2.test.predictions
write.csv(x = temp_sub, file = "rf_model_submission.csv",row.names = FALSE)

#############
# random forest 2

#1. Build & Train
m3.rf <- randomForest(formula = count ~ holiday * houroftheday * season * weather * workingday * temp * atemp * windspeed * humidity,
                      data = train_data)

# 2. Get validation set accuracy
m3.predictions <- predict(object = m3.rf,newdata = validation_data,type = "response")
performance_table[dim(performance_table)[1]+1,] <- c("m3.rf",
                                                     "rf with complete featureset",
                                                     "count ~ holiday * houroftheday * season * weather * workingday * temp * atemp * windspeed * humidity",
                                                     validate_predictions(predictions = m3.predictions, validation_set = validation_data))


# 3. predict on test set and submit
# before submitting, retrain on whole training set
m3.rf <- randomForest(formula = count ~ holiday * houroftheday * season * weather * workingday * temp * atemp * windspeed * humidity,
                      data = rbind(train_data,validation_data))
m3.test.predictions <- predict(object = m3.rf,newdata = test_data,type = "response")
temp_sub$count <- m3.test.predictions
write.csv(x = temp_sub, file = "rf2_model_submission.csv",row.names = FALSE)



#############
# random forest 2
# separating casual & regular rentals in modelling and summing up for total predictions

#1. Build & Train
m4.rf.casual <- randomForest(formula = casual ~ holiday * houroftheday * season * weather * workingday * temp * atemp * windspeed * humidity,
                             data = train_data)
m4.rf.registered <- randomForest(formula = registered ~ holiday * houroftheday * season * weather * workingday * temp * atemp * windspeed * humidity,
                             data = train_data)

# 2. Get validation set accuracy
m4.predictions <- predict(object = m4.rf.casual,newdata = validation_data,type = "response") + predict(object = m4.rf.registered,newdata = validation_data,type = "response")
performance_table[dim(performance_table)[1]+1,] <- c("m4.rf",
                                                     "rf with complete featureset and separate training for casual and registered rentals",
                                                     "casual ~ holiday * houroftheday * season * weather * workingday * temp * atemp * windspeed * humidity",
                                                     validate_predictions(predictions = m4.predictions, validation_set = validation_data))




# 3. predict on test set and submit
# before submitting, retrain on whole training set



#############
# random forest 2
# separating casual & regular rentals in modelling and summing up for total predictions

#1. Build & Train
m5.rf.casual <- randomForest(formula = casual ~ holiday * houroftheday * season * weather * workingday * temp * atemp * windspeed * humidity * month,
                             data = train_data)
m5.rf.registered <- randomForest(formula = registered ~ holiday * houroftheday * season * weather * workingday * temp * atemp * windspeed * humidity * month,
                                 data = train_data)

# 2. Get validation set accuracy
m5.predictions <- predict(object = m5.rf.casual,newdata = validation_data,type = "response") + predict(object = m5.rf.registered,newdata = validation_data,type = "response")
performance_table[dim(performance_table)[1]+1,] <- c("m5.rf",
                                                     "rf with complete featureset + month and separate training for casual and registered rentals",
                                                     "casual ~ holiday * houroftheday * season * weather * workingday * temp * atemp * windspeed * humidity * month",
                                                     validate_predictions(predictions = m5.predictions, validation_set = validation_data))


#0.68 
# 3. predict on test set and submit
# before submitting, retrain on whole training set


#############
# SVM regression
# following the (simple) example of http://www.svm-tutorial.com/2014/10/support-vector-regression-r/

# 1. Build & Train
library(e1071)
m6.svm <- svm(formula = count ~ holiday * houroftheday * season * weather * workingday * temp * atemp * windspeed * humidity,
               data = train_data)

m6.predictions <- predict(m6.svm, newdata = validation_data)

performance_table[dim(performance_table)[1]+1,] <- c("m6.svm",
                                                     "simple svm as a starting point",
                                                     "count ~ houroftheday * season",
                                                     validate_predictions(predictions = m6.predictions, validation_set = validation_data))

#############
# SVM regression
# tuning parameters first

# 1. Tune, Build & Train
library(e1071)
# do not run - too slow
m7.tune <- tune(method = svm,
                train.x = count ~ season * weather * workingday,
                ranges = list(epsilon = seq(0.5,1.0,0.1), cost = 2^(5:8)),
                data = train_data)
# just some quick experiment

############

m8.tune <- tune(method = svm,
                train.x = count ~ holiday * houroftheday * season * weather * workingday * temp * atemp * windspeed * humidity,
                ranges = list(epsilon = seq(0.5,1.0,0.1), cost = 2^(5:8)),
                data = train_data)


############
# random forest on logarithmic rentals as dependent var in order to deal with outliers
library(randomForest)

#1. Build & Train
m9.rf.casual <- randomForest(formula = logcasual ~ holiday * houroftheday * season * weather * workingday * temp * atemp * windspeed * humidity,
                             data = train_data)
m9.rf.registered <- randomForest(formula = logregistered ~ holiday * houroftheday * season * weather * workingday * temp * atemp * windspeed * humidity,
                                 data = train_data)

# 2. Get validation set accuracy + revert the logarithm
m9.predictions <- exp(predict(object = m9.rf.casual,newdata = validation_data,type = "response")) - 1 + exp(predict(object = m9.rf.registered,newdata = validation_data,type = "response")) -1

performance_table[dim(performance_table)[1]+1,] <- c("m9.rf",
                                                     " random forest on logarithmic rentals as dependent var in order to deal with outliers",
                                                     "logcasual ~ holiday * houroftheday * season * weather * workingday * temp * atemp * windspeed * humidity",
                                                     validate_predictions(predictions = m9.predictions, validation_set = validation_data))


# 3. predict on test set and submit
# before submitting, retrain on whole training set



###########
#playing around

tuneRF(x = holiday * houroftheday * season * weather * workingday * temp * atemp * windspeed * humidity,
       y = logcasual,
       data = train_data,
       stepFactor=1.5, 
       improve=1e-5, 
       ntree=500)
