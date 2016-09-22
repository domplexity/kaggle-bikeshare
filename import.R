source('helper_functions.R')

train <- read.csv('train.csv')
test <- read.csv('test.csv')

temp <- preparedata(train,is_training_data = TRUE)
train_data <- temp[[1]]
validation_data <- temp[[2]]

test_data <- preparedata(test)








