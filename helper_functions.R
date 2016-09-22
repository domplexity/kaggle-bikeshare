preparedata <- function(raw_dataset, is_training_data = FALSE){
  #shitty variable preparation
  
  raw_dataset$workingday <- as.logical(raw_dataset$workingday)
  raw_dataset$holiday <- as.logical(raw_dataset$holiday)
  raw_dataset$season[raw_dataset$season == 1] <- "spring"
  raw_dataset$season[raw_dataset$season == 1] <- "summer"
  raw_dataset$season[raw_dataset$season == 1] <- "fall"
  raw_dataset$season[raw_dataset$season == 1] <- "winter"
  raw_dataset$season <- as.factor(x = raw_dataset$season)
  levels(raw_dataset$season) <- c("spring","summer","fall","winter")
  
  raw_dataset$datetime <- as.POSIXct(raw_dataset$datetime,tz = "EST")
  
  raw_dataset$houroftheday <- as.numeric(format(raw_dataset$datetime, "%H"))
  raw_dataset$dayofthemonth <- as.POSIXlt(raw_dataset$datetime)$mday
  raw_dataset$month <- as.POSIXlt(raw_dataset$datetime)$mon + 1
  if (is_training_data == TRUE) {
    raw_dataset$logcasual <- log(raw_dataset$casual +1)
    raw_dataset$logregistered <- log(raw_dataset$registered +1)
    raw_dataset$logcount <- log(raw_dataset$count +1)
    val_selector <- as.POSIXlt(raw_dataset$datetime)$mday > 14
    train_data <- raw_dataset[val_selector == FALSE,]
    validation_data <- raw_dataset[val_selector == TRUE,]
    return(list(train_data, validation_data))
  }else{
    return(raw_dataset)
  }
}

validate_predictions <- function(predictions,validation_set){
  #must be a vector of the length of the validation set
  rmsle <- 0
  for (i in c(1:length(predictions))) {
    rmsle <-rmsle + (log(predictions[i]+1)-log(validation_set$count[i]+1))^2
  }
  rmsle <- sqrt(rmsle/length(predictions))
  return(rmsle)
}
