train <- read.csv('train.csv')

str(train)

#shitty variable preparation

train$workingday <- as.logical(train$workingday)
train$holiday <- as.logical(train$holiday)
train$season[train$season == 1] <- "spring"
train$season[train$season == 1] <- "summer"
train$season[train$season == 1] <- "fall"
train$season[train$season == 1] <- "winter"
train$season <- as.factor(x = train$season)
levels(train$season) <- c("spring","summer","fall","winter")

train$datetime <- as.POSIXct(train$datetime,tz = "Washington")

train$houroftheday <- as.numeric(format(train$datetime, "%H"))

#plotting a bit, to get to know dataset

library("ggplot2")

ggplot(data = train) + geom_bar(aes(houroftheday,count),stat="identity") +
  facet_grid(weather~season)

ggplot(data = train) + geom_point(aes(x = temp, y = count, colour = holiday), alpha = 0.4) +
  facet_wrap(~weather)

ggplot(data = train) + geom_density(aes(x = houroftheday)) +
  facet_grid(~weather)



