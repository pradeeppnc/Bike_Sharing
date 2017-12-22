#Load the training set
train = read.csv(choose.files())
str(train)
table(train$count)

#Load the testing set
test = read.csv(choose.files())
str(test)

#Split the datetime column & create time column in train & test dataset
train$time  = substring(train$datetime, 12, 20)
test$time = substring(test$datetime, 12, 20)
table(train$time)

#Create day column in train & test dataset
train$day = weekdays(as.Date(train$datetime))
test$day = weekdays(as.Date(test$datetime))
table(train$day)

#Create weekend for train & test dataset
train$weekends[(train$day == "Sunday")] = "1"
train$weekends[train$day == "Saturday"] = "1"
train$weekends[train$day != "1"] = "0"
train$weekends[train$day == "Sunday"] = "1"
train$weekends[train$day == "Saturday"] = "1"
table(train$weekends)

test$weekends[test$day == "Sunday"] = "1"
test$weekends[test$day == "Saturday"] = "1"
test$weekends[test$day != "1"] = "0"
test$weekends[test$day == "Sunday"] = "1"
test$weekends[test$day == "Saturday"] = "1"
table(test$weekends)

#Create hours
train$hour = as.numeric(substr(train$time, 1, 2))
test$hour = as.numeric(substr(test$time, 1, 2))
table(train$hour)

#Create daystart
train$daystart = "4"
test$daystart = "4"

train$daystart[(train$hour < 5)] = 1
test$daystart[(test$hour < 5)] = 1

train$daystart[(train$hour > 4) & (train$hour < 11)] = 2
test$daystart[(test$hour > 4) & (test$hour < 11)] = 2

train$daystart[(train$hour > 10) & (train$hour < 18)] = 3
test$daystart[(test$hour > 10) & (test$hour < 18)] = 3

table(train$daystart)

#Convert variables to factors in train
train$weather = as.factor(train$weather)
train$holiday = as.factor(train$holiday)
train$workingday = as.factor(train$workingday)
train$season = as.factor(train$season)
train$time = as.factor(train$time)
train$day = as.factor(train$day)
train$weekends = as.factor(train$weekends)
train$hour = as.factor(train$hour)
train$daystart = as.factor(train$daystart)
str(train)

#Convert variables to factors in test
test$weather = as.factor(test$weather)
test$holiday = as.factor(test$holiday)
test$workingday = as.factor(test$workingday)
test$season = as.factor(test$season)
test$time = as.factor(test$time)
test$day = as.factor(test$day)
test$weekends = as.factor(test$weekends)
test$hour = as.factor(test$hour)
test$daystart = as.factor(test$daystart)
str(test)

#Load libraries
install.packages("")
library('party')
library(rattle)
library(rpart)
library(rpart.plot)

#Create model
Model1 = ctree(count ~ season + holiday + workingday + weather 
               + temp + humidity + windspeed + weekends + hour 
               + daystart , data=train)

#Make Prediction
prediction = predict(Model1, newdata = test, type = "response")

#Create Dataframe as per requirement 
Output = data.frame(datetime = test$datetime, count = prediction)
Output

#Save the Output as csv file
write.csv(Output, file = "bike_sharing.csv", row.names = FALSE)
