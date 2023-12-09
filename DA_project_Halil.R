# Data Analytics Project
# Halil ARICI

# Before running this R file do the following:
# Run the file named 'training_data_load.R'
# Next, run the file named 'load_weather_data.R'
# While running these files you will need to set the directories.
library(dplyr)
# EDA, Data Cleaning and Data PreProcessing
head(merged_data)
summary(merged_data)
dim(merged_data)

sapply(merged_data, function(x) sum(is.na(x)))
backup <- merged_data

sum(is.na(merged_data))
queens_data <- filter(merged_data, borough == "QUEENS")
qbackup <- queens_data
queens_data <- qbackup
# Remove the patient transfer operations
queens_data <- filter(queens_data, transfer_indicator =="N")

# Remove the stand by operations
queens_data <- filter(queens_data, standby_indicator =="N")
qbackup<-queens_data
queens_data <- qbackup
summary(queens_data)
#Remove some data that won't be used
queens_data <- subset(queens_data, select=-c(first_assign_dt,first_act_dt,first_to_hosp_dt,
                                             first_hosp_arrival_dt,incident_close_dt))
queens_data<- subset(queens_data, select=-c(FMTM,PGTM,TAVG,TSUN,WDF2,WDF5,WSF5,WT19))
queens_data<- queens_data[!duplicated(queens_data$cad_incident_id),]
noNA <- na.omit(queens_data)
noNA2008 <- filter(noNA, incident_year == 2008)
noNA2008<- subset(noNA2008, select=-c(cad_incident_id,incident_dt,first_on_scene_dt,incident_year))
noNA2008<- subset(noNA2008, select=-c(DATE,STATION,NAME,month.y))
noNA2008<- subset(noNA2008, select=-c(borough))
noNA2008<- subset(noNA2008, select=-c(transfer_indicator))
## We need to remove any column that includes time information because it is known after the incident is finished
noNA2008<- subset(noNA2008, select=-c(dispatch_response_seconds_qy,incident_travel_tm_seconds_qy))

# Convert Y/N data to T/F
noNA2008$valid_dispatch_rspns_time_indc <- ifelse(noNA2008$valid_dispatch_rspns_time_indc=="Y",T,F)
noNA2008$valid_incident_rspns_time_indc <- ifelse(noNA2008$valid_incident_rspns_time_indc=="Y",T,F)
noNA2008$held_indicator <- ifelse(noNA2008$held_indicator=="Y",T,F)
noNA2008$reopen_indicator <- ifelse(noNA2008$reopen_indicator=="Y",T,F)
noNA2008$special_event_indicator <- ifelse(noNA2008$special_event_indicator=="Y",T,F)
noNA2008$standby_indicator <- ifelse(noNA2008$standby_indicator=="Y",T,F)
noNA2008$held_indicator <- ifelse(noNA2008$held_indicator=="Y",T,F)

noNA2008$WT01 <- ifelse(noNA2008$WT01==1,T,F)
noNA2008$WT02 <- ifelse(noNA2008$WT02==1,T,F)
noNA2008$WT03 <- ifelse(noNA2008$WT03==1,T,F)
noNA2008$WT04 <- ifelse(noNA2008$WT04==1,T,F)
noNA2008$WT05 <- ifelse(noNA2008$WT05==1,T,F)
noNA2008$WT06 <- ifelse(noNA2008$WT06==1,T,F)
noNA2008$WT07 <- ifelse(noNA2008$WT07==1,T,F)
noNA2008$WT08 <- ifelse(noNA2008$WT08==1,T,F)
noNA2008$WT09 <- ifelse(noNA2008$WT09==1,T,F)
noNA2008$WT11 <- ifelse(noNA2008$WT11==1,T,F)
noNA2008$WT13 <- ifelse(noNA2008$WT13==1,T,F)
noNA2008$WT14 <- ifelse(noNA2008$WT14==1,T,F)
noNA2008$WT16 <- ifelse(noNA2008$WT16==1,T,F)
noNA2008$WT17 <- ifelse(noNA2008$WT17==1,T,F)
noNA2008$WT18 <- ifelse(noNA2008$WT18==1,T,F)
noNA2008$WT22 <- ifelse(noNA2008$WT22==1,T,F)
summary(noNA2008)

noNA2008<- subset(noNA2008, select=-c(date_m))
noNA2008<- subset(noNA2008, select=-c(incident_dispatch_area))

summary(noNA2008)

boxplot(noNA2008$incident_response_seconds_qy, main = "BoxPlot of Response Time in Seconds")
###### Machine Learning STARTS HERE ###########
###############################################
############################################### 

library(caTools)## sample.split() comes from this.
#Split the data into training and test for validation
noNA2008backup <- noNA2008

noNA2008 <- noNA2008[which(noNA2008$incident_response_seconds_qy>10),]
split = sample.split(noNA2008$incident_response_seconds_qy,SplitRatio = 0.8)
train_data = subset(noNA2008, split==TRUE)
train_y <- train_data$incident_response_seconds_qy
train_data_noGT = train_data[-5] #removing ground truth

test_data=subset(noNA2008,split==FALSE)
test_data_noGT =test_data[-5]
groundTruth <- test_data$incident_response_seconds_qy


## Model 1: Decision Tree Regression
library(rpart)
library(tidymodels)
library(tidyr)

tree_spec <- decision_tree()%>%
  set_engine("rpart") %>%
  set_mode("regression")

tree_fit <- tree_spec %>%
  fit(incident_response_seconds_qy~.,data=train_data)

predictions <- tree_fit %>%
  predict(test_data)%>%
  pull(.pred)

metrics<- metric_set(mae,rmse,rsq)
dtmodel_performance <- test_data %>% mutate(predictions=predictions) %>% metrics(truth = incident_response_seconds_qy,estimate = predictions)

print(dtmodel_performance)

summary(dtmodel_performance)

library(rpart.plot)
rpart.plot(tree_fit$fit,type = 4, extra = 101, under = TRUE, cex = 0.8, box.palette = "auto")


## Model 2: Random Forest Regression
library(randomForest)
library(ggplot2)

rf <- randomForest(incident_response_seconds_qy~., data=train_data, ntree=100, keep.forest=T,importance=T)
rf

rfpred <- predict(rf,test_data)
predictions <- rfpred
rfmodel_performance <- test_data %>% mutate(predictions=predictions) %>% metrics(truth = incident_response_seconds_qy,estimate = predictions)
print(rfmodel_performance)
## Model 3: XGBoost Regression
library(xgboost)

xtrain_x = data.matrix(train_data_noGT)
xtest_x = data.matrix(test_data_noGT)

xgb_train = xgb.DMatrix(data = xtrain_x, label = train_y)
xgb_test = xgb.DMatrix(data = xtest_x, label = groundTruth)

#defining a watchlist
watchlist = list(train=xgb_train, test=xgb_test)

#fit XGBoost model and display training and testing data at each iteartion
model = xgb.train(data = xgb_train, max.depth = 4, watchlist=watchlist, nrounds = 300)

model_xgboost = xgboost(data = xgb_train, max.depth = 4, nrounds = 172)

summary(model_xgboost)

pred_y = predict(model_xgboost, xgb_test)

XGMAE <- caret::MAE(groundTruth,pred_y) #MAE
XGMAE
XGmse <- mean((groundTruth - pred_y)^2) #mse - Mean Squared Error
XGmse
XGRMSE <- caret::RMSE(groundTruth, pred_y) #rmse - Root Mean Squared Error
XGRMSE
XGRSQ <- caret::R2(groundTruth, pred_y)
XGRSQ
## Model 4:Support Vector Regression
library(caret)
smalltrain_data <- sample_n(train_data, 10000)
SVMmodel = train(incident_response_seconds_qy~., data = smalltrain_data, method = "svmLinear")

print(SVMmodel)
SVMpred_y = predict(SVMmodel,test_data)

## Models for No Weather Data

newData <- noNA2008[,1:20]
split = sample.split(newData$incident_response_seconds_qy,SplitRatio = 0.8)
train_data = subset(newData, split==TRUE)
train_y <- train_data$incident_response_seconds_qy
train_data_noGT = train_data[-5] #removing ground truth

test_data=subset(newData,split==FALSE)
test_data_noGT =test_data[-5]
groundTruth <- test_data$incident_response_seconds_qy

### Decision Tree

tree_spec <- decision_tree()%>%
  set_engine("rpart") %>%
  set_mode("regression")

tree_fit <- tree_spec %>%
  fit(incident_response_seconds_qy~.,data=train_data)

predictions <- tree_fit %>%
  predict(test_data)%>%
  pull(.pred)

metrics<- metric_set(mae,rmse,rsq)
dtmodel_performance <- test_data %>% mutate(predictions=predictions) %>% metrics(truth = incident_response_seconds_qy,estimate = predictions)

print(dtmodel_performance)

summary(dtmodel_performance)

library(rpart.plot)
rpart.plot(tree_fit$fit,type = 4, extra = 101, under = TRUE, cex = 0.8, box.palette = "auto")


### Random Forest

library(randomForest)
library(ggplot2)

rf <- randomForest(incident_response_seconds_qy~., data=train_data, ntree=100, keep.forest=T,importance=T)
rf

rfpred <- predict(rf,test_data)
predictions <- rfpred
rfmodel_performance <- test_data %>% mutate(predictions=predictions) %>% metrics(truth = incident_response_seconds_qy,estimate = predictions)
print(rfmodel_performance)





### XGBoost
library(xgboost)

xtrain_x = data.matrix(train_data_noGT)
xtest_x = data.matrix(test_data_noGT)

xgb_train = xgb.DMatrix(data = xtrain_x, label = train_y)
xgb_test = xgb.DMatrix(data = xtest_x, label = groundTruth)

#defining a watchlist
watchlist = list(train=xgb_train, test=xgb_test)

#fit XGBoost model and display training and testing data at each iteartion
model = xgb.train(data = xgb_train, max.depth = 4, watchlist=watchlist, nrounds = 300)

model_xgboost = xgboost(data = xgb_train, max.depth = 4, nrounds = 172)

summary(model_xgboost)

pred_y = predict(model_xgboost, xgb_test)

XGMAE <- caret::MAE(groundTruth,pred_y) #MAE
XGMAE
XGmse <- mean((groundTruth - pred_y)^2) #mse - Mean Squared Error
XGmse
XGRMSE <- caret::RMSE(groundTruth, pred_y) #rmse - Root Mean Squared Error
XGRMSE
XGRSQ <- caret::R2(groundTruth, pred_y)
XGRSQ




### SVM Regression


library(caret)
smalltrain_data <- sample_n(train_data, 10000)
SVMmodel = train(incident_response_seconds_qy~., data = smalltrain_data, method = "svmLinear")

print(SVMmodel)
SVMpred_y = predict(SVMmodel,test_data)




