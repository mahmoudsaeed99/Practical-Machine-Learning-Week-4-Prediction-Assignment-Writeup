library(knitr)
library(caret)
library(rpart)
library(rpart.plot)
library(rattle)
library(randomForest)
library(RColorBrewer)
library(RGtk2)
library(gbm)

train_url<- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
test_url<- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
training_data<- read.csv(url(train_url))
testing_data<- read.csv(url(test_url))
dim(training_data)
dim(testing_data)

nzv <- nearZeroVar(training_data)
train_data <- training_data[,-nzv]
test_data <- testing_data[,-nzv]
dim(train_data)
dim(test_data)


na_val_col <- sapply(train_data, function(x) mean(is.na(x))) > 0.95
train_data <- train_data[,na_val_col == FALSE]
test_data <- test_data[,na_val_col == FALSE]
dim(train_data)
dim(test_data)

train_data<- train_data[, 8:59]
test_data<- test_data[, 8:59]
dim(train_data)
dim(test_data)


inTrain <- createDataPartition(y = train_data$classe ,p = 0.6 , list = F )
training <- train_data[inTrain,]
testing <- train_data[-inTrain,]
DT_model <- train(classe ~ . , data = training , method = "rpart")
fancyRpartPlot(DT_model$finalModel)



set.seed(21243)
DT_prediction <- predict(DT_model , testing)
confusionMatrix(DT_prediction , testing$classe)

set.seed(26817)
RF_model<- train(classe ~. , data=training, method= "rf", ntree=100)
RF_prediction<- predict(RF_model, testing)
RF_cm<-confusionMatrix(RF_prediction, testing$classe)
RF_cm


plot(RF_cm$table, col=RF_cm$byClass, main="Random Forest Accuracy")

set.seed(25621)
gbm_model<- train(classe~., data=training, method="gbm", verbose= FALSE)
gbm_model$finalmodel

gbm_prediction<- predict(gbm_model, testing)
gbm_cm<-confusionMatrix(gbm_prediction, testing$classe)
gbm_cm

RF_cm$overall

gbm_cm$overall

prediction_test<- predict(RF_model, test_data)
prediction_test



