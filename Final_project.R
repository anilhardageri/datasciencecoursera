library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(randomForest)
library(knitr)




set.seed(12345)

trainUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

training <- read.csv(url(trainUrl), na.strings=c("NA","#DIV/0!",""))
testing <- read.csv(url(testUrl), na.strings=c("NA","#DIV/0!",""))

inTrain <- createDataPartition(training$classe, p=0.6, list=FALSE)
myTraining <- training[inTrain, ]
myTesting <- training[-inTrain, ]
dim(myTraining); dim(myTesting)


nzv <- nearZeroVar(myTraining, saveMetrics=TRUE)
myTraining <- myTraining[,nzv$nzv==FALSE]

nzv<- nearZeroVar(myTesting,saveMetrics=TRUE)
myTesting <- myTesting[,nzv$nzv==FALSE]

myTraining <- myTraining[c(-1)]

trainingV3 <- myTraining
for(i in 1:length(myTraining)) {
  if( sum( is.na( myTraining[, i] ) ) /nrow(myTraining) >= .7) {
    for(j in 1:length(trainingV3)) {
      if( length( grep(names(myTraining[i]), names(trainingV3)[j]) ) == 1)  {
        trainingV3 <- trainingV3[ , -j]
      }   
    } 
  }
}

# Set back to the original variable name
myTraining <- trainingV3
rm(trainingV3)

clean1 <- colnames(myTraining)
clean2 <- colnames(myTraining[, -58])  # remove the classe column
myTesting <- myTesting[clean1]         # allow only variables in myTesting that are also in myTraining
testing <- testing[clean2]             # allow only variables in testing that are also in myTraining

dim(myTesting)

for (i in 1:length(testing) ) {
  for(j in 1:length(myTraining)) {
    if( length( grep(names(myTraining[i]), names(testing)[j]) ) == 1)  {
      class(testing[j]) <- class(myTraining[i])
    }      
  }      
}

# To get the same class between testing and myTraining
testing <- rbind(myTraining[2, -58] , testing)
testing <- testing[-1,]

#Classification tree
library(rattle)
library(rpart.plot)
library(rpart)
set.seed(123)
Model1_CT <- train(classe ~ .,data = myTraining,method = "rpart")
save(Model1_CT,file = "Model1_CT.RData")
load("Model1_CT.RData")
fancyRpartPlot(Model1_CT$finalModel)
pred1_CT <- predict(Model1_CT,myTesting)
cm_CT <- confusionMatrix(pred1_CT,myTesting$classe)
cm_CT$overall[1]
cm_CT$table
library(knitr)
kable(cm_CT$table)

#Random forest

set.seed(123)

system.time(Mod1 <- train(classe ~ ., method = "rf", data = myTraining, importance = T,trControl = trainControl(method = "cv", number = 3)))
save(Mod1,file="Mod1.RData")
load("Mod1.RData")
vi <- varImp(Mod1)
vi$importance[1:10,]
pred1 <- predict(Mod1, myTesting)
cm_RF <- confusionMatrix(pred1, myTesting$classe)
cm_RF$table

plot(Mod1) #number of predictors for model to converge
plot(Mod1$finalModel) #number of trees for model to converge

plot(varImp(Mod1),top = 10) #important predictors

#boosting
set.seed(12345)
fitControl <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 1)

gbmFit1 <- train(classe ~ ., data=myTraining, method = "gbm",
                 trControl = fitControl,
                 verbose = FALSE)

gbmFinMod1 <- gbmFit1$finalModel

gbmPredTest <- predict(gbmFit1, newdata=myTesting)
cm_boost <- confusionMatrix(gbmPredTest, myTesting$classe)
cm_boost

#bagging

system.time({Mod3 <- train(classe ~ .,data=myTraining,method="treebag")})

save(Mod3,file="Mod3.RData")

load("Mod3.RData")
pred3 <- predict(Mod3, myTesting)
cm_bag <- confusionMatrix(pred3, myTesting$classe)

varImp(Mod3)
plot(varImp(Mod3), top = 10)

#overallpredictions

re <- data.frame(Tree=cm_CT$overall[1], 
                 rf=cm_RF$overall[1], 
                 boosting=cm_boost$overall[1],
                 bagging=cm_bag$overall[1])
library(knitr)
re

# compare the sensitivity and specificity btw random forest and boosting method

par(mfrow=c(2,2))
plot(cm0$byClass, main="classification tree", xlim=c(0.4, 1.005), ylim=c(0.7,1))
text(cm0$byClass[,1]+0.04, cm0$byClass[,2], labels=LETTERS[1:5], cex= 0.7)
plot(cm1$byClass, main="random forest", xlim=c(0.96, 1.005))
text(cm1$byClass[,1]+0.003, cm1$byClass[,2], labels=LETTERS[1:5], cex= 0.7)
plot(cm2$byClass, main="boosting", xlim=c(0.93, 1.001))
text(cm2$byClass[,1]+0.005, cm2$byClass[,2], labels=LETTERS[1:5], cex= 0.7)
plot(cm3$byClass, main="bagging", xlim=c(0.97, 1.005))
text(cm3$byClass[,1]+0.003, cm3$byClass[,2], labels=LETTERS[1:5], cex= 0.7)


test$classe <- as.character(predict(Mod1, test))

# write prediction files
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("./predict/problem_id_", i, ".txt")
    write.table(x[i], file = filename, quote = FALSE, row.names = FALSE, col.names = FALSE)
  }
}
pml_write_files(test$classe)
Mod1$finalModel
summary(test$classe)

