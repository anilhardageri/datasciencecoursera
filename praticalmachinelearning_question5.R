library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p=3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(3433)
IL <- grep("^IL", colnames(training), value=TRUE)
ILpredictors <- predictors[, IL]
df <- data.frame(diagnosis, ILpredictors)
inTrain <- createDataPartition(df$diagnosis, p=3/4)[[1]]
training <- df[inTrain, ]
testing <- df[-inTrain, ]
modelFit <- train(diagnosis ~ ., method="glm", data=training)
predictions <- predict(modelFit, newdata=testing)
C1 <- confusionMatrix(predictions, testing$diagnosis)
print(C1)
acc1 <- C1$overall[1]
acc1 # Non-PCA Accuracy: 0.65 

modelFit <- train(training$diagnosis ~ ., 
                  method="glm", 
                  preProcess="pca", 
                  data=training, 
                  trControl=trainControl(preProcOptions=list(thresh=0.8)))
C2 <- confusionMatrix(testing$diagnosis, predict(modelFit, testing))
print(C2)
acc2 <- C2$overall[1]
acc2