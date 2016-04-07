library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p=3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
ss <- training[,grep('^IL', x = names(training) )]
preProc <- preProcess(ss, method='pca', thresh=0.9, 
                      outcome=training$diagnosis)
preProc$rotation