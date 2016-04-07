library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p=3/4)[[1]]
training = mixtures[inTrain,]
testing = mixtures[-inTrain,]
xnames <- colnames(concrete)[1:8]
featurePlot(x=training[, xnames], y=training$CompressiveStrength, plot="pairs")
# No relation between the outcome and other variables
index <- seq_along(1:nrow(training))
ggplot(data=training, aes(x=index, y=CompressiveStrength)) + geom_point() + 
  theme_bw()
# Step-like pattern -> 4 categories
library(Hmisc)
cutCompressiveStrength <- cut2(training$CompressiveStrength, g=4)
summary(cutCompressiveStrength)
ggplot(data=training, aes(y=index, x=cutCompressiveStrength)) + 
  geom_boxplot() + geom_jitter(col="blue") + theme_bw()
# Another way
library(plyr)
splitOn <- cut2(training$Age, g=4)
splitOn <- mapvalues(splitOn, 
                     from=levels(factor(splitOn)), 
                     to=c("red", "blue", "yellow", "green"))
plot(training$CompressiveStrength, col=splitOn)
# There is a step-like pattern in the plot of outcome versus index 
# in the training set that isn't explained by any of the predictor 
# variables so there may be a variable missing.
