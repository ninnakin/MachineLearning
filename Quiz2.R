# Question 1
library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)

# Q2
data(concrete)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

hist(training$Superplasticizer)
range(training$Superplasticizer)

# Q3
set.seed(3433)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

# Find all the predictor variables in the training set that begin with IL. Perform principal 
# components on these variables with the preProcess() function from the caret package. 
# Calculate the number of principal components needed to capture 80% of the variance. How many are there?

IL <- training[,grep("^IL",names(training))]
pc <- preProcess(IL, method="pca", thresh=0.80)
pc
summary(prcomp(IL))
# Why so different results? 7 vs 3 variables

# Q4
# Create a training data set consisting of only the predictors with variable names beginning with 
# IL and the diagnosis. Build two predictive models, one using the predictors as they are and one
# using PCA with principal components explaining 80% of the variance in the predictors. Use method="glm" 
# in the train function. What is the accuracy of each method in the test set? Which is more accurate?
set.seed(3433)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

IL <- training[,grep("^IL",names(training))]

# set variance explained to 80%
ctrl <- trainControl(preProcOptions = list(thresh = 0.80))
pca<-train(IL,training$diagnosis, method="glm", preProcess = "pca", trControl = ctrl)
non_pca<-train(IL, training$diagnosis, method="glm")

IL_test <- testing[,grep("^IL",names(testing))]

p_non<-predict(non_pca, IL_test)
p_pca<-predict(pca, IL_test)

# Compute accuracy
mean(testing$diagnosis==p_non)
mean(testing$diagnosis==p_pca)


# Can method return accuraccy by itself?
IL_test$prediction <- testing$diagnosis
p_non<-predict(non_pca, IL_test)
p_pca<-predict(pca, IL_test)
mean(testing$diagnosis==p_non)
mean(testing$diagnosis==p_pca)
# Yes :)
