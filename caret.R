# Load libraries
library(mlbench)
library(caret)
library(caretEnsemble)

# Load the dataset
data(Ionosphere)
dataset <- Ionosphere
dataset <- dataset[,-2]
dataset$V1 <- as.numeric(as.character(dataset$V1))

# Example of Boosting Algorithms
control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "Accuracy"
# C5.0
set.seed(seed)
## Tuning c50 
grid <- expand.grid( .winnow = c(TRUE,FALSE), .trials=c(1,5,10,15,20,25,30), .model="tree" )
fit.c50 <- train(Class~., data=dataset,tuneGrid=grid, method="C5.0", metric=metric, trControl=control)
fit.c50 <- train(Class~., data=dataset, method="C5.0", metric=metric, trControl=control)

# Stochastic Gradient Boosting
set.seed(seed)
## Used for Tuning
gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                       n.trees = (1:30)*50, 
                       shrinkage = 0.1,
                       n.minobsinnode = 20)

## Tuning GBM
fit.gbm1 <- train(Class~., data=dataset, method="gbm",tuneGrid = gbmGrid, metric=metric, trControl=control, verbose=FALSE)

fit.gbm2 <- train(Class~., data=dataset, method="gbm", metric=metric, trControl=control, verbose=FALSE)


# summarize results
boosting_results <- resamples(list(c5.0=fit.c50, gbm=fit.gbm1))
summary(boosting_results)
dotplot(boosting_results)

# Example of Bagging algorithms
control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "Accuracy"
# Bagged CART
set.seed(seed)
fit.treebag <- train(Class~., data=dataset, method="treebag", metric=metric, trControl=control)
# Random Forest
set.seed(seed)
fit.rf <- train(Class~., data=dataset, method="rf", metric=metric, trControl=control)
# summarize results
bagging_results <- resamples(list(treebag=fit.treebag, rf=fit.rf))
summary(bagging_results)
dotplot(bagging_results)

# Example of Stacking algorithms
# create submodels
control <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE, classProbs=TRUE)
algorithmList <- c('lda', 'rpart', 'glm', 'knn', 'svmRadial','gbm')
set.seed(seed)
models <- caretList(Class~., data=dataset, trControl=control, methodList=algorithmList)
results <- resamples(models)
summary(results)
dotplot(results)

# stack using glm
stackControl <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE, classProbs=TRUE)
set.seed(seed)
stack.glm <- caretStack(models, method="glm", metric="Accuracy", trControl=stackControl)
print(stack.glm)

# stack using random forest
set.seed(seed)
stack.rf <- caretStack(models, method="rf", metric="Accuracy", trControl=stackControl)
print(stack.rf)
