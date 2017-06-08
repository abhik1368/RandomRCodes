library(dplyr)
library(data.table)
library(xgboost)
library(Metrics)



cat("training model...\n")
#Expected <- trainSum$Expected
#trainSum <- as.matrix(trainSum[,-c(1:2)])
#trainSum <- matrix(as.numeric(trainSum),nrow(trainSum),ncol(trainSum))

Expected <- df.train$ptype
trainSum <- as.matrix(df.train[,-1025])
trainSum <- xgb.DMatrix(trainSum, label = Expected)

testSum <- as.matrix(df.test[, -1025])
testSum <- matrix(as.numeric(testSum),nrow(testSum),ncol(testSum))

testSum <- xgb.DMatrix(testSum)


train <- xgb.DMatrix(data = as.matrix(df.train[,-1025]), label=df.train$ptype)
test <- xgb.DMatrix(data = testSum, label=df.test$ptype)

xgbHex <- xgboost(data = trainSum, label = Expected,
                  objective  = "reg:linear", 
                  eval_metric = "rmse",
                  eta = 0.015,
                  subsample = 0.7,
                  min_child_weight =10,    
                  max_depth = 6,
                  nthreads = 4,
                  nrounds = 1000)
xgb_prediction <- expm1(predict(xgbHex,testSum))


## CV with XGBoost

param <- list("objective" = "binary:logistic",    # multiclass classification 
              "nthread" = 8,   # number of threads to be used 
              "max_depth" = 16,    # maximum depth of tree 
              "eta" = 0.3,    # step size shrinkage 
              "gamma" = 0,    # minimum loss reduction 
              "subsample" = 1,    # part of data instances to grow tree 
              "colsample_bytree" = 1,  # subsample ratio of columns when constructing each tree 
              "min_child_weight" = 12  # minimum sum of instance weight needed in a child 
)


set.seed(1234)
# k-fold cross validation, with timing
nround.cv = 200
system.time( cv_results <- xgb.cv(param=param, data = trainSum, label = Expected,
                              nfold=5, nrounds=nround.cv, prediction=TRUE, verbose=FALSE) )

tail(bst.cv$dt)
cv_results$dt$idx = 1:nround.cv
ggplot(data=cv_results$dt,aes()) + geom_line(aes(x=idx,y=test.error.mean), colour='red') +
    geom_line(aes(x=idx,y=train.error.mean), colour='blue') + xlab('Epoch') + ylab('Error') + ggtitle('Cross-validation Performance')

min_error_idx = which.min(cv_results$dt[,test.error.mean])
min_error = cv_results$dt[min_error_idx,test.error.mean]

set.seed(100)
xgb <- xgboost(data = trainSum, params=param, label = Expected,
               nrounds=min_error_idx, missing = "NA", prediction=TRUE)#, verbose=FALSE)

model <- xgb.dump(xgb, with.stats = T)
model[1:10]

names <- dimnames(data.matrix(testSum))[[2]]

# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = xgb)

# Nice graph
xgb.plot.importance(importance_matrix[1:10,])

#In case of a version issue, try the following :
barplot(importance_matrix[,1:2])


pred <- predict(xgb, testSum)  
head(pred, 10)  
label = getinfo(test, "label")
pred <- predict(xgb, test)
err <- as.numeric(sum(as.integer(pred > 0.5) != label))/length(label)
print(paste("test-error=", err))


predictions <- as.numeric(predict(xgb,test, outputmargin=TRUE) > .5)
truth = df.test$ptype
err <- rmse(truth, predictions)
auc <- auc(truth,predictions)