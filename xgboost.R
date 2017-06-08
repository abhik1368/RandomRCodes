setwd("/Users/wang/Desktop/kaggle/influencers-in-social-networks/")
library(xgboost)

train = read.csv('train.csv', header = TRUE)
test = read.csv('test.csv', header = TRUE) 
y = train[,1]
names(train)

train = as.matrix(train[,-1])
test = as.matrix(test)
# colnames(train)
# colnames(test)

new.train = cbind(train[,12:22], train[,1:11]) #1-11 is A, 12-22 is B

# colnames(train[,12:22])
# colnames(train[,1:11])

train = rbind(train, new.train)
y = c(y, 1-y)
x = rbind(train, test)

# calculate ratio followers/following

calcRatio = function(dat, i, j, lambda = 1){
    (dat[,i]+lambda)/(dat[,j]+lambda)
}

A.follow.ratio = calcRatio(x,1,2)
A.mention.ratio = calcRatio(x,4,6)
A.retweet.ratio = calcRatio(x,5,7)

A.follow.post = calcRatio(x,1,8)
A.mention.post = calcRatio(x,4,8)
A.retweet.post = calcRatio(x,5,8)

B.follow.ratio = calcRatio(x,12,13)
B.mention.ratio = calcRatio(x,15,17)
B.retweet.ratio = calcRatio(x,16,18)

B.follow.post = calcRatio(x,12,19)
B.mention.post = calcRatio(x,15,19)
B.retweet.post = calcRatio(x,16,19)

x = cbind(x[,1:11], 
          A.follow.ratio, A.mention.ratio, A.retweet.ratio,
          A.follow.post, A.mention.post, A.retweet.post,
          x[,12:22],
          B.follow.ratio, B.mention.ratio, B.retweet.ratio,
          B.follow.post, B.mention.post, B.retweet.post)

AB.diff = x[,1:17]-x[,18:34]

x = cbind(x, AB.diff)
train = x[1:nrow(train),]
test = x[-(1:nrow(train)),]

set.seed(1024)
cv.res = xgb.cv(data = train, nfold = 3, label = y, nrounds = 100, verbose = FALSE, 
                objective = 'binary:logistic', eval_metric = 'auc')

library(ggplot2)
ggplot(cv.res, aes(1:100)) +
    geom_line(color = 'red',  aes(y = test.auc.mean)) +
    geom_line(color = 'blue', aes(y = train.auc.mean)) +
    xlab("index") +
    ylab("AUC")

set.seed(1024)
cv.res = xgb.cv(data = train, nfold = 3, label = y, nrounds = 3000, 
                verbose = FALSE, objective = 'binary:logistic', eval_metric = 'auc',
                eta = 0.005, gamma = 1, lambda = 3, nthread = 8, max_depth = 4,
                min_child_weight = 1, subsample = 0.8, colsample_bytree = 0.8)

ggplot(cv.res, aes(1:3000)) +
    geom_line(color = 'red',  aes(y = test.auc.mean)) +
    geom_line(color = 'blue', aes(y = train.auc.mean)) +
    xlab("index") +
    ylab("AUC")

bestRound = which.max(as.matrix(cv.res)[,3] - as.matrix(cv.res)[,4])
bestRound
cv.res[bestRound,]

set.seed(1024)
bst = xgboost(data = train, nfold = 3, label = y, nrounds = 3000, 
              verbose = FALSE, objective = 'binary:logistic', eval_metric = 'auc',
              eta = 0.005, gamma = 1, lambda = 3, nthread = 8, max_depth = 4,
              min_child_weight = 1, subsample = 0.8, colsample_bytree = 0.8)
preds = predict(bst, test, ntreelimit = bestRound)

# prepare for submission
result = data.frame(Id= 1:nrow(test), Choice = preds)
write.csv(result, 'submssion.csv', quote = F, row.names = F)
