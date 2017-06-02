# gradient boosting machine(with CV)

# without QC:      train accuracy(0.98, 0.80) test accuracy(0.95, 0.60) Gini 0.83
# with QCGROUP:    train accuracy(0.98, 0.82) test accuracy(0.95, 0.62) Gini 0.85
# with QCOneGroup: train accuracy(0.97, 0.78) test accuracy(0.95, 0.63) Gini 0.82
# with QCSingle:   train accuracy(0.98, 0.81) test accuracy(0.95, 0.62) Gini 0.85

library(caret)
library(gbm)
library(plyr)
library(survival)

# DATA INPUT
mysample <- read.csv(# "E:\\Allinpay\\Data\\riskData\\\\sample.csv",
  # "E:\\Allinpay\\Data\\riskData\\withQCGroup.csv",
  # "E:\\Allinpay\\Data\\riskData\\withQCOneGroup.csv",
  "E:\\Allinpay\\Data\\riskData\\withQCSingle.csv",
  header = T,
  stringsAsFactors = T)
options(scipen=3)


length.train <- ceiling(nrow(mysample)*0.6)
train <- sample(nrow(mysample), length.train)

sample.train <- mysample[train,]
sample.test <- mysample[-train,]


sample.train$OVERDUE <- as.factor(sample.train$OVERDUE)

x.train <- sample.train[,-ncol(sample.train)]
y.train <- as.factor(sample.train[,ncol(sample.test)])

x.test <- sample.test[,-ncol(sample.test)]
y.test <- as.factor(sample.test[,ncol(sample.test)])


fitControl <- trainControl(method = "cv",
                           number = 10)

tune_Grid <- expand.grid(interaction.depth = 2,  # the complexity of the tree i.e. total number of splits
                                                 # it has to perform on a tree
                         # setted as 6 could get a much high accuracy
                         n.trees = 500,  # number of iterations i.e. tree which will be taken to grow the trees
                         shrinkage = 0.1, # learning rate
                         n.minobsinnode = 10) # minimum number of training samples required in a node to perform splitting

set.seed(825)
fit <- train(OVERDUE~., data = sample.train,
             method = "gbm",
             trControl = fitControl,
             verbose = F, # no output generated
             tuneGrid = tune_Grid)

predicted.train <- predict(fit, x.train, type = "prob")[,2]
pred.train <- rep("0", nrow(x.train))
pred.train[predicted.train > 0.5] = "1"
train.table.rate <- table(pred.train, y.train)
(train.table.rate[1,1]+train.table.rate[2,2])/(sum(train.table.rate))
train.table.rate[2,2]/(train.table.rate[1,2]+train.table.rate[2,2])


predicted.test <- predict(fit, x.test, type = "prob")[,2]
pred.test <- rep("0", nrow(x.test))
pred.test[predicted.test > 0.5] = "1"
test.table.rate <- table(pred.test, y.test)
(test.table.rate[1,1]+test.table.rate[2,2])/(sum(test.table.rate))
test.table.rate[2,2]/(test.table.rate[1,2]+test.table.rate[2,2])


# Gini index and Lorenz Curve
library(ineq)
gini.index <- Gini(predicted.test)
Distr <- predicted.test
Distr <- Lc(Distr, n = rep(1,  length(Distr)), plot = F)
plot(Distr$p, Distr$L,
     col = "black",
     type = "b",
     lty = 1,
     lwd = 3,
     main = "Lorenz Curve for Distributions")
points(c(0,1), c(0,1), type = "l", lty = 2, lwd = 2, col = "grey")


library(ROCR)
pred <- prediction(predicted.test, y.test)
perf <- performance(pred, measure = "rec", x.measure = "rpp")
plot(perf, colorize = T)
grid(5, 5, lwd = 1)
points(c(0,1), c(0,1), type = "l", lty = 2, lwd = 2, col = "grey")