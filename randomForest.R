# RANDOM FOREST (mtry: p/3 with regression trees and sqrt(p) with classification trees)
# 对基决策树的每个结点，先从d个属性中随机选择k个属性，再从这个子集中选择一个最优属性用于划分，推荐使用k = log(2)d
# optimal mtry could be determined by tuneRF() but may cause bias


# without QC:      train accuracy(1, 1) test accuracy(0.95, 0.55) Gini 0.74
# with QCGROUP:    train accuracy(1, 1) test accuracy(0.95, 0.57) Gini 0.79
# with QCOneGroup: train accuracy(1, 1) test accuracy(0.95, 0.60) Gini 0.73
# with QCSingle:   train accuracy(1, 1) test accuracy(0.96, 0.59) Gini 0.78



library(randomForest)

# DATA INPUT
mysample <- read.csv("E:\\Allinpay\\Data\\riskData\\sample.csv",
  # "E:\\Allinpay\\Data\\riskData\\withQCGroup.csv",
  # "E:\\Allinpay\\Data\\riskData\\withQCOneGroup.csv",
  # "E:\\Allinpay\\Data\\riskData\\withQCSingle.csv",
  header = T,
  stringsAsFactors = T)
options(scipen=3)


length.train <- ceiling(nrow(mysample)*0.6)
train <- sample(nrow(mysample), length.train)

# drop variables performed week towards dependent variables
# sample.train <- mysample[train,c(-16:-17,-40,-46,-62:-66)]
# sample.test <- mysample[-train,c(-16:-17,-40,-46,-62:-66)]

sample.train <- mysample[train,]
sample.test <- mysample[-train,]


sample.train$OVERDUE <- as.factor(sample.train$OVERDUE)

x.train <- sample.train[,-ncol(sample.train)]
y.train <- as.factor(sample.train[,ncol(sample.test)])

x.test <- sample.test[,-ncol(sample.test)]
y.test <- as.factor(sample.test[,ncol(sample.test)])


set.seed(1)
rf.fit <- randomForest(OVERDUE~., sample.train, ntree = 500)  # mtry = 8, improtance = T
# summary(rf.fit)
# predict <- predict(rf.fit, x.test)

# Measuring variable importance: The former is based upon the mean decrease of accuracy in predictions on the out of bag samples
# when a given variable is excluded from the model; The latter is a measure of the total decrease in node impurity that
# results from splits over that variable, averaged over all trees.
importance(rf.fit)

varImpPlot(rf.fit)

predicted.train <- predict(rf.fit, x.train, type = "prob")[,2]
pred.train <- rep("0", nrow(x.train))
pred.train[predicted.train > 0.5] = "1"
train.table.rate <- table(pred.train, y.train)
(train.table.rate[1,1]+train.table.rate[2,2])/(sum(train.table.rate))
train.table.rate[2,2]/(train.table.rate[1,2]+train.table.rate[2,2])


predicted.test <- predict(rf.fit, x.test, type = "prob")[,2]
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

