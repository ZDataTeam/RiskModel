# ridge regression and lasso regression to do regularization

# for ridge: train accuracy(0.9497545, 0.5229358) test accuracy(0.9428076, 0.4956522)
# for lasso: train accuracy(0.9537973, 0.5504587) test accuracy(0.9493068, 0.5347826)

require(glmnet)

# DATA INPUT
mysample <- read.csv("E:\\Allinpay\\Data\\LOAN\\sqlExport\\sample.csv",
                     header = T,
                     stringsAsFactors = T)
options(scipen=3)


length.train <- ceiling(nrow(mysample)*0.6)
train <- sample(nrow(mysample), length.train)
sample.train <- mysample[train,]
sample.test <- mysample[-train,]


x.train <- as.matrix(sample.train[, -63:-67])
y.train <- as.double(as.matrix(sample.train[,ncol(sample.train)]))

x.test <- as.matrix(sample.test[,-63:-67])
y.test <- as.double(as.matrix(sample.test[,ncol(sample.test)]))

# fitting the model(ridge: alpha = 0)
set.seed(999)
cv.ridge <- cv.glmnet(x.train, y.train, family = 'binomial', alpha = 0, parallel = T, standardize = T, type.measure = 'auc')


# results
plot(cv.ridge)
cv.ridge$lambda.min
cv.ridge$lambda.1se
coef(cv.ridge, s = cv.ridge$lambda.min)


# train ridge
ridge.probs <- predict(cv.ridge, s = cv.ridge$lambda.min, x.train, type = "response")
# contrasts(as.factor(y.test))
ridge.pred <- rep("0", nrow(x.train))
ridge.pred[ridge.probs > 0.5] = "1"
ridge.table.train <- table(ridge.pred, as.factor(y.train))
(ridge.table.train[1,1]+ridge.table.train[2,2])/sum(ridge.table.train)
ridge.table.train[2,2]/(ridge.table.train[1,2]+ridge.table.train[2,2])


# test ridge
ridge.probs <- predict(cv.ridge, s = cv.ridge$lambda.min, x.test, type = "response")
# contrasts(as.factor(y.test))
ridge.pred <- rep("0", nrow(x.test))
ridge.pred[ridge.probs > 0.5] = "1"
ridge.table.test <- table(ridge.pred, as.factor(y.test))
(ridge.table.test[1,1]+ridge.table.test[2,2])/sum(ridge.table.test)
ridge.table.test[2,2]/(ridge.table.test[1,2]+ridge.table.test[2,2])




# fitting the model(lasso: alpha = 1)
set.seed(999)
cv.lasso <- cv.glmnet(x.train, y.train, family = 'binomial', alpha = 1, parallel = T, standardize = T, type.measure = 'auc')


# results
plot(cv.lasso)
plot(cv.lasso$glmnet.fit, xvar = "lambda", label = T)
cv.lasso$lambda.min
cv.lasso$lambda.1se
coef(cv.lasso, s = cv.lasso$lambda.min)


# train lasso
lasso.probs <- predict(cv.lasso, s = cv.lasso$lambda.min, x.train, type = "response")
# contrasts(as.factor(y.test))
lasso.pred <- rep("0", nrow(x.train))
lasso.pred[lasso.probs > 0.5] = "1"
lasso.table.train <- table(lasso.pred, as.factor(y.train))
(lasso.table.train[1,1]+lasso.table.train[2,2])/sum(lasso.table.train)
lasso.table.train[2,2]/(lasso.table.train[1,2]+lasso.table.train[2,2])


# test lasso
lasso.probs <- predict(cv.lasso, s = cv.lasso$lambda.min, x.test, type = "response")
# contrasts(as.factor(y.test))
lasso.pred <- rep("0", nrow(x.test))
lasso.pred[lasso.probs > 0.5] = "1"
lasso.table.test <- table(lasso.pred, as.factor(y.test))
(lasso.table.test[1,1]+lasso.table.test[2,2])/sum(lasso.table.test)
lasso.table.test[2,2]/(lasso.table.test[1,2]+lasso.table.test[2,2])





# Gini index and Lorenz Curve
library(ineq)
gini.index <- Gini(ridge.probs)
Distr <- ridge.probs
Distr <- Lc(Distr, n = rep(1,  length(Distr)), plot = F)
plot(Distr$p, Distr$L,
     col = "black",
     type = "b",
     lty = 1,
     lwd = 3,
     main = "Lorenz Curve for Distributions")
points(c(0,1), c(0,1), type = "l", lty = 2, lwd = 2, col = "grey")



library(ROCR)
pred <- prediction(ridge.probs, y.test)
perf <- performance(pred, measure = "rec", x.measure = "rpp")
plot(perf, colorize = T)
grid(5, 5, lwd = 1)
points(c(0,1), c(0,1), type = "l", lty = 2, lwd = 2, col = "grey")


gini.index <- Gini(lasso.probs)
Distr <- lasso.probs
Distr <- Lc(Distr, n = rep(1,  length(Distr)), plot = F)
plot(Distr$p, Distr$L,
     col = "black",
     type = "b",
     lty = 1,
     lwd = 3,
     main = "Lorenz Curve for Distributions")
points(c(0,1), c(0,1), type = "l", lty = 2, lwd = 2, col = "grey")

pred <- prediction(lasso.probs, y.test)
perf <- performance(pred, measure = "rec", x.measure = "rpp")
plot(perf, colorize = T)
grid(5, 5, lwd = 1)
points(c(0,1), c(0,1), type = "l", lty = 2, lwd = 2, col = "grey")






