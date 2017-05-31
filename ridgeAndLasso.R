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


x <- as.matrix(sample.train[, -63:67])
y <- as.double(as.matrix(sample.train[,ncol(sample.train)]))

x.test <- as.matrix(sample.test[,-63:-67])
y.test <- as.double(as.matrix(sample.test[,ncol(sample.test)]))

# fitting the model(ridge: alpha = 0)
set.seed(999)
cv.ridge <- cv.glmnet(x, y, family = 'binomial', alpha = 0, parallel = T, standardize = T, type.measure = 'auc')


# results
plot(cv.ridge)
cv.ridge$lambda.min
cv.ridge$lambda.1se
coef(cv.ridge, s = cv.ridge$lambda.min)


# train ridge
ridge.probs <- predict(cv.ridge, s = cv.ridge$lambda.min, x, type = "response")
# contrasts(as.factor(y))
ridge.pred <- rep("0", nrow(x))
ridge.pred[ridge.probs > 0.5] = "1"
ridge.table.train <- table(ridge.pred, as.factor(y))
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
cv.lasso <- cv.glmnet(x, y, family = 'binomial', alpha = 1, parallel = T, standardize = T, type.measure = 'auc')


# results
plot(cv.lasso)
plot(cv.lasso$glmnet.fit, xvar = "lambda", label = T)
cv.lasso$lambda.min
cv.lasso$lambda.1se
coef(cv.lasso, s = cv.lasso$lambda.min)


# train lasso
lasso.probs <- predict(cv.lasso, s = cv.lasso$lambda.min, x, type = "response")
# contrasts(as.factor(y))
lasso.pred <- rep("0", nrow(x))
lasso.pred[lasso.probs > 0.5] = "1"
lasso.table.train <- table(lasso.pred, as.factor(y))
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





