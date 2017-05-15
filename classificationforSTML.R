# TO DO!!!!
  
  # COMPARISON OF SYSTEM.TIME()
  # AROUND 40:80:300 WITH K = 5 AND KERNEL AND GAMMA SET AS DEFAULT

  # COMPARISON OF ACCURACY
  # AROUND (80%:99%:85%) WITH WITH K = 5 AND KERNEL AND GAMMA SET AS DEFAULT
  
  # TRY MULTI COMBINATION OF FUNCTION

  # MSE OF EACH ALGORITHM
library(sca)

# DATA INPUT
mySample <- read.csv("E:\\Allinpay\\Data\\STLM_INS_CIRCLE\\workingPaper\\PrecisionMarketing\\data\\sample.csv",
                     header = TRUE,
                     stringsAsFactors = TRUE)
options(scipen=3)
rownames(mySample) <- mySample[,1]
mySample <- mySample[,-1]
mySample$stlm_ins_circle <- as.factor(mySample$stlm_ins_circle)

# linear combination with all weight set
# mySample$SUM <- apply(mySample[,2:9], 1, sum)
comb <- function(x){
  # weight <- c(1, 1, 1, 1, 1, 1, 1, 1)
  # weight <- c(5, 5, 10, 20, 20, 1, 1, 1)
  weight <- c(5, 50, 20, 20, 20, 1, 10, 1)
  y <- sum(weight*x)
  return(y)
}
mySample$SUM <- apply(mySample[,2:9], 1, comb)



# NORMAL DISTRIBUTION
# Label establish
mySample$NORM <- rnorm(length(mySample$stlm_ins_circle), mean = 0, sd = 3)
# mySample$NORM <- rnorm(length(mySample$stlm_ins_circle)) # rnor(n, mean = 0, sd = 1)
mySample$SUM.NORM <- mySample$SUM + mySample$NORM
mySample$channel.Nor <- rep("0", length(mySample$stlm_ins_circle))
mySample[which(mySample$SUM.NORM > 0),] <- transform(mySample[which(mySample$SUM.NORM > 0),], channel.Nor = "1")
mySample$channel.Nor <- as.factor(mySample$channel.Nor)

# UNIFORM
mySample$UNIFORM <- runif(length(mySample$stlm_ins_circle), min = -5, max = 5)
# mySample$UNIFORM <- runif(length(mySample$stlm_ins_circle), min = -1, max = 1) # runif(n, min = 0, max = 1)
mySample$SUM.UNIF <- mySample$SUM + mySample$UNIFORM
mySample$channel.Unif <- rep("0", length(mySample$stlm_ins_circle))
mySample[which(mySample$SUM.UNIF >0),] <- transform(mySample[which(mySample$SUM.UNIF > 0),], channel.Unif = "1")
mySample$channel.Unif <- as.factor(mySample$channel.Unif)

# EXPONENTIAL
mySample$EXPONENTIAL <- rexp(length(mySample$stlm_ins_circle), rate = 10)
# mySample$EXPONENTIAL <- rexp(length(mySample$stlm_ins_circle), rate = 1) # rexp(n, rate = 1)
mySample$SUM.EXPO <- mySample$SUM + mySample$EXPONENTIAL
mySample$channel.Exp <- rep("0", length(mySample$stlm_ins_circle))
mySample[which(mySample$SUM.EXPO >0),] <- transform(mySample[which(mySample$SUM.EXPO > 0),], channel.Exp = "1")
mySample$channel.Exp <- as.factor(mySample$channel.Exp)

# POISSON
mySample$POISSON <- rpois(length(mySample$stlm_ins_circle), lambda = 3)
# mySample$POISSON <- rpois(length(mySample$stlm_ins_circle), lambda = 2) # rpois(n, lambda)
mySample$SUM.POI <- mySample$SUM + mySample$POISSON
mySample$channel.Poi <- rep("0", length(mySample$stlm_ins_circle))
mySample[which(mySample$SUM.POI > 0),] <- transform(mySample[which(mySample$SUM.POI > 0),], channel.Poi = "1")
mySample$channel.Poi  <- as.factor(mySample$channel.Poi)

# STUDENT'S T-TEST
mySample$TTEST <- rt(length(mySample$stlm_ins_circle), df = 5, ncp = 2)
# mySample$TTEST <- rt(length(mySample$stlm_ins_circle), df = 1, ncp = 2) # rt(n, df, ncp)
mySample$SUM.TTEST <- mySample$SUM + mySample$TTEST
mySample$channel.Ttest <- rep("0", length(mySample$stlm_ins_circle))
mySample[which(mySample$SUM.TTEST > 0),] <- transform(mySample[which(mySample$SUM.TTEST > 0),], channel.Ttest = "1")
mySample$channel.Ttest <- as.factor(mySample$channel.Ttest)

# WEIBULL
mySample$WEIBULL <- rweibull(length(mySample$stlm_ins_circle),shape = 5, scale = 1)
# mySample$WEIBULL <- rweibull(length(mySample$stlm_ins_circle),shape = 1, scale = 1) # rweibull(n, shape, scale = 1)
mySample$SUM.WEIBULL <- mySample$SUM + mySample$WEIBULL
mySample$channel.WEIBULL <- rep("0", length(mySample$stlm_ins_circle))
mySample[which(mySample$SUM.WEIBULL > 0),] <- transform(mySample[which(mySample$SUM.WEIBULL > 0),], channel.WEIBULL = "1")
mySample$channel.WEIBULL <- as.factor(mySample$channel.WEIBULL)


# LOGIT
mySample$Logit <- rlogis(length(mySample$stlm_ins_circle), location = 0, scale = 3)
# mySample$Logit <- rlogis(length(mySample$stlm_ins_circle), location = 0, scale = 1) # rlogis(n, location = 0, scale = 1)
mySample$SUM.Logit <- mySample$SUM + mySample$Logit
mySample$channel.Logit <- rep("0", length(mySample$stlm_ins_circle))
mySample[which(mySample$SUM.Logit > 0),] <- transform(mySample[which(mySample$SUM.Logit > 0),], channel.Logit = "1")
mySample$channel.Logit <- as.factor(mySample$channel.Logit)


# LOGNORMAL
mySample$LogNormal <- rlnorm(length(mySample$stlm_ins_circle), meanlog = 0, sdlog = 5)
# mySample$LogNormal <- rlnorm(length(mySample$stlm_ins_circle), meanlog = 0, sdlog = 1)  # rlnorm(n, meanlog = 0, sdlog = 1)
mySample$SUM.LogNormal <- mySample$SUM + mySample$LogNormal
mySample$channel.LogNormal <- rep("0", length(mySample$stlm_ins_circle))
mySample[which(mySample$SUM.LogNormal > 0),] <- transform(mySample[which(mySample$SUM.LogNormal > 0),], channel.LogNormal = "1")
mySample$channel.LogNormal <- as.factor(mySample$channel.LogNormal)




# Samplit preparation with spliting into train and test
# Sample split into train and test
sample.train <- mySample[which(mySample$stlm_ins_circle == "1"),-1]
sample.test <- mySample[which(mySample$stlm_ins_circle == "0"),-1]

sample.train.Nor <- sample.train[,c(1:8,12)]
sample.test.Nor <- sample.test[,c(1:8,12)]

sample.train.Unif <- sample.train[,c(1:8,15)]
sample.test.Unif <- sample.test[,c(1:8,15)]

sample.train.Exp <- sample.train[,c(1:8,18)]
sample.test.Exp <- sample.test[,c(1:8,18)]

sample.train.Poi <- sample.train[,c(1:8,21)]
sample.test.Poi <- sample.test[,c(1:8,21)]

sample.train.Ttest <- sample.train[,c(1:8,24)]
sample.test.Ttest <- sample.test[,c(1:8,24)]

sample.train.WEIBULL <- sample.train[,c(1:8,27)]
sample.test.WEIBULL <- sample.test[,c(1:8,27)]

sample.train.Logit <- sample.train[,c(1:8,30)]
sample.test.Logit <- sample.test[,c(1:8,30)]

sample.train.LogNormal <- sample.train[,c(1:8,33)]
sample.test.LogNormal <- sample.test[,c(1:8,33)]


# statisticDescription <- function(x){
#   return(c(mean(x),var(x),min(x),max(x)))
# }
# 
# 
# TransferRate <- function(alpha){
#   TR <- (1-alpha) * accuracy[2,2]/sum(accuracy[2,])
#   return(TR)
# }
# 
# alpha.interval <- seq(0,1,0.1)
# 
# TRate <- data.frame()
predict.result.th <- c()
p <- c()

  # DISTANCE-BASED
  # Center Matrix
  center.Nor.1 <- colMeans(subset(sample.train.Nor, channel.Nor == "1")[,-ncol(sample.train.Nor)])
  center.Nor.0 <- colMeans(subset(sample.train.Nor, channel.Nor == "0")[,-ncol(sample.train.Nor)])

  center.Unif.1 <- colMeans(subset(sample.train.Unif, channel.Unif == "1")[,-ncol(sample.train.Unif)])
  center.Unif.0 <- colMeans(subset(sample.train.Unif, channel.Unif == "0")[,-ncol(sample.train.Unif)])

  center.Exp.1 <- colMeans(subset(sample.train.Exp, channel.Exp == "1")[,-ncol(sample.train.Exp)])
  center.Exp.0 <- colMeans(subset(sample.train.Exp, channel.Exp == "0")[,-ncol(sample.train.Exp)])

  center.Poi.1 <- colMeans(subset(sample.train.Poi, channel.Poi == "1")[,-ncol(sample.train.Poi)])
  center.Poi.0 <- colMeans(subset(sample.train.Poi, channel.Poi == "0")[,-ncol(sample.train.Poi)])

  center.Ttest.1 <- colMeans(subset(sample.train.Ttest, channel.Ttest == "1")[,-ncol(sample.train.Ttest)])
  center.Ttest.0 <- colMeans(subset(sample.train.Ttest, channel.Ttest == "0")[,-ncol(sample.train.Ttest)])
  
  center.WEIBULL.1 <- colMeans(subset(sample.train.WEIBULL, channel.WEIBULL == "1")[,-ncol(sample.train.WEIBULL)])
  center.WEIBULL.0 <- colMeans(subset(sample.train.WEIBULL, channel.WEIBULL == "0")[,-ncol(sample.train.WEIBULL)])
  
  center.Logit.1 <- colMeans(subset(sample.train.Logit, channel.Logit == "1")[,-ncol(sample.train.Logit)])
  center.Logit.0 <- colMeans(subset(sample.train.Logit, channel.Logit == "0")[,-ncol(sample.train.Logit)])

  center.LogNormal.1 <- colMeans(subset(sample.train.LogNormal, channel.LogNormal == "1")[,-ncol(sample.train.LogNormal)])
  center.LogNormal.0 <- colMeans(subset(sample.train.LogNormal, channel.LogNormal == "0")[,-ncol(sample.train.LogNormal)])
    
  cnames <- c("Zd_time", "Zmale", "Zage", "Ztotal_amt", "Ztotal_cnt", "ZifType0", "ZifType1", "ZifType2")
  rnames <- c("center.Nor.1", "center.Nor.0", "center.Unif.1", "center.Unif.0", "center.Exp.1",
            "center.Exp.0", "center.Poi.1", "center.Poi.0", "center.Ttest.1", "center.Ttest.0",
            "center.WEIBULL.1", "center.WEIBULL.0", "center.Logit.1", "center.Logit.0", "center.LogNormal.1", "center.LogNormal.0")
  center <- matrix(c(center.Nor.0,center.Nor.1,
            center.Unif.0, center.Unif.1,
            center.Exp.0, center.Exp.1,
            center.Poi.0, center.Poi.1,
            center.Ttest.0, center.Ttest.1,
            center.WEIBULL.0, center.WEIBULL.1,
            center.Logit.0, center.Logit.1,
            center.LogNormal.0, center.LogNormal.1), length(rnames), length(cnames), byrow = T,
            dimnames = list(rnames, cnames))

  # General euclidian distance compute
  dist.compute <- function(x,n){
    res <- c()
    distance.0 <- dist(rbind(center[2 * n - 1,], x), method = "euclidian")
    distance.1 <- dist(rbind(center[2 * n,], x), method = "euclidian")
    if(distance.1 <= distance.0){
      res <- 1
    }else{
      res <- 0
    }
    return(res)
  }
  

  # Classification Result and verification 
  system.time(test.class.Nor <- apply(sample.test.Nor[,1:8], 1, dist.compute, n = 1))
  accuracy <- table(test.class.Nor, sample.test.Nor$channel.Nor)
  print(accuracy)
  predict.result.th <- append(predict.result.th,percent((accuracy[1,1]+accuracy[2,2])/sum(accuracy), d =2))
  p <- append(p, percent(accuracy[2,2]/(accuracy[2,2] + accuracy[1,2]), d= 2))
  # transfer.rate <- lapply(alpha.interval, TransferRate)
  # TRate <- rbind(TRate, transfer.rate)
  
  system.time(test.class.Unif <- apply(sample.test.Unif[,1:8], 1, dist.compute, n = 2))
  accuracy <- table(test.class.Unif, sample.test.Unif$channel.Unif)
  print(accuracy)
  predict.result.th <- append(predict.result.th,percent((accuracy[1,1]+accuracy[2,2])/sum(accuracy), d =2))
  p <- append(p, percent(accuracy[2,2]/(accuracy[2,2] + accuracy[1,2]), d= 2))
  # transfer.rate <- lapply(alpha.interval, TransferRate)
  # TRate <- rbind(TRate, transfer.rate)
  
  
  system.time(test.class.Exp <- apply(sample.test.Exp[,1:8], 1, dist.compute, n = 3))
  accuracy <- table(test.class.Exp, sample.test.Exp$channel.Exp)
  print(accuracy)
  predict.result.th <- append(predict.result.th,percent((accuracy[1,1]+accuracy[2,2])/sum(accuracy), d =2))
  p <- append(p, percent(accuracy[2,2]/(accuracy[2,2] + accuracy[1,2]), d= 2))
  # transfer.rate <- lapply(alpha.interval, TransferRate)
  # TRate <- rbind(TRate, transfer.rate)
  
  
  system.time(test.class.Poi <- apply(sample.test.Poi[,1:8], 1, dist.compute, n = 4))
  accuracy <- table(test.class.Poi, sample.test.Poi$channel.Poi)
  print(accuracy)
  predict.result.th <- append(predict.result.th,percent((accuracy[1,1]+accuracy[2,2])/sum(accuracy), d =2))
  p <- append(p, percent(accuracy[2,2]/(accuracy[2,2] + accuracy[1,2]), d= 2))
  # transfer.rate <- lapply(alpha.interval, TransferRate)
  # TRate <- rbind(TRate, transfer.rate)
  
  system.time(test.class.Ttest <- apply(sample.test.Ttest[,1:8], 1, dist.compute, n = 5))
  accuracy <- table(test.class.Ttest, sample.test.Ttest$channel.Ttest)
  print(accuracy)
  predict.result.th <- append(predict.result.th,percent((accuracy[1,1]+accuracy[2,2])/sum(accuracy), d =2))
  p <- append(p, percent(accuracy[2,2]/(accuracy[2,2] + accuracy[1,2]), d= 2))
  # transfer.rate <- lapply(alpha.interval, TransferRate)
  # TRate <- rbind(TRate, transfer.rate)
  
  system.time(test.class.WEIBULL <- apply(sample.test.WEIBULL[,1:8], 1, dist.compute, n = 6))
  accuracy <- table(test.class.WEIBULL, sample.test.WEIBULL$channel.WEIBULL)
  print(accuracy)
  predict.result.th <- append(predict.result.th,percent((accuracy[1,1]+accuracy[2,2])/sum(accuracy), d =2))
  p <- append(p, percent(accuracy[2,2]/(accuracy[2,2] + accuracy[1,2]), d= 2))
  # transfer.rate <- lapply(alpha.interval, TransferRate)
  # TRate <- rbind(TRate, transfer.rate)
  
  system.time(test.class.Logit <- apply(sample.test.Logit[,1:8], 1, dist.compute, n = 7))
  accuracy <- table(test.class.Logit, sample.test.Logit$channel.Logit)
  print(accuracy)
  predict.result.th <- append(predict.result.th,percent((accuracy[1,1]+accuracy[2,2])/sum(accuracy), d =2))
  p <- append(p, percent(accuracy[2,2]/(accuracy[2,2] + accuracy[1,2]), d= 2))
  # transfer.rate <- lapply(alpha.interval, TransferRate)
  # TRate <- rbind(TRate, transfer.rate)
  
  system.time(test.class.LogNormal <- apply(sample.test.LogNormal[,1:8], 1, dist.compute, n = 8))
  accuracy <- table(test.class.LogNormal, sample.test.LogNormal$channel.LogNormal)
  print(accuracy)
  predict.result.th <- append(predict.result.th,percent((accuracy[1,1]+accuracy[2,2])/sum(accuracy), d =2))
  p <- append(p, percent(accuracy[2,2]/(accuracy[2,2] + accuracy[1,2]), d= 2))
  # transfer.rate <- lapply(alpha.interval, TransferRate)
  # TRate <- rbind(TRate, transfer.rate)
  
  
      
  # K-NN
  
  library(class)
  set.seed(1)
  
  # K-NN MODEL
  system.time(knn.pred.Nor <- knn(sample.train.Nor, sample.test.Nor, sample.train.Nor$channel.Nor, k = 5))
  # Verification of K-NN
  accuracy <- table(knn.pred.Nor, sample.test.Nor$channel.Nor)
  print(accuracy)
  predict.result.th <- append(predict.result.th,percent((accuracy[1,1]+accuracy[2,2])/sum(accuracy), d =2))
  p <- append(p, percent(accuracy[2,2]/(accuracy[2,2] + accuracy[1,2]), d= 2))
  # transfer.rate <- lapply(alpha.interval, TransferRate)
  # TRate <- rbind(TRate, transfer.rate)
  
  
  system.time(knn.pred.Unif <- knn(sample.train.Unif, sample.test.Unif, sample.train.Unif$channel.Unif, k = 5))
  accuracy <- table(knn.pred.Unif, sample.test.Unif$channel.Unif)
  print(accuracy)
  predict.result.th <- append(predict.result.th,percent((accuracy[1,1]+accuracy[2,2])/sum(accuracy), d =2))
  p <- append(p, percent(accuracy[2,2]/(accuracy[2,2] + accuracy[1,2]), d= 2))
  # transfer.rate <- lapply(alpha.interval, TransferRate)
  # TRate <- rbind(TRate, transfer.rate)
  

  system.time(knn.pred.Exp <- knn(sample.train.Exp, sample.test.Exp, sample.train.Exp$channel.Exp, k = 5))
  accuracy <- table(knn.pred.Exp, sample.test.Exp$channel.Exp)
  print(accuracy)
  predict.result.th <- append(predict.result.th,percent((accuracy[1,1]+accuracy[2,2])/sum(accuracy), d =2))
  p <- append(p, percent(accuracy[2,2]/(accuracy[2,2] + accuracy[1,2]), d= 2))
  # transfer.rate <- lapply(alpha.interval, TransferRate)
  # TRate <- rbind(TRate, transfer.rate)
  
  system.time(knn.pred.Poi <- knn(sample.train.Poi, sample.test.Poi, sample.train.Poi$channel.Poi, k = 5))
  accuracy <- table(knn.pred.Poi, sample.test.Poi$channel.Poi)
  print(accuracy)
  predict.result.th <- append(predict.result.th,percent((accuracy[1,1]+accuracy[2,2])/sum(accuracy), d =2))
  p <- append(p, percent(accuracy[2,2]/(accuracy[2,2] + accuracy[1,2]), d= 2))
  # transfer.rate <- lapply(alpha.interval, TransferRate)
  # TRate <- rbind(TRate, transfer.rate)
  
  system.time(knn.pred.Ttest <- knn(sample.train.Ttest, sample.test.Ttest, sample.train.Ttest$channel.Ttest, k = 5))
  accuracy <- table(knn.pred.Ttest , sample.test.Ttest$channel.Ttest)
  print(accuracy)
  predict.result.th <- append(predict.result.th,percent((accuracy[1,1]+accuracy[2,2])/sum(accuracy), d =2))
  p <- append(p, percent(accuracy[2,2]/(accuracy[2,2] + accuracy[1,2]), d= 2))
  # transfer.rate <- lapply(alpha.interval, TransferRate)
  # TRate <- rbind(TRate, transfer.rate)
  
  system.time(knn.pred.WEIBULL <- knn(sample.train.WEIBULL, sample.test.WEIBULL, sample.train.WEIBULL$channel.WEIBULL, k = 5))
  accuracy <- table(knn.pred.WEIBULL, sample.test.WEIBULL$channel.WEIBULL)
  print(accuracy)
  predict.result.th <- append(predict.result.th,percent((accuracy[1,1]+accuracy[2,2])/sum(accuracy), d =2))
  p <- append(p, percent(accuracy[2,2]/(accuracy[2,2] + accuracy[1,2]), d= 2))
  # transfer.rate <- lapply(alpha.interval, TransferRate)
  # TRate <- rbind(TRate, transfer.rate)
  
  system.time(knn.pred.Logit <- knn(sample.train.Logit, sample.test.Logit, sample.train.Logit$channel.Logit, k = 5))
  accuracy <- table(knn.pred.Logit, sample.test.Logit$channel.Logit)
  print(accuracy)
  predict.result.th <- append(predict.result.th,percent((accuracy[1,1]+accuracy[2,2])/sum(accuracy), d =2))
  p <- append(p, percent(accuracy[2,2]/(accuracy[2,2] + accuracy[1,2]), d= 2))
  # transfer.rate <- lapply(alpha.interval, TransferRate)
  # TRate <- rbind(TRate, transfer.rate)
  
  system.time(knn.pred.LogNormal <- knn(sample.train.LogNormal, sample.test.LogNormal, sample.train.LogNormal$channel.LogNormal, k = 5))
  accuracy <- table(knn.pred.LogNormal, sample.test.LogNormal$channel.LogNormal)
  print(accuracy)
  predict.result.th <- append(predict.result.th,percent((accuracy[1,1]+accuracy[2,2])/sum(accuracy), d =2))
  p <- append(p, percent(accuracy[2,2]/(accuracy[2,2] + accuracy[1,2]), d= 2))
  # transfer.rate <- lapply(alpha.interval, TransferRate)
  # TRate <- rbind(TRate, transfer.rate)
  
  

  # SVM
  
  set.seed(1)
  library(e1071)
  
  # Nor
  # Set the label as "1" and "-1"
  sample.train.Nor.svm <- sample.train.Nor
  sample.train.Nor.svm$channel.Nor <- as.character(sample.train.Nor.svm$channel.Nor)
  sample.train.Nor.svm$channel.Nor <- as.numeric(sample.train.Nor.svm$channel.Nor)
  sample.train.Nor.svm[which(sample.train.Nor.svm$channel.Nor == 0),] <- transform(sample.train.Nor.svm[which(sample.train.Nor.svm$channel.Nor == 0),], channel.Nor = -1)
  
  sample.test.Nor.svm <- sample.test.Nor
  sample.test.Nor.svm$channel.Nor <- as.character(sample.test.Nor.svm$channel.Nor)
  sample.test.Nor.svm$channel.Nor <- as.numeric(sample.test.Nor.svm$channel.Nor)
  sample.test.Nor.svm[which(sample.test.Nor.svm$channel.Nor == 0),] <- transform(sample.test.Nor.svm[which(sample.test.Nor.svm$channel.Nor == 0),], channel.Nor = -1)
  system.time(svmfit.Nor <- svm(channel.Nor~., data = sample.train.Nor.svm))
  system.time(pred.svm.Nor <- predict(svmfit.Nor, sample.test.Nor.svm))

  # Verification of SVM
  pred.svm.Nor <- ifelse(pred.svm.Nor < 0, -1, 1)
  accuracy <- table(pred.svm.Nor, sample.test.Nor.svm$channel.Nor)
  print(accuracy)
  predict.result.th <- append(predict.result.th,percent((accuracy[1,1]+accuracy[2,2])/sum(accuracy), d =2))
  p <- append(p, percent(accuracy[2,2]/(accuracy[2,2] + accuracy[1,2]), d= 2))
  # transfer.rate <- lapply(alpha.interval, TransferRate)
  # TRate <- rbind(TRate, transfer.rate)
  
  # Unif
  # Set the label as "1" and "-1"
  sample.train.Unif.svm <- sample.train.Unif
  sample.train.Unif.svm$channel.Unif <- as.character(sample.train.Unif.svm$channel.Unif)
  sample.train.Unif.svm$channel.Unif <- as.numeric(sample.train.Unif.svm$channel.Unif)
  sample.train.Unif.svm[which(sample.train.Unif.svm$channel.Unif == 0),] <- transform(sample.train.Unif.svm[which(sample.train.Unif.svm$channel.Unif == 0),], channel.Unif = -1)
  
  sample.test.Unif.svm <- sample.test.Unif
  sample.test.Unif.svm$channel.Unif <- as.character(sample.test.Unif.svm$channel.Unif)
  sample.test.Unif.svm$channel.Unif <- as.numeric(sample.test.Unif.svm$channel.Unif)
  sample.test.Unif.svm[which(sample.test.Unif.svm$channel.Unif == 0),] <- transform(sample.test.Unif.svm[which(sample.test.Unif.svm$channel.Unif == 0),], channel.Unif = -1)
  system.time(svmfit.Unif <- svm(channel.Unif~., data = sample.train.Unif.svm))
  system.time(pred.svm.Unif <- predict(svmfit.Unif, sample.test.Unif.svm))

    
  # Verification of SVM
  pred.svm.Unif <- ifelse(pred.svm.Unif < 0, -1, 1)
  accuracy <- table(pred.svm.Unif, sample.test.Unif.svm$channel.Unif)
  print(accuracy)
  predict.result.th <- append(predict.result.th,percent((accuracy[1,1]+accuracy[2,2])/sum(accuracy), d =2))
  p <- append(p, percent(accuracy[2,2]/(accuracy[2,2] + accuracy[1,2]), d= 2))
  # transfer.rate <- lapply(alpha.interval, TransferRate)
  # TRate <- rbind(TRate, transfer.rate)
  
  # Exp
  # Set the label as "1" and "-1"
  sample.train.Exp.svm <- sample.train.Exp
  sample.train.Exp.svm$channel.Exp <- as.character(sample.train.Exp.svm$channel.Exp)
  sample.train.Exp.svm$channel.Exp <- as.numeric(sample.train.Exp.svm$channel.Exp)
  sample.train.Exp.svm[which(sample.train.Exp.svm$channel.Exp == 0),] <- transform(sample.train.Exp.svm[which(sample.train.Exp.svm$channel.Exp == 0),], channel.Exp = -1)
  
  sample.test.Exp.svm <- sample.test.Exp
  sample.test.Exp.svm$channel.Exp <- as.character(sample.test.Exp.svm$channel.Exp)
  sample.test.Exp.svm$channel.Exp <- as.numeric(sample.test.Exp.svm$channel.Exp)
  sample.test.Exp.svm[which(sample.test.Exp.svm$channel.Exp == 0),] <- transform(sample.test.Exp.svm[which(sample.test.Exp.svm$channel.Exp == 0),], channel.Exp = -1)
  system.time(svmfit.Exp <- svm(channel.Exp~., data = sample.train.Exp.svm))
  system.time(pred.svm.Exp <- predict(svmfit.Exp, sample.test.Exp.svm))

  
  
  # Verification of SVM
  pred.svm.Exp <- ifelse(pred.svm.Exp < 0, -1, 1)
  accuracy <- table(pred.svm.Exp, sample.test.Exp.svm$channel.Exp)
  print(accuracy)
  predict.result.th <- append(predict.result.th,percent((accuracy[1,1]+accuracy[2,2])/sum(accuracy), d =2))
  p <- append(p, percent(accuracy[2,2]/(accuracy[2,2] + accuracy[1,2]), d= 2))
  # transfer.rate <- lapply(alpha.interval, TransferRate)
  # TRate <- rbind(TRate, transfer.rate)
  
  # Poi
  # Set the label as "1" and "-1"
  sample.train.Poi.svm <- sample.train.Poi
  sample.train.Poi.svm$channel.Poi <- as.character(sample.train.Poi.svm$channel.Poi)
  sample.train.Poi.svm$channel.Poi <- as.numeric(sample.train.Poi.svm$channel.Poi)
  sample.train.Poi.svm[which(sample.train.Poi.svm$channel.Poi == 0),] <- transform(sample.train.Poi.svm[which(sample.train.Poi.svm$channel.Poi == 0),], channel.Poi = -1)
  
  sample.test.Poi.svm <- sample.test.Poi
  sample.test.Poi.svm$channel.Poi <- as.character(sample.test.Poi.svm$channel.Poi)
  sample.test.Poi.svm$channel.Poi <- as.numeric(sample.test.Poi.svm$channel.Poi)
  sample.test.Poi.svm[which(sample.test.Poi.svm$channel.Poi == 0),] <- transform(sample.test.Poi.svm[which(sample.test.Poi.svm$channel.Poi == 0),], channel.Poi = -1)
  system.time(svmfit.Poi <- svm(channel.Poi~., data = sample.train.Poi.svm))
  system.time(pred.svm.Poi <- predict(svmfit.Poi, sample.test.Poi.svm))
  
  
  # Verification of SVM
  pred.svm.Poi <- ifelse(pred.svm.Poi < 0, -1, 1)
  accuracy <- table(pred.svm.Poi, sample.test.Poi.svm$channel.Poi)
  print(accuracy)
  predict.result.th <- append(predict.result.th,percent((accuracy[1,1]+accuracy[2,2])/sum(accuracy), d =2))
  p <- append(p, percent(accuracy[2,2]/(accuracy[2,2] + accuracy[1,2]), d= 2))
  # transfer.rate <- lapply(alpha.interval, TransferRate)
  # TRate <- rbind(TRate, transfer.rate)
  
  
  # Ttest
  # Set the label as "1" and "-1"
  sample.train.Ttest.svm <- sample.train.Ttest
  sample.train.Ttest.svm$channel.Ttest <- as.character(sample.train.Ttest.svm$channel.Ttest)
  sample.train.Ttest.svm$channel.Ttest <- as.numeric(sample.train.Ttest.svm$channel.Ttest)
  sample.train.Ttest.svm[which(sample.train.Ttest.svm$channel.Ttest == 0),] <- transform(sample.train.Ttest.svm[which(sample.train.Ttest.svm$channel.Ttest == 0),], channel.Ttest = -1)
  
  sample.test.Ttest.svm <- sample.test.Ttest
  sample.test.Ttest.svm$channel.Ttest <- as.character(sample.test.Ttest.svm$channel.Ttest)
  sample.test.Ttest.svm$channel.Ttest <- as.numeric(sample.test.Ttest.svm$channel.Ttest)
  sample.test.Ttest.svm[which(sample.test.Ttest.svm$channel.Ttest == 0),] <- transform(sample.test.Ttest.svm[which(sample.test.Ttest.svm$channel.Ttest == 0),], channel.Ttest = -1)
  system.time(svmfit.Ttest <- svm(channel.Ttest~., data = sample.train.Ttest.svm))
  system.time(pred.svm.Ttest <- predict(svmfit.Ttest, sample.test.Ttest.svm))

  
  # Verification of SVM
  pred.svm.Ttest <- ifelse(pred.svm.Ttest < 0, -1, 1)
  accuracy <- table(pred.svm.Ttest, sample.test.Ttest.svm$channel.Ttest)
  print(accuracy)
  predict.result.th <- append(predict.result.th,percent((accuracy[1,1]+accuracy[2,2])/sum(accuracy), d =2))
  p <- append(p, percent(accuracy[2,2]/(accuracy[2,2] + accuracy[1,2]), d= 2))
  # transfer.rate <- lapply(alpha.interval, TransferRate)
  # TRate <- rbind(TRate, transfer.rate)
  
  
  # WEIBULL
  # Set the label as "1" and "-1"
  sample.train.WEIBULL.svm <- sample.train.WEIBULL
  sample.train.WEIBULL.svm$channel.WEIBULL <- as.character(sample.train.WEIBULL.svm$channel.WEIBULL)
  sample.train.WEIBULL.svm$channel.WEIBULL <- as.numeric(sample.train.WEIBULL.svm$channel.WEIBULL)
  sample.train.WEIBULL.svm[which(sample.train.WEIBULL.svm$channel.WEIBULL == 0),] <- transform(sample.train.WEIBULL.svm[which(sample.train.WEIBULL.svm$channel.WEIBULL == 0),], channel.WEIBULL = -1)
  
  sample.test.WEIBULL.svm <- sample.test.WEIBULL
  sample.test.WEIBULL.svm$channel.WEIBULL <- as.character(sample.test.WEIBULL.svm$channel.WEIBULL)
  sample.test.WEIBULL.svm$channel.WEIBULL <- as.numeric(sample.test.WEIBULL.svm$channel.WEIBULL)
  sample.test.WEIBULL.svm[which(sample.test.WEIBULL.svm$channel.WEIBULL == 0),] <- transform(sample.test.WEIBULL.svm[which(sample.test.WEIBULL.svm$channel.WEIBULL == 0),], channel.WEIBULL = -1)
  system.time(svmfit.WEIBULL <- svm(channel.WEIBULL~., data = sample.train.WEIBULL.svm))
  system.time(pred.svm.WEIBULL <- predict(svmfit.WEIBULL, sample.test.WEIBULL.svm))
  
  
  # Verification of SVM
  pred.svm.WEIBULL <- ifelse(pred.svm.WEIBULL < 0, -1, 1)
  accuracy <- table(pred.svm.WEIBULL, sample.test.WEIBULL.svm$channel.WEIBULL)
  print(accuracy)
  predict.result.th <- append(predict.result.th,percent((accuracy[1,1]+accuracy[2,2])/sum(accuracy), d =2))
  p <- append(p, percent(accuracy[2,2]/(accuracy[2,2] + accuracy[1,2]), d= 2))
  # transfer.rate <- lapply(alpha.interval, TransferRate)
  # TRate <- rbind(TRate, transfer.rate)
  
  
  
  
  # Logit
  # Set the label as "1" and "-1"
  sample.train.Logit.svm <- sample.train.Logit
  sample.train.Logit.svm$channel.Logit <- as.character(sample.train.Logit.svm$channel.Logit)
  sample.train.Logit.svm$channel.Logit <- as.numeric(sample.train.Logit.svm$channel.Logit)
  sample.train.Logit.svm[which(sample.train.Logit.svm$channel.Logit == 0),] <- transform(sample.train.Logit.svm[which(sample.train.Logit.svm$channel.Logit == 0),], channel.Logit = -1)
  
  sample.test.Logit.svm <- sample.test.Logit
  sample.test.Logit.svm$channel.Logit <- as.character(sample.test.Logit.svm$channel.Logit)
  sample.test.Logit.svm$channel.Logit <- as.numeric(sample.test.Logit.svm$channel.Logit)
  sample.test.Logit.svm[which(sample.test.Logit.svm$channel.Logit == 0),] <- transform(sample.test.Logit.svm[which(sample.test.Logit.svm$channel.Logit == 0),], channel.Logit = -1)
  system.time(svmfit.Logit <- svm(channel.Logit~., data = sample.train.Logit.svm))
  system.time(pred.svm.Logit <- predict(svmfit.Logit, sample.test.Logit.svm))
  
  
  # Verification of SVM
  pred.svm.Logit <- ifelse(pred.svm.Logit < 0, -1, 1)
  accuracy <- table(pred.svm.Logit, sample.test.Logit.svm$channel.Logit)
  print(accuracy)
  predict.result.th <- append(predict.result.th,percent((accuracy[1,1]+accuracy[2,2])/sum(accuracy), d =2))
  p <- append(p, percent(accuracy[2,2]/(accuracy[2,2] + accuracy[1,2]), d= 2))
  # transfer.rate <- lapply(alpha.interval, TransferRate)
  # TRate <- rbind(TRate, transfer.rate)
  
  
  
  
  # LogNormal
  # Set the label as "1" and "-1"
  sample.train.LogNormal.svm <- sample.train.LogNormal
  sample.train.LogNormal.svm$channel.LogNormal <- as.character(sample.train.LogNormal.svm$channel.LogNormal)
  sample.train.LogNormal.svm$channel.LogNormal <- as.numeric(sample.train.LogNormal.svm$channel.LogNormal)
  sample.train.LogNormal.svm[which(sample.train.LogNormal.svm$channel.LogNormal == 0),] <- transform(sample.train.LogNormal.svm[which(sample.train.LogNormal.svm$channel.LogNormal == 0),], channel.LogNormal = -1)
  
  sample.test.LogNormal.svm <- sample.test.LogNormal
  sample.test.LogNormal.svm$channel.LogNormal <- as.character(sample.test.LogNormal.svm$channel.LogNormal)
  sample.test.LogNormal.svm$channel.LogNormal <- as.numeric(sample.test.LogNormal.svm$channel.LogNormal)
  sample.test.LogNormal.svm[which(sample.test.LogNormal.svm$channel.LogNormal == 0),] <- transform(sample.test.LogNormal.svm[which(sample.test.LogNormal.svm$channel.LogNormal == 0),], channel.LogNormal = -1)
  system.time(svmfit.LogNormal <- svm(channel.LogNormal~., data = sample.train.LogNormal.svm))
  system.time(pred.svm.LogNormal <- predict(svmfit.LogNormal, sample.test.LogNormal.svm))
  
  
  # Verification of SVM
  pred.svm.LogNormal <- ifelse(pred.svm.LogNormal < 0, -1, 1)
  accuracy <- table(pred.svm.LogNormal, sample.test.LogNormal.svm$channel.LogNormal)
  print(accuracy)
  predict.result.th <- append(predict.result.th,percent((accuracy[1,1]+accuracy[2,2])/sum(accuracy), d =2))
  p <- append(p, percent(accuracy[2,2]/(accuracy[2,2] + accuracy[1,2]), d= 2))
  # transfer.rate <- lapply(alpha.interval, TransferRate)
  # TRate <- rbind(TRate, transfer.rate)
  

  
  
  # colnames(TRate) <- c(alpha.interval)
  
  
  # const <- TRate
  # d.const <- const[1:8,]
  # d.const <- apply(d.const, 2, mean)
  # d.const <- round(d.const, 2)
  # k.const <- const[9:16,]
  # k.const <- apply(k.const, 2, mean)
  # k.const <- round(k.const, 2)
  # s.const <- const[17:24,]
  # s.const <- apply(s.const, 2, mean)
  # s.const <- round(s.const, 2)
  
  
  
  
  
  
  
  
  
  