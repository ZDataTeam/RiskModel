# TO DO!!!!
  
  # COMPARISON OF SYSTEM.TIME()
  # AROUND 40:80:300 WITH K = 5 AND KERNEL AND GAMMA SET AS DEFAULT

  # COMPARISON OF ACCURACY
  # AROUND (80%:99%:85%) WITH WITH K = 5 AND KERNEL AND GAMMA SET AS DEFAULT
  
  # TRY MULTI COMBINATION OF FUNCTION


# DATA INPUT
mySample <- read.csv("E:\\Allinpay\\Data\\STLM_INS_CIRCLE\\workingPaper\\data\\sample.csv",
                     header = TRUE,
                     stringsAsFactors = TRUE)
options(scipen=3)
rownames(mySample) <- mySample[,1]
mySample <- mySample[,-1]
mySample$stlm_ins_circle <- as.factor(mySample$stlm_ins_circle)

# linear combination with all weight set
# mySample$SUM <- apply(mySample[,2:9], 1, sum)
comb <- function(x){
  weight <- c(5,5,10,20,20,1,1,1)
  y <- sum(weight*x)
  return(y)
}
mySample$SUM <- apply(mySample[,2:9], 1, comb)



# NORMAL DISTRIBUTION
# Label establish
mySample$NORM <- rnorm(length(mySample$stlm_ins_circle))
mySample$SUM.NORM <- mySample$SUM + mySample$NORM
mySample$channel.Nor <- rep("0", length(mySample$stlm_ins_circle))
mySample[which(mySample$SUM.NORM > 0),] <- transform(mySample[which(mySample$SUM.NORM > 0),], channel.Nor = "1")
mySample$channel.Nor <- as.factor(mySample$channel.Nor)

# UNIFORM
mySample$UNIFORM <- runif(length(mySample$stlm_ins_circle), min = -1, max = 1)
mySample$SUM.UNIF <- mySample$SUM + mySample$UNIFORM
mySample$channel.Unif <- rep("0", length(mySample$stlm_ins_circle))
mySample[which(mySample$SUM.UNIF >0),] <- transform(mySample[which(mySample$SUM.UNIF > 0),], channel.Unif = "1")
mySample$channel.Unif <- as.factor(mySample$channel.Unif)

# EXPONENTIAL
mySample$EXPONENTIAL <- rexp(length(mySample$stlm_ins_circle), rate = 1)
mySample$SUM.EXPO <- mySample$SUM + mySample$EXPONENTIAL
mySample$channel.Exp <- rep("0", length(mySample$stlm_ins_circle))
mySample[which(mySample$SUM.EXPO >0),] <- transform(mySample[which(mySample$SUM.EXPO > 0),], channel.Exp = "1")
mySample$channel.Exp <- as.factor(mySample$channel.Exp)

# POISSON
mySample$POISSON <- rpois(length(mySample$stlm_ins_circle), lambda = 2)
mySample$SUM.POI <- mySample$SUM + mySample$POISSON
mySample$channel.Poi <- rep("0", length(mySample$stlm_ins_circle))
mySample[which(mySample$SUM.POI > 0),] <- transform(mySample[which(mySample$SUM.POI > 0),], channel.Poi = "1")
mySample$channel.Poi  <- as.factor(mySample$channel.Poi)

# STUDENT'S T-TEST
mySample$TTEST <- rt(length(mySample$stlm_ins_circle), df = 1, ncp = 2)
mySample$SUM.TTEST <- mySample$SUM + mySample$TTEST
mySample$channel.Ttest <- rep("0", length(mySample$stlm_ins_circle))
mySample[which(mySample$SUM.TTEST > 0),] <- transform(mySample[which(mySample$SUM.TTEST > 0),], channel.Ttest = "1")
mySample$channel.Ttest <- as.factor(mySample$channel.Ttest)

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

  cnames <- c("Zd_time", "Zmale", "Zage", "Ztotal_amt", "Ztotal_cnt", "ZifType0", "ZifType1", "ZifType2")
  rnames <- c("center.Nor.1", "center.Nor.0", "center.Unif.1", "center.Unif.0", "center.Exp.1",
            "center.Exp.0", "center.Poi.1", "center.Poi.0", "center.Ttest.1", "center.Ttest.0")
  center <- matrix(c(center.Nor.0,center.Nor.1,
            center.Unif.0, center.Unif.1,
            center.Exp.0, center.Exp.1,
            center.Poi.0, center.Poi.1,
            center.Ttest.0, center.Ttest.1), 10, 8, byrow = T,
            dimnames = list(rnames, cnames))

  # General euclidian distance compute
  dist.compute <- function(x,n){
    res <- c()
    distance.0 <- dist(rbind(center[2*n-1,], x), method = "euclidian")
    distance.1 <- dist(rbind(center[2*n,], x), method = "euclidian")
    if(distance.1 <= distance.0){
      res <- 1
    }else{
      res <- 0
    }
    return(res)
  }

  # Classification Result and verification 
  system.time(test.class.Nor <- apply(sample.test.Nor[,1:8], 1, dist.compute, n = 1))
  table(sample.test.Nor$channel.Nor, test.class.Nor)
  accuracy <- table(sample.test.Nor$channel.Nor, test.class.Nor)
  (accuracy[1,1]+accuracy[2,2])/sum(accuracy)
  
  system.time(test.class.Unif <- apply(sample.test.Unif[,1:8], 1, dist.compute, n = 2))
  table(sample.test.Unif$channel.Unif, test.class.Unif)
  accuracy <- table(sample.test.Unif$channel.Unif, test.class.Unif)
  (accuracy[1,1]+accuracy[2,2])/sum(accuracy)
  
  system.time(test.class.Exp <- apply(sample.test.Exp[,1:8], 1, dist.compute, n = 3))
  table(sample.test.Exp$channel.Exp, test.class.Exp)
  accuracy <- table(sample.test.Exp$channel.Exp, test.class.Exp)
  (accuracy[1,1]+accuracy[2,2])/sum(accuracy)
  
  system.time(test.class.Poi <- apply(sample.test.Poi[,1:8], 1, dist.compute, n = 4))
  table(sample.test.Poi$channel.Poi, test.class.Poi)
  accuracy <- table(sample.test.Poi$channel.Poi, test.class.Poi)
  (accuracy[1,1]+accuracy[2,2])/sum(accuracy)
  
  system.time(test.class.Ttest <- apply(sample.test.Ttest[,1:8], 1, dist.compute, n = 5))
  table(sample.test.Ttest$channel.Ttest, test.class.Ttest)
  accuracy <- table(sample.test.Ttest$channel.Ttest, test.class.Ttest)
  (accuracy[1,1]+accuracy[2,2])/sum(accuracy)
    
  # K-NN
  
  library(class)
  set.seed(1)
  
  # K-NN MODEL
  system.time(knn.pred.Nor <- knn(sample.train.Nor, sample.test.Nor, sample.train.Nor$channel.Nor, k = 5))
  # Verification of K-NN
  table(knn.pred.Nor, sample.test.Nor$channel.Nor)
  accuracy <- table(knn.pred.Nor, sample.test.Nor$channel.Nor)
  (accuracy[1,1]+accuracy[2,2])/sum(accuracy)
  
  system.time(knn.pred.Unif <- knn(sample.train.Unif, sample.test.Unif, sample.train.Unif$channel.Unif, k = 5))
  table(knn.pred.Unif, sample.test.Unif$channel.Unif)
  accuracy <- table(knn.pred.Unif, sample.test.Unif$channel.Unif)
  (accuracy[1,1]+accuracy[2,2])/sum(accuracy)

  system.time(knn.pred.Exp <- knn(sample.train.Exp, sample.test.Exp, sample.train.Exp$channel.Exp, k = 5))
  table(knn.pred.Exp, sample.test.Exp$channel.Exp)
  accuracy <- table(knn.pred.Exp, sample.test.Exp$channel.Exp)
  (accuracy[1,1]+accuracy[2,2])/sum(accuracy)

  system.time(knn.pred.Poi <- knn(sample.train.Poi, sample.test.Poi, sample.train.Poi$channel.Poi , k = 5))
  table(knn.pred.Poi, sample.test.Poi$channel.Poi)
  accuracy <- table(knn.pred.Poi, sample.test.Poi$channel.Poi)
  (accuracy[1,1]+accuracy[2,2])/sum(accuracy)

  system.time(knn.pred.Ttest <- knn(sample.train.Ttest, sample.test.Ttest, sample.train.Ttest$channel.Ttest , k = 5))
  table(knn.pred.Ttest , sample.test.Ttest$channel.Ttest)
  accuracy <- table(knn.pred.Ttest , sample.test.Ttest$channel.Ttest)
  (accuracy[1,1]+accuracy[2,2])/sum(accuracy)
  
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
  table(pred.svm.Nor, sample.test.Nor.svm$channel.Nor)
  accuracy <- table(pred.svm.Nor, sample.test.Nor.svm$channel.Nor)
  (accuracy[1,1]+accuracy[2,2])/sum(accuracy)
  
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
  table(pred.svm.Unif, sample.test.Unif.svm$channel.Unif)
  accuracy <- table(pred.svm.Unif, sample.test.Unif.svm$channel.Unif)
  (accuracy[1,1]+accuracy[2,2])/sum(accuracy)
  

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
  table(pred.svm.Exp, sample.test.Exp.svm$channel.Exp)
  accuracy <- table(pred.svm.Exp, sample.test.Exp.svm$channel.Exp)
  (accuracy[1,1]+accuracy[2,2])/sum(accuracy)
  

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
  table(pred.svm.Poi, sample.test.Poi.svm$channel.Poi)
  accuracy <- table(pred.svm.Poi, sample.test.Poi.svm$channel.Poi)
  (accuracy[1,1]+accuracy[2,2])/sum(accuracy)
  
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
  table(pred.svm.Ttest, sample.test.Ttest.svm$channel.Ttest)
  accuracy <- table(pred.svm.Ttest, sample.test.Ttest.svm$channel.Ttest)
  (accuracy[1,1]+accuracy[2,2])/sum(accuracy)
  
  