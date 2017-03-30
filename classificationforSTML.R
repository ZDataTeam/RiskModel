# DATA INPUT
# Group stlm_ins_circle as 0 and 1
# mySample <- read.csv("E:\\Allinpay\\Data\\STLM_INS_CIRCLE\\01normalize\\01sample.csv",
#                      header = TRUE,
#                      stringsAsFactors = TRUE,
#                      encoding = "UTF-8")
# mySample$X.U.FEFF.mcht_cd <- as.factor(mySample$X.U.FEFF.mcht_cd)
# rownames(mySample) <- mySample[,1]
# mySample <- mySample[,-1]
# mySample$stlm_ins_circle <- substr(mySample$stlm_ins_circle,1,1)
# mySample <- mySample[,-7:-9]
# mySample[which(mySample$stlm_ins_circle == "2"),] <- transform(mySample[which(mySample$stlm_ins_circle == "2"),], stlm_ins_circle = "1")
# mySample$stlm_ins_circle <- as.factor(mySample$stlm_ins_circle)

# Add random col
# mySample <- read.csv("E:\\Allinpay\\Data\\STLM_INS_CIRCLE\\workingPaper\\data\\sample.csv",
mySample <- read.csv("/Users/mk/Documents/RiskModel/zsy_signal_pos/UpdateDataForClassification.csv",
                     header = TRUE,
                     stringsAsFactors = TRUE)
options(scipen=3)
rownames(mySample) <- mySample[,1]
mySample <- mySample[,-1]
mySample$stlm_ins_circle <- as.factor(mySample$stlm_ins_circle)
mySample$SUM <- Reduce('+',mySample[,2:9])


# NORMAL DISTRIBUTION
mySample$NORM <- rnorm(length(mySample$stlm_ins_circle))
mySample$SUM.NORM <- apply(mySample[,10:11],1,sum)
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

# write.csv(mySample, file = "E:\\Allinpay\\Data\\STLM_INS_CIRCLE\\workingPaper\\data\\SampleWithLable.csv")

sample.train <- mySample[which(mySample$stlm_ins_circle == "1"),2:25]
sample.test <- mySample[which(mySample$stlm_ins_circle == "0"),2:25]

# distribution <- c(13, 16, 19, 22, 25)

# Data Preparation
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

# Distance-based
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

distanceCompute <- function(x, type){
  res <- c()
    #TODO:
    # Somehow a waste of memory
    # try R for loop
    for( irow in 1:nrow(x) ) {
       d1 <- dist(rbind(center[1,], x[irow,-ncol(x)]), method = "euclidian")[1]  # [3:nrow(x),1:2]
       d0 <- dist(rbind(center[2,], x[irow,-ncol(x)]), method = "euclidian")[1]  # [3:nrow(x),1:2]
       if(d1 <= d0){
            res = append(res,c(1))
        }else{
            res = append(res,c(0))
        }
    }
  return(res)
}


# dist(rbind(center[1,], sample.test.Nor[1,-ncol(sample.test.Nor)]), method = "euclidian")





labels <- distanceCompute(sample.test.Nor, 1)
Label.Nor <- cbind(sample.test.Nor,labels)



# K-NN
library(class)
set.seed(1)

knn.pred.Nor = knn(sample.train.Nor, sample.test.Nor, sample.train.Nor$channel.Nor, k = 5)
table(knn.pred.Nor, sample.test.Nor$channel.Nor)

knn.pred.Unif = knn(sample.train.Unif, sample.test.Unif, sample.train.Unif$channel.Unif, k = 5)
table(knn.pred.Unif, sample.test.Unif$channel.Unif)

knn.pred.Exp = knn(sample.train.Exp, sample.test.Exp, sample.train.Exp$channel.Exp, k = 5)
table(knn.pred.Exp, sample.test.Exp$channel.Exp)

knn.pred.Poi = knn(sample.train.Poi, sample.test.Poi, sample.train.Poi$channel.Poi , k = 5)
table(knn.pred.Poi, sample.test.Poi$channel.Poi)

knn.pred.Ttest = knn(sample.train.Ttest, sample.test.Ttest, sample.train.Ttest$channel.Ttest , k = 5)
table(knn.pred.Ttest , sample.test.Ttest$channel.Ttest)

# SVM
# Set the label as "1" and "-1"
set.seed(1)
library(e1071)
sample.train.Exp.svm <- sample.train.Exp
sample.train.Exp.svm[which(sample.train.Exp.svm$channel.Exp == "0",),] <- transform(sample.train.Exp.svm[which(sample.train.Exp.svm$channel.Exp == "0"),], channel.Exp = "-1")

sample.test.Exp.svm <- transform(sample.test.Exp[which(sample.test.Exp$channel.Exp == "0"),], channel.Exp = "-1")
svmfit.Exp = svm(sample.train.Exp.svm[1:8], sample.train.Exp.svm$channel.Exp)



# Simulation with bootstrap
boot.samples <- list()
for(i in 1:length(mySample$stlm_ins_circle)){
  b.samples.cases <- sample(length(mySample$Zd_time), length(mySample$Zd_time), replace = TRUE)
  b.mydf <- mydf[b.samples.cases,]
  boot.samples[[i]] <- b.mydf
}
str(boot.samples)
boot.samples[1]

# Parametric bootstrap

library(glmnet)
attach(mySample)
glm.fit <- glm(stlm_ins_circle~Zd_time+Zmale+Zage+Ztotal_amt+Ztotal_cnt+ZifType0+ZifType1+ZifType2
               +rnorm(nrow(mySample)), data = mySample, family = binomial)
summary(glm.fit)

nb = 5000
bet = NULL
n = nrow(mySample)
for(i in 1:nb){
  unifnum = sample(c(1:n),n, replace = T)
  bet[i] = glm(stlm_ins_circle[unifnum,1]~Zd_time[unifnum,]+Zmale[unifnum,]
               +Zage[unifnum,]+Ztotal_amt[unifnum,]+Ztotal_cnt[unifnum,]+ZifType0[unifnum,]+ZifType1[unifnum,]+ZifType2[unifnum,],
               data = mySample, family = binomial)
}

detach(mySample)