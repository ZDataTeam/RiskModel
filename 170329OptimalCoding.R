# DATA INPUT
mysample <- read.csv("E:\\Allinpay\\Data\\LOAN\\sqlExport\\sample.csv",
                     header = T,
                     stringsAsFactors = T)
options(scipen=3)

# TO BE SOLVED!!! HANDLING IMBALANCE!!! RESULTS INDICATED DATA IMBALANCE COULD CAUSE PREDICTION RATE DECREASE

  # random insert
    # index <- sample(c(1:nrow(mysample)-1),length(mysample[which(mysample$OVERDUE != 0)]),replace = T)
  # newSample <- rbind(mysample[index,],mysample[which(mysample$OVERDUE != 0),],mysample[index+1,])

  # rbind directly
  # mysample <-rbind(mysample, mysample[which(mysample$OVERDUE != 0),])

mysample$OVERDUE <- as.factor(mysample$OVERDUE)

# Pos related variable combined
# mysample <- mysample[,-27:-74]


# IV COMPUTE
library(woe)
va <- c()
iv <- c()
for(i in 1:length(mysample)){
  if(is.numeric(mysample[,i])){
    va[i] <- paste(names(mysample)[i],
                   "WOE",
                   sep = "")
    assign(paste(names(mysample)[i],
                 "WOE",
                 sep = ""),
           woe(mysample,
               names(mysample)[i],
               Continuous = T,
               "OVERDUE",
               C_Bin = 5,
               Good = "1",
               Bad = "0"))
    iv[i] <- sum(get(paste(names(mysample)[i],
                           "WOE",
                           sep = ""))$IV)
  } else if(is.factor(mysample[,i])){
    va[i] <- paste(names(mysample)[i],
                   "WOE",
                   sep = "")
    assign(paste(names(mysample)[i],
                 "WOE",
                 sep = ""),
           woe(mysample,
               names(mysample)[i],
               Continuous = F,
               "OVERDUE",
               C_Bin = 5,
               Good = "1",
               Bad = "0"))
    iv[i] <- sum(get(paste(names(mysample)[i],
                           "WOE",
                           sep = ""))$IV)
  }
}

infovalue <- data.frame(names(mysample),iv,row.names = substr(va,1,(nchar(va)-3)))
infovalue <- infovalue[-which(is.infinite(infovalue$iv)),]

# PCA CLASSIFIED
library(corrplot)

PCA <- function(x){
  pr.out.class <- prcomp(x, scale = T)
  cor.result <- cor(x,pr.out.class$x[,1:2])
  # cor.sample <- cbind(x, pr.out.class$x[,1:2])
  # cor.temp <- cor(cor.sample)
  # cor.result <- cor.temp[-(nrow(cor.temp)-1):-nrow(cor.temp),(ncol(cor.temp)-1):ncol(cor.temp)]
  # corrplot(cor.result, tl.cex = 0.5)
  class.1 <- x[,which(cor.result[,1] >= cor.result[,2])]
  class.2 <- x[,which(cor.result[,1] < cor.result[,2])]
  return(list(class.1, class.2))
}

# end.split的输入为第一层的两个父节点、自身节点及其他所有子节点
end.split <- function(class.1, class.2, class.1.1, list){
  end.split <- F
  class.1.ratio <- rs.compute(class.1, class.2)
  class.2.ratio <- rs.compute(class.2, class.1)
  
  class.1.1.ratio  <- rs.compute(class.1.1, list)
  class.1.1.merge <- merge(class.1.1.ratio, class.1.ratio, by = "names.x.", all.x = T)
  decrease.number <- sum(class.1.1.merge$Ratio.x < class.1.1.merge$Ratio.y)
  decrease.ratio <- decrease.number/ncol(class.1.1.ratio)
  if(decrease.ratio >= 0.5 | ncol(class.1.1) == 1){
    end.split <- T
  } else{
    end.split <- F
    # keep PCA?
  }
}


# When PCA finished, consider ceiling(max(1, IV(currentClass)/IV(allClass)*numbers of index in current class))
# to determine the number left in each class
index.left <- function(class.list){
  reduction.sample <- c()
  for(i in class.list){
    class.sample <- merge(t(i), infovalue, by = row.names, all.x = T)
    class.sample.IV <- sum(class.sample.IV[,ncol(class.sample.IV)])
    all.class.IV <- sum(infovalue[,2])
    number.left <- ceiling(max(1, class.sample.IV/all.class.IV*ncol(class.sample)))
    class.reduction <- sort(i, decreasing = T)
    reduction.sample <- append(reduction.sample, class.reduction)
  }
  return(reduction.sample)
}


  class.1 <- data.frame(PCA(mysample[,-ncol(mysample)])[1])
  class.2 <- data.frame(PCA(mysample[,-ncol(mysample)])[2])
  class.1.1 <- data.frame(PCA(class.1)[1])
  class.1.2 <- data.frame(PCA(class.1)[2])
  class.2.1 <- data.frame(PCA(class.2)[1])
  class.2.2 <- data.frame(PCA(class.2)[2])
  
  end.split(class.1, class.2, class.1.1, list(class.1.2, class.2.1, class.2.2))

  
  first.reduction.sample <- index.left(PCARESULT)
  


rs.compute <- function(x,y){
  r.squared <- c()
  add.r.squared <- c()
  Ratio <- c()
  temp <- x
  for(i in 1:length(x)){
    temp[, 1:i] <- x[, i:1]
    names(temp)[1] <- "Ddiff"
    r.squared[i] <-  summary(lm(Ddiff~.-Ddiff, data = temp))$r.squared
    for(j in y){
      dtAdd <- data.frame(temp[,1], j)
      names(dtAdd)[1] <- "Ddiff"
      add.r.squared <- append(summary(lm(Ddiff~.-Ddiff, data = dtAdd))$r.squared, add.r.squared)
    }
    Ratio[i] <- r.squared[i]/max(add.r.squared)
  }
  return(data.frame(names(x),Ratio))
}

rsoutput <- rs.compute(class.1, class.2)




# Remained to be optimal
pr.out.class <- prcomp(mysample[,-ncol(mysample)], scale = TRUE)
classBind <- cbind(mysample[,-ncol(mysample)], pr.out.class$x[,1:2])
corClassResult <- cor(classBind)[-nrow(classBind),(ncol(classBind)-1):ncol(classBind)]
# corrplot(corClassResult, tl.cex = 0.5)
classPCA1 <- corClassResult[which(corClassResult[,1] >= corClassResult[,2])]
classPCA2 <- corClassResult[which(corClassResult[,1] < corClassResult[,2])]

for(i in (ncol(mysample)-1):1){
  if(corClassResult[i,1] > corClassResult[i,2]){
    classPCA2 <- classPCA2[, -i]
  } else{
    classPCA1 <- classPCA1[, -i]
  }
}

# PCA1: PCA11 & PCA12
pr.out.class2 <- prcomp(classPCA1, scale = TRUE)
classBind2 <- cbind(classPCA1, pr.out.class2$x[,1:2])
corClassResult2 <- cor(classBind2)[,(length(classBind2)-1):(length(classBind2))]
corrplot(corClassResult2, tl.cex = 0.5)
classPCA11 <- classPCA1
classPCA12 <- classPCA1
for(i in length(classPCA1):1){
  if(corClassResult2[i,1] > corClassResult2[i,2]){
    classPCA12 <- classPCA12[, -i]
  } else{
    classPCA11 <- classPCA11[, -i]
  }
}

# PCA2: PCA21 & PCA22
pr.out.class2 <- prcomp(classPCA2, scale = TRUE)
classBind2 <- cbind(classPCA2, pr.out.class2$x[,1:2])
corClassResult2 <- cor(classBind2)[,(length(classBind2)-1):(length(classBind2))]
# corrplot(corClassResult2, tl.cex = 0.5)
classPCA21 <- classPCA2
classPCA22 <- classPCA2
for(i in length(classPCA2):1){
  if(corClassResult2[i,1] > corClassResult2[i,2]){
    classPCA22 <- classPCA22[, -i]
  } else{
    classPCA21 <- classPCA21[, -i]
  }
}

# PCA12: PCA121 & PCA122
pr.out.class2 <- prcomp(classPCA12, scale = TRUE)
classBind2 <- cbind(classPCA12, pr.out.class2$x[,1:2])
corClassResult2 <- cor(classBind2)[,(length(classBind2)-1):(length(classBind2))]
# corrplot(corClassResult2, tl.cex = 0.5)
classPCA121 <- classPCA12
classPCA122 <- classPCA12
for(i in length(classPCA12):1){
  if(corClassResult2[i,1] > corClassResult2[i,2]){
    classPCA122 <- classPCA122[, -i]
  } else{
    classPCA121 <- classPCA121[, -i]
  }
}


# R-SQUARED RATIO
library(dplyr)
r.squared <- c()
add.r.squared <- c()
add.r.squaredy <- c()
add.r.squaredm <- c()
add.r.squaredn <- c()
add.r.squareda <- c()
add.r.squaredb <- c()
add.r.squaredc <- c()
add.r.squaredd <- c()

Ratio <- c()

rs.compute <- function(x,y){
  temp <- x
  for(i in 1:length(x)){
    temp[, 1:i] <- x[, i:1]
    names(temp)[1] <- "Ddiff"
    r.squared[i] <-  summary(lm(Ddiff~.-Ddiff, data = temp))$r.squared
    
    dtAdd <- data.frame(temp[,1], y)
    names(dtAdd)[1] <- "Ddiff"
    add.r.squared[i] <- summary(lm(Ddiff~.-Ddiff, data = dtAdd))$r.squared
    Ratio[i] <- r.squared[i]/max(add.r.squared[i])
  }
  return(data.frame(names(x),Ratio))
}
rsoutput <- rs.compute(class.1, class.2)



# WITH 2-TYPE! 
rsCompute <- function(x,y){
  temp <- x
  for(i in 1:length(x)){
    temp[, 1:i] <- x[, i:1]
    names(temp)[1] <- "Ddiff"
    r.squared[i] <-  summary(lm(Ddiff~.-Ddiff, data = temp))$r.squared
    dtAdd <- data.frame(temp[,1], y)
    names(dtAdd)[1] <- "Ddiff"
    add.r.squared[i] <- summary(lm(Ddiff~.-Ddiff, data = dtAdd))$r.squared
    Ratio[i] <- r.squared[i]/add.r.squared[i]
  }
  return(data.frame(names(x),Ratio))
}



# WITH 4-TYPE! REVISE COLUMN NAME OF LM VARIABLE!!!
rsCompute <- function(x,y,m,n){
  temp <- x
  for(i in 1:length(x)){
    temp[, 1:i] <- x[, i:1]
    names(temp)[1] <- "Ddiff"
    r.squared[i] <-  summary(lm(Ddiff~.-Ddiff, data = temp))$r.squared
    
    dtAddy <- data.frame(temp[,1], y)
    names(dtAddy)[1] <- "Ddiff"
    add.r.squaredy[i] <- summary(lm(Ddiff~.-Ddiff, data = dtAddy))$r.squared
    
    dtAddm <- data.frame(temp[,1], m)
    names(dtAddm)[1] <- "Ddiff"
    add.r.squaredm[i] <- summary(lm(Ddiff~.-Ddiff, data = dtAddm))$r.squared
    
    dtAddn <- data.frame(temp[,1], n)
    names(dtAddn)[1] <- "Ddiff"
    add.r.squaredn[i] <- summary(lm(Ddiff~.-Ddiff, data = dtAddn))$r.squared
    
    
    Ratio[i] <- r.squared[i]/max(add.r.squaredy[i], add.r.squaredm[i], add.r.squaredn[i])
    
  }
  return(data.frame(names(x),Ratio))
}

# WITH 5-TYPE! REVISE COLUMN NAME OF LM VARIABLE!!!
rsCompute <- function(x,y,a,b,c){
  temp <- x
  for(i in 1:length(x)){
    temp[, 1:i] <- x[, i:1]
    names(temp)[1] <- "Ddiff"
    r.squared[i] <-  summary(lm(Ddiff~.-Ddiff, data = temp))$r.squared
    
    dtAddy <- data.frame(temp[,1], y)
    names(dtAddy)[1] <- "Ddiff"
    add.r.squaredy[i] <- summary(lm(Ddiff~.-Ddiff, data = dtAddy))$r.squared
    
    dtAdda <- data.frame(temp[,1], a)
    names(dtAdda)[1] <- "Ddiff"
    add.r.squareda[i] <- summary(lm(Ddiff~.-Ddiff, data = dtAdda))$r.squared
    
    dtAddb <- data.frame(temp[,1], b)
    names(dtAddb)[1] <- "Ddiff"
    add.r.squaredb[i] <- summary(lm(Ddiff~.-Ddiff, data = dtAddb))$r.squared
    
    dtAddc <- data.frame(temp[,1], c)
    names(dtAddc)[1] <- "Ddiff"
    add.r.squaredc[i] <- summary(lm(Ddiff~.-Ddiff, data = dtAddc))$r.squared
    
    Ratio[i] <- r.squared[i]/max(add.r.squaredy[i],
                                 add.r.squareda[i], add.r.squaredb[i],add.r.squaredc[i])
    
  }
  return(data.frame(names(x),Ratio))
}


reductionSample <- data.frame(mysample$APP_OPEN_RD30,
                              mysample$CUSTOMER_TYPE,
                              mysample$APP_OPEN_RD7,
                              mysample$STLM_INS_CIRCLE1,
                              mysample$TXNAMT1011,
                              mysample$CTAMT0,
                              mysample$CT0,
                              mysample$CTAMT1,
                              mysample$CTAMT2,
                              mysample$NOPASS,
                              mysample$TXNAMT3011,
                              mysample$BRANCHNUM,
                              mysample$ORI_CHNL3,
                              mysample$QUALITY3,
                              mysample$TXNAMT1031,
                              mysample$QUALITY1,
                              mysample$ORATE,
                              mysample$ORI_CHNL1,
                              mysample$QUALITY4,
                              mysample$MCHT_STATUS0,
                              mysample$OVERDUE)
names(reductionSample) <- c("APP_OPEN_RD30",
                            "CUSTOMER_TYPE",
                            "APP_OPEN_RD7",
                            "STLM_INS_CIRCLE1",
                            "TXNAMT1011",
                            "CTAMT0",
                            "CT0",
                            "CTAMT1",
                            "CTAMT2",
                            "NOPASS",
                            "TXNAMT3011",
                            "BRANCHNUM",
                            "ORI_CHNL3",
                            "QUALITY3",
                            "TXNAMT1031",
                            "QUALITY1",
                            "ORATE",
                            "ORI_CHNL1",
                            "QUALITY4",
                            "MCHT_STATUS0",
                            "OVERDUE")

# Stepwise with Logistics
library(glmnet)
attach(reductionSample)

fullmod <- glm(OVERDUE ~., data = reductionSample, family = binomial)
coefficients.fullmod <- summary(fullmod)$coefficients[,4]
which(coefficients.fullmod < 0.05)
final.fullmod <- glm(OVERDUE ~ APP_OPEN_RD30+APP_OPEN_RD7+CT0+QUALITY1+ORATE+ORI_CHNL1,
                     data = reductionSample, family = binomial)


nothing <- glm(OVERDUE ~ 1, data = reductionSample, family = binomial)
summary(nothing)

backwards <- step(fullmod)
coefficients.backwards <- summary(backwards)$coefficients[,4]
which(coefficients.backwards < 0.05)
final.backwards <- glm(OVERDUE ~ APP_OPEN_RD30+APP_OPEN_RD7+TXNAMT1011+CT0+QUALITY1+ORATE+ORI_CHNL1,
                       data = reductionSample, family = binomial)

forwards <- step(nothing, scope = list(lower = formula(nothing), upper = formula(fullmod)), direction = "forward")
coefficients.forwards <- summary(forwards)$coefficients[,4]
which(coefficients.forwards < 0.05)
final.forwards <- glm(OVERDUE ~ TXNAMT1011+APP_OPEN_RD30+CT0+QUALITY1+ORI_CHNL1+APP_OPEN_RD7+ORATE,
                      data = reductionSample, family = binomial)


bothways <- step(nothing, list(lower = formula(nothing), upper = formula(fullmod)), direction = "both", trace = 0)
coefficients.bothways <- summary(bothways)$coefficients[,4]
which(coefficients.bothways < 0.05)
final.bothways <- glm(OVERDUE ~ TXNAMT1011+APP_OPEN_RD30+CT0+QUALITY1+ORI_CHNL1+APP_OPEN_RD7+ORATE,
                      data = reductionSample, family = binomial)


# Boundary: one star
glm.selection <- function(x){
  Plist <- c()
  index <- NULL  #added item
  index.list <- c()  #added list

  for(i in 1:ncol(x)){
    assign(paste("glm.fit.", i, sep = ""),
           glm(OVERDUE ~ x[,i], data = x, family = binomial))
    Plist[i] <- summary(get(paste("glm.fit.", i, sep = "")))$coefficients[2,4]
  }
  while(any(Plist < 0.05)){
    index <- which.min(Plist)
    index.list <- append(index.list, index)
    for(i in 1:ncol(x)){
      while(!(i %in% index.list)){
      current.glm <- glm(OVERDUE ~ x[,index], data = x, family = binomial)
      current.glm <- update(current.glm, ~.+x[,i])
      Plist <- c()
      Plist[i] <- summary(current.glm)$coefficients[,4]
      }
    }
    Recursion(x, Plist, index.list, current.glm)
    Recursion <- function(x,Plist,index.list,current.glm){
      while(all(Plist) < 0.05){
        # print(all(Plist))
        index <- which.min(Plist)
        index.list <- append(index.list, index)
        for(i in 1:ncol(x)){
          while(!(i %in% index.list)){
            current.glm <- glm(OVERDUE ~ x[,index], data = x, family = binomial)
            current.glm <- update(current.glm, ~.+x[,i])
            Plist <- c()
            Plist[i] <- summary(current.glm)$coefficients[,4]
          }
        }
        print(current.glm)
      }
      return(Recursion(x, Plist, index.list, current.glm))
    }
  }
}

glm.selection(reductionSample)


# Model for verify
stepwiseGlm <- glm(OVERDUE ~ CT0+TXNAMT1011+APP_OPEN_RD30+QUALITY1+ORI_CHNL1+ORATE+APP_OPEN_RD7,
                   data = reductionSample, family = binomial)
summary(stepwiseGlm)




summary(glm(OVERDUE ~reductionSample[,1], data = reductionSample, family = binomial))$coefficients[,4] < 0.05
temp <- glm(OVERDUE ~reductionSample[,1], data = reductionSample, family = binomial)
update(glm(OVERDUE~reductionSample[,1], data = reductionSample, family = binomial), ~.+reductionSample[,2])


for(i in 1:length(reductionSample)){
  if(!(i %in% c("7","5","1","16","18","3","17","2","10"))){
    assign(paste("glm.fit", i, sep = ""),
           glm(OVERDUE ~ reductionSample[,i]+CT0+TXNAMT1011+APP_OPEN_RD30+QUALITY1+ORI_CHNL1+
                 APP_OPEN_RD7+ORATE+CUSTOMER_TYPE+NOPASS,
               data = reductionSample, family = binomial))
    Plist1[i] <- summary(get(paste("glm.fit", i, sep = "")))$coefficients[2,4]
  }
}
which.min(Plist1)
names(reductionSample)[which.min(Plist1)]

stepwiseGlm <- glm(OVERDUE ~ CT0+TXNAMT1011+APP_OPEN_RD30+QUALITY1+ORI_CHNL1+APP_OPEN_RD7
                   +ORATE+CUSTOMER_TYPE+NOPASS,
                   data = reductionSample, family = binomial)
summary(stepwiseGlm)



library(ggplot2)
compare.table <- function(x){
  glm.probs <- predict(x, type = "response")
  contrasts(factor(reductionSample$OVERDUE))
  glm.pred <- rep("0", nrow(reductionSample))
  glm.pred[glm.probs > 0.5] = "1"
  table.rate <- table(glm.pred, factor(reductionSample$OVERDUE))
  print(qplot(seq(-200,200, length = 5771), sort(glm.probs), col = "response"))
  print(table.rate)
  total.correct.rate <- (table.rate[1,1]+table.rate[2,2])/(sum(table.rate))
  overdue.correct.rate <- table.rate[2,2]/(table.rate[1,2]+table.rate[2,2])
  return(c(total.correct.rate, overdue.correct.rate))
}

compare.table(stepwiseGlm)
compare.table(final.fullmod)
compare.table(final.backwards)
compare.table(final.forwards)
compare.table(final.bothways)





