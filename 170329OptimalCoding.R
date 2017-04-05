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

# TO BE SOLVED!!! WHETHER TERMINAL IN KEEPING CURRENT NODE OR BACK TO FATHER NODE!!!(VERIFY)

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
# When could be classified by value, done!
# When can't, chosen minimum variable between PCA1 and PCA2 as one class ?????? any better??????
# library(corrplot)
library(data.tree)
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

# return needsplit and ratio of currnode
calNeedSplitAndRatio <- function(currnode, parentnode, list.otherleafs) {
  need.split <- T
  # class.1.ratio <- rs.compute(class.1, class.2)
  # class.2.ratio <- rs.compute(class.2, class.1)
  ratio  <- rs.compute(currnode$data, list.otherleafs)
  
  if( is.null(parentnode$ratio) ) {
    return( list(TRUE, ratio) ) 
  }else{
    merge <- merge(ratio, parentnode$ratio, by = "names.x.", all.x = T)
    print( names(merge) )
    decrease.number <- sum(merge$Ratio.x < merge$Ratio.y)
    decrease.ratio <- decrease.number/nrow(ratio)
    print("decrease.number=")
    print(decrease.number)
    print("ncol(ratio)")
    print(nrow(ratio))
    if(decrease.ratio >= 0.5 | ncol(currnode$data) == 1){
      need.split <- F
    }
    return( list( need.split, ratio ) )
  }
}


PCA <- function(x){
  pr.out.class <- prcomp(x, scale = T)
  cor.result <- cor(x, pr.out.class$x[,1:2])
  # cor.sample <- cbind(x, pr.out.class$x[,1:2])
  # cor.temp <- cor(cor.sample)
  # cor.result <- cor.temp[-(nrow(cor.temp)-1):-nrow(cor.temp),(ncol(cor.temp)-1):ncol(cor.temp)]
  # corrplot(cor.result, tl.cex = 0.5)
  if(!(all(cor.result[,1] <= cor.result[,2]) | all(cor.result[,1] >= cor.result[,2]))){
    class.1 <- x[,which(cor.result[,1] >= cor.result[,2])]
    class.2 <- x[,which(cor.result[,1] < cor.result[,2])]
  }else{
    class.1 <- x[,which.min(abs(cor.result[,1] - cor.result[,2]))]
    class.2 <- x[,-which.min(abs(cor.result[,1] - cor.result[,2]))]
  }
  return(list(class.1, class.2))
}


#create tree test data
data.root <- mysample[,-ncol(mysample)]
# data.root <- mysample[,-(ncol(mysample)-2): -ncol(mysample)]
t <- Node$new("t",ratio=NULL,needsplit=TRUE,data=data.root)  #root
#create tree
createRESTree <- function(tree){
  needsplits <- tree$Get('needsplit',filterFun = isLeaf)   #get leaf nodes split flag
  names <- tree$Get('name',filterFun = isLeaf)             #get leaf nodes name
  datas <- tree$Get('data',filterFun = isLeaf)             #get leaf nodes data
  
  node.index <- 1
  for(nm in needsplits) {
    if(nm == TRUE) {  # need to be splitted
      left.node.name <- paste(names[node.index],".1", sep="")  # create the left child node name str
      right.node.name <- paste(names[node.index],".2", sep="") # create the right child node name str
      curr.node <- get(names[node.index])   #  node.index on curr node 
      # others.node.names <- names[-node.index] # other leaf nodes name
      # others.node <- mget(as.vector(others.node.names)) # other leaf nodes
      #cal data
      PCA.data <- PCA(curr.node$data)   # split node data use pca
      left.data <- as.data.frame(PCA.data[1])
      right.data <- as.data.frame(PCA.data[2])
      
      assign(left.node.name , curr.node$AddChild( left.node.name, ratio=0.00, needsplit=TRUE, data=left.data ), envir = .GlobalEnv )  #add left child node
      assign(right.node.name , curr.node$AddChild( right.node.name, ratio=0.00, needsplit=TRUE, data=right.data ), envir = .GlobalEnv )  #add right child node
    }
    node.index <- node.index + 1
  }
  # add ratio and needsplit to leaf nodes
  
  needsplits <- tree$Get('needsplit',filterFun = isLeaf)   #get leaf nodes split flag
  names <- tree$Get('name',filterFun = isLeaf)             #get leaf nodes name
  datas <- tree$Get('data',filterFun = isLeaf)             #get leaf nodes data
  
  
  nd.index <- 1
  for(nd in names) {
    cur.nd <- get(nd)
    # other.node.names <- names[-node.index] # other leaf nodes name
    # print(as.vector(other.node.names))
    # other.nodes <- mget(as.vector(other.node.names)) # other leaf nodes
    calres <- calNeedSplitAndRatio(cur.nd, cur.nd$parent, datas)
    cur.nd$needsplit <- calres[1]
    cur.nd$ratio <- calres[2]
    nd.index <- nd.index + 1
  }
  
  print(all(needsplits==FALSE))   #all leaf nodes needsplits==FALSE  break out 
  return("func done")
}


while(all(t$Get('needsplit',filterFun = isLeaf)==TRUE)){
  createRESTree(t)
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





rsoutput <- rsCompute(class.1, class.2)




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



# for(i in 1:length(reductionSample)){
#   if(!(i %in% c("7","5","1","16","18","3","17","2","10"))){
#     assign(paste("glm.fit", i, sep = ""),
#            glm(OVERDUE ~ reductionSample[,i]+CT0+TXNAMT1011+APP_OPEN_RD30+QUALITY1+ORI_CHNL1+
#                  APP_OPEN_RD7+ORATE+CUSTOMER_TYPE+NOPASS,
#                data = reductionSample, family = binomial))
#     Plist1[i] <- summary(get(paste("glm.fit", i, sep = "")))$coefficients[2,4]
#   }
# }
# which.min(Plist1)
# names(reductionSample)[which.min(Plist1)]

# stepwiseGlm <- glm(OVERDUE ~ CT0+TXNAMT1011+APP_OPEN_RD30+QUALITY1+ORI_CHNL1+APP_OPEN_RD7
#                    +ORATE+CUSTOMER_TYPE+NOPASS,
#                    data = reductionSample, family = binomial)
# summary(stepwiseGlm)



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





