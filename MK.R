# DATA INPUT

# mysample <- read.csv("E:\\Allinpay\\Data\\LOAN\\sqlExport\\sample.csv", # win
mysample <- read.csv("/Users/mk/Desktop/sample.csv", # mac
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

infovalue <- data.frame(iv,row.names = substr(va,1,(nchar(va)-3)))
infovalue <- infovalue[-which(is.infinite(infovalue$iv)),]

# PCA CLASSIFIED
library(corrplot)
library(data.tree)

#compute ratio   (node,othernodes)
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


#use PCA to split the tree node
PCA <- function(x){
  pr.out.class <- prcomp(x, scale = T)
  cor.result <- cor(x,pr.out.class$x[,1:2])
  class.1 <- x[,which(cor.result[,1] >= cor.result[,2])]
  class.2 <- x[,which(cor.result[,1] < cor.result[,2])]
  return(list(class.1, class.2))
}



#create tree test data
data.root <- mysample[,-ncol(mysample)]
# data.root <- mysample[,-(ncol(mysample)-2): -ncol(mysample)]
t <- Node$new("t",ratio=NULL,needsplit=TRUE,data=data.root)  #root
# t.1 <- t$AddChild("t.1",ratio=0.00,needsplit=TRUE,data=NA)
# t.1.1 <- t.1$AddChild("t.1.1",ratio=0.00,needsplit=FALSE,data=NA)
# t.1.2 <- t.1$AddChild("t.1.2",ratio=0.00,needsplit=FALSE,data=NA)
# t.2 <- t$AddChild("t.2",ratio=0.00,needsplit=FALSE,data=NA)

print( t$parent )
print( t$parent$name )
#you can print the node use : print(t,"ratio","needsplit") but don't add data attr the rstudio may crash
#instead print the node data alone: print(t$data)
print(t,"needsplit","ratio")

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

print(t,"ratio","needsplit")



# end.split的输入为当前节点的两个父节点、自身节点及其他所有子节点
# end.split <- function(class.1, class.1.1, list){
#   end.split <- F
#   class.1.ratio <- rs.compute(class.1, class.2)
#   class.2.ratio <- rs.compute(class.2, class.1)
#   class.1.1.ratio  <- rs.compute(class.1.1, list)
#   class.1.1.merge <- merge(class.1.1.ratio, class.1.ratio, by = "names.x.", all.x = T)
#   decrease.number <- sum(class.1.1.merge$Ratio.x < class.1.1.merge$Ratio.y)
#   decrease.ratio <- decrease.number/ncol(class.1.1.ratio)
#   if(decrease.ratio >= 0.5 | ncol(class.1.1) == 1){
#     end.split <- T
#   } else{
#     end.split <- F
#   }
# }

class.1 <- data.frame(PCA(mysample[,-ncol(mysample)])[1])
class.2 <- data.frame(PCA(mysample[,-ncol(mysample)])[2])
class.1.1 <- data.frame(PCA(class.1)[1])
class.1.2 <- data.frame(PCA(class.1)[2])
class.2.1 <- data.frame(PCA(class.2)[1])
class.2.2 <- data.frame(PCA(class.2)[2])

end.split(class.1, class.2, class.1.1, list(class.1.2, class.2.1, class.2.2))





rsoutput <- rs.compute(class.1, class.2)


temp <- PCA(mysample[,-ncol(mysample)])



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
summary(fullmod)$coefficents[,4]
coefficients.fullmod <- summary(fullmod)$coefficients[,4]
which(coefficients.fullmod < 0.05)
formula(fullmod)

nothing <- glm(OVERDUE ~ 1, data = reductionSample, family = binomial)
summary(nothing)


backwards <- step(fullmod)
summary(backwards)

forwards <- step(nothing, scope = list(lower = formula(nothing), upper = formula(fullmod)), direction = "forward")
summary(forwards)


bothways <- step(nothing, list(lower = formula(nothing), upper = formula(fullmod)), direction = "both", trace = 0)
summary(bothways)

finalModel <- function(x){
  for(i in x){
    assign(paste("coefficients.", i, sep = ""), summary(i)$coefficients[,4])
    variable.selected <- (which(get(paste("coefficients.", i, sep = "")) < 0.05))
    return(c(i, variable.selected))
  }
}

finalModel(list(fullmod,backwards,forwards,bothways))
finalModel(backwards)
finalModel(forwards)
finalModel(bothways)

glm.selection <- function(x){
  Plist <- c()
  add.index <- c()
  for(i in 1:ncol(x)){
    assign(paste("glm.fit", i, sep = ""),
           glm(OVERDUE ~ x[,i], data = x, family = binomiall))
    Plist[i] <- summary(get(paste("glm.fit", i, sep = "")))$coefficients[2,4]
  }
  add.index <- append(add.index, which.min(Plist))
  # add.variable <- x[,add.index]
  for(i in 1:ncol(x)){
    if(!(i %in% add.index)){
      assign(paste("glm.fit", i, sep = ""),
             glm(OVERDUE ~ x[,i] + x[,add.index], data = x, family = binomiall))
      Plist[i] <- summary(get(paste("glm.fit", i, sep = "")))$coefficients[2,4]
    }
  }
  add.index <- append(add.index, which.min(Plist))
  # add.variable <- append(x[,add.index])
}
glm.selection(reductionSample)

step

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

glm.probs = predict(stepwiseGlm, type = "response")

contrasts(OVERDUE)
glm.pred = rep("0", nrow(reductionSample))
glm.pred[glm.probs>.5] = "1"
table(glm.pred, OVERDUE)
detach(reductionSample)

library(ggplot2)
qplot(seq(-200,200, length = 5771), sort(glm.probs), col = "response")

