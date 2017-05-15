# 1.TO BE SOLVED!!! 
  # HANDLING IMBALANCE!!! RESULTS INDICATED DATA IMBALANCE COULD CAUSE PREDICTION RATE DECREASE
  # random insert
  # index <- sample(c(1:nrow(mysample)-1),length(mysample[which(mysample$OVERDUE != 0),]),replace = T)
  # mysample <- rbind(mysample[index,],mysample[which(mysample$OVERDUE != 0),],mysample[index+1,])

  # rbind directly
  # mysample <-rbind(mysample, mysample[which(mysample$OVERDUE != 0),])

# 2.TO BE SOLVED!!!
  # WHETHER TERMINAL IN KEEPING CURRENT NODE OR BACK TO FATHER NODE!!!(VERIFY)
  # mysample$OVERDUE <- as.factor(mysample$OVERDUE)

  # Pos related variable combined
  # mysample <- mysample[,-27:-74]

# 3.TO BE SOLVED!!!
  # ALGORITHM COMPARASON BY MSE AND PREDICTION ACCURACY




# DATA INPUT
mysample <- read.csv("E:\\Allinpay\\Data\\LOAN\\sqlExport\\sample.csv",
                     header = T,
                     stringsAsFactors = T)
options(scipen=3)


# # ADDING POS QUALITY CONTROL INDEX
mcht_cd <- read.csv("E:\\Allinpay\\Data\\LOAN\\sqlExport\\POSQC\\zsy_signal_pos\\MerchantID.csv",
                    header = T,
                    stringsAsFactors = T)
mcht_cd <- mcht_cd[,1, drop = F]
mysample <- cbind(mcht_cd, mysample)


pos.qc <- dir("E:\\Allinpay\\Data\\LOAN\\sqlExport\\POSQC\\zsy_signal_pos\\output", pattern = "csv$")

pos.qc.path <- paste("E:\\Allinpay\\Data\\LOAN\\sqlExport\\POSQC\\zsy_signal_pos\\output", pos.qc, sep = "\\")


pos.qc.data <- lapply(pos.qc.path, function(x) read.csv(x, header = T, stringsAsFactors = F))

for(i in 1:length(pos.qc)){
  assign(substr(pos.qc[i],1,nchar(pos.qc[i])-4), pos.qc.data[[i]])
}

# PART 1: WITH QUALITY CONTROL INDEX OF GROUP DESIGN

pos.qc.group <- list(g.1.res, g.2.res, g.3.res, g.4.res, g.5.res, g.6.res, g.7.res, g.8.res, g.23.res)

pos.qc.merge.group <- Reduce(function(x,y) merge(x,y,by = "X"), pos.qc.group)
names(pos.qc.merge.group) <- c("MCHT_CD","g.1.res", "g.2.res", "g.3.res", "g.4.res", "g.5.res", "g.6.res", "g.7.res", "g.8.res", "g.23.res")

pos.qc.merge.group[,-1] <- apply(pos.qc.merge.group[,-1], c(1,2), function(x) ifelse(x == TRUE, 1, 0))

mysample <- merge(pos.qc.merge.group, mysample, by = "MCHT_CD", all.y = T)
mysample <- mysample[,-1]


# RESULTS WITH ONLY GROUP ((0.93, 0.55), (0.93, 0.48), (0.93, 0.55), (0.93, 0.55), (0.93, 0.55))
# RESULTS WITH GROUP AND TOTAL ((0.93, 0.53), (0.92, 0.46), (0.93, 0.53), (0.93, 0.53), (0.93, 0.53))

# PART 2: WITH QUALITY CONTROL INDEX OF SINGLE DESIGN(g.single.11 deleted for all value standed TRUE) 

# pos.qc.single <- list(g.single.1, g.single.2, g.single.3, g.single.4, g.single.5, g.single.6, g.single.7, g.single.8, g.single.9, g.single.10,
#                            g.single.12, g.single.13, g.single.14, g.single.15, g.single.16, g.single.17, g.single.18, g.single.19, g.single.20,
#                            g.single.21, g.single.22, g.single.23)
# pos.qc.merge.single <- Reduce(function(x,y) merge(x,y, by = "X"),pos.qc.single)
# names(pos.qc.merge.single) <- c("MCHT_CD","g.single.1", "g.single.2", "g.single.3", "g.single.4", "g.single.5", "g.single.6", "g.single.7", "g.single.8", "g.single.9", "g.single.10",
#                                "g.single.12", "g.single.13", "g.single.14", "g.single.15", "g.single.16", "g.single.17", "g.single.18", "g.single.19", "g.single.20",
#                                "g.single.21", "g.single.22", "g.single.23")
# pos.qc.merge.single[,-1] <- apply(pos.qc.merge.single[,-1], c(1,2), function(x) ifelse(x == TRUE, 1, 0))
# mysample <- merge(pos.qc.merge.single, mysample, by = "MCHT_CD", all.y = T)
# mysample <- mysample[,-1]

# RESULTS WITH ONLY SINGLE ((0.93, 0.53), (0.92, 0.42), (0.93, 0.51), (0.93, 0.52), (0.93, 0.52))
# RESULTS WITH SINGLE AND TOTAL ((0.92, 0.48), (0.92, 0.43), (0.92, 0.48), (0.92,0.49), (0.92, 0.49))

# PART 3: WITH QUALITY CONTROL INDEX OF ONLY g.23.res

# names(g.23.res) <- c("MCHT_CD", "g.23.res")
# g.23.res$g.23.res <- ifelse(g.23.res$g.23.res == TRUE, 1, 0)
# mysample <- merge(g.23.res, mysample, by = "MCHT_CD", all.y = T)
# mysample <- mysample[,-1]

# RESULTS ((0.92, 0.46), (0.92, 0.45), (0.92, 0.47), (0.93, 0.48), (0.93, 0.48))


# data split into train and test
length.train <- ceiling(nrow(mysample)*0.6)
train <- sample(nrow(mysample), length.train)
sample.train <- mysample[train,]
sample.test <- mysample[-train,]


# IV COMPUTE
library(woe)
va <- c()
iv <- c()

# iv <- c()
# IV <- function(x){
#   if(is.numeric(x)){
#     # woe(sample.train, x, Continuous = T, "OVERDUE", C_Bin = 5, Good = "1", Bad = "0")
#     iv <- append(iv, sum((woe(sample.train, names(x), Continuous = T, "OVERDUE", C_Bin = 5, Good = "1", Bad = "0"))$IV))
#   } else if(is.factor(x)){
#     # assign(names(x), woe(sample.train, x, Continuous = F, "OVERDUE", C_Bin = 5, Good = "1", Bad = "0"))
#     # iv <- append(iv, sum(get(names(x))$IV))
#     iv <- append(iv, sum((woe(sample.train, names(x), Continuous = F, "OVERDUE", C_Bin = 5, Good = "1", Bad = "0"))$IV))
#     
#   }
#   return(iv)
# }
# 
# iv.output <- apply(sample.train, 2, IV)





for(i in 1:length(sample.train)){
  if(is.numeric(sample.train[,i])){
    va[i] <- paste(names(sample.train)[i],
                   "WOE",
                   sep = "")
    assign(paste(names(sample.train)[i],
                 "WOE",
                 sep = ""),
           woe(sample.train,
               names(sample.train)[i],
               Continuous = T,
               "OVERDUE",
               C_Bin = 5,
               Good = "1",
               Bad = "0"))
    iv[i] <- sum(get(paste(names(sample.train)[i],
                           "WOE",
                           sep = ""))$IV)
  } else if(is.factor(sample.train[,i])){
    va[i] <- paste(names(sample.train)[i],
                   "WOE",
                   sep = "")
    assign(paste(names(sample.train)[i],
                 "WOE",
                 sep = ""),
           woe(sample.train,
               names(sample.train)[i],
               Continuous = F,
               "OVERDUE",
               C_Bin = 5,
               Good = "1",
               Bad = "0"))
    iv[i] <- sum(get(paste(names(sample.train)[i],
                           "WOE",
                           sep = ""))$IV)
  }
}

infovalue <- data.frame(names(sample.train),iv,row.names = substr(va,1,(nchar(va)-3)))
infovalue <- infovalue[-which(is.infinite(infovalue$iv)),]

# PCA CLASSIFIED
# When could be classified by value, done!
# When can't, chosen minimum variable between PCA1 and PCA2 as one class ?????? any better??????
# library(corrplot)
library(data.tree)
rs.compute <- function(x,y){
  r.squared <- c()
  Ratio <- c()
  temp <- x
  for(i in 1:length(x)){
    add.r.squared <- c()
    temp[, 1:i] <- x[, i:1]
    names(temp)[1] <- "Ddiff"
    r.squared[i] <-  summary(lm(Ddiff~.-Ddiff, data = temp))$r.squared
    for(j in y){
      dtAdd <- data.frame(temp[,1], j)
      names(dtAdd)[1] <- "Ddiff"
      add.r.squared <- append(add.r.squared, summary(lm(Ddiff~.-Ddiff, data = dtAdd))$r.squared)
      # print(add.r.squared)
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
  # list.oleafsdata <- list()
  # for(leaf in list.otherleafs){
  #   list.oleafsdata <- append(list.oleafsdata,leaf$data)
  # }
  ratio  <- rs.compute(currnode$data, list.otherleafs)
  
  if( is.null(parentnode$ratio) ) {
    return( list(TRUE, ratio) ) 
  }else{
    merge <- merge(ratio, parentnode$ratio, by = "names.x.", all.x = T)
    # print( names(merge) )
    decrease.number <- sum(merge$Ratio.x < merge$Ratio.y)
    decrease.ratio <- decrease.number/nrow(ratio)
    # print("decrease.number=")
    # print(decrease.number)
    # print("nrow(ratio)")
    # print(nrow(ratio))
    if(decrease.ratio >= 0.5 | ncol(currnode$data) == 1){
      need.split <- F
    }
    return( list( need.split, ratio ) )
  }
}

library(corrplot)
PCA <- function(x){
  pr.out.class <- prcomp(x, scale = T)
  cor.result <- cor(x, pr.out.class$x[,1:2])
  # corrplot(cor.result, tl.cex = 0.5)
  if(!(all(cor.result[,1] <= cor.result[,2]) | all(cor.result[,1] >= cor.result[,2]))){
    class.1 <- x[,which(cor.result[,1] >= cor.result[,2]), drop = F]
    class.2 <- x[,which(cor.result[,1] < cor.result[,2]), drop = F]
  }else{
    class.1 <- x[,which.min(abs(cor.result[,1] - cor.result[,2])), drop = F]
    class.2 <- x[,-which.min(abs(cor.result[,1] - cor.result[,2])), drop = F]
  }
  return(list(class.1, class.2))
}

# rs.compute(c.1.1, list(c.1.2,c.2.1,c.2.2))


#create tree test data
data.root <- sample.train[,-ncol(sample.train)]
# data.root <- sample.train[,-(ncol(sample.train)-2): -ncol(sample.train)]
t <- Node$new("t",ratio=NULL,needsplit=TRUE,data=data.root)  #root
#create tree
createRESTree <- function(){
  # print("89898989")
  needsplits <- t$Get('needsplit',filterFun = isLeaf)   #get leaf nodes split flag
  names <- t$Get('name',filterFun = isLeaf)             #get leaf nodes name
  datas <- t$Get('data',filterFun = isLeaf)             #get leaf nodes data
  
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
  
  needsplits <- t$Get('needsplit',filterFun = isLeaf)   #get leaf nodes split flag
  names <- t$Get('name',filterFun = isLeaf)             #get leaf nodes name
  datas <- t$Get('data',filterFun = isLeaf)             #get leaf nodes data
  # print(names(datas))
  # print("hello")
  
  nd.index <- 1
  for(nd in names) {
    cur.nd <- get(nd)
    # other.node.names <- names[-node.index] # other leaf nodes name
    # print(as.vector(other.node.names))
    # other.nodes <- mget(as.vector(other.node.names)) # other leaf nodes
    # print("others")
    # print(names(datas[-nd.index]))
    calres <- calNeedSplitAndRatio(cur.nd, cur.nd$parent, datas[-nd.index])
    cur.nd$needsplit <- calres[1]
    cur.nd$ratio <- calres[2]
    nd.index <- nd.index + 1
  }
  
  # print(all(needsplits==FALSE))   #all leaf nodes needsplits==FALSE  break out 
  return("func done")
}


while(any(t$Get('needsplit',filterFun = isLeaf)==TRUE)){
  createRESTree()
}



# When PCA finished, consider ceiling(max(1, IV(currentClass)/IV(allClass)*numbers of index in current class))
# to determine the number left in each class

# names <- t$Get('name',filterFun = isLeaf)             #get leaf nodes name
datas <- t$Get('data',filterFun = isLeaf)             #get leaf nodes data

# index.left <- function(x){
  for(i in 1:length(datas[])){
    new.sample <- t(datas[[i]])
    # print(str(new.sample))
    
    class.sample <- merge(new.sample, infovalue, by = "row.names")
    # print(str(class.sample))
    class.sample.IV <- sum(class.sample[,ncol(class.sample)])
    all.class.IV <- sum(infovalue[,2])
    number.left <- ceiling(max(1, class.sample.IV/all.class.IV*nrow(class.sample)))
   
    #first number.left rows order by IV
    
    reduction.sample <- t(class.sample[order(class.sample[,ncol(class.sample)], decreasing = T)[1:number.left],])
    # print(str(reduction.sample))
    if(i == 1){
      final.sample <- reduction.sample
    }else {
      final.sample <- cbind(final.sample, reduction.sample)
    }
  }

colnames(final.sample) <- final.sample[1,]
final.sample <- final.sample[-1,] #5773
final.sample <- final.sample[-(nrow(final.sample)-1):-nrow(final.sample),]   #5771
# options(stringsAsFactors = FALSE)
final.sample <- data.frame(final.sample,stringsAsFactors = FALSE)
# final.sample <- transform(final.sample, class=as.numeric(as.character(final.sample)))
indx <- sapply(final.sample, is.character)
final.sample[indx] <- lapply(final.sample[indx], function(x) as.numeric(as.character(x)))
final.sample$OVERDUE <- sample.train$OVERDUE



# Stepwise
fullmod <- glm(OVERDUE ~., data = final.sample, family = binomial)
coefficients.fullmod <- summary(fullmod)$coefficients[-1,4]
final.fullmod <- glm(as.formula(paste("OVERDUE~", paste(names(which(coefficients.fullmod < 0.05)), collapse = "+"))),
                 data = final.sample, family = binomial)


nothing <- glm(OVERDUE ~ 1, data = final.sample, family = binomial)


backwards <- step(fullmod)
coefficients.backwards <- summary(backwards)$coefficients[-1,4]
final.backwards <- glm(as.formula(paste("OVERDUE~", paste(names(which(coefficients.backwards < 0.05)),collapse = "+"))),
                       data = final.sample, family = binomial)

forwards <- step(nothing, scope = list(lower = formula(nothing), upper = formula(fullmod)), direction = "forward")
(coefficients.forwards <- summary(forwards)$coefficients[-1,4])
final.forwards <- glm(as.formula(paste("OVERDUE~", paste(names(which(coefficients.forwards < 0.05)), collapse = "+"))),
                      data = final.sample, family = binomial)


bothways <- step(nothing, list(lower = formula(nothing), upper = formula(fullmod)), direction = "both", trace = 0)
coefficients.bothways <- summary(bothways)$coefficients[-1,4]
final.bothways <- glm(as.formula(paste("OVERDUE~", paste(names(which(coefficients.bothways < 0.05)), collapse = "+"))),
                      data = final.sample, family = binomial)



newGLMSelect <- function(p.data) {
  namelist <- colnames(final.sample)
  n <- ncol(p.data)-1
  i.currLayer <- 1         #layer index
  list.formula <- list()   
  c.p <- c(0.00001)        # should have a initvalue  for any(na.omit(c.p) in while condition
  c.minpindex <- c() #max layer -1
  c.index <- 1:(ncol(p.data)-1) #1:25
  while(any(na.omit(c.p)<0.05) & (if(i.currLayer==1){TRUE}else{all(summary(list.formula[[c.minpindex[length(c.minpindex)]]])$coefficients[-1,4] < 0.05)})){
    for( i in c.index){ #if(!is.null(c.minpindex)){c.index[-c.minpindex]}else{c.index[]}
      if(i %in% c.minpindex){
        c.p[i] <- NA
      }else{
        if(!is.null(c.minpindex)){
          xnam <- paste("", namelist[append(i,c.minpindex)], sep="")
          # print(paste("OVERDUE ~ ", paste(xnam, collapse= "+")))
          (fmla <- as.formula(paste("OVERDUE ~ ", paste(xnam, collapse= "+"))))
          list.formula[[i]] <- glm(fmla, data = p.data, family = binomial) # put formula into list.formula # overwrite formula into list.formula
        }else{
          # print(namelist[i])
          list.formula[[i]] <- glm(OVERDUE ~ get(namelist[i]), data = p.data, family = binomial) # put formula into list.formula # overwrite formula into list.formula
        }
        c.p[i] <- summary(list.formula[[i]])$coefficients[2,4] # put p into list.p
        # print(c.p[i])
      }
    }
    c.minpindex <- append(c.minpindex,which.min(c.p))
    i.currLayer <- i.currLayer + 1
  }
  if(i.currLayer==1){
    return("Model failed")
  } else{
    fmla <- as.formula(paste("OVERDUE ~ ", paste(namelist[c.minpindex[-length(c.minpindex)]], collapse = "+")))
    return(glm(fmla, data = p.data, family = binomial))
    # return(c.minpindex[-1])
  }
}
stepwiseGlm <- newGLMSelect(final.sample)


# Model for verify
# stepwiseGlm <- glm(OVERDUE ~ CT0+CT1+APP_OPEN_RD30+TXNAMT1011+UPDATE+STATUS0+SIGN+CT2+NORMALTERM_COUNT+CTAMT2+NOPASS,
#                    data = final.sample, family = binomial)
# summary(stepwiseGlm)

library(ggplot2)

compare.table <- function(x, dataset){
  glm.probs <- predict(x, dataset, type = "response")
  contrasts(factor(dataset$OVERDUE))
  glm.pred <- rep("0", nrow(dataset))
  glm.pred[glm.probs > 0.5] = "1"
  table.rate <- table(glm.pred, factor(dataset$OVERDUE))
  print(table.rate)
  total.correct.rate <- (table.rate[1,1]+table.rate[2,2])/(sum(table.rate))
  overdue.correct.rate <- table.rate[2,2]/(table.rate[1,2]+table.rate[2,2])
  return(c(total.correct.rate, overdue.correct.rate))
}

if(!is.character(stepwiseGlm)){
  compare.table(stepwiseGlm, sample.train)
}

compare.table(final.fullmod, sample.train)
compare.table(final.backwards, sample.train)
compare.table(final.forwards, sample.train)
compare.table(final.bothways, sample.train)

if(!is.character(stepwiseGlm)){
  compare.table(stepwiseGlm, sample.test)
}

compare.table(final.fullmod, sample.test)
compare.table(final.backwards, sample.test)
compare.table(final.forwards, sample.test)
compare.table(final.bothways, sample.test)



# Gini index and Lorenz Curve
library(ineq)
gini.index <- Gini(predict(stepwiseGlm, sample.test, type = "response"))
Distr <- predict(stepwiseGlm, type = "response")
Distr <- Lc(Distr, n = rep(1,  length(Distr)), plot = F)
plot(Distr$p, Distr$L,
     col = "black",
     type = "b",
     lty = 1,
     lwd = 3,
     main = "Lorenz Curve for Distributions")
points(c(0,1), c(0,1), type = "l", lty = 2, lwd = 2, col = "grey")


library(ROCR)
pred <- prediction(predict(stepwiseGlm, sample.test, type = "response"), sample.test$OVERDUE)
perf <- performance(pred, measure = "rec", x.measure = "rpp")
plot(perf, colorize = T)
grid(5, 5, lwd = 1)
points(c(0,1), c(0,1), type = "l", lty = 2, lwd = 2, col = "grey")





 


