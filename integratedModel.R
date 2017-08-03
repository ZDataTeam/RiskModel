lapply(list("woe", "data.tree", "ggplot2", "car", "glmnet", "randomForest",
          "caret", "gbm", "plyr", "survival", "ROCR"), require, character.only = T)

# --------------------Logistics---------------------------------------------------- #
Logistic.Regression <- function(sample.train){
  
  # IV compute
  va <- c()
  iv <- c()
  
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
  
  # PCA
    # R-squared ratio computation
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
  
    # tree split
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
  
    # PCA into two class
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
  
  # Stepwise with Logistics
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
  return(stepwiseGlm)
}
# --------------------------------------------------------------------------------- #

# ---------------------------Ridge------------------------------------------------- #
Ridge.Regression <- function(sample.train){
  x.train <- as.matrix(sample.train[,-ncol(sample.train)])
  y.train <- as.double(as.matrix(sample.train[,ncol(sample.train)]))
  
  cv.ridge <- cv.glmnet(x.train, y.train, family = 'binomial', alpha = 0,
                        parallel = T, standardize = T, type.measure = 'auc')
  
  return(cv.ridge)
}

# --------------------------------------------------------------------------------- #

# ---------------------------Lasso------------------------------------------------- #
Lasso.Regression <- function(sample.train){
  x.train <- as.matrix(sample.train[,-ncol(sample.train)])
  y.train <- as.double(as.matrix(sample.train[,ncol(sample.train)]))
  
  cv.lasso <- cv.glmnet(x.train, y.train, family = 'binomial', alpha = 1,
                        paralle = T, standardize = T, type.measure = 'auc')
  
  return(cv.lasso)
}
# --------------------------------------------------------------------------------- #

# -----------------------------RandomForest---------------------------------------- #
Random.Forest <- function(sample.train){
  sample.train$OVERDUE <- as.factor(sample.train$OVERDUE)
  rf.fit <- randomForest(OVERDUE~., sample.train, ntree = 500)
  
  return(rf.fit)
}
# --------------------------------------------------------------------------------- #

# -----------------------------GBM------------------------------------------------- #
Gradient.Boosting.Machine <- function(sample.train){
  sample.train$OVERDUE <- as.factor(sample.train$OVERDUE)
  fitControl <- trainControl(method = "cv",
                             number = 10)
  
  tune_Grid <- expand.grid(interaction.depth = 2, # the complexity of the tree i.e. total number of splits
                           # it has to perform on a tree
                           # setted as 6 could get a much high accuracy
                           n.trees = 500,  # number of iterations i.e. tree which will be taken to grow the trees
                           shrinkage = 0.1, # learning rate
                           n.minobsinnode = 10) # minimum number of training samples required in a node to perform splitting
  
  gbm.fit <- train(OVERDUE~., data = sample.train,
               method = "gbm",
               trControl = fitControl,
               verbose = F, # no output generated
               tuneGrid = tune_Grid)
  
  return(gbm.fit)
}
# --------------------------------------------------------------------------------- #

# -----------------------------LorenzCurve----------------------------------------- #
Lorenz.curve <- function(algorithm, sample.test, dependent.variable){
  pred <- prediction(predict(algorithm, sample.test, type = "response"),
                     dependent.variable)
  perf <- performance(pred, measure = "rec", x.measure = "rpp")
  plot(perf, colorize = T, xlab = "Accumalated paid rate",
       ylab = "Accumulated default rate",
       main = "Lorenz Curve for Distributions")
  grid(5, 5, lwd = 1)
  points(c(0,1), c(0,1),
         type = "l",
         lty = 2,
         lwd = 2,
         col = "grey")
}
# -------------------------------------------------------------------------------- #

# -------Probability vs. Default Proporsition + Prediction Accuracy--------------- #
prob.interval.table <- function(model, test.data, dependent){
  if(length(grep("lambda", names(model)))){
    probs <- predict(model, s = model$lambda.min, as.matrix(test.data[,-ncol(test.data)]), type = "response")
  } else if(length(grep("bestTune", names(model))) | length(grep("forest", names(model)))){
    probs <- predict(model, test.data[,-ncol(test.data)], type = "prob")[,2]
  } else{
    probs <- predict(model, test.data, type = "response")
  }
  
  # contrasts(factor(dependent))
  probs.cut <- cut(probs, breaks = seq(0, 1, 0.1))
  model.table.segm <- table(probs.cut, factor(dependent))
  
  pred <- rep("0", nrow(test.data))
  pred[probs > 0.5] = "1"
  pred.table <- table(pred, factor(dependent))
  total.correct.rate <- (pred.table[1,1]+pred.table[2,2])/sum(pred.table)
  overdue.correct.rate <- pred.table[2,2]/sum(pred.table[,2])
  
  return(list(model.table.segm, pred.table, total.correct.rate, overdue.correct.rate))
}
# --------------------------------------------------------------------------------- #


# ------------------------- DATA INPUT------------------------------------------- #
mysample <- read.csv(# "D:\\Allinpay\\Data\\riskData\\sample.csv",
  "D:\\Allinpay\\Data\\riskData\\withQCGroup.csv",
  # "D:\\Allinpay\\Data\\riskData\\withQCOneGroup.csv",
  # "D:\\Allinpay\\Data\\riskData\\withQCSingle.csv",
  header = T,
  stringsAsFactors = T)
options(scipen=3)


# data split into train and test
length.train <- ceiling(nrow(mysample)*0.6)
train <- sample(nrow(mysample), length.train)
sample.train <- mysample[train,]
sample.test <- mysample[-train,]

# --------------------------------------------------------------------------------- #


# ------------------------------Valuation Results--------------------------------- #
logistic <- Logistic.Regression(sample.train)
cv.ridge <- Ridge.Regression(sample.train)
cv.lasso <- Lasso.Regression(sample.train)
rf.fit <- Random.Forest(sample.train)
gbm.fit <- Gradient.Boosting.Machine(sample.train)

prob.interval.table(logistic, sample.test, sample.test$OVERDUE)
prob.interval.table(cv.ridge, sample.test, sample.test$OVERDUE)
prob.interval.table(cv.lasso, sample.test, sample.test$OVERDUE)
prob.interval.table(rf.fit, sample.test, sample.test$OVERDUE)
prob.interval.table(gbm.fit, sample.test, sample.test$OVERDUE)

# -------------------------------------------------------------------------------- #

