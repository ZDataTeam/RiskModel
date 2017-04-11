# DATA INPUT
mysample <- read.csv("E:\\Allinpay\\Data\\LOAN\\sqlExport\\sample.csv",
                     header = T,
                     stringsAsFactors = T)
options(scipen=3)

# TO BE SOLVED!!! HANDLING IMBALANCE!!! RESULTS INDICATED DATA IMBALANCE COULD CAUSE PREDICTION RATE DECREASE

# random insert
# index <- sample(c(1:nrow(mysample)-1),length(mysample[which(mysample$OVERDUE != 0),]),replace = T)
# mysample <- rbind(mysample[index,],mysample[which(mysample$OVERDUE != 0),],mysample[index+1,])

# rbind directly
# mysample <-rbind(mysample, mysample[which(mysample$OVERDUE != 0),])

# TO BE SOLVED!!! WHETHER TERMINAL IN KEEPING CURRENT NODE OR BACK TO FATHER NODE!!!(VERIFY)

# mysample$OVERDUE <- as.factor(mysample$OVERDUE)

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
    class.1 <- x[,which.min(abs(cor.result[,1] - cor.result[,2])), drop = F]
# TO DO!!!!!!!!!!!!!!!!
    # colnames(class.1) <- names(x[,which.min(abs(cor.result[,1] - cor.result[,2]))])
    # print(names(class.1))
    # View(class.1)
    # print(names(x)[which.min(abs(cor.result[,1] - cor.result[,2]))])
    class.2 <- x[,-which.min(abs(cor.result[,1] - cor.result[,2]))]
  }
  return(list(class.1, class.2))
}

# rs.compute(c.1.1, list(c.1.2,c.2.1,c.2.2))


#create tree test data
data.root <- mysample[,-ncol(mysample)]
# data.root <- mysample[,-(ncol(mysample)-2): -ncol(mysample)]
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
final.sample$OVERDUE <- mysample$OVERDUE



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
compare.table <- function(x){
  glm.probs <- predict(x, type = "response")
  contrasts(factor(final.sample$OVERDUE))
  glm.pred <- rep("0", nrow(final.sample))
  glm.pred[glm.probs > 0.5] = "1"
  table.rate <- table(glm.pred, factor(final.sample$OVERDUE))
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





