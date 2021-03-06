# SQL获取源数据
# -- 两个月全量流水数据  
# create table ten_pos_01 as 
# select * from apms_posflow where inst_date between '20161001' and '20161130'; 


# create table mx_pos_01 as 
# select * from ten_pos_01 where ten_pos_01.MCHT_CD in 
# (select mcht_cd from s9910 where data_dt = to_date('20161201','YYYYMMDD') and s9910.IS_SIGN = '是'
# );


# -- 430271
# select count(*) from mx_pos_01;


# --  344329
# select * from mx_pos_01 where mx_pos_01.TRANS_AMT >1;
# irowcount = 5200
# icolcount = 61

# 取消科学记数法展示 
options(scipen=3)


mx.gt1 <- read.csv('E:\\Allinpay\\Data\\LOAN\\sqlExport\\POSQC\\zsy_signal_pos\\mx_flow_01_gt1.csv',header=TRUE) 
mcht.openclose <- read.csv('E:\\Allinpay\\Data\\LOAN\\sqlExport\\POSQC\\zsy_signal_pos\\MerchantID.csv',sep=',',header=TRUE)
## 日交易金额
taly.day.amt <- tapply(mx.gt1$TRANS_AMT,list(mx.gt1$MCHT_CD,mx.gt1$INST_DATE),sum)
## 日交易笔数
taly.day.cnt <- tapply(mx.gt1$TRANS_AMT,list(mx.gt1$MCHT_CD,mx.gt1$INST_DATE),length)


## 非营业时间交易金额/笔数
# 关联营业时间
merg.mcht <- merge(x = mx.gt1,y = mcht.openclose, by = "MCHT_CD")

# 非营业时间交易数据
merg.close.trx <- merg.mcht[ which( strptime(merg.mcht$INST_TIME,"%H%M%S") < strptime(merg.mcht$STARTTIME,"%H:%M")  |  strptime(merg.mcht$INST_TIME,"%H%M%S") > strptime(merg.mcht$ENDTIME,"%H:%M")  ), ]
taly.close.amt <- tapply(merg.close.trx$TRANS_AMT,list(merg.close.trx$MCHT_CD,merg.close.trx$INST_DATE),sum)
taly.close.cnt <- tapply(merg.close.trx$TRANS_AMT,list(merg.close.trx$MCHT_CD,merg.close.trx$INST_DATE),length)

## 10:00 pm-8:00 am 交易金额/笔数
merg.2208.trx <- merg.mcht[ which( strptime(merg.mcht$INST_TIME,"%H%M%S") >= strptime("22:00","%H:%M") | strptime(merg.mcht$INST_TIME,"%H%M%S") <= strptime("8:00","%H:%M")  ), ]
taly.2208.amt <- tapply(merg.2208.trx$TRANS_AMT,list(merg.2208.trx$MCHT_CD,merg.2208.trx$INST_DATE),sum)
taly.2208.cnt <- tapply(merg.2208.trx$TRANS_AMT,list(merg.2208.trx$MCHT_CD,merg.2208.trx$INST_DATE),length)


## 交易类型金额/笔数占比 1-4
# 交易类型唯一值
unique(merg.mcht[c("TXN_NUM")])

   # TXN_NUM
   #    1011 消费
   #   3011  消费撤销
   #  1031  预授权完成
   #  3031  预授权完成撤销

# 1011 
merg.n1011.trx <- merg.mcht[ which( merg.mcht$TXN_NUM == "1011" ), ]
taly.n1011.amt <- tapply(merg.n1011.trx$TRANS_AMT,list(merg.n1011.trx$MCHT_CD,merg.n1011.trx$INST_DATE),sum)
taly.n1011.cnt <- tapply(merg.mcht$TXN_NUM,list(merg.mcht$MCHT_CD,merg.mcht$INST_DATE),FUN = function(x) sum(x == 1011)/length(x) )

# 3011
merg.n3011.trx <- merg.mcht[ which( merg.mcht$TXN_NUM == "3011" ), ]
taly.n3011.amt <- tapply(merg.n3011.trx$TRANS_AMT,list(merg.n3011.trx$MCHT_CD,merg.n3011.trx$INST_DATE),sum)
taly.n3011.cnt <- tapply(merg.mcht$TXN_NUM,list(merg.mcht$MCHT_CD,merg.mcht$INST_DATE),FUN = function(x) sum(x == 3011)/length(x) )

# 1031
merg.n1031.trx <- merg.mcht[ which( merg.mcht$TXN_NUM == "1031" ), ]
taly.n1031.amt <- tapply(merg.n1031.trx$TRANS_AMT,list(merg.n1031.trx$MCHT_CD,merg.n1031.trx$INST_DATE),sum)
taly.n1031.cnt <- tapply(merg.mcht$TXN_NUM,list(merg.mcht$MCHT_CD,merg.mcht$INST_DATE),FUN = function(x) sum(x == 1031)/length(x) )


# 3031
merg.n3031.trx <- merg.mcht[ which( merg.mcht$TXN_NUM == "3031" ), ]
taly.n3031.amt <- tapply(merg.n3031.trx$TRANS_AMT,list(merg.n3031.trx$MCHT_CD,merg.n3031.trx$INST_DATE),sum)
taly.n3031.cnt <- tapply(merg.mcht$TXN_NUM,list(merg.mcht$MCHT_CD,merg.mcht$INST_DATE),FUN = function(x) sum(x == 3031)/length(x) )



## 用卡类型金额/笔数占比 1-3
# 0
# 1
# 2
merg.c0.trx <- merg.mcht[ which( merg.mcht$CARD_TYPE == "0" ), ]
taly.c0.amt <- tapply(merg.c0.trx$TRANS_AMT,list(merg.c0.trx$MCHT_CD,merg.c0.trx$INST_DATE),sum)
taly.c0.cnt <- tapply(merg.mcht$CARD_TYPE,list(merg.mcht$MCHT_CD,merg.mcht$INST_DATE),FUN = function(x) sum(x == 0)/length(x))


merg.c1.trx <- merg.mcht[ which( merg.mcht$CARD_TYPE == "1" ), ]
taly.c1.amt <- tapply(merg.c1.trx$TRANS_AMT,list(merg.c1.trx$MCHT_CD,merg.c1.trx$INST_DATE),sum)
taly.c1.cnt <- tapply(merg.mcht$CARD_TYPE,list(merg.mcht$MCHT_CD,merg.mcht$INST_DATE),FUN = function(x) sum(x == 1)/length(x))


merg.c2.trx <- merg.mcht[ which( merg.mcht$CARD_TYPE == "2" ), ]
taly.c2.amt <- tapply(merg.c2.trx$TRANS_AMT,list(merg.c2.trx$MCHT_CD,merg.c2.trx$INST_DATE),sum)
taly.c2.cnt <- tapply(merg.mcht$CARD_TYPE,list(merg.mcht$MCHT_CD,merg.mcht$INST_DATE),FUN = function(x) sum(x == 2)/length(x))


## 当日使用终端数量
taly.term.cnt <- tapply( merg.mcht$TERM_ID,list(merg.mcht$MCHT_CD,merg.mcht$INST_DATE), FUN = function(x) length(unique(x)) )

uni.mcht = unique(mcht.openclose["MCHT_CD"])
rownames(uni.mcht) <- uni.mcht[,1]

dlist = list(20161001,20161002,20161003,20161004,20161005
                    ,20161006,20161007,20161008,20161009,20161010
                    ,20161011,20161012,20161013,20161014,20161015
                    ,20161016,20161017,20161018,20161019,20161020
                    ,20161021,20161022,20161023,20161024,20161025
                    ,20161026,20161027,20161028,20161029,20161030
                    ,20161031,20161101,20161102,20161103,20161104
                    ,20161105,20161106,20161107,20161108,20161109
                    ,20161110,20161111,20161112,20161113,20161114
                    ,20161115,20161116,20161117,20161118,20161119
                    ,20161120,20161121,20161122,20161123,20161124
                    ,20161125,20161126,20161127,20161128,20161129
                    ,20161130)

fillMatrix <- function(template,mat,col1,col2){
	merge.mat <- merge(  x = template , y = mat  , by.x=col1  , by.y = col2 , all.x = TRUE )
	rownames(merge.mat) <- merge.mat[,1]
	merge.mat<-merge.mat[,-1]
	if( ncol(merge.mat) < length(dlist) ){   # col nums not match days num then
		for( dnm in dlist ){
				if( !(dnm %in% colnames(merge.mat)) ){
					# print(dnm)
					merge.mat <- transform(merge.mat,  dnm, check.names=FALSE)    # add the missing columns
					inx = paste(dnm,sep='')                                       # the transform function set dnm col values defaults to dnm itself , next two line convert it to 0
					merge.mat[inx] <- 0.00
			}
		}
	}
	merge.mat <- merge.mat[, order(as.integer(colnames(merge.mat)))]
	return(merge.mat)
}

olist <- list(  "taly.day.amt", "taly.day.cnt", "taly.term.cnt",
				"taly.n1011.amt", "taly.n3011.amt", "taly.n1031.amt", "taly.n3031.amt",
				"taly.n1011.cnt", "taly.n3011.cnt", "taly.n1031.cnt", "taly.n3031.cnt",
				"taly.c0.amt", "taly.c1.amt", "taly.c2.amt",
				"taly.c0.cnt", "taly.c1.cnt", "taly.c2.cnt",
				"taly.close.amt", "taly.2208.amt",
				"taly.close.cnt", "taly.2208.cnt")

for( o in olist ){
	oname = paste("merge.",o,sep='')   # add prefix   as  object name
	print(oname)  
	res = fillMatrix(uni.mcht,get(o),"MCHT_CD","row.names")
	res[is.na(res)] <- 0.00
	assign(oname,res, envir = .GlobalEnv)   # produce a global object with merge prefix
}


iContiZeroFromRight <- function(vec,n)
{
  res = 0
  len = length(vec)
  if(len-n-1<1){
  		begin = 1
	}else{
		begin = len-n-1
	}
  for( i in len:begin ){
  	if( vec[i] == 0 ){
  		res = res + 1
  	}else{
  		return(res)
  	}
  }
  res
}

mx.30contina.cnt = merge.taly.day.cnt  # matrix(nrow=nrow(taly.day.cnt), ncol=ncol(taly.day.cnt))

for(i in 1:nrow(merge.taly.day.cnt)) {
  for(j in 1:ncol(merge.taly.day.cnt)) {
   		mx.30contina.cnt[i,j] <- iContiZeroFromRight(merge.taly.day.cnt[i,1:j],30)  # 30内持续空白
  }
}


# 30天内流水累计空白时长（流水时长[2016.10.02, 2016.11.30]）
# iAllNaFromRight <- function(vec,n)
# {
#   res = 0
#   len = length(vec)
#   if(len-n-1<1){
#   		begin = 1
# 	}else{
# 		begin = len-n-1
# 	}

#   sum( is.na(vec[begin:len]) )
# }

iAllZeroFromRight <- function(vec,n)
{
  res = 0
  len = length(vec)
  if(len-n-1<1){
  		begin = 1
	}else{
		begin = len-n-1
	}

  sum( vec[begin:len] == 0 )
}

mx.30allna.cnt = merge.taly.day.cnt  # matrix(nrow=nrow(taly.day.cnt), ncol=ncol(taly.day.cnt))

for(i in 1:nrow(merge.taly.day.cnt)) {
  for(j in 1:ncol(merge.taly.day.cnt)) {
   		mx.30allna.cnt[i,j] <- iAllZeroFromRight(merge.taly.day.cnt[i,1:j],30)  # 30内累计空白
  }
}

## 标准化数据

normData <- function(mrx)
{
  mrx <- mrx[,32:ncol(mrx)]
  out <- scale(t(mrx), center = TRUE, scale = TRUE)  #scale标准化列  转置 标准化后转回来
                    #如果所有的值一样 标准化的分母标准差为0因此标准化后为NaN  把NaN转换成0
  out[is.nan(out)] <- 0
  return(t(out))
}


## 样本分组

# group 1   1. 日交易金额，日交易笔数，当日使用终端数量；（n = 3）
g.1.1 <- normData(merge.taly.day.amt)
g.1.2 <- normData(merge.taly.day.cnt)
g.1.3 <- normData(merge.taly.term.cnt)

# group 2   2. 交易类型金额；（n = 4）
g.2.1 <- normData(merge.taly.n1011.amt)
g.2.2 <- normData(merge.taly.n3011.amt)
g.2.3 <- normData(merge.taly.n1031.amt)
g.2.4 <- normData(merge.taly.n3031.amt)
#group 3  交易类型笔数占比；（n = 4）
g.3.1 <- normData(merge.taly.n1011.cnt)
g.3.2 <- normData(merge.taly.n3011.cnt)
g.3.3 <- normData(merge.taly.n1031.cnt)
g.3.4 <- normData(merge.taly.n3031.cnt)

#group 4 用卡类型金额；（n = 3）
g.4.1 <- normData(merge.taly.c0.amt)
g.4.2 <- normData(merge.taly.c1.amt)
g.4.3 <- normData(merge.taly.c2.amt)

#group 5 用卡类型笔数占比；（n = 3）
g.5.1 <- normData(merge.taly.c0.cnt)
g.5.2 <- normData(merge.taly.c1.cnt)
g.5.3 <- normData(merge.taly.c2.cnt)

#group 6 非营业时间交易金额，10:00 pm-8:00 am 交易金额；（n = 2）
g.6.1 <- normData(merge.taly.close.amt)
g.6.2 <- normData(merge.taly.2208.amt)

#group 7 非营业时间交易笔数，10:00 pm-8:00 am 交易笔数；（n = 2)
g.7.1 <- normData(merge.taly.close.cnt)
g.7.2 <- normData(merge.taly.2208.cnt)

#group 8  30天内流水持续空白时长，30天内流水累计空白时长；（n = 2）
g.8.1 <- normData(mx.30contina.cnt)
g.8.2 <- normData(mx.30allna.cnt)

##8 group
group1 = list(g.1.1,g.1.2,g.1.3)
group2 = list(g.2.1,g.2.2,g.2.3,g.2.4)
group3 = list(g.3.1,g.3.2,g.3.3,g.3.4)
group4 = list(g.4.1,g.4.2,g.4.3)
group5 = list(g.5.1,g.5.2,g.5.3)
group6 = list(g.6.1,g.6.2)
group7 = list(g.7.1,g.7.2)
group8 = list(g.8.1,g.8.2)

##all group
groupall = list(g.1.1,g.1.2,g.1.3,g.2.1,g.2.2,g.2.3,g.2.4,g.3.1,g.3.2,g.3.3,g.3.4,g.4.1,g.4.2,g.4.3,g.5.1,g.5.2,g.5.3,g.6.1,g.6.2,g.7.1,g.7.2,g.8.1,g.8.2)

# 用矩阵构建一个参数表
# 其中，当n = 2时， A2 = 1.880， D3 = 0， D4 = 3.267；
# 当n = 3时， A2 = 1.023， D3 = 0， D4 = 2.574；
# 当n = 4时， A2 = 0.729， D3 = 0， D4 = 2.282。
# 其中，n = 23，A3 = 0.633， B3 = 0.545， B4 = 1.455。
# 其中，n = 1,  d2 = 1.128，D3 = 0， D4 = 3.267

cells <- c(1.880,0,3.267,
           1.023,0,2.574,
           0.729,0,2.282,
           0.633,0.545,1.455,
           1.128,0,3.267)

rnames <- c(2,3,4,23,1)

cnames <- c("A2|A3|d2","D3","D4")

m.params <-  matrix( cells, nrow=5, ncol=3, byrow=TRUE, dimnames=list(rnames,cnames) )



## 分组的计算逻辑，遍历所有8个group，生成8个objects结果，输出csv文件
groupControlCharts <- function(g){
  A2 <- m.params[toString(length(g)),"A2|A3|d2"]
  D3 <- m.params[toString(length(g)),"D3"]
  D4 <- m.params[toString(length(g)),"D4"]
  # xba <- g[1]
  # xba[1==1] <- 0
  .xba <- Reduce('+',g)/length(g)   # add all matrixs in a list  
  .max <- do.call(pmax,g)
  .min <- do.call(pmin,g)
  .R <- .max - .min
  .xbaba <- rowMeans(.xba, na.rm = FALSE, dims = 1)
  .rba <- rowMeans(.R, na.rm = FALSE, dims = 1)

  .CLx <- .xbaba
  .CLr <- .rba

  UCLx <- .CLx + A2*.CLr
  LCLx <- .CLx - A2*.CLr

  UCLr <- D4*.rba
  LCLr <- D3*.rba

  # 1.  超出上下限
  # 2.  连续8个点位于均值同一侧
  # 3.  连续8个点持续上升或下降

  points8 <- .xba[,(ncol(.xba)-8):ncol(.xba)]
  
  points8.max <- apply(points8,1,max)

  points8.min <- apply(points8,1,min)
  
  points8.r <- .R[,(ncol(.R)-8):ncol(.R)]
  
  points8.r.max <- apply(points8.r,1,max)
  
  points8.r.min <- apply(points8.r,1,min)
  
  .warn1 <- .xba[,ncol(.xba)]>UCLx | .xba[,ncol(.xba)]<LCLx
  
  .warn2 <- points8.min>=.CLx | points8.max<=.CLx

  .warn3 <- all(points8 == cummax(points8))

  .warn4 <- .R[,ncol(.R)]>UCLr | .R[,ncol(.R)]<LCLr

  .warn5 <- points8.r.min>=.CLr | points8.r.max<=.CLr
  
  .warn6 <- all(points8.r == cummax(points8.r))

  .res = .warn1|.warn2|.warn3|.warn4|.warn5|.warn6

  .res <- transform(.res, check.names=FALSE) 

  return(.res)
}

g.1.res = groupControlCharts(group1)
g.2.res = groupControlCharts(group2)
g.3.res = groupControlCharts(group3)
g.4.res = groupControlCharts(group4)
g.5.res = groupControlCharts(group5)
g.6.res = groupControlCharts(group6)
g.7.res = groupControlCharts(group7)
g.8.res = groupControlCharts(group8)

table(g.1.res)
table(g.2.res)
table(g.3.res)
table(g.4.res)
table(g.5.res)
table(g.6.res)
table(g.7.res)
table(g.8.res)


write.csv(g.1.res, file = paste("E:\\Allinpay\\Data\\LOAN\\sqlExport\\POSQC\\zsy_signal_pos\\output\\g.1.res.csv",sep=''), row.names = T)
write.csv(g.2.res, file = paste("E:\\Allinpay\\Data\\LOAN\\sqlExport\\POSQC\\zsy_signal_pos\\output\\g.2.res.csv",sep=''), row.names = T)
write.csv(g.3.res, file = paste("E:\\Allinpay\\Data\\LOAN\\sqlExport\\POSQC\\zsy_signal_pos\\output\\g.3.res.csv",sep=''), row.names = T)
write.csv(g.4.res, file = paste("E:\\Allinpay\\Data\\LOAN\\sqlExport\\POSQC\\zsy_signal_pos\\output\\g.4.res.csv",sep=''), row.names = T)
write.csv(g.5.res, file = paste("E:\\Allinpay\\Data\\LOAN\\sqlExport\\POSQC\\zsy_signal_poS\\output\\g.5.res.csv",sep=''), row.names = T)
write.csv(g.6.res, file = paste("E:\\Allinpay\\Data\\LOAN\\sqlExport\\POSQC\\zsy_signal_pos\\output\\g.6.res.csv",sep=''), row.names = T)
write.csv(g.7.res, file = paste("E:\\Allinpay\\Data\\LOAN\\sqlExport\\POSQC\\zsy_signal_pos\\output\\g.7.res.csv",sep=''), row.names = T)
write.csv(g.8.res, file = paste("E:\\Allinpay\\Data\\LOAN\\sqlExport\\POSQC\\zsy_signal_pos\\output\\g.8.res.csv",sep=''), row.names = T)



##样本不分组
allControlCharts <- function(g){
  A3 <- m.params[toString(length(g)),"A2|A3|d2"]
  B3 <- m.params[toString(length(g)),"D3"]
  B4 <- m.params[toString(length(g)),"D4"]

  .xba <- Reduce('+',g)/length(g)   # add all matrixs in a list  
  .xbaba <- rowMeans(.xba, na.rm = FALSE, dims = 1)
  # .max <- do.call(pmax,g)
  # .min <- do.call(pmin,g)
  # .R <- .max - .min

  # View(.xba)
  # formula3 <- function(x,y){((x-1)^2)+((y-1)^2)}

  # .S_ini <- Reduce(function(x,y){((x-.xba)^2)+((y-.xba)^2)},g)     # 好像不太对
  .S_ini <- 0
  for(i in g){
     .S_ini <- .S_ini + (i-.xba)^2
  }

  .S <- sqrt(.S_ini/(length(g)-1))
  # View(.S)

  # .rba <- rowMeans(.R, na.rm = FALSE, dims = 1)  
  .Sba <- rowMeans(.S, na.rm = FALSE, dims = 1)

  .CLx <- .xbaba
  .CLs <- .Sba

  UCLx <- .CLx + A3*.CLs
  LCLx <- .CLx - A3*.CLs

  UCLs <- B4*.CLs
  LCLs <- B3*.CLs

#   出现以下情况之一则判定为异常值：
# 1.  超出上下限
# 2.  连续8个点位于均值同一侧
# 3.  连续8个点持续上升或下降
  
  points8 <- .xba[,(ncol(.xba)-8):ncol(.xba)]

  points8.max <- apply(points8,1,max)

  points8.min <- apply(points8,1,min)
  
  points8.s <- .S[,(ncol(.S)-8):ncol(.S)]
  
  points8.s.max <- apply(points8.s,1,max)
  
  points8.s.min <- apply(points8.s,1,min)

  .warn1 <- .xba[,ncol(.xba)]>UCLx | .xba[,ncol(.xba)]<LCLx

  .warn2 <- points8.min>=.CLx | points8.max<=.CLx

  .warn3 <- all(points8 == cummax(points8))

  .warn4 <- .S[,ncol(.S)]>UCLs | .S[,ncol(.S)]<LCLs

  .warn5 <- points8.s.min>=.CLs | points8.s.max<=.CLs
  
  .warn6 <- all(points8.s == cummax(points8.s))

  .res <- .warn1|.warn2|.warn3|.warn4|.warn5|.warn6

  .res <- transform(.res, check.names=FALSE) 

  return(.res)

}

g.23.res <- allControlCharts(groupall)
table(g.23.res)
write.csv(g.23.res, file = paste("E:\\Allinpay\\Data\\LOAN\\sqlExport\\POSQC\\zsy_signal_pos\\output\\g.23.res.csv",sep=''), row.names = T)

## ##单个变量
singleControlCharts <- function(g){
  d2 <- m.params["1","A2|A3|d2"]
  D3 <- m.params["1","D3"]
  D4 <- m.params["1","D4"]

  # .xba <- Reduce('+',g)/length(g)   # add all matrixs in a list  
  .xba <- rowMeans(g, na.rm = FALSE, dims = 1)

  r <- 0
  for( i in ncol(g):2 ){
   r <- cbind(r,abs(g[,i] - g[,i-1]))
  }

  .MRba <- rowMeans(r[,2:ncol(r)], na.rm = FALSE, dims = 1)

  .CLx <- .xba
  .CLs <- .MRba

  UCLx <- .CLx + (3/d2)*.CLs
  LCLx <- .CLx - (3/d2)*.CLs

  UCLs <- D4*.CLs
  LCLs <- D3*.CLs



#   出现以下情况之一则判定为异常值：
# 1.  超出上下限
# 2.  连续8个点位于均值同一侧
# 3.  连续8个点持续上升或下降

  points8 <- g[,(ncol(g)-8):ncol(g)]

  points8.max <- apply(points8,1,max)

  points8.min <- apply(points8,1,min)
  
  points8.MR <- r[,(ncol(r)-8):ncol(r)]
  
  points8.MR.max <- apply(points8.MR,1,max)
  
  points8.MR.min <- apply(points8.MR,1,min)

  .warn1 <- g[,ncol(g)]>UCLx | g[,ncol(g)]<LCLx

  .warn2 <- points8.min>=.CLx | points8.max<=.CLx

  .warn3 <- all(points8 == cummax(points8))

  .warn4 <- r[,ncol(r)]>UCLs | r[,ncol(r)]<LCLs

  .warn5 <- points8.MR.min>=.CLs | points8.MR.max<=.CLs
  
  .warn6 <- all(points8.MR == cummax(points8.MR))

  .res <- .warn1|.warn2|.warn3|.warn4|.warn5|.warn6

  .res <- transform(.res, check.names=FALSE) 

  return(.res)

}


i<-1
for(g in groupall){
  resname = paste("g.single." ,i,  sep='')
  assign(resname,singleControlCharts(g), envir = .GlobalEnv)
  # print(table(get(resname)))
  write.csv(get(resname), file = paste("E:\\Allinpay\\Data\\LOAN\\sqlExport\\POSQC\\zsy_signal_pos\\output\\",resname,".csv",sep=''), row.names = T)
  i<-i+1
}


# oname = paste("merge.",o,sep='')   # add prefix   as  object name
#   print(oname)  
#   res = fillMatrix(uni.mcht,get(o),"MCHT_CD","row.names")
#   res[is.na(res)] <- 0.00
#   assign(oname,res, envir = .GlobalEnv)



# cbind(m1[,3]-m1[,2],m1[,2]-m1[,1])


# m1 = matrix(c(2,4,5,34,5,6,5,3,5),nrow=3,byrow=TRUE)
# m2 = matrix(c(1,2,1,3,15,3,15,13,5),nrow=3,byrow=TRUE)
# m3 = matrix(c(2,24,5,34,5,6,5,3,5),nrow=3,byrow=TRUE)

# listall = list(m1,m2,m3)
# View(Reduce('+',listall))
# View(Reduce(function(x,y){(x-1)^2+(y-1)^2},listall))









