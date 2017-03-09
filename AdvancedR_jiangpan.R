########## 题目1，耗时30分钟############
findNum <- function(x,l){
  n <- length(x)
  Num <- NULL
  for(i in 1:(n-l+1)){
    if(all(x[i:(i+l-1)] == 1)) Num <- c(Num,i)
  }
  return(Num)
}

y <- c(1,0,0,1,1,1,0,1,1)
findNum(y,2)
#[1] 4 5 8
##上面写法得到答案与举例一致。
##但我以为是要找到第l次出现的位置，
##就应该返回“i+l-1”，如下：
findNum <- function(x,l){
  n <- length(x)
  Num <- NULL
  for(i in 1:(n-l+1)){
    if(all(x[i:(i+l-1)] == 1)) Num <- c(Num,i+l-1)
  }
  return(Num)
}
findNum(y,2)
#[1] 5 6 9

######### 题目2，耗时20分钟 ###############
raw <- read.delim("data/weather.txt",check.names = F, na.strings = ".")

t_difference <- function(x,i){
  max(raw[,i+3],na.rm=T)-min(raw[,i+3],na.rm=T)
}
#输入i为日期，例如查找1日的温度差，为：
t_difference(raw,1)
#[1] 161


######### 题目3，耗时2小时 ##########
library(hflights)
str(hflights)
#hflights[220000,1],大致观察到年份都是2011年，可以不考虑年份的差异
unique(hflights[,7]) #获取一共有几家航空公司
#[1] "AA" "AS" "B6" "CO" "DL" "OO" "UA" "US" "WN" "EV" "F9" "FL" "MQ" "XE" "YV"

hf <- na.omit(hflights)#去掉NA缺失值

qtl <- function(x){quantile(x,prob=0.1)}
#定义qtl为十分位数计算公式

delay <- tapply(hf$ArrDelay,list(hf$Month,hf$UniqueCarrier),qtl) #计算出每家航空公司每个月延误时间的十分位数
#
apply( delay[,1:15], 2,mean )  #计算每家每个月公司延误十分位数的均值

#  AA         AS         B6         CO         DL         EV         F9         FL 
#-18.383333 -18.300000 -20.175000 -15.000000 -17.116667 -19.591667  -7.866667 -15.741667 
#  MQ         OO         UA         US         WN         XE         YV 
#-18.425000 -13.300000 -18.158333 -18.483333 -12.416667 -11.666667         NA 

