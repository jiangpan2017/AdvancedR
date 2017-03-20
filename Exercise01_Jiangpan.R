install.packages("dplyr")
library(dplyr)

ds<-data.frame(id=seq(10,80,by=10),
               anest=c("baker","baker",rep("dow",6)),
               start=c("08:00","09:00","09:00","08:00","10:00","12:30","13:30","18:00"),
               end=c("11:00","13:00","15:30","13:30","11:30","13:30","14:30","19:00"))

ds
str(ds)

#将开始时间转化为数值型
start_fac<- ds$start
start_chr  <-  as.character(start_fac)#先把因子转换成字符
start_split  <-  strsplit(start_chr, split = ':')#然后根据冒号分割字符
start_collapse  <-  sapply(start_split, paste0, collapse = '')#接着合并字符
start0 <-  as.numeric(start_collapse)#最后将字符转化成数字
#简写 start0 <-  as.numeric(sapply(strsplit(as.character(ds$start), split = ':'), paste0, collapse = ''))
start0

#将结束时间转化为数值型
end_fac<- ds$end
end_chr  <-  as.character(end_fac)#先把因子转换成字符
end_split  <-  strsplit(end_chr, split = ':')#然后根据冒号分割字符
end_collapse  <-  sapply(end_split, paste0, collapse = '')#接着合并字符
end0 <-  as.numeric(end_collapse)#最后将字符转化成数字
#简写 end0 <-  as.numeric(sapply(strsplit(as.character(ds$end), split = ':'), paste0, collapse = ''))
end0

ds0 <- cbind(ds,start0,end0)#构建新数据集，包含数值型的起始和结束时间

levels(ds0$anest)#确认有两种资产，"baker" "dow"  

ds0_baker <- subset(ds0,anest=="baker",select=c(id,start0,end0))#取baker子集
ds0_dow <- subset(ds0,anest=="dow",select=c(id,start0,end0))#取dow子集


#找到重合时间段，放在矩阵Y里面，重合为T，否则为F.边界取不等号
length(ds0_dow$start0)

i <- 1:6
j <- 1:6
ni <- length(i)
nj <- length(j)
y <- matrix(NA,ni,nj)
for (k in 1:ni){
  for (p in 1:nj){
    y[p,k] <- if(ds0_dow$start0[k]>ds0_dow$start0[p]&ds0_dow$start0[k]<ds0_dow$end0[p]|| 
      ds0_dow$start0[p]>ds0_dow$start0[k]&ds0_dow$start0[p]<ds0_dow$end0[k])
      "T"  else "F"
    
  }
}
y
#怎样查看y，并得到结论？ 
#y矩阵是对称矩阵，观察每行和每列，可以画出最多出现的时段，但后面不会用代码实现。 







###寻找y矩阵一般形式
###################################################
#对于一般形式的样本长度,写成findy()函数算不出新值？
#带入新数据实验
ds<-data.frame(id=seq(10,70,by=10),
               anest=c("baker",rep("dow",6)),
               start=c("08:00","09:00","09:00","08:00","10:00","12:30","08:00"),
               end=c("11:00","13:00","15:30","13:30","11:30","13:30","11:30"))

ds


start0 <-  as.numeric(sapply(strsplit(as.character(ds$start), split = ':'), paste0, collapse = ''))
end0 <-  as.numeric(sapply(strsplit(as.character(ds$end), split = ':'), paste0, collapse = ''))
ds0 <- cbind(ds,start0,end0)
ds0_dow <- subset(ds0,anest=="dow",select=c(id,start0,end0))#取子集
ds0_dow
length(ds0_dow$start0)#7

findy <- function(x){
i <- 1:6
j <- 1:6
ni <- length(i)
nj <- length(j)
y <- matrix(NA,ni,nj)

for (k in 1:ni){
    for (p in 1:nj){
      y[p,k] <- if(ds0_dow$start0[k]>ds0_dow$start0[p]&ds0_dow$start0[k]<ds0_dow$end0[p]|| 
                   ds0_dow$start0[p]>ds0_dow$start0[k]&ds0_dow$start0[p]<ds0_dow$end0[k])
        "T"  else "F"
  }
}
}

findy(ds)
y

###################################################
#先写一个已知数据如样本的循环,用between，错误结果，边界取等号了
i <- 1:6
j <- 1:6
ni <- length(i)
nj <- length(j)
for (k in 1:ni){
  for (p in 1:nj){
    y[p,k] <- if(between(ds0_dow$start0[k],ds0_dow$start0[p],ds0_dow$end0[p])== "TRUE" || between(ds0_dow$start0[p],ds0_dow$start0[k],ds0_dow$end0[k])=="TRUE")
      "T"  else "F"
    
  }
}
y 
########################################################


















