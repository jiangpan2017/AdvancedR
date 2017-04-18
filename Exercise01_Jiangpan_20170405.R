library(dplyr)

ds<-data.frame(id=seq(10,80,by=10),
               anest=c("baker","baker",rep("dow",6)),
               start=c("08:00","09:00","09:00","08:00","10:00","12:30","13:30","18:00"),
               end=c("11:00","13:00","15:30","13:30","11:30","13:30","14:30","19:00"))


str(ds)

ds0 <- subset(ds,anest =="dow") #只看dow资产的时间

#将开始时间转化为数值型
start_fac<- ds0$start
start_chr  <-  as.character(start_fac)#先把因子转换成字符
start_split  <-  strsplit(start_chr, split = ':')#然后根据冒号分割字符
start_collapse  <-  sapply(start_split, paste0, collapse = '')#接着合并字符
start0 <-  as.numeric(start_collapse)#最后将字符转化成数字
#简写 start0 <-  as.numeric(sapply(strsplit(as.character(ds$start), split = ':'), paste0, collapse = ''))
start0

#将结束时间转化为数值型
end_fac<- ds0$end
end_chr  <-  as.character(end_fac)#先把因子转换成字符
end_split  <-  strsplit(end_chr, split = ':')#然后根据冒号分割字符
end_collapse  <-  sapply(end_split, paste0, collapse = '')#接着合并字符
end0 <-  as.numeric(end_collapse)#最后将字符转化成数字
#简写 end0 <-  as.numeric(sapply(strsplit(as.character(ds$end), split = ':'), paste0, collapse = ''))
end0

ds00 <- cbind(ds0,start0,end0) #得到一个新的数据框

#将所有开始时间和结束时间放在一个并集中，并按照由小到大排列
time <- union(start0,end0)#并集 
time0 <- sort(time) #由小到大排列

#下面在time0中，每两个相邻元素之间任意去一个数(应保证其为真实存在的时间数字，例如800-900之间可以取859，不能取861)
#time0中有n个元素，就要取到n-1个数字。本例中有10个元素，要取9个数。
#但经过我思考，即便取到假的时间数字，也不会影响后面的判断结果。所以这里不考虑取数对时间限制，861也可行。
#例子：runif(10,min=0,max=1) #产生10个最小值为0，最大值为1的随机数
length(time0) #10
time0[5]
i <- 1:9
ni <- length(i)
z <- vector(mode="numeric",length=9)
for (k in 1:ni){
  
    z[k] <- runif(1,min=time0[k],max=time0[k+1])
    
  }
z

#> z
# [1]  884.6035  952.5240 1027.3189 1185.9519 1266.5466 1408.8760 1434.2751 1594.0930 1828.0669
#然后判断z中每个元素是否在ds00数据框的，对应每一个id的起始和结束时间之间，其实就是判断数值大小。在其中得到1，不在为0.
#z[1],得到一列0-1构成的向量。z[2]得到一列。。。。z[9].
#计算每行向量的长度，最大者为所求。找到最大的向量是哪行，查看其为1的元素的列名，为所求区间。
length(ds00$id)
i <- 1:9 #每一个随机时间 i ni k
j <- 1:6 #每一个id       j nj p
ni <- length(i)
nj <- length(j)
y <- data.frame(NA,ni,nj)
for (k in 1:ni){
  for (p in 1:nj){
    y[k,p] <- if(ds00$end0[p]>z[k]&ds00$start0[p]<z[k])
      
      1  else 0
    
  }
}
y
colnames(y)=c( ds00$id)
#y矩阵中，每一列按照id命名。下面要寻找每行元素和最大值是哪行
s <- rowSums (y, na.rm = FALSE, dims = 1) 
s <- as.vector(s)
d <- which(s==max(s))#[1] 3 5 
#这里不能直接用which.max因为其只能返回第一个最大值，本例中有两个3都是最大值。
#[1] 3 5 意思是，在矩阵y里面，第三行和第五行的值最大，下面只要找到每行中，1的位置就得到结果了，位置列标对应id.
colnames(y) 
which(y[3,]==1)
y[3,]
y[5,]
y <- as.matrix(y)#转换为矩阵后，print可以打印出列名id

print(which(y[3,]==1))#30 40 50 
print(which(y[5,]==1))#30 40 60

