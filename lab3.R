rm(list=ls(all=TRUE))
setwd("~/Desktop/3511/")

# 1
wip <- read.table("wip.txt", header=TRUE)
# 1(a)(b) plants 1
wip1 <- wip[wip$plant==1,]$time
summary(wip1)
var(wip1)
sd(wip1)
hist(wip1)
boxplot(wip1)

# 1(a)(b) plants 2
wip2 <- wip[wip$plant==2,]$time
summary(wip2)
var(wip2)
sd(wip2)
hist(wip2)
boxplot(wip2)

# 2
testscores <- read.table("testscores.txt", header=TRUE)
# 2(a)
plot(testscores[testscores$gender=="F",]$A,testscores[testscores$gender=="F",]$B,
     main="2(a)",xlab="testA",ylab="testB",xlim=c(80,130),ylim=c(10,100))
par(new=T) 
plot(testscores[testscores$gender=="M",]$A,testscores[testscores$gender=="M",]$B,
     main="",xlab="",ylab="",xlim=c(80,130),ylim=c(10,100),axes=F,pch=0,col=2)
# 2(b)
plot(testscores[testscores$gender=="F",]$A,testscores[testscores$gender=="F",]$B,
     main="2(b)F",xlab="testA",ylab="testB")
plot(testscores[testscores$gender=="M",]$A,testscores[testscores$gender=="M",]$B,
     main="2(b)M",xlab="testA",ylab="testB")
# 2(c)
cov(testscores$A, testscores$B)

# 3
babiesI <- read.table("babiesI.data", header=TRUE)
# 3(a)
nrow(babiesI[babiesI$smoke==1,])
nrow(babiesI[babiesI$smoke==0,])
# 3(b)
summary(babiesI[babiesI$smoke==1,]$bwt)
summary(babiesI[babiesI$smoke==0,]$bwt)

