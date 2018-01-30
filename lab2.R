rm(list=ls(all=TRUE))
setwd("~/Desktop/3511/")

# 1. read lab2fixed.txt
lab2 <- read.table("lab2fixed.txt")
colnames(lab2) <- c("rawInput")

# parse the raw input
lab2$id <- as.numeric(substr(lab2$rawInput, 1, 3))
lab2$gender <- substr(lab2$rawInput, 4, 4)
lab2$height <- as.numeric(substr(lab2$rawInput, 5, 7))
lab2$weight <- as.numeric(substr(lab2$rawInput, 8, 9))
lab2$siblings <- as.numeric(substr(lab2$rawInput, 10, 10))

head(lab2)

# 2. extract all males
lab2m <- lab2[lab2$gender=="M",]

# 3. import lab2test and merge
lab2test <- read.table("lab2test.txt", header=TRUE)
lab2merge <- merge(lab2, lab2test, by=c("id"))

# 3. identify the test score of those heights greater than 182
lab2merge[lab2merge$height>182,]

# 4. remove subject 211
lab2merge[lab2merge$id==211,]
lab2remo <- lab2merge[lab2merge$id!=211,]

# 5. change weight of subject 211 into 80
lab2merge[lab2merge$id==211,]$weight <- 80

# 6. find the second tallest female
lab2f <- lab2merge[lab2merge$gender=="F",]
lab2f <- lab2f[order(-lab2f$height),]
lab2f[2,]

# 7. compute b
X <- matrix(c(1,1,1,1,1,3,5,7), nrow=4, ncol=2)
Y <- matrix(c(4,6,13,20), nrow=4, ncol=1)
b <- solve(t(X)%*%X) %*% t(X) %*% Y

# 8(i)
x1 <- 0
x2 <- 1
for (i in 1:28){
  t <- 3*x2 -2*x1
  x1 <- x2
  x2 <- t
}
cat("30th term is: ", x2)
# 8(ii)
x1 <- 0
x2 <- 1
sum <- x1 + x2
for (i in 1:10){
  t <- 3*x2 -2*x1
  sum <- sum + t
  x1 <- x2
  x2 <- t
}
cat("sum of the first 12 is: ", sum)

# 9
m <- function(list, n){
  avg <- sum(list)/length(list)
  return(sum((list-avg)^n)/length(list))
}
list <- lab2$height
M1 <- sum(list)/length(list)
M2 <- m(list, 2)
M3 <- m(list, 3)
M4 <- m(list, 4)
lab2.Q9 <- data.frame(c(M1, M2, M3, M4))

# 10. find the root
f <- function(x){-2*x^2-5*x+7}
xlower <- -4
xupper <- 0
while (TRUE){
  t <- f((xlower+xupper)/2)
  if (abs(t-0)<0.001){
    cat("the root is: ", (xlower+xupper)/2)
    break
  }
  if (t>0){
    xupper <- (xlower+xupper)/2
  } else {
    xlower <- (xlower+xupper)/2
  }
}

