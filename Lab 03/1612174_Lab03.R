# Bai 1
X <- function(){
  khanang <- c(1, 0)
  thosan1 <- sample(khanang, 1, prob = c(0.2, 0.8))
  thosan2 <- sample(khanang, 1, prob = c(0.4, 0.6))
  thosan3 <- sample(khanang, 1, prob = c(0.6, 0.4))
  
  return (thosan1 + thosan2 + thosan3)
}

meanX <- function(N){
  kq <- replicate(N, X())
  return (mean(kq))
}

meanX(50000)
#> meanX(50000)
#[1] 1.19892

#Bai 2
X <- function(){
  khanag <- c(1, 0) #1: Head, 0: Tail
  count <- 0
  while(TRUE){
    tung_dong_xu <- sample(khanag, 1, prob = c(0.4,0.6))
    count <- count + 1
    if(tung_dong_xu == 1){
      break
    }
  }
  return (count)
}

meanX <- function(N){
  kq <- replicate(N, X())
  return (mean(kq))
}

meanX(50000)
#> meanX(50000)
#[1] 2.5062

# Cau 3
Y <- function(){
  l <- runif(1, min = 0, max = 1)
  return (l^2)
}

meanY <- function(N){
  kq <- replicate(N, Y())
  return (mean(kq))
}

meanY(50000)
#> meanY(50000)
#[1] 0.333004

