# Bai 1: 10 vali (3 vali banh xe xoay, con lai khong co banh xe xoay)
# Lay 5 vali tu kho
# X: so vali co banh xe xoay lay tu kho
# Tinh ki vong cua X trong trong 2 TH:
#  a) Lay co hoan lai
#  b) Lay khong hoan lai

XA <- function(){
  vali <- c(rep(1, 3), rep(0, 7))
  namVali <- sample(vali, 5, replace = TRUE)
  return (sum(namVali == 1))
}

meanX <- function(N){
  kq <- replicate(N, XA())
  return (mean(kq))
}

meanX(500000)

# > meanX(500000)
# [1] 1.50108

# b

XB <- function(){
  vali <- c(rep(1, 3), rep(0, 7))
  namVali <- sample(vali, 5, replace = FALSE)
  return (sum(namVali == 1))
}

meanXB <- function(N){
  kq <- replicate(N, XB())
  return (mean(kq))
}

meanXB(50000)
#> meanXB(50000)
#[1] 1.50032


# Bai 2: Gieo 2 xuc xac cung kich thuoc, cung chat. Tinh ki vong cua X trong cac TH
# a) X la tong 2 mat
Xa <- function(){
  n <- c(1:6)
  xs1 <- sample(n, 1, replace = FALSE)
  xs2 <- sample(n, 1, replace = FALSE)
  
  return (sum(xs1 + xs2))
}

meanXa <- function(N){
  kq <- replicate(N, Xa())
  return (mean(kq))
}

meanXa(50000)
# > meanXa(50000)
# [1] 7.0007

# b) X la hieu cua mat1 - mat2
Xb <- function(){
  n <- c(1:6)
  xs1 <- sample(n, 1, replace = FALSE)
  xs2 <- sample(n, 1, replace = FALSE)
  
  return (sum(xs1 - xs2))
}

meanXb <- function(N){
  kq <- replicate(N, Xb())
  return (mean(kq))
}

meanXb(50000)
#> meanXb(50000)
#[1] 0.0082

# c) X la so lan gieo duoc mat chan
Xc <- function(){
  n <- c(1:6)
  xs1 <- sample(n, 1, replace = FALSE)
  xs2 <- sample(n, 1, replace = FALSE)
  
  return (sum(xs1 %% 2 == 0 && xs2 %% 2 == 0))
}

meanXc <- function(N){
  kq <- replicate(N, Xc())
  return (mean(kq))
}

meanXc(50000)
# > meanXc(50000)
# [1] 0.25144

# Bai 3: Tren thanh go dai 1m, chon ngau nhien 2 diem roi cat tai 2 diem do, 
# giu lai thanh o giua
# Tinh ki vong cua chieu dai thanh duoc giu lai

Y <- function(){
  x1 <- runif(1, min = 0, max = 1)
  x2 <- runif(1, min = 0, max = 1)
  return (abs(x1 - x2))
}

meanY <- function(N){
  kq <- replicate(N, Y())
  return (mean(kq))
}

meanY(50000)

# > meanY(50000)
# [1] 0.332967
