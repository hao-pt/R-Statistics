#Cau 1:
#a) Bang phan phoi xac suat cua X
#Voi X la so vien dan trung bia
X <- function(){
  khanang <- c(1, 0)
  thosan1 <- sample(khanang, 1, prob = c(0.2, 0.8))
  thosan2 <- sample(khanang, 1, prob = c(0.4, 0.6))
  thosan3 <- sample(khanang, 1, prob = c(0.6, 0.4))
  return(thosan1 + thosan2 + thosan3)
}
pfX <- function(N){
  ketqua <- replicate(N, X())
  return (table(ketqua)/N)
}
pfX(50000)
#b) So vien dan trung bia khong qua 1 
X_khong_qua_1 <- function(){
  khanang <- c(1, 0)
  thosan1 <- sample(khanang, 1, prob = c(0.2, 0.8))
  thosan2 <- sample(khanang, 1, prob = c(0.4, 0.6))
  thosan3 <- sample(khanang, 1, prob = c(0.6, 0.4))
  return(thosan1 + thosan2 + thosan3 <= 1)
}
tansuat <- function(N){
  ketqua <- replicate(N, X_khong_qua_1())
  return (sum(ketqua)/N)
}
tansuat(50000)

#Cau 2:
#a)
#Ham chon canh x trong [0, 1]
X <- function(){
  x <- runif(1, min = 0, max = 1)
  return (x^2)
}
#Ve histogram
histY <- function(N){
  ketqua <- replicate(N, X())
  hist(ketqua)
}
histY(50000)
#b) Xac suat de dien tich hinh vuong khong qua 0.5 m2
X <- function(){
  x <- runif(1, min = 0, max = 1)
  return (x^2 <= 0.5)
}
tansuat <- function(N){
  ketqua <- replicate(N, X())
  return (sum(ketqua)/N)
}
tansuat(500000)
