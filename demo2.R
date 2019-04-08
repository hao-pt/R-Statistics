#Tinh xac suat
n <- 5; p <- 4/(4+6)
k <- 0:5
pf <- choose(n, k) * p^k * (1-p)^(n-k)
data.frame(k, pf)

#Mo phong tren R
X <- function(){
  hopbi <- c(rep('R',4), rep('B', 6))
  nambi <- sample(hopbi, 5, replace = TRUE)
  return (sum(nambi == 'R'))
}
pfX <- function(N){
  ketqua <- replicate(N, X())
  return(table(ketqua)/N)
}
pfX(50000)

#BT1:
#Cau a:
X <- function(){
  kho <- c(rep('C', 3), rep('K', 7))
  sauvali <- sample(kho, 6, replace = FALSE)
  return (sum(sauvali == 'C'))
}
pfX <- function(N){
  ketqua <- replicate(N, X())
  return (table(ketqua)/N)
}
pfX(500000)
#Cau b: Doi replace = FALSE

#Cau 2:
#a) X la tong 2 mat
X <- function(){
  n <- 1:6
  hai_xs <- sample(n, 2, replace = TRUE)
  return (sum(hai_xs))
}
pfX <- function(N){
  ketqua <- replicate(N, X())
  return (table(ketqua)/N)
}
pfX(500000)
#b) X la hieu mat1 & mat2
X <- function(){
  n <- 1:6
  hai_xs <- sample(n, 2, replace = TRUE)
  return (hai_xs[1] - hai_xs[2])
}
pfX <- function(N){
  ketqua <- replicate(N, X())
  return (table(ketqua)/N)
}
pfX(500000)

#c) X la so lan gieo duoc mat chan
X <- function(){
  n <- 1:6
  hai_xs <- sample(n, 2, replace = TRUE)
  return (sum(hai_xs %% 2 == 0))
}
pfX <- function(N){
  ketqua <- replicate(N, X())
  return (table(ketqua)/N)
}
pfX(500000)

#Cau 3:
Y <- function()
{
  X <- runif(2, min = 0, max = 1)
  return (abs(X[1] - X[2]))
}
histY <- function(N){
  ketqua <- replicate(N, Y())
  hist(ketqua)
}
histY(500000)

