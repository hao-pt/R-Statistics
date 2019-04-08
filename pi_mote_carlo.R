pimotecarlo <- function(n){
  x <- runif(n, min=-1, max=1)
  y <- runif(n, min=-1, max=1)
  m <- sum(x^2 + y^2 <= 1)
  pi <- 4*m/n
  return (pi)
}

tuqui <- function(n){
  x1 <- sample(1:13, n, replace = TRUE)
  x2 <- sample(1:13, n, replace = TRUE)
  x3 <- sample(1:13, n, replace = TRUE)
  x4 <- sample(1:13, n, replace = TRUE)
  
  m <- 0
  for(i in 1:n){
    if (x1[i] == x2[i] && x1[i] == x3[i] && x1[i] == x4[i]){
      m <- m + 1
    }
  }
  pA = m/(n*13)
  return (pA)
}

x = c()
x = (c(1, 2))
x = c(c(3, 4))
x
