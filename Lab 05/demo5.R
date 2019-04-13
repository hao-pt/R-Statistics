# 1. Uong luong khoang tin cay cho ti le
# Xay dung khoang tin cay bang cong thuc ly thuyet
n <- 100; p_hat <- 0.6; alpha <- 1 - 0.95
z <- qnorm(1 - alpha/2); z
se <- sqrt(p_hat*(1-p_hat)/n); se
# Khoang tin cay
conf_int <- p_hat + c(-z*se, z*se); conf_int

# Hoac dung prop.test (Phan phoi chi binh phuong)
prop.test(60, 100, conf.level = 1 - alpha)


# xay dung khoang tin cay bang bootstrap
n <- 100
data <- c(rep(1, 60), rep(0, 40)) #Sample data: 60 nu, 40 nam

# Tinh ti le mau
stat <- function(data){
  return (sum(data)/length(data))
}

# Lay mau co hoan lai tren chinh sample data
# Thuc hien bootstrap B lan
bootstrap <- function(B){
  return (replicate(B, stat(sample(data, n, replace = TRUE))))
}

alpha <- 1 - 0.95
boots_dist <- bootstrap(10000) # phan phoi cua bootstrap
se <- sd(boots_dist); se
conf_int <- quantile(boots_dist, c(alpha/2, 1-alpha/2)); conf_int

# 2. Uoc luong khoang tin cay cho ky vong:
data <- c(1.56, 1.47, 1.59, 1.65, 1.62, 1.78, 1.69, 1.49, 1.92, 1.55,
          1.65, 1.52, 1.65, 1.60, 1.71, 1.48, 1.69, 1.65, 1.59, 1.74,
          1.70, 1.61, 1.58, 1.65, 1.75, 1.65, 1.46, 1.53, 1.59, 1.62,
          1.60, 1.55, 1.57, 1.46, 1.57, 1.63, 1.46, 1.68, 1.53, 1.48)

n <- length(data); x_bar <- mean(data); alpha = 1 - 0.95
se <- sd(data)/sqrt(n); se
z <- qnorm(1 - alpha/2); z
zconf_int <- x_bar + c(-z*se, z*se); zconf_int
t <- qt(1 - alpha/2, df = n - 1); t
tconf_int <- x_bar + c(-t*se, t*se); tconf_int

# Hoacj dung ham t.test
t.test(data, conf.level = 1 - alpha)

# Xay dung khoang tin cay bang bootstap
stat <- function(data){ # TK can tinh
  return (mean(data))
}
boostrap <- function(B){
  return (replicate(B, stat(data, n, replace = TRUE)))
}

alpha <- 1 - 0.95
boots_dist <- bootstrap(10000)
se_boots <- sd(boots_dist); se_boots
conf_boots <- quantile(boots_dist, c(alpha/2, 1 - alpha/2)); conf_boots

# Xay dung khoang tin cay cho trung vi
boxplot(data)
stat <- function(data){ # TK can tinh
  return (median(data))
}
