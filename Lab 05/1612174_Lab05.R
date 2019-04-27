require(Lock5withR) # Load package
library(Lock5withR)
head(SpeedDating)
attach(SpeedDating) # Avoid dollar sign before each varibles name

# 1. Bien dinh tinh
# a) DecisionMale
# - Xay dung khoang tin cay cho ti le nam phan hoi yes

# Cac TK can tinh
stat <- function(data){
  return (sum(data == 'Yes')/length(data)) # Ti le
}

# Bootstrap
bootstrap <- function(B){
  return (replicate(B, stat(sample(data, length(data), replace = TRUE))))
}

# Lay du lieu
data <- DecisionMale

(alpha <- 1 - 0.95)
boots_dist <- bootstrap(10000) # Tim phan phoi cua bootstrap
(se <- sd(boots_dist)) # Tinh standard deviation
(conf_boots <- quantile(boots_dist, c(alpha/2, 1 - alpha/2))) # Tim khoang tin cay cho p

# - Xay dung khoang tin cay cho ki vong cua nam phan hoi yes
# Cac TK can tinh
stat <- function(data){
  return (mean(data == 'Yes'))
}

(alpha <- 1 - 0.95)
boots_dist <- bootstrap(10000) # Tim phan phoi cua bootstrap
(se <- sd(boots_dist)) # Tinh standard deviation
(conf_boots <- quantile(boots_dist, c(alpha/2, 1 - alpha/2))) # Tim khoang tin cay cho p

#- b) RaceF
# - Xay dung khoang tin cay cho ti le dan toc nu la nguoi da trang

# Cac TK can tinh
stat <- function(data){
  return (sum(data == 'Caucasian')/length(data)) # Ti le
}

# Bootstrap
bootstrap <- function(B){
  return (replicate(B, stat(sample(data, length(data), replace = TRUE))))
}

# Lay du lieu
data <- RaceF

(alpha <- 1 - 0.95)
boots_dist <- bootstrap(10000) # Tim phan phoi cua bootstrap
(se <- sd(boots_dist)) # Tinh standard deviation
(conf_boots <- quantile(boots_dist, c(alpha/2, 1 - alpha/2))) # Tim khoang tin cay cho p



#2. Bien dinh luong
#a) AttractiveM
# - Xay dung khoang tin cay cho ky vong cua AttractiveM

# Cac TK can tinh
stat <- function(data){
  return (mean(data)) # Mean
}

# Bootstrap
bootstrap <- function(B){
  return (replicate(B, stat(sample(data, length(data), replace = TRUE))))
}

# Lay du lieu
data <- AttractiveM

(alpha <- 1 - 0.95)
boots_dist <- bootstrap(10000) # Tim phan phoi cua bootstrap
(se <- sd(boots_dist, na.rm = TRUE)) # Tinh standard deviation (missing value se bi bo qua)
(conf_boots <- quantile(boots_dist, c(alpha/2, 1 - alpha/2), na.rm = TRUE)) # Tim khoang tin cay cho p

# - Xay dung khoang tin cay cho trung vi cua AttractiveM

# Cac TK can tinh
stat <- function(data){
  return (median(data)) # Median
}

(alpha <- 1 - 0.95)
boots_dist <- bootstrap(10000) # Tim phan phoi cua bootstrap
(se <- sd(boots_dist, na.rm = TRUE)) # Tinh standard deviation (missing value se bi bo qua)
(conf_boots <- quantile(boots_dist, c(alpha/2, 1 - alpha/2), na.rm = TRUE)) # Tim khoang tin cay cho p

# 3. 2 bien dinh tinh:
# Xay dung khoang tin cho ti le nguoi da trang nhan phan hoi yes
# Cac TK can tinh
stat <- function(data){
  # Tinh le nguoi da trang nhan phan hoi Yes
  return (sum(data$DecisionMale == 'Yes' & data$RaceF == 'Caucasian') / sum(data$DecisionMale == 'Yes'))
}

# Bootstrap
bootstrap <- function(B){
  return (replicate(B, stat(sample(data, length(data), replace = TRUE))))
}

# Concatenate 2 column
data <- data.frame(DecisionMale, RaceF)

(alpha <- 1 - 0.95)
boots_dist <- bootstrap(10000) # Tim phan phoi cua bootstrap
(se <- sd(boots_dist, na.rm = TRUE)) # Tinh standard deviation (missing value se bi bo qua)
(conf_boots <- quantile(boots_dist, c(alpha/2, 1 - alpha/2), na.rm = TRUE)) # Tim khoang tin cay cho p

# - Xay dung khoang tin cay cho ti le nguoi chau a nhan phan hoi Yes
# Cac TK can tinh
stat <- function(data){
  # Tinh le nguoi da trang nhan phan hoi Yes
  return (sum(data$DecisionMale == 'Yes' & data$RaceF == 'Asian') / sum(data$DecisionMale == 'Yes'))
}

(alpha <- 1 - 0.95)
boots_dist <- bootstrap(10000) # Tim phan phoi cua bootstrap
(se <- sd(boots_dist, na.rm = TRUE)) # Tinh standard deviation (missing value se bi bo qua)
(conf_boots <- quantile(boots_dist, c(alpha/2, 1 - alpha/2), na.rm = TRUE)) # Tim khoang tin cay cho p

# - Xay dung khoang tin cay cho ti le nguoi da trang a nhan phan hoi No
stat <- function(data){
  # Tinh le nguoi da trang nhan phan hoi Yes
  return (sum(data$DecisionMale == 'No' & data$RaceF == 'Caucasian') / sum(data$DecisionMale == 'No'))
}

(alpha <- 1 - 0.95)
boots_dist <- bootstrap(10000) # Tim phan phoi cua bootstrap
(se <- sd(boots_dist, na.rm = TRUE)) # Tinh standard deviation (missing value se bi bo qua)
(conf_boots <- quantile(boots_dist, c(alpha/2, 1 - alpha/2), na.rm = TRUE)) # Tim khoang tin cay cho p

# 4. 2 bien dinh luong

detach(SpeedDating)