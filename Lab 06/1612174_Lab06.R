require(Lock5withR) # Load package
library(Lock5withR)
library(mosaic)
head(SpeedDating)
attach(SpeedDating) # Avoid dollar sign before each varibles name

# 1. Bien dinh tinh
# a) DecisionMale
# - Kiem dinh thong ke cho phan hoi yes co nhieu hon phan hon no
# H0 = 0.5
# H1 > 0.5
# TK can tinh
stat <- function(data){
  return (mean(data == "Yes")) # Ti le
}

randomization <- function(B){
  return (replicate(B, stat(sample(nullsample, n, replace = TRUE))))
}

# Mau du lieu ban dau
sample <- DecisionMale
# Kich thuoc mau, tham so mac dinh, ti le mau, muc y nghia
(n <- length(sample)); (p0 <- 0.5); (p_hat <- stat(sample)); (alpha <- 0.05)
nullsample <- c(rep("Yes", n/2), rep("No", n/2)) # Mau du lieu tuong ung voi H0

# Lay phan phoi cua randomization
rand_dist <- randomization(10000)
# Tinh p-value trong kiem dinh mot phia (one-tailed)
(p_value <- mean(rand_dist >= p_hat))
# Tinh gia tri toi han (critical value) voi muc phan vi 1-alpha
(crit_val <- quantile(rand_dist, 1 - alpha, names = FALSE))
# Kiem tra xem p_value co be hon alpha, neu co thi bac bo H0
p_value < alpha; crit_val < p_hat

#- b) RaceF
# - Kiem dinh TK cho ti le dan toc nua da trang nhieu hon cac dan toc con lai
# H0 = 0.5
# H1 > 0.5

# TK can tinh
stat <- function(data){
  return (mean(data)) # Ti le
}

randomization <- function(B){
  return (replicate(B, stat(sample(nullsample, n, replace = TRUE))))
}

# Mau du lieu ban dau
sample <- RaceF
# Kich thuoc mau, tham so mac dinh, ti le mau, muc y nghia
(n <- length(sample)); (p0 <- 0.5); (p_hat <- mean(sample == "Caucasian")); (alpha <- 0.05)
nullsample <- c(rep(1, n/2), rep(0, n/2)) # Mau du lieu tuong ung voi H0

# Lay phan phoi cua randomization
rand_dist <- randomization(10000)
# Tinh p-value trong kiem dinh mot phia (one-tailed)
(p_value <- mean(rand_dist >= p_hat))
# Tinh gia tri toi han (critical value) voi muc phan vi 1-alpha
(crit_val <- quantile(rand_dist, 1 - alpha, names = FALSE))
# Kiem tra xem p_value co be hon alpha, neu co thi bac bo H0
p_value < alpha; crit_val < p_hat

#2. Bien dinh luong
#a) AttractiveM
# - Kiem dinh thong ke cho muc do quyen ru tb la 6.6
# H0 = 6.6
# H0 != 6.6

# TK can tinh
stat <- function(data){
  return (mean(data, na.rm = TRUE)) # tb mau
}

randomization <- function(B){
  return (replicate(B, stat(sample(nullsample, n, replace = TRUE))))
}

# Mau du lieu ban dau
sample <- AttractiveM
# Kich thuoc mau, tham so mac dinh, trung binh mau, muc y nghia
(n <- length(sample)); (mu0 <- 6.6); (x_bar <- stat(sample)); (alpha <- 0.05)
nullsample <- sample - (x_bar - mu0) # Mau du lieu tuong ung voi H0
# Check lai mean cua nullsample co bang mu0 chua
mean(nullsample, na.rm = TRUE)

# Lay phan phoi cua randomization
rand_dist <- randomization(10000)
# Tinh p-value trong kiem dinh hai phia (two-tailed)
(p_value <- mean(abs(rand_dist - mu0) >= abs(x_bar - mu0)))
# Tinh gia tri toi han (critical value) voi muc phan vi 1-alpha/2
(crit_val <- quantile(rand_dist, 1 - alpha/2, names = FALSE))
# Kiem tra xem p_value co be hon alpha, neu co thi bac bo H0
p_value < alpha; abs(crit_val - mu0) < abs(x_bar - mu0)

# - Kiem dinh thong ke cho muc do quyen ru trung vi la 7.0
# H0 = 7.0
# H0 != 7

# TK can tinh
stat <- function(data){
  return (median(data, na.rm = TRUE)) # tb mau
}

# tham so mac dinh, trung vi mau, muc y nghia
(med0 <- 7.0); (med_hat <- stat(sample)); (alpha <- 0.05)
nullsample <- sample - (med_hat - med0) # Mau du lieu tuong ung voi H0
stat(nullsample)

# Lay phan phoi cua randomization
rand_dist <- randomization(10000)
# Tinh p-value trong kiem dinh hai phia (two-tailed)
(p_value <- mean(abs(rand_dist - med0) >= abs(med_hat - med0)))
# Tinh gia tri toi han (critical value) voi muc phan vi 1-alpha/2
(crit_val <- quantile(rand_dist, 1 - alpha/2, names = FALSE))
# Kiem tra xem p_value co be hon alpha, neu co thi bac bo H0
p_value < alpha; abs(crit_val - med0) < abs(med_hat - med0)

#b) LikeM
# - Kiem dinh thong ke cho muc do thich tb la 6.6
# H0 = 6.6
# H0 != 6.6

# TK can tinh
stat <- function(data){
  return (mean(data, na.rm = TRUE)) # tb mau
}

randomization <- function(B){
  return (replicate(B, stat(sample(nullsample, n, replace = TRUE))))
}

# Mau du lieu ban dau
sample <- LikeM
# Kich thuoc mau, tham so mac dinh, trung binh mau, muc y nghia
(n <- length(sample)); (mu0 <- 6.6); (x_bar <- stat(sample)); (alpha <- 0.05)
nullsample <- sample - (x_bar - mu0) # Mau du lieu tuong ung voi H0
# Check lai mean cua nullsample co bang mu0 chua
mean(nullsample, na.rm = TRUE)

# Lay phan phoi cua randomization
rand_dist <- randomization(10000)
# Tinh p-value trong kiem dinh hai phia (two-tailed)
(p_value <- mean(abs(rand_dist - mu0) >= abs(x_bar - mu0)))
# Tinh gia tri toi han (critical value) voi muc phan vi 1-alpha/2
(crit_val <- quantile(rand_dist, 1 - alpha/2, names = FALSE))
# Kiem tra xem p_value co be hon alpha, neu co thi bac bo H0
p_value < alpha; abs(crit_val - mu0) < abs(x_bar - mu0)

# - Kiem dinh thong ke cho muc do thich trung vi la 7.0
# H0 = 7.0
# H0 != 7

# TK can tinh
stat <- function(data){
  return (median(data, na.rm = TRUE)) # median
}

# tham so mac dinh, trung vi mau, muc y nghia
(med0 <- 7.0); (med_hat <- stat(sample)); (alpha <- 0.05)
nullsample <- sample - (med_hat - med0) # Mau du lieu tuong ung voi H0
stat(nullsample)

# Lay phan phoi cua randomization
rand_dist <- randomization(10000)
# Tinh p-value trong kiem dinh hai phia (two-tailed)
(p_value <- mean(abs(rand_dist - med0) >= abs(med_hat - med0)))
# Tinh gia tri toi han (critical value) voi muc phan vi 1-alpha/2
(crit_val <- quantile(rand_dist, 1 - alpha/2, names = FALSE))
# Kiem tra xem p_value co be hon alpha, neu co thi bac bo H0
p_value < alpha; abs(crit_val - med0) < abs(med_hat - med0)

#c) SincereM
# - Kiem dinh thong ke cho muc do chan thanh tb la 7.8
# H0 = 7.8
# H0 != 7.8

# TK can tinh
stat <- function(data){
  return (mean(data, na.rm = TRUE)) # tb mau
}

randomization <- function(B){
  return (replicate(B, stat(sample(nullsample, n, replace = TRUE))))
}

# Mau du lieu ban dau
sample <- SincereM
# Kich thuoc mau, tham so mac dinh, trung binh mau, muc y nghia
(n <- length(sample)); (mu0 <- 7.8); (x_bar <- stat(sample)); (alpha <- 0.05)
nullsample <- sample - (x_bar - mu0) # Mau du lieu tuong ung voi H0
# Check lai mean cua nullsample co bang mu0 chua
mean(nullsample, na.rm = TRUE)

# Lay phan phoi cua randomization
rand_dist <- randomization(10000)
# Tinh p-value trong kiem dinh hai phia (two-tailed)
(p_value <- mean(abs(rand_dist - mu0) >= abs(x_bar - mu0)))
# Tinh gia tri toi han (critical value) voi muc phan vi 1-alpha/2
(crit_val <- quantile(rand_dist, 1 - alpha/2, names = FALSE))
# Kiem tra xem p_value co be hon alpha, neu co thi bac bo H0
p_value < alpha; abs(crit_val - mu0) < abs(x_bar - mu0)

# - Kiem dinh thong ke cho muc do chan thanh trung vi la 8.0
# H0 = 8.0
# H0 != 8.0

# TK can tinh
stat <- function(data){
  return (median(data, na.rm = TRUE)) # trung vi
}

# tham so mac dinh, trung vi mau, muc y nghia
(med0 <- 8.0); (med_hat <- stat(sample)); (alpha <- 0.05)
nullsample <- sample - (med_hat - med0) # Mau du lieu tuong ung voi H0
stat(nullsample)

# Lay phan phoi cua randomization
rand_dist <- randomization(10000)
# Tinh p-value trong kiem dinh hai phia (two-tailed)
(p_value <- mean(abs(rand_dist - med0) >= abs(med_hat - med0)))
# Tinh gia tri toi han (critical value) voi muc phan vi 1-alpha/2
(crit_val <- quantile(rand_dist, 1 - alpha/2, names = FALSE))
# Kiem tra xem p_value co be hon alpha, neu co thi bac bo H0
p_value < alpha; abs(crit_val - med0) < abs(med_hat - med0)

# 3. 2 bien dinh tinh
# - TK kiem dinh: Ty le nu da trang nhan phan hoi yes nhieu hon phan hoi no
#H0 = 0
#H1 > 0

stat <- function(data){
  return (mean(data$DecisionMale == 'Yes' & data$RaceF == 'Caucasian') - mean(data$DecisionMale == 'No' & data$RaceF == 'Caucasian')) # Ti le khac biet
}

randomization <- function(B){
  return (replicate(B, stat(sample(nullsample, n, replace = TRUE))))
}

# Mau du lieu ban dau
sample <- data.frame(DecisionMale, RaceF)
# tham so mac dinh, ti le mau, muc y nghia
(p0 <- 0.5); (p_hat <- stat(sample)); (alpha <- 0.05)
# Kich thuoc mau
(n <- nrow(sample))
nullsample <- data.frame("DecisionMale" = c(rep("Yes", n/2), rep("No", n/2)), "RaceF" = rep("Caucasian", n)) # Mau du lieu tuong ung voi H0


# Lay phan phoi cua randomization
rand_dist <- randomization(10000)
# Tinh p-value trong kiem dinh mot phia (one-tailed)
(p_value <- mean(rand_dist >= p_hat))
# Tinh gia tri toi han (critical value) voi muc phan vi 1-alpha
(crit_val <- quantile(rand_dist, 1 - alpha, names = FALSE))
# Kiem tra xem p_value co be hon alpha, neu co thi bac bo H0
p_value < alpha; crit_val < p_hat

# - TK kiem dinh: Ty le nu chau nhan phan hoi yes bang phan hoi no
#H0 = 0
#H1 != 0

stat <- function(data){
  return (mean(data$DecisionMale == 'Yes' & data$RaceF == 'Asian') - mean(data$DecisionMale == 'No' & data$RaceF == 'Asian')) # Ti le khac biet
}

randomization <- function(B){
  return (replicate(B, stat(sample(nullsample, n, replace = TRUE))))
}

# tham so mac dinh, ti le mau, muc y nghia
(p0 <- 0); (p_hat <- stat(sample)); (alpha <- 0.05)
# Kich thuoc mau
(n <- nrow(sample))
nullsample <- data.frame("DecisionMale" = c(rep("Yes", n/2), rep("No", n/2)), "RaceF" = rep("Asian", n)) # Mau du lieu tuong ung voi H0


# Lay phan phoi cua randomization
rand_dist <- randomization(10000)
# Tinh p-value trong kiem dinh hai phia (two-tailed)
(p_value <- mean(abs(rand_dist - p0) >= abs(p_hat - p0)))
# Tinh gia tri toi han (critical value) voi muc phan vi 1-alpha/2
(crit_val <- quantile(rand_dist, 1 - alpha/2, names = FALSE))
# Kiem tra xem p_value co be hon alpha, neu co thi bac bo H0
p_value < alpha; crit_val < p_hat

# 4. Bien dinh tinh va bien dinh luong
# - Kiem dinh thong ke ki vong AttractiveM|"Yes" se cao hon ki vong AttractiveM|"No"
# H0 = 0
# H0 > 0

# TK can tinh
diffmean <- function(data1, data2) {
  # Lay index
  index <- 1:(n1+n2) %in% sample(1:(n1+n2), n1)
  # random sample cua yes
  rand_sample1 <- c(data1, data2)[index]
  # random sample cua no
  rand_sample2 <- c(data1, data2)[!index]
  return(mean(rand_sample1, na.rm = TRUE)-mean(rand_sample2, na.rm = TRUE))
}

randomization <- function(B){
  return (replicate(B, diffmean(sample1, sample2)))
}

# Sample
sample1 <- subset(SpeedDating, DecisionMale=='Yes', select=c(AttractiveM))[[1]]; 
sample2 <- subset(SpeedDating, DecisionMale=='No', select=c(AttractiveM))[[1]];
# Kich thuoc mau
n1 <- length(sample1); n2 <- length(sample2); n1; n2

# tb mau 1, tb mau 2 va diff mean cua mau 1 va 2
(x_1 <- mean(sample1, na.rm = TRUE)); (x_2 <- mean(sample2, na.rm = TRUE)); (diff_x <- x_1 - x_2)
# tham so mac dinh, muc y nghia
(diff_mu0 <- 0); (alpha <- 0.05)

# Lay phan phoi cua randomization
rand_dist <- randomization(10000); hist(rand_dist)
# Tinh p-value trong kiem dinh mot phia (one-tailed)
(p_value <- mean(rand_dist >= diff_x))
# Tinh gia tri toi han (critical value) voi muc phan vi 1-alpha
(crit_val <- quantile(rand_dist, 1 - alpha, names = FALSE))
# Kiem tra xem p_value co be hon alpha, neu co thi bac bo H0
p_value < alpha; crit_val < diff_x

# - Kiem dinh thong ke median AttractiveM|"Yes" se cao hon median AttractiveM|"No"
# H0 = 0
# H0 > 0

# TK can tinh
diffmed <- function(data1, data2) {
  # Lay index
  index <- 1:(n1+n2) %in% sample(1:(n1+n2), n1)
  # random sample cua yes
  rand_sample1 <- c(data1, data2)[index];
  # random sample cua no
  rand_sample2 <- c(data1, data2)[!index];
  return(median(rand_sample1, na.rm = TRUE)-median(rand_sample2, na.rm = TRUE))
}

randomization <- function(B){
  return (replicate(B, diffmed(sample1, sample2)))
}

# Sample
sample1 <- subset(SpeedDating, DecisionMale=='Yes', select=c(AttractiveM))[[1]]; 
sample2 <- subset(SpeedDating, DecisionMale=='No', select=c(AttractiveM))[[1]];
# Kich thuoc mau
n1 <- length(sample1); n2 <- length(sample2); n1; n2

# med mau 1, med mau 2 va diff med cua mau 1 va 2
(med_1 <- median(sample1, na.rm = TRUE)); (med_2 <- median(sample2, na.rm = TRUE)); (diff_med <- med_1 - med_2)
# tham so mac dinh, muc y nghia
(diff_med0 <- 0); (alpha <- 0.05)

# Lay phan phoi cua randomization
rand_dist <- randomization(10000); hist(rand_dist)
# Tinh p-value trong kiem dinh mot phia (one-tailed)
(p_value <- mean(rand_dist >= diff_med))
# Tinh gia tri toi han (critical value) voi muc phan vi 1-alpha
(crit_val <- quantile(rand_dist, 1 - alpha, names = FALSE))
# Kiem tra xem p_value co be hon alpha, neu co thi bac bo H0
p_value < alpha; crit_val < diff_med

# 5. Hai bien dinh luong
# - Kiem dinh thong ke he so tuong quan giua AttractiveM va LikeM > 0.5
# H0 = 0.5
# H0 > 0.5

library("ecodist") # Generate data.frame with specific correlation

#TK can tinh
stat <- function(data){
  #Tinh correlation
  return (cor(data$AttractiveM, data$LikeM, use = "complete.obs")) # Avoid missing values
}

randomization <- function(B){
  return (replicate(B, stat(sample(nullsample, n, replace = TRUE))))
}

# Sample
sample <- data.frame(AttractiveM, LikeM)
# Kich thuoc mau
(n <- nrow(sample))

# tham so mac dinh, correlation tren mau, muc y nghia
(cor0 <- 0.5); (cor_hat <- stat(sample)); (alpha <- 0.05)
nullsample <- corgen(len = n, r = 0.5, epsilon = 0.01) # Mau tuong thich voi H0
#rename column
names(nullsample)[1] = "AttractiveM"
names(nullsample)[2] = "LikeM"
stat(nullsample)

# Lay phan phoi cua randomization
rand_dist <- randomization(10000)
# Tinh p-value trong kiem dinh mot phia (one-tailed)
(p_value <- mean(rand_dist >= cor_hat))
# Tinh gia tri toi han (critical value) voi muc phan vi 1-alpha
(crit_val <- quantile(rand_dist, 1 - alpha, names = FALSE))
# Kiem tra xem p_value co be hon alpha, neu co thi bac bo H0
p_value < alpha; crit_val < cor_hat

# - Dung khoang tin cay
# Bootstrap
bootstrap <- function(B){
  return (replicate(B, stat(sample(sample, nrow(data), replace = TRUE))))
}

boots_dist <- bootstrap(10000) # Tim phan phoi cua bootstrap
(se <- sd(boots_dist, na.rm = TRUE)) # Tinh standard deviation (missing value se bi bo qua)
(conf_boots <- quantile(boots_dist, c(alpha, 1), names = FALSE)) # Tim khoang tin cay cho correlation
# Neu cor0 nam ngoai khoang tin cay thi ta se bac bo H0
!(conf_boots[1] <= cor0 && cor0 <= conf_boots[2])

# - Kiem dinh thong ke he so slope cua regression giua AttractiveM va LikeM > 0.5
# H0 = 0.5
# H0 > 0.5

# - Dung khoang tin cay
# Cac TK can tinh
stat <- function(data){
  #Tim best-fit line
  lmInfo <- lm(data$LikeM~data$AttractiveM)
  return (lmInfo$coefficients[2])
}
# Bootstrap
bootstrap <- function(B){
  return (replicate(B, stat(sample(sample, nrow(data), replace = TRUE))))
}

# tham so mac dinh, correlation tren mau, muc y nghia
(slope0 <- 0.5); (slope_hat <- stat(sample)); (alpha <- 0.05)

boots_dist <- bootstrap(10000) # Tim phan phoi cua bootstrap
(se <- sd(boots_dist, na.rm = TRUE)) # Tinh standard deviation (missing value se bi bo qua)
(conf_boots <- quantile(boots_dist, c(alpha, 1), names = FALSE)) # Tim khoang tin cay cho correlation
# Neu cor0 nam ngoai khoang tin cay thi ta se bac bo H0
!(conf_boots[1] <= slope0 && slope0 <= conf_boots[2])


