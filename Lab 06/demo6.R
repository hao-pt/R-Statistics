# Kiem dinh ti le
# C1: randomization
# Kich thuoc mau, tham so mac dinh, ti le mau, muc y nghia
n <- 100; p0 <- 0.5; p_hat <- 0.6; anpha <- 0.05
nullsample <- c(rep(1, 50), rep(0, 50)) #m???u d??? li???u tuong ???ng v???i H0

stat <- function(data) #th???ng kê c???n tính
{
  return(sum(data)/length(data)) #t??? l??? m???u
}

randomization <- function(B)
{
  return(replicate(B, stat(sample(nullsample, n, replace = TRUE))))
}
# Lay phan phoi cua randomization
rand_dist <- randomization(10000)
# Tinh p-value trong kiem dinh mot phia (one-tailed)
(p_value <- sum(rand_dist >= p_hat)/length(rand_dist))
# Tinh gia tri toi han (critical value) voi muc phan vi 1-alpha
(crit_val <- quantile(rand_dist, 1 - anpha))
# Kiem tra xem p_value co be hon alpha va crit_val co be hon ti le mau p_hat
p_value < anpha; crit_val < p_hat

prop.test(60, 100, conf.level = 1 - anpha, alternative = 'less')

# C2: Kiem dinh bang xay dung khoang tin cay cho p
sample1 <- c(rep(1, 60), rep(0, 40))
boot_dist <- replicate(10000, stat(sample(sample1, n, replace=TRUE)))
# Xay dung khoang tin tren 1 - alpha cho p la [a, 1]
(confint <- quantile(boot_dist, c(anpha, 1), names = FALSE))

# Kiem dinh cho ki vong
# C1: Radomization
sample1 <- c(1.56, 1.47, 1.59, 1.65, 1.62, 1.78, 1.69, 1.49, 1.92, 1.55,
             1.65, 1.52, 1.65, 1.60, 1.71, 1.48, 1.69, 1.65, 1.59, 1.74,
             1.70, 1.61, 1.58, 1.65, 1.75, 1.65, 1.46, 1.53, 1.59, 1.62,
             1.60, 1.55, 1.57, 1.46, 1.57, 1.63, 1.46, 1.68, 1.53, 1.48)

# kich thuoc mau, Tham so mac dinh, muc y nghia
n <- length(sample1); mu0 <- 1.6; anpha <- 0.05
(x_bar <- mean(sample1)) # Trung binh mau
nullsample <- sample1 - (x_bar - mu0) #m???u d??? li???u tuong ???ng v???i H0
mean(nullsample) # Check lai mean cua nullsample co bang mu0 chua

stat <- function(data) #th???ng kê c???n tính
{
  return(mean(data)) #trung bình m???u
}

randomization <- function(B)
{
  return(replicate(B, stat(sample(nullsample, n, replace = TRUE))))
}

# Lay phan phoi cua randomization
rand_dist <- randomization(10000)
# Tinh p-value trong kiem dinh hai phia (two-tailed)
(p_value <- sum(abs(rand_dist-mu0) >= abs(x_bar-mu0))/length(rand_dist))
# Tinh gia tri toi han (critical value) bang phan vi muc 1-alpha/2
(crit_val <- quantile(rand_dist, 1 - anpha/2))
# Kiem tra xem p_value co be hon alpha; tuong cho critical value
p_value < anpha; abs(crit_val - mu0) < abs(x_bar - mu0)

# C2: Xay dung khoang tin cay cho ki vong
boot_dist <- replicate(10000, stat(sample(sample1, n, replace = TRUE)))
# Xay dung khoang tin cay voi muc phan vi alpha/2 va 1-alpha/2 cho mean la [a, b]
(confint <- quantile(boot_dist, c(anpha/2, 1-anpha/2), names = FALSE))
# Kiem tra mu0 co nam ben ngoai confident interval khong?
!(confint[1] <= mu0 && mu0 <= confint[2])

# Kiem dinh median
stat <- function(data) #th???ng kê c???n tính
{
  return(median(data)) #trung v??? m???u
}

# Tham so mac dinh
med0 <- 0.5
# Tinh trung vi mau
(med_hat <- stat(sample1))
# Lay phan phoi cua randomization
rand_dist <- randomization(10000)
# Tinh p-value trong kiem dinh hai phia (two-tailed)
(p_value <- sum(abs(rand_dist-med0) >= abs(med_hat-med0))/length(rand_dist))
# Tinh gia tri toi han (critical value) bang phan vi muc 1-alpha/2
(crit_val <- quantile(rand_dist, 1 - anpha/2))
# Kiem tra xem p_value co be hon alpha; tuong cho critical value
p_value < anpha; abs(crit_val - med0) < abs(med_hat - med0)

require(mosaicData); 
set.seed(500); anpha <- 0.05
boxplot(wage ~ sex, data=CPS85)
sample1 <- subset(CPS85, sex=='M', select=c(wage))[[1]]; 
sample2 <- subset(CPS85, sex=='F', select=c(wage))[[1]]
n1 <- length(sample1); n2 <- length(sample2); n1; n2

x_1 <- mean(sample1); x_2 <- mean(sample2); x_1 - x_2
diffmean <- function() {
  index <- 1:(n1+n2) %in% sample(1:(n1+n2), n1); print(sum(index))
  rand_sample1 <- c(sample1, sample2)[index];
  rand_sample2 <- c(sample1, sample2)[!index];
  return(mean(rand_sample1)-mean(rand_sample2))
}
diffmean()
rand_dist <- replicate(10000, diffmean()); hist(rand_dist)
