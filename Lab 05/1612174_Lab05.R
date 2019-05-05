require(Lock5withR) # Load package
library(Lock5withR)
library(mosaic)
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

# - Xay dung khoang tin cay cho ti le cua nam phan hoi no
# Cac TK can tinh
stat <- function(data){
  return (mean(data == 'No'))
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

# - Xay dung khoang tin cay cho ti le dan toc nu la nguoi chau A

# Cac TK can tinh
stat <- function(data){
  return (sum(data == 'Asian')/length(data)) # Ti le
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
(conf_boots <- quantile(boots_dist, c(alpha/2, 1 - alpha/2), na.rm = TRUE)) # Tim khoang tin cay cho mean

# - Xay dung khoang tin cay cho trung vi cua AttractiveM
boxplot(data)
# Cac TK can tinh
stat <- function(data){
  return (median(data)) # Median
}

(alpha <- 1 - 0.95)
boots_dist <- bootstrap(10000) # Tim phan phoi cua bootstrap
(se <- sd(boots_dist, na.rm = TRUE)) # Tinh standard deviation (missing value se bi bo qua)
(conf_boots <- quantile(boots_dist, c(alpha/2, 1 - alpha/2), na.rm = TRUE)) # Tim khoang tin cay cho median

#b) LikeM:
# - Xay dung khoang tin cay cho ky vong cua LikeM

# Cac TK can tinh
stat <- function(data){
  return (mean(data)) # Mean
}

# Bootstrap
bootstrap <- function(B){
  return (replicate(B, stat(sample(data, length(data), replace = TRUE))))
}

# Lay du lieu
data <- LikeM

(alpha <- 1 - 0.95)
boots_dist <- bootstrap(10000) # Tim phan phoi cua bootstrap
(se <- sd(boots_dist, na.rm = TRUE)) # Tinh standard deviation (missing value se bi bo qua)
(conf_boots <- quantile(boots_dist, c(alpha/2, 1 - alpha/2), na.rm = TRUE)) # Tim khoang tin cay cho mean

# - Xay dung khoang tin cay cho trung vi cua LikeM
# Ve boxplot de phat hien outliers
boxplot(data)
# Cac TK can tinh
stat <- function(data){
  return (median(data)) # Median
}

(alpha <- 1 - 0.95)
boots_dist <- bootstrap(10000) # Tim phan phoi cua bootstrap
(se <- sd(boots_dist, na.rm = TRUE)) # Tinh standard deviation (missing value se bi bo qua)
(conf_boots <- quantile(boots_dist, c(alpha/2, 1 - alpha/2), na.rm = TRUE)) # Tim khoang tin cay cho median

#b) SincereM:
# - Xay dung khoang tin cay cho ky vong cua SincereM

# Cac TK can tinh
stat <- function(data){
  return (mean(data)) # Mean
}

# Bootstrap
bootstrap <- function(B){
  return (replicate(B, stat(sample(data, length(data), replace = TRUE))))
}

# Lay du lieu
data <- SincereM

(alpha <- 1 - 0.95)
boots_dist <- bootstrap(10000) # Tim phan phoi cua bootstrap
(se <- sd(boots_dist, na.rm = TRUE)) # Tinh standard deviation (missing value se bi bo qua)
(conf_boots <- quantile(boots_dist, c(alpha/2, 1 - alpha/2), na.rm = TRUE)) # Tim khoang tin cay cho mean

# - Xay dung khoang tin cay cho trung vi cua SincereM
# Ve boxplot de phat hien outliers
boxplot(data)
# Cac TK can tinh
stat <- function(data){
  return (median(data)) # Median
}

(alpha <- 1 - 0.95)
boots_dist <- bootstrap(10000) # Tim phan phoi cua bootstrap
(se <- sd(boots_dist, na.rm = TRUE)) # Tinh standard deviation (missing value se bi bo qua)
(conf_boots <- quantile(boots_dist, c(alpha/2, 1 - alpha/2), na.rm = TRUE)) # Tim khoang tin cay cho median

# 3. 2 bien dinh tinh:
# 2 bien dinh tinh
tab1 = table(DecisionMale, RaceF)
# Them margin
addmargins(tab1)

# 2-way table
# Ti le chung toc nu (Asian, Black, ...) nhan phan hoi
prop.table(tab1, margin = 1)

barplot(tab1, legend = TRUE)
# Xay dung khoang tin cho ti le khac biet giua nu da trang nhan phan hoi yes/no
# va gom nhom cac cot gia tri con lai
# Cac TK can tinh
stat <- function(data){s
  return (sum(data$DecisionMale == 'Yes' & data$RaceF == 'Caucasian')/ sum(data$DecisionMale == 'Yes') - sum(data$DecisionMale == 'No' & data$RaceF == 'Caucasian')/ sum(data$DecisionMale == 'No'))
}

# Bootstrap
bootstrap <- function(B){
  return (replicate(B, stat(sample(data, nrow(data), replace = TRUE))))
}

# Concatenate 2 column
data <- data.frame(DecisionMale, RaceF)

(alpha <- 1 - 0.95)
boots_dist <- bootstrap(10000) # Tim phan phoi cua bootstrap
(se <- sd(boots_dist, na.rm = TRUE)) # Tinh standard deviation (missing value se bi bo qua)
(conf_boots <- quantile(boots_dist, c(alpha/2, 1 - alpha/2), na.rm = TRUE)) # Tim khoang tin cay cho p

# - Xay dung khoang tin cay cho ti le khac biet giua nu chau a nhan phan hoi Yes va phan hoi No
# Cac TK can tinh
stat <- function(data){
  return (sum(data$DecisionMale == 'Yes' & data$RaceF == 'Asian')/sum(data$DecisionMale == 'Yes') - sum(data$DecisionMale == 'No' & data$RaceF == 'Asian')/ sum(data$DecisionMale == 'No'))
}


(alpha <- 1 - 0.95)
boots_dist <- bootstrap(10000) # Tim phan phoi cua bootstrap
(se <- sd(boots_dist, na.rm = TRUE)) # Tinh standard deviation (missing value se bi bo qua)
(conf_boots <- quantile(boots_dist, c(alpha/2, 1 - alpha/2), na.rm = TRUE)) # Tim khoang tin cay cho p

# 4. 1 bien dinh tinh va 1 bien dinh luong
# Tinh favarite statistics
favstats(AttractiveM ~ DecisionMale)
# Ve boxplot
boxplot(AttractiveM ~ DecisionMale, xlab = "DecisionMale", ylab = "AttractiveM")

# Xay dung khoang tin cho su chenh lech giua mean cua AttractiveM|Yes va cua AttractiveM|No
# Cac TK can tinh
stat <- function(data){
  #Tinh mean cho no va yes
  mean2 <- mean(data$AttractiveM~data$DecisionMale, na.rm = TRUE)
  return (mean2[2] - mean2[1])
}

# Bootstrap
bootstrap <- function(B){
  return (replicate(B, stat(sample(data, nrow(data), replace = TRUE))))
}

# Concatenate 2 column
data <- data.frame(DecisionMale, AttractiveM)

(alpha <- 1 - 0.95)
boots_dist <- bootstrap(10000) # Tim phan phoi cua bootstrap
(se <- sd(boots_dist, na.rm = TRUE)) # Tinh standard deviation (missing value se bi bo qua)
(conf_boots <- quantile(boots_dist, c(alpha/2, 1 - alpha/2), na.rm = TRUE)) # Tim khoang tin cay cho mean

# - Xay dung khoang tin cay cho median chenh lech giau AttractiveM|Yes vaf AttractiveM|No
# Cac TK can tinh
stat <- function(data){
  #Tinh median cho no va yes
  med2 <- median(data$AttractiveM~data$DecisionMale, na.rm = TRUE)
  return (med2[2] - med2[1])
}


(alpha <- 1 - 0.95)
boots_dist <- bootstrap(10000) # Tim phan phoi cua bootstrap
(se <- sd(boots_dist, na.rm = TRUE)) # Tinh standard deviation (missing value se bi bo qua)
(conf_boots <- quantile(boots_dist, c(alpha/2, 1 - alpha/2), na.rm = TRUE)) # Tim khoang tin cay cho median

# 5. 2 bien dinh luong
# Correlation of 2 quantative variables: AttractiveM and LikeM
cor(AttractiveM, LikeM, use = "complete.obs") # Avoid missing values

# Fit regression line
lmInfo <- lm(LikeM~AttractiveM)
summary(lmInfo) # get more info
plot(lmInfo$residuals, pch = 16, col = "red") #Plot residual de xem du lieu co phan bo ngau nhieu khong?


# Graphical display: scatterplot
plot(AttractiveM, LikeM, main = "Scatter plot example", pch=19)
# Add fit lines
abline(lm(LikeM~AttractiveM), col="red") # regression line (y~x)

# Xay dung khoang tin cho correlation cua AttractiveM va LikeM
# Cac TK can tinh
stat <- function(data){
  #Tinh correlation
  return (cor(data$AttractiveM, data$LikeM, use = "complete.obs")) # Avoid missing values

}

# Bootstrap
bootstrap <- function(B){
  return (replicate(B, stat(sample(data, nrow(data), replace = TRUE))))
}

# Concatenate 2 column
data <- data.frame(AttractiveM, LikeM)

(alpha <- 1 - 0.95)
boots_dist <- bootstrap(10000) # Tim phan phoi cua bootstrap
(se <- sd(boots_dist, na.rm = TRUE)) # Tinh standard deviation (missing value se bi bo qua)
(conf_boots <- quantile(boots_dist, c(alpha/2, 1 - alpha/2), na.rm = TRUE)) # Tim khoang tin cay cho correlation

# Xay dung khoang tin cay cho best-fit line cua AttractiveM va LikeM voi 2 tham so a, b
# Cac TK can tinh
stat <- function(data){
  #Tim best-fit line
  lmInfo <- lm(data$LikeM~data$AttractiveM)
  return (lmInfo$coefficients) # Avoid missing values
}

# Bootstrap
bootstrap <- function(B){
  return (replicate(B, stat(sample(data, nrow(data), replace = TRUE))))
}

# Concatenate 2 column
data <- data.frame(AttractiveM, LikeM)

(alpha <- 1 - 0.95)
boots_dist <- bootstrap(10000) # Tim phan phoi cua bootstrap

# Tim sai lech chuan va khoang tin cay cho he so a
a_dist <- boots_dist[seq(1, 10000, by = 2)]
(se <- sd(a_dist, na.rm = TRUE)) # Tinh standard deviation (missing value se bi bo qua)
(conf_boots <- quantile(a_dist, c(alpha/2, 1 - alpha/2), na.rm = TRUE)) # Tim khoang tin cay cho a

# Tim sai lech chuan va khoang tin cay cho he so b
b_dist <- boots_dist[seq(2, 10000, by = 2)]
(se <- sd(b_dist, na.rm = TRUE)) # Tinh standard deviation (missing value se bi bo qua)
(conf_boots <- quantile(b_dist, c(alpha/2, 1 - alpha/2), na.rm = TRUE)) # Tim khoang tin cay cho b


detach(SpeedDating)