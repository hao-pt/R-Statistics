require(Lock5withR) # Load package
library(Lock5withR)
library(mosaic)

# head(SpeedDating)
attach(SpeedDating) # Avoid dollar sign before each varibles name

#1.  Test goodness of fit for single categorical variables
# a) DecisionMale
# H0: p = p0 = 0.5
# H1: p != 0.5

# Load data
data <- DecisionMale
# Significant level
alpha <- 0.05
# Frequency table
t <- table(data); t
# Chisq test
res <- chisq.test(t); res
# If p.value < alpha, we ignore H0
(res$p.value < alpha)
# Expected values
res$expected

# b) RaceF
# H0: Cac chung toc co phan phoi deu nhau (bang 1/6)
# H1: Cac chung toc co phan phoi khong nhau nha (khac 1/6)

# Load data
data <- RaceF
# Significant level
alpha <- 0.05

# Frequency table
t <- table(data); t
t.prob <- prop.table(t); t.prob

# Chisq test
res <- chisq.test(t, p = rep(1/6, 6)); res
# If p.value < alpha, we ignore H0
(res$p.value < alpha)
# Expected values
res$expected

# 2. Test independence of 2 catogorical variables
# DecisionMale and RaceF
# H0: DecisionMale va RaceF doc lap nhau
# H1: DecisionMale va RaceF co moi lien ket voi nhau

# Load data
data <- data.frame(DecisionMale, RaceF)
# Significant level
alpha <- 0.05

# Frequency table
t <- table(data); t
# P(RaceF|DecisionMale)
t.prob <- prop.table(t, margin = 1); t.prob

# Graph
library("gplots")
balloonplot(t(t), main = "data", xlab="", ylab="", label = FALSE, show.margins = TRUE)

library("graphics")
# shade: color graph
# las = 1: horizontal labels
mosaicplot(t(t), shade = TRUE, las = 1, main = "data")

# Chisq test
res <- chisq.test(t); res
# If p.value < alpha, we ignore H0
(res$p.value < alpha)

# Observed values
res$observed
# Expected values
round(res$expected, 2)

# Pearson Residuals: Do lech giua observed values and expected values
round(res$residuals, 3)
# Visualize Pearson residuals
library(corrplot)
corrplot(res$residuals, is.cor = FALSE)

# Contribution (Percentage %) of given cell to total chi-squre
contrib <- 100*res$residuals^2 / res$statistic
round(contrib, 3)
# Visualiza contribution
corrplot(contrib, is.cor = FALSE)

31.516 + 28.102 + 18.742 + 16.688
