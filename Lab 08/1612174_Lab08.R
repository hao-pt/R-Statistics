require(Lock5withR)
library(Lock5withR)
library(mosaic)

# View data
# View(SpeedDating)
# Avoid using $
attach(SpeedDating)

# Tinh cac TK co ban
favstats(AttractiveM~RaceF)

# Ve bivariate scatter plot va boxplot
xyplot(AttractiveM~RaceF)
boxplot(AttractiveM~RaceF, xlab = "RaceF", ylab = "AttractiveM")

# Analyst of variance
# C1:
# Phan tich phuong sai bang ham lm
Male.model <- lm(AttractiveM~RaceF); Male.model
# Anova test
anova(Male.model)
# C2: Dung truc tiep ham aov de tinh toan bo ban ANOVA
res <- aov(AttractiveM~RaceF)
summary(res)

# Multiple-comparison
# Khi so sanh tren nhieu nhom cung 1 luc thi bay gio bat dau so sanh 
# tung cap rieng le de xem co su khac biet dang ke khong
pairwise.t.test(AttractiveM, RaceF, p.adjust = "holm") 
# p.adjust = "holm": tell R know that adjust p-value for multiple comparision
pairwise.t.test(AttractiveM, RaceF, p.adjust = "bonferroni")
# pool.sd = FALSE de khong su dung chung sd cho cac nhom
pairwise.t.test(AttractiveM, RaceF, p.adjust = "bonferroni", pool.sd = FALSE)

# To know difference means between 2 groups and conf interval 95% of if
tukey.model <- TukeyHSD(res); tukey.model
plot(tukey.model)
