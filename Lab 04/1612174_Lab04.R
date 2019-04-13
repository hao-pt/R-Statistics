require(Lock5withR) # Load package
data("SpeedDating") # Load dataset
head(SpeedDating) # Get 1st 6 cases

# Cau 1
View(SpeedDating) # view dataset
?SpeedDating # Chua cac descriptions cua 1 bang du lieu

# Cau 2
str(SpeedDating) # structure of dataset
names(SpeedDating) # List all varibles in dataset

# Cau 3
attach(SpeedDating) # Avoid dollar sign before each varibles name

# a) One categorical varible
# DecisionMale varible
tab1 = table(DecisionMale) # Count so luong nam yes va no
# Them total
addmargins(tab1)

prop.table(tab1) # Proportions

barplot(tab1) # Ve barchart

# RaceF varbile
tab2 = table(RaceF) #Count so luong nu cho tung chung toc
# Them total
addmargins(tab2)

prop.table(tab2) # Proportions

barplot(tab2)

# b) One quantitative varible
# AttractiveM
five_num = summary(AttractiveM) # 5-number summary

# Range:
range = five_num[6] - five_num[1]; range
# Interquartile range
IQR = five_num[5] - five_num[2]; IQR
# Detect outlier: smaller than Q1 - 1.5(IQR) or greater than Q3 + 1.5(IQR)
(t1 <- five_num[2] - 1.5*IQR); (t2 <- five_num[5] + 1.5*IQR)

# Dotplot de dem so luong cho tung diem tuong ung
dotPlot(~AttractiveM, width = 1, cex = 0.35)
# Ve histogram
hist(AttractiveM)
# Ve phan bo cua du lieu
densityplot(AttractiveM)

# LikeM
five_num = summary(LikeM) # 5-number summary
# Range:
range = five_num[6] - five_num[1]; range
# Interquartile range
IQR = five_num[5] - five_num[2]; IQR
# Detect outlier: smaller than Q1 - 1.5(IQR) or greater than Q3 + 1.5(IQR)
(t1 <- five_num[2] - 1.5*IQR); (t2 <- five_num[5] + 1.5*IQR)

# Tim so luong cac doi tuong outlier
# TH: < t1
count(subset(SpeedDating, LikeM < t1))

# Dotplot de dem so luong cho tung diem tuong ung
dotPlot(~LikeM, width = 1, cex = 0.35)
# Ve histogram
hist(LikeM)
# Ve phan bo cua du lieu
densityplot(LikeM)

# SincereM
five_num = summary(SincereM) # 5-number summary

# Range:
range = five_num[6] - five_num[1]; range
# Interquartile range
IQR = five_num[5] - five_num[2]; IQR
# Detect outlier: smaller than Q1 - 1.5(IQR) or greater than Q3 + 1.5(IQR)
(t1<-five_num[2] - 1.5*IQR); (t2 <- five_num[5] + 1.5*IQR)

# Tim so luong cac doi tuong outlier
# TH: < t1
count(subset(SpeedDating, LikeM < t1))

# Dotplot de dem so luong cho tung diem tuong ung
dotPlot(~SincereM, width = 1, cex = 0.35)
# Ve histogram
hist(SincereM)
# Ve phan bo cua du lieu
densityplot(SincereM)

detach(SpeedDating)

# Cau 4:
attach(SpeedDating) # Avoid dollar sign before each varibles name
# 2 bien dinh tinh
tab1 = table(DecisionMale, RaceF)
# Them margin
addmargins(tab1)

# 2-way table
# Ti le nam (yes/no) dieu kien chung toc nu (Asian, Black, ...)
prop.table(tab1, margin = 1)

barplot(tab1, legend = TRUE)

detach(SpeedDating)

# Cau 5:
attach(SpeedDating) # Avoid dollar sign before each varibles name
# 1 quantitative and 1 categorical varibles
# statistics for the quantitative variable within each category
by(AttractiveM, DecisionMale, mean, na.rm=TRUE)

# Tinh favorite statistics
favstats(~AttractiveM | DecisionMale)

# side-by-side boxplots
boxplot(AttractiveM ~ DecisionMale, xlab = 'DecisionMale', ylab = 'AttractiveM')
detach(SpeedDating)

# Cau 6:
attach(SpeedDating) # Avoid dollar sign before each varibles name
# 2 quantitative varibles
# Graphical display: scatterplot
plot(AttractiveM, LikeM, main = "Scatter plot example", pch=19)
# Add fit lines
abline(lm(LikeM~AttractiveM), col="red") # regression line (y~x)

# Summary statistics: correlation, regression line
cor(AttractiveM, LikeM, use = "complete.obs") # avoid missing value NA
lm(LikeM~AttractiveM) # Linear regression for 2 varibles

detach(SpeedDating)

# Cau 7:
library(GGally)
attach(SpeedDating) # Avoid dollar sign before each varibles name
# Multiple regression
fit <- lm(LikeM~AttractiveM + SincereM)
summary(fit) # show the results

#shows the correlation coefficient of multiple variables 
#in conjunction with a scatterplot 
#(including a line of best fit with a confidence interval) and a density plot.
ggpairs(SpeedDating, 
        columns = c("AttractiveM", "SincereM", "LikeM"), 
        upper = list(continuous = wrap("cor", 
                                       size = 10)), 
        lower = list(continuous = "smooth"))


detach(SpeedDating)
