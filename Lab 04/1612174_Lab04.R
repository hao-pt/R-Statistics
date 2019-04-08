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
summary(AttractiveM) # 5-number summary
hist(AttractiveM)

# LikeM
summary(LikeM) # 5-number summary
hist(LikeM)

# SincereM
summary(SincereM) # 5-number summary
hist(SincereM)

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
