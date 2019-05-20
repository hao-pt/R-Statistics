group <- c(rep(1, 9), rep(2, 11), rep(3, 20)); group
# Convert to factor
group <- as.factor(group); group
galactose <- c(1343,1393,1420,1641,1897,2160,2169,2279,2890,
               1264,1314,1399,1605,2385,2511,2514,2767,2827,2895,3011,
               1809,2850,1926,2964,2283,2973,2384,3171,2447,3257,2479,3271,2495,3288,
               2525,3358,2541,3643,2769,3657)
data <- data.frame(group, galactose); data
attach(data)

# ANOVA test
res <- aov(galactose~group)
summary(res)
# Boxplot
boxplot(galactose~group)
# Pair-wise test
TukeyHSD(res)

# Randomization test
k <- max(c(group)); k
ni <- rep(0, k)
for (i in 1:k)
  ni[i] <- sum(group == i)
ni
n <- sum(ni); n

f_stat <- function(groupIndex){
  # Mean overall
  x_bar <- mean(galactose)
  # Mean of each group
  xi_bar <- rep(0, k)
  for(i in 1:k)
    xi_bar[i] <- mean(subset(galactose, groupIndex == i))
  
  SST <- sum((galactose - x_bar)^2)
  SSG <- sum(ni*(xi_bar - x_bar)^2)
  SSE <- SST - SSG
  # Return f-statistic
  return ((SSG/(k - 1))/(SSE/(n - k)))
}

randomization <- function(B)
  return (replicate(B, f_stat(sample(group, n))))


# Tinh f-stat tren mau goc
f_sample <- f_stat(group); f_sample
# Randomization distribution
rand_dist <- randomization(10000); hist(rand_dist)
# Tinh pvalue
p_value <- sum(rand_dist >= f_sample)/length(rand_dist); p_value


