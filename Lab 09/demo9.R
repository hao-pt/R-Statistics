#Population
n <- 20
x <- runif(n, min = 0, max = 1)
b0 <- 2; b1 <- 1; sigma <- 0.4
epsilon <- rnorm(n, mean = 0, sd = sigma)
y <- b0 + b1*x + epsilon
plot(y~x)

# Sample
model <- lm(y~x)
model

summary(model)
# Confident interval for intercept and slode
confint(model, "x")
confint(model, "(Intercept)", level = 0.95)

# Bootstrap
stat <- function(){
  rand_index <- sample(1:n, n, replace = TRUE)
  rand_model <- lm(y[rand_index]~x[rand_index])
  return (unname(rand_model$coefficients[2]))
}

boot_dist <- replicate(10000, stat())
hist(boot_dist, breaks = 40)
se <- sd(boot_dist)
alpha <- 1 - 0.95
conf_int <- quantile(boot_dist, c(alpha/2, 1-alpha/2)); conf_int

# Plot multiple lines for different random sample
myplot <- function(){
  rand_index <- sample(1:n, n, replace = TRUE)
  rand_model <- lm(y[rand_index]~x[rand_index])
  abline(rand_model, col = 'gray')
}

plot(y~x)
replicate(1000, myplot())
abline(model, col = 'red')

# Predict
x_star <- c(0.4, 0.6)
new_x <- data.frame(x = c(x_star))
predict(model, new_x)
# Confident interval for y_bar at x_star
predict(model, new_x, interval = 'confidence', level = 1 - alpha)
# Prediction interval for y at x_star
predict(model, new_x, interval = 'prediction', level = 1 - alpha)

