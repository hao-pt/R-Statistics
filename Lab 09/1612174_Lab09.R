require(Lock5WithR)
library(Lock5withR)
library(mosaic)

# View data
View(SpeedDating)
# Avoid $ symbol
attach(SpeedDating)

# Categorical variables with 2 levels
# Hoi qui cho 1 bien dinh tinh va 1 bien dinh luong

# Plot data
plot(LikeM~DecisionMale)
# Plot residual to check if data contains pattern or not  
plot(model$residuals, pch = 16, col = "blue")

# Compute Linear regression model
model <- lm(LikeM~DecisionMale)
summary(model)

# Use contrasts() function to return codes that R have used to create dummy var
contrasts(DecisionMale)

# We can specify the baseline to Yes by function relevel()
SpeedDating2 <- SpeedDating %>% mutate(DecisionMale = relevel(DecisionMale, ref = "Yes"))
model2 <- lm(LikeM~DecisionMale, data = SpeedDating2)
summary(model2)


# Categorical variables with more than 2 levels
plot(LikeM~AttractiveM + DecisionMale + RaceF + SincereM)

# Check dummy code of RaceF
dummy_code <- model.matrix(~RaceF)
head(dummy_code[, -1])

model <- lm(LikeM~AttractiveM + DecisionMale + RaceF + SincereM)
anova(model)
summary(model)

# Because RaceF is not significantly associated with LikeM, we remove it
model2 <- lm(LikeM~AttractiveM + DecisionMale + SincereM)
summary(model2)

# Anova test
anova(model2)

detach(SpeedDating)
