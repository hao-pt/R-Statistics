# Phan I
# Cau 1
people <- c(5.2, 3.4, 2.9, 4.9, 2.6, 2.3, 3.0, 5.9, 2.7, 3.9,
            2.3, 2.3, 3.2, 4.9, 3.0, 4.3, 2.0, 3.1, 2.7, 3.1,
            4.7, 3.4, 3.7, 3.8, 2.9, 2.1, 3.2, 3.6, 4.5, 3.2,
            3.2, 5.1, 2.7, 3.9, 2.9, 3.9, 2.0, 3.0, 3.2, 4.5)
hist(people, breaks = 8)

# Cau 2
mu <- mean(people)
median(people)
mode(people)
five_num <- summary(people)
3.9 - 2.85
se <- sd(people)
mu - se; mu + se

# Cau 3
t.test(people, conf.level = 0.9)
t.test(people, conf.level = 0.95)

# Cau 4
alpha <- 0.05
t.test(people, mu = 2.8, conf.level = 1 - alpha, alternative = "less")

# Phan II
# Cau 5

tab <- matrix(c(24, 20, 44, 52, 52, 50, 30, 28), nrow = 2, byrow = FALSE)
colnames(tab) <- c("Yeu", "TB", "Kha", "Gioi")
rownames(tab) <- c("Nu", "Nam")
tab <- as.table(tab)
tab

# Cau 6
prop.test(tab[1] + tab[3] + tab[5] + tab[7], n = sum(tab), conf.level = 0.9)
sum(tab)

# Cau 7
tab2 <- c(tab[1] + tab[2], tab[3] + tab[4], tab[5] + tab[6], tab[7] + tab[8]); tab2;
chisq.test(tab2, p = c(1/6, 1/3, 1/3, 1/6))

# cCau 7
chisq.test(tab)

# Phan III
nam_cao <- c(1.62, 1.54, 1.6, 1.59, 1.69, 1.68, 1.62, 1.55, 
             1.68, 1.72, 1.71, 1.72, 1.59, 1.69, 1.64)

nam_can <- c(62, 58, 55, 58, 64, 61, 60, 57, 62, 65, 65, 67, 57, 63, 58)

nam <- data.frame(nam_cao, nam_can)

nu_cao <- c(1.55, 1.69, 1.71, 1.57, 1.75, 1.62, 1.49, 1.6, 1.69, 1.7, 1.52, 1.75,
            1.68, 1.57, 1.6)
nu_can <- c(49, 54, 56, 49, 56, 53, 50, 53,57, 57, 47, 59, 55, 53, 51)
nu <- data.frame(nu_cao, nu_can)

boxplot(nam$nam_cao, nu$nu_cao)

t.test(nam$nam_cao, nu$nu_cao, alternative = "greater", conf.level = 1 - 0.05)

chieu_cao <- c(nam$nam_cao, nu$nu_cao)
can_nang <- c(nam$nam_can, nu$nu_can)
model <- lm(chieu_cao~can_nang)
summary(model)

confint(model, "x", level = 0.9)

new_x <- data.frame(x = c(52))
predict(model, new_x)
# Confident interval for y_bar at x_star
predict(model, new_x, interval = 'confidence', level = 0.9)
predict(model, new_x, interval = 'prediction', level = 0.9)

gioi_tinh <- c(rep('Nam', 15), rep('Nu', 15))

model2 <- lm(chieu_cao~ can_nang + gioi_tinh)
summary(model2)

new_x <- data.frame(can_nang = c(52), gioi_tinh = c("Nu"))
# Confident interval for y_bar at x_star
predict(model2, new_x, interval = 'confidence', level = 0.9)
predict(model2, new_x, interval = 'prediction', level = 0.9)
