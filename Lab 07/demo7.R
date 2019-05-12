require("Lock5withR")
require("APMultipleChoice")
library(mosaic)
library(mosaicData)

head(APMultipleChoice)

# Calculate p-value when we have x_squre = 3.1 and df = 2
pchisq(3.1, 2, lower.tail=FALSE)

# Test for Goodness of Fit
# Get table
t <- table(APMultipleChoice); t
f <- prop.table(t); f
# Chi-squre test
chisq.test(t)

# Test for Association.
attach(StudentSurvey)
chisq.test(table(Award, Gender))
# Randomization test
chisq.test(table(Award, Gender), simulate.p.value=TRUE)
detach(StudentSurvey)

# Chi-squre distribution
chisq.sample <- do(1000) * chisq.test(table(resample(toupper(letters[1:5]), 400)))$statistic
histogram(~X.squared, data = chisq.sample)

# Plot distribution of Chi-squre
plotDist("chisq", params = list(df = 4), type = c("h", "l"), groups = x > 3.425, lty = 1)
# Add lattice plots
ladd(grid.text("3.425", 3.425, 0.175, default.units = "native", hjust = 0))


jury <- c(780, 117, 114, 384, 58)
chisq.test(jury, p = c(0.54, 0.18, 0.12, 0.15, 0.01))
xchisq.test(jury, p = c(0.54, 0.18, 0.12, 0.15, 0.01)) # to list expected counts


# Import the data
file_path <- "http://www.sthda.com/sthda/RDoc/data/housetasks.txt"
housetasks <- read.delim(file_path, row.names = 1)
# head(housetasks)

library("gplots")
# 1. convert the data as a table
dt <- as.table(as.matrix(housetasks))
# 2. Graph
balloonplot(t(dt), main ="housetasks", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)

library("graphics")
mosaicplot(dt, shade = TRUE, las = 2, main = "housetasks")
