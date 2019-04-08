require(Lock5withR) # Load package containing our data set
data("SpeedDating") #load table
str(SpeedDating) # structure of table
View(SpeedDating) # hien structure duoi dang bang
?SpeedDating # Chua cac descriptions cua 1 bang du lieu
data() # List cac data
summary(SpeedDating)

dim(SpeedDating) # dimension of table (row, col)
sample(SpeedDating, 5)

data = head(LifeExpectancyVehicles, 10) # Lay 10 dong dau tien
sub <- filter(LifeExpectancyVehicles, data["Year"]%%4 == 2)
sub
xyplot(LifeExpectancy ~ Vehicles, xlab = "Vehicles (millions)", 
       ylab = "Life Expectancy", data = LifeExpectancyVehicles)

require(lattice) # De ve hinh
require(mosaicData)
head(Births78) #  first six cases of the data set
# Plot data with date on x-axis and no. of birthday on y-axis
xyplot(births ~ date, data = Births78) 
# Mean of no. of birthdays
mean(~births, data = Births78)

#OneTrueLove <- read.csv("https://raw.githubusercontent.com/rpruim/Lock5withR/master/Book/OneTrueLove.csv")
dim(OneTrueLove)
barchart(~Response, data = OneTrueLove)



require(Lock5withR) # Load package containing our data set
require(dplyr)
data("StudentSurvey")
View(StudentSurvey)
#tally(~Gender + Award, margin = TRUE, data = StudentSurvey)
tally()

names(StudentSurvey)
