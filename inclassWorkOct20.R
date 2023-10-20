# In class work Oct 20

#Cook's Distance' example using mtcars
mtcars
head(mtcars)
str(mtcars)
model1 <- lm(mpg~cyl+wt,data=mtcars)
model1
help("cooks.distance")
plot(model1,pch=18,col="red",which=c(4))

#we can use the cooks.distance() function to identift the Cook's distance to each observation
cooks.distance(model1)
CooksDistance <- cooks.distance(model1)

#Now we will round off the values to 5 decimal points to that it is east to read
# we can use the round() funciton to round off values in R.
round(CooksDistance,5)
sort(round(CooksDistance,5))

#Outlier Detection using "Cooks distance"
#Multivariate Regression using Cook's Distance
#Cook's Distance is an estimate of the influece of a data point.
#Cook's Distance is a summary of how much a regression model changes when the ith ovserbation is removed from the data.
library(ISLR)
#Let's look at the baseball hitters dataset in ISLR package.
head(Hitters)
dim(Hitters)
is.na(Hitters)#check for missing values.
#Now we will remove the NA (missing values) using the na.omit() function
HittersData <- na.omit(Hitters)
dim(HittersData) #Checking the dimensions after removing the NAs.
glimpse(HittersData)
head(HittersData)
#Now we will implement a multivariate regression model using all the features in the dataset to
# predict the salary of the Baseball player
SalaryPredictModel1 <- lm(Salary~., data=HittersData)
summary(SalaryPredictModel1)
#Multiple R-squared: 0.5461, Adjusted R-squared: 0.5106

#Cook's distance.
cooksD <- cooks.distance(SalaryPredictModel1)
influential <- cooksD[(cooksD>(3*mean(cooksD,na.rm=T)))]
influential
#We see that 18 players have a Cook's Distance greater than 3x the mean.
#Let's exclude these 18 players and rerun the model to see if we have a better fit in our regression model.
names_of_influential <- names(influential)#checking the names of the influential/outlier players
names_of_influential
outliers<- HittersData[names_of_influential,]
#Hitters_Without_Outliers <- HittersData%>%anti_join(outliers)

#Normality Tests...
#Normal Distribution
#Read the documentation of the random distribution fucntion
help("rnorm")
set.seed(10)
data1 <- rnorm(50)

set.seed(30)
data2 <- rnorm(50)
shapiro.test((data1))
hist(data1,col="green")
shapiro.test(data2)
hist(data2,col="steelblue")

set.seed(0)
data <- rnorm(100)
shapiro.test(data)
data <- rpois(n=100,lambda = 3)
shapiro.test(data)
hist(data,col="yellow")
library(nortest)
set.seed(1)
x <- rnorm(100,0,1)
ad.test(x)

x<-runif(100,0,1)
ad.test(x)
