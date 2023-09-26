#Creating Plots
#Chapter 2 -- R Graphics Cookbook
plot(mtcars$wt,mtcars$mpg)
library(ggplot2)
qplot(mtcars$wt,mtcars$mpg)
qplot(wt,mpg,data=mtcars)
ggplot(mtcars,aes(x=wt,y=mpg))+geom_point()
plot(pressure$temperature,pressure$pressure,type="l")
points(pressure$temperature,pressure$pressure)

lines(pressure$temperature,pressure$pressure/2, col="red")
points(pressure$temperature,pressure$pressure/2, col="blue")

qplot(pressure$temperature,pressure$pressure, geom="line")
qplot(temperature,pressure,data=pressure, geom="line")
ggplot(pressure,aes(x=temperature,y=pressure))+geom_line()+geom_point()
ggplot(pressure,aes(x=temperature,y=pressure))+geom_line()+geom_point()

#Creating Bar Graphs
barplot(BOD$demand, names.arg = BOD$Time)
table(mtcars$cyl)
barplot(table(mtcars$cyl)) # generate a table of counts
qplot(mtcars$cyl) #cyl is continuous here
qplot(factor(mtcars$cyl)) # treat cyl as discrete

#Bar graph of counts
qplot(factor(cyl),data=mtcars)
ggplot(mtcars,aes(x=factor(cyl)))+geom_bar()

#Creating Histogram
#View the distribution of one-dimensional data with a histogram
hist(mtcars$mpg)
hist(mtcars$mpg, breaks=10) # specify approximate number of bins with breaks
hist(mtcars$mpg, breaks=5)
hist(mtcars$mpg, breaks=12)
qplot(mpg, data=mtcars, binwidth=4)
ggplot(mtcars, aes(x=mpg))+geom_histogram(binwidth = 4)
ggplot(mtcars, aes(x=mpg))+geom_histogram(binwidth = 5)

#Creating Box-plot
plot(ToothGrowth$supp, ToothGrowth$len)# using plot() function and pass it a factor of x-values and vector of y-values
#Formula Syntax
boxplot(len ~ supp, data = ToothGrowth)# if the tow vectors are in the same dataframe, you can use the formula syntax. With
#this syntax you can combine two variables on the x-axis.
#put interaction of two variables on x-axis.
boxplot(len ~ supp + dose, data = ToothGrowth)
# with ggplot2 you can get the same results above
qplot(ToothGrowth$supp, ToothGrowth$len, geom="boxplot")
# if the two vectors are in the same dataframe, you can use the following syntax
qplot(supp, len, data = ToothGrowth, geom = "boxplot")
# in ggplot2, the above equivalent to:
ggplot(ToothGrowth, aes(x=supp, y=len)) + geom_boxplot()
# Using three separate vectors
qplot(interaction(ToothGrowth$supp,ToothGrowth$dose),ToothGrowth$len, geom="boxplot")
# We can write the same thing above, get the columns from the dataframe
qplot(interaction(supp,dose), len, data = ToothGrowth, geom="boxplot")
# Using ggplot() you can do the samething and it is equivalent to:
ggplot(ToothGrowth, aes(x=interaction(supp,dose), y=len)) + geom_boxplot()


