EPI_data <- read.csv("C:/Users/halil/OneDrive/Documents/DA_Files/EPI/2010EPI_data.csv",skip = 1)
#or
#library(XLS)
#EPI_data <- read.xls("C:/Users/halil/OneDrive/Documents/DA_Files/EPI/2010EPI_data.xls")
# Note: replace default data frame name – cannot start with numbers!
View(EPI_data)
#
attach(EPI_data) 	# sets the ‘default’ object
fix(EPI_data) 	# launches a simple data editor
EPI <- EPI_data$EPI			# prints out values EPI_data$EPI
EPI
tf <- is.na(EPI) # records True values if the value is NA
E <- EPI[!tf] # filters out NA values, new array

summary(EPI)
fivenum(EPI,na.rm=TRUE)
stem(EPI)

#Save the plot to a jpeg
jpeg(file="histogramEPI.jpeg")
hist(EPI)
hist(EPI,seq(30.,95.,1.0),prob=TRUE)
lines(density(EPI,na.rm = TRUE,bw=1.))#or try bw="SJ"

rug(EPI)     
dev.off() #saved
help(rug)

help(ecdf)

plot(ecdf(EPI),do.points=FALSE,verticals=TRUE)
help(par)
par(pty="s")#"s" generates a square plotting region par=parameter
qqnorm(EPI);qqline(EPI)
help(seq)
x <- seq(30,95,1)
help(qqplot)
qqplot(qt(ppoints(250),df=5),x,xlab="Q-Q Plot for t dsn")
qqline(x)

#Exercise 1: Fitting a distribution for another 2 variables
#I will try DALY and WATER_H
DALY <- EPI_data$DALY
WATER_H <- EPI_data$WATER_H
plot(ecdf(DALY),do.points=F,verticals=T)
par(pty="s")
qqnorm(DALY);qqline(DALY)
qqplot(qt(ppoints(250),df=5),x,xlab="Q-Q Plot for t dsn")

plot(ecdf(WATER_H),do.points=F,verticals=T)
par(pty="s")
qqnorm(WATER_H);qqline(WATER_H)
qqplot(qt(ppoints(250),df=5),x,xlab="Q-Q Plot for t dsn")

#Comparing distributions
boxplot(EPI,DALY)
qqplot(EPI,DALY)

#Slide 19 Intercompare
BIODIVERSITY <- EPI_data$BIODIVERSITY
FORESTRY <- EPI_data$FORESTRY

plot(ecdf(BIODIVERSITY),do.points=F,verticals=T)
par(pty="s")
qqnorm(BIODIVERSITY);qqline(BIODIVERSITY)
qqplot(qt(ppoints(250),df=5),x,xlab="Q-Q Plot for t dsn")

plot(ecdf(FORESTRY),do.points=F,verticals=T)
par(pty="s")
qqnorm(FORESTRY);qqline(FORESTRY)
qqplot(qt(ppoints(250),df=5),x,xlab="Q-Q Plot for t dsn") #ASK

#Comparing Distributions
boxplot(BIODIVERSITY,FORESTRY)
qqplot(BIODIVERSITY,FORESTRY)

help(distributions)
#EXERCISE 2 Filtering
#Conditional Filtering
EPILand <- EPI[!Landlock]
ELand <- EPILand[!is.na(EPILand)]
hist(ELand)
hist(ELand,seq(30.,95.,1.0),prob=TRUE)

plot(ecdf(ELand),do.points=F,verticals=T)
par(pty="s")
qqnorm(ELand);qqline(ELand)
qqplot(qt(ppoints(250),df=5),x,xlab="Q-Q Plot for t dsn")

#No_surface_water
EPI_Water <- EPI[!No_surface_water]
EWater <- EPI_Water[!is.na(EPI_Water)]
hist(EWater)
hist(EWater, seq(30.,95.,1.0),prob=T)

plot(ecdf(EWater),do.points=F,verticals=T)
par(pty="s")
qqnorm(EWater);qqline(EWater)
qqplot(qt(ppoints(250),df=5),x,xlab="Q-Q Plot for t dsn")

#Desert
EPI_Desert <- EPI[!Desert]
EDesert <- EPI_Desert[!is.na(EPI_Desert)]
hist(EDesert)
hist(EDesert,seq(30.,95.,1.0),prob=T)

plot(ecdf(EDesert),do.points=F,verticals=T)
par(pty="s")
qqnorm(EDesert);qqline(EDesert)
qqplot(qt(ppoints(250),df=5),x,xlab="Q-Q Plot for t dsn")

#High_Population_Density
EPI_HPD <- EPI[!High_Population_Density]
EHPD <- EPI_HPD[!is.na(EPI_HPD)]
hist(EPHD)
hist(EPHD,seq(30.,95.,1.0),prob=T)

#YOUR TASK
EPI_South_Asia <- EPI[GEO_subregion=="South Asia"]

#other data
GRUMP_data <- read.csv("C:/Users/halil/OneDrive/Documents/DA_Files/GPW3_GRUMP_SummaryInformation_2010.csv")

View(GRUMP_data)
#
attach(GRUMP_data) 	# sets the ‘default’ object
fix(GRUMP_data) 	# launches a simple data editor
Area <- GRUMP_data$Area			# prints out values EPI_data$EPI
Area
tf <- is.na(Area) # records True values if the value is NA
A <- Area[!tf] # filters out NA values, new array

summary(A)
