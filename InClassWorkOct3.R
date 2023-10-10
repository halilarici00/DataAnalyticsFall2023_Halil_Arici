# In-class work Oct 3
multivariate <- read.csv("DA_Files/multivariate.csv")
attach(multivariate)
mm <- lm(Homeowners~Immigrant)
mm

#Create some Scatterplots
plot(Income,Immigrant,main="Scatterplot")

#Fitting linear models using "lm" function
help(lm)
mm<-lm(Homeowners~Immigrant)
mm
plot(Immigrant,Homeowners)
abline(mm)
abline(mm, col=2, lwd=3)

summary(mm)
attributes(mm)
mm$coefficients

help(abline)
