# CASE STUDY STQD6214 (20%)

# Variables
age<-c(35,34,37,26,28,34,33,28,39,36,30,27,30,23,32,
       31,23,22,33,25,27,33,30,30,34,35,35,30,33,38)
gest<-c(265,269,271,268,273,274,269,269,266,275,269,252,262,271,259,
        274,265,280,280,273,263,270,273,266,280,264,271,267,255,267)
weight<-c(2.9748,3.1148,3.3549,2.4846,2.8745,3.3949,2.6246,3.0249,3.0446,3.4548,
          2.8947,2.3646,3.1446,3.1245,2.8747,2.8748,3.1848,3.0648,2.9048,2.9946,
          3.7149,2.8147,2.7946,2.7145,3.0548,3.7750,3.3649,3.3750,2.9247,2.7446)
# Create data frame
mother<-data.frame(age,gest,weight)
names(mother)<-c("Age","Gestational_period","Newborn_Weight")

# Descriptive statistics
summary(mother)
library(DescTools)
library(e1071)
Mode(mother$Age)


# Create a function of summary plus standard deviation, variance, 
# coefficient of variation, skewness and kurtosis

summary2=function(x)
{
out=c(summary(x), Stdev=sd(x), Var=var(x), CV=sd(x)/mean(x)*100, 
    Sk=skewness(x), Kt=kurtosis(x))
out
}

summary2(mother$Age)
summary2(mother$Gestational_period)
summary2(mother$Newborn_Weight)

# Histogram for each variable
par(mfrow=c(1,3))
hist(mother$Age, main="Histogram of Mother's Age", xlab="Age")
hist(mother$Gestational_period, main="Histogram of Gestational Period (days)", xlab="No of Days")
hist(mother$Newborn_Weight, main="Histogram of Newborn Weight (kg)", xlab="Weight (kg)")

# Boxplots for each variable
par(mfrow=c(1,3))
boxplot(mother$Age, main="Boxplot of Mother's Age", ylab="Age")
boxplot(mother$Gestational_period, main="Boxplot of Gestational Period (days)", ylab="No of Days")
boxplot(mother$Newborn_Weight, main="Boxplot of Newborn Weight (kg)", ylab="Weight (kg)")


# Plot a scatterplot matrix
pairs(~Newborn_Weight + Age + Gestational_period, data=mother)

# Fit a multiple linear regression
mother.lm <- lm(Newborn_Weight~ Age + Gestational_period, data=mother)
summary(mother.lm)

# Adjusted R-squared is negative
# Model is worse than just using the mean to predict outcomes

# The fitted equation based on the collected data is
# weight = 0.701199 + 0.012083age + 0.007291gest

# Fit a simple linear regression between Gestational_period and Newborn_Weight
ex.lm<-lm(Newborn_Weight~Gestational_period,data=mother)
summary(ex.lm)
plot(Newborn_Weight~Gestational_period,data=mother,pch=19)
abline(ex.lm)

# Monte Carlo
# Age
m <- 10000
n <- 1000
minAge <- min(mother$Age)
maxAge <- max(mother$Age)
xAge <- matrix(runif(m*n,min=minAge,max=maxAge), nrow=m)
meanAgeMC <- apply(xAge,MARGIN=1,FUN=mean)
sdevAgeMC <- apply(xAge,MARGIN=1,FUN=sd)
CVAgeMC <- sdevAgeMC/meanAgeMC*100
rangeAgeMC <- apply(apply(xAge,MARGIN=1,FUN=range),MARGIN=2,FUN=diff)

# Plots
par(mfrow=c(2,2)) # Creates a 2x2 graphics window.
hist(meanAgeMC,xlab="Mean",ylab="Frequency", main="Monte Carlo Mean (Age)")
hist(sdevAgeMC,xlab="Std. Deviation",ylab= "Frequency",main="Monte Carlo 
Standard Deviation (Age)")
hist(CVAgeMC,xlab="Coefficient of Variation",ylab="Frequency", main="Monte Carlo 
Coefficient of Variation (Age)")
hist(rangeAgeMC,xlab="Range",ylab="Frequency", main="Monte Carlo 
Range (Age)")


# Gestational_period
p <- 10000
q <- 1000
minGest <- min(mother$Gestational_period)
maxGest <- max(mother$Gestational_period)
xGest <- matrix(runif(p*q,min=minGest,max=maxGest), nrow=p)
meanGestMC <- apply(xGest,MARGIN=1,FUN=mean)
sdevGestMC <- apply(xGest,MARGIN=1,FUN=sd)
CVGestMC <- sdevGestMC/meanGestMC*100
rangeGestMC <- apply(apply(xGest,MARGIN=1,FUN=range),MARGIN=2,FUN=diff)

# Plots
par(mfrow=c(2,2)) # Creates a 2x2 graphics window.
hist(meanGestMC,xlab="Mean",ylab="Frequency", main="Monte Carlo Mean (Gestational_period)")
hist(sdevGestMC,xlab="Std. Deviation",ylab= "Frequency",main="Monte Carlo 
Standard Deviation (Gestational_period)")
hist(CVGestMC,xlab="Coefficient of Variation",ylab="Frequency", main="Monte Carlo 
Coefficient of Variation (Gestational_period)")
hist(rangeGestMC,xlab="Range",ylab="Frequency", main="Monte Carlo 
Range (Gestational_period)")


# Newborn_Weight
a <- 10000
b <- 1000
minNbW <- min(mother$Newborn_Weight)
maxNbW <- max(mother$Newborn_Weight)
xNbW <- matrix(runif(a*b,min=minNbW,max=maxNbW), nrow=a)
meanNbWMC <- apply(xNbW,MARGIN=1,FUN=mean)
sdevNbWMC <- apply(xNbW,MARGIN=1,FUN=sd)
CVNbWMC <- sdevNbWMC/meanNbWMC*100
rangeNbWMC <- apply(apply(xNbW,MARGIN=1,FUN=range),MARGIN=2,FUN=diff)

# Plots
par(mfrow=c(2,2)) # Creates a 2x2 graphics window.
hist(meanNbWMC,xlab="Mean",ylab="Frequency", main="Monte Carlo 
Mean (Newborn_Weight)")
hist(sdevNbWMC,xlab="Std. Deviation",ylab= "Frequency",main="Monte Carlo 
Standard Deviation (Newborn_Weight)")
hist(CVNbWMC,xlab="Coefficient of Variation",ylab="Frequency", main="Monte Carlo 
Coefficient of Variation (Newborn_Weight)")
hist(rangeNbWMC,xlab="Range",ylab="Frequency", main="Monte Carlo 
Range (Newborn_Weight)")


