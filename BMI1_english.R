
################################
## Set the working directory

## In RStudio use conveniently the menu "Session->Set Working
## Directory->To Source File Location"
## In R use only "/" for separating in paths (i.e. no backslash)
setwd("Replace with path to where the project files are")

################################
## Import the data

## Read the BMI1_data.csv file containing the data
D <- read.table("BMI1_data.csv", header=TRUE, sep=",", as.is=TRUE)

#############################
## Overview of the data

## Dimension of D (number of rows and columns)
dim(D)
## Column names
names(D)
## The first rows
head(D)
## The last rows
tail(D)
## Default summary
summary(D)
## Another summary function also including the data type
str(D)

################################
## Factor variable defined
D$Gender <- as.factor(D$Gender)
D$Alone <- as.factor(D$Alone)
D$Children <- as.factor(D$Children)
D$Labeling <- as.factor(D$Labeling)
D$Urbanity <- as.factor(D$Urbanity)
D$Education <- as.factor(D$Education)

################################
## Variable FastFood recodes
D$FastFood_k <-as.numeric(ifelse(D$FastFood==1,0,
                            ifelse(D$FastFood==2,1,
                            ifelse(D$FastFood==3,6,
                            ifelse(D$FastFood==4,24,
                            ifelse(D$FastFood==5,78.2,
                            ifelse(D$FastFood==6,182.5,
                            ifelse(D$FastFood==7,286.8,
                            ifelse(D$FastFood==8,365,NA)))))))))


################################
## Descriptive analysis of selected variables
D$BMI <- (D$Weight/(D$Height)^2)
var(D$BMI)

################################
## Boxplots of selected variables
boxplot(c(D$Weight, 49), col = "red", main = "Weight boxplot")
text(1.3, quantile(c(D$Weight, 150)), c("Minimum","Q1","Median","Q3","Maximum"), col="blue")
boxplot(c(D$BMI, .0025), col = "red", main = "BMI boxplot")
text(1.3, quantile(c(D$BMI, .0025)), c("Minimum","Q1","Median","Q3","Maximum"), col="blue")
boxplot(c(D$Height, 174.15), col = "red", main = "Height boxplot")
text(1.3, quantile(c(D$Height, 174.15)), c("Minimum","Q1","Median","Q3","Maximum"), col="blue")
boxplot(c(D$FastFood_k, 19.04), col = "red", main = "Fastfood_k boxplot")
text(1.3, quantile(c(D$FastFood_k, 19.04)), c("Minimum","Q1","Median","Q3","Maximum"), col="blue")

## min/max values of data
min(D$BMI)
max(D$BMI)

min(D$Height)
max(D$Height)

min(D$Weight)
max(D$Height)

min(D$FastFood_k)
max(D$FastFood_k)

## To run a function on the selected columns you can use the "apply",
## "2" indicates that the calculations are performed by column

t(apply(D[,c("BMI", "Weight","Height","FastFood_k")],2,function(x){
  c(n=length(x),mean=mean(x), var=var(x), sd=sd(x),
    quantile(x,c(0,0.25,0.5,0.75,1)))}))

################################
qqnorm(D$BMI,main='Validation of normal distribution assumption for BMI',
       cex.main=0.8,xlab='z-scores',ylab='BMI')
qqline(D$BMI)

## Required for manual calculation
mean(D$BMI)
sd(D$BMI)

################################
## Calculations of the 95% confidence interval for BMI alternatively logBMI
t.test(D$BMI, conf.level=0.95)$conf.int

## Required for manual calculation
qt(.975, df = 846)

## Calclations of 95% confidence interval for BMI
x <- sd(D$BMI)*sd(D$BMI)
846*x/qchisq(.975, df = 846)
846*x/qchisq(.025, df = 846)

################################
## Right-sided t-test of the hypothesis mu = 25 for BMI
## alternative mu=log(25) equivalent to logBMI
t.test(D$BMI,mu=.0025,alternative="greater")

################################
## Implementation of the t-test (Welch-test) for comparison
## two normal samples.
## Remember to change for log-transformed values

## Comparison of male and female BMI
t.test(subset(D$BMI,D$Gender==0), subset(D$BMI,D$Gender==1))

## Values required for calculations
mean(subset(D$BMI,D$Gender==1))
sd(subset(D$BMI,D$Gender==1))
str(subset(D$BMI,D$Gender==1))

mean(subset(D$BMI,D$Gender==0))
sd(subset(D$BMI,D$Gender==0))
str(subset(D$BMI,D$Gender==0))

################################
## Sample mean for all urbanity groups
## Remember to change for log-transformed values
tapply(D$BMI, D$Urbanity, mean)

################################
## Comparison of two samples
## Remember to change for log-transformed values
t.test(subset(D$BMI, D$Urbanity == 1),
       subset(D$BMI, D$Urbanity == 5))

## Values required for calculations
mean(subset(D$BMI, D$Urbanity == 1))
sd(subset(D$BMI, D$Urbanity == 1))
str(subset(D$BMI, D$Urbanity == 1))

mean(subset(D$BMI, D$Urbanity == 5))
sd(subset(D$BMI, D$Urbanity == 5))
str(subset(D$BMI, D$Urbanity == 5))

################################
## Determination of correlation between selected variables
cor(D[,c("Age","Height","Weight",
           "FastFood_k","BMI")],use = "p")

plot(D$BMI, D$Weight)
plot(D$Height, D$Weight)
