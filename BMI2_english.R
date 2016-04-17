
################################
## Import data

## Import data file BMI2_data.csv
D <- read.table("BMI2_data.csv", header=TRUE, sep=",", as.is=TRUE)

log_BMI = log(D$BMI)
log_Weight = log(D$Weight)


D$HouseholdIncome_k <-as.numeric(ifelse(D$HouseholdIncome==1,51.8,
                                 ifelse(D$HouseholdIncome==2,150.0,
                                 ifelse(D$HouseholdIncome==3,247.2,
                                 ifelse(D$HouseholdIncome==4,347.2,
                                 ifelse(D$HouseholdIncome==5,446.7,
                                 ifelse(D$HouseholdIncome==6,549.5,
                                 ifelse(D$HouseholdIncome==7,649.1,
                                 ifelse(D$HouseholdIncome==8,747.4,
                                 ifelse(D$HouseholdIncome==9,846.7,
                                 ifelse(D$HouseholdIncome==10,946.3,
                                 ifelse(D$HouseholdIncome==11,1502.4,NA))))))))))))


hist(log_BMI)
hist(log_Weight)
hist(D$Age)

mean(log_BMI)
mean(log_Weight)
mean(D$Age)
mean(D$HouseholdIncome_k)
mean(D$FastFood_k)

var(log_BMI)
var(log_Weight)
var(D$Age)
var(D$HouseholdIncome_k)
var(D$FastFood_k)

sd(log_BMI)
sd(log_Weight)
sd(D$Age)
sd(D$HouseholdIncome_k)
sd(D$FastFood_k)

boxplot(c(log_BMI, 3.228), col = "red", main = "BMI boxplot")
text(1.3, quantile(c(log_BMI, 3.228)), c("Minimum","Q1","Median","Q3","Maximum"), col="blue")
boxplot(c(log_Weight, 4.335194), col = "red", main = "Weight boxplot")
text(1.3, quantile(c(log_Weight, 4.335194)), c("Minimum","Q1","Median","Q3","Maximum"), col="blue")
boxplot(c(D$Age, 44.6222), col = "red", main = "Age boxplot")
text(1.3, quantile(c(D$Age, 44.6222)), c("Minimum","Q1","Median","Q3","Maximum"), col="blue")
boxplot(c(D$HouseholdIncome_k, 583.5498), col = "red", main = "HouseholdIncome_k boxplot")
text(1.3, quantile(c(D$HouseholdIncome_k, 583.5498)), c("Minimum","Q1","Median","Q3","Maximum"), col="blue")
boxplot(c(D$FastFood_k, 19.04463), col = "red", main = "Fastfood_k boxplot")
text(1.3, quantile(c(D$FastFood_k, 19.04463)), c("Minimum","Q1","Median","Q3","Maximum"), col="blue")

################################
## Determine the portion of the data set include the last 7 observations
BMI_test <- subset(D,Obs.no >= 841)

## Determine data set with the 840 first observations - analysis data!
BMI_analysis <- subset(D,Obs.no <= 840)


#########################
## c)

BMI_analysis$log_Weight <- log(BMI_analysis$Weight) 
BMI_analysis$log_BMI    <- log(BMI_analysis$BMI)

#########################
## d)

## Multiple regression analysis
lm1 <- lm(BMI_analysis$log_BMI ~ BMI_analysis$log_Weight+BMI_analysis$Age+BMI_analysis$FastFood_k+
              BMI_analysis$HouseholdIncome_k, BMI_analysis)
summary(lm1)

##########################################
## e)
## Model validation
par(mfrow=c(1,1))

## Observed model output vs. fitted: the assumption of linearity
plot(BMI_analysis$log_BMI, lm1$fitted.values, xlab='log_BMI',
       ylab='Fitted log_BMI', pch=16, cex.lab=0.8)
title(main = list("Validation of the linearity assumption", cex=0.9))

## Residual vs. each of the explanatory variables
## Assumption of linearity
plot(BMI_analysis$log_Weight, lm1$residuals,
        xlab='log_Weight', ylab='Residuals', pch=16, cex.lab=0.8)
title(main = list("Validation of the linearity assumption", cex=0.9))

plot(BMI_analysis$Age, lm1$residuals,
     xlab='Age', ylab='Residuals', pch=16, cex.lab=0.8)
title(main = list("Validation of the linearity assumption", cex=0.9))

plot(BMI_analysis$FastFood_k, lm1$residuals,
     xlab='FastFood_k', ylab='Residuals', pch=16, cex.lab=0.8)
title(main = list("Validation of the linearity assumption", cex=0.9))

plot(BMI_analysis$HouseholdIncome_k, lm1$residuals,
    xlab='HouseholdIncome_k', ylab='Residuals', pch=16, cex.lab=0.8)
title(main = list("Validation of the linearity assumption", cex=0.9))

## Residuals vs. fittedd: The assumption of constant variance
plot(lm1$fitted.values, lm1$residuals, xlab='Fitted values',
     ylab='Residuals', pch=16, cex.lab=0.8)
title(main = list("Validation of variance homogenity", cex=0.9))

# Normal plot of residuals: The normal distribution assumption
qqnorm(lm1$residuals, ylab='Residuals', xlab='z-scores',
       main='Validation of The normal distribution assumption',
       cex.lab=0.9, pch=16, cex.main=0.9)
qqline(lm1$residuals)

###############################
# h)
qt(.975, 835)
# R command that determines the confidence intervals for the parameters
confint(lm1, level=0.95)

# p-value for part g)
pt(21.938, 835)

# R command for for backward selection
lm3 <- lm(BMI_analysis$log_BMI ~ BMI_analysis$log_Weight+BMI_analysis$Age+BMI_analysis$HouseholdIncome_k, BMI_analysis)
summary(lm3)
###############################
## i)
## model using the last 7 observations - BMI_test
BMI_test$log_BMI = log(BMI_test$BMI)
BMI_test$log_Weight = log(BMI_test$Weight)
lm2 <- lm(BMI_test$log_BMI ~ BMI_test$log_Weight + BMI_test$Age + BMI_test$FastFood_k + BMI_test$HouseholdIncome_k, BMI_test)

## obtain values for manual calculation of prediction interval
npred <- predict(lm2, newdata=BMI_test, se = TRUE)
npred

## Prediction and pBMI_testn interval is given by
pred <- predict(lm2, newdata=BMI_test, interval="prediction",
              level=0.95)
as.data.frame(cbind(Obs.no=BMI_test$Obs.no, log_BMI=BMI_test$log_BMI, pred))

#########################
## j)
##############################
## Examination of the importance of collinearity problems
lm3a <- lm(BMI_analysis$log_BMI~BMI_analysis$log_Weight, BMI_analysis)
summary(lm3a)
lm3b <- lm(BMI_analysis$log_BMI~BMI_analysis$Age, BMI_analysis)
summary(lm3b)
lm3c <- lm(BMI_analysis$log_BMI~BMI_analysis$FastFood_k, BMI_analysis)
summary(lm3c)
lm3d <- lm(BMI_analysis$log_BMI~BMI_analysis$HouseholdIncome_k, BMI_analysis)
summary(lm3d)
lm3e <- lm(BMI_analysis$log_BMI~BMI_analysis$log_Weight+BMI_analysis$Age+
             BMI_analysis$FastFood_k+BMI_analysis$HouseholdIncome_k,
           BMI_analysis)
summary(lm3e)

## testing correlations between explanatory variables
cor(BMI_analysis[,c("log_Weight", "Age", "FastFood_k","HouseholdIncome_k")],use = "p")
