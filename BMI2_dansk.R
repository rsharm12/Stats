
################################
## Indlæs data

## Indlæs BMI2_data.csv filen med data
D <- read.table("BMI2_data.csv", header=TRUE, sep=",", as.is=TRUE)




################################
## Bestem den del af datasættet der vedr. de 7 sidste observationer
BMI_test <- subset(D,Obs.no >= 841)

## Bestem datasæt med de 840 første observationer - analyse datasættet!
BMI_analysis <- subset(D,Obs.no <= 840)

#########################
## d)
## Multipel regressionsanalyse
lm1 <- lm(log_BMI~log_Weight+Age+FastFood_k+HouseholdIncome_k,
          BMI_analysis)
summary(lm1)

##########################################
## e)
##Modelkontrol
par(mfrow=c(1,1))

## observerede respons vs. fittet: Antagelsen om linearitet
plot(BMI_analysis$log_BMI, lm1$fitted.values, xlab='log_BMI',
       ylab='Fitted log_BMI', pch=16, cex.lab=0.8)
title(main = list("Vurdering af Linearitetsantagelsen", cex=0.9))

## residual vs. hver af de forklarende variable: Antagelse om
## linearitet
plot(BMI_analysis$FORKLARENDE VARIABEL, lm1$residuals,
        xlab='INDSÆT TEKST', ylab='Residuals', pch=16, cex.lab=0.8)
title(main = list("Vurdering af linearitets antagelsen", cex=0.9))

## residuals vs. fittet: Antagelsen om konstant varians
plot(lm1$fitted.values, lm1$residuals, xlab='Fitted values',
     ylab='Residuals', pch=16, cex.lab=0.8)
title(main = list("Vurdering af Varians homogenitet", cex=0.9))

# normal plot of residuals: Normalfordelingsantagelsen
qqnorm(lm1$residuals, ylab='Residuals', xlab='z-scores',
       main='Vurdering af Normalfordelingsantagelse', cex.lab=0.9,
       pch=16, cex.main=0.9)
qqline(lm1$residuals)

################################
## R kommando der bestemmer konfidensintervaller for parametrene
confint(lm1,level=0.95)

###############################
## i)
## Prediktion og Prediktionsinterval for prediktionen bestemmes
pred <- predict(ANGIV MODEL NAVN, newdata=BMI_test, interval="prediction",
              level=0.95)
as.data.frame(cbind(Obs.no=BMI_test$Obs.no, log_BMI=BMI_test$log_BMI, pred))

#########################
## j)
##############################
## Undersøgelse af betydningen af kollinearitets problemer
Lm3a <- lm(log_BMI~log_Weight, BMI_analysis)
summary(lm3a)
lm3b <- lm(log_BMI~Age, BMI_analysis)
summary(lm3b)
lm3c <- lm(log_BMI~FastFood_k, BMI_analysis)
summary(lm3c)
lm3d <- lm(log_BMI~HouseholdIncome_k, BMI_analysis)
summary(lm3d)
lm3e <- lm(log_BMI~log_Weight+Age+FastFood_k+HouseholdIncome_k,
           BMI_analysis)
summary(lm3e)
