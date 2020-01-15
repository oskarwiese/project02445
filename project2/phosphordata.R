rm(list=ls())
#setwd("/Users/ejer/Desktop/02445 project/project02445/project2")


load(file = "fosfor_data.Rdata")
data = Phosphorous ; rm(Phosphorous)
data$location <- as.factor(data$location)

data



# Linear fits for sjov og for at vise at dette ikke er et godt valg
fit1 <- lm(data$yield ~ log(data$DGT))
summary(fit1)

fit2 <- lm(data$yield ~ log(data$olsenP))
summary(fit2)


par(mfrow = c(1,1))
plot(data)
boxplot(data[,2:4], main = "Distribution of phosphorous")

par(mfrow = c(1,2))
plot(log(data$DGT), data$yield, col = data$location, xlab = expression(paste("DGT [",mu, "g/L]")), ylab = "Yield [hkg/ha]")
curve(fit1$coefficients[1] + x * fit1$coefficients[2], add = T)
plot(log(data$olsenP), data$yield, col = data$location, xlab = "olsenP [mg/hg]", ylab = "Yield [hkg/ha]")
curve(fit2$coefficients[1] + x * fit2$coefficients[2], add = T)

par(mfrow = c(1,1))
plot(data$olsenP, data$DGT, col = data$location, xlab = "olsenP [mg/hg]", ylab = expression(paste("DGT [", mu, "g/L]")))



par(mfrow = c(1,2))
# Fit non-linear model til DGT og vis fit
modelDGT <- nls(yield ~ alfa * DGT/(beta + DGT), data = data, start = list(alfa = 90 , beta = 1))
summary(modelDGT)
coef(modelDGT)
plot(data$DGT, data$yield, col = data$location, xlab = expression(paste("DGT [",mu, "g/L]")), ylab = "Yield [hkg/ha]")
curve(coef(modelDGT)[1] * x/(coef(modelDGT)[2] + x), add= T)


# Fit non-linear model til olsenP og vis fit
modelolsenP <- nls(yield ~ alfa * olsenP/(beta + olsenP), data = data, start = list(alfa = 90 , beta = 1))
summary(modelolsenP)
coef(modelolsenP)
plot(data$olsenP, data$yield, col = data$location, xlab = "olsenP [mg/hg]", ylab = "Yield [hkg/ha]")
curve(coef(modelolsenP)[1] * x/(coef(modelolsenP)[2] + x), add= T)


# Unders?g om DGT eller olsen er det bedste m?l for yield
fit <- lm(data$yield ~ data$DGT + data$olsenP)
anova(fit) # DGT er signifikant med 0.005087 og olsenP usignifikant med 0.489226


# Unders?g om m?ngden af fosfor har en signifikant betydning for yield
fitDGT <- lm(data$yield ~ data$DGT+data$location)
anova(fitDGT)

fitolsenP <- lm(data$yield ~ data$olsenP+data$location)
anova(fitolsenP)

summary(data$location)
par(mfrow=c(1,1))
plot(data$DGT,data$olsenP)


