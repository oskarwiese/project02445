rm(list=ls())
setwd("/Users/ejer/Desktop/02445 project/project02445/project2")


load(file = "fosfor_data.RData")
data = Phosphorous ; rm(Phosphorous)
data$location <- as.factor(data$location)


# Linear fits for sjov og for at vise at dette ikke er et godt valg
fit1 <- lm(data$yield ~ log(data$DGT))
summary(fit1)

fit2 <- lm(data$yield ~ log(data$olsenP))
summary(fit2)


plot(data)
par(mfrow = c(1,2))
plot(log(data$DGT), data$yield, col = data$location, xlab = expression(paste("DGT [",mu, "g/L]")), ylab = "Yield [hkg/ha]")
curve(fit1$coefficients[1] + x * fit1$coefficients[2], add = T)
plot(log(data$olsenP), data$yield, col = data$location, xlab = "olsenP [mg/hg]", ylab = "Yield [hkg/ha]")
curve(fit2$coefficients[1] + x * fit2$coefficients[2], add = T)
plot(data$olsenP, data$DGT, col = data$location, xlab = "olsenP [mg/hg]", ylab = expression(paste("DGT [", mu, "g/L]")))


# Fit non-linear model til DGT og vis fit
modelDGT <- nls(yield ~ alfa * log(DGT)/(beta + log(DGT)), data = data, start = list(alfa = 90 , beta = 1))
coef(modelDGT)
plot(log(data$DGT), data$yield, col = data$location, xlab = expression(paste("DGT [",mu, "g/L]")), ylab = "Yield [hkg/ha]")
curve(coef(modelDGT)[1] * x/(coef(modelDGT)[2] + x), add= T)


# Fit non-linear model til olsenP og vis fit
modelolsenP <- nls(yield ~ alfa * log(olsenP)/(beta + log(olsenP)), data = data, start = list(alfa = 90 , beta = 1))
coef(modelolsenP)
plot(log(data$olsenP), data$yield, col = data$location, xlab = expression(paste("DGT [",mu, "g/L]")), ylab = "Yield [hkg/ha]")
curve(coef(modelolsenP)[1] * x/(coef(modelolsenP)[2] + x), add= T)


# Undersøg om der er en mængden af fosfor har indflydelse på yield
fit <- lm(data$yield ~ data$DGT + data$olsenP)
summary(fit)
anova(fit)
