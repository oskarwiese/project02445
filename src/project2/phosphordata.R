rm(list=ls())
#setwd("/Users/ejer/Desktop/02445 project/project02445/src/project2")

#Forberedelse af data
load(file = "fosfor_data.Rdata")
data = Phosphorous ; rm(Phosphorous)
data$location <- as.factor(data$location)



# Plot af olsenP og DGT i forhold til yield
cols <- c("violet", "red", "deepskyblue3", "green", "deeppink2", "black", "gray", "darkgoldenrod", "purple")
colsrep <- rep(cols, each = 4)

par(mfrow = c(1,2))
plot(data$DGT, data$yield, col = colsrep, xlab = expression(paste("DGT [",mu, "g/L]")), ylab = "Yield [hkg/ha]", pch = 19)
legend("bottomright", legend=c("Field 1", "Field 2", "Field 3", "Field 4", "Field 5", "Field 6", "Field 7", "Field 8", "Field 9"),
       col=cols, cex=0.7, pch = 19)
plot(data$olsenP, data$yield, col = colsrep, xlab = "olsenP [mg/hg]", ylab = "Yield [hkg/ha]", pch = 19)



# Linear fits til data 
fit1 <- lm(data$yield ~ data$DGT)
summary(fit1) # r^2 = 0.4239

fit2 <- lm(data$yield ~ data$olsenP)
summary(fit2) # r^2 = 0.1188



# Plot med fit
par(mfrow = c(1,2))
plot(data$DGT, data$yield, col = data$location, xlab = expression(paste("DGT [",mu, "g/L]")), ylab = "Yield [hkg/ha]")
curve(fit1$coefficients[1] + x * fit1$coefficients[2], add = T)
legend("bottomright", legend=c(expression(paste( r^2, " = 0.4239"))),
       col=c("black"), cex=0.8)

plot(data$olsenP, data$yield, col = data$location, xlab = "olsenP [mg/hg]", ylab = "Yield [hkg/ha]")
curve(fit2$coefficients[1] + x * fit2$coefficients[2], add = T)
legend("bottomright", legend=c(expression(paste( r^2, " = 0.1188"))),
       col=c("black"), cex=0.8)



#Linear sammenhæng mellem olsenP og DGT
par(mfrow = c(1,1))
plot(data$olsenP, data$DGT, col = data$location, xlab = "olsenP [mg/hg]", ylab = expression(paste("DGT [", mu, "g/L]")))
cor(data$olsenP, data$DGT)



#Fit til michalis-Menten modellen
par(mfrow = c(1,2))

modelDGT <- nls(yield ~ alfa * DGT/(beta + DGT), data = data, start = list(alfa = 90 , beta = 1))
summary(modelDGT)
coef(modelDGT)
plot(data$DGT, data$yield, col = data$location, xlab = expression(paste("DGT [",mu, "g/L]")), ylab = "Yield [hkg/ha]")
curve(coef(modelDGT)[1] * x/(coef(modelDGT)[2] + x),from=2,to=170, add= T)
legend("bottomright", legend=c("MSE = 10.84"),
       col=c("black"), cex=0.8)

modelolsenP <- nls(yield ~ alfa * olsenP/(beta + olsenP), data = data, start = list(alfa = 90 , beta = 1))
summary(modelolsenP)
coef(modelolsenP)
plot(data$olsenP, data$yield, col = data$location, xlab = "olsenP [mg/hg]", ylab = "Yield [hkg/ha]")
curve(coef(modelolsenP)[1] * x/(coef(modelolsenP)[2] + x), from=1, to=10, add= T)
legend("bottomright", legend=c("MSE = 14.65"),
       col=c("black"), cex=0.8)



# Har fosfor en betydning for yield
fitDGT <- lm(data$yield ~ data$DGT+data$location)
anova(fitDGT)

fitolsenP <- lm(data$yield ~ data$olsenP+data$location)
anova(fitolsenP)
