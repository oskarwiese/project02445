rm(list=ls())
setwd("/Users/ejer/Desktop/02445 project/project02445/project2")


load(file = "fosfor_data.RData")
data = Phosphorous ; rm(Phosphorous)
data$location <- as.factor(data$location)


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


expression(paste("Sampled values, ", mu, "=5, ", sigma, "=1")))