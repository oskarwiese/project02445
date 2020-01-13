rm(list=ls())
setwd("/Users/ejer/Desktop/02445 project/project02445/project2")


load(file = "fosfor_data.RData")
data = Phosphorous ; rm(Phosphorous)
data$location <- as.factor(data$location)


plot(data)
par(mfrow = c(1,2))
plot(data$DGT, data$yield, col = data$location, xlab = "DGT [mikrog/L]", ylab = "Yield [hkg/ha]")
plot(data$olsenP, data$yield, col = data$location, xlab = "olsenP [mg/hg]", ylab = "Yield [hkg/ha]")
plot(data$olsenP, data$DGT, col = data$location, xlab = "olsenP [mg/hg]", ylab = "DGT [mikrog/L]")


fit1 <- lm(data$yield ~ data$DGT)
summary(fit1)

fit2 <- lm(data$yield ~ data$olsenP)
summary(fit2)

curve(fit1$coefficients[1] + x * fit1$coefficients[2], add = T)
curve(fit2$coefficients[1] + x * fit2$coefficients[2], add = T)
