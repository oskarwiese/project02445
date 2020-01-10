rm(list=ls())
n <- 40 ; lmao <- lm
X <- runif(n,min = -1,max=1)
Y <- 0.3*X+rnorm(n,0,sqrt(0.16))
plot(X,Y)
model1 <- lmao(Y~X)
abline(model1)

idx <- 1:n ; k<-1000 
slopes <- 1:k
for (i in 1:k){
  sample_idx <- sample(idx,n,replace=T)
  sx <- X[sample_idx]
  sy <- Y[sample_idx]
  model <- lm(sy~sx);
  slope <- as.numeric(model$coefficients[2])
  slopes[i] <- slope
}
hist(slopes)
CI_1 <- quantile(slopes,c(0.025,0.975)) 
CI_2 <- quantile(slopes,c(0.005,0.995))
CI_1;CI_2
CI_stan <-(c(0.33575+qt(0.05,38)*0.10818,0.33575-qt(0.05,38)*0.10818))
CI_stan
