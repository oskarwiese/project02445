rm(list=ls())



library(ff)
library(MASS)


# Forberedelse af data
load(file = "armdata.RData")
names. <- rep(NA, 300);
for (i in 1:300) {
  if (i <= 100) {
    names.[i] <- paste(c("x", as.character(i)), collapse = "");
  } else if (i <= 200) {
    names.[i] <- paste(c("y", as.character(i-100)), collapse = "");
  } else {
    names.[i] <- paste(c("z", as.character(i-200)), collapse = "");
  }
}

coordinate<-rep(names.,1600)
raw_movement <- unlist(armdata, recursive = T)
repetition <- c()
person <- c()
experiment <- c()
for (i in 1:16){ #experiments
  print(i)
  for (j in 1:10){ #persons
    for (k in 1:10){ #repetitions
      experiment <- c(experiment, rep(i, 300))
      person <- c(person, rep(j, 300))
      repetition <- c(repetition, rep(k, 300))
    }
  }
}

df <- data.frame(
  "position" <- raw_movement,
  "xyz" <- as.factor(coordinate),
  "repetition" <- as.factor(repetition),
  "person" <- as.factor(person),
  "experiment" <- as.factor(experiment)
)

names(df) <- c("position","xyz","repetition","person","experiment")

  

# Beregning af p-værdier for de 300 xyz variable
index <- as.vector(df$xyz)

p_vals <- c(rep(NA,300))
for (i in 1:300){
      x <- subset.data.frame(df,df$xyz == index[i])
      model <- lm(x$position ~ x$experiment + x$person)
      an <- anova(model)
      p_vals[i] <- an$'Pr(>F)'[1]
}
p_vals <- sort(p_vals)
adjust_p <- p.adjust(p_vals, method="BH")



# Plot for størrelse af de 300 p-værdierne
par(mfrow = c(1,1))
length(adjust_p[adjust_p<0.05])/300 # 0.8666667
plot(adjust_p, xlab = "Index", ylab = "Adjusted p-value")
legend("topleft", legend = expression(paste("p-values under ", alpha, "= 0.8666667% ")))
curve(0.05 + 0 * x, pch = 2, from = -10,to = 310, col = "red", lty = 2, add = T)



# Plot residuals for at undersgøe normalfordelingsantagelsen for forskellige variable
par(mar = c(4.1, 2.1, 2.1, 2.1))
par(mfrow=c(4,4))
p_vals <- c(rep(NA,300))
for (i in round(seq(1,300,length = 8))){
  x <- subset.data.frame(df,df$xyz == index[i])
  model <- lm(x$position ~ x$experiment + x$person)
  an <- anova(model)
  p_vals[i] <- an$'Pr(>F)'[1]
  
  hist(model$residuals, main = NULL, xlab = paste(index[i], "model residuals"), prob = T)
  x <- model$residuals
  fit <- fitdistr(x, "normal")
  para <- fit$estimate
  curve(dnorm(x, para[1], para[2]), col = 2, add = TRUE)
  qqnorm(model$residuals, main = NULL)
  qqline(model$residuals) 
}
