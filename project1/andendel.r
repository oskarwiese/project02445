rm(list=ls())
library("ff")
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

  

index <- as.vector(df$xyz)

par(mar=c(1,1,2,2))
par(mfrow=c(4,5))
p_vals <- c(rep(NA,300))
for (i in 1:300){
      x <- subset.data.frame(df,df$xyz == index[298])
      model <- lm(x$position ~ x$experiment + x$person) ; hist(model$residuals)
      an <- anova(model)
      p_vals[i] <- an$'Pr(>F)'[1]
      
      if (i %% 30 == 0){
        hist(model$residuals)
        qqnorm(model$residuals)
        qqline(model$residuals) 
      }
} ; 
p_vals <- sort(p_vals) ;
adjust_p <- p.adjust(p_vals, method="BH") ; plot(adjust_p)
par(mfrow=c(1,2)) ; hist(model$residuals); qqnorm(model$residuals);qqline(model$residuals)
length(adjust_p[adjust_p<0.05])/300
?p.adjust
# Not finnished 


  

