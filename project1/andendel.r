rm(list=ls())
load(file = "armdata.RData")

raw_movement <- unlist(armdata, recursive = T)
coordinate <- rep( c(
  rep("x", 100),
  rep("y", 100),
  rep("z", 100)
),
1600
) 
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
model <- lm(position~xyz+repetition+person+experiment,data = df)
anova(model)

k=0
p_vals <- rep(NA,120)
for (i in 1:15){
  for (j in i:16){
    if (i != j){
      x <- subset.data.frame(df,df$person == 1 & df$experiment == i)$position
      y <- subset.data.frame(df,df$person == 1 & df$experiment == j)$position
      p_val <- t.test(x,y,paired = T)$p.value
      p_vals[i] <- p_val
      print(i)
    }}
}
p_vals
x <- subset.data.frame(df,df$person == 1 & df$experiment == 1)$position
y <- subset.data.frame(df,df$person == 1 & df$experiment == 16)$position
t.test(x,y,paired=T)$p.value
