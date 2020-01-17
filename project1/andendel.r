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
df
model <- lm(position~xyz+repetition+person+experiment,data = df)
anova(model)


par( mfrow=c(5,4))
par(mar=c(1,1,1,1))
for (t in 1:10){
  person_num <- t
  k=1
  p_vals <- c(rep(NA,120))
  for (i in 1:15){
    for (j in i:16){
      if (i != j){
        x <- subset.data.frame(df,df$person == person_num & df$experiment == i)$position
        y <- subset.data.frame(df,df$person == person_num & df$experiment == j)$position
        p_val <- t.test(x,y,paired = T)$p.value
        p_vals[k] <- p_val
        k = k +1 
      }}
  }
  plot(sort(p_vals))
  length(p_vals[p_vals < 0.05])
  adjust_p <- p.adjust(sort(p_vals),method = "BH")
  plot(adjust_p)
  length(adjust_p[adjust_p < 0.05])
}
?pbinom



# Not finnished 

df <- data.frame(matrix(ncol=304,nrow=100*16))
names. <- rep(NA, 304);
names.[1] <- "position" ; names.[2] <- "person"; names.[3] <- "repetition"; names.[4] <-"experiment"
for (k in 1:16){
  for (i in 1:300) {
    if (i <= 100) {
      names.[i+4] <- paste(c("x", as.character(i)), collapse = "");
    } else if (i <= 200) {
      names.[i+4] <- paste(c("y", as.character(i-100)), collapse = "");
    } else {
      names.[i+4] <- paste(c("z", as.character(i-200)), collapse = "");
    }
  }}
names.
names(df) <- names.;
k <- 1
for (l in 1:16){
  for (i in 1:10){
   for (j in 1:10){
      df[k, 2] <- i
      df[k, 3] <- j
      df[k, 4] <- l
      exp <- armdata[[l]]
      example <- exp[[i]][[j]]
      example <- as.vector(example)
      df[k,5:304] <- example 
      k <- k+1
  }
}}
df$person <- as.factor(df$person)
df$repetition <- as.factor(df$repetition)
df$experiment <- as.factor(df$experiment)
anova(lm(~df$experiment+df$person+df$repetition))


