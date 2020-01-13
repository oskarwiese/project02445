rm(list=ls())
#setwd("/Users/ejer/Desktop/02445 project/project02445/project1")


load(file = "armdata.RData")
exp_num <- 6
exp6 <- armdata[[exp_num]]
# Example er person 1, rep 1 ; SOM HER exp6[[person]][[rep]]
example <- exp6[[1]][[1]]
summary(example)
as.vector(example)
# Opretter en data fram, hvor vi har p , r, x ,y ,z

data <- data.frame(matrix(ncol=302,nrow=100))
numbers <- seq(1,300,1)
names <- c("person","replication",numbers)
names(data) <- names
k <- 1
for (i in 1:10){
  for (j in 1:10){
  data[k, 1] <- i
  data[k, 2] <- j
  example <- exp6[[i]][[j]]
  example <- as.vector(example)
  data[k,3:302] <- example 
  k <- k+1
  }}
data$person <- as.factor(data$person)
data$replication <- as.factor(data$replication)

# Nu har vi lært at plot med data frame ;( )
plot(as.numeric(data[1,3:102]),as.numeric(data[1,103:202]))


model <- glm(person~replication*as.numeric(data[,1]),data=data,family="binomial")
summary(model)
?glm

anovmodel <- lm(, data = data, family = "binomial")