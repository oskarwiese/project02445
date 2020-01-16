rm(list=ls())

set.seed(59)
library(class)
library(tree)
#setwd("/Users/ejer/Desktop/02445 project/project02445/project1")


load(file = "armdata.RData")
exp_num <- 6
exp6 <- armdata[[exp_num]]
# Example er person 1, rep 1 ; SOM HER exp6[[person]][[rep]]
example <- exp6[[1]][[1]]
summary(example)
#as.vector(example)
# Opretter en dataframe, hvor vi har p , r, x ,y ,z
df <- data.frame(matrix(ncol=302,nrow=100))
names. <- rep(NA, 302);
names.[1] <- "person"; names.[2] <- "repetition";
for (i in 1:300) {
  if (i <= 100) {
    names.[i+2] <- paste(c("x", as.character(i)), collapse = "");
  } else if (i <= 200) {
    names.[i+2] <- paste(c("y", as.character(i-100)), collapse = "");
  } else {
    names.[i+2] <- paste(c("z", as.character(i-200)), collapse = "");
  }
}
names(df) <- names.;
k <- 1
for (i in 1:10){
  for (j in 1:10){
    df[k, 1] <- i
    df[k, 2] <- j
    example <- exp6[[i]][[j]]
    example <- as.vector(example)
    df[k,3:302] <- example 
    k <- k+1
  }
}
df$person <- as.factor(df$person)
df$repetition <- as.factor(df$repetition)
k = 0
pred_tree <- rep(NA, 100)
for (i in 1:100){
  #sample <- sample.int(n = nrow(df), size = floor(splitting[i]*nrow(df)), replace = F);
  train <- df[-i, ]
  test  <- df[i, ]
  tree_model  <- tree(person ~ . -repetition, data=train)
  model_tree <- predict(tree_model,test,type="class") ;
  pred_tree[i] <- model_tree
  if (model_tree == as.numeric(test[1])){
    k = k + 1
  }}
accuracy_tree <- k / 100 ; accuracy_tree

# Nu har vi lært at plot med data frame ;( )
#plot(as.numeric(df[1,3:102]),as.numeric(df[1,103:202]))
tree_model  <- tree(person ~ . -repetition, data=train)
plot(tree_model)
text(tree_model)
#summary(tree_model)

# KNN 

#Modellen opskrives som følgende: model_knn <- knn(train,test,cl=train[,1],k=10)
j=0
pred_knn <- rep(NA, 100)
test_val <- rep(NA,100)
for (i in 1:100){
  train <- df[-i, ]
  test  <- df[i, ]
  model_knn <- knn(train,test,cl=train[,1],k=3)
  test_val[i] <- as.numeric(test[1])
  pred_knn[i] <- model_knn
  if (model_knn == as.numeric(test[1])){
    j = j + 1
  }}
accuracy_knn = j /100 ; accuracy_knn

#Lidt pca 

pca <- prcomp(df[,3:302], center = T, scale. = T)
s<-summary(pca)
par(mfrow=c(1,1))
plot(s$importance[3,1:15],xlab = "Principle Component", ylab = "Variance Explained")
curve(0.9+0*x,pch=2,from=0,to=16,col="red",lty=2,add=T)
#install.packages("remotes")
#library(remotes)
#remotes::install_github('vqv/ggbiplot')
library(ggbiplot)
#remotes::install_github('vqv/ggbiplot')
ggbiplot(pca,labels = rownames(df[,3:302]), var.axes = F)
ggbiplot(pca, choices = 3:4, labels = rownames(df[,3:302]) , var.axes=F)

df[1:10,]



#install.packages("plot3D")
library(plot3D)

temp1 <- unlist(df[1:10,3:102])
temp2 <- unlist(df[1:10,103:202])
temp3 <- unlist(df[1:10,203:302])

par(mfrow = c(1,1))
default <- par()$mar
par(mar = c(rep(0.4,4)))
scatter3D(temp1, temp2, temp3, theta = 220, phi = 10, xlab = "Left-right", ylab = "Back-forth", zlab = "Up-down", col = rainbow(1270), colvar = NULL, pch = 16, cex = 0.6, bty = "b2")
par(mar = default)


par(mfrow = c(1,1))
plot(as.numeric(df[1,3:102]), as.numeric(df[1,203:302]), type = "l", xlab = "Left-right movement", ylab = "Up-down movement", main = "Comparison of arm movement of two people")
for(i in 2:10){
  lines(as.numeric(df[i,3:102]), as.numeric(df[i,203:302]), type = "l")
}

for(i in 71:80){
  lines(as.numeric(df[i,3:102]), as.numeric(df[i,203:302]), type = "l", col = "red", lty=2)
}
legend("bottom", legend=c("Person 1", "Person 2"),
       col=c("black", "red"), lty=1:2, cex=0.8)


par(mfrow = c(1,1))
boxplot(df[,3:102])
boxplot(df[,103:202], main = "Distributions of movement")
boxplot(df[,203:302])


# McNemar's test p? KNN og decision tree

wilcox.test(pred_knn, pred_tree, paired=TRUE)

par(mfrow = c(1,3))
hist(pred_knn, breaks = seq(0,10,1), ylim = c(0,18))
hist(pred_tree, breaks = seq(0,10,1), ylim = c(0,18))
hist(test_val, breaks = seq(0,10,1), ylim = c(0,18))

# En anden m?de at implementere mcnemar, der m?ske ogs? er korrekt

n <- rep(NA, 4)
alltrue <- pred_knn == test_val & pred_tree == test_val
n[1] <- sum(alltrue, na.rm = T)
onetrue <- pred_knn == test_val & pred_tree != test_val
n[2] <- sum(onetrue, na.rm = T)
othertrue <- pred_knn != test_val & pred_tree == test_val
n[3] <- sum(othertrue, na.rm = T)
notrue <- pred_knn != test_val & pred_tree != test_val
n[4] <- sum(notrue, na.rm = T)
N <- 100

theta <- (n[2] - n[3]) / N ; theta
Q <- (N^2 * (N + 1) * (theta + 1) * (1 - theta)) / (N * (n[2] + n[3]) - (n[2] - n[3])^2)
p <- ((theta + 1) * (Q - 1)) / 2
q <- ((1 - theta) * (Q - 1)) / 2

lower <- 2 * qbeta(0.025, p, q)-1
upper <- 2 * qbeta(0.975, p, q)-1
c(lower, theta, upper)

2 * pbinom(min(n[2],n[3]),n[2]+n[3],1/2)

