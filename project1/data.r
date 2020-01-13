rm(list=ls())

require(class)
require(tree)
#setwd("/Users/ejer/Desktop/02445 project/project02445/project1")


install.packages("plot3D")
#scatter3D(as.numeric(df[1,3:102]), as.numeric(df[1,103:202]), as.numeric(df[1,203:302]), theta = 35, phi = 50)
plot(as.numeric(df[1,3:102]), as.numeric(df[1,203:302]), type = "l", xlab = "Left-right movement", ylab = "Up-down movement", main = "Comparison of arm movement of two people")
for(i in 2:10){
  lines(as.numeric(df[i,3:102]), as.numeric(df[i,203:302]), type = "l")
}

for(i in 71:80){
  lines(as.numeric(df[i,3:102]), as.numeric(df[i,203:302]), type = "l", col = "red", lty=2)
}
legend("bottom", legend=c("Person 1", "Person 2"),
       col=c("black", "red"), lty=1:2, cex=0.8)


boxplot(df[,3:102], main = "Distribution of left-right movement")
boxplot(df[,103:202])
boxplot(df[,203:302])


load(file = "armdata.RData")
exp_num <- 6
exp6 <- armdata[[exp_num]]
# Example er person 1, rep 1 ; SOM HER exp6[[person]][[rep]]
example <- exp6[[1]][[1]]
summary(example)
as.vector(example)
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

for (i in 1:100){
  #sample <- sample.int(n = nrow(df), size = floor(splitting[i]*nrow(df)), replace = F);
  train <- df[-i, ]
  test  <- df[i, ]
  tree_model  <- tree(person ~ . -repetition, data=train)
  pred <- predict(tree_model,test,type="class") ;
  if (pred == as.numeric(test[1])){
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
for (i in 1:100){
  train <- df[-i, ]
  test  <- df[i, ]
  model_knn <- knn(train,test,cl=train[,1],k=4) ;
  if (model_knn == as.numeric(test[1])){
    j = j + 1
  }}
accuracy_knn = j /100 ; accuracy_knn

