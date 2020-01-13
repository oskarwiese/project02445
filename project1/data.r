rm(list=ls())
#setwd("/Users/ejer/Desktop/02445 project/project02445/project1")


load(file = "armdata.RData")
data <- armdata ; rm(armdata)

par(mfrow=c(2,5))

#For den første liste i dataen 
ploti = F ; 
for (i in 1:10){
x <- ((data[[1]])[1])[[1]][[i]]
if (ploti == T){
  plot(x,add=T)
}}

#Lidt test af dataen 
y <- 0
rm(x) & rm(y)
  x <- ((data[[6]])[1])[[1]][[1]] ; y <- ((data[[1]])[1])[[1]][[2]]
  par(mfrow=c(1,1))
  plot(x[,1],y[,1]) 

dim(x)
#Eksperimenter:
x[,1]
#Perons: 
x[,2]
#Repetitions:
x[,3]

