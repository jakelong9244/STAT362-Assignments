#Q1
#WIP
library(ggpubr)
library(tidyverse)

b <- function(x) {
  (0.5)*(ifelse(abs(x)<=1,1,0))
}

g <-function(x){
  (1/sqrt(2*pi))*exp((-x^2)/2)
}

e <- function(x){
  (3/4)*(1-x^2)*(ifelse(abs(x)<=1,1,0))
}

t <- function(x){
  (70/81)*(1-abs(x)^3)^3*(ifelse(abs(x)<=1,1,0))
}

boxcar <- ggplot(data=data.frame(x=c(-4,4)),mapping=aes(x=x))+
  stat_function(fun=b) +
  ylim(0,0.5) +
  ggtitle("Boxcar")
gaussian <- ggplot(data=data.frame(x=c(-4,4)),mapping=aes(x=x))+
  stat_function(fun=g) +
  ylim(0,0.4)+
  ggtitle("Gaussian")
epan <- ggplot(data=data.frame(x=c(-4,4)),mapping=aes(x=x))+
  stat_function(fun=e) +
  ylim(0,0.8)+
  ggtitle("Epanechinikov")
tricube <- ggplot(data=data.frame(x=c(-4,4)),mapping=aes(x=x))+
  stat_function(fun=t) +
  ylim(0,1)+
  ggtitle("Tricube")

ggarrange(boxcar,gaussian,epan,tricube)

#Q2
ribbons <- c(171.6,191.8,178.3,184.9,189.1)
t.test(x=ribbons,alternative="l",mu=185)

#Q3
traffic <- c(142600,167800,136500,108300,126400,133700,162000,149400)
t.test(x=traffic,conf.level=0.95)

#Q4
flu <- matrix(data=c(35,365),ncol=2)
prop.test(x=flu,n=100,conf.level=0.95)

#Q5
detergentA <- c( 232, 260, 197 )
shoppers <- c( 400, 500, 400 )
prop.test(detergentA,shoppers)

#Q6
#Get the data from the datasets
iris_train <- read.csv("iris_train.csv")
iris_test <- read.csv("iris_test.csv")

#Labels
iris_train_labels <- iris_train$Species
iris_test_labels <- iris_test$Species

#Features
iris_train <- iris_train[,-5]
iris_test <- iris_test[,-5]

iris_train_n <- iris_train
iris_test_n <- iris_test

train_min <- apply(iris_train, 2, min)
train_max <- apply(iris_train, 2, max)

for (i in 1:ncol(iris_train)) {
  iris_train_n[, i] <- (iris_train[, i] - train_min[i]) / (train_max[i] - train_min[i]) 

  iris_test_n[, i] <- (iris_test[, i] - train_min[i]) / (train_max[i] - train_min[i]) 
}

#Q7a
library(class)
knn_predicted <- knn(train = iris_train_n, test = iris_test_n, 
                     cl = iris_train_labels, k = 3)

#Q7b
performance <- table(iris_test_labels,knn_predicted)
accuracy <- (sum(diag(performance)))/(sum(performance))
print(accuracy)

#Q8
#when "prob" is true, the proportion of the votes for the winning class are returned as prob
knn_predicted <- knn(train = iris_train_n, test = iris_test_n, 
                     cl = iris_train_labels, k = 3, prob=TRUE)

