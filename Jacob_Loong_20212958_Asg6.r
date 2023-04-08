library(boot)
library(tree)
library(randomForest)
library(FNN)

#Q1
set.seed(1)
n <- 1000
X <- rnorm(n, 0.01, 0.05)
Y <- 0.5 * X + rnorm(n, 0, 0.05)
data <- cbind(X, Y)

est_alpha <- function(data, index) {
  X <- data[index, 1]
  Y <- data[index, 2]
  (var(Y) - cov(X, Y)) / (var(X) + var(Y) - 2 * cov(X, Y))
}

boot(data, est_alpha, R = 1000)

#Q2
concrete <- read.csv("C:\\Users\\jakel\\OneDrive\\Documents\\.Uni Stuff\\.Winter 2023\\STAT 362\\.Assignments\\concrete.csv")
names(concrete)[1] <- "cement"
set.seed(2)
index <- sample(nrow(concrete), 700)
concrete_train <- concrete[index, ]
concrete_test <- concrete[-index, ]

concrete_tree <- tree(strength ~., data=concrete_train)
# MSE in test data
mean((concrete_test$strength - predict(concrete_tree, concrete_test))^2)

#Q3
plot(concrete_tree)
text(concrete_tree,pretty=0)

#Q4
concrete_fit <- lm(strength ~., data=concrete_train)
# MSE 
mean((concrete$strength - predict(concrete_fit, concrete_train))^2)

#Q5
concrete_rf <- randomForest(strength ~., data=concrete_train,
                            mtry = (ncol(concrete_train) - 1)/3, ntree = 1000, importance = TRUE)
# MSE 
mean((concrete$strength - predict(concrete_rf, concrete_train))^2)

#Q6
varImpPlot(concrete_rf)
# The "most" important feature is age

#Q7
concrete_train_n <- concrete_train
concrete_test_n <- concrete_test

train_min <- apply(concrete_train, 2, min)
train_max <- apply(concrete_train, 2, max)

for (i in 1:ncol(concrete_train)) {
  concrete_train_n[, i] <- (concrete_train[, i] - train_min[i]) / (train_max[i] - train_min[i]) 

  concrete_test_n[, i] <- (concrete_test[, i] - train_min[i]) / (train_max[i] - train_min[i]) 
}

knn_predicted <- knn.reg(train=concrete_train_n,test=concrete_test_n,y=concrete_train$strength,k=3)$pred

# MSE 
mean((knn_predicted - concrete_test$strength)^2)

#Q8
# k nearest neighbour regression gives the smallest test error
