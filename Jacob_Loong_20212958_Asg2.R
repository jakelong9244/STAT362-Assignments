#Q1
A2_Q1 <- function(A){
  for (i in 1:length(A)){
    if (A[i] < 0){
      A[i] <- 0
    }
  }
  return(A)
}

#Q2
A2_Q2 <- function(A){
  return(sum(diag(A)))
}

#Q3
A2_Q3 <- function(data,colName){
  total <- 0
  for (i in 1:length(data[[colName]])){
    if (is.na(data[[colName]][i])){
      total <- total + 1
    }
  }
  return(total)
}

#Q4a
numb_of_sims <- 10000
prob_greater <- rep(0,numb_of_sims)
for (i in 1:numb_of_sims){
  X <- rnorm(1,0,2)
  Y <- rexp(1,3)
  prob_greater[i] <- (X > Y)
}
mean(prob_greater)

#Q4b
numb_of_sims <- 10000
expected_min <- rep(0,numb_of_sims)
min <- rep(0,numb_of_sims)
for (i in 1:numb_of_sims){
  X <- rnorm(1000,2,1)
  Y <- rexp(1000,2)
  min <- min(c(X,Y))
}
mean(min)


#Q5
x <- 1:100
S <- 0
for (i in x){
  if ((i %% 2) == 0){
    S <- S - i^2
  }
  else {
    S <- S + i^2
  }
}
print(S)

#Q6
library(tidyverse)

filter(mtcars, gear == 4)

#Q7
filter(mtcars, gear == 4 | cyl == 6)

#Q8
arrange(mtcars, wt)
print(arrange(mtcars,wt)[1,])
#Lightest Car is Lotus Europa
arrange(mtcars, desc(wt))
print(arrange(mtcars,desc(wt))[1,])
#Heaviest Car is Lincoln Continental

#Q9
library(nycflights13)

filter(flights, month == 1 | month == 11, arr_delay <= 10)

#Q10
may_deps <- flights %>% 
  filter(month==5) %>% 
  nrow
print(may_deps)
