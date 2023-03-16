#Q1a
my_LS <- function(X, Y) {
  return(as.vector(solve(t(X) %*% X) %*% t(X) %*% Y))
}

#Q1b
my_ridge <- function(X, Y, lambda) {
  I <- t(X) %*% X
  I[,] <- 0
  diag(I) <- 1
  return(as.vector(solve(t(X) %*% X + lambda * I) %*% t(X) %*% Y))
}

#Q2a
my_sum <- function(n, m) {
  sum <- 0
  for (x in 1:n){
    for (y in 1:m){
      z <- (x^2 * y)/(x + y)
      sum <- sum + z
    }
  }
  return(sum)
}

#Q2b
my_sum2 <- function(n, m) {
  A <- matrix(seq(1:n), nrow = n, ncol = m)
  B <- matrix(seq(1:m), nrow = n, ncol = m)
  C <- (A^2 * B)/(A + B)
  return(sum(C))
}

#Q3
trunc(runif(1, 1, 6))

#Q4
rbinom(10, 1, 0.6)

#Q5
l_p <- function(x, p) {
  return(sum(abs(x)^p)^(1/p))
}

#Q6
sum(v*v%%2)

#Q7
plot(1:10,dgeom(1:10,0.3),type="h")

#Q8
numb_of_sims <- 10000
price_below <- rep(0, numb_of_sims)
price_above <- rep(0,numb_of_sims)
price_both <- rep(0,numb_of_sims)
  
for (i in 1:numb_of_sims) {
  price <- 100 * exp(cumsum(rnorm(40, mean = 0.0002, sd = 0.015)))
  price_below[i] <- min(price[1:20]) < 95
  price_above[i] <- max(price) > 101

  price_both[i] <- price_below[i] & price_above[i]
}
mean(price_both)

#Q9
x <- sample(c(-1.5,1),size=100,replace=TRUE,prob=c(0.3,0.7))
games <- 1:100
plot(games,cumsum(x),type="l")

#Q10
#1. Clear Console:                          Ctrl+L
#2. Move Cursor to Console:                 Ctrl+2
#3. Interrupt Currently Executing Command:  Esc
#4. Move Cursor to Source Editor:           Ctrl+1
#5. Save Active Document:                   Ctrl+S
#6. Run Current Line/Selection:             Ctrl+Enter
#7. Undo:                                   Ctrl+Z
#8. Cut:                                    Ctrl+X
#9. Copy:                                   Ctrl+C
#10.Paste:                                  Ctrl+V