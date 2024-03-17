set.seed(361)
k <- 0 #counter for accepted ones
i <- 0 #counter for iterations



## 1.
y <- numeric()
f1 <- function(n){
  while (k < n) {
    u1 <- runif(1)
    i <- i+1
    u2 <- runif(1) #random variate from g -> X
    if(u1 <= u2^2){
      #accept x
      k <- k+1
      y[k] <- u2
    }
  }
  hist(y,prob=T, main = "f(x)=3x^2")
  a <- seq(0,1,0.01)
  lines(a, 3*a^2, col = "red", lwd = 2.5)
  return(list(summary(y),k/i))
}
f1(10000)


## 2.
y <- c()
f2 <- function(n){
  while(k<n){
    U1 <- runif(1)
    i <- i+1
    U2 <- runif(1)
    if(U1 <= (256/27)*U2*(1-U2)^3){
      k <- k+1
      y[k] <- x
    }
  }
  hist(y, prob = T, main = "f(x) = 20x(1-x)^3")
  a <- seq(0,1,0.01)
  lines(a, 20*a*(1-a)^3, col ="red",lwd = 2.5)
  return(list(summary(y),k/i))
}
f2(10000)

#OR

alt_f2 <- function(n){
  x <- runif(n)
  y <- runif(n)
  #acceptance cond.
  s <- x[y<(256/27)*x*(1-x)^3] #from x, take the ones satisfaying the func.
  length(s)
  
  hist(s, prob = T, main = "f(x) = 20x(1-x)^3")
  a <- seq(0,1,0.01)
  lines(a, 20*a*(1-a)^3, col ="firebrick",lwd = 2)
  return(summary(s))
  
}
alt_f2(10000)

#OR


alt_f3 <- function(n){
  x <- runif(n)
  y <- runif(n)
  #acceptance cond.
  s <- x[y<(256/27)*x*(1-x)^3] #from x, take the ones satisfaying the func.
  length(s)
  
  plot(x,y,pch = '.', col = "lightblue",lwd = 3)
  points(s, y[y<(256/27)*x*(1-x)^3],pch = '.', col = "firebrick")
  summary(y)
}
alt_f3(10000)



## 3.

i <- 1:5
q <- 1/length(i)

# for acceptance variates
x <- numeric()
y <- numeric()

#for rejected ones
x_rej <- numeric()
y_rej <- numeric()

i <- 1 #initialization of index for accepted r.v.'s
j <- 1 ##initialization of index for rejected r.v.'s

set.seed(1)
f3 <- function(n, probs){
  constant <- max(probs)/(1/length(probs))
  cq <- constant * (1/length(probs))
  
  while(i <= n){
    k <- sample(1:length(probs), size = 1) #random number for q(k)
    u <- runif(1) #random number for comparison
    if(u <= probs[k]/cq){
      x[i] <- k
      y[i] <- u*cq
      i <- i+1
    } else {
      x_rej[j] <- k
      y_rej[j] <- k
      j <- j+1
    }
    
  }
  plot(x,y,type = "p", col = "yellow", ylim = c(0,1))
  points(x_rej, y_rej, col = "purple")
  table(x)/n #estimated probs.
}



n <- 10000
probs <- c(0.15,0.22, 0.33, 0.10, 0.20)
f3(n, probs)



## Q5
pi
f5 <- function(n){
  x <- runif(n)
  y<- runif(n)
  s <- x[y < sqrt(1-x^2)]
  length(s)
  
  hist(s, prob = T, main = "f(x) = (2/pi)*sqrt(1-x^2)")
  a <- seq(-1,1,0.01)
  lines(a, (2/pi)*sqrt(1-a^2), col = "green", lwd = 3)
  return(summary(s))
}
f5(10000)





set.seed(361)
y <- numeric()
i <- 0
k <- 0 

f6 <- function(n){
  while (k < n) {
    x <- runif(1)
    i <- i+1
    u <- runif(1)
    if(u <= sqrt(1 - x^2)){
      k <- k+1
      y[k] <- x
    }
  }
  hist(y, prob = T)
  a <- seq(0, 4/pi, 0.01)
  lines(a, 4/pi * sqrt(1 - a^2), col = "red", lwd = 3)
  return(list(summary(y), k/i))
}
f6(10000)





f7 <- function(n){
  while (k < n) {
    U1 <- runif(1)
    i <- i+1
    U2 <- runif(1)
    if (U1 < 23.095*U2^8*(1-U2)){
      k <- k+1
      y[k] <- U2
    }
  }
  hist(y, prob = T, main = "f(x) = 90X^8(1-X)")
  a <- seq(0,1, 0.01)
  lines(a, 90*a^8*(1-a), col = "red", lwd = 3)
  return(list(summary(y), k/i))
}
f7(10000)











