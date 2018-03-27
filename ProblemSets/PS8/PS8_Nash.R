install.packages("nloptr")

#4
set.seed(100)^1
x <- matrix(rnorm(100000*10), 100000)
x[,1] <- 1
x
eps <- (rnorm(10, mean=0, sd=0.5))
beta <- c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)
y <- (x*beta + eps)
y

#5
betaols <- ((crossprod(x))^1)%*%(crossprod(x,y))
betaols

#6
# set up a stepsize
alpha <- 0.0000003

# set up a number of iterations
maxiter <- 500000


objfun <- function(beta,y,X) {
  return ( sum((y-X%*%beta)^2) )
}


gradient <- function(beta,y,x) {
  return ( as.vector(-2*t(x)%*%(y-x%*%beta)) )
}


beta <- runif(dim(x)[2])


set.seed(100)^1

beta.All <- matrix("numeric",length(beta),maxiter)

iter  <- 1
beta0 <- 0*beta
while (norm(as.matrix(beta0)-as.matrix(beta))>1e-8) {
  beta0 <- beta
  beta <- beta0 - alpha*gradient(beta0,y,x)
  beta.All[,iter] <- beta
  if (iter%%10000==0) {
    print(beta)
  }
  iter <- iter+1
}

print(iter)
print(paste("The minimum of f(beta,y,x) is ", beta))



#7
library(nloptr)

objfun <- function(beta,y,x) {
  return ( crossprod(y-X%*%beta) 
}


gradient <- function(beta,y,x) {
  return ( as.vector(-2*t(x)%*%(y-x%*%beta)) )
}


beta0 <- runif(dim(x)[2]) 


options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e3)


result <- nloptr( x0=beta0,eval_f=objfun,eval_grad_f=gradient,opts=options,y=y,X=x)
print(result)

#8
library(nloptr)
objfun  <- function(theta,y,x) {
  
  beta    <- theta[1:(length(theta)-1)]
  sig     <- theta[length(theta)]
  
  loglike <- -sum( -.5*(log(2*pi*(sig^2)) + ((y-x%*%beta)/sig)^2) ) 
  return (loglike)
}

gradient <- function (theta ,y,x) {
  grad <- as. vector ( rep (0, length ( theta )))
  beta <- theta [1:( length ( theta ) -1)]
  sig <- theta [ length ( theta )]
  grad [1:( length ( theta ) -1)] <- -t(X)%*%(y - x%*% beta )/( sig ^2)
  grad [ length ( theta )] <- dim (X) [1] /sig - crossprod (y-x%*% beta )/( sig
                                                                            ^3)
  return (grad)
}


theta0 <- runif(dim(x)[2]+1)
theta0 <- append(as.vector(summary(lm(y~ x*beta + eps))$coefficients[,1]),runif(1))


options <- list("algorithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=1.0e-6,"maxeval"=1e4)


result <- nloptr( x0=theta0,eval_f=objfun,opts=options,y=y,x=x)
print(result)
betahat  <- result$solution[1:(length(result$solution)-1)]
sigmahat <- result$solution[length(result$solution)]
# Couldn't get this to work. nloptr package was not loading.

#9
easyway <- lm(y ~ x-1)
summary(easyway)
library(stargazer)
stargazer(easyway) # Even this won't work
