### ========== Prob1 ==========
## Random variables

## Build.in functions in R to work with rvs ------------------------------------
?dnorm #pdf/pmf
?pnorm #cdf
?qnorm #quantile
?rnorm #random numbers generation

#other examples: dgamma, dbinom, dpois, dexp, dchisq, dt,...

## gamma-exp (using plot and lines) --------------------------------------------
xx <- seq(0, 10, l=1000)
plot(xx, dgamma(xx, 2, 2), xlab ="x", ylab ="f(x)", type ="l")
lines(xx, dgamma(xx, 2, 1), col = 2)
lines(xx, dgamma(xx, 2, .5), col = 3)
lines(xx, dgamma(xx, 1, .5), col = 4) 
lines(xx, dgamma(xx, 10, 1), col = 5) 

## beta-uniform (using curve)  -------------------------------------------------
curve(dbeta(x, 6, 2), xlab ="x", ylab ="f(x)", type ="l")
curve(dbeta(x, 1, 1), col = 2, add = T)
curve(dbeta(x, 2, 5), col = 3, add = T)
curve(dbeta(x, .5, .5), col = 4, add = T)

## qqplot normal vs student-t  -------------------------------------------------
set.seed(123)
par(mfrow=c(1,2))
x <- rt(1000,2);
qqnorm(x, pch = 16, main = "")
qqline(x)

y <- rnorm(1000)
qqnorm(y, pch = 16, main = "")
qqline(y)


### EXERCISE -------------------------------------------------------------------
## Compute P(X = 0) for X ∼ Pois(3) --------------------------------------------

## Compute P(X = x) for X ∼ Pois(3), for x = 0,1,...,5 -------------------------

## Make a plot of the Poisson p.m.f. with lambda = 3 and for x=0,...20----------


## Compare the Binomial and the Poisson distribution for large n and small p----


## Explore how the p.m.f. of a Poisson rv changes varying lambda


##repeat the points above with another continuous rv of your choice

## R lab: uniform transformation

x <- rnorm(10^5) ### simulate values from N(0,1) 
hist.scott(x, main = "") ### from MASS package distrib of simulated data
xx <- seq(min(x), max(x), l = 1000) #sequence to plot the theoretical curve
lines(xx, dnorm(xx), col = "red", lwd = 2) ### theoretical data

u <- pnorm(x) ### that's the uniform transformation: X~N(0,1)
hist.scott(u, prob = TRUE, main="")
segments(0, 1, 1, 1, col = 2, lwd = 2)
#equiv
curve(dbeta(x,1,1), lty=2, lwd=2, col ="blue", add = T) # unif
curve(dunif(x), lty=3, lwd=2, col ="green", add = T) # unif

## Quantiles
# choose the quantile order you want to highligh
p <- 0.2 #a number between 0 and 1
par(mfrow=c(2,1), mar = c(4,4,1,1))
plot(sort(x), pnorm(sort(x)), type = "l",  xlab="x",  ylab="Cdf") 
abline(h=p, lty=2, col="red")
abline(v=qnorm(p), lty=2, col="red")
curve(dnorm(x), from = min(xx), to = max(xx), xlab="x", ylab = "pdf")
abline(v=qnorm(p), lty=2, col="red")
# the quantile of order p is: 
qnorm(p)

# change the distribution and update the previous code


## Inversion sampling: Simulate a Normal(5,4) from a Unif
## qq plot: build-in function
par(mfrow=c(1,1))
u <- runif(10^4); y <- qnorm(u, m = 5, s = 2)
par(pty = "s",  cex = 0.8)
qqnorm(y, pch = 16, main = "")
qqline(y)

## make qq plot from scratch
set.seed(123)
par(mfrow=c(1,2))
n <- 11 #sample size
x <- rnorm(n) #simulate from a std normal
#the next line plot the empirical cumulative distribution function
plot(sort(x), (1:n-1/2)/n, ylim=c(0,1), xlim=c(-2.5,2.5)); #plot(ecdf(x)) 
curve(pnorm, from=-1.5, to=1.5, add=T) #add the theoretical cdf
qqnorm(x, pch = 16, main = "", xlim=c(-2.5,2.5)) #build in function
qqline(x)
# find  the points coordinates
points(qnorm(0.045), sort(x)[1], col = "red") #(1-1/2)/11=0.045
points(qnorm((2-1/2)/11), sort(x)[2], col = "red") #(1-1/2)/11=0.045
#...
