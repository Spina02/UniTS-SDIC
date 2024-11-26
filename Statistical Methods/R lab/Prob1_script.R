## Build.in functions in R to work with rvs ------------------------------------
?dnorm #pdf/pmf
?pnorm #cdf
?qnorm #quantile
?rnorm #random numbers generation

## pdf, cdf, quantile f ----

# theoretical
curve(dnorm(x), col = "gold", lwd = 2, from = -4, to = 4)
curve(pnorm(x), from = -4, to = 4, col = "gold", lwd = 2)
curve(qnorm(x), col = "gold", lwd = 2)

# empirical 
par(mfrow = c(2,3))
xx <- rnorm(100)
hist(xx, prob = T)
plot(sort(xx), cumsum(rep(1/length(xx), length(xx))), pch = 20)
plot(cumsum(rep(1/length(xx), length(xx))), sort(xx), pch = 20)

par(mfrow = c(1,3))
xx <- rnorm(100)
hist(xx, prob = T)
curve(dnorm(x), col = "gold", add = T, lwd = 2, from = -4, to = 4)
plot(sort(xx), cumsum(rep(1/length(xx), length(xx))), pch = 20)
curve(pnorm(x), from = -4, to = 4, add = T,  col = "gold", lwd = 2)
p <- 0.25
abline(h=p, lty=2, col="red")
abline(v=qnorm(p), lty=2, col="red")

plot(cumsum(rep(1/length(xx), length(xx))), sort(xx),  pch = 20)
curve(qnorm(x), col = "gold", lwd = 2, add = T)
#abline(..., lty=2, col="red")
#abline(..., lty=2, col="red")


## qqplot normal vs student-t  -------------------------------------------------
set.seed(123)
par(mfrow=c(1,2))
x <- rt(1000,2);
qqnorm(x, pch = 16, main = "")
qqline(x)

y <- rnorm(1000)
qqnorm(y, pch = 16, main = "")
qqline(y)


## Inversion sampling: Simulate a Normal(5,4) from a Unif
par(mfrow=c(1,1))
y <- runif(10^4); x <- qnorm(y, m = 5, s = 2) # s = standard deviation
par(pty = "s",  cex = 0.8)
qqnorm(x, pch = 16, main = "")
qqline(x)


### EXERCISE -------------------------------------------------------------------
## Compute P(X = 0) for X ∼ Pois(3) --------------------------------------------

## Compute P(X = x) for X ∼ Pois(3), for x = 0,1,...,5 -------------------------

## Make a plot of the Poisson p.m.f. with lambda = 3 and for x=0,...20----------

## Explore how the p.m.f. of a Poisson rv changes varying lambda ---------------

## Compare a random sample from a Poisson with the theoretical quantiles--------

##repeat the points above with another continuous rv of your choice

## Inversion sampling: Simulate a Gamma(shape = 5, scale = 4) from a Unif
## compare the simulated data with the theoretical distribution using a qqplot
