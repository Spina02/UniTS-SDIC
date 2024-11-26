# ## Build.in functions in R to work with rvs ------------------------------------
# ?dnorm #pdf/pmf
# ?pnorm #cdf
# ?qnorm #quantile
# ?rnorm #random numbers generation
# 
# par(mfrow = c(1,3))
# xx <- rnorm(100)
# hist(xx, prob = T)
# curve(dnorm(x), col = "gold", add = T, lwd = 2, from = -4, to = 4)
# plot(sort(xx), cumsum(rep(1/length(xx), length(xx))), pch = 20)
# curve(pnorm(x), from = -4, to = 4, add = T,  col = "gold", lwd = 2)
# 
# p <- 0.25
# abline(h=p, lty=2, col="red")
# abline(v=qnorm(p), lty=2, col="red")
# plot(cumsum(rep(1/length(xx), length(xx))), sort(xx),  pch = 20)
# curve(qnorm(x), col = "gold", lwd = 2, add = T)
# 
# abline(v = p, lty=2, col="red")        # vertical line in corrispondence of the quantile (0.25)
# abline(h = qnorm(p), lty=2, col="red") # horizontal line in corrispondence of the quantile

#? --------------------------------------------------------------------------------

# ## qqplot normal vs student-t  -------------------------------------------------
# set.seed(123)
# par(mfrow=c(1,2))
# x <- rt(1000,2);
# qqnorm(x, pch = 16, main = "")
# qqline(x)
# 
# y <- rnorm(1000)
# qqnorm(y, pch = 16, main = "")
# qqline(y)

# ## Inversion sampling: Simulate a Normal(5,4) from a Unif
# par(mfrow=c(1,1))
# y <- runif(10^4); x <- qnorm(y, m = 5, s = 2) # s = standard deviation
# par(pty = "s",  cex = 0.8)
# qqnorm(x, pch = 16, main = "")
# qqline(x)


### EXERCISE -------------------------------------------------------------------
## Compute P(X = 0) for X ∼ Pois(3) --------------------------------------------
dpois(0, 3)

## Compute P(X = x) for X ∼ Pois(3), for x = 0,1,...,5 -------------------------
dpois(0:5, 3)

## Make a plot of the Poisson p.m.f. with lambda = 3 and for x=0,...20----------
plot(dpois(0:20, 3), type = "h", xlab = "x", ylab = "poisson distribution", main = "p.m.f.")

## Explore how the p.m.f. of a Poisson rv changes varying lambda ---------------
par(mfrow = c(2,3))
plot(dpois(0:40, 1), xlab = "x", ylab = "poisson distribution", main = "p.m.f., lambda = 1", type = "h")
plot(dpois(0:40, 2), xlab = "x", ylab = "poisson distribution", main = "p.m.f., lambda = 2", type = "h")
plot(dpois(0:40, 4), xlab = "x", ylab = "poisson distribution", main = "p.m.f., lambda = 3", type = "h")
plot(dpois(0:40, 8), xlab = "x", ylab = "poisson distribution", main = "p.m.f., lambda = 4", type = "h")
plot(dpois(0:40, 16), xlab = "x", ylab = "poisson distribution", main = "p.m.f., lambda = 5", type = "h")
plot(dpois(0:40, 20), xlab = "x", ylab = "poisson distribution", main = "p.m.f., lambda = 6", type = "h")

## Compare a random sample from a Poisson with the theoretical quantiles--------
par(mfrow = c(1, 1))
n <- 1000
x <- rpois(n, 3)
plot(ecdf(x))
points(sort(x), 1:n/n, type = "s")
points(0:n, ppois(0:n,3), col = "red", lwd = 2, type = "s")

# ##repeat the points above with another continuous rv of your choice
# par(mfrow = c(2, 2))
# 
# x1 <- rbinom(100, 3, 0.9)
# qqnorm(x1, main = "binomial")
# qqline(x1)
# 
# x2 <- rnorm(100, 3)
# qqnorm(x2, main = "normal")
# qqline(x2)
# 
# x3 <- rgamma(100, 3, 0.9)
# qqnorm(x3, main = "gamma")
# qqline(x3)
# 
# x4 <- rexp(100, 3)
# qqnorm(x4, main = "exp")
# qqline(x4)
# 
# 
# ## Inversion sampling: Simulate a Gamma(shape = 5, scale = 4) from a Unif
# ## compare the simulated data with the theoretical distribution using a qqplot
# 
par(mfrow = c(1,1))
y <- runif(10^4); x <- qgamma(y, 5, scale = 4)
par(pty = "s", cex = 0.8)
qqnorm(x, pch = 16, main = "")
qqline(x)