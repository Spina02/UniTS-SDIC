#################################
# R code for LAB1 - 08/10/2024  #
#                               #
# ! This is the pre-LAB version # 
# You must replace XXX properly # 
#################################

## Approximation with CLT: Example ----
# Waterpolo match: 

# 1) Set the needed information (provided in the slides)
p <- 0.5
q <- 0.7
n <- m <- 20

# 2) obtain mean, variance and standard deviation of W
mW <- n*p - m*q
varW <- n*p*(1-p) + m*q*(1-q)
sdW <- sqrt(varW)

# 3) compute the probability of interest: P(X > Y) = P(W > 0) = ? 

PWin_P <- pnorm(0, mean = mW, sd = sdW, lower.tail = FALSE)
PWin_P

# Visualisation of the pmf $X$ (red) and $Y$ (blue) 
# and the pdf of $W=X-Y$
curve(dnorm(x, mW, sdW), xlim = c(-12, 20), ylim = c(0, 0.3),
      xlab = "", ylab = "", cex.lab = 1.25)
points(0 : n, dbinom(0 : n, n, p), pch = 21, bg = 1, col = "blue")
points(0 : m, dbinom(0 : m, m, q), pch = 21, bg = 2)
segments(x0 = 0, y0 = -0.01, x1 = 0, y1 = dnorm(0, mW, sdW), lwd = 2)
text(14.25, 0.22, "Y", cex = 2, col = 2)
text(10, 0.2, "X", cex = 2, col = "blue")
text(-4, 0.15, "X - Y", cex = 2, col = 1)

# Obtain the result applying the continuity correction
PWin_P_cc <- pnorm(0.5, mean = mW, 
                   sd = sdW, 
                   lower.tail = FALSE)

# Compare
PWin_P_cc
PWin_P # Without c.c.

# Obtain the exact result: this is a naive R implementation
Px <- dbinom(0 : n, n, p = p)  # Pmf X
Py <- dbinom(0: m, m, p = q)   # Pmf Y
Pw <- rep(NA, 2 * n + 1)       # Pmf W

count <- 1          # Counter 1          
count2 <- 2 * n + 1 # Counter 2
for(i in 0 : n){
  idx1 <- 1 : (i + 1)
  idx2 <- (n + 1 - i) : (n + 1)
  if(i == n){
    Pw[count] <- sum(Px[idx1] * Py[idx2])  
  } else { 
    Pw[count] <- sum(Px[idx1] * Py[idx2]) 
    Pw[count2] <- sum(Px[idx2] * Py[idx1])
  }
  count <- count + 1
  count2 <- count2 - 1
}
sum(Pw) # check whether the probability sum up to 1

sum(Pw[(n+2) : length(Pw)]) # P(W > 0)


# To compare the effect of n on the quality of the approximation
# it is smarter to build a function 
PMFw <- function(n, m, p, q){
 Px <- dbinom(0 : n, n, p = p) 
 Py <- dbinom(0: m, m, p = q) 
 Pw <- rep(NA, 2 * n + 1)

 count <- 1; count2 <- 2 * n + 1
 for(i in 0 : n){
   idx1 <- 1 : (i + 1)
   idx2 <- (n + 1 - i) : (n + 1)
   if(i == n){
     Pw[count] <- sum(Px[idx1] * Py[idx2])  
   } else { 
     Pw[count] <- sum(Px[idx1] * Py[idx2]) 
     Pw[count2] <- sum(Px[idx2] * Py[idx1])
   }
   count <- count + 1
   count2 <- count2 - 1
 }
 return(Pw)
}

p <- 0.5 
q <- 0.7 

# Case lower n
n <- m <- 5 

mW <-  p * n - q * m 
varW <- n * p * (1 - p) + m * q * (1 - q) 
sdW <- sqrt(varW) 

PWin_P <- pnorm(0, mean = mW, sd = sdW,  lower.tail = FALSE)
PWin_P_cc <- pnorm(0.5, mean = mW, sd = sdW,  lower.tail = FALSE)


Pw <-PMFw(n, m, p, q)
sum(Pw[(n + 2) : length(Pw)]) # P(W>0 )
PWin_P                        # appr. P(W>0 )
PWin_P_cc                     # appr. with c.c. P(W>0 )


plot(-n : m, Pw, xlab = "w", col = "red", cex.axis = 1.5, cex= 2)
curve(dnorm(x, mW, sdW), add = TRUE)

# Case higher n
n <- m <- 100

mW <-  p * n - q * m ### XXX
varW <- n * p * (1 - p) + m * q * (1 - q) # XXX
sdW <- sqrt(varW) # XXX

PWin_P <- pnorm(0, mean = mW, sd = sdW,  lower.tail = FALSE)
PWin_P_cc <- pnorm(0.5, mean = mW, sd = sdW,  lower.tail = FALSE)


Pw <- PMFw(n, m, p, q)
sum(Pw[(n + 2) : length(Pw)]) # P(W>0 )
PWin_P                        # appr. P(W>0 )
PWin_P_cc                     # appr. with c.c. P(W>0 )


plot(-n : m, Pw, xlab = "w", col = "red", cex.axis = 1.5, cex= 2)
curve(dnorm(x, mW, sdW), add = TRUE)  


## Bivariate normal distribution ----
 
# Write a function for obtaining the value of the pdf 
# of a bivariate normal distribution with mean vector
# elements equal to zero and unity variances 

NBiv <- function(XXX){
  den <- XXX
  num <- XXX
  return(num/den)
}

# 3D plots
xx <- yy <- seq(-3, 3, 0.1)
z <- outer(xx, yy, NBiv, rho = 0.5)
par(mfrow = c(1, 2))
persp(xx, yy, z, xlab = "x", ylab = "y", zlab = "f(x,y)", cex.lab = 3)
persp(xx, yy, z, theta = 30, phi = 30, xlab ="x", ylab = "y", zlab = "f(x,y)", cex.lab= 3)

# Contour level plots (varying rho)
rho <- c(-0.5, 0, 0.9)
z <- list()
for(j in 1:3) z[[j]] <- outer(xx, yy, NBiv, rho = rho[j])
par(mfrow = c(1, 3))
for(j in 1:3) contour(xx, yy, z[[j]], main = paste0("rho = ", rho[j]), 
                      cex.main = 3, cex.axis = 3, labcex = 2)


# Install the mvtnorm package and load it 
#install.packages("mvtnorm")
library(mvtnorm)

# Biometric example
# Set the values of mean vector, variances and correlations 
mu_h <- 176
mu_w <- 85.52 
sd_h <- 7 
sd_w <- 14.24
rho <- 0.47

# Build the covariance matrix 
cov <- rho * sd_h * sd_w
Sigma <- matrix(c(sd_h^2, cov, cov, sd_w^2), 2,2, byrow=T)
X <- rmvnorm(1000, c(mu_h, mu_w), Sigma)

# Plot the marginal distributions
par(mfrow = c(1,2))
hist(X[,1], prob = TRUE, breaks = 30, cex.lab = 2)
curve(dnorm(x, mu_h, sd_h), col = "red", add=TRUE)
hist(X[,2], prob = TRUE, breaks = 30)
curve(dnorm(x, mu_w, sd_w), col = "red", add=TRUE)

# Get quantities for the conditional distribution
x1.fix <- 180
muw_c <- mu_w + rho * (sd_w/sd_h) * (x1.fix - mu_h)  
sdw_c <- sqrt(sd_w^2*(1-rho^2))
c(sd_w, sdw_c)

x2.fix <- 50
muh_c <- mu_h + rho * (sd_h/sd_w) * (x2.fix - mu_w)  
sdh_c <- sqrt(sd_h^2*(1-rho^2))
c(sd_h, sdh_c)

# Plot the conditional distributions
par(mfrow = c(1,2))
curve(dnorm(x, muw_c, sdw_c), from = 50, to = 130, 
      cex.lab = 2, cex.axis = 2, xlab = "x2")
curve(dnorm(x, muh_c, sdh_c), from = 140, to = 200, 
      cex.lab = 2, cex.axis = 2, xlab = "x1")

# Scatterplot and contour plot 
par(mfrow=c(1,1))
xx <- seq(min(X), max(X), length.out = 500)
yy <- seq(min(X), max(X), length.out = 500)
zz <- outer(X = xx, Y = yy, FUN = function(x,y) 
  dmvnorm(cbind(x,y), mean = c(mu_h, mu_w), sigma = Sigma))
plot(X[,1], X[,2], xlab = "x1", ylab = "x2", cex.axis = 2, cex.lab = 2)
contour(xx, yy, zz, add = TRUE, col = "red", nlevels = 20)

## Extra: Inverse sampling method ----

# Generating from the exponential distribution
theta <- 2
R <- 1000
x <- -log(runif(R))/theta
hist(x, prob = T, breaks = 30, cex.main = 2, cex.axis = 2, ylim = c(0,2))
curve(dexp(x, theta), from = 1e-9, add = T, lwd = 2, col = "red")
