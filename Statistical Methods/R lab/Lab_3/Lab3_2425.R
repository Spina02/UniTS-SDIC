# Statistical Methods: Lab 3 - 22/10/2024 #
# This is the pre-lab version             #
# You must complete the lines with XXX    #


# Point estimation: Comparison of four estimators of the mean parameter ----

# Initial settings
R <- 1000   # Number of replications
n <- 10     # Sample size
mu <- 2     # Population mean
sigma <- 2  # Population standard deviation

# Matrix where we save the results of the four estimators 
array(0, dim = c(2,2,2))
matrix(NA, 2, 4)

est <- matrix(0, nrow = R, ncol = 4)
label_est <- c("Mean", "Median", "(Min + Max)/2", "10pTrimMean")

set.seed(1234)

for (i in 1 : R) {
 x <- rnorm(n, mu, sigma)
 est[i, 1] <- mean(x)
 est[i, 2] <- median(x)
 est[i, 3] <- (min(x) + max(x))/2
 est[i, 4] <- mean(x, trim = 0.1) 
}

par(mfrow = c(1, 1), xaxt = "n")
boxplot(est, main="Comparison between four estimators")
par(xaxt = "s")
axis(1, 1 : 4, label_est)
abline(h = mu, lwd = 2, col = "blue")

# Compute the Bias
bias <- apply(est, 2, mean) - mu
bias

# Compute the Variances
variance <- apply(est, 2, var)
variance

# Compute the MSE 
MSE <- variance + bias^2
MSE 


# Point estimation: Assess properties increasing n ----
R <- 1000
n <- c(10, 200, 1000)
mu <- 2
sigma <- 2

est <- array(0, dim = c(R, 4, 3))
label_est <- c("Mean", "Median", "(Min + Max)/2", "10pTrimMean")

set.seed(13)


for(j in 1 : length(n)){
  for (i in 1 : R) {
    x <- rnorm(n[j], mu, sigma)
    est[i, 1, j] <- mean(x)
    est[i, 2, j] <- median(x)
    est[i, 3, j] <- (max(x) + min(x))/2
    est[i, 4, j] <- mean(x, trim = 0.1)
  }
}  

# Note that the plots obtained here are related to the last case 
# -) You can use the left arrow to see the other plots 
# -) Produce the plots for each k (do not consider the for loop and fix k)
for(k in 1: 4){
  par(mfrow = c(1, 3))
  for (j in 1 : length(n)) {
    hist(est[,k, j], nclass = 30,  probability = T, xlab = "", xlim = c(-2, 6),
         main = label_est[k], cex.main = 2)
    abline(v = mu, col = "blue", lwd = 2)
  }
}  

# Variances
variance <- matrix(0, 3, 4)
for(j in 1:length(n)){
  variance[j,] <- apply(est[,,j], 2, var)
}
# Bias
bias <- matrix(0, 3, 4)
for(j in 1:length(n)){
  bias[j,] <- apply(est[,,j], 2, mean) - mu 
}

# MSE 
MSE <- variance + bias^2

colnames(bias) <- colnames(variance) <- colnames(MSE) <- label_est
rownames(bias) <- rownames(variance) <- 
  rownames(MSE) <- c("n = 10", "n = 200", "n = 1000" )

variance 
bias
MSE


# Interval estimation: for $\mu$ under the Gaussian case, when $\sigma^2$ is known ----

B <- 1000     # Number of replications
n <- 10       # sample size
mu <- 5       # True population mean 
sigma <- 2    # True population standard deviation

alpha <- 0.05 # Confidence level: 1- alpha

# CI is matrix where we save the confidence intervals for each replication:
# -) first column: lower bound
# -) second column: upper bound
CI <- matrix(0, B, 2)

# l is a vector whose elements assume TRUE (1) or FALSE(0) depending on 
# whether the true parameter value lies within the interval
l <- rep(0, B)


set.seed(1234)
q0975 <- qnorm(1 - alpha/2) # quantile of order 1-alpha/2 of N(0,1)
for(i in 1 : B) {
  x <- rnorm(n, mu, sigma)
  CI[i,] <- mean(x) + c(-q0975, q0975) * sigma/sqrt(n)
  l[i] <- (mu > CI[i,1] & mu < CI[i,2])
}

#Empirical coverage probability: 
mean(l)
par(mfrow= c(1,1))
#Plot the first 100 c.i.:
# black: intervals not including mu
# red: intervals including  mu
plot(1, xlim = c(0, 10), ylim = c(0, 11), type = "n", 
     xlab = expression(mu), ylab = "", yaxt  = "n", 
     main = paste("100 IC for the mean (known variance)"), cex.main = 1.2)
abline(v = mu)

d <- 0
for(i in 1 : 100){
  d <- d + 0.1 
  lines(seq(CI[i, 1], CI[i, 2], length = 100), rep(d, 100), col = (l[i] + 1))
}

# number of intervals (out the 100) including the true parameter value 
sum(l[1 : 100]) 

# Interval estimation: for $\mu$ under the Gaussian case, when $\sigma^2$ is unknown ----
# consider the same setting as above: 
# mu = 5, sigma=2, n=10, B=1000, alpha=0.05
CI <- matrix(0, B, 2)
l <- rep(0, B)

set.seed(1234)
q0975 <- qt(1 - alpha/2, n - 1) # quantile of order 1-alpha/2  of t(n-1)

# -) Generate samples from the N(mu, sigma^2)
# -) Obtain the confidence interval for each replication 
# -) Use a flag to detect if the confidence interval 
#    includes the true parameter value
for (i in 1 : B){
  x <- rnorm(n, mu, sigma)
  CI[i, ] <- mean(x) + c(-q0975, q0975) * sd(x)/sqrt(n)
  l[i] <- (mu > CI[i,1] & mu < CI[i,2]) 
}

#Empirical coverage probability
mean(l)

# Plot of the (first 100) CIs
plot(1, xlim = c(0,10), ylim = c(0,11), type = "n", 
     xlab = expression(mu), ylab = "", yaxt  = "n", 
     main = paste("100 IC for the mean (unknown variance)"), cex.main = 1.2)
abline(v = mu)

d <- 0
for (i in 1 : 100){
  d <- d + 0.1 
  lines(seq(CI[i, 1], CI[i, 2], length = 100), rep(d, 100), col = (l[i] + 1))
}

# number of intervals (out the 100) including the true parameter value 
sum(l[1 : 100]) 




# Interval estimation: difference between two means 

Anor <- read.table("http://stat4ds.rwth-aachen.de/data/Anorexia.dat", 
                   header=TRUE)
# Get difference post-pre treatment for the group cb and c 
cogbehav <- Anor$after[Anor$therapy == "cb"] - Anor$before[Anor$therapy == "cb"]
control <- Anor$after[Anor$therapy == "c"] - Anor$before[Anor$therapy == "c"]


# Get the 95% CI via t.test function   
res <- t.test(cogbehav,control,var.equal=TRUE,conf.level=0.95)
res$conf.int


# Obtain the result above by hand: we will see together during next lab
# Fill XXX
n1 <- XXX
n2 <- XXX

s2 <- XXX

CI <- XXX 


# Interval estimation: difference between two proportions 
success <- c(315, 304)
total <-  c(604, 597)
res <- prop.test(success, total, conf.level = 0.95, correct = FALSE)
res$conf.int

p1 <- success[1]/total[1]
p2 <- success[2]/total[2]

p1 - p2 + c(-1, 1) * qnorm(0.975) * sqrt(p1 * (1 - p1)/total[1] + p2 * (1 - p2)/total[2])
