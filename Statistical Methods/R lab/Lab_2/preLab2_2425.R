########################################
# R code - LAB 2 - Statistical Methods #
#                                      #
# !!! This is the pre-LAB version      #
# You can find XXX to complete         #
########################################

## Distribution of the sample mean ----

# Case 1) Generate from N(0,1)
# Case 2) Generate from t-student with 3 degrees of freedom 
# Case 3) Generate from (continuous) uniform in (0,1)


set.seed(1234) # Set a seed 
R <- 1000      # Fix the number of simulations
n <- 30        # Set the sample size 

# Generate the sample of size n (for the 3 distributions) R times 
samples <- array(0, c(3, n, R))
for(i in 1 : R){
  samples[1, ,i] <- rnorm(n, 0, 1)
  samples[2, ,i] <- rt(n, df = 3)
  samples[3, ,i] <- runif(n, 0, 1)
}


# Getting the sample mean
sample_stat <- apply(samples, c(1,3), mean)

# Equivalently (creating a)
sample_stat2 <- matrix(0, nrow = 3, ncol = R)
for(i in 1 : R) {
  sample_stat2[, i] <- apply(samples[, , i], 1, mean)
}
max(abs(sample_stat - sample_stat2)) # Check 


# Visualise the sample distributions by means of hiostograms 
# par (mfrow=c(1,3))
# 
# hist(sample_stat[1,], nclass = 30, probability = TRUE, 
#      xlab="y", main= "N(0,1)", cex.main = 1.5)
# hist(sample_stat[2,], nclass = 30, probability = TRUE, 
#      xlab = "y", main = expression(t[3]), cex.main = 1.5)
# hist(sample_stat[3,], nclass = 30, probability = TRUE, 
#      xlab = "y", main = "U(0,1)", cex.main = 1.5)
# 


# It's your turn: Overlap the density of the proper normal distribution to the histogram:
# Case 1) N( , )
# Case 2) N( , )
# Case 3) N( , )

# hist(sample_stat[1,], nclass = 30, probability = TRUE, 
#      xlab="y", main= "N(0,1)", cex.main = 1.5)
# curve(dnorm(x, 0, sqrt(1/n)), add = TRUE, col = "red", lwd = 2)
# 
# hist(sample_stat[2,], nclass = 30, probability = TRUE, 
#      xlab = "y", main = expression(t[3]), cex.main = 1.5)
# curve(dnorm(x, 0, sqrt(3/n)), add = TRUE, col = "red", lwd = 2)
# 
# hist(sample_stat[3,], nclass = 30, probability = TRUE, 
#      xlab = "y", main = "U(0,1)", cex.main = 1.5)
# curve(dnorm(x, 0.5, sqrt(1/(12*n))), add = TRUE, col = "red", lwd = 2)
# 
# # Other graphichal tools for this assesment 
# 
# # ECDF vs CDF 
# par(mfrow = c(1, 3))
# plot(ecdf(sample_stat[1,]), xlab="y", main= "N(0,1)", cex.main = 1.5)
# curve(pnorm(x, 0, sqrt(1/n)), add = TRUE, col = "red", lty = 2)
# 
# plot(ecdf(sample_stat[2,]), xlab="y", main= expression(t[3]), cex.main = 1.5)
# curve(pnorm(x, 0, sqrt(3/n)), add = TRUE, col = "red", lty = 2)
# 
# plot(ecdf(sample_stat[3,]), xlab="y", main= "U(0,1)", cex.main = 1.5)
# curve(pnorm(x, 0.5, sqrt(1/(12*n))), add = TRUE, col = "red", lty = 2)
# 
# 
# # QQ-Norm 
# par(mfrow = c(1, 3))
# 
# qqnorm(sample_stat[1,])
# abline(0, sqrt(1/n))
# 
# qqnorm(sample_stat[2,])
# abline(0, sqrt(3/n))
# 
# qqnorm(sample_stat[3,])
# abline(1/2, sqrt(1/(12*n)))
# 
# 

#? Distribution of the sample variance under the Gaussian case---- 

# 
# par (mfrow = c(1, 2))
# sigma <- 1 
# 
# # Recall the the samples are arleady generated and stored in the array samples
# sample_var <- apply(samples, c(1,3), var)[1,]
# 
# # Histogram 
# hist(sample_var, nclass = 30, probability = TRUE, 
#      xlab = expression(s^2), main = "Case 1: N(0,1)", cex.main = 1.5)
# curve((n-1)/sigma^2 * dchisq((n-1)*x/sigma^2, df = n -1),  add = TRUE, col = "red", lwd = 2)
# 
# # ECDF vs CDF 
# plot(ecdf(sample_var), xlab = expression(s^2), main = "Case 1: N(0,1)", cex.main = 1.5)
# curve(pchisq((n-1)*x/sigma^2, df = n-1),  add = TRUE, col = "red", lwd = 2)


# Point estimation: Comparison of unbiased and biased sample variance estimators ----
# Check the biased nature of the (biased) sample variance estimator via MC simulation, 
# by generating n = 10 iid values from N(0,1), and compare the results with the unbiased estimator 
  
#Initial settings
set.seed(2)
R <- 1000
n <- 10
mu <- 0
sigma <- 1

# Save the results in two vectors:
     # s2: unbiased sample variance estimates 
     # s2_b: biased sample variance estimates

s2 <- rep(0, R)
s2_b <- rep(0, R)
#
## For each replication we generate 10 samples from
## a normal r.v. with mean mu and variance sigma^2
## and we compute the four sample variance estimates 

for(i in 1 : R) {
  y <- rnorm(n, mu, sigma)
  s2[i] <- var(y) 
  s2_b[i] <- var(y) * (n - 1)/n
}

# Get a point estimates 
s2_mean <- mean(s2)
s2_b_mean <- mean(s2_b)

#plot s2
par(mfrow = c(1, 2), oma = c(0, 0, 0, 0))
hist(s2, breaks = 50, xlab = expression(s^2), probability = TRUE,
     main = expression(s^2), cex.main = 1.5)
#in red the true mean, in blue the estimated mean
abline(v = sigma^2, col = "red", lwd = 2)
abline(v = s2_mean, col = "blue", lwd = 3, lty = 3) 

#plot s2 biased
hist(s2_b, breaks = 50, xlab = expression(s[b]^2), probability = TRUE,
     main = expression(s[b]^2), cex.main = 1.5)
#in red the true mean, in blue the estimated mean
abline(v = sigma^2, col = "red", lwd = 3)
abline(v = s2_b_mean, col = "blue", lwd = 3, lty = 2)