y <- (nhtemp - 32) / 1.8
plot(1912:1971, y, pch = 16, xlab = "Year", ylab = "y")
title("samples")

#hist(y, prob = T)
# try with a normal distribution
#curve(dnorm(x, mean(y), sd(y)), add = T, col = "red")
# try with a t_5 distribution
#curve(dt((x - mean(y))/sd(y), 5), add = T, col = "blue")
# try assuming a linear trend over time
lm1 <- lm(y ~ 1912:1971)
abline(lm1, col = "green")


