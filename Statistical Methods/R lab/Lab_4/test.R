# Statistical Methods: Lab 3 - 22/10/2024 #
# This is the pre-lab version             #
# You must complete the lines with XXX    #

# Interval estimation ----

## Difference between two means ----

Anor <- read.table("http://stat4ds.rwth-aachen.de/data/Anorexia.dat", 
                   header=TRUE)

# Get difference post-pre treatment for the group cb and c 
cogbehav <- Anor$after[Anor$therapy == "cb"] - Anor$before[Anor$therapy == "cb"]
control <- Anor$after[Anor$therapy == "c"] - Anor$before[Anor$therapy == "c"]


# Get the 95% CI via t.test function   
res <- t.test(cogbehav,control,var.equal=TRUE,conf.level=0.95)
res$conf.int

n1 <- length(cogbehav)
n2 <- length(control)

s2 <- (sum((cogbehav - mean(cogbehav))^2) + sum((control - mean(control))^2))/(n1 + n2 - 2)

CI <- (mean(cogbehav) - mean(control)) + c(-1, 1) * qt(0.975, n1 + n2 - 2) * sqrt(s2 * (1/n1 + 1/n2))

# One-sided two-sample test

## Difference between two proportions ----
success <- c(315, 304)
total <-  c(604, 597)
res <- prop.test(success, total, conf.level = 0.95, correct = FALSE)
res$conf.int

p1 <- success[1]/total[1]
p2 <- success[2]/total[2]

p1 - p2 + c(-1, 1) * qnorm(0.975) * sqrt(p1 * (1 - p1)/total[1] + p2 * (1 - p2)/total[2])


# Hypothesis testing ----

## Test for the mean difference ----

# Two-sided two-sample test

res.two  <- t.test(cogbehav, control, var.equal = TRUE)
res.two



# PErform th test by hand (complete XXX)
testStat <- (mean(cogbehav) - mean(control))/sqrt(s2*(1/n1 + 1/n2))

# p-value
p-value.two <- 2 * pt(testStat, df = n1 + n2 - 2, lower.tail = FALSE)
# or p-value.two = 2* (1-pt(testStat, df = n1 + n2 - 2))

# One-sided two-sample test
res.one  <- t.test(cogbehav, control, var.equal = TRUE)
res.one

## one-sided
p-value.one <- pt(testStat, df = n1 + n2 - 2, lower.tail = FALSE)

library(RColorBrewer)
plotclr <- brewer.pal(6, "YlOrRd")

curve(dt(x, n1 + n2 - 2), xlim = c(-5, 5), ylim = c(0, 0.4), 
      main = "p-values and rejection region", col = "blue", 
      lwd = 2, xlab = "x-y",  ylab = expression(t[13]),  yaxs="i")
cord.x <- c(qt(0.95, n1 + n2 - 2),seq(qt(0.95, n1 + n2 - 2), 5, 0.01), 5)
cord.y <- c(0, dt(seq(qt(0.95, n1 + n2 - 2), 5, 0.01), 13), 0)
polygon(cord.x, cord.y, col = plotclr[3], border = NA )