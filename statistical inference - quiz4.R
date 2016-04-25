test <- t.test(x = examinations$baseline, y = examinations$week2, alt = "two.sided", paired = TRUE)
pval <- test$p.value
round(pval,3)

n <- 9
?? <- 1100
?? <- 30
quantile = 0.975 # is 95% with 2.5% on both sides of the range
confidenceInterval = ?? + c(-1, 1) * qt(quantile, df=n-1) * ?? / sqrt(n)
confidenceInterval

n <- 4
x <- 3
test <- binom.test(x=x, n=n, alt="greater")
round(test$p.value,2)

rate <- 1/100
errors <- 10
days <- 1787
test <-  poisson.test(errors, T = days, r = rate, alt="less")
round(test$p.value,2)

n_y <- 9 # subjects treated
n_x <- 9 # subjects placebo
??_y <- 1.5# kg/m2 std.dev. treated 
??_x <- 1.8# kg/m2 std.dev. placebo 
??_y <- -3#  kg/m2 average difference treated
??_x <- 1#  kg/m2 average difference placebo

# calculate pooled standard deviation
??_p <- (((n_x - 1) * ??_x^2 + (n_y - 1) * ??_y^2)/(n_x + n_y - 2))
pval <- pt((??_y - ??_x) / (??_p * (1 / n_x + 1 / n_y)^.5), df=n_y + n_x -2)
pval

n <- 100 #subject
?? <- 0.01# m^3 brain volume loss mean
?? <- 0.04# m^3 brain volume loss std. dev.
p <- 0.05 # sign level

pow <- power.t.test(n=n, delta=??, sd=?? , sig.level=p, type="one.sample", alt="one.sided")$power
round(pow, 2)