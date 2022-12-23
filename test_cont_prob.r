set.seed(16)
act_scores <- rnorm(10000, 20.9, 5.7)

m <- mean(act_scores)
s <- sd(act_scores)
#score
perfect <- sum(act_scores >= 36)
mid <- mean(act_scores >= 30)
bad <- mean(act_scores <= 10)
#plots
x <- seq(1,36)
f_x <- dnorm(x, 20.9, 5.7)
plot(x, f_x)

#zscores
zscores <- (act_scores - mean(act_scores))/sd(act_scores)

q_3a <- mean(zscores > 2)
q_3b <- 2*sd(act_scores) + mean(act_scores)
q_c <- qnorm(.975, m, s)
#CDF
cdf <- sapply(1:36, function (x){
  mean(act_scores <= x)
})
q_4a <- min(which(cdf >= .95))
q_4b <- qnorm(.95, 20.9, 5.7)
p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores, p)
tab <- table(sample_quantiles)
library(tidyverse)
library(ggplot2)
theoretical_quantiles <- qnorm(p, 20.9, 5.7)
qplot(theoretical_quantiles, sample_quantiles) + geom_abline()
