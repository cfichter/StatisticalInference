#Initial settings
## ------------------------------------------------------------------------
set.seed(3)
lambda <- 0.2
num_sim <- 1000
sample_size <- 40
sim <- matrix(rexp(num_sim*sample_size, rate=lambda), num_sim, sample_size)
row_means <- rowMeans(sim)
## ------------------------------------------------------------------------
# plot the histogram of averages
hist(row_means, breaks=50, prob=TRUE,
     main="Distribution of averages of samples,
     drawn from exponential distribution with lambda=0.2",
     xlab="")
# density of the averages of samples
lines(density(row_means))
# theoretical center of distribution
abline(v=1/lambda, col="red")
# theoretical density of the averages of samples
xfit <- seq(min(row_means), max(row_means), length=100)
yfit <- dnorm(xfit, mean=1/lambda, sd=(1/lambda/sqrt(sample_size)))
lines(xfit, yfit, pch=22, col="red", lty=2)
# add legend
legend('topright', c("simulation", "theoretical"), lty=c(1,2), col=c("black", "red"))
## ------------------------------------------------------------------------
qqnorm(row_means); qqline(row_means)

## ------------------------------------------------------------------------
#confidence interval for *lambda*
X_hat <- mean(row_means) + c(-1, 1) * 1.96 * sd(row_means)/sqrt(sample_size)

#coverage
set.seed(3)
num_sim <- 1000
sample_size <- 40

#define a sequence of 1/lambda values near what we are estimating
lambda_vals <- seq(4, 6, by=0.01)

coverage <- sapply(lambda_vals, function(lmbd) {
  #Calculate X_hats for a thousand of simulations 
  s <- matrix(rexp(num_sim*sample_size, rate=0.2), num_sim, sample_size)
  X_hats <- rowMeans(s) #means of simulations
  #calculate limits  
  lowerlimit <- X_hats - (qnorm(0.975) * (1/lambda)/sqrt(sample_size))
  upperlimit <- X_hats + (qnorm(0.975) * (1/lambda)/sqrt(sample_size))
  #calculate the proportion of times that they can cover
  #the true value of lambda used to simulate the data
  mean(lowerlimit < lmbd & upperlimit > lmbd)
  #sum(lmbd > lowerlimit & lmbd < upperlimit,na.rm = TRUE) / length(X_hats)
})

#plot
library(ggplot2)
qplot(lambda_vals, coverage) + geom_hline(yintercept=0.95)+ylim(.75, 1.10)
