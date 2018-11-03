library(tidyverse)
library(rethinking)

plotPosterior = function(grid, posterior){
  ggplot() +
    aes(x = grid, y = posterior) +
    geom_point() + 
    geom_line() +
    theme_bw() +
    xlab("Parameter value") +
    ylab("Posterior probability")
}

calcPosterior = function(prior, likelihood){
  posterior = prior * likelihood
  posterior = posterior / sum(posterior)
  return(posterior)
}

### 3.1
num_points = 1000
parameter_grid = seq(0,1,length.out = num_points)
prior = rep(1, num_points)
likelihood = dbinom(6,9,prob=parameter_grid)
posterior = calcPosterior(prior, likelihood)
plotPosterior(parameter_grid, posterior)

samples = sample(parameter_grid, prob = posterior, size = 1e4, replace = TRUE)
plot(samples)
dens(samples)

### 3.2
sum(posterior[parameter_grid < 0.5])
sum(samples < 0.5) / 1e4
sum(samples > 0.5 & samples < 0.75) / 1e4

quantile(samples)
quantile(samples, 0.8)

# new example of a highly skewed distribution
# parameter_grid = seq(0, 1, length.out=1000)
# prior = rep(1,num_points)
# likelihood = dbinom(3,3,parameter_grid)
# posterior = calcPosterior(prior, likelihood)
# plotPosterior(parameter_grid, posterior)
# samples = sample(parameter_grid, size=1e4, replace=TRUE, prob=posterior)

# the following two are the same
quantile(samples, c(0.25,0.75))
PI(samples, prob=0.5)

HPDI(samples, prob=0.5)

# point estimates
parameter_grid[which.max(posterior)]
chainmode(samples, adj=0.01)
mean(samples)
median(samples)

### 3.3

dbinom(0:2, size=2, prob=0.7)
rbinom(10, size=2, prob=0.7)

dummy_w = rbinom(1e5, size=9, prob=0.7)
ggplot() +
  aes(dummy_w) +
  geom_histogram()
simplehist(dummy_w)

# Calculate the posterior predictive distribution
w = rbinom(1e4, size=9, prob = samples)
simplehist(w)
