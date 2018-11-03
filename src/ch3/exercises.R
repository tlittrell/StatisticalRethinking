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

paramter_grid = seq(0,1,length.out = 1000)
prior = rep(1,1000)
likelihood = dbinom(6,9,prob=paramter_grid)
posterior = calcPosterior(prior, likelihood)
set.seed(100)
samples = sample(paramter_grid, prob=posterior, size=1e4, replace = TRUE)
plotPosterior(parameter_grid, posterior)

# 3E1-7
sum(samples < 0.2)/1e4
sum(samples > 0.8)/1e4
sum(samples > 0.2 & samples < 0.8)/1e4
quantile(samples, 0.2)
quantile(samples, 0.8)
HPDI(samples, 0.66)
quantile(samples, c(1/6, 5/6))

### 3H
data(homeworkch3)

# 1
parameter_grid = seq(0,1,length.out = 1000)
prior = rep(1,1000)
likelihood = dbinom(sum(birth1) + sum(birth2),200,prob=parameter_grid)
posterior = calcPosterior(prior, likelihood)
plotPosterior(parameter_grid, posterior)
parameter_grid[which.max(posterior)]

# 2
samples = sample(parameter_grid, size=1e4, prob = posterior, replace = TRUE)
HPDI(samples,0.5)
HPDI(samples,0.89)
HPDI(samples,0.97)

#3
posterior_preds = rbinom(1e4,size=200,prob=samples)
dens(posterior_preds)
