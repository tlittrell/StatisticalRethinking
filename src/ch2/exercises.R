library(tidyverse)

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


### 2M1
points = 20
n = 7
p = 5
parameter_grid = seq(0,1,length.out = points)
prior = rep(1,points)
likelihood = dbinom(p, n, parameter_grid)
posterior = calcPosterior(prior, likelihood)
plotPosterior(parameter_grid, posterior)

### 2M2
prior = ifelse(parameter_grid < 0.5, 0, 2)
likelihood = dbinom(p, n, parameter_grid)
posterior = calcPosterior(prior, likelihood)
plotPosterior(parameter_grid, posterior)

### 2M3
prior = c(0.5, 0.5)
likelihood = c(0.3, 1)
posterior = calcPosterior(prior, likelihood)
posterior

### 2H1
# Model which species we have
prior = c(0.5, 0.5)
likelihood = c(0.1, 0.2)
posterior = calcPosterior(prior, likelihood)
# probability that next observation will be twins is the marginal p(twins) = \sum_{panda type} posterior * likelihood
sum(posterior * likelihood)

### 2H2
# Answered above in the posterior

### 2H3
# Start with posterior from 2H1 as our new prior
likelihood = c(0.9, 0.8)
posterior = calcPosterior(posterior, likelihood)
posterior

### 2H4
# Going to just do this with the prior set to the posterior after the two births.
# The whole game here is constructing the likelihood. We have P(Test A | Actually A) = 0.8 and
# P(Test B | Actually B) = 0.65. Our likelihood, then is (0.8, 1-0.65) = (0.8, 0.35)
likelihood = c(0.8, 0.35)
posterior = calcPosterior(posterior, likelihood)
posterior