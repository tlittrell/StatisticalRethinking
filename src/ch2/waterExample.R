library(tidyverse)

# Set up problem
points = 20
p_grid = seq(from=0, to=1, length.out = points)
prior = rep(1,points) # uniform prior
likelihood = dbinom(6, 9, prob=p_grid)

# Compute posterior
unstd.posterior = likelihood * prior
posterior = unstd.posterior / sum(unstd.posterior)

# Plot results
ggplot() +
  aes(x = p_grid, y = posterior) +
  geom_point() + 
  geom_line() +
  theme_bw() +
  xlab("Probability of water") +
  ylab("Posterior probability")

library(rethinking)

# Quadratic approximation
globe.qa = rethinking::map(
  alist(
    w ~ dbinom(9,p), # binomial likelihood
    p ~ dunif(0,1)   # uniform prior
  ),
  data = list(w=6)
)
precis(globe.qa)