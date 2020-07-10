## Runs “Formatting Data.R”, executes the Stan model, 
## and has code for calculating WAIC and 
## summarizing posterior distributions for slope 
## parameters.

setwd("")

library(rstan) #install.packages("rstan")
source('Formatting Data.R')

# takes about 12 hours to run
fit <- stan('model3.stan', data = data, pars = params, chains = 2, init = inits,
            iter = 2000, warmup = 1000, thin = 1)

# checking convergence
max(summary(fit)[[1]][, 'Rhat'])

post.lik <- extract(fit, 'll')

# WAIC
## an estimator of out-of-sample prediction error 
## and thereby relative quality of statistical models 
## for a given set of data
-2 * sum(log(apply(exp(post.lik[[1]]), 2, mean))) +
  2 * sum(apply(post.lik[[1]], 2, var))

# summarizing slope coefficients
post.beta <- extract(fit, 'beta')

rbind(apply(post.beta[[1]], 2, quantile, probs = 0.025),
      apply(post.beta[[1]], 2, mean),
      apply(post.beta[[1]], 2, quantile, probs = 0.975))