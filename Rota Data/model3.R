setwd("")

library(rstan)
source('Formatting Data.R')

# takes about 12 hours to run
fit <- stan('model3.stan', data = data, pars = params, chains = 2, init = inits,
            iter = 2000, warmup = 1000, thin = 1)

# checking convergence
max(summary(fit)[[1]][, 'Rhat'])

post.lik <- extract(fit, 'll')

# WAIC
-2 * sum(log(apply(exp(post.lik[[1]]), 2, mean))) +
  2 * sum(apply(post.lik[[1]], 2, var))

# summarizing slope coefficients
post.beta <- extract(fit, 'beta')

rbind(apply(post.beta[[1]], 2, quantile, probs = 0.025),
      apply(post.beta[[1]], 2, mean),
      apply(post.beta[[1]], 2, quantile, probs = 0.975))