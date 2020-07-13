## Runs “Formatting Data.R”, executes the Stan model, 
## and has code for calculating WAIC and 
## summarizing posterior distributions for slope 
## parameters.

setwd("")

library(rstan) #install.packages("rstan")
source('Rota Data/Formatting Data.R')

# takes about 12 hours to run
fit <- stan('Rota Data/model3.stan', data = data, pars = params, chains = 2, init = inits,
            iter = 2000, warmup = 1000, thin = 1)

# checking convergence (this should be < 1.1, as I understand it -kaitlyn)
max(summary(fit)[[1]][, 'Rhat'])

# extract output (log likelihood) from STAN model
post.lik <- extract(fit, 'll')

# WAIC - calculate information criterion of model 
# from Rota et al We compared our candidate models with Watanabe–Akaike Information Criterion (WAIC), a fully Bayesian information criterion analogous to Akaike Information Criterion (AIC)
# this helps you identify best covariates / most parsimonious model (lower WAIC = better model)
## an estimator of out-of-sample prediction error 
## and thereby relative quality of statistical models 
## for a given set of data
-2 * sum(log(apply(exp(post.lik[[1]]), 2, mean))) +
  2 * sum(apply(post.lik[[1]], 2, var))

# summarizing slope coefficients
post.beta <- extract(fit, 'beta')

# calculate lower CI, mean, and upper CI for slopes for each of the 32 occupancy covariates
rbind(apply(post.beta[[1]], 2, quantile, probs = 0.025),
      apply(post.beta[[1]], 2, mean),
      apply(post.beta[[1]], 2, quantile, probs = 0.975))
