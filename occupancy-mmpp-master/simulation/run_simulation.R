source('sim_functions.R')

#Begin simulation
set.seed(123)

#Sample size
N <- 100

#True occupancy probability for each state
f <- c(1.7,-0.7,1)
psi <- c(exp(sum(f)),exp(f[1]),exp(f[2]),exp(0))
psi <- psi/sum(psi)

#Probability of initial markov state
mu1 <- c(3, 150)
mu2 <- c(1, 60)

#coyotes: 0.68, 0.34, 0.17
#deer: 1, 0.5, 0.25

#Detection intensities (always 0 in first state)
#lambdas are rough rounding of values from fitted model
#can't set first value to 0, has to at least be a little bigger
lam1 <- c(0.0001,40) #sp1 | sp2 present
lam2 <- c(0.0001,11) #sp1 | sp2 absent
lam3 <- c(0.0001,3.5) #sp2

#Time intervals
maxt <- 21 #3 days, intervals by hour

truth <- c(f, log(mu1), log(mu2), log(lam1[2]), log(lam2[2]), log(lam3[2]))

#Find values for mu[1]---------------------------------------------------------

#Deer
mu_deer_vals <- rep(NA,3)
cands <- as.matrix(data.frame(m1=seq(0.05,3,0.001), m2=mu1[2]))
pval <- rep(NA, nrow(cands))
for (i in 1:length(pval)){
  pval[i] <- get_p(cands[i,], lam1, maxt)
}
mu_deer_vals[1] <- cands[,1][which(round(pval,2)==0.25)[1]]
mu_deer_vals[2] <- cands[,1][which(round(pval,2)==0.5)[1]]
mu_deer_vals[3] <- mu1[1]

#plot(cands[,1], pval, type='l')
#for (i in 1:3){
#  abline(v=mu_deer_vals[i], col='red')
#}

#Coyote
mu_coy_vals <- rep(NA,3)
cands <- as.matrix(data.frame(m1=seq(0.05,1,0.001), m2=mu2[2]))
pval <- rep(NA, nrow(cands))
for (i in 1:length(pval)){
  pval[i] <- get_p(cands[i,], lam3, maxt)
}
mu_coy_vals[1] <- cands[,1][which(round(pval,2)==0.25)[1]]
mu_coy_vals[2] <- cands[,1][which(round(pval,2)==0.5)[1]]
mu_coy_vals[3] <- mu2[1]

#plot(cands[,1], pval, type='l')
#for (i in 1:3){
#  abline(v=mu_coy_vals[i], col='red')
#}

#Set up simulation matrix------------------------------------------------------

Nsize <- c(100,1000)

sim_mat <- expand.grid(mu_deer_vals, mu_coy_vals, Nsize)

saveRDS(sim_mat, "sim_mat.Rds")

#Run simulations---------------------------------------------------------------

nrep <- 30

N100 <- lapply(1:9, function(i){
  out_list <- vector("list", nrep)
  for (j in 1:nrep){
    cat(paste0("\nOn simulation ",i," replicate ",j,"\n\n"))
    pars <- as.numeric(sim_mat[i,])
    mu1 <- c(pars[1], 150)
    mu2 <- c(pars[2], 60)
    out_list[[j]] <- mmpp_simulation(pars[3], truth, maxt, psi, mu1, mu2,
                                     lam1, lam2, lam3, threads=4)
  }
  out_list
})

saveRDS(N100, "sim_N100.Rds")

#--------------------
set.seed(789)

nrep <- 30

N1000 <- lapply(10:18, function(i){
  out_list <- vector("list", nrep)
  for (j in 1:nrep){
    cat(paste0("\nOn simulation ",i," replicate ",j,"\n\n"))
    pars <- as.numeric(sim_mat[i,])
    mu1 <- c(pars[1], 150)
    mu2 <- c(pars[2], 60)
    out_list[[j]] <- mmpp_simulation(pars[3], truth, maxt, psi, mu1, mu2,
                                     lam1, lam2, lam3, threads=4)
  }
  out_list
})

saveRDS(N1000, "sim_N1000.Rds")












#Build plot to compare
truth <- c(est[1:3], log(mu1), log(mu2), log(lam1[2]), log(lam2[2]), log(lam3[2]))
cbind(truth, test$par)

names(est) <- c(paste0("f1_",colnames(X_f1)), paste0("f2_",colnames(X_f2)),
                paste0("f12_",colnames(X_f12)),
                "log_mu1[1]","log_mu1[2]","log_mu2[1]","log_mu2[2]",
                paste0("loglam1_",colnames(X_lam1)),
                paste0("loglam2_",colnames(X_lam2)), paste0("loglam3_",colnames(X_lam3)))
se <- sqrt(diag(solve(fit$hessian)))


library(tidyverse)
library(ggplot2)

parnames <- c('f1','f2','f12','mu1[1]','mu1[2]','mu2[1]','mu2[2]','lam1','lam2','lam3')

results <- data.frame(est=round(est, 3), se=round(se,3))
results$lower <- results$est - 1.96*results$se
results$upper <- results$est + 1.96*results$se
results$truth <- truth
results$par <- parnames
results

ptdf <- results %>%
  gather(key="type", value="val", est, truth)

ptdf$lower[ptdf$type=="truth"] <- ptdf$upper[ptdf$type=="truth"] <- NA
ptdf$type[ptdf$type=="est"] <- "estimate + 95% CI"

ptdf %>%
  ggplot(aes(x=par)) +
  geom_errorbar(aes(ymin=lower,ymax=upper, col=type), width=0.2) +
  geom_point(aes(y=val, col=type), alpha=0.5, size=3) +
  theme_bw() +
  labs(y="Parameter value", x="Parameter (transformed scale)") +
  theme(axis.title=element_text(size=16), axis.text=element_text(size=12),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        legend.title=element_blank(), legend.text=element_text(size=16),
        legend.position=c(0.2,0.9))

ggsave("sim_results.png", width=8)
