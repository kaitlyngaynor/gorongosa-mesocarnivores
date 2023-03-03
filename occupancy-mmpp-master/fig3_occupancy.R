library(ggplot2)
library(tidyverse)

fit <- readRDS('../application/fit_covs3.Rds')
results <- readRDS('../application/results.Rds')

beta <- fit$par
Sigma <- solve(fit$hessian)
bsamp <- MASS::mvrnorm(100, beta, Sigma)

#no hunting
f1 <- beta[1]
f2 <- beta[3]
f12 <- beta[5]

psi <- rep(NA, 4)
psi[1] <- exp(f1 + f2 + f12)
psi[2] <- exp(f1)
psi[3] <- exp(f2)
psi[4] <- 1
psi_est <- psi/sum(psi)

pred <- c(sum(psi_est[1:2]), psi_est[1]/sum(psi_est[c(1,3)]),
          psi_est[2]/sum(psi_est[c(2,4)]),
          sum(psi_est[c(1,3)]), psi_est[1]/sum(psi_est[c(1,2)]),
          psi_est[3]/sum(psi_est[c(3,4)]))

#hunting
f1 <- beta[1]+beta[2]
f2 <- beta[3]+beta[4]
f12 <- beta[5] + beta[6]

psi <- rep(NA, 4)
psi[1] <- exp(f1 + f2 + f12)
psi[2] <- exp(f1)
psi[3] <- exp(f2)
psi[4] <- 1
psi_est <- psi/sum(psi)

pred_hunt <- c(sum(psi_est[1:2]), psi_est[1]/sum(psi_est[c(1,3)]),
               psi_est[2]/sum(psi_est[c(2,4)]),
          sum(psi_est[c(1,3)]), psi_est[1]/sum(psi_est[c(1,2)]),
          psi_est[3]/sum(psi_est[c(3,4)]))

pred <- c(pred, pred_hunt)

pred_samp <- matrix(NA, nrow=12, ncol=nrow(bsamp))
for (i in 1:nrow(bsamp)){
  f1 <- bsamp[i,1]
  f2 <- bsamp[i,3]
  f3 <- bsamp[i,5]
  psi <- rep(NA, 4)
  psi[1] <- exp(f1+f2+f3)
  psi[2] <- exp(f1)
  psi[3] <- exp(f2)
  psi[4] <- 1
  psi <- psi/sum(psi)
  pred_samp[1:6,i] <- c(sum(psi[1:2]),psi[1]/sum(psi[c(1,3)]), psi[2]/sum(psi[c(2,4)]),
                        sum(psi[c(1,3)]),psi[1]/sum(psi[c(1,2)]),psi[3]/sum(psi[c(3,4)]))

  #Hunt
  f1 <- bsamp[i,1]+bsamp[i,2]
  f2 <- bsamp[i,3]+bsamp[i,4]
  f3 <- bsamp[i,5]+bsamp[i,6]
  psi <- rep(NA, 4)
  psi[1] <- exp(f1+f2+f3)
  psi[2] <- exp(f1)
  psi[3] <- exp(f2)
  psi[4] <- 1
  psi <- psi/sum(psi)
  pred_samp[7:12,i] <- c(sum(psi[1:2]),psi[1]/sum(psi[c(1,3)]), psi[2]/sum(psi[c(2,4)]),
                        sum(psi[c(1,3)]),psi[1]/sum(psi[c(1,2)]),psi[3]/sum(psi[c(3,4)]))

}
lower <- apply(pred_samp, 1, quantile,0.025)
upper <- apply(pred_samp, 1, quantile,0.975)


pdat <- data.frame(species=rep(rep(c("White-tailed deer","Coyote"),each=3),2),
                   lab=c("Marginal", "Coyote present", "Coyote absent",
                         "Marginal", "WTD present", "WTD absent"),
                   occ=pred, lower=lower, upper=upper,
                   hunt=rep(c("Not hunted","Hunted"), each=6))

pdat %>%
  mutate(lab=factor(lab, levels=c("Marginal","WTD present","Coyote present",
                                  "WTD absent","Coyote absent"))) %>%
  mutate(species=factor(species, levels=c("White-tailed deer", "Coyote"))) %>%
  ggplot(aes(x=lab, y=occ, col=hunt)) +
  geom_errorbar(aes(ymin=lower,ymax=upper), width=0.1,
                position = position_dodge(width = 0.5)) +
  scale_color_manual(values=c('#999999','#333333')) +
  geom_point(size=2,position = position_dodge(width = 0.5)) +
  facet_wrap("species", scales='free_x') +
  #ylim(c(0.4,1)) +
  labs(y="Occupancy probability") +
  #geom_vline(xintercept=2.5, linetype=1) +
  theme_bw() +
  theme(axis.title.x=element_blank()) +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        axis.text=element_text(size=14), axis.title=element_text(size=16),
        axis.text.x = element_text(angle = 25, vjust = 0.65),
        legend.title=element_blank(), legend.text=element_text(size=14),
        strip.background=element_rect("white"),strip.text=element_text(size=14),
        legend.position=c(0.2,0.2))

ggsave("fig3_occupancy.tiff", compression='lzw')

