# Detection figure
library(ggplot2)
library(tidyverse)
library(lubridate)

fit <- readRDS('../application/fit_covs3.Rds')
results <- readRDS('../application/results.Rds')

raw <- read_csv('../application/Raw Data.csv') %>%
  select(begin_date_time, `Common Name`) %>%
  filter(`Common Name` %in% c("White-tailed Deer","Coyote")) %>%
  mutate(begin_date_time = as.POSIXlt(begin_date_time, tz = 'US/Eastern',
                 format = '%m/%d/%Y %H:%M')) %>%
  mutate(hr=hour(begin_date_time), min=minute(begin_date_time)) %>%
  mutate(dec_time = hr + min/60)

raw$`Common Name`[raw$`Common Name` == "White-tailed Deer"] <- "WTD"

data_plot <- raw %>%
  mutate(`Common Name`=factor(`Common Name`,levels=c("WTD","Coyote"))) %>%
  ggplot() +
  geom_histogram(aes(x=dec_time,fill=`Common Name`), position='identity') +
  #scale_fill_manual(values=get_palette('npg',3)[c(2,1)]) +
  scale_fill_manual(values=c('#999999','#333333')) +
  annotate("text", x=0,y=1200, label='A', size=7) +
  xlab('Hour of day') + ylab('Detection frequency') +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title=element_text(size=16),
        axis.text=element_text(size=14),
        legend.title=element_blank(),
        legend.text=element_text(size=14),
        legend.spacing.x = unit(0.3, 'cm'),
        legend.position=c(0.55,0.9))

tseq <- seq(0, 23.99, by=0.01)

tmat <- as.matrix(data.frame(int=1,
                             f1c=cos(pi*tseq/12),
                             f2c=cos(2*pi*tseq/12),
                             f1s=sin(pi*tseq/12),
                             f2s=sin(2*pi*tseq/12)))


#pars <- c(1.57416144, 0.24573173, 0.43002500, -3.27283682, -0.59568686, -1.97377927,  0.37436148,  2.40517378,
# 0.11765465, -0.20384948, -0.08825643,  0.13445622,  0.71235373, -0.51243227,  0.15409516,  0.23447457,
#0.25086146, -0.83372487,  0.36713810,  0.17453413,  0.19656721, -0.11497074)

#Remove mu parameters as SE could not be estimated for mu[2] (?)
#beta <- fit$par[c(1:5,10:24)]
#Sigma <- solve(fit$hessian)
#Sigma <- Sigma[c(1:5,10:24),c(1:5,10:24)]
beta <- fit$par
Sigma <- solve(fit$hessian)

#lambda 1: deer, coyote present
inds <- 11:15
pred <- exp(tmat %*% beta[inds])

lower <- upper <- rep(NA, length(pred))

bsamp <- MASS::mvrnorm(100, beta, Sigma)

samps <- matrix(NA, nrow=length(pred), ncol=nrow(bsamp))
for (i in 1:nrow(bsamp)){
  samps[,i] <- exp(tmat %*% bsamp[i,inds])
}
lower <- apply(samps, 1, quantile, 0.025)
upper <- apply(samps, 1, quantile, 0.975)

l1 <- data.frame(hour=tseq, lambda=pred, lower=lower, upper=upper,
                species='WTD, coyote present')


#expected detections at night

flam <- function(tseq, beta){
  mat <- as.matrix(data.frame(int=1,
                             f1c=cos(pi*tseq/12),
                             f2c=cos(2*pi*tseq/12),
                             f1s=sin(pi*tseq/12),
                             f2s=sin(2*pi*tseq/12)))
  mat %*% beta

}

prop_dets_night <- function(beta){

  int_night <- integrate(flam, (8+12), (12+12), beta=beta)$value +
    integrate(flam, 0, 5, beta=beta)$value

  int_total <- integrate(flam, 0, 24, beta=beta)$value

  int_night/int_total
}

prop_dets_night(beta[inds])

prop_samps <- rep(NA, nrow(bsamp))
for (i in 1:nrow(bsamp)){
  prop_samps[i] <- prop_dets_night(bsamp[i,inds])
}
quantile(prop_samps, c(0.025,0.975))



#lambda 2: deer, coyote absent
inds <- 16:20
pred <- exp(tmat %*% beta[inds])

lower <- upper <- rep(NA, length(pred))

#bsamp <- MASS::mvrnorm(100, beta, Sigma)

samps <- matrix(NA, nrow=length(pred), ncol=nrow(bsamp))
for (i in 1:nrow(bsamp)){
  samps[,i] <- exp(tmat %*% bsamp[i,inds])
}
lower <- apply(samps, 1, quantile, 0.025)
upper <- apply(samps, 1, quantile, 0.975)

l2 <- data.frame(hour=tseq, lambda=pred, lower=lower, upper=upper,
                species='WTD, coyote absent')

#prop night
prop_dets_night(beta[inds])

prop_samps <- rep(NA, nrow(bsamp))
for (i in 1:nrow(bsamp)){
  prop_samps[i] <- prop_dets_night(bsamp[i,inds])
}
quantile(prop_samps, c(0.025,0.975))



#lambda 3: coyote
inds <- 21:25
pred <- exp(tmat %*% beta[inds])

lower <- upper <- rep(NA, length(pred))

#bsamp <- MASS::mvrnorm(100, beta, Sigma)

samps <- matrix(NA, nrow=length(pred), ncol=nrow(bsamp))
for (i in 1:nrow(bsamp)){
  samps[,i] <- exp(tmat %*% bsamp[i,inds])
}
lower <- apply(samps, 1, quantile, 0.025)
upper <- apply(samps, 1, quantile, 0.975)

l3 <- data.frame(hour=tseq, lambda=pred, lower=lower, upper=upper,
                species='Coyote')

plot_dat <- rbind(l1,l2,l3)

fit_plot <- plot_dat %>%
  mutate(species=factor(species, levels=c("WTD, coyote present",
                                          "WTD, coyote absent", "Coyote"))) %>%
  ggplot(aes(x=hour, y=lambda/24)) +
  geom_ribbon(aes(ymin=lower/24,ymax=upper/24, group=species), alpha=0.2) +
  geom_line(aes(linetype=species)) +
  annotate("text", x=0,y=3.2, label='B', size=7) +
  theme_bw() +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        axis.text=element_text(size=14), axis.title=element_text(size=16),
        legend.title=element_blank(), legend.text=element_text(size=14),
        legend.position=c(0.6,0.9)) +
  xlim(0,23.8) +
  ylim(0,3.3) +
  labs(y=expression(Detection~intensity~(lambda)), x="Hour of day")

cowplot::plot_grid(data_plot, fit_plot, nrow=1)

ggsave("fig2_detection.tiff", compression='lzw', width=9)
