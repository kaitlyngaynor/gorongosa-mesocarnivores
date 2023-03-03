library(ggplot2)

# importing deer survival curves
deer_theo <- readRDS('../application/deer_theo.rds')
deer_empr <- readRDS('../application/deer_empr.rds')

# importing coyote curvival curves
coys_theo <- readRDS('../application/coys_theo.rds')
coys_empr <- readRDS('../application/coys_empr.rds')

gg_df <- data.frame(
  y = c(deer_theo, deer_empr, coys_theo, coys_empr),
  x = rep(cumsum(rep(1 / 24, times = length(coys_empr))), 4),
  species = rep(c('White-tailed deer', 'Coyote'), each = 2 * length(coys_empr)),
  curve = rep(c('Theoretical', 'Empirical'), each = length(coys_empr),
              times = 2)
)
gg_df$species <- factor(gg_df$species, levels=c("White-tailed deer","Coyote"))

ggplot(gg_df, aes(x, y, color = curve, linetype=curve)) +
  facet_wrap(.~species, scales = 'free_y') +
  geom_line(size=1) +
  ylab('Survival probability') +
  xlab('Days camera deployed') +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=c('#999999','#333333')) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title=element_text(size=16),
        axis.text=element_text(size=14),
        legend.title=element_blank(),
        legend.text=element_text(size=14),
        legend.spacing.x = unit(0.3, 'cm'),
        legend.position=c(0.3,0.9),
        strip.background=element_rect("white"),
        strip.text=element_text(size=16))

ggsave('fig1_fit.tiff', compression='lzw', height=5, width=7)
