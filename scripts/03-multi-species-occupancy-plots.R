library(tidyverse)
library(stringr)
library(here)

# PLOT
pdf("figures/species-covariates.pdf", width = 7, height = 6)
ggplot(data_to_plot, aes(x = Factor_Full, y = Mean, col = Group_Full)) + 
  geom_hline(aes(yintercept = 0), linetype="dashed", size = 0.5, color = "darkgrey") +
  geom_errorbar(aes(ymin = LCI, ymax = UCI), width=0, position = position_dodge(width = 1), alpha=.75) +
  geom_point(position = position_dodge(width = 1), size = 2) +
  theme(axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.key = element_rect(fill = alpha("white", 0.0)),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(size=rel(1.25)),
        panel.border = element_rect(color = "black", fill = NA, size = .5),
        legend.text=element_text(size=rel(1))
  ) +
  coord_flip() + # switch x and y coordinates
  ylab("Beta Coefficient") +
  scale_color_manual(values = c("#1e9b56", "#99a82b", "#f8be1d", "#f27229", "#ea3633", "black"),
                     guide = guide_legend(reverse = TRUE)) +
  facet_grid(Factor_Full~., scales = "free") # facet grid helps to space out the factors a bit more
dev.off()