library(tidyverse)
library(here)
library(ggpubr)

# read in model output - modeled species richness at each camera trap site
site.richness <- read.csv(here::here("scripts/alternative-multispecies-model/results/mesocarnivore_spprich.csv"))

# bring in associated site metadata
site.metadata <- read.csv("data/gorongosa-cameras/cam_metadata_fromfield_and_raw_raster.csv")

# merge modeled species richness and site metadata
site.richness <- left_join(site.richness, site.metadata)

# plot species richness vs termite mound count
termite <- ggplot(site.richness, aes(x = termite.count.100m, y = Mean)) + 
    geom_errorbar(aes(ymin=LCI, ymax=UCI), width=0, colour = "lightgrey") +
    geom_point() + 
    geom_smooth(method = lm, se = F, col = "black", linetype = "dashed", size = 0.5) +
    xlab("Termite Mound Density (mounds w/in 100m)") +
    ylab("Mesocarnivore Richness") + 
    theme_bw() 

# plot species richness vs Urema distance
lake <- ggplot(site.richness, aes(x = urema_dist/1000, y = Mean)) + 
    geom_errorbar(aes(ymin=LCI, ymax=UCI), width=0, colour = "lightgrey") +
    geom_point() + 
    geom_smooth(method = lm, se = F, col = "black", linetype = "dashed", size = 0.5) +
    xlab("Proximity to Lake Urema (km)") +
    ylab("Mesocarnivore Richness") + 
    theme_bw() +
    scale_x_reverse() 

# plot species richness vs tree cover
tree <- ggplot(site.richness, aes(x = tree_hansen, y = Mean)) +
    geom_errorbar(aes(ymin=LCI, ymax=UCI), width=0, colour = "lightgrey") +
    geom_point() + 
    geom_smooth(method = lm, se = F, col = "black", linetype = "dashed", size = 0.5) +
    xlab("Tree Cover (%)") +
    ylab("Mesocarnivore Richness") + 
    theme_bw()

pdf("scripts/alternative-multispecies-model/figures/richness-covariates.pdf", width = 10, height = 6, useDingbats = FALSE)
ggarrange(tree, lake, termite)
dev.off()
