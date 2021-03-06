library(tidyverse)
library(here)
library(patchwork)
library(ggpubr)

# PREP DATA

# bring in group covariate means for model with groups
sppcov <- read.csv(here::here("scripts/alternative-multispecies-model/results/mesocarnivore_species.csv"))

# bring in species (for full common names and groups)
Spp <- read.csv("data/gorongosa-cameras/2018spp_kingdon.csv") # Species IDs and traits

# make keys for changing factor levels and names (not sure if a better way...)
factor_key <- data.frame(Levels_Factor = 1:3,
                         Factor = c("urema.distance", "tree.hansen", "termite.count.100m"),
                         Factor_Full = c("Lake Proximity", "Tree Cover", "Termite Mound Density"))

#order:  ATPA BDCR CICI GASA GEGE HEIC HEPA ICAL LESE MECA MUMU
# marsh mongoose, bushy-tailed, civet, slender, genet, large grey, dwarf, white-tailed, serval, honey badger, banded

data_to_plot <- sppcov %>% 
    dplyr::rename(SppCode = Species) %>% 
    left_join(Spp) %>% 
    left_join(factor_key) %>% 
    mutate(Factor_Full = fct_reorder(Factor_Full, Levels_Factor))

#assigning a color to each species
colkey <- data.frame(color_code = c("A", "B", "C", "B", "D", "B", "B", 
                                           "B", "B", "E", "B"),
                     SppCode = unique(data_to_plot$SppCode))
data_to_plot <- left_join(data_to_plot, colkey)

color_codes <- c("darkgreen", "black", "blue", "darkorange", "red")

# PLOT
#genet: darkorange, 1
#civet: blue, 2
#honey badger: red, 3
#marsh mongoose: darkgreen, 4

# Make theme
sppcov_theme <-     theme(axis.title.y = element_blank(),
                          legend.position = "none",
                          legend.key = element_rect(fill = alpha("white", 0.0)),
                          strip.background = element_blank(),
                          strip.text = element_blank(),
                          panel.grid.major = element_blank(),
                          panel.background = element_blank(),
                          axis.ticks.y = element_blank(),
                          axis.text.y = element_text(size=rel(1.25)),
                          panel.border = element_rect(color = "black", fill = NA, size = .5),
                          legend.text=element_text(size=rel(1)))

# Tree --------------------------------------------------------------------

tree_occ <-
    data_to_plot %>% 
    subset(Factor == "tree_hansen") %>% 
    mutate(CommName_Full = fct_reorder(CommName_Full, Mean)) %>% 
    ggplot(aes(x = CommName_Full, y = Mean, colour = color_code)) + 
    geom_hline(aes(yintercept = 0), linetype="dashed", size = 0.5, color = "darkgrey") +
    geom_errorbar(aes(ymin = LCI, ymax = UCI), width=0, position = position_dodge(width = 1), alpha=.75) +
    geom_point(position = position_dodge(width = 1), size = 3) +
    scale_color_manual(values=color_codes) +
    sppcov_theme +
    coord_flip() + # switch x and y coordinates
    labs(y = "Beta Coefficient") +
    ggtitle("Tree Cover") +
    theme(plot.title = element_text(hjust = 0.5))

# Lake -------------------------------------------------------------------

lake_occ <-
    data_to_plot %>% 
    subset(Factor == "urema_dist") %>% 
    mutate(CommName_Full = fct_reorder(CommName_Full, Mean)) %>% 
    ggplot(aes(x = CommName_Full, y = Mean, colour = color_code)) + 
    geom_hline(aes(yintercept = 0), linetype="dashed", size = 0.5, color = "darkgrey") +
    geom_errorbar(aes(ymin = LCI, ymax = UCI), width=0, position = position_dodge(width = 1), alpha=.75) +
    geom_point(position = position_dodge(width = 1), size = 3) +
    scale_color_manual(values=color_codes) +
    sppcov_theme +
    coord_flip() + # switch x and y coordinates
    labs(y = "Beta Coefficient") +
    ggtitle("Distance to Lake") +
    theme(plot.title = element_text(hjust = 0.5))
    
# Termite --------------------------------------------------------------------

termite_occ <-
    data_to_plot %>% 
    subset(Factor == "termite.count.100m") %>% 
    mutate(CommName_Full = fct_reorder(CommName_Full, Mean)) %>% 
    ggplot(aes(x = CommName_Full, y = Mean, colour = color_code)) + 
    geom_hline(aes(yintercept = 0), linetype="dashed", size = 0.5, color = "darkgrey") +
    geom_errorbar(aes(ymin = LCI, ymax = UCI), width=0, position = position_dodge(width = 1), alpha=.75) +
    geom_point(position = position_dodge(width = 1), size = 3) +
    scale_color_manual(values=color_codes) +
    sppcov_theme +
    coord_flip() + # switch x and y coordinates
    labs(y = "Beta Coefficient") + 
    ggtitle("Termite Mound Density") +
    theme(plot.title = element_text(hjust = 0.5))


pdf("scripts/alternative-multispecies-model/figures//sppcov-termite.pdf", width = 6, height = 5)
termite_occ
dev.off()

pdf("scripts/alternative-multispecies-model/figures//sppcov-tree.pdf", width = 6, height = 5)
tree_occ
dev.off()

pdf("scripts/alternative-multispecies-model/figures//sppcov-lake.pdf", width = 6, height = 5)
lake_occ
dev.off()

#plot together and save
pdf("scripts/alternative-multispecies-model/figures/sppcov-all.pdf", width = 4, height = 7)
ggarrange(tree_occ, lake_occ, termite_occ, ncol = 1, nrow = 3)
dev.off()