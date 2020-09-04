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
color_key <- data.frame(color_group = c("darkgreen", "black", "blue", "black", "darkorange", "black", "black", 
                                        "black", "black", "blue", "black"), SppCode = unique(sppcov$Species))

#order:  ATPA BDCR CICI GASA GEGE HEIC HEPA ICAL LESE MECA MUMU
# marsh mongoose, bushy-tailed, civet, slender, genet, large grey, dwarf, white-tailed, serval, honey badger, banded

data_to_plot <- sppcov %>% 
    dplyr::rename(SppCode = Species) %>% 
    left_join(Spp) %>% 
    left_join(factor_key) %>% 
    left_join(color_key) %>%
    mutate(Factor_Full = fct_reorder(Factor_Full, Levels_Factor)) 

# make the distance values negative so that they can be 'proximity' values
for(i in 1:nrow(data_to_plot)) {
    if(data_to_plot[i,1] %in% c("urema.distance")) {
        data_to_plot[i,3] <- data_to_plot[i,3] * -1
        data_to_plot[i,5] <- data_to_plot[i,5] * -1
        data_to_plot[i,6] <- data_to_plot[i,6] * -1
    }
}

# PLOT
#genet: darkorange, 1
#civet: blue, 2
#honey badger: red, 3
#marsh mongoose: darkgreen, 4

# Make theme
sppcov_theme <-     theme(axis.title.y = element_blank(),
                     #     legend.title = element_blank(),
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

tree <-
    data_to_plot %>% 
    subset(Factor == "tree_hansen") %>% 
    mutate(CommName_Full = fct_reorder(CommName_Full, Mean)) %>% 
    ggplot(aes(x = CommName_Full, y = Mean)) + 
    geom_hline(aes(yintercept = 0), linetype="dashed", size = 0.5, color = "darkgrey") +
    geom_errorbar(aes(ymin = LCI, ymax = UCI), width=0, position = position_dodge(width = 1), alpha=.75) +
    geom_point(position = position_dodge(width = 1), size = 2) +
    sppcov_theme +
    coord_flip() + # switch x and y coordinates
    labs(y = "Beta Coefficient - Tree Density") 

# Lake -------------------------------------------------------------------

lake <-
    data_to_plot %>% 
    subset(Factor == "urema_dist") %>% 
    mutate(CommName_Full = fct_reorder(CommName_Full, Mean)) %>% 
    ggplot(aes(x = CommName_Full, y = Mean)) + 
    geom_hline(aes(yintercept = 0), linetype="dashed", size = 0.5, color = "darkgrey") +
    geom_errorbar(aes(ymin = LCI, ymax = UCI), width=0, position = position_dodge(width = 1), alpha=.75) +
    geom_point(position = position_dodge(width = 1), size = 2) +
    sppcov_theme +
    coord_flip() + # switch x and y coordinates
    labs(y = "Beta Coefficient - Lake Proximity") 
    
# Termite --------------------------------------------------------------------

termite <-
    data_to_plot %>% 
    subset(Factor == "termite.count.100m") %>% 
    mutate(CommName_Full = fct_reorder(CommName_Full, Mean)) %>% 
    ggplot(aes(x = CommName_Full, y = Mean)) + 
    geom_hline(aes(yintercept = 0), linetype="dashed", size = 0.5, color = "darkgrey") +
    geom_errorbar(aes(ymin = LCI, ymax = UCI), width=0, position = position_dodge(width = 1), alpha=.75) +
    geom_point(position = position_dodge(width = 1), size = 2) +
    sppcov_theme +
    coord_flip() + # switch x and y coordinates
    labs(y = "Beta Coefficient - Termite Mound Density") 


pdf("scripts/alternative-multispecies-model/figures//sppcov-termite.pdf", width = 6, height = 5)
termite
dev.off()

pdf("scripts/alternative-multispecies-model/figures//sppcov-tree.pdf", width = 6, height = 5)
tree
dev.off()

pdf("scripts/alternative-multispecies-model/figures//sppcov-lake.pdf", width = 6, height = 5)
lake
dev.off()

#trying to plot them together
ggarrange(tree, lake, termite, ncol = 2, nrow = 2)
ggarrange(civet_lake, civet_termite, honey_badger_lake, honey_badger_tree, genet_lake, marsh_mongoose_termite, 
          ncol = 2, nrow = 3, labels = c("civet", "", "honey badger", "", "genet", "marsh mongoose"))
