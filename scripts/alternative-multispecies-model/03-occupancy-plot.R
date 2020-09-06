library(ggplot2)
library(tidyverse)
library(dplyr)
library(plyr)
library(stringr)
library(here)
library(paletteer)

# bring in and format occupancy values and CIs
alpha <- read.csv(here::here("scripts/alternative-multispecies-model/results/mesocarnivore_alphaspsi()p().csv"))
alpha.ci <- read.csv(here::here("scripts/alternative-multispecies-model/results/mesocarnivore_alphaCI.psi()p().csv")) %>% t() # transpose as it's read in
alpha <- cbind(alpha, alpha.ci) %>% rownames_to_column(var = "SppCode") # merge the alphas with the confidence intervals and change row names into first column
names(alpha) <- c("SppCode", "alpha_occupancy", "alpha_025", "alpha_10", "alpha_50", "alpha_90", "alpha_975") # change names to match code

# bring in species (for full common names)
Spp <- read.csv("data/gorongosa-cameras/2018spp_kingdon.csv") # Species IDs and traits

# combine and reorder factors
data_to_plot <- alpha %>% 
    left_join(Spp) %>% 
    mutate(CommName_Full = fct_reorder(CommName_Full, alpha_occupancy))

#assigning a color to each species
colkey <- data.frame(color_code = c("A", "B", "C", "B", "D", "B", "B", 
                                    "B", "B", "E", "B"),
                     SppCode = unique(data_to_plot$SppCode))
data_to_plot <- left_join(data_to_plot, colkey)

color_codes <- c("darkgreen", "black", "blue", "darkorange", "red")

# NOT facet-wrapped
pdf("scripts/alternative-multispecies-model/figures/occupancy-by-species.pdf", width = 6, height = 5)
ggplot(data_to_plot, aes(x = CommName_Full, y = alpha_occupancy, colour = color_code)) + 
    geom_errorbar(aes(ymin=alpha_025, ymax=alpha_975, width=.1)) +
    geom_line() +
    geom_point(size = 2) +
    ylab("Occupancy Probability") +
    theme(axis.title.y = element_blank(),
          #legend.title = element_blank(),
          #legend.key = element_rect(fill = alpha("white", 0.0)),
          strip.background = element_blank(),
          strip.text = element_text(size = rel(1)),
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_text(size=rel(1.25)),
          panel.border = element_rect(color = "black", fill = NA, size = .5),
          #legend.text=element_text(size=rel(1)),
          #legend.position = c(.7, .15),
          #legend.box.background = element_rect(color="black", size=.5)
    ) +
    theme(legend.position="none") +
    scale_color_manual(values=color_codes) +
    coord_flip() 
    #scale_color_manual(values = c("#ea3633", "#f27229", "#f8be1d","#99a82b","#1e9b56")) 
dev.off()
