


#Script for Multivariate Composition Plot:


#library
library(tidyverse)
library(viridis)
library(patchwork)
library(ggrepel)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")




###define inputs

#PCAs
p.pca.file <- "Intermediates/Part_PCA_with_metadata.csv"
d.pca.file <- "Intermediates/Diss_PCA_with_metadata.csv"

#PCA-Regressions
p.pcareg.file <- "Intermediates/Part_PCA_Regression_Output.csv"
d.pcareg.file <- "Intermediates/Diss_PCA_Regression_Output.csv"

#POC-Regressions
p.pocreg.file <- "Intermediates/Part_POC_Osmo_Regression_Output.csv"
d.pocreg.file <- "Intermediates/Diss_POC_Osmo_Regression_Output.csv"

#Rank Means
p.rankmean.file <- "Intermediates/Part_Region_Mean_Rank.csv"
d.rankmean.file <- "Intermediates/Diss_Region_Mean_Rank.csv"

#Rank Correlations 
p.rankcorr.file <- "Intermediates/Part_Region_Rank_Corr_Output.csv"
d.rankcorr.file <- "Intermediates/Diss_Region_Rank_Corr_Output.csv"





#######Load in data for PCAs

p.pca.dat <- read_csv(p.pca.file)
d.pca.dat <- read_csv(d.pca.file)



###___________Plot PCAs________________

#Particulate:
p.pca.plot <- ggplot(p.pca.dat, aes(x = PC1, y = PC2)) +
  geom_point(aes(fill = Region, shape = Cruise), stroke = 0.15, size = 3, shape = 21) +
  # scale_shape_manual(values = c(22, 24, 21)) +
  #  scale_shape_manual(values = c(22, 21, 23)) +
  scale_fill_manual(values = region.palette.7) +
  ylab("PC2 (11%)") +
  xlab("PC1 (68%)") +
  theme_test() +
  # theme(panel.border = element_blank(), axis.line = element_line(), 
  #       axis.text.x = element_blank(), axis.title.x = element_blank(), 
  #      plot.title = element_text(hjust = 0.5)) +
  ggtitle("Particulate") +
  theme(plot.title = element_text(hjust = 0.5)) 
p.pca.plot



#Dissolved:
d.pca.plot <- ggplot(d.pca.dat, aes(x = PC1, y = PC2)) +
  geom_point(aes(fill = Region, shape = Cruise), stroke = 0.15, size = 3, shape = 21) +
  # scale_shape_manual(values = c(22, 24, 21)) +
  #  scale_shape_manual(values = c(22, 21, 23)) +
  scale_fill_manual(values = region.palette.7) +
  ylab("PC2 (10%)") +
  xlab("PC1 (50%)") +
  theme_test() +
  # theme(panel.border = element_blank(), axis.line = element_line(), 
  #       axis.text.x = element_blank(), axis.title.x = element_blank(), 
  #      plot.title = element_text(hjust = 0.5)) +
  ggtitle("Dissolved") +
  theme(plot.title = element_text(hjust = 0.5))
d.pca.plot





#____________Plot PCA Regressions:_______________

#load in data
p.pcareg.dat <- read_csv(p.pcareg.file) 
d.pcareg.dat <- read_csv(d.pcareg.file)

ticks <- tibble(x = c(-1, -0.5, 0, 0.5, 1))

#particulate
p.pcareg.plot <- ggplot(p.pcareg.dat, aes(x = cor, y = 0)) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_segment(data = ticks,
               aes(x = x, xend = x, y = -0.02, yend = 0.02),
               linewidth = 0.3) +
  geom_jitter(height = 0, width = 0.01, size = 3, shape = 21, alpha = 0.6, fill = "#F5A622") +
  scale_x_continuous(
    limits = c(-1, 1),
    breaks = ticks$x,
    labels = ticks$x) +
  scale_y_continuous(limits = c(-0.05, 0.3)) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.y = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid   = element_blank()) +
  labs(x = "Correlation with PC1") +
  geom_text_repel(data = p.pcareg.dat, aes(x = cor, label = param),
                  size = 3,
                  min.segment.length = 0.01, 
                  point.padding = 0.5, 
                  segment.curvature = -0.1e-20,
                  box.padding = 1,
                  segment.size = 0.25,
                  force = 0.1,
                  ylim = c(0.1, NA)) 
p.pcareg.plot   


#dissolved
d.pcareg.plot <- ggplot(d.pcareg.dat, aes(x = cor, y = 0)) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_segment(data = ticks,
               aes(x = x, xend = x, y = -0.03, yend = 0.03),
               linewidth = 0.3) +
  geom_jitter(height = 0, width = 0.01, size = 3, shape = 21, alpha = 0.6, fill = "#F5A622") +
  scale_x_continuous(
    limits = c(-1, 1),
    breaks = ticks$x,
    labels = ticks$x) +
  scale_y_continuous(limits = c(-0.05, 0.3)) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.y = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid   = element_blank()) +
  labs(x = "Correlation with PC1") +
  geom_text_repel(data = d.pcareg.dat, aes(x = cor, label = param),
                  size = 3,
                  min.segment.length = 0.01, 
                  point.padding = 0.5, 
                  segment.curvature = -0.1e-20,
                  box.padding = 1,
                  segment.size = 0.25,
                  force = 0.1,
                  ylim = c(0.1, NA)) 
d.pcareg.plot   



###______Mean XYZ number lines_______________






####_______Mean rank heatmaps_____________







####_____Mean Rank Correlation Heatmaps_____
p.rankcorr.dat <- read_csv(p.rankcorr.file) %>%
  filter(Region.1 < Region.2) 
d.rankcorr.dat <- read_csv(d.rankcorr.file) %>%
  filter(Region.1 < Region.2) 


##Particulate
p.rankcorr.plot <- ggplot(p.rankcorr.dat, aes(x = Region.1, y = Region.2, fill = cor)) +
  geom_tile(color = "black") +
  coord_fixed() +
  theme_minimal() +
  scale_fill_gradient2(
    low = "steelblue",
    mid = "white",
    high = "#F5A622",
    limits = c(0.5, 1),
    midpoint = 0.75
  ) +
  scale_x_discrete(position = "top") +
  geom_text(aes(label = cor)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 0)) +
  labs(fill = "Correlation of\nMean Rank") +
  ggtitle("Particulate")
p.rankcorr.plot


##Dissolved
d.rankcorr.plot <- ggplot(d.rankcorr.dat, aes(x = Region.1, y = Region.2, fill = cor)) +
  geom_tile(color = "black") +
  coord_fixed() +
  theme_minimal() +
  scale_fill_gradient2(
    low = "steelblue",
    mid = "white",
    high = "#F5A622",
    limits = c(0.5, 1),
    midpoint = 0.75
  ) +
  scale_x_discrete(position = "top") +
  geom_text(aes(label = cor)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 0)) +
  labs(fill = "Correlation of\nMean Rank") +
  ggtitle("Dissolved")
d.rankcorr.plot 













































