


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

#Hclust ordereing
order.file <- "Intermediates/Region_hclust_results.csv"






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
p.pcareg.plot <- ggplot(p.pcareg.dat, aes(x = cor, y = 1)) +
  geom_jitter(height = 0, width = 0.01, size = 3, shape = 21, alpha = 0.6, fill = "#F5A622") +
  scale_x_continuous(
    limits = c(-1, 1),
    breaks = ticks$x,
    labels = ticks$x) +
  scale_y_continuous(
    limits = c(0.5,3),
    breaks = 1,
    labels = NULL) +
  theme_bw() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_line(color = "black", linewidth = 0.2),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.border = element_blank(),
    axis.line.x = element_line(color = "black", linewidth = 0.2)) +
  labs(x = "Correlation with PC1") +
#  coord_fixed(ratio = 0.1) +
  geom_text_repel(data = p.pcareg.dat, aes(x = cor, label = param),
                  size = 3,
                  min.segment.length = 0.01, 
                  point.padding = 0.5, 
                  segment.curvature = -0.1e-20,
                  box.padding = 1,
                  segment.size = 0.25,
                  force = 0.1,
                  ylim = c(1.1, NA))  +
  ggtitle("Particulate") +
  theme(plot.title = element_text(hjust = 0.5)) 
p.pcareg.plot







#dissolved
d.pcareg.plot <- ggplot(d.pcareg.dat, aes(x = cor, y = 1)) +
  geom_jitter(height = 0, width = 0.01, size = 3, shape = 21, alpha = 0.6, fill = "#F5A622") +
  scale_x_continuous(
    limits = c(-1, 1),
    breaks = ticks$x,
    labels = ticks$x) +
  scale_y_continuous(
    limits = c(0.5,3),
    breaks = 1,
    labels = NULL) +
  theme_bw() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_line(color = "black", linewidth = 0.2),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.border = element_blank(),
    axis.line.x = element_line(color = "black", linewidth = 0.2)) +
  labs(x = "Correlation with PC1") +
 # coord_fixed(ratio = 0.1) +
  geom_text_repel(data = d.pcareg.dat, aes(x = cor, label = param),
                  size = 3,
                  min.segment.length = 0.01, 
                  point.padding = 0.5, 
                  segment.curvature = -0.1e-20,
                  box.padding = 1,
                  segment.size = 0.25,
                  force = 0.1,
                  ylim = c(1.1, NA)) +
  ggtitle("Dissolved") +
  theme(plot.title = element_text(hjust = 0.5)) 
d.pcareg.plot



###______Comound correlation with POC number lines_______________

#load data:
p.pocreg.dat <- read_csv(p.pocreg.file) %>%
  left_join(., compound.order)
d.pocreg.dat <- read_csv(d.pocreg.file) %>%
  left_join(., compound.order)



#particulate
p.pocreg.plot <-  ggplot(p.pocreg.dat, aes(x = cor, y = reorder(class, -order))) +
  geom_point(size = 3, shape = 21, alpha = 0.7, aes(fill = class)) +
  scale_x_continuous(
    limits = c(-1, 1),
    breaks = ticks$x,
    labels = ticks$x) +
  theme_bw() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_line(color = "black", linewidth = 0.2),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.border = element_blank(),
    axis.line.x = element_line(color = "black", linewidth = 0.2)) +
  labs(x = "Correlation with POC") +
  scale_fill_manual(values = class.pal) #+
 # coord_fixed(ratio = 0.2)
p.pocreg.plot


#dissolved
d.pocreg.plot <-  ggplot(d.pocreg.dat, aes(x = cor, y = reorder(class, -order))) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  # geom_hline(yintercept )
  geom_point(size = 3, shape = 21, alpha = 0.7, aes(fill = class)) +
  scale_x_continuous(
    limits = c(-1, 1),
    breaks = ticks$x,
    labels = ticks$x) +
  #  scale_y_continuous(limits = c(-0.05, 0.3)) +
  theme_bw() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_line(color = "black", linewidth = 0.2),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.border = element_blank(),
    axis.line.x = element_line(color = "black", linewidth = 0.2)) +
  labs(x = "Correlation with POC") +
  scale_fill_manual(values = class.pal) +
 # coord_fixed(ratio = 0.2) +
  theme(legend.position = "none")
d.pocreg.plot





####_______Mean rank heatmaps_____________

#load in order:
clust.order.dat <- read_csv(order.file)

##Load in data:
p.rankmean.dat <- read_csv(p.rankmean.file) %>%
  group_by(compound.name.figure) %>%
  mutate(overall.rank = mean(mean.rank)) %>%
  left_join(., clust.order.dat %>% 
              filter(phase == "part", param == "rank") %>% 
              select(Region, order))

d.rankmean.dat <- read_csv(d.rankmean.file) %>%
  group_by(compound.name.figure) %>%
  mutate(overall.rank = mean(mean.rank)) %>%
  left_join(., clust.order.dat %>% 
              filter(phase == "diss", param == "rank") %>% 
              select(Region, order))


### particulate
p.rankmean.plot <- ggplot(p.rankmean.dat, aes(x = reorder(Region, order), y = reorder(compound.name.figure, -overall.rank), fill = mean.rank)) +
  geom_tile(color = "black", size = 0.05) +
  scale_fill_viridis(direction = -1, option = "G") +
  coord_fixed() +
  labs(fill = "Mean Rank") +
  ylab("Compound") +
  xlab("Region") +
  scale_x_discrete(position = "top") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0)) +
  ggtitle("Particulate") +
  theme(plot.title = element_text(hjust = 0.5)) 
p.rankmean.plot


### dissolved
d.rankmean.plot <- ggplot(d.rankmean.dat, aes(x = reorder(Region, order), y = reorder(compound.name.figure, -overall.rank), fill = mean.rank)) +
  geom_tile(color = "black", size = 0.05) +
  scale_fill_viridis(direction = -1, option = "G", limits = c(0,33)) +
  coord_fixed() +
  labs(fill = "Rank") +
  ylab("Compound") +
  xlab("Region") +
  scale_x_discrete(position = "top") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0)) +
  theme(legend.position = "none") +
  ggtitle("Dissolved") +
  theme(plot.title = element_text(hjust = 0.5)) 
d.rankmean.plot








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
  ggtitle("Particulate") +
  theme(plot.title = element_text(hjust = 0.5)) 
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
  ggtitle("Dissolved") +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5)) 
d.rankcorr.plot 






###combine all plots into a single figure:


combination.plot <- (free(p.pca.plot) + plot_spacer() + (p.pcareg.plot + p.pocreg.plot + plot_layout(ncol = 1, heights = c(1.1,2))) +
                       plot_layout(nrow = 1, widths = c(0.4, 0.1, 0.5))) /
                    plot_spacer() /
                    (free(d.pca.plot) + plot_spacer() + (d.pcareg.plot + d.pocreg.plot + plot_layout(ncol = 1, heights = c(1.1,2))) +
                       plot_layout(nrow = 1, widths = c(0.4, 0.1, 0.5))) /
                    plot_spacer() /
                    (p.rankmean.plot + d.rankmean.plot + plot_spacer() + (p.rankcorr.plot / d.rankcorr.plot) + plot_layout(widths = c(1.3,  1,  0.3, 2))) +
  plot_layout(guides = "collect", heights = c(1.5, 0.01, 1.5, 0.1, 3.5)) +
  plot_annotation(tag_levels = "a")
                    
combination.plot

ggsave(combination.plot, filename = "Figures/Output_Oct25/Multivariate_Composition_Plot.pdf", dpi = 900,
       scale = 1.5, height = 9.5, width = 7.5)


#















###Archived code:
# p.pcareg.plot <- ggplot(p.pcareg.dat, aes(x = cor, y = 0)) +
#   geom_hline(yintercept = 0, linewidth = 0.3) +
#   geom_segment(data = ticks,
#                aes(x = x, xend = x, y = -0.02, yend = 0.02),
#                linewidth = 0.3) +
#   geom_jitter(height = 0, width = 0.01, size = 3, shape = 21, alpha = 0.6, fill = "#F5A622") +
#   scale_x_continuous(
#     limits = c(-1, 1),
#     breaks = ticks$x,
#     labels = ticks$x) +
#   scale_y_continuous(limits = c(-0.05, 0.3)) +
#   theme_minimal(base_size = 14) +
#   theme(
#     axis.title.y = element_blank(),
#     axis.text.y  = element_blank(),
#     axis.ticks.y = element_blank(),
#     panel.grid   = element_blank()) +
#   labs(x = "Correlation with PC1") +
#   geom_text_repel(data = p.pcareg.dat, aes(x = cor, label = param),
#                   size = 3,
#                   min.segment.length = 0.01, 
#                   point.padding = 0.5, 
#                   segment.curvature = -0.1e-20,
#                   box.padding = 1,
#                   segment.size = 0.25,
#                   force = 0.1,
#                   ylim = c(0.1, NA)) 
# p.pcareg.plot   














