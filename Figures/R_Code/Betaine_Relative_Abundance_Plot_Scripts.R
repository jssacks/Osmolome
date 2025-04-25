



library(tidyverse)
library(patchwork)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")


###define inputs
all.dat.file <- "Intermediates/Final_Osmo_Meta_Env_Dataframe.csv"



#Read in data
dat <- read_csv(all.dat.file)





# Relative Abundance Plots -------------------------------------------

####Gradient Survey Plot
lat.breaks <- seq(-3,50, by = 1)

gradients.dat <- dat %>%
  filter(Cruise %in% c("TN397", "KM1906")) %>%
  filter(Long < -124) %>%
  mutate(lat_bin = cut(Lat, breaks = lat.breaks, include.lowest = TRUE, right = FALSE)) %>%
  group_by(lat_bin, compound.name.figure, order, class) %>%
  reframe(Mean.Part.nM = mean(Part.Conc.nM, na.rm = TRUE),
          Mean.Diss.nM = mean(Diss.Conc.nM.adj, na.rm = TRUE),
          Mean.Total.nM = mean(Total.Conc.nM, na.rm = TRUE)) %>%
  filter(class == "Betaine")

##Concentration:

#Make particulate plot
g.p.rel.plot <- ggplot(gradients.dat, aes(x = lat_bin, y=Mean.Part.nM, fill = reorder(compound.name.figure, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15, position = "fill") +
  scale_fill_manual(values = compound.pal.fig)+
  guides(fill = guide_legend(ncol = 1)) +
  #  scale_fill_manual(values = stepped2(n = 20)) +
  scale_y_continuous(expand = c(0,NA,NA,NA)) +
  theme_test() + 
  theme(axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  theme(legend.key.size = unit(0.4, "cm"), legend.text = element_text(size = 6)) +
  ylab("Particulate Mole Fraction") +
  xlab("Latitude") +
  labs(fill = "Compound")
g.p.rel.plot


#Make dissolved plot
g.d.rel.plot <- ggplot(gradients.dat, aes(x = lat_bin, y=Mean.Diss.nM, fill = reorder(compound.name.figure, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15, position = "fill") +
  scale_fill_manual(values = compound.pal.fig)+
  guides(fill = guide_legend(ncol = 1)) +
  #  scale_fill_manual(values = stepped2(n = 20)) +
  scale_y_continuous(expand = c(0,NA,NA,NA)) +
  theme_test() + 
  theme(axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  theme(legend.key.size = unit(0.4, "cm"), legend.text = element_text(size = 6)) +
  ylab("Dissolved Mole Fraction") +
  xlab("Latitude") +
  labs(fill = "Compound")
g.d.rel.plot


#Make total plot
g.t.rel.plot <- ggplot(gradients.dat, aes(x = lat_bin, y=Mean.Total.nM, fill = reorder(compound.name.figure, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15, position = "fill") +
  scale_fill_manual(values = compound.pal.fig)+
  guides(fill = guide_legend(ncol = 1)) +
  #  scale_fill_manual(values = stepped2(n = 20)) +
  scale_y_continuous(expand = c(0,NA,NA,NA)) +
  theme_test() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.8),
        axis.text.y = element_blank()) +
  theme(legend.key.size = unit(0.4, "cm"), legend.text = element_text(size = 6)) +
  ylab("Total Mole Fraction") +
  xlab("Latitude") +
  labs(fill = "Compound")
g.t.rel.plot



# DINIMITE Relative Abundance Plots

dinimite.dat <- dat %>%
  filter(Cruise %in% c("RC078")) %>%
  group_by(station, compound.name.figure, order, class) %>%
  reframe(Mean.Part.nM = mean(Part.Conc.nM, na.rm = TRUE),
          Mean.Diss.nM = mean(Diss.Conc.nM.adj, na.rm = TRUE),
          Mean.Total.nM = mean(Total.Conc.nM, na.rm = TRUE)) %>%
  filter(!station %in% c(1, 8))  %>%
  filter(class == "Betaine")

#particulate_______________________________
d.p.rel.plot <- ggplot(dinimite.dat, aes(x = as.factor(station), y=Mean.Part.nM, fill = reorder(compound.name.figure, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15, position = "fill") +
  scale_fill_manual(values = compound.pal.fig)+
  guides(fill = guide_legend(ncol = 1)) +
  #  scale_fill_manual(values = stepped2(n = 20)) +
  scale_y_continuous(expand = c(0,NA,NA,NA)) +
  theme_test() + 
  theme(axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank()) +
  theme(legend.key.size = unit(0.4, "cm"), legend.text = element_text(size = 6)) +
  ylab("Mole Fraction") +
  xlab("Station") +
  labs(fill = "Compound")
d.p.rel.plot

#dissolved_______________________________
d.d.rel.plot <- ggplot(dinimite.dat, aes(x = as.factor(station), y=Mean.Diss.nM, fill = reorder(compound.name.figure, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15, position = "fill") +
  scale_fill_manual(values = compound.pal.fig)+
  guides(fill = guide_legend(ncol = 1)) +
  #  scale_fill_manual(values = stepped2(n = 20)) +
  scale_y_continuous(expand = c(0,NA,NA,NA)) +
  theme_test() + 
  theme(axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank()) +
  theme(legend.key.size = unit(0.4, "cm"), legend.text = element_text(size = 6)) +
  ylab("Mole Fraction") +
  xlab("Station") +
  labs(fill = "Compound")
d.d.rel.plot


#total_______________________________
d.t.rel.plot <- ggplot(dinimite.dat, aes(x = as.factor(station), y=Mean.Total.nM, fill = reorder(compound.name.figure, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15, position = "fill") +
  scale_fill_manual(values = compound.pal.fig)+
  guides(fill = guide_legend(ncol = 1)) +
  #  scale_fill_manual(values = stepped2(n = 20)) +
  scale_y_continuous(expand = c(0,NA,NA,NA)) +
  theme_test() + 
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank()) +
  theme(legend.key.size = unit(0.4, "cm"), legend.text = element_text(size = 6)) +
  ylab("Mole Fraction") +
  xlab("Station") +
  labs(fill = "Compound")
d.t.rel.plot



###########_____XXX__________

rel.abun.plots <- g.p.rel.plot + d.p.rel.plot + 
  g.d.rel.plot + d.d.rel.plot + 
  g.t.rel.plot + d.t.rel.plot + 
  plot_layout(guides = "collect", widths = c(7.5, 2.5)) +
  plot_annotation(tag_levels = "A")
rel.abun.plots


ggsave(rel.abun.plots, file = "Figures/Outputs/Betaine_Relative_Abundance_Plots.png", 
       height = 5, width = 8, scale = 1.4, dpi = 600)





