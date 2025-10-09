

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
  group_by(lat_bin, compound.name.figure, order) %>%
  reframe(Mean.Part.nM = mean(Part.Conc.nM, na.rm = TRUE),
          Mean.Diss.nM = mean(Diss.Conc.nM.adj, na.rm = TRUE),
          Mean.Total.nM = mean(Total.Conc.nM, na.rm = TRUE)) 

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

#Make particulate plot
g.p.abs.plot <- ggplot(gradients.dat, aes(x = lat_bin, y=Mean.Part.nM, fill = reorder(compound.name.figure, order))) +
  geom_col(alpha = 0.9, width = 0.6, color = "black", size = 0.10) +
  scale_fill_manual(values = compound.pal.fig)+
  guides(fill = guide_legend(ncol = 1)) +
  #  scale_fill_manual(values = stepped2(n = 20)) +
  scale_y_continuous(expand = c(0,NA,NA,NA), limits = c(0,35)) +
  theme_test() +
  theme(
        #axis.text.x = element_text(angle=90),
        axis.text.x = element_blank(),
        axis.title.x = element_blank()) +
  theme(legend.key.size = unit(0.4, "cm"), legend.text = element_text(size = 6)) +
  ylab("Concentration (nM)") +
  xlab("Latitude") +
  labs(fill = "Compound")
g.p.abs.plot

#ggsave(g.p.abs.plot, file = "Figures/Outputs/Transect_Abundance_Plots_pres.pdf", 
#       height = 5.5, width = 9, scale = 1.2, dpi = 600)

#Make dissolved plot
g.d.rel.plot <- ggplot(gradients.dat, aes(x = lat_bin, y=Mean.Diss.nM, fill = reorder(compound.name.figure, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15, position = "fill") +
  scale_fill_manual(values = compound.pal.fig)+
  guides(fill = guide_legend(ncol = 1)) +
  #  scale_fill_manual(values = stepped2(n = 20)) +
  scale_y_continuous(expand = c(0,NA,NA,NA)) +
  theme_test() + 
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.8)) +
  # axis.title.x = element_blank(),
  #  axis.text.x = element_blank()) +
  theme(legend.key.size = unit(0.4, "cm"), legend.text = element_text(size = 6)) +
  ylab("Dissolved Mole Fraction") +
  xlab("Latitude") +
  labs(fill = "Compound")
g.d.rel.plot

#Make dissolved plot
g.d.abs.plot <- ggplot(gradients.dat, aes(x = lat_bin, y=Mean.Diss.nM, fill = reorder(compound.name.figure, order))) +
  geom_col(alpha = 0.9, width = 0.6, color = "black", size = 0.10) +
  scale_fill_manual(values = compound.pal.fig)+
  guides(fill = guide_legend(ncol = 1)) +
  #  scale_fill_manual(values = stepped2(n = 20)) +
  scale_y_continuous(expand = c(0,NA,NA,125), limits = c(0, 115)) +
#  ylim(c(0,125)) +
  theme_test() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.8)) +
  # axis.title.x = element_blank(),
  #  axis.text.x = element_blank()) +
  theme(legend.key.size = unit(0.4, "cm"), legend.text = element_text(size = 6)) +
  ylab("Concentration (nM)") +
  xlab("Latitude") +
  labs(fill = "Compound")
g.d.abs.plot


###gradients absolute plots:
g.abs.comb <- g.p.abs.plot / g.d.abs.plot + 
  plot_layout(guides = "collect") 
g.abs.comb


###Combine both
g.rel.comb <- g.p.rel.plot / g.d.rel.plot + 
  plot_layout(guides = "collect") 
g.rel.comb




####

gradients.dat.2 <- dat %>%
  filter(Cruise %in% c("TN397", "KM1906")) %>%
  group_by(Part.SampID)
