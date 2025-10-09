



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
  filter(Long < -129) %>%
  mutate(lat_bin = cut(Lat, breaks = lat.breaks, include.lowest = TRUE, right = FALSE)) %>%
  group_by(lat_bin, Cruise, compound.name.figure, order) %>%
  reframe(Mean.Part.nM = mean(Part.Conc.nM, na.rm = TRUE),
          Mean.Diss.nM = mean(Diss.Conc.nM.adj, na.rm = TRUE),
          Mean.Total.nM = mean(Total.Conc.nM, na.rm = TRUE)) %>%
  mutate(Cruise = as.factor(Cruise)) %>%
  mutate(Cruise = fct_relevel(Cruise, c("TN397", "KM1906")))

G4.Leg1.dat <- dat %>%
  filter(Cruise %in% c("TN397")) %>%
  filter(Long > -129) %>%
  mutate(Station = case_when(str_detect(Diss.SampID, "U1") ~ "U1",
                              str_detect(Diss.SampID, "U2") ~ "U2",
                              str_detect(Diss.SampID, "U3") ~ "U3",
                              str_detect(Diss.SampID, "U4") ~ "U4",
                              str_detect(Diss.SampID, "U5") ~ "U5",
                              str_detect(Diss.SampID, "U6") ~ "U6",
                              str_detect(Diss.SampID, "S1") ~ "S1",
                              str_detect(Diss.SampID, "S2") ~ "S2")) %>%
  group_by(Station, Long, Cruise, compound.name.figure, order) %>%
  reframe(Mean.Part.nM = mean(Part.Conc.nM, na.rm = TRUE),
          Mean.Diss.nM = mean(Diss.Conc.nM.adj, na.rm = TRUE),
          Mean.Total.nM = mean(Total.Conc.nM, na.rm = TRUE)) %>%
  mutate(Percent_Dissolved = 100*Mean.Diss.nM/Mean.Total.nM)






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
  labs(fill = "Compound") +
  facet_grid(.~Cruise, scales = "free", space = "free")
g.p.rel.plot

#Make particulate plot
g.p.abs.plot <- ggplot(gradients.dat, aes(x = lat_bin, y=Mean.Part.nM, fill = reorder(compound.name.figure, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15) +
  scale_fill_manual(values = compound.pal.fig)+
  guides(fill = guide_legend(ncol = 1)) +
  #  scale_fill_manual(values = stepped2(n = 20)) +
  scale_y_continuous(expand = c(0,NA,NA,NA)) +
  theme_test() +
  theme(axis.text.x = element_text(angle=90)) +
  theme(legend.key.size = unit(0.4, "cm"), legend.text = element_text(size = 6)) +
  ylab("Concentration (nM)") +
  xlab("Latitude") +
  labs(fill = "Compound") +
  facet_grid(.~Cruise, scales = "free", space = "free")
g.p.abs.plot

#Make particulate plot
g.leg1.p.rel.plot <- ggplot(G4.Leg1.dat, aes(x = reorder(Station, Long), y=Mean.Part.nM, fill = reorder(compound.name.figure, order))) +
  geom_col(alpha = 0.9, width = 0.5, color = "black", size = 0.15, position = "fill") +
  scale_fill_manual(values = compound.pal.fig)+
  guides(fill = guide_legend(ncol = 1)) +
  #  scale_fill_manual(values = stepped2(n = 20)) +
  scale_y_continuous(expand = c(0,NA,NA,NA)) +
  theme_test() + 
  theme(axis.text.y = element_blank()) +
  theme(legend.key.size = unit(0.4, "cm"), legend.text = element_text(size = 6)) +
  ylab("Particulate Mole Fraction") +
  xlab("Station") +
  labs(fill = "Compound") 
g.leg1.p.rel.plot

#Make particulate plot
g.leg1.p.abs.plot <- ggplot(G4.Leg1.dat, aes(x = reorder(Station, Long), y=Mean.Part.nM, fill = reorder(compound.name.figure, order))) +
  geom_col(alpha = 0.9, width = 0.5, color = "black", size = 0.15) +
  scale_fill_manual(values = compound.pal.fig)+
  guides(fill = guide_legend(ncol = 1)) +
  #  scale_fill_manual(values = stepped2(n = 20)) +
  scale_y_continuous(expand = c(0,NA,NA,NA)) +
  theme_test() + 
  theme(axis.text.y = element_blank()) +
  theme(legend.key.size = unit(0.4, "cm"), legend.text = element_text(size = 6)) +
  ylab("Particulate Mole Fraction") +
  xlab("Station") +
  labs(fill = "Compound") 
g.leg1.p.abs.plot




ggsave(g.p.abs.plot, file = "Figures/Outputs/Transect_Abundance_Plots_pres.pdf", 
       height = 5.5, width = 9, scale = 1.2, dpi = 600)

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
  labs(fill = "Compound")  +
  facet_grid(.~Cruise, scales = "free", space = "free")
g.d.rel.plot

#Make particulate plot
g.leg1.d.rel.plot <- ggplot(G4.Leg1.dat, aes(x = reorder(Station, Long), y=Mean.Diss.nM, fill = reorder(compound.name.figure, order))) +
  geom_col(alpha = 0.9, width = 0.5, color = "black", size = 0.15, position = "fill") +
  scale_fill_manual(values = compound.pal.fig)+
  guides(fill = guide_legend(ncol = 1)) +
  #  scale_fill_manual(values = stepped2(n = 20)) +
  scale_y_continuous(expand = c(0,NA,NA,NA)) +
  theme_test() + 
  theme(axis.text.y = element_blank()) +
  theme(legend.key.size = unit(0.4, "cm"), legend.text = element_text(size = 6)) +
  ylab("Particulate Mole Fraction") +
  xlab("Station") +
  labs(fill = "Compound") 
g.leg1.d.rel.plot


#Make particulate plot
g.leg1.d.abs.plot <- ggplot(G4.Leg1.dat, aes(x = reorder(Station, Long), y=Mean.Diss.nM, fill = reorder(compound.name.figure, order))) +
  geom_col(alpha = 0.9, width = 0.5, color = "black", size = 0.15) +
  scale_fill_manual(values = compound.pal.fig)+
  guides(fill = guide_legend(ncol = 1)) +
  #  scale_fill_manual(values = stepped2(n = 20)) +
  scale_y_continuous(expand = c(0,NA,NA,NA)) +
  theme_test() + 
  theme() +
  theme(legend.key.size = unit(0.4, "cm"), legend.text = element_text(size = 6)) +
  ylab("Mean Dissolved Concentration (nM)") +
  xlab("Station") +
  labs(fill = "Compound") 
g.leg1.d.abs.plot

g.leg1.percent.plot <- ggplot(G4.Leg1.dat, aes(x = reorder(Station, Long), y=Percent_Dissolved, 
                                               color = reorder(compound.name.figure, order), group = compound.name.figure)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = compound.pal.fig)+
  guides(color = guide_legend(ncol = 1)) +
  #  scale_fill_manual(values = stepped2(n = 20)) +
  scale_y_continuous(expand = c(0,NA,NA,NA)) +
  theme_test() + 
  facet_wrap(.~compound.name.figure) +
  theme(legend.key.size = unit(0.4, "cm"), legend.text = element_text(size = 6)) 
g.leg1.percent.plot
###










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
  labs(fill = "Compound")  +
  facet_grid(.~Cruise, scales = "free", space = "free")
g.t.rel.plot

  












# DINIMITE Relative Abundance Plots

dinimite.dat <- dat %>%
  filter(Cruise %in% c("RC078")) %>%
  group_by(station, compound.name.figure, order) %>%
  reframe(Mean.Part.nM = mean(Part.Conc.nM, na.rm = TRUE),
          Mean.Diss.nM = mean(Diss.Conc.nM.adj, na.rm = TRUE),
          Mean.Total.nM = mean(Total.Conc.nM, na.rm = TRUE)) %>%
  filter(!station %in% c(1, 8))

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
    #    axis.title.x = element_blank(),
     #   axis.text.x = element_blank(),
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


ggsave(rel.abun.plots, file = "Figures/Outputs/Relative_Abundance_Plots.png", 
       height = 5, width = 8, scale = 1.4, dpi = 600)



###########_____XXX__________

rel.abun.plots.pres <- g.p.rel.plot + d.p.rel.plot + 
  g.d.rel.plot + d.d.rel.plot + 
  plot_layout(guides = "collect", widths = c(8, 2)) +
  plot_annotation(tag_levels = "A")
rel.abun.plots.pres

ggsave(rel.abun.plots.pres, file = "Figures/Outputs/Relative_Abundance_Plots_pres.pdf", 
       height = 6, width = 12, scale = 1.2, dpi = 600)











