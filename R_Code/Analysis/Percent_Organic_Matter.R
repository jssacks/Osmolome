



library(tidyverse)
library(patchwork)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")


###define inputs
all.dat.file <- "Intermediates/Final_Osmo_Meta_Env_Dataframe.csv"



#Read in data
dat <- read_csv(all.dat.file)



###Estimate Percent of POS concentrations and calculate Metabolite Percentage of POC, PON, and POS
percent.dat <- dat %>%
  mutate(pos.uM.est = poc*(1.3/124),
         metab.perc.c = (Part.C.nM/(poc*1000))*100,
         metab.perc.n = (Part.N.nM/(pn*1000))*100,
         metab.perc.s = (Part.S.nM/(pos.uM.est*1000))*100) %>%
  filter(is.na(Part.Flag)) %>%
  filter(!is.na(poc))


percent.sum <- percent.dat %>%
  select(Part.SampID, Compound, Cruise, Lat, Long, station, depth_m, Region, metab.perc.c, metab.perc.n, metab.perc.s) %>%
  unique() %>%
  group_by(Part.SampID, Cruise, Lat, Long, station, depth_m, Region) %>%
  reframe(metab.perc.c = sum(metab.perc.c, na.rm = TRUE),
          metab.perc.n = sum(metab.perc.n, na.rm = TRUE),
          metab.perc.s = sum(metab.perc.s, na.rm = TRUE)) %>%
  filter(Cruise %in% c("TN397", "KM1906", "RC078")) %>%
  filter(depth_m < 16) %>%
  filter(metab.perc.c < 10) 

###_________
gradients.sum <- percent.sum %>%
  filter(Cruise %in% c("TN397", "KM1906"))

####________
d.sum <- percent.sum %>%
  filter(Cruise %in% c("RC078")) %>%
  filter(!station == 1)


###Make Gradients Plots
g.perc.C <- ggplot(gradients.sum, aes(x = Lat, y = metab.perc.c, fill = Region)) + 
  geom_point(shape = 21, color = "black", stroke = 0.15, size = 2.5) +
  theme_bw() +
  scale_fill_manual(values = region.palette.2) +
  xlab("Latitude") +
  ylab("Percent of POC (%)")
g.perc.C

g.perc.N <- ggplot(gradients.sum, aes(x = Lat, y = metab.perc.n, fill = Region)) + 
  geom_point(shape = 21, color = "black", stroke = 0.15, size = 2.5) +
  theme_bw() +
  scale_fill_manual(values = region.palette.2)  +
  xlab("Latitude") +
  ylab("Percent of PON (%)")
g.perc.N

g.perc.S <- ggplot(gradients.sum, aes(x = Lat, y = metab.perc.s, fill = Region)) + 
  geom_point(shape = 21, color = "black", stroke = 0.15, size = 2.5) +
  theme_bw() +
  scale_fill_manual(values = region.palette.2) +
  xlab("Latitude") +
  ylab("Percent of POS (%)")
g.perc.S

###Make DINIMITE Plots

#carbon
d.perc.C <- ggplot(d.sum, aes(x = as.factor(station), y = metab.perc.c)) +
  geom_boxplot(width = 0.5) +
  geom_jitter(aes(fill = Region), shape = 21, color = "black", stroke = 0.15, size = 2.5, width = 0.05) +
  theme_bw() +
  scale_fill_manual(values = region.palette.2)  +
  xlab("Latitude") +
  ylab("Percent of POC (%)")
d.perc.C

#nitrogen
d.perc.N <- ggplot(d.sum, aes(x = as.factor(station), y = metab.perc.n)) +
  geom_boxplot(width = 0.5) +
  geom_jitter(aes(fill = Region), shape = 21, color = "black", stroke = 0.15, size = 2.5, width = 0.05) +
  theme_bw() +
  scale_fill_manual(values = region.palette.2)  +
  xlab("Latitude") +
  ylab("Percent of PON (%)")
d.perc.N

#sulfur
d.perc.S <- ggplot(d.sum, aes(x = as.factor(station), y = metab.perc.s)) +
  geom_boxplot(width = 0.5) +
  geom_jitter(aes(fill = Region), shape = 21, color = "black", stroke = 0.15, size = 2.5, width = 0.05) +
  theme_bw() +
  scale_fill_manual(values = region.palette.2)  +
  xlab("Latitude") +
  ylab("Percent of POS (%)")
d.perc.S



percent.plots <- g.perc.C + d.perc.C + 
  g.perc.N + d.perc.N + g.perc.S + d.perc.S + 
  plot_layout(guides = "collect", widths = c(6.5, 3.5)) +
  plot_annotation(tag_levels = "A")
percent.plots

ggsave(percent.plots, file = "Figures/Outputs/Percent_POC_PON_POS_Plots.png", 
       height = 6, width = 8, scale = 1.2, dpi = 600)


















