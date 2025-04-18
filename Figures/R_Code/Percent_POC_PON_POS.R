



library(tidyverse)
library(patchwork)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")


###define inputs
all.dat.file <- "Intermediates/Final_Osmo_Meta_Env_Dataframe.csv"



#Read in data
dat <- read_csv(all.dat.file) %>%
  filter(!Part.SampID == "220902_Smp_TN397_S11_600_U_C")



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
  filter(depth_m < 10) %>%
  filter(metab.perc.c < 10) 

###All sum of CN, CS, and NS ratios:
all.sum <- dat %>%
  filter(Cruise %in% c("TN397", "KM1906", "RC078")) %>%
  group_by(Part.SampID) %>%
  reframe(cn = sum(Part.C.nM, na.rm = TRUE)/sum(Part.N.nM, na.rm = TRUE),
          cs = sum(Part.C.nM, na.rm = TRUE)/sum(Part.S.nM, na.rm = TRUE),
          ns = sum(Part.N.nM, na.rm = TRUE)/sum(Part.S.nM, na.rm = TRUE)) %>%
  mutate(mean.cn = mean(cn),
         sd.cn = sd(cn),
         mean.cs = mean(cs),
         sd.cs = sd(cs),
         mean.ns = mean(ns),
         sd.ns = sd(ns))


###_________
gradients.sum <- percent.sum %>%
  filter(Cruise %in% c("TN397", "KM1906")) %>%
  group_by(Region) %>%
  mutate(mean.perc.c = mean(metab.perc.c),
         sd.perc.c = sd(metab.perc.c),
         mean.perc.n = mean(metab.perc.n),
         sd.perc.n = sd(metab.perc.n),
         mean.perc.s = mean(metab.perc.s),
         sd.perc.s = sd(metab.perc.s)) %>%
  full_join(., tibble(Region = "PS"))

####________
d.sum <- percent.sum %>%
  filter(Cruise %in% c("RC078")) %>%
  filter(!station %in% c(1,8)) %>%
 # group_by(station) %>%
  mutate(mean.perc.c = mean(metab.perc.c),
         sd.perc.c = sd(metab.perc.c),
         mean.perc.n = mean(metab.perc.n),
         sd.perc.n = sd(metab.perc.n),
         mean.perc.s = mean(metab.perc.s),
         sd.perc.s = sd(metab.perc.s))


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
  xlab("Station") +
  ylab("Percent of POC (%)") +
  theme(legend.position = "none")
d.perc.C

#nitrogen
d.perc.N <- ggplot(d.sum, aes(x = as.factor(station), y = metab.perc.n)) +
  geom_boxplot(width = 0.5) +
  geom_jitter(aes(fill = Region), shape = 21, color = "black", stroke = 0.15, size = 2.5, width = 0.05) +
  theme_bw() +
  scale_fill_manual(values = region.palette.2)  +
  xlab("Station") +
  ylab("Percent of PON (%)") +
  theme(legend.position = "none")
d.perc.N

#sulfur
d.perc.S <- ggplot(d.sum, aes(x = as.factor(station), y = metab.perc.s)) +
  geom_boxplot(width = 0.5) +
  geom_jitter(aes(fill = Region), shape = 21, color = "black", stroke = 0.15, size = 2.5, width = 0.05) +
  theme_bw() +
  scale_fill_manual(values = region.palette.2)  +
  xlab("Station") +
  ylab("Percent of POS (%)") +
  theme(legend.position = "none")
d.perc.S



percent.plots <- g.perc.C + d.perc.C + 
  g.perc.N + d.perc.N + g.perc.S + d.perc.S + 
  plot_layout(guides = "collect", widths = c(6.5, 3.5)) +
  plot_annotation(tag_levels = "A")
percent.plots

ggsave(percent.plots, file = "Figures/Outputs/Percent_POC_PON_POS_Plots.png", 
       height = 6, width = 8, scale = 1.2, dpi = 600)


















