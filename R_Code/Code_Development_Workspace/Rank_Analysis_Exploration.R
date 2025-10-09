





library(viridis)
library(tidyverse)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")


#load in data:

######define inputs
all.dat.file <- "Intermediates/Final_Osmo_Meta_Env_Dataframe.csv"
cult.file <- "Intermediates/Culture_Quant_Output.csv"
cult.meta.file <- "Intermediates/All_culture_metadata.csv"




#Read in data
dat <- read_csv(all.dat.file) %>%
  filter(!Part.SampID == "220902_Smp_TN397_S11_600_U_C")



#### Look at Relative abundance by sample across region and get overall rank:
Rel.metab.dat <- dat %>%
  select(Part.SampID, Cruise, Region, Part.Conc.nM, Compound, compound.name.figure, class, depth_m) %>%
  unique() %>%
  filter(Cruise %in% c("TN397", "KM1906", "RC078")) %>%
  #  filter(!Cruise %in% c("RC078")) %>%
  filter(depth_m < 16) %>%
  group_by(Part.SampID) %>%
  mutate(Tot.Part.Conc.nM = sum(Part.Conc.nM),
         Rel.Osmo.Conc = Part.Conc.nM/Tot.Part.Conc.nM) %>%
  group_by(Compound) %>%
  mutate(Comp.Mean.Rel.Conc = mean(Rel.Osmo.Conc),
         SD.Rel.Conc = sd(Rel.Osmo.Conc),
         RSD.Rel.Conc = SD.Rel.Conc/Comp.Mean.Rel.Conc,
         Variance.Rel.Conc = max(Rel.Osmo.Conc) - min(Rel.Osmo.Conc),
         Rel.Variance = Variance.Rel.Conc/Comp.Mean.Rel.Conc)


comp.rsd.summary <- Rel.metab.dat %>%
  select(Compound, compound.name.figure, class, Comp.Mean.Rel.Conc, SD.Rel.Conc, RSD.Rel.Conc, Variance.Rel.Conc, Rel.Variance) %>%
  unique()


####Rank order metabolite data:
rank.p.metab.dat <- dat %>%
  select(Part.SampID, Cruise, Region, Part.Conc.nM, Compound, compound.name.figure, class, depth_m) %>%
  unique() %>%
  group_by(Region, Compound) %>%
  mutate(mean.conc.nM = mean(Part.Conc.nM)) %>%
  select(Region, Compound, mean.conc.nM, compound.name.figure, class) %>%
  unique() %>%
  group_by(Region) %>%
  mutate(rank.order = rank(1/mean.conc.nM)) 


ggplot(rank.p.metab.dat, aes(x = Region, y = reorder(compound.name.figure, -rank.order), fill = rank.order)) +
  geom_tile(color = "black", size = 0.05) +
  scale_fill_viridis(option = "G", direction = -1)

###Rank order by regions
rank.cc <- rank.p.metab.dat %>%
  filter(Region == "CC") %>%
  rename(rank.order.cc = rank.order) %>%
  ungroup() %>%
  select(Compound, class, compound.name.figure, rank.order.cc) %>% 
  unique()

rank.eq <- rank.p.metab.dat %>%
  filter(Region == "Equator") %>%
  rename(rank.order.eq = rank.order) %>%
  ungroup() %>%
  select(Compound, class, compound.name.figure, rank.order.eq) %>% 
  unique()

rank.npsg <- rank.p.metab.dat %>%
  filter(Region == "NPSG") %>%
  rename(rank.order.npsg = rank.order) %>%
  ungroup() %>%
  select(Compound, class, compound.name.figure, rank.order.npsg) %>% 
  unique()

rank.nptz <- rank.p.metab.dat %>%
  filter(Region == "NPTZ") %>%
  rename(rank.order.nptz = rank.order) %>%
  ungroup() %>%
  select(Compound, class, compound.name.figure, rank.order.nptz) %>% 
  unique()

rank.ps <- rank.p.metab.dat %>%
  filter(Region == "PS") %>%
  rename(rank.order.ps = rank.order) %>%
  ungroup() %>%
  select(Compound, class, compound.name.figure, rank.order.ps) %>% 
  unique()

rank.region <- left_join(rank.cc, rank.eq) %>%
  left_join(., rank.npsg) %>%
  left_join(., rank.nptz) %>%
  left_join(., rank.ps) 



#rank order figures:
rankfig1 <- ggplot(rank.region, aes(x = rank.order.npsg, y = rank.order.nptz)) +
  geom_abline(slope = 1, intercept = 0) +
  coord_fixed() +
  geom_point(shape = 21, aes(fill = class), size = 3, stroke = 0.15) +
  theme_test() +
  xlab("Rank NPSG") +
  ylab("Rank NPTZ") +
  scale_fill_manual(values = class.pal) 
rankfig1

rankfig2 <- ggplot(rank.region, aes(x = rank.order.npsg, y = rank.order.eq)) +
  geom_abline(slope = 1, intercept = 0) +
  coord_fixed() +
  geom_point(shape = 21, aes(fill = class), size = 3, stroke = 0.15) +
  theme_test() +
  xlab("Rank NPSG") +
  ylab("Rank Equator") +
  scale_fill_manual(values = class.pal) 
rankfig2

rankfig3 <- ggplot(rank.region, aes(x = rank.order.nptz, y = rank.order.eq)) +
  geom_abline(slope = 1, intercept = 0) +
  coord_fixed() +
  geom_point(shape = 21, aes(fill = class), size = 3, stroke = 0.15) +
  theme_test() +
  xlab("Rank NPSG") +
  ylab("Rank Equator") +
  scale_fill_manual(values = class.pal) 
rankfig3

lm1 <- lm(rank.order.nptz ~ rank.order.npsg, data = rank.region)
summary(lm1)

lm2  <- lm(rank.order.eq ~ rank.order.npsg, data = rank.region)
summary(lm2)

lm3 <- lm(rank.order.eq ~ rank.order.nptz, data = rank.region)
summary(lm3)

rank.fig.compilation <- rankfig1 + rankfig2 + rankfig3 +  plot_layout(guides = "collect")
rank.fig.compilation 

ggsave(rank.fig.compilation, filename = "Figures/Outputs/Rank_order_presentation_figures.jpg", dpi = 600, 
       height = 5, width = 10, scale = 1.15)


#Rank order figures
ggplot(rank.region, aes(x = mean.rank.npsg, y = mean.rank.nptz)) + 
  geom_errorbar(aes(ymin = mean.rank.nptz-sd.rank.nptz, ymax = mean.rank.nptz+sd.rank.nptz)) +
  geom_errorbarh(aes(xmin = mean.rank.npsg - sd.rank.npsg, xmax = mean.rank.npsg + sd.rank.npsg)) +
  geom_point(size = 2, shape = 21, fill = "white") +
  geom_abline(intercept = 0, slope = 1, color = "blue")

ggplot(rank.region, aes(x = mean.rank.eq, y = mean.rank.nptz)) + 
  geom_errorbar(aes(ymin = mean.rank.nptz-sd.rank.nptz, ymax = mean.rank.nptz+sd.rank.nptz)) +
  geom_errorbarh(aes(xmin = mean.rank.eq - sd.rank.eq, xmax = mean.rank.eq + sd.rank.eq)) +
  geom_point(size = 2, shape = 21, fill = "white") +
  geom_abline(intercept = 0, slope = 1, color = "blue")

ggplot(rank.region, aes(x = mean.rank.cc, y = mean.rank.nptz)) + 
  geom_errorbar(aes(ymin = mean.rank.nptz-sd.rank.nptz, ymax = mean.rank.nptz+sd.rank.nptz)) +
  geom_errorbarh(aes(xmin = mean.rank.cc - sd.rank.cc, xmax = mean.rank.cc + sd.rank.cc)) +
  geom_point(size = 2, shape = 21, fill = "white") +
  geom_abline(intercept = 0, slope = 1, color = "blue")

ggplot(rank.region, aes(x = mean.rank.ps, y = mean.rank.nptz)) + 
  geom_errorbar(aes(ymin = mean.rank.nptz-sd.rank.nptz, ymax = mean.rank.nptz+sd.rank.nptz)) +
  geom_errorbarh(aes(xmin = mean.rank.ps - sd.rank.ps, xmax = mean.rank.ps + sd.rank.ps)) +
  geom_point(size = 2, shape = 21, fill = "white") +
  geom_abline(intercept = 0, slope = 1, color = "blue")


### XXYYZZ
rank.p.metab.dat.all <- dat %>%
  select(Part.SampID, Cruise, Region, Part.Conc.nM, Compound, compound.name.figure, class, depth_m) %>%
  unique() %>%
  group_by(Part.SampID) %>%
  mutate(rank.order = rank(1/Part.Conc.nM)) %>%
  group_by(Compound) %>%
  mutate(mean.rank = mean(rank.order),
         sd.rank = sd(rank.order)) %>%
  select(compound.name.figure, mean.rank, sd.rank) %>% 
  unique()



comp.rsd.summary 

x.dat <- left_join(rank.p.metab.dat.all, comp.rsd.summary)

ggplot(x.dat, aes(x = mean.rank, y = sd.rank)) +
  geom_point() 





















































































































