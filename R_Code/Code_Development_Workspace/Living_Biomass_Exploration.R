



library(tidyverse)
library(patchwork)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")


###define inputs
all.dat.file <- "Intermediates/Final_Osmo_Meta_Env_Dataframe.csv"
atp.g3.g4.file <- "Collaborator_Data/ATP_all/ATP_G3_G4.csv"


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












# Living Biomass Stuff ----------------------------------------------------



###Pull in ATP data:
atp.dat <- read_csv(atp.g3.g4.file) %>%
  mutate(Living.C.ng.L = PATP_ng_L*250,
         Percent_Living = (Living.C.ng.L/(PC*1000*12.01))*100,
         depth_m = as.numeric(Depth)) %>%
  mutate(depth_m = case_when(is.na(depth_m) ~ 4.5,
                             TRUE ~ depth_m)) %>%
  filter(is.na(ATP_Flag),
         depth_m < 10) %>%
  filter(!`Station...2` == "UW 8") 

ggplot(atp.dat, aes(x = Lat, y = PATP_ng_L)) + 
  geom_point() 




###Try out Percent Living biomass estimate:
Live.biomass <- dat %>%
  dplyr::select(Part.SampID, Cruise, Part.Conc.nM, sst, sss, chla, poc, Lat, Long, N_N, Region) %>%
  group_by(Part.SampID, Cruise, chla, poc, Lat, Long, sst, sss, N_N, Region) %>%
  reframe(Sum.Part.nM = sum(Part.Conc.nM)) %>%
  ungroup() %>%
  unique() %>%
  mutate(Living_Biomass_ng_C = 660*Sum.Part.nM^0.976,
         Living_Biomass_nM_C = Living_Biomass_ng_C/12.01,
         Percent_Living = Living_Biomass_nM_C/(poc*1000)*100)

Gradients.biomass <- Live.biomass %>%
  filter(Cruise %in% c("TN397", "KM1906"))

ggplot(Gradients.biomass, aes(x = Lat, y = Percent_Living, color = Long)) +
  geom_point()


#Plot Living biomass of both:
ggplot(Gradients.biomass, aes(x = Lat, y = Living_Biomass_ng_C, color = Long, shape = Cruise)) +
  geom_point() +
  geom_point(data = atp.dat, aes(x = Lat, y = Living.C.ng.L), color = "red") +
  facet_wrap(.~Cruise, scales = "free")



###Group both datasets in 1 degree bins by cruise 

#Bin POC by lat and calculate mean and sd:
lat.breaks <- seq(-3,50, by = 1)

lat.bin.dat.osmo <- Gradients.biomass %>%
  mutate(lat_bin = cut(Lat, breaks = lat.breaks, include.lowest = TRUE, right = FALSE)) %>%
  group_by(Cruise, lat_bin) %>%
  summarize(Mean.Part.Osmo.nM = mean(Sum.Part.nM),
            SD.Part.Osmo.nM = sd(Sum.Part.nM),
            Mean.Osmo.Liv.C.ng.L = mean(Living_Biomass_ng_C, na.rm = TRUE),
            SD.Osmo.Liv.C.ng.L = sd(Living_Biomass_ng_C, na.rm = TRUE),
            mean_perc_living = mean(Percent_Living, na.rm = TRUE),
            sd_perc_living = sd(Percent_Living, na.rm = TRUE))


lat.bin.dat.atp <- atp.dat %>%
  mutate(lat_bin = cut(Lat, breaks = lat.breaks, include.lowest = TRUE, right = FALSE)) %>%
  group_by(Cruise, lat_bin) %>%
  summarize(Mean.ATP.liv.C.ng.L = mean(Living.C.ng.L, na.rm = TRUE),
            SD.ATP.liv.C.ng.L = sd(Living.C.ng.L, na.rm = TRUE))

comb.lat.bin.dat <- left_join(lat.bin.dat.osmo, lat.bin.dat.atp)

ggplot(comb.lat.bin.dat, aes(x = Mean.Osmo.Liv.C.ng.L, y = Mean.ATP.liv.C.ng.L)) +
  geom_smooth(method = "lm") +
  geom_errorbar(aes(ymin = Mean.ATP.liv.C.ng.L - SD.ATP.liv.C.ng.L, ymax = Mean.ATP.liv.C.ng.L - SD.ATP.liv.C.ng.L)) +
  geom_point(aes(color = Cruise)) +
  geom_abline(slope = 1, intercept = 0) 


bio.osmo.calibration.plot <- ggplot(comb.lat.bin.dat, aes(x = Mean.Part.Osmo.nM, y = Mean.ATP.liv.C.ng.L)) +
  geom_smooth(method = "lm", color = "black") +
  geom_errorbar(aes(color = Cruise, ymin = Mean.ATP.liv.C.ng.L - SD.ATP.liv.C.ng.L, ymax = Mean.ATP.liv.C.ng.L + SD.ATP.liv.C.ng.L)) +
  geom_errorbarh(aes(color = Cruise, xmin = Mean.Part.Osmo.nM - SD.Part.Osmo.nM, xmax = Mean.Part.Osmo.nM + SD.Part.Osmo.nM)) +
  geom_point(aes(fill = Cruise), shape = 21, size = 3, stroke = 0.15) +
  theme_test() +
  xlab("Particulate Osmolyte Concentration (nM)") +
  ylab("Living Biomass from ATP (ng C/L)")
bio.osmo.calibration.plot
 # geom_abline(slope = 1, intercept = 0) 
ggsave(bio.osmo.calibration.plot, filename = "Figures/Outputs/Biomass_Osmo_Calibration_Figure.png", dpi = 600,
       height = 4, width = 5.5, scale = 1.1)

lm.liv.bio.comp <- lm(Mean.ATP.liv.C.ng.L ~ Mean.Part.Osmo.nM, data = comb.lat.bin.dat)
summary(lm.liv.bio.comp)





###Try out Percent Living biomass estimate using the ATP calibration
Live.biomass.atp.cal <- dat %>%
  filter(Cruise %in% c("TN397", "KM1906")) %>%
  dplyr::select(Part.SampID, Cruise, Part.Conc.nM, sst, sss, chla, poc, Lat, Long, N_N, Region, Local_Time) %>%
  group_by(Part.SampID, Cruise, chla, poc, Lat, Long, sst, sss, N_N, Region, Local_Time) %>%
  reframe(Sum.Part.nM = sum(Part.Conc.nM)) %>%
  ungroup() %>%
  unique() %>%
  mutate(Living_Biomass_ng_C = 2156*Sum.Part.nM-2980,
         Living_Biomass_nM_C = Living_Biomass_ng_C/12.01,
         Percent_Living = Living_Biomass_nM_C/(poc*1000)*100) %>%
  mutate(lt = hour(Local_Time),
         TOD = as.factor(case_when(lt > 3 & lt < 11 ~ "morning",
                         lt > 10 & lt < 17 ~ "midday",
                         lt > 16 & lt < 22 ~ "evening",
                         TRUE ~ "night")),
         TOD = fct_relevel(TOD, c("morning", "midday", "evening", "night")))




#Plot Living biomass of both:
g3.g4.living.biomass.plot <- ggplot(Live.biomass.atp.cal, aes(x = Lat, y = Living_Biomass_ng_C, shape = Cruise)) +
  geom_point(aes(fill = TOD), shape = 21, size = 3, stroke = 0.2) +
  scale_fill_viridis(discrete = TRUE, direction = -1, option = "G", end = 0.95) +
  geom_point(data = atp.dat, aes(x = Lat, y = Living.C.ng.L), fill = "red", shape = 22, stroke = 0.2, size = 3) +
  facet_wrap(.~Cruise, scales = "free") +
  theme_bw() +
  ylab("Living Biomass (ng C/L)")
g3.g4.living.biomass.plot
  
ggsave(g3.g4.living.biomass.plot, filename = "Figures/Outputs/G3_G4_Living_Biomass_Figure.png", dpi = 600,
       height = 4, width = 10, scale = 1.1)



g3.g4.percent.living.plot <- ggplot(Live.biomass.atp.cal, aes(x = Lat, y = Percent_Living, shape = Cruise)) +
  geom_point(aes(fill = TOD), shape = 21, size = 3, stroke = 0.2) +
  scale_fill_viridis(discrete = TRUE, direction = -1, option = "G", end = 0.95) +
#  geom_point(data = atp.dat, aes(x = Lat, y = Living.C.ng.L), fill = "red", shape = 22, stroke = 0.2, size = 3) +
  facet_wrap(.~Cruise, scales = "free_x") +
  geom_point(data = atp.dat, aes(x = Lat, y = Percent_Living), fill = "red", shape = 22, stroke = 0.2, size = 3) +
  theme_bw() +
  ylab("Percent Living Biomass (%)") +
  geom_hline(yintercept = 30, color = "darkred", linetype = "dashed")
g3.g4.percent.living.plot

ggsave(g3.g4.percent.living.plot, filename = "Figures/Outputs/G3_G4_Percent_Living_Biomass_Figure.png", dpi = 600,
       height = 4, width = 10, scale = 1.1)


Gradients.biomass.atp.cal <- Live.biomass.atp.cal %>%
  filter(Cruise %in% c("TN397", "KM1906"))

ggplot(Gradients.biomass.atp.cal, aes(x = Lat, y = Percent_Living, color = as.numeric(Local_Time))) +
  geom_point() +
  facet_wrap(.~Cruise, scales = "free_x")






ggplot(lat.bin.dat, aes(x = lat_bin, y = Mean.Osmo.Liv.C.ng.L, shape = Cruise)) +
  geom_point()












#Chla
ggplot(Live.biomass, aes(y = Sum.Part.nM, x = chla, color = Region)) +
  geom_point() +
  scale_y_log10() +
  scale_x_log10()

#POC
ggplot(Live.biomass, aes(y = Sum.Part.nM, x = poc, color = Region)) +
  geom_point() +
  scale_y_log10() +
  scale_x_log10()

#
ggplot(Live.biomass, aes(y = Sum.Part.nM, x = N_N, color = Region)) +
  geom_point() +
  scale_y_log10() +
  scale_x_log10()

#sst
ggplot(Live.biomass, aes(y = Sum.Part.nM, x = sst, color = Region)) +
  geom_point() +
  scale_y_log10() 
#  scale_x_log10()

#sst
ggplot(Live.biomass, aes(y = Sum.Part.nM, x = sss, color = Region)) +
  geom_point() +
  scale_y_log10() 
