



##packages:
library(tidyverse)
library(lubridate)
library(rstatix)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")




#define inputs:
atp.file <- "Intermediates/Gradients_ATP_Enviro_Meta_Data.csv"
metab.file <- "Intermediates/Enviro_Osmo_Final_Dataset_with_metadata.csv"





#load data:
atp.dat <- read_csv(atp.file)
metab.dat <- read_csv(metab.file)




#match up data based on 3 hour time window:
atp.dat.match <- atp.dat %>%
  select(Cruise, Station, UTC.time.round, PATP_ng_L, SD_PATP_ng_L) %>%
  mutate(atp.timewindow.max = UTC.time.round + hours(3),
         atp.timewindow.min = UTC.time.round - hours(3))  %>%
  rename(UTC.time.round.ATP = UTC.time.round) %>%
  select(-Cruise)
  

metab.dat.match <- metab.dat %>%
  filter(Cruise %in% c("TN397", "KM1906")) %>%
  filter(Part.detected == "Yes") %>%
  select(Cruise, Parent_ID, Lat, Long, UTC.time.round, Part.Conc.nM) %>%
  mutate(metab.sample = str_remove(Parent_ID, "_A"),
         metab.sample = str_remove(metab.sample, "_B"),
         metab.sample = str_remove(metab.sample, "_C")) %>%
  group_by(Cruise, Parent_ID, metab.sample, Lat, Long, UTC.time.round) %>%
  reframe(Sum.Osmo.Conc.nM = sum(Part.Conc.nM)) %>%
  ungroup() %>%
  group_by(Cruise, metab.sample, Lat, Long, UTC.time.round) %>%
  reframe(Mean.Osmo.Conc.nM = mean(Sum.Osmo.Conc.nM),
          SD.Osmo.Conc.nM = sd(Sum.Osmo.Conc.nM)) %>%
  rename(UTC.time.round.osmo = UTC.time.round) 


##combine data:
atp.metab.dat <- cross_join(atp.dat.match, metab.dat.match) %>%
  filter(UTC.time.round.osmo < atp.timewindow.max,
         UTC.time.round.osmo > atp.timewindow.min) %>%
  mutate(Mean.living.biomass.ngL = 250*PATP_ng_L,
         SD.living.biomass.ngL = 250*SD_PATP_ng_L) %>%
  unique()
  
  


proxy.plot <- ggplot(atp.metab.dat, aes(y = Mean.living.biomass.ngL, x = Mean.Osmo.Conc.nM)) +
  geom_smooth(method = "lm", color = "black", alpha = 0.2) +
  geom_errorbarh(aes(xmin = Mean.Osmo.Conc.nM-SD.Osmo.Conc.nM, xmax = Mean.Osmo.Conc.nM+SD.Osmo.Conc.nM), size = 0.2) +
  geom_errorbar(aes(ymin = Mean.living.biomass.ngL-SD.living.biomass.ngL, ymax = Mean.living.biomass.ngL+SD.living.biomass.ngL), size = 0.2) +
  geom_point(aes(fill = Cruise), shape = 21, stroke = 0.2, size = 2.5)  +
  theme_test() +
  xlab("Mean Particulate Osmolyte Concentration (nM)") +
  ylab("Mean Living Biomass (ng C/L)")
proxy.plot
ggsave(proxy.plot, filename = "Figures/SCOPE_2025/ATP_Osmo_Calibration_Plot.png", height = 3.5, width = 4.5, dpi = 800)


# ggplot(atp.metab.dat, aes(x = PATP_ng_L, y = Mean.Osmo.Conc.nM)) +
#   geom_point(aes(color = Cruise)) +
#   geom_errorbar(aes(ymin = Mean.Osmo.Conc.nM-SD.Osmo.Conc.nM, ymax = Mean.Osmo.Conc.nM+SD.Osmo.Conc.nM)) +
#   geom_errorbarh(aes(xmin = PATP_ng_L-SD_PATP_ng_L, xmax = PATP_ng_L+SD_PATP_ng_L)) +
#   geom_smooth(method = "lm") +
#   scale_x_log10() +
#   scale_y_log10()

# 
#   pp14c_time_min = pp14c_time - hours(12),
# pp14c_time_max = pp14c_time + hours(12)) %>%


lm.both <- lm(Mean.living.biomass.ngL ~ Mean.Osmo.Conc.nM, data = atp.metab.dat)
summary(lm.both)

#.    y = 1682.3x + 161.3 (y = living biomass (ng/L), x = Osmo.Conc (nmol/L))






###Apply model 
osmo.biomass.prediction <- metab.dat %>%
  filter(Cruise %in% c("TN397", "KM1906")) %>%
  filter(Part.detected == "Yes") %>%
  select(Cruise, Parent_ID, Lat, Long, Local_Time, UTC.time.round, Part.Conc.nM, poc) %>%
  mutate(metab.sample = str_remove(Parent_ID, "_A"),
         metab.sample = str_remove(metab.sample, "_B"),
         metab.sample = str_remove(metab.sample, "_C")) %>%
  group_by(Cruise, Parent_ID, metab.sample, Lat, Long, Local_Time, UTC.time.round, poc) %>%
  reframe(Sum.Osmo.Conc.nM = sum(Part.Conc.nM),
          Living.biomass.ngL = Sum.Osmo.Conc.nM*1681.3+161.3,
          Living.biomass.nM.C = Living.biomass.ngL/12.01,
          Percent.Living = Living.biomass.nM.C/(poc*1000)*100) %>%
  ungroup() %>%
  group_by(Cruise, metab.sample) %>%
  mutate(Mean.living.biomass.ngL = mean(Living.biomass.ngL),
         SD.living.biomass.ngL = sd(Living.biomass.ngL),
         Mean.Percent.Living = mean(Percent.Living),
         SD.Percent.Living = sd(Percent.Living)) %>%
  mutate(lt = hour(Local_Time),
         TOD = as.factor(case_when(lt > 3 & lt < 11 ~ "morning",
                                   lt > 10 & lt < 17 ~ "midday",
                                   lt > 16 & lt < 22 ~ "evening",
                                   TRUE ~ "night")),
         TOD = fct_relevel(TOD, c("morning", "midday", "evening", "night"))) %>%
  mutate() %>%
  unique() %>%
  mutate(Region = case_when(Cruise == "KM1906" & Lat >= 35 ~ "NPTZ",
                            Cruise == "KM1906" & Lat < 35 ~ "NPSG",
                            Cruise == "TN397" & Long > -122 ~ "CUCP",
                            Cruise == "TN397" & Long < -122 & Lat > 10.5 ~ "NPSG",
                            Cruise == "TN397" & Lat < 10.5 & Lat > 4.5 ~ "NPEC",
                            Cruise == "TN397" & Lat < 4.5 ~ "PEDP",
                            Cruise == "RC078" ~ "SS")) %>%
  mutate(Region = as.factor(Region)) %>%
  mutate(Region = fct_relevel(Region, c("PEDP", "NPEC", "NPSG", "NPTZ", "CUCP"))) %>%
  filter(!is.na(Region)) %>%
  mutate(Cruise = as.factor(Cruise)) %>%
  mutate(Cruise = fct_relevel(Cruise, c("TN397", "KM1906"))) 



#make a dataframe of just averages and standard deviations 
Osmo.biomass.sml <- osmo.biomass.prediction %>%
  select(Cruise, metab.sample, Lat, Long, Mean.living.biomass.ngL, 
         SD.living.biomass.ngL, Mean.Percent.Living, SD.Percent.Living, TOD, Region) %>%
  mutate(Cruise = as.factor(Cruise)) %>%
  mutate(Cruise = fct_relevel(Cruise, c("TN397", "KM1906")))




##Organize ATP data for plotting:
atp.plot.dat <- atp.dat %>%
  mutate(Mean.living.biomass.ngL = 250*PATP_ng_L,
         SD.living.biomass.ngL = 250*SD_PATP_ng_L,
         Mean.Percent.Living = Mean.living.biomass.ngL/12.01/(poc*1000)*100,
         SD.Percent.Living = SD.living.biomass.ngL/12.01/(poc*1000)*100) %>%
  unique() %>%
  mutate(Region = case_when(Cruise == "KM1906" & Lat >= 35 ~ "NPTZ",
                            Cruise == "KM1906" & Lat < 35 ~ "NPSG",
                            Cruise == "TN397" & Long > -122 ~ "CUCP",
                            Cruise == "TN397" & Long < -122 & Lat > 10.5 ~ "NPSG",
                            Cruise == "TN397" & Lat < 10.5 & Lat > 4.5 ~ "NPEC",
                            Cruise == "TN397" & Lat < 4.5 ~ "PEDP")) %>%
  mutate(Region = as.factor(Region)) %>%
  mutate(Region = fct_relevel(Region, c("PEDP", "NPEC", "NPSG", "NPTZ", "CUCP"))) %>%
  filter(!is.na(Region))  %>%
  mutate(Cruise = as.factor(Cruise)) %>%
  mutate(Cruise = fct_relevel(Cruise, c("TN397", "KM1906")))


#define lines
lines.df <- tibble(
  Cruise = c("TN397", "TN397", "TN397", "KM1906"),
  Lat = c(4.5, 10.5, 29, 35)
)






#Just Osmolytes:
o.percent.living.plot <- ggplot(Osmo.biomass.sml, aes(x = Lat, y = Mean.Percent.Living)) +
  geom_vline(data = lines.df, aes(xintercept = Lat), linetype = "dashed", size = 0.2) +
  geom_errorbar(aes(ymin = Mean.Percent.Living-SD.Percent.Living, ymax = Mean.Percent.Living+SD.Percent.Living), size = 0.2) +
  #  geom_errorbar(data = Osmo.biomass.sml, aes(ymin = Mean.osmo.percent.living-SD.osmo.percent.living, ymax = Mean.osmo.percent.living+SD.osmo.percent.living)) +
  geom_point(aes(fill = TOD), shape = 21, size = 3, stroke = 0.25) +
  scale_fill_viridis(discrete = TRUE, direction = -1, option = "G", end = 0.95) +
  #  geom_point(data = atp.dat, aes(x = Lat, y = Living.C.ng.L), fill = "red", shape = 22, stroke = 0.2, size = 3) +
  facet_wrap(.~Cruise, scales = "free_x") +
 # geom_errorbar(data = atp.plot.dat, aes(ymin = Mean.Percent.Living-SD.Percent.Living, ymax = Mean.Percent.Living+SD.Percent.Living), size = 0.2) +
 # geom_point(data = atp.plot.dat, aes(x = Lat, y = Mean.Percent.Living), fill = "red", shape = 22, stroke = 0.2, size = 3) +
  theme_test() +
  ylab("Percent Living Biomass (%)") +
  geom_hline(yintercept = 30, color = "darkred", linetype = "dashed")
o.percent.living.plot

ggsave(o.percent.living.plot, filename = "Figures/SCOPE_2025/o.percent.living.plot.png", height = 4, width = 10, dpi = 800)

#Just ATP:
atp.percent.living.plot <- ggplot(Osmo.biomass.sml, aes(x = Lat, y = Mean.Percent.Living)) +
  facet_wrap(.~Cruise, scales = "free_x") +
  geom_vline(data = lines.df, aes(xintercept = Lat), linetype = "dashed", size = 0.2) +
 # geom_errorbar(aes(ymin = Mean.Percent.Living-SD.Percent.Living, ymax = Mean.Percent.Living+SD.Percent.Living), size = 0.2) +
  #  geom_errorbar(data = Osmo.biomass.sml, aes(ymin = Mean.osmo.percent.living-SD.osmo.percent.living, ymax = Mean.osmo.percent.living+SD.osmo.percent.living)) +
 # geom_point(aes(fill = TOD), shape = 21, size = 3, stroke = 0.25) +
  scale_fill_viridis(discrete = TRUE, direction = -1, option = "G", end = 0.95) +
  #  geom_point(data = atp.dat, aes(x = Lat, y = Living.C.ng.L), fill = "red", shape = 22, stroke = 0.2, size = 3) +
  geom_errorbar(data = atp.plot.dat, aes(ymin = Mean.Percent.Living-SD.Percent.Living, ymax = Mean.Percent.Living+SD.Percent.Living), size = 0.2) +
  geom_point(data = atp.plot.dat, aes(x = Lat, y = Mean.Percent.Living), fill = "red", shape = 22, stroke = 0.2, size = 3) +
  theme_test() +
  ylim(c(NA, 105)) +
  ylab("Percent Living Biomass (%)") +
  geom_hline(yintercept = 30, color = "darkred", linetype = "dashed")
atp.percent.living.plot

ggsave(atp.percent.living.plot, filename = "Figures/SCOPE_2025/atp.percent.living.plot.png", height = 4, width = 9, dpi = 800)

#Combined:
comb.percent.living.plot <- ggplot(Osmo.biomass.sml, aes(x = Lat, y = Mean.Percent.Living)) +
  geom_vline(data = lines.df, aes(xintercept = Lat), linetype = "dashed", size = 0.2) +
  geom_errorbar(aes(ymin = Mean.Percent.Living-SD.Percent.Living, ymax = Mean.Percent.Living+SD.Percent.Living), size = 0.2) +
#  geom_errorbar(data = Osmo.biomass.sml, aes(ymin = Mean.osmo.percent.living-SD.osmo.percent.living, ymax = Mean.osmo.percent.living+SD.osmo.percent.living)) +
  geom_point(aes(fill = TOD), shape = 21, size = 3, stroke = 0.25) +
  scale_fill_viridis(discrete = TRUE, direction = -1, option = "G", end = 0.95) +
  #  geom_point(data = atp.dat, aes(x = Lat, y = Living.C.ng.L), fill = "red", shape = 22, stroke = 0.2, size = 3) +
  facet_wrap(.~Cruise, scales = "free_x") +
  geom_errorbar(data = atp.plot.dat, aes(ymin = Mean.Percent.Living-SD.Percent.Living, ymax = Mean.Percent.Living+SD.Percent.Living), size = 0.2) +
  geom_point(data = atp.plot.dat, aes(x = Lat, y = Mean.Percent.Living), fill = "red", shape = 22, stroke = 0.2, size = 3) +
  theme_test() +
  ylab("Percent Living Biomass (%)") +
  geom_hline(yintercept = 30, color = "darkred", linetype = "dashed")
comb.percent.living.plot


ggsave(comb.percent.living.plot, filename = "Figures/SCOPE_2025/comb.percent.living.plot.png", height = 4, width = 10, dpi = 800)







#Living Biomass Combined
biomass.plot <- ggplot(Osmo.biomass.sml, aes(x = Lat, y = Mean.living.biomass.ngL)) +
  geom_vline(data = lines.df, aes(xintercept = Lat), linetype = "dashed", size = 0.2) +
  geom_errorbar(aes(ymin = Mean.living.biomass.ngL-SD.living.biomass.ngL, ymax = Mean.living.biomass.ngL+SD.living.biomass.ngL), size = 0.2) +
 # geom_errorbar(data = Osmo.biomass.sml, aes(ymin = Mean.osmo.percent.living-SD.osmo.percent.living, ymax = Mean.osmo.percent.living+SD.osmo.percent.living)) +
  geom_point(aes(fill = TOD), shape = 21, size = 3, stroke = 0.25) +
  scale_fill_viridis(discrete = TRUE, direction = -1, option = "G", end = 0.95) +
  #  geom_point(data = atp.dat, aes(x = Lat, y = Living.C.ng.L), fill = "red", shape = 22, stroke = 0.2, size = 3) +
  facet_wrap(.~Cruise, scales = "free_x") +
  
  geom_errorbar(data = atp.plot.dat, aes(ymin = Mean.living.biomass.ngL-SD.living.biomass.ngL, ymax = Mean.living.biomass.ngL+SD.living.biomass.ngL), size = 0.2) +
#  geom_errorbar(data = atp.plot.dat, aes(ymin = Mean.Percent.Living-SD.Percent.Living, ymax = Mean.Percent.Living+SD.Percent.Living), color = "gray50") +
  geom_point(data = atp.plot.dat, aes(x = Lat, y = Mean.living.biomass.ngL), fill = "red", shape = 22, stroke = 0.2, size = 3) +
  theme_test() +
  ylab("Living Biomass (ng C/L)") 
biomass.plot

ggsave(biomass.plot, filename = "Figures/SCOPE_2025/biomass.plot.png", height = 4, width = 10, dpi = 800)



### Runs Statistics:

#Osmolytes:
osmo.percent.living.w.anova <- osmo.biomass.prediction %>%
  group_by(Cruise) %>%
  welch_anova_test(Percent.Living ~ Region)

osmo.percent.living.gh.test <- osmo.biomass.prediction %>%
  group_by(Cruise) %>%
  games_howell_test(Percent.Living ~ Region)


#ATP:
atp.percent.living.w.anova <- atp.plot.dat %>%
  group_by(Cruise) %>%
  welch_anova_test(Mean.Percent.Living ~ Region)

atp.percent.living.gh.test <- atp.plot.dat %>%
  group_by(Cruise) %>%
  games_howell_test(Mean.Percent.Living ~ Region)








#####XXXXX

osmo.PL.boxplot <- ggplot(osmo.biomass.prediction %>%
         filter(!is.na(Percent.Living)), aes(y = Percent.Living, x = Region)) +
  geom_boxplot(width = 0.2) +
  geom_jitter(width = 0.1, shape = 21, size = 2, stroke = 0.2, color = "black", aes(fill = Region)) + 
  scale_fill_manual(values = region.palette.7) +
  facet_grid(.~Cruise, scales = "free_x", space = "free") +
  theme_test() +
  ylab("Percent Living Biomass (%)")
osmo.PL.boxplot

ggsave(osmo.PL.boxplot, filename = "Figures/SCOPE_2025/osmo.PL.boxplot.png", height = 4, width = 5, dpi = 800)


atp.PL.boxplot <- ggplot(atp.plot.dat %>%
         filter(!is.na(Mean.Percent.Living)), aes(y = Mean.Percent.Living, x = Region)) +
  geom_boxplot(width = 0.2) +
  geom_jitter(width = 0.1, shape = 21, size = 2, stroke = 0.2, color = "black", aes(fill = Region)) + 
  scale_fill_manual(values = region.palette.7) +
  facet_grid(.~Cruise, scales = "free_x", space = "free") +
  theme_test() +
  ylab("Percent Living Biomass (%)")
atp.PL.boxplot

ggsave(atp.PL.boxplot, filename = "Figures/SCOPE_2025/atp.PL.boxplot.png", height = 4, width = 5, dpi = 800)

##




##___________________Make plots of just G3_______________________
g3.atp.plot.dat <- atp.plot.dat %>% filter(Cruise == "KM1906")
g3.osmo <- Osmo.biomass.sml %>% filter(Cruise == "KM1906")



##G3 Living biomass:
g3.biomass.plot <- ggplot(g3.osmo, aes(x = Lat, y = Mean.living.biomass.ngL)) +
  geom_vline(xintercept = 32, linetype = "dashed", size = 0.2, color = "steelblue") +
  geom_vline(xintercept = 35, linetype = "dashed", size = 0.2) +
  geom_errorbar(aes(ymin = Mean.living.biomass.ngL-SD.living.biomass.ngL, ymax = Mean.living.biomass.ngL+SD.living.biomass.ngL), size = 0.2) +
  # geom_errorbar(data = Osmo.biomass.sml, aes(ymin = Mean.osmo.percent.living-SD.osmo.percent.living, ymax = Mean.osmo.percent.living+SD.osmo.percent.living)) +
  geom_point(aes(fill = TOD), shape = 21, size = 3, stroke = 0.25) +
  scale_fill_viridis(discrete = TRUE, direction = -1, option = "G", end = 0.95) +
  #  geom_point(data = atp.dat, aes(x = Lat, y = Living.C.ng.L), fill = "red", shape = 22, stroke = 0.2, size = 3) +
#  facet_wrap(.~Cruise, scales = "free_x") +
  geom_errorbar(data = g3.atp.plot.dat, aes(ymin = Mean.living.biomass.ngL-SD.living.biomass.ngL, ymax = Mean.living.biomass.ngL+SD.living.biomass.ngL), size = 0.2) +
  #  geom_errorbar(data = atp.plot.dat, aes(ymin = Mean.Percent.Living-SD.Percent.Living, ymax = Mean.Percent.Living+SD.Percent.Living), color = "gray50") +
  geom_point(data = g3.atp.plot.dat, aes(x = Lat, y = Mean.living.biomass.ngL), fill = "red", shape = 22, stroke = 0.2, size = 3) +
  theme_test() +
  ylab("Living Biomass (ng C/L)") 
g3.biomass.plot

ggsave(g3.biomass.plot, filename = "Figures/SCOPE_2025/g3.biomass.plot.png", height = 4.5, width = 9, dpi = 800, scale = 0.8)



g3.comb.percent.living.plot <- ggplot(g3.osmo, aes(x = Lat, y = Mean.Percent.Living)) +
  geom_vline(xintercept = 32, linetype = "dashed", size = 0.2, color = "steelblue") +
  geom_vline(xintercept = 35, linetype = "dashed", size = 0.2) +
  geom_errorbar(aes(ymin = Mean.Percent.Living-SD.Percent.Living, ymax = Mean.Percent.Living+SD.Percent.Living), size = 0.2) +
  #  geom_errorbar(data = Osmo.biomass.sml, aes(ymin = Mean.osmo.percent.living-SD.osmo.percent.living, ymax = Mean.osmo.percent.living+SD.osmo.percent.living)) +
  geom_point(aes(fill = TOD), shape = 21, size = 3, stroke = 0.25) +
  scale_fill_viridis(discrete = TRUE, direction = -1, option = "G", begin = 0.35, end = 0.95) +
  #  geom_point(data = atp.dat, aes(x = Lat, y = Living.C.ng.L), fill = "red", shape = 22, stroke = 0.2, size = 3) +
#  facet_wrap(.~Cruise, scales = "free_x") +
  geom_errorbar(data = g3.atp.plot.dat, aes(ymin = Mean.Percent.Living-SD.Percent.Living, ymax = Mean.Percent.Living+SD.Percent.Living), size = 0.2) +
  geom_point(data = g3.atp.plot.dat, aes(x = Lat, y = Mean.Percent.Living), fill = "red", shape = 22, stroke = 0.2, size = 3) +
  theme_test() +
  ylab("Percent Living Biomass (%)") +
  geom_hline(yintercept = 30, color = "darkred", linetype = "dashed")
g3.comb.percent.living.plot


ggsave(g3.comb.percent.living.plot, filename = "Figures/SCOPE_2025/g3.comb.percent.living.plot.png", height = 4.5, width = 9, dpi = 800, scale = 0.8)















# 
# ggplot(Osmo.biomass.sml, aes(x = Lat, y = Mean.osmo.living.biomass.ngL)) +
#   geom_errorbar(aes(ymin = Mean.Percent.Living-SD.Percent.Living, ymax = Mean.Percent.Living+SD.Percent.Living)) +
#   #  geom_errorbar(data = Osmo.biomass.sml, aes(ymin = Mean.osmo.percent.living-SD.osmo.percent.living, ymax = Mean.osmo.percent.living+SD.osmo.percent.living)) +
#   geom_point(aes(fill = TOD), shape = 21, size = 3, stroke = 0.25) +
#   scale_fill_viridis(discrete = TRUE, direction = -1, option = "G", end = 0.95) +
#   #  geom_point(data = atp.dat, aes(x = Lat, y = Living.C.ng.L), fill = "red", shape = 22, stroke = 0.2, size = 3) +
#   facet_wrap(.~Cruise, scales = "free_x") +
#   
#   geom_errorbar(data = atp.plot.dat, aes(ymin = Mean.Percent.Living-SD.Percent.Living, ymax = Mean.Percent.Living+SD.Percent.Living)) +
#   geom_point(data = atp.plot.dat, aes(x = Lat, y = Mean.Percent.Living), fill = "red", shape = 22, stroke = 0.2, size = 3) +
#   theme_bw() +
#   ylab("Percent Living Biomass (%)") +
#   geom_hline(yintercept = 30, color = "darkred", linetype = "dashed")
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ggplot(osmo.biomass.prediction, aes(x = Lat, y = Percent.Living)) +
#   geom_point(aes(fill = TOD), shape = 21, size = 3, stroke = 0.2) +
#   scale_fill_viridis(discrete = TRUE, direction = -1, option = "G", end = 0.95) +
#   #  geom_point(data = atp.dat, aes(x = Lat, y = Living.C.ng.L), fill = "red", shape = 22, stroke = 0.2, size = 3) +
#   facet_wrap(.~Cruise, scales = "free_x") +
#   geom_point(data = atp.plot.dat, aes(x = Lat, y = Mean.Percent.Living), fill = "red", shape = 22, stroke = 0.2, size = 3) +
#   theme_bw() +
#   ylab("Percent Living Biomass (%)") +
#   geom_hline(yintercept = 30, color = "darkred", linetype = "dashed")
# 
# 
# 
# 
# ggplot(osmo.biomass.prediction, aes(x = Lat, y = Osmo.living.biomass.ngL)) +
#   geom_point(aes(fill = TOD), shape = 21, size = 3, stroke = 0.2) +
#   scale_fill_viridis(discrete = TRUE, direction = -1, option = "G", end = 0.95) +
#   #  geom_point(data = atp.dat, aes(x = Lat, y = Living.C.ng.L), fill = "red", shape = 22, stroke = 0.2, size = 3) +
#   facet_wrap(.~Cruise, scales = "free_x") +
#    geom_point(data = atp.plot.dat, aes(x = Lat, y = Mean.living.biomass.ngL), fill = "red", shape = 22, stroke = 0.2, size = 3) +
#   theme_bw() +
#   ylab("Living Biomass (ng C/L)") 
# 
# 
# 

















































































































































































