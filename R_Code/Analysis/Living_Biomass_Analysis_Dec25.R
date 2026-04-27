



library(tidyverse)
library(lubridate)
library(patchwork)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")




###______________define inputs____________________

#Osmolytes
all.dat.file <- "Intermediates/Enviro_Osmo_Final_Dataset_with_metadata.csv"

#environmental data:
g3.file <- "Intermediates/G3_metadata_with_interpolations.csv"
g4.file <- "Intermediates/G4_metadata_with_interpolations.csv"

#atp data:
atp.file <- "Intermediates/ATP_G3_G4_data.csv"






##load in and organize environmental data:
g3.dat <- read_csv(g3.file)
g4.dat <- read_csv(g4.file)

gradients.enviro.dat <- full_join(g3.dat, g4.dat) %>%
  rename("UTC.time.round" = time) 



#load in ATP data:
atp.dat <- read_csv(atp.file)

#Combine gradients data with atp data
gradients.atp.dat <- left_join(atp.dat, gradients.enviro.dat) %>%
  select(Cruise, Station, Lat, Long, UTC.time.round, PATP_ng_L, SD_PATP_ng_L, ATP_Flag, sst, sss, chla_interp, pc_interp, pn_interp, N_N_interp) %>%
  rename("chla" = chla_interp,
         "poc" = pc_interp,
         "pn" = pn_interp,
         "N_N" = N_N_interp) 


#ATP living biomass data:
atp.live.bio <- gradients.atp.dat %>%
  mutate(Mean.living.biomass.umolC.L = 250*PATP_ng_L/12.01/1000,
         SD.living.biomass.umolC.L = 250*SD_PATP_ng_L/12.01/1000,
         Mean.Percent.Living= Mean.living.biomass.umolC.L/poc*100,
         SD.Percent.Living = SD.living.biomass.umolC.L/poc*100) %>%
  mutate(cruise.order = case_when(Cruise == "TN397" ~ 1,
                                  TRUE ~ 2)) %>%
  mutate(Region = case_when(Cruise == "KM1906" & Lat >= 35 ~ "NPTZ",
                            Cruise == "KM1906" & Lat < 35 ~ "NPSG",
                            Cruise == "TN397" & Long > -122 ~ "CUCP",
                            Cruise == "TN397" & Long < -122 & Lat > 10.5 ~ "NPSG",
                            Cruise == "TN397" & Lat < 10.5 & Lat > 4.5 ~ "NPEC",
                            Cruise == "TN397" & Lat < 4.5 ~ "PEDP")) %>%
  mutate(Region = as.factor(Region)) %>%
  mutate(Region = fct_relevel(Region, c("PEDP", "NPEC", "NPSG", "NPTZ", "CUCP"))) 







#Load in osmolyte data:

#_____________Read in data________________________ 
osmo.dat <- read_csv(all.dat.file) %>%
  left_join(., compound.order) %>%
  filter(!str_detect(Part.SampID, "Blk")) %>%
  filter(!is.na(class)) %>%
  mutate(Region = case_when(Cruise == "KM1906" & Lat >= 35 ~ "NPTZ",
                            Cruise == "KM1906" & Lat < 35 ~ "NPSG",
                            Cruise == "TN397" & Long > -122 ~ "CUCP",
                            Cruise == "TN397" & Long < -122 & Lat > 10.5 ~ "NPSG",
                            Cruise == "TN397" & Lat < 10.5 & Lat > 4.5 ~ "NPEC",
                            Cruise == "TN397" & Lat < 4.5 ~ "PEDP",
                            Cruise == "RC078" ~ "SS")) %>%
  mutate(Region = as.factor(Region)) %>%
  mutate(Region = fct_relevel(Region, c("PEDP", "NPEC", "NPSG", "NPTZ", "CUCP", "SS"))) %>%
  filter(!is.na(class)) %>%
  mutate(class = as.factor(class)) %>%
  mutate(class = fct_relevel(class, c("AA", "Betaine", "Sugar", "Sulfonium", "Sulfonate", "Other"))) %>%
  filter(Part.detected == "Yes") %>%
  group_by(Parent_ID, Region, Cruise, Lat, Long, sss, sst, poc, chla, pn, N_N, SRP, sum.median.biovol.ul.l) %>%
  reframe(Sum.Part.Conc.nM = sum(Part.Conc.nM))


#Convert osmolytes to living biomass
osmo.live.bio <- osmo.dat  %>%
  mutate(Live.Biomass.umolC.L = 0.14388*Sum.Part.Conc.nM + 0.00124,
         Percent.Living = Live.Biomass.umolC.L/poc*100) %>%
  filter(!is.na(Live.Biomass.umolC.L)) %>%
  filter(!Cruise == "RC078")


###Calculate Regional means and standard deviations 
osmo.live.bio.sum <- osmo.live.bio %>%
  group_by(Region, Cruise) %>%
  reframe(mean.live.bio.umol.C.L = mean(Live.Biomass.umolC.L),
          sd.live.bio.umol.C.L = sd(Live.Biomass.umolC.L),
          min.live.bio.umol.C.L = min(Live.Biomass.umolC.L),
          max.live.bio.umol.C.L = max(Live.Biomass.umolC.L),
          mean.perc.live = mean(Percent.Living, na.rm = TRUE),
          sd.perc.live = sd(Percent.Living, na.rm = TRUE),
          min.perc.live = min(Percent.Living, na.rm = TRUE),
          max.perc.live = max(Percent.Living, na.rm = TRUE))
  
atp.live.bio.sum <- atp.live.bio  %>%
  group_by(Region, Cruise) %>%
  filter(!is.na(Region)) %>%
  reframe(mean.live.bio.umol.C.L = mean(Mean.living.biomass.umolC.L),
          sd.live.bio.umol.C.L = sd(Mean.living.biomass.umolC.L),
          min.live.bio.umol.C.L = min(Mean.living.biomass.umolC.L),
          max.live.bio.umol.C.L = max(Mean.living.biomass.umolC.L),
          mean.perc.live = mean(Mean.Percent.Living, na.rm = TRUE),
          sd.perc.live = sd(Mean.Percent.Living, na.rm = TRUE),
          min.perc.live = min(Mean.Percent.Living, na.rm = TRUE),
          max.perc.live = max(Mean.Percent.Living, na.rm = TRUE))


###Percent Living Stats:

#Osmolytes:
osmo.percent.living.w.anova <- osmo.live.bio %>%
  group_by(Cruise) %>%
  welch_anova_test(Percent.Living ~ Region)

osmo.percent.living.gh.test <- osmo.live.bio %>%
  group_by(Cruise) %>%
  games_howell_test(Percent.Living ~ Region)


#ATP:
atp.percent.living.w.anova <- atp.live.bio %>%
  group_by(Cruise) %>%
  welch_anova_test(Mean.Percent.Living ~ Region)

atp.percent.living.gh.test <- atp.live.bio %>%
  group_by(Cruise) %>%
  games_howell_test(Mean.Percent.Living ~ Region)



