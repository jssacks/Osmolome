







library(tidyverse)
library(lubridate)




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
gradients.atp.dat <- left_join(atp.time.dat, gradients.enviro.dat) %>%
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
                                  TRUE ~ 2))







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
         Percent.Living = Live.Biomass.umolC.L/poc*100)
  
osmo.live.bio.sum <- osmo.live.bio %>%
  group_by(Region, Cruise, Lat, Long, sss, sst, poc, chla) %>%
  reframe(Mean.Percent.Living = mean(Percent.Living),
          SD.Percent.Living = sd(Percent.Living)) %>%
  filter(!Cruise == "RC078") %>%
  mutate(cruise.order = case_when(Cruise == "TN397" ~ 1,
                                  TRUE ~ 2))



### Plot G3 and G4 Samples:
#Combined:
comb.percent.living.plot <- ggplot(osmo.live.bio.sum, aes(x = Lat, y = Mean.Percent.Living)) +
  geom_hline(yintercept = 30, color = "gray40", linetype = "dashed") +
 # geom_vline(data = lines.df, aes(xintercept = Lat), linetype = "dashed", size = 0.2) +
  geom_errorbar(aes(ymin = Mean.Percent.Living-SD.Percent.Living, ymax = Mean.Percent.Living+SD.Percent.Living), size = 0.2) +
  #  geom_errorbar(data = Osmo.biomass.sml, aes(ymin = Mean.osmo.percent.living-SD.osmo.percent.living, ymax = Mean.osmo.percent.living+SD.osmo.percent.living)) +
  geom_point(aes(fill = Region), shape = 21, size = 3, stroke = 0.25) +
  scale_fill_manual(values = region.palette.7) +
 # scale_fill_viridis(discrete = TRUE, direction = -1, option = "G", end = 0.95) +
  #  geom_point(data = atp.dat, aes(x = Lat, y = Living.C.ng.L), fill = "red", shape = 22, stroke = 0.2, size = 3) +
  facet_grid(.~reorder(Cruise, cruise.order), scales = "free_x") +
  geom_errorbar(data = atp.live.bio, aes(ymin = Mean.Percent.Living-SD.Percent.Living, ymax = Mean.Percent.Living+SD.Percent.Living), size = 0.2) +
  geom_point(data = atp.live.bio, aes(x = Lat, y = Mean.Percent.Living), 
             shape = 22, stroke = 0.4, size = 2, fill = "red", alpha = 0.6) +
  theme_bw() +
  ylab("Percent Living Biomass (%)") +
  scale_y_continuous(limits = c(0,NA), expand = c(0,NA)) 
comb.percent.living.plot
















































