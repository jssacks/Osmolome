




library(tidyverse)
library(lubridate)



##Define inputs:

#ATP data:
atp.time.file <- "Collaborator_Data/ATP_all/G3G4_ATP_TimeCollection.csv"
atp.measurements.file <- "Collaborator_Data/ATP_all/ATP_G3_G4.csv"

#environmental data:
g3.file <- "Intermediates/G3_metadata_with_interpolations.csv"
g4.file <- "Intermediates/G4_metadata_with_interpolations.csv"
 



#Load in datasets:
atp.time.dat.raw <- read_csv(atp.time.file)
atp.measurements.dat <- read_csv(atp.measurements.file) %>%
  rename(Station = `Station...2`,
         POC_Paired = PC) %>%
  select(Cruise, Station, Lat, Long, Depth, POC_Paired, PATP_ng_L, SD_PATP_ng_L, ATP_Flag)

g3.dat <- read_csv(g3.file)
g4.dat <- read_csv(g4.file)

#Convert both sets of time to datetime objects in GMT

## G4
g4.atp.tod <- atp.time.dat.raw %>%
  filter(Cruise == "TN397") %>%
  mutate(UTC.dt = ymd_hms(DT_GMT)) %>%
  mutate(UTC.time.round = round_date(UTC.dt, unit = "hour")) %>%
  select(Cruise, Station, UTC.dt, UTC.time.round) %>% 
  unique()

## G3
g3.atp.tod <- atp.time.dat.raw %>%
  filter(Cruise == "KM1906") %>%
  mutate(date_2 = mdy(Date)) %>%
  unite(c("date_2", "Time_HST"), col = "Local_DT", remove = FALSE, sep = " ") %>%
  mutate(Local_DT_obj = ymd_hms(Local_DT)) %>%
  mutate(UTC.dt = Local_DT_obj + hours(10)) %>%
  mutate(UTC.time.round = round_date(UTC.dt, unit = "hour")) %>%
  select(Cruise, Station, UTC.dt, UTC.time.round)
  
##Combine ATP datasets
atp.time.dat <- rbind(g4.atp.tod, g3.atp.tod) %>%
  full_join(., atp.measurements.dat)


##Organize Environmental data:
gradients.enviro.dat <- full_join(g3.dat, g4.dat) %>%
  rename("UTC.time.round" = time) 


#create a dataset that only interpolates within 12 hours on either side of the datapoint 
pp13c.g.env.dat <- gradients.enviro.dat %>%
  select(UTC.time.round, pp_13c, pp_13c_interp) %>%
  mutate(pp13c_time = case_when(!is.na(pp_13c) ~ UTC.time.round,
                                TRUE ~ NA),
         pp13c_time_min = pp13c_time - hours(12),
         pp13c_time_max = pp13c_time + hours(12)) %>%
  filter(!is.na(pp13c_time)) %>%
  select(pp_13c, pp13c_time, pp13c_time_min, pp13c_time_max) %>%
  cross_join(., gradients.enviro.dat %>% select(UTC.time.round, pp_13c_interp)) %>%
  filter(UTC.time.round > pp13c_time_min,
         UTC.time.round < pp13c_time_max) %>%
  select(UTC.time.round, pp_13c, pp_13c_interp) %>%
  filter(!is.na(pp_13c_interp))
  

##PP 14 C
pp14c.g.env.dat <- gradients.enviro.dat %>%
  select(UTC.time.round, pp_14c, pp_14c_interp) %>%
  mutate(pp14c_time = case_when(!is.na(pp_14c) ~ UTC.time.round,
                                TRUE ~ NA),
         pp14c_time_min = pp14c_time - hours(12),
         pp14c_time_max = pp14c_time + hours(12)) %>%
  filter(!is.na(pp14c_time)) %>%
  select(pp_14c, pp14c_time, pp14c_time_min, pp14c_time_max) %>%
  cross_join(., gradients.enviro.dat %>% select(UTC.time.round, pp_14c_interp)) %>%
  filter(UTC.time.round > pp14c_time_min,
         UTC.time.round < pp14c_time_max) %>%
  select(UTC.time.round, pp_14c, pp_14c_interp) %>%
  filter(!is.na(pp_14c_interp))




#Combine gradients data with atp data
gradients.atp.dat <- left_join(atp.time.dat, gradients.enviro.dat) %>%
  select(Cruise, Station, Lat, Long, UTC.time.round, PATP_ng_L, SD_PATP_ng_L, ATP_Flag, sst, sss, chla_interp, pc_interp, pn_interp, N_N_interp) %>%
  rename("chla" = chla_interp,
         "poc" = pc_interp,
         "pn" = pn_interp,
         "N_N" = N_N_interp) 

#combine gradients and data with PP 13C and 14C data:
pp.atp.dat <- gradients.atp.dat %>%
  left_join(., pp13c.g.env.dat) %>%
  left_join(., pp14c.g.env.dat) %>%
  mutate(pp13c_atp_ratio = pp_13c_interp/PATP_ng_L,
         pp14c_atp_ratio = pp_14c_interp/PATP_ng_L) %>%
  filter(is.na(ATP_Flag)) %>%
  filter(!Station == "UW 8") 


write_csv(pp.atp.dat, file = "Intermediates/Gradients_ATP_Enviro_Meta_Data.csv")







### Make plots of PP vs. ATP
ggplot(pp.atp.dat, aes(x = Lat, y = pp13c_atp_ratio)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(size = 3, fill = "blue", shape = 21, color = "black", stroke = 0.2) +
  xlim(c(-4, 30)) +
  ylim(c(0, 0.45)) +
  ylab("13C Primary Production/P-ATP")


ggplot(pp.atp.dat, aes(x = Lat, y = pp14c_atp_ratio)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(size = 3, fill = "blue", shape = 21, color = "black", stroke = 0.2) +
  xlim(c(-4, 30)) +
  ylim(c(0, 0.6)) +
  ylab("14C Primary Production/P-ATP")







###Make some plots for comparison 

###Check on ATP to POC Ratio 
atp.poc.ratio.dat <- gradients.atp.dat %>%
  select(Cruise, Station, Lat, Long, PATP_ng_L, SD_PATP_ng_L, poc, chla, ATP_Flag) %>%
  mutate(Living_Biomass_nM_C = PATP_ng_L*250/12.01,
         SD_Living_Biomass_nM_C = SD_PATP_ng_L*250/12.01,
         Percent_Living = Living_Biomass_nM_C/(poc*1000)*100,
         SD_Percent_Living = SD_Living_Biomass_nM_C/(poc*1000)*100,
         chl_poc_ratio = chla/poc) %>%
  filter(is.na(ATP_Flag)) %>%
  filter(!Station == "UW 8") %>%
  mutate(Cruise = as.factor(Cruise)) %>%
  mutate(Cruise = fct_relevel(Cruise, c("TN397", "KM1906")))


##Plot just ATP living biomass data:
ggplot(atp.poc.ratio.dat, aes(x = Lat, y = Living_Biomass_nM_C)) +
#  geom_hline(yintercept = 30, color = "darkred", linetype = "dashed") +
  geom_errorbar(aes(ymin = Living_Biomass_nM_C-SD_Living_Biomass_nM_C, ymax = Living_Biomass_nM_C+SD_Living_Biomass_nM_C)) +
  geom_point(shape = 21, fill = "darkgray", size = 3, stroke = 0.2, color = "black") +
  facet_wrap(.~Cruise, scales = "free") +
#  ylim(c(0,100)) +
  theme_bw() +
  ylab("PATP Estimated Living Biomass (nM C)")





#Separate cruises:
ggplot(atp.poc.ratio.dat, aes(x = Lat, y = Percent_Living)) +
  geom_hline(yintercept = 30, color = "darkred", linetype = "dashed") +
  geom_errorbar(aes(ymin = Percent_Living-SD_Percent_Living, ymax = Percent_Living+SD_Percent_Living)) +
  geom_point(shape = 21, fill = "darkgray", size = 3, stroke = 0.2, color = "black") +
  facet_wrap(.~Cruise, scales = "free") +
  ylim(c(0,100)) +
  theme_bw() +
  ylab("Percent Living Biomass (%) \n (Karl Lab PATP-estimated living biomass/White Lab POC)")

#G3 and G4 combined 
ggplot(atp.poc.ratio.dat, aes(x = Lat, y = Percent_Living)) +
  geom_hline(yintercept = 30, color = "darkred", linetype = "dashed") +
  geom_errorbar(aes(ymin = Percent_Living-SD_Percent_Living, ymax = Percent_Living+SD_Percent_Living)) +
  geom_point(aes(shape = Cruise, fill = log10(chla)), size = 3, stroke = 0.2, color = "black") +
  scale_shape_manual(values = c(21, 23)) +
  scale_fill_viridis() +
  ylim(c(0,100)) +
  theme_bw() 



#Just plot chla
ggplot(atp.poc.ratio.dat, aes(x = Lat, y = Percent_Living)) +
  geom_hline(yintercept = 30, color = "darkred", linetype = "dashed") +
  geom_errorbar(aes(ymin = Percent_Living-SD_Percent_Living, ymax = Percent_Living+SD_Percent_Living)) +
  geom_point(aes(shape = Cruise, fill = log(chla)), size = 3, stroke = 0.2, color = "black") +
  scale_shape_manual(values = c(21, 23)) +
  scale_fill_viridis() +
  ylim(c(0,100)) +
  theme_bw() 



ggplot(atp.poc.ratio.dat, aes(x = Lat, y = chla)) +
  geom_point() +
  facet_wrap(.~Cruise, scales = "free")

ggplot(atp.poc.ratio.dat, aes(x = Lat, y = chl_poc_ratio)) +
  geom_point() +
  facet_wrap(.~Cruise, scales = "free") 

ggplot(atp.poc.ratio.dat, aes(x = chla, y = PATP_ng_L)) +
  geom_point() 

ggplot(atp.poc.ratio.dat, aes(x = chla, y = poc)) +
  geom_point() 

  # %>%
  # unite(c("Local_Date", "Local_Time"), col = "Local_DT", remove = FALSE, sep = " ") %>%
  # mutate(Local_DT_obj = mdy_hms(Local_DT)) %>%
  # mutate(time.round = round_date(Local_DT_obj, unit = "hour"),
  #        UTC.time.round = time.round + hours(10))








#match up if the time and xyz are correct.
  

  
  
#XXX
  
  
#XXX























































