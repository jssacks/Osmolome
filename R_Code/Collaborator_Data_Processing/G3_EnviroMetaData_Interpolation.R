

#load required packages
library(tidyverse)
library(stinepack)


#Define inputs:

#KM1906 (Gradeints 3)
nut.uw.file <- "Collaborator_Data/KM1906/Gradients3_KM1906_AA_Nutrients_UW.csv"
pc.uw.file <- "Collaborator_Data/KM1906/Gradients3_KM1906_PCPN_UW.csv"
#g3.don.file <- no DON data from what I can tell
ts.file <- "Collaborator_Data/KM1906/KM1906_Gradients3_uw_tsg.csv"
#bact.file <- "Collaborator_Data/KM1906/G3_Influx_Stations_Gradients_2019.csv"
chla.file <- "Collaborator_Data/KM1906/Gradients3_KM1906_Optics_LISST_ACS_ECO.csv"

#Read in datasets:
nut.uw.dat <- read_csv(nut.uw.file)
pc.uw.dat <- read_csv(pc.uw.file)
#don.dat <- read_csv(g3.don.file).    #No DON Data for G3
ts.dat <- read_csv(ts.file)
chla.dat <- read_csv(chla.file)
#pp.13c.dat <- read_csv(pp.13c.file)

#select desired variables and summarize datasets at the level of hour

##temp and sal
ts.hr <- ts.dat %>%
  mutate(time = round_date(time, unit = "hour")) %>%
  group_by(time) %>%
  mutate(sst = mean(SST),
         sss = mean(salinity),
         lat = mean(lat),
         lon = mean(lon)) %>%
  select(time, sst, sss, lat, lon) %>%
  unique()

##Chl-a
chla.hr <- chla.dat %>%
  mutate(time = round_date(time, unit = "hour")) %>%
  group_by(time) %>%
  mutate(chla = mean(chla_acs, na.rm = TRUE)) %>%
  select(time, chla) %>%
  ungroup() %>%
  unique() %>%
  filter(!chla == "NaN")

##PC and PN
pcpn.hr <- pc.uw.dat %>%
  mutate(time = round_date(time, unit = "hour")) %>%
  group_by(time) %>%
  mutate(pc = mean(pc),
         pn = mean(pn)) %>%
  select(time, pc, pn) %>%
  unique()

##Nutrients
nut.hr <- nut.uw.dat %>%
  mutate(Nitrate_Nitrite = case_when(Nitrate_Nitrite_below_detection_flag == 1 ~ 0.001,
                         TRUE ~ Nitrate_Nitrite)) %>%
  mutate(time = round_date(time, unit = "hour")) %>%
  group_by(time) %>%
  mutate(N_N = mean(Nitrate_Nitrite),
         SRP = mean(SRP)) %>%
  select(time, N_N, SRP) %>%
  unique()

##DON
#don.hr <- don.dat %>%
#  filter(depth < 10) %>%
#  mutate(time = round_date(time, unit = "hour")) %>%
#  group_by(time) %>%
#  mutate(DON = mean(DON)) %>%
#  select(time, DON) %>%
#  unique()

##Bacteria
bact.hr <- bact.dat %>%
  filter(depth < 16) %>%
  mutate(time = round_date(time, unit = "hour")) %>%
  group_by(time) %>%
  mutate(bact_abu = mean(abundance_bacteria, na.rm = T),
         bact_c = mean(biomass_bacteria, na.rm = T)) %>%
  select(time, bact_abu, bact_c) %>%
  unique()


###_________Put it all together and attempt interpolation
full.dat <- ts.hr %>%
  left_join(., chla.hr) %>%
  left_join(., pcpn.hr) %>%
  left_join(., nut.hr) %>%
#  left_join(., don.hr) %>%
#  left_join(., bact.hr) %>%
  arrange(time)


##Use stineman interpolation to interpolate values for all variables 

#chla
chla.interp <- data_frame(chla_interp = na.stinterp(full.dat$chla, along = time(full.dat$time), na.rm = F))

#pc
pc.interp <- data_frame(pc_interp = na.stinterp(full.dat$pc, along = time(full.dat$time), na.rm = F))

#pn
pn.interp <- data_frame(pn_interp = na.stinterp(full.dat$pn, along = time(full.dat$time), na.rm = F))

#N_N
N_N.interp <- data_frame(N_N_interp = na.stinterp(full.dat$N_N, along = time(full.dat$time), na.rm = F))

#SRP
SRP.interp <- data_frame(SRP_interp = na.stinterp(full.dat$SRP, along = time(full.dat$time), na.rm = F))


###Put all datasets together:
full.interp <- full.dat %>%
  cbind(., chla.interp, pc.interp, pn.interp, N_N.interp, SRP.interp)

write_csv(full.interp, file = "Intermediates/G3_metadata_with_interpolations.csv")


#Prelim_Viz
ggplot(full.interp, aes(x = lat, y = N_N_interp)) +
  geom_point(aes(color = lat)) +
  geom_path() +
  geom_point(aes(x = lat, y = N_N), color = "red")

#chla
ggplot(full.interp, aes(x = lat, y = chla_interp)) +
  geom_point(aes(color = lat)) +
  geom_path() +
  geom_point(aes(x = lat, y = chla), color = "red")

#poc
ggplot(full.interp, aes(x = lat, y = pc_interp)) +
  geom_point(aes(color = lat)) +
  geom_path() +
  geom_point(aes(x = lat, y = pc), color = "red")

#pon
ggplot(full.interp, aes(x = lat, y = pn_interp)) +
  geom_point(aes(color = lat)) +
  geom_path() +
  geom_point(aes(x = lat, y = pn), color = "red")

#srp
ggplot(full.interp, aes(x = lat, y = SRP_interp)) +
  geom_point(aes(color = lat)) +
  geom_path() +
  geom_point(aes(x = lat, y = SRP), color = "red")














































































