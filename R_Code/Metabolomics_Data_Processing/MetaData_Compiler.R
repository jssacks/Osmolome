

###Overall metadata compillation:

#Define inputs:

#volume.filtered data
g4.vol.file  <- "Meta_Data/Ingalls_Lab_Data/G4_vol_filt.csv"
g3.vol.file <- "Meta_Data/Ingalls_Lab_Data/G3_vol_filt.csv"
d1.vol.file <- "Meta_Data/Ingalls_Lab_Data/RC078_metadata.csv"
perifix.file <-  "Meta_Data/Ingalls_Lab_Data/PERIFIX_metadata_2.csv"
g4dp.file <- "Meta_Data/Ingalls_Lab_Data/G4_DepthProfile_metadata_2.csv"

#Location data:
g3.loc.file <- "Meta_Data/Ingalls_Lab_Data/G3_Samp_Locations.csv"
g4.loc.file <- "Meta_Data/Ingalls_Lab_Data/G4_Samp_Locations.csv"



#G4
g4.vol.filt <- read_csv(g4.vol.file) %>%
 mutate(SampID = paste("220902_Smp_", Samp_ID, sep = "")) %>%
 rename("Vol_L" = Vol_Filt_L) %>%
 select(SampID, Vol_L) %>%
 mutate(Cruise = "TN397")

#G3
g3.vol.filt <- read_csv(g3.vol.file) %>%
 rename("SampID" = `Sample ID`,
        "Vol_L" = `Vol Filtered (L)`) %>%
 filter(str_detect(SampID, "MU")) %>%
 mutate(SampID = paste("220628_Smp_", SampID, sep = "")) %>%
 mutate(Vol_L = as.numeric(str_remove(Vol_L, "L"))) %>%
 mutate(Cruise = "KM1906")

#D1
d1.vol.filt <- read_csv(d1.vol.file) %>%
 filter(sample_type == "METABS") %>%
 select(sample_id, parent_id, station, depth_m, cast, niskin, vol_filt_l) %>%
 rename("SampID" = sample_id,
        "Vol_L" = vol_filt_l) %>%
 mutate(SampID = paste("221006_Smp_", SampID, sep = "")) %>%
 select(SampID, Vol_L) %>%
 mutate(Cruise = "RC078")

#perifix
perifix.vol.filt <- read_csv(perifix.file) %>%
  select(SampID, Vol_L) %>%
  mutate(Cruise = "PERIFIX")

#G4 Depth Profiles
g4dp.vol.filt <- read_csv(g4dp.file) %>%
  select(SampID, Vol_L)  %>%
  mutate(Cruise = "G4_DepthProfiles")

###All Volume Filtered Data:
vol.dat <- rbind(g4.vol.filt, g3.vol.filt, d1.vol.filt, g4dp.vol.filt, perifix.vol.filt)

write_csv(vol.dat, file = "Intermediates/all_vol_filt_data.csv")




####Make file of Location and Depth... 

#G4
g4.samp.info <- read_csv(g4.loc.file) %>% 
  filter(!is.na(Samp_ID)) %>%
  mutate(SampID = paste("220902_Smp_", Samp_ID, sep = "")) %>%
  mutate(Cruise = "TN397",
         Long = -1*abs(Long_W)) %>%
  rename("Lat" = Lat_N) %>%
  select(SampID, Cruise, Lat, Long, Local_Date, Local_Time) %>%
  mutate(depth_m = 7)


#G3
g3.samp.info <- read_csv(g3.loc.file) %>%
  mutate(Cruise = "KM1906") %>%
  rename("SampID" = Sample_ID) %>%
  filter(str_detect(SampID, "MU")) %>%
  mutate(SampID = paste("220628_Smp_", SampID, sep = "")) %>%
  mutate(depth_m = 7)


#D1
d1.samp.info <- read_csv(d1.vol.file) %>%
  filter(!is.na(depth_m),
         !is.na(station)) %>%
  select(sample_id, parent_id, station, depth_m) %>%
  rename("SampID" = sample_id) %>%
  mutate(SampID = paste("221006_Smp_", SampID, sep = ""),
         Cruise = "RC078") %>%
  select(SampID, Cruise, station, depth_m, Cruise) %>%
  unique()

#PERIFIX
perifix.samp.info <- read_csv(perifix.file) %>%
  select(SampID, Treatment) %>%
  mutate(Cruise = "PERIFIX")

#G4 Depth Profiles
g4dp.samp.info <- read_csv(g4dp.file) %>%
  mutate(Cruise = "G4_DepthProfiles",
         Long = -1*abs(Lon)) %>%
  mutate(station = str_extract(SampID, "_S\\d+"),
         station = as.numeric(str_remove(station, "_S"))) %>%
  rename("depth_m" = Depth_m) %>%
  select(SampID, Cruise, station, depth_m, Lat, Long)
  

##All Sample Information:
all.samp.info <- g4.samp.info %>%
  full_join(., g3.samp.info) %>%
  full_join(., d1.samp.info) %>%
  full_join(., perifix.samp.info) %>%
  full_join(., g4dp.samp.info) 

write_csv(all.samp.info, file = "Intermediates/All_metadata_information.csv")





































































































