

###Overall metadata compillation:

#Define inputs:

#volume.filtered data
g4.vol.file  <- "Meta_Data/G4_vol_filt.csv"
g3.vol.file <- "Meta_Data/G3_vol_filt.csv"
d1.vol.file <- "Meta_Data/RC078_metadata.csv"
perifix.file <-  "Meta_Data/PERIFIX_metadata_2.csv"
g4dp.file <- "Meta_Data/G4_DepthProfile_metadata_2.csv"
g3dp.file <- "Meta_Data/G3_DepthProfile_metadata.csv"
rr.file <- "Meta_data/RR_MetaData.csv"

#Location data:
g3.loc.file <- "Meta_Data/G3_Samp_Locations.csv"
g4.loc.file <- "Meta_Data/G4_Samp_Locations.csv"
d1.ctd.file <- "Meta_Data/RC078_ctd_data.csv"

#Time data:
d1.d.t.file <- "Meta_Data/Rc078_Local_Date_Time.csv"

#KinExp Metadata:
kin.exp.meta.file <- "Meta_Data/KinExp_MetaData.csv"

#culture data:
culture.meta.file <- "Meta_Data/Culture_Meta_Data.csv"





#____Make vol filtered dataset_______

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

#G3 Depth profiles
g3dp.vol.filt <- read_csv(g3dp.file) %>%
  select(SampID, Vol_L)  %>%
  mutate(Cruise = "G3_DepthProfiles")

#G2 Resource Ratio:
RR.vol.filt <- read_csv(rr.file) %>%
  select(SampID, Vol_L) %>%
  mutate(Cruise = "RR")


###All Volume Filtered Data:
vol.dat <- rbind(g4.vol.filt, g3.vol.filt, d1.vol.filt, g4dp.vol.filt, perifix.vol.filt, g3dp.vol.filt, RR.vol.filt)

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

#Locations
d1.samp.locs <- read_csv(d1.ctd.file) %>%
  dplyr::select(Station, latitude, longitude) %>%
  group_by(Station) %>%
  summarise(Long = mean(longitude),
            Lat = mean(latitude)) %>%
  mutate(station = as.numeric(str_remove(Station, "S"))) %>%
  select(-Station)

#date and time
d1.time <- read_csv(d1.d.t.file) %>%
  rename("SampID" = sample_id) %>%
  mutate(SampID = paste("221006_Smp_", SampID, sep = ""),
         Cruise = "RC078") %>%
  select(-parent_id) %>%
  filter(!is.na(Local_Date))

#Volumes, depths, stations
d1.samp.info <- read_csv(d1.vol.file) %>%
  filter(!is.na(depth_m),
         !is.na(station)) %>%
  select(sample_id, parent_id, station, depth_m) %>%
  rename("SampID" = sample_id) %>%
  mutate(SampID = paste("221006_Smp_", SampID, sep = ""),
         Cruise = "RC078") %>%
  select(SampID, Cruise, station, depth_m, Cruise) %>%
  filter(!str_detect(SampID, "-")) %>%
  unique() %>%
  left_join(., d1.samp.locs) %>%
  left_join(., d1.time) 

#PERIFIX
perifix.samp.info <- read_csv(perifix.file) %>%
  select(-Vol_L, -`...9`) %>%
 # select(SampID, Treatment) %>%
  mutate(Cruise = "PERIFIX")

#G4 Depth Profiles
g4dp.samp.info <- read_csv(g4dp.file) %>%
  mutate(Cruise = "G4_DepthProfiles",
         Long = -1*abs(Lon)) %>%
  mutate(station = str_extract(SampID, "_S\\d+"),
         station = as.numeric(str_remove(station, "_S"))) %>%
  rename("depth_m" = Depth_m) %>%
  select(SampID, Cruise, station, depth_m, Lat, Long, Local_Date, Local_Time)
  

#G3 Depth Profiles
g3dp.samp.info <- read_csv(g3dp.file) %>%
  mutate(Cruise = "G3_DepthProfiles",
         Long = -1*abs(Lon)) %>%
  rename("depth_m" = Depth_m,
         "Local_Date" = Date,
         "Local_Time" = Time) %>%
  select(SampID, Cruise, station, depth_m, Lat, Long, Local_Date, Local_Time)






##All Sample Information:
all.samp.info <- g4.samp.info %>%
  full_join(., g3.samp.info) %>%
  full_join(., d1.samp.info) %>%
  full_join(., perifix.samp.info) %>%
  full_join(., g4dp.samp.info) %>%
  full_join(., g3dp.samp.info) %>%
  rename(Part.SampID = SampID)


##Add in Kin Experiment metadata:
kexp.meta <- read_csv(kin.exp.meta.file) %>%
  rename(Diss.SampID = SampID) %>%
  rename("Local_Date" = Date,
         "Local_Time" = Time)

all.samp.info.add <- full_join(all.samp.info, kexp.meta)

write_csv(all.samp.info.add, file = "Intermediates/All_metadata_information.csv")




###Culture Meta Data:
cult.meta.dat <- read_csv(culture.meta.file) %>%
  mutate(cell_volume_on_filter_uL = case_when(is.na(cell_volume_on_filter_uL) ~ Cell_Volume_um3*tot_cells_filt*(1/1E9),
                                              TRUE ~ cell_volume_on_filter_uL)) %>%
  rename("SampID" = Samp_ID) %>%
  mutate(SampID = str_remove(SampID, "_pos"))
  
write_csv(cult.meta.dat, file = "Intermediates/All_culture_metadata.csv")




###Dissolved_Particulate Match key:































































































