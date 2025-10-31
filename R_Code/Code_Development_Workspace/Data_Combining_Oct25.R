



#Script for matching up osmolyte data with XXX and XXX:



library(tidyverse)




#Define inputs:

#osmolyte and metadata
osmo.file <- "Intermediates/Full_Particulate_Dissolved_Osmolome_Dat_100725.csv"
g3.gbt.replace.file <- "Intermediates/KM1906_Predicted_Dissolved_GBT_Concentrations.csv"

#metadata
meta.file <- "Intermediates/All_metadata_information.csv"
peri.meta.file <- "PERIFIX_metadata_2.csv"

#Environmental data
g3.file <- "Intermediates/G3_metadata_with_interpolations.csv"
g4.file <- "Intermediates/G4_metadata_with_interpolations.csv"
d1.file <- "Intermediates/RC078_MetaData_Compiled.csv"






#Load in datasets:

#osmolyte and metadata
osmo.dat <- read_csv(osmo.file)
meta.dat <- read_csv(meta.file)


#Environmental data
g3.dat <- read_csv(g3.file)
g4.dat <- read_csv(g4.file)
d1.dat <- read_csv(d1.file)





#### Match up osmolyte data and meta data:

#particulate matches:
osmo.meta.part <- left_join(meta.dat %>%
                              filter(!is.na(Part.SampID)) %>%
                              select(-Diss.SampID),
                            osmo.dat)

#dissolved only matches
osmo.meta.diss <- meta.dat %>%
  filter(is.na(Part.SampID)) %>%
  select(-Part.SampID) %>% 
  mutate(Diss.SampID = str_replace(Diss.SampID, "nm", "nM")) %>%
  mutate(Diss.SampID = str_replace(Diss.SampID, "2202602", "220602")) %>%
  left_join(., osmo.dat) %>%
  filter(!is.na(Compound))
  


###Combine dataset:
full.osmo.meta <- rbind(osmo.meta.part, osmo.meta.diss) %>%
  mutate(Parent_ID = str_remove(Part.SampID, "220628_Smp_")) %>%
  mutate(Parent_ID = str_remove(Parent_ID, "221006_Smp_")) %>%
  mutate(Parent_ID = str_remove(Parent_ID, "220902_Smp_")) %>%
  mutate(Parent_ID = case_when(is.na(Parent_ID) ~ Diss.SampID,
                               TRUE ~ Parent_ID)) %>%
  mutate(Parent_ID = str_remove(Parent_ID, "220602_Smp_")) 




##Replace GBT in KM1906 with predicted GBT concentrations:
gbt.replace.dat <- read_csv(g3.gbt.replace.file) %>%
  rename(Diss.SampID = SampID) %>%
  mutate(Parent_ID = str_remove(Diss.SampID, "220623_Smp_")) 

##GBT.G3 
g3.gbt.osmo.meta <- full.osmo.meta %>%
  filter(Compound == "Glycine betaine",
         Cruise == "KM1906") %>%
  select(-(Diss.detected:Diss.LOD.no.blk.sub.nM)) %>%
  left_join(gbt.replace.dat) %>%
  rename(Diss.detected = detected,
         Diss.Impute.Conc.nM = impute.conc.nM,
         Diss.LOD.nM = LOD.nM,
         Diss.LOD.no.blk.sub.nM = LOD.no.blk.sub.nM)


##add new dissolved GBT data into larger dataframe:
full.osmo.meta.2 <- full.osmo.meta %>%
  mutate(Predicted_Value = NA) %>%
  mutate(remove = case_when(Compound == "Glycine betaine" & Cruise == "KM1906" ~ "Yes",
                            TRUE ~ "No")) %>%
  filter(remove == "No") %>%
  select(-remove)  %>%
  rbind(., g3.gbt.osmo.meta)







##Match up gradients environmental data with metadata
#Gradients 4
g4.samp.dat <- meta.dat %>%
  mutate(Cruise = case_when(str_detect(Diss.SampID, "TN397") ~ "TN397",
                            TRUE ~ Cruise)) %>%
  filter(Cruise == "TN397") %>%
  unite(c("Local_Date", "Local_Time"), col = "Local_DT", remove = FALSE, sep = " ") %>%
  mutate(Local_DT_obj = dmy_hms(Local_DT)) %>%
  mutate(time.diff.val = case_when(Local_DT_obj < ymd_hms("2021-11-22 24:00:00") ~ 8,
                                   Local_DT_obj > ymd_hms("2021-11-22 24:00:00") & Local_DT_obj < ymd_hms("2021-12-10 24:00:00") ~ 9,
                                   TRUE ~ 10)) %>%
  mutate(time.round = round_date(Local_DT_obj, unit = "hour"),
         UTC.time.round = time.round + hours(time.diff.val)) %>%
  select(Part.SampID, Diss.SampID, Cruise, Lat, Long, Local_Date, Local_Time, depth_m, UTC.time.round)  %>%
  mutate(Parent_ID = str_remove(Part.SampID, "220628_Smp_")) %>%
  mutate(Parent_ID = str_remove(Parent_ID, "221006_Smp_")) %>%
  mutate(Parent_ID = str_remove(Parent_ID, "220902_Smp_")) %>%
  mutate(Parent_ID = case_when(is.na(Parent_ID) ~ Diss.SampID,
                               TRUE ~ Parent_ID)) %>%
  mutate(Parent_ID = str_remove(Parent_ID, "2202602_Smp_")) %>%
  select(-Part.SampID, -Diss.SampID)



#Gradients 3
g3.samp.dat <- meta.dat %>%
  # mutate(Cruise = case_when(str_detect(Diss.SampID, "MU12") ~ "KM1906",
  #                           str_detect(Diss.SampID, "MU13") ~ "KM1906",
  #                           TRUE ~ Cruise)) #%>%
  filter(Cruise == "KM1906") %>%
  unite(c("Local_Date", "Local_Time"), col = "Local_DT", remove = FALSE, sep = " ") %>%
  mutate(Local_DT_obj = mdy_hms(Local_DT)) %>%
  mutate(time.round = round_date(Local_DT_obj, unit = "hour"),
         UTC.time.round = time.round + hours(10)) %>%
  select(Part.SampID, Diss.SampID, Cruise, Lat, Long, Local_Date, Local_Time, depth_m, UTC.time.round) %>%
    mutate(Parent_ID = str_remove(Part.SampID, "220628_Smp_")) %>%
    mutate(Parent_ID = str_remove(Parent_ID, "221006_Smp_")) %>%
    mutate(Parent_ID = str_remove(Parent_ID, "220902_Smp_")) %>%
    mutate(Parent_ID = case_when(is.na(Parent_ID) ~ Diss.SampID,
                                 TRUE ~ Parent_ID)) %>%
    mutate(Parent_ID = str_remove(Parent_ID, "2202602_Smp_")) %>%
    select(-Part.SampID, -Diss.SampID) %>%
  unique()


##all gradients samples for matching:
g.samp.dat <- rbind(g4.samp.dat, g3.samp.dat)


##Match up gradients environmental data
gradients.dat <- full_join(g3.dat, g4.dat)  %>%
  rename("UTC.time.round" = time) 


##Match metadata and sample data, organize and only include interpolated data in final dataframe:
g.samp.enviro.dat <- left_join(g.samp.dat, gradients.dat) %>%
  mutate(keep = case_when(str_detect(Parent_ID, "TN397_S3_U") & pc_interp < 1.36 ~ "no",
                          TRUE ~ "yes")) %>%
  filter(keep == "yes") %>%
  select(-keep) %>%
  unique() %>%
  select(Cruise, Lat, Long, UTC.time.round, Local_Date, Local_Time, depth_m, Parent_ID, sst, sss, chla_interp, pc_interp, pn_interp, N_N_interp) %>%
  rename(chla = chla_interp,
         poc = pc_interp,
         pn = pn_interp,
         N_N = N_N_interp) %>%
  mutate(doc = NA,
         Station = NA)



### Match up all metadata and environmental data with osmolyte data:
g.osmo.full <- left_join(full.osmo.meta.2, g.samp.enviro.dat) %>%
  filter(Cruise %in% c("TN397", "KM1906")) 






######Combine gradients environmental data with Carson Cruise Environmental Data:
d1.samp.IDs <- meta.dat %>%
  filter(Cruise == "RC078") %>%
  filter(!is.na(Part.SampID)) %>%
  mutate(ParentID = str_remove(Part.SampID, "221006_Smp_"))
# 



d1.env.dat <- read_csv(d1.file) %>%
  filter(!is.na(depth_m),
         !is.na(station)) %>%
  # select(sample_id, parent_id, station, depth_m) %>%
#  rename("SampID" = sample_id) %>%
#  mutate(SampID = paste("221006_Smp_", SampID, sep = ""),
  mutate(Cruise = "RC078") %>%
  #  select(SampID, Cruise, station, depth_m, Cruise) %>%
#  filter(!str_detect(SampID, "-")) %>%
  unique() %>%
  rename("poc" = "POC_uM",
         "pn" = "PN_uM",
         "sss" = sal,
         "sst" = temp,
         "chla" = Chl_fluor,
         "doc" = DOC_uM) %>%
  mutate("N_N" = NO3 + NO2) %>%
 # rename(Part.SampID = SampID) %>%
  left_join(., d1.samp.IDs) %>%
  rename(Parent_ID = sample_id) %>%
  select(Cruise, Lat, Long, Local_Date, Local_Time, Station, depth_m, Parent_ID, sss, sst, chla, poc, pn, N_N, doc) %>%
  mutate(UTC.time.round = NA)



##### D1 environmental data with osmolyte data:
d.osmo.full <- left_join(full.osmo.meta.2, d1.env.dat) %>%
  filter(Cruise %in% c("RC078"))





###Combine gradients and dinimite datasets and remove samples that will not be analyzed in this project:
g.d.osmo.full <- rbind(g.osmo.full, d.osmo.full) %>%
  filter(!str_detect(Part.SampID, "BB")) %>%
  mutate(keep = case_when(Cruise == "RC078" & is.na(Diss.SampID) ~ "No",
                          TRUE ~ "Yes")) %>% 
  filter(keep == "Yes") %>%
  select(-keep)

##export
write_csv(g.d.osmo.full, file = "Intermediates/Enviro_Osmo_Final_Dataset_with_metadata.csv")




















































































































































