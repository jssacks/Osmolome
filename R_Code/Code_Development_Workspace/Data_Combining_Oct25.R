



#Script for matching up osmolyte data with XXX and XXX:



library(tidyverse)




#Define inputs:

#osmolyte and metadata
osmo.file <- "Intermediates/Full_Particulate_Dissolved_Osmolome_Dat_100725.csv"
meta.file <- "Intermediates/All_metadata_information.csv"
  

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


##Match metadata and sample data:
g.samp.enviro.dat <- left_join(g.samp.dat, gradients.dat) %>%
  mutate(keep = case_when(str_detect(Parent_ID, "TN397_S3_U") & pc_interp < 1.36 ~ "no",
                          TRUE ~ "yes")) %>%
  filter(keep == "yes") %>%
  select(-keep) %>%
  unique()




###























































































































