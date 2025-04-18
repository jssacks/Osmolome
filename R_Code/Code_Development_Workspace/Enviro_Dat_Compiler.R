


library(tidyverse)




#sample lists
all.data.file <- "Intermediates/Combined_Final_Osmolyte_Dataset.csv"



#environmental data
g3.file <- "Intermediates/G3_metadata_with_interpolations.csv"
g4.file <- "Intermediates/G4_metadata_with_interpolations.csv"
d1.file <- "Intermediates/RC078_MetaData_Compiled.csv"



#read in datasets:
g3.dat <- read_csv(g3.file)
g4.dat <- read_csv(g4.file)
d1.dat <- read_csv(d1.file)


###read in sample meta data
meta.dat <- read_csv(all.data.file) %>%
  select(Part.SampID, Diss.SampID, Cruise, Local_Date, Local_Time, Lat, Long, depth_m) %>%
  unique()




#Attempt to match environmental data with samples based on time


#gradients environmental data
gradients.dat <- full_join(g3.dat, g4.dat) %>%
  rename("UTC.time.round" = time)


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
  select(Part.SampID, Diss.SampID, Cruise, Lat, Long, Local_Date, Local_Time, depth_m, UTC.time.round)  

#Gradients 3
g3.samp.dat <- meta.dat %>%
  mutate(Cruise = case_when(str_detect(Diss.SampID, "MU12") ~ "KM1906",
                            str_detect(Diss.SampID, "MU13") ~ "KM1906",
                              TRUE ~ Cruise)) %>%
  filter(Cruise == "KM1906") %>%
  unite(c("Local_Date", "Local_Time"), col = "Local_DT", remove = FALSE, sep = " ") %>%
  mutate(Local_DT_obj = mdy_hms(Local_DT)) %>%
  mutate(time.round = round_date(Local_DT_obj, unit = "hour"),
         UTC.time.round = time.round + hours(10)) %>%
  select(Part.SampID, Diss.SampID, Cruise, Lat, Long, Local_Date, Local_Time, depth_m, UTC.time.round)


##all gradients samples for matching:
g.samp.dat <- rbind(g4.samp.dat, g3.samp.dat)

#gradients environmental data
gradients.dat <- full_join(g3.dat, g4.dat) %>%
  rename("UTC.time.round" = time) 
  
##Match metadata and sample data:
g.samp.enviro.dat <- left_join(g.samp.dat, gradients.dat) %>%
  mutate(keep = case_when(str_detect(Part.SampID, "TN397_S3_U") & pc_interp < 1.36 ~ "no",
                          TRUE ~ "yes")) %>%
  filter(keep == "yes") %>%
  select(-keep)


###write gradients environmental data to csv
write_csv(gradients.dat, file = "Intermediates/combined_gradients_enviromental_data.csv")

##write gradients combined sample data to csv
write_csv(g.samp.enviro.dat, file = "Intermediates/Gradients_Matched_Environmental_Metadata.csv")





###Combine gradients environmental data with Carson Cruise Environmental Data:
d1.samp.IDs <- meta.dat %>%
  mutate(Cruise = case_when(str_detect(Diss.SampID, "RC078") ~ "RC078",
                            TRUE ~ Cruise)) %>%
  filter(Cruise == "RC078") %>%
  select(Part.SampID, Diss.SampID)


d1.env.dat <- read_csv(d1.file) %>%
  filter(!is.na(depth_m),
         !is.na(station)) %>%
  # select(sample_id, parent_id, station, depth_m) %>%
  rename("SampID" = sample_id) %>%
  mutate(SampID = paste("221006_Smp_", SampID, sep = ""),
         Cruise = "RC078") %>%
  #  select(SampID, Cruise, station, depth_m, Cruise) %>%
  filter(!str_detect(SampID, "-")) %>%
  unique() %>%
  rename("poc" = "POC_uM",
         "pn" = "PN_uM",
         "sss" = sal,
         "sst" = temp,
         "chla" = Chl_fluor) %>%
  mutate("N_N" = NO3 + NO2) %>%
  rename(Part.SampID = SampID) %>%
  left_join(., d1.samp.IDs)



###Organize all enviro dat
all.env.dat <- g.samp.enviro.dat %>%
  select(Part.SampID, Diss.SampID, sst, sss, chla_interp, pc_interp, pn_interp, N_N_interp) %>%
  rename("chla" = chla_interp,
         "poc" = pc_interp,
         "pn" = pn_interp,
         "N_N" = N_N_interp) %>%
  rbind(., d1.env.dat %>% select(Part.SampID, Diss.SampID, sst, sss, chla, poc, pn, N_N)) 



##Make Final Dataset with all environmental data, all metabolite data, and all metadata:
final.dat.env.meta.metab <- read_csv(all.data.file) %>%
  left_join(., all.env.dat) %>%
  filter(!Part.SampID == "221006_Smp_S4")

write_csv(final.dat.env.meta.metab, file = "Intermediates/Final_Osmo_Meta_Env_Dataframe.csv")






# 
# 
# 
# 
# ##Pull in environmental metadata 
# g.env.dat <- read_csv("Intermediates/Gradients_Matched_Environmental_Metadata.csv") 
# d.env.dat <- read_csv("Intermediates/RC078_MetaData_Compiled.csv") %>%
#   filter(!is.na(depth_m),
#          !is.na(station)) %>%
#   # select(sample_id, parent_id, station, depth_m) %>%
#   rename("SampID" = sample_id) %>%
#   mutate(SampID = paste("221006_Smp_", SampID, sep = ""),
#          Cruise = "RC078") %>%
#   #  select(SampID, Cruise, station, depth_m, Cruise) %>%
#   filter(!str_detect(SampID, "-")) %>%
#   unique() %>%
#   rename("poc" = "POC_uM",
#          "pn" = "PN_uM",
#          "sss" = sal,
#          "sst" = temp,
#          "chla" = Chl_fluor)
# 
# ###Organize all enviro dat
# all.env.dat <- g.env.dat %>%
#   select(SampID, sst, sss, chla_interp, pc_interp, pn_interp) %>%
#   rename("chla" = chla_interp,
#          "poc" = pc_interp,
#          "pn" = pn_interp) %>%
#   rbind(., d.env.dat %>% select(SampID, sst, sss, chla, poc, pn)) 
# 
# 
# 
# ###Combine enviro and surface data:
# osmo.enviro.dat <- left_join(dat.surface, all.env.dat) 
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
# 
# 
# ###Make Gradients Transect Plots:
# 
# #g3+g4 transect data:
# gradients.dat <- full_join(g3.dat, g4.dat) %>%
#   mutate(pc.pn.ratio = pc/pn,
#          chla.pc.ratio = chla/pc_interp)
# 
# ggplot(gradients.dat, aes(x = lat, y = pc_interp, fill = lon)) +
#  # geom_line(aes(group = time)) +
#   geom_point(shape = 21, size = 2) +
#   geom_point(aes(x = lat, y = pc), shape = 21, size = 2, color = "red") +
#   scale_fill_viridis()
# 
# ggplot(gradients.dat, aes(x = lat, y = pc.pn.ratio)) +
#   geom_point()
# 
# 
# ggplot(gradients.dat, aes(x = lat, y = sss, fill = lon)) +
#   geom_point(shape = 21, size = 2) +
#   scale_fill_viridis()
# 
# 
# ggplot(gradients.dat, aes(x = lat, y = sst, fill = lon)) +
#   geom_point(shape = 21, size = 2) +
#   scale_fill_viridis()
# 
# ggplot(gradients.dat, aes(x = lat, y = chla.pc.ratio, fill = lon)) +
#   geom_point(shape = 21, size = 2) +
#   scale_fill_viridis()
# 
# 
