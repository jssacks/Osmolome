


library(tidyverse)




#sample lists
meta.file <- "Intermediates/All_metadata_information.csv"



#environmental data
g3.file <- "Intermediates/G3_metadata_with_interpolations.csv"
g4.file <- "Intermediates/G4_metadata_with_interpolations.csv"
d1.file <- "Intermediates/RC078_MetaData_Compiled.csv"



#read in datasets:
g3.dat <- read_csv(g3.file)
g4.dat <- read_csv(g4.file)
d1.dat <- read_csv(d1.file)


###read in sample meta data
meta.dat <- read_csv(meta.file)


#Attempt to match environmental data with samples based on time


#Gradients 4
g4.samp.dat <- meta.dat %>%
  filter(Cruise == "TN397") %>%
  unite(c("Local_Date", "Local_Time"), col = "Local_DT", remove = FALSE, sep = " ") %>%
  mutate(Local_DT_obj = dmy_hms(Local_DT)) %>%
  mutate(time.diff.val = case_when(Local_DT_obj < ymd_hms("2021-11-22 24:00:00") ~ 8,
                                   Local_DT_obj > ymd_hms("2021-11-22 24:00:00") & Local_DT_obj < ymd_hms("2021-12-10 24:00:00") ~ 9,
                                   TRUE ~ 10)) %>%
  mutate(time.round = round_date(Local_DT_obj, unit = "hour"),
         UTC.time.round = time.round + hours(time.diff.val)) %>%
  select(SampID, Cruise, Lat, Long, Local_Date, Local_Time, depth_m, UTC.time.round)


#Gradients 3
g3.samp.dat <- meta.dat %>%
  filter(Cruise == "KM1906") %>%
  unite(c("Local_Date", "Local_Time"), col = "Local_DT", remove = FALSE, sep = " ") %>%
  mutate(Local_DT_obj = mdy_hms(Local_DT)) %>%
  mutate(time.round = round_date(Local_DT_obj, unit = "hour"),
         UTC.time.round = time.round + hours(10)) %>%
  select(SampID, Cruise, Lat, Long, Local_Date, Local_Time, depth_m, UTC.time.round)


##all gradients samples for matching:
g.samp.dat <- rbind(g4.samp.dat, g3.samp.dat)

#gradients environmental data
gradients.dat <- full_join(g3.dat, g4.dat) %>%
  rename("UTC.time.round" = time)


##Match metadata and sample data:
g.samp.enviro.dat <- left_join(g.samp.dat, gradients.dat)


###write gradients environmental data to csv
write_csv(gradients.dat, file = "Intermediates/combined_gradients_enviromental_data.csv")

##write gradients combined sample data to csv
write_csv(g.samp.enviro.dat, file = "Intermediates/Gradients_Matched_Environmental_Metadata.csv")



















###Make Gradients Transect Plots:

#g3+g4 transect data:
gradients.dat <- full_join(g3.dat, g4.dat) %>%
  mutate(pc.pn.ratio = pc/pn,
         chla.pc.ratio = chla/pc_interp)

ggplot(gradients.dat, aes(x = lat, y = pc_interp, fill = lon)) +
 # geom_line(aes(group = time)) +
  geom_point(shape = 21, size = 2) +
  geom_point(aes(x = lat, y = pc), shape = 21, size = 2, color = "red") +
  scale_fill_viridis()

ggplot(gradients.dat, aes(x = lat, y = pc.pn.ratio)) +
  geom_point()


ggplot(gradients.dat, aes(x = lat, y = sss, fill = lon)) +
  geom_point(shape = 21, size = 2) +
  scale_fill_viridis()


ggplot(gradients.dat, aes(x = lat, y = sst, fill = lon)) +
  geom_point(shape = 21, size = 2) +
  scale_fill_viridis()

ggplot(gradients.dat, aes(x = lat, y = chla.pc.ratio, fill = lon)) +
  geom_point(shape = 21, size = 2) +
  scale_fill_viridis()


