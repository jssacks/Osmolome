
library(lubridate)



#define inputs:
g3.enviro.file <- "Intermediates/G3_metadata_with_interpolations.csv"
g4.enviro.file <- "Intermediates/G4_MetaData_Interpolated.csv"
meta.file <- "Intermediates/All_metadata_information.csv"



#Compile metadata and match with environmental information:



#work on just G4 to start
meta.dat.g4 <- read_csv(meta.file) %>%
  filter(Cruise == "TN397") %>%
  unite(c("Local_Date", "Local_Time"), col = "Local_DT", remove = FALSE, sep = " ") %>%
  mutate(Local_DT_obj = dmy_hms(Local_DT)) %>%
  mutate(time.round = round_date(Local_DT_obj, unit = "hour"))

g4.enviro.dat <- read_csv(g4.enviro.file) %>%
  rename(time.round = time) 

ggplot(g4.enviro.dat, aes(x = lat, y = N_N)) +
  geom_point() +
  scale_y_log10()

g4.meta.dat <- left_join(meta.dat.g4, g4.enviro.dat) %>%
  mutate(c_n_ratio = pc_interp/pn_interp)

ggplot(g4.meta.dat, aes(x = Lat, y = c_n_ratio, color = Long)) +
  geom_jitter()


#Try G3
meta.dat.g3 <- read_csv(meta.file) %>%
  filter(Cruise == "KM1906") %>%
  unite(c("Local_Date", "Local_Time"), col = "Local_DT", remove = FALSE, sep = " ") %>%
  mutate(Local_DT_obj = mdy_hms(Local_DT)) %>%
  mutate(time.round = round_date(Local_DT_obj, unit = "hour"))

g3.enviro.dat <- read_csv(g3.enviro.file) %>%
  rename(time.round = time) %>%
  mutate(c_n_ratio = pc/pn)

ggplot(g3.enviro.dat, aes(x = lat, y = c_n_ratio)) +
  geom_jitter()

g3.meta.dat <- left_join(meta.dat.g3, g3.enviro.dat)
