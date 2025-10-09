


library(tidyverse)
library(patchwork)
library(viridis)


dat.g4.atp <- read_csv("Collaborator_Data/ATP_all/G4_ATP_check.csv")

source("R_Code/Code_Development_Workspace/Figure_Palettes.R")


###define inputs
all.dat.file <- "Intermediates/Final_Osmo_Meta_Env_Dataframe.csv"
atp.g3.g4.file <- "Collaborator_Data/ATP_all/ATP_G3_G4.csv"
















#Read in data
dat <- read_csv(all.dat.file) %>%
  filter(!Part.SampID == "220902_Smp_TN397_S11_600_U_C")



#Convert to local time
lt.atp.dat <- dat.g4.atp %>%
  mutate(UTC_dt = ymd_hms(time)) %>%
  mutate(Local_dt = UTC_dt - hours(9)) %>%
  filter(!is.na(PATP)) %>%
  filter(depth < 10) %>%
  mutate(sample_hour_local = hour(Local_dt))


ggplot(lt.atp.dat, aes(x = lat, y = PATP, color = sample_hour_local)) +
  geom_point(size = 3) +
  scale_color_viridis()














#Example code:
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


















































