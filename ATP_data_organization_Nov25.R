




library(tidyverse)
library(lubridate)



##Define inputs:

#ATP data:
atp.time.file <- "Collaborator_Data/ATP_all/G3G4_ATP_TimeCollection.csv"
atp.measurements.file <- "Collaborator_Data/ATP_all/ATP_G3_G4.csv"

#environmental data:
# g3.file <- "Intermediates/G3_metadata_with_interpolations.csv"
# g4.file <- "Intermediates/G4_metadata_with_interpolations.csv"




#Load in datasets:
atp.time.dat.raw <- read_csv(atp.time.file)
atp.measurements.dat <- read_csv(atp.measurements.file) %>%
  rename(Station = `Station...2`,
         POC_Paired = PC) %>%
  select(Cruise, Station, Lat, Long, Depth, POC_Paired, PATP_ng_L, SD_PATP_ng_L, ATP_Flag)

# g3.dat <- read_csv(g3.file)
# g4.dat <- read_csv(g4.file)

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
  full_join(., atp.measurements.dat) %>%
  filter(is.na(ATP_Flag)) %>%
  filter(!Station == "UW 8") 


##Export
write_csv(atp.time.dat, file = "Intermediates/ATP_G3_G4_data.csv")
