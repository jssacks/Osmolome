



library(readr)
library(tidyverse)



####Source Functions
source("R_Code/Functions.R")

#Define Inputs:
part.file <- "Intermediates/g2_osmo_data_raw.csv"
Stds.info.file <- "Meta_Data/Ingalls_Lab_Standards_03172023.csv" 
part.RF.RFratio.file <- "Intermediates/Particulate_Stds_RFs_RFratios.csv"


#Load in data, remove incorrect additional std additions for KM1906 samples
part.dat <- read_csv(part.file) %>%
  rename("SampID" = Rep,
         "Name" = Compound)# %>%
  filter(str_detect(.$SampID, "Std")) %>%
  filter(!str_detect(.$SampID, "220713")) %>%
  mutate(Area = replace_na(Area, 0))



####Load in standards
stds.dat <- part.dat %>%
  mutate(Mix = str_extract(SampID, "Mix\\d")) %>%
  select(-Mix)

###Get Mix and Concentration info:
stds.info <- read_csv(Stds.info.file) %>%
  filter(Priority == TRUE) %>%
  select(Compound_Name, z, Column, Concentration_uM) %>%
  rename("Name" = Compound_Name) 

###Join stuff together + remove Matrix Samples
stds.dat.info <- left_join(stds.dat, stds.info) #%>%
  # mutate(HILIC_Mix = case_when(Name == "Betonicine" & Cruise == "G4_DepthProfiles" ~ "Mix1",
  #                              #  Name == "Betonicine" & Cruise == "G4_DepthProfiles" ~ "Mix1",
  #                              Name == "Glycine betaine" & Cruise == "G4_DepthProfiles" ~ "Mix2",
  #                              # Name == "Glycine betaine" & Cruise == "G4_DepthProfiles" ~ "Mix2",
  #                              Name == "Trimethylamine N-oxide" & Cruise == "G4_DepthProfiles" ~ "Mix2",
  #                              # Name == "Trimethylamine N-oxide" & Cruise == "G4_DepthProfiles" ~ "Mix2",
  #                              Name == "Dimethylsulfonioacetate" & Cruise == "G4_DepthProfiles" ~ "Mix1",
  #                              # Name == "Dimethylsulfonioacetate" & Cruise == "G4_DepthProfiles" ~ "Mix1",
  #                              Name == "Gonyol" & Cruise == "G4_DepthProfiles" ~ "Mix1",
  #                              # Name == "Gonyol" & Cruise == "G4_DepthProfiles" ~ "Mix1",
  #                              Name == "5-Hydroxyectoine" & Cruise == "G3_DepthProfiles" ~ "Mix1",
  #                              Name == "Hydroxyisoleucine" & Cruise == "G4_DepthProfiles" ~ "Mix1",
  #                              Name == "Arsenobetaine" & Cruise == "G4_DepthProfiles" ~ "Mix1",
  #                              TRUE ~ HILIC_Mix))




##Calculate RFs
RF.dat <- stds.dat.info %>%
 # filter(Mix == HILIC_Mix) %>%
 # select(-Mix, -HILIC_Mix) %>%
  filter(!str_detect(.$SampID, "Matrix")) %>%
  mutate(RF = as.numeric(Area)/Concentration_uM) %>%
  group_by(Name) %>%
  summarise(RFmax = max(RF, na.rm = TRUE),
            RFmin = min(RF, na.rm = TRUE),
            RF = mean(RF, na.rm = TRUE))  %>%
  ungroup()

RFratio.dat <- stds.dat.info# %>%
#  filter(HILIC_Mix == Mix | is.na(Mix)) %>% 
  mutate(RunNumber = str_extract(SampID, "_\\d$")) %>%
  mutate(RunType = ifelse(str_detect(SampID, "StdsMix\\dInH2O")|
                            str_detect(SampID, "StdsInH2O"), "Std_in_h2O", 
                          ifelse(str_detect(SampID, "StdsMix\\dInMatrix") |
                                   str_detect(SampID, "StdsInMatrix"), "Std_in_matrix",
                                 "Matrix_in_h2O"))) %>%
#  filter(HILIC_Mix == Mix | is.na(Mix)) %>% 
  select(-SampID, -Concentration_uM) %>%
  spread(key = RunType, value = Area ) 

RF.ratios <- RFratio.dat %>%
  ungroup() %>%
  group_by(Name, RunNumber) %>%
  summarize("Std_in_matrix" = Std_in_matrix,
            "Matrix_in_h2o" = Matrix_in_h2O,
            "Std_in_h2o" = Std_in_h2O) %>%
  mutate(diff_matrices = Std_in_matrix - Matrix_in_h2o,
         diff_h2o_matrix = Std_in_h2o - Std_in_matrix) %>%
  mutate(RFratio = (Std_in_matrix - Matrix_in_h2o)/ Std_in_h2o) %>%
  group_by(Name) %>%
  summarise(RFratio = mean(RFratio)) %>%
  ungroup()

























































