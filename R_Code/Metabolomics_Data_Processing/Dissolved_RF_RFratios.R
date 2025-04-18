


library(readr)
library(tidyverse)



####Source Functions
source("R_Code/Functions.R")

#Define Inputs:
diss.file <- "Intermediates/dissolved_osmo_data_raw.csv"

Stds.info.file <- "Meta_Data/Ingalls_Lab_Standards_03172023.csv" 


#Load in data
diss.dat <- read_csv(diss.file) %>%
  rename("SampID" = Rep,
         "Name" = Compound) %>%
  filter(str_detect(.$SampID, "Std"))


####Load in standards
stds.dat <- diss.dat %>%
  mutate(Mix = str_extract(SampID, "Mix\\d")) 

###Get Mix and Concentration info:
stds.info <- read_csv(Stds.info.file) %>%
  filter(Priority == TRUE) %>%
  select(Compound_Name, z, Column, HILIC_Mix, Concentration_uM) %>%
  rename("Name" = Compound_Name) 

###Join stuff together + remove Matrix Samples + fix mix info for comounds with stds mix changes in 
# samples sets run after April 2023
stds.dat.info <- left_join(stds.dat, stds.info) %>%
  mutate(HILIC_Mix = case_when(Name == "Betonicine" & Cruise == "KinExp" ~ "Mix1",
                               Name == "Betonicine" & Cruise == "RC078" ~ "Mix1",
                               Name == "Glycine betaine" & Cruise == "KinExp" ~ "Mix2",
                               Name == "Glycine betaine" & Cruise == "RC078" ~ "Mix2",
                               Name == "Trimethylamine N-oxide" & Cruise == "KinExp" ~ "Mix2",
                               Name == "Trimethylamine N-oxide" & Cruise == "RC078" ~ "Mix2",
                               Name == "Dimethylsulfonioacetate" & Cruise == "KinExp" ~ "Mix1",
                               Name == "Dimethylsulfonioacetate" & Cruise == "RC078" ~ "Mix1",
                               Name == "Gonyol" & Cruise == "KinExp" ~ "Mix1",
                               Name == "Gonyol" & Cruise == "RC078" ~ "Mix1",
                               Name == "Hydroxyisoleucine" & Cruise == "KinExp" ~ "Mix1",
                               Name == "Hydroxyisoleucine" & Cruise == "RC078" ~ "Mix1",
                               Name == "L-Glutamine" & Cruise == "KinExp" ~ "Mix1",
                               Name == "L-Glutamine" & Cruise == "RC078" ~ "Mix1",
                               Name == "Arsenobetaine" & Cruise == "KinExp" ~ "Mix1",
                               Name == "Arsenobetaine" & Cruise == "RC078" ~ "Mix1",
                               TRUE ~ HILIC_Mix))

##Calculate RFs
RF.dat <- stds.dat.info %>%
  filter(Mix == HILIC_Mix) %>%
  select(-Mix, -HILIC_Mix) %>%
  filter(!str_detect(.$SampID, "Matrix")) %>%
  mutate(RF = as.numeric(Area)/Concentration_uM, NA) %>%
  group_by(Name, Cruise) %>%
  summarise(RFmax = max(RF, na.rm = TRUE),
            RFmin = min(RF, na.rm = TRUE),
            RF = mean(RF, na.rm = TRUE))  %>%
  ungroup()

RFratio.dat <- stds.dat.info %>%
  filter(HILIC_Mix == Mix | is.na(Mix)) %>% 
  mutate(RunNumber = str_extract(SampID, "_\\d$")) %>%
  mutate(RunType = ifelse(str_detect(SampID, "StdsMix\\dInH2O")|
                            str_detect(SampID, "StdsInH2O"), "Std_in_h2O", 
                          ifelse(str_detect(SampID, "StdsMix\\dInMatrix") |
                                   str_detect(SampID, "StdsInMatrix"), "Std_in_matrix",
                                 "Matrix_in_h2O"))) %>%
  filter(HILIC_Mix == Mix | is.na(Mix)) %>% 
  select(-Mix, -HILIC_Mix, -SampID, -Concentration_uM) %>%
  group_by(Cruise) %>%
  spread(key = RunType, value = Area) 

RF.ratios <- RFratio.dat %>%
  ungroup() %>%
  group_by(Name, RunNumber, Cruise) %>%
  summarize("Std_in_matrix" = Std_in_matrix,
            "Matrix_in_h2o" = Matrix_in_h2O,
            "Std_in_h2o" = Std_in_h2O) %>%
  mutate(RFratio = (Std_in_matrix - Matrix_in_h2o)/ Std_in_h2o) %>%
  filter(RFratio > 0.3) %>%
  filter(RFratio < 3) %>%
  group_by(Name, Cruise) %>%
  summarise(RFratio = mean(RFratio, na.rm = TRUE)) %>%
  ungroup()

###Join it all together
RF.RFratios <- left_join(RF.dat, RF.ratios)


##Calculate predicted RFratios for Homoserine betaine and Threnonine betaine as average values for all betaines
Betaine.RF.RFratios <- RF.RFratios %>%
  filter(Name %in% c("(3-Carboxypropyl)trimethylammonium", "Betonicine", "Glycine betaine",
                     "Proline betaine", "Trigonelline", "beta-Alaninebetaine", "Carnitine")) %>%
  group_by(Cruise) %>%
  summarize(RFmax = mean(RFmax),
            RFmin = mean(RFmin),
            RF = mean(RF),
            RFratio = mean(RFratio)) %>%
  cross_join(., tibble(Name = c("Homoserine Betaine (tentative)", "Threonine Betaine (tentative)")))

##Add in RFs and RFratios for Homoserine betaine and Threnonine betaine
RF.RFratios.final <- rbind(RF.RFratios, Betaine.RF.RFratios)


write_csv(RF.RFratios.final, file = "Intermediates/Dissolved_Stds_RFs_RFratios.csv")



