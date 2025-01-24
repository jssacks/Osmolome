




library(readr)
library(tidyverse)



####Source Functions
source("R_Code/Functions.R")

#Define Inputs:
part.file <- "Intermediates/particulate_osmo_data_raw.csv"

Stds.info.file <- "Meta_Data/Ingalls_Lab_Data/Ingalls_Lab_Standards_03172023.csv" 


#Load in data, remove incorrect additional std additions for KM1906 samples
part.dat <- read_csv(part.file) %>%
  rename("SampID" = Rep,
         "Name" = Compound) %>%
  filter(str_detect(.$SampID, "Std")) %>%
  filter(!str_detect(.$SampID, "220713")) %>%
  mutate(Area = replace_na(Area, 0))

  

####Load in standards
stds.dat <- part.dat %>%
  mutate(Mix = str_extract(SampID, "Mix\\d")) 

###Get Mix and Concentration info:
stds.info <- read_csv(Stds.info.file) %>%
  filter(Priority == TRUE) %>%
  select(Compound_Name, z, Column, HILIC_Mix, Concentration_uM) %>%
  rename("Name" = Compound_Name) 

###Join stuff together + remove Matrix Samples
stds.dat.info <- left_join(stds.dat, stds.info) %>%
  mutate(HILIC_Mix = case_when(Name == "Betonicine" & Cruise == "G4_DepthProfiles" ~ "Mix1",
                             #  Name == "Betonicine" & Cruise == "G4_DepthProfiles" ~ "Mix1",
                               Name == "Glycine betaine" & Cruise == "G4_DepthProfiles" ~ "Mix2",
                              # Name == "Glycine betaine" & Cruise == "G4_DepthProfiles" ~ "Mix2",
                               Name == "Trimethylamine N-oxide" & Cruise == "G4_DepthProfiles" ~ "Mix2",
                              # Name == "Trimethylamine N-oxide" & Cruise == "G4_DepthProfiles" ~ "Mix2",
                               Name == "Dimethylsulfonioacetate" & Cruise == "G4_DepthProfiles" ~ "Mix1",
                              # Name == "Dimethylsulfonioacetate" & Cruise == "G4_DepthProfiles" ~ "Mix1",
                               Name == "Gonyol" & Cruise == "G4_DepthProfiles" ~ "Mix1",
                              # Name == "Gonyol" & Cruise == "G4_DepthProfiles" ~ "Mix1",
                               TRUE ~ HILIC_Mix))

##Calculate RFs
RF.dat <- stds.dat.info %>%
  filter(Mix == HILIC_Mix) %>%
  select(-Mix, -HILIC_Mix) %>%
  filter(!str_detect(.$SampID, "Matrix")) %>%
  mutate(RF = as.numeric(Area)/Concentration_uM) %>%
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
  spread(key = RunType, value = Area ) 

RF.ratios <- RFratio.dat %>%
  ungroup() %>%
  group_by(Name, RunNumber, Cruise) %>%
  summarize("Std_in_matrix" = Std_in_matrix,
            "Matrix_in_h2o" = Matrix_in_h2O,
            "Std_in_h2o" = Std_in_h2O) %>%
  mutate(diff_matrices = Std_in_matrix - Matrix_in_h2o,
           diff_h2o_matrix = Std_in_h2o - Std_in_matrix) %>%
  mutate(RFratio = (Std_in_matrix - Matrix_in_h2o)/ Std_in_h2o) %>%
  group_by(Name, Cruise) %>%
  summarise(RFratio = mean(RFratio)) %>%
  ungroup()



## Calculate mean RF ratios across batches and replace outlier or 
#  negative RFratio values with consensus mean RFratio values or for glutamic acid use 1

#Ok values to take concensus of... 
RFratios.removeissues <- RF.ratios %>%
  filter(!RFratio == Inf,
         !RFratio > 3,
         !RFratio < 0.2) 

#Problems to remove...
RFratio.issues <- RF.ratios %>%
  filter(RFratio == Inf |
         RFratio > 3 |
         RFratio < 0.2) %>%
  mutate(remove = "Yes") %>%
  select(Name, Cruise, remove)

#Determine consensus values for all compounds with standards
RFratio.consensus <- RFratios.removeissues %>%
  group_by(Name) %>%
  reframe(mean.RFratio = mean(RFratio, na.rm = TRUE)) %>%
  mutate(mean.RFratio = case_when(Name == "L-Glutamic acid" ~ 1.45,
                                  TRUE ~ mean.RFratio))

##Create data frame of values to replace problematic ones:
RFratio.replace <-RFratio.issues %>%
  left_join(RFratio.consensus) %>%
  select(-remove) %>%
  rename(RFratio = mean.RFratio)

#remove problematic values and add in consensus ones
RFratio.final.values <- RF.ratios %>%
  left_join(RFratio.issues) %>%
  filter(is.na(remove)) %>%
  select(-remove) %>%
  rbind(RFratio.replace) %>%
  mutate(RFratio = case_when(Name == "Gonyol" ~ 1.02,
                             TRUE ~ RFratio))
    
  
###Join it all together
RF.RFratios <- left_join(RF.dat, RFratio.final.values)


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

###Export final values
write_csv(RF.RFratios.final, file = "Intermediates/Particulate_Stds_RFs_RFratios.csv")

ggplot(RF.RFratios, aes(y = Name, x = RF)) +
  
  geom_point(aes(y = Name, x = RFmax)) + 
  geom_point(aes(y = Name, x = RFmin)) + 
  geom_point(size = 3, color = "blue") +
  facet_wrap(.~Cruise, scales = "free_x") +
  scale_x_log10()
