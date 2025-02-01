





library(readr)
library(tidyverse)



####Source Functions
source("R_Code/Functions.R")

#Define Inputs:
part.file <- "Intermediates/culture_osmo_data_raw.csv"

Stds.info.file <- "Meta_Data/Ingalls_Lab_Standards_03172023.csv" 


#Load in data
part.dat <- read_csv(part.file) %>%
  rename("SampID" = Rep,
         "Name" = Compound) %>%
  filter(str_detect(.$SampID, "Std")) %>%
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
stds.dat.info <- left_join(stds.dat, stds.info) 

##Calculate RFs
RF.dat <- stds.dat.info %>%
  filter(Mix == HILIC_Mix) %>%
  select(-Mix, -HILIC_Mix) %>%
  filter(!str_detect(.$SampID, "Matrix")) %>%
  mutate(RF = as.numeric(Area)/Concentration_uM, NA) %>%
  group_by(Name, Batch) %>%
  summarise(RFmax = max(RF),
            RFmin = min(RF),
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
  group_by(Name, RunNumber, Batch) %>%
  summarize("Std_in_matrix" = Std_in_matrix,
            "Matrix_in_h2o" = Matrix_in_h2O,
            "Std_in_h2o" = Std_in_h2O) %>%
  mutate(RFratio = (Std_in_matrix - Matrix_in_h2o)/Std_in_h2o) %>%
  group_by(Name, Batch) %>%
  summarise(RFratio = mean(RFratio, na.rm = TRUE)) %>%
  ungroup()



## Calculate mean RF ratios across batches and replace outlier or 
#  negative RFratio values with consensus mean RFratio values or for glutamic acid use 1

#Ok values to take concensus of... 
RFratios.removeissues <- RF.ratios %>%
  filter(!RFratio == Inf,
         !RFratio > 2,
         !RFratio < 0.3) 

#Problems to remove...
RFratio.issues <- RF.ratios %>%
  filter(RFratio == Inf |
           RFratio > 2 |
           RFratio < 0.3) %>%
  mutate(remove = "Yes") %>%
  select(Name, Batch, remove)

#Determine consensus values for all compounds with standards
RFratio.consensus <- RFratios.removeissues %>%
  group_by(Name) %>%
  reframe(mean.RFratio = mean(RFratio, na.rm = TRUE))# %>%
#mutate(mean.RFratio = case_when(Name == "L-Glutamic acid" ~ 1.45,
#                                TRUE ~ mean.RFratio))

##Create data frame of values to replace problematic ones:
RFratio.replace <-RFratio.issues %>%
  left_join(RFratio.consensus) %>%
  select(-remove) %>%
  rename(RFratio = mean.RFratio)

#remove problematic values and add in consensus values 
RFratio.final.values <- RF.ratios %>%
  left_join(RFratio.issues) %>%
  filter(is.na(remove)) %>%
  select(-remove) %>%
  rbind(RFratio.replace) 


###Join it all together
RF.RFratios <- left_join(RF.dat, RFratio.final.values)


##Calculate predicted RFratios for Homarine, Homoserine betaine, and Threnonine betaine as average values for all betaines
Betaine.RF.RFratios <- RF.RFratios %>%
  filter(Name %in% c("(3-Carboxypropyl)trimethylammonium", "Betonicine", "Glycine betaine",
                     "Proline betaine", "Trigonelline", "beta-Alaninebetaine", "Carnitine")) %>%
  group_by(Batch) %>%
  summarize(RFmax = mean(RFmax),
            RFmin = mean(RFmin),
            RF = mean(RF),
            RFratio = mean(RFratio)) %>%
  cross_join(.,  tibble(Name = c("Homoserine Betaine (tentative)", "Threonine Betaine (tentative)", "Homarine")))


##Add in RFs and RFratios for Homoserine betaine and Threnonine betaine
RF.RFratios.final <- rbind(RF.RFratios %>% filter(!Name == "Homarine"), Betaine.RF.RFratios)



###Join it all together
write_csv(RF.RFratios.final, file = "Intermediates/Culture_Stds_RFs_RFratios.csv")































































