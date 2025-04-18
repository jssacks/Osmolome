



library(tidyverse)
library(patchwork)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")



##Define inputs:
diss.file <- "Intermediates/Dissolved_Final_Quant_QCed.csv"
part.file <- "Intermediates/Particualte_Final_Quant_QCed.csv"
meta.file <- "Intermediates/All_metadata_information.csv"
g.env.file <- "Intermediates/combined_gradients_enviromental_data.csv"

###load in data and combine with metadata
diss.dat <- read_csv(diss.file)
part.dat <- read_csv(part.file)
meta.dat <- read_csv(meta.file)


##Match part and diss samples:
diss.samp.info <- diss.dat %>%
  mutate(Diss.SampID = Rep) %>%
  select(SampID, Cruise, Diss.SampID, replicate) %>%
  unique() %>%
  filter(!str_detect(SampID, "Blk"),
         !str_detect(SampID, "blk"),
         !str_detect(SampID, "Poo"),
         !str_detect(SampID, "Mix"),
         !str_detect(SampID, "UKG")) 



#### Gather particulate Data:
part.samp.info <- left_join(part.dat, meta.dat) %>%
  filter(depth_m < 16) %>%
  mutate(Part.SampID = Rep) %>%
  select(SampID, Cruise, Part.SampID, replicate) %>%
  unique() %>%
  filter(!Part.SampID == "220902_Smp_TN397_S11_600_U_C") 


###Match up G3 Samples:
g3.p <- part.samp.info %>%
  filter(Cruise %in% c("KM1906", "G3_DepthProfiles")) %>%
  mutate(SampID.shared= str_replace(SampID, "KM1906U12", "MU12"),
         SampID.shared = str_replace(SampID.shared, "KM1906U13", "MU13"),
         SampID.shared = str_replace(SampID.shared, "220628_Smp_", ""),
         SampID.shared = str_replace(SampID.shared, "190715_Smp_", "")) %>%
  filter(!str_detect(SampID.shared, "D")) %>%
 # rename(sampID.part = SampID) %>%
  select(-Cruise, -SampID)

g3.d <- diss.samp.info %>%
  filter(Cruise %in% c("KM1906")) %>%
  mutate(SampID.shared = str_replace(SampID, "220623_Smp_", "")) %>%
 # rename(sampID.diss = SampID) %>%
  select(-Cruise, -SampID)

g3.match <- full_join(g3.p, g3.d)  
  



###Match up G4 Samples:
g4.p <- part.samp.info %>%
  filter(Cruise %in% c("TN397")) %>%
  mutate(SampID.shared = str_replace(SampID, "220902_Smp_TN397_", "")) %>%
#  rename(sampID.part = SampID) %>%
  select(-Cruise, -SampID)

g4.d <- diss.samp.info %>%
  filter(Cruise %in% c("TN397")) %>%
  filter(!str_detect(SampID, "KM1906")) %>%
  mutate(SampID.shared = str_replace(SampID, "220602_Smp_TN397_", "")) %>%
 # rename(sampID.diss = SampID) %>%
  select(-Cruise, -SampID)

g4.match <- full_join(g4.p, g4.d)  





###Match up D1 Samples:
d1.p <- part.samp.info %>%
  filter(Cruise %in% c("RC078")) %>%
  mutate(SampID.shared = str_replace(SampID, "221006_Smp_", "")) %>%
#  rename(sampID.part = SampID) %>%
  select(-Cruise, -SampID)

d1.d <- diss.samp.info %>%
  filter(Cruise %in% c("RC078")) %>%
  filter(!str_detect(SampID, "RC104"),
         !str_detect(SampID, "CXBLK")) %>%
  mutate(SampID.shared = str_replace(SampID, "240415_Smp_", ""),
         SampID.shared = str_replace(SampID.shared, "240102_Smp_RC078_", "")) %>%
#  rename(sampID.diss = SampID) %>%
  select(-Cruise, -SampID)

d1.match <- full_join(d1.p, d1.d)  




### combine all matching files
all.match <- rbind(g3.match, g4.match, d1.match) 

#export:
write_csv(all.match, file = "Intermediates/Particulate_Dissolved_Matching_Key.csv")








































