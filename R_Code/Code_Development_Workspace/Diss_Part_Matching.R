



library(tidyverse)
library(patchwork)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")



##Define inputs:
diss.file <- "Intermediates/Dissolved_Quantified_Data.csv"
part.file <- "Intermediates/Particulate_Quant_Output.csv"
meta.file <- "Intermediates/All_metadata_information.csv"
g.env.file <- "Intermediates/combined_gradients_enviromental_data.csv"

###load in data and combine with metadata
diss.dat <- read_csv(diss.file)
part.dat <- read_csv(part.file)
meta.dat <- read_csv(meta.file)


##Match part and diss samples:
diss.samp.info <- diss.dat %>%
  select(SampID, Cruise) %>%
  unique() %>%
  filter(!str_detect(SampID, "Blk"),
         !str_detect(SampID, "blk"),
         !str_detect(SampID, "Poo"),
         !str_detect(SampID, "Mix"),
         !str_detect(SampID, "UKH"),
         !str_detect(SampID, "UKG")) 







#### Gather Surface Data:
dat.surface <- left_join(part.dat, meta.dat) %>%
  rename("Compound" = Name) %>%
  filter(Compound %in% compound.order$Compound) %>%
  left_join(., compound.order) %>%
  # filter(!station == 4) %>%
  filter(!SampID == "220902_Smp_TN397_S11_600_U_C") %>%
  unique() %>%
  filter(Cruise %in% c( "KM1906", "TN397", "RC078")) %>%
  filter(!Compound == "Threonine Betaine (tentative)") %>%
  filter(!Compound == "Homoserine Betaine (tentative)") %>%
  filter(depth_m < 10) %>%
  mutate(Region = case_when(Cruise == "RC078" ~ "PS",
                            Lat > 36 ~ "NPTZ",
                            Lat < 7 ~ "Equator",
                            Cruise == "TN397" & Lat > 29 ~ "CC",
                            TRUE ~ "NPSG")) %>%
  filter(!station %in% c(1, 8))
















































