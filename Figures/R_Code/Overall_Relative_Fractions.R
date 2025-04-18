




library(tidyverse)
library(patchwork)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")


###define inputs
all.dat.file <- "Intermediates/Final_Osmo_Meta_Env_Dataframe.csv"



#Read in data
dat <- read_csv(all.dat.file) %>%
  filter(!Part.SampID == "220902_Smp_TN397_S11_600_U_C") %>%
  filter(!station %in% c(1, 8)) %>%
  filter(depth_m < 10) %>%
  filter(Cruise %in% c("TN397", "KM1906", "RC078")) %>%
  filter(is.na(Tot.Flag))


###Calculat relative fractions:
part.dat.class.sum <- dat %>%
  filter(!is.na(Part.SampID)) %>%
  group_by(Part.SampID, total)