



library(tidyverse)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")



#define inputs:
cult.file <- "Intermediates/Culture_Quant_Output.csv"
meta.file <- "Intermediates/All_culture_metadata.csv"



###load in data:
cult.dat <- read_csv(cult.file)
meta.dat <- read_csv(meta.file) 


###combine data with meta data
cult.meta.dat <- left_join(cult.dat, meta.dat) %>%
  filter(!str_detect(SampID, "Blk"))


cult.fig <- cult.meta.dat %>%
  rename("Compound" = Name) %>%
  filter(Compound %in% compound.order$Compound) %>%
  left_join(., compound.order) %>%
  filter(!is.na(Type)) %>%
  mutate(SampID.fig = str_remove(SampID, "220111_Smp_"),
         SampID.fig = str_remove(SampID.fig, "211221_Smp_"),
         SampID.fig = str_remove(SampID.fig, "211215_Smp_"),
         SampID.fig = str_remove(SampID.fig, "211218_Smp_"),
         SampID.fig = str_remove(SampID.fig, "220104_Smp_"))




Cult.dat.relabun <- cult.fig %>%
  ungroup() %>%
  group_by(Compound, compound.name.figure, Organism, Type) %>%
  reframe(ave.conc.uM = mean(uM.in.vial.ave)) %>%
  group_by(Organism, Type) %>%
  mutate(Total.Osmo.Conc.uM = sum(ave.conc.uM),
         Rel.Osmo.Conc = (ave.conc.uM/Total.Osmo.Conc.uM)*100)



cult.pres.one.percent <- Cult.dat.relabun %>%
  filter(Rel.Osmo.Conc >= 1) %>%
  mutate(Presence = 1) %>%
  group_by(Compound) %>%
  mutate(num.present = sum(Presence))


sum <- cult.pres.one.percent %>%
  dplyr::select(compound.name.figure, num.present) %>%
  unique()






























