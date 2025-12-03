



library(patchwork)
library(tidyverse)
library(viridis)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")

#load in data:
osmo.file <- "Intermediates/G2SF_Quant_Output.csv"
meta.file <- "Meta_Data/G2_SF_Sample_MetaData.csv"






# Make G2SF Heatmaps ------------------------------------------------------



###load in data
g2.dat <- read_csv(osmo.file)
meta.dat <- read_csv(meta.file)



###
sf.dat <- g2.dat %>%
  left_join(., meta.dat)  %>%
  rename(Compound = Name) %>%
  group_by(Compound, Station, Lat, Size_Fraction) %>%
  reframe(Mean.nM = mean(nM.in.smp),
          Mean.nM.C = mean(nM_C),
          Mean.nM.N = mean(nM_N),
          Mean.nM.S = mean(nM_S)) %>%
  group_by(Compound, Station, Lat) %>%
  # mutate(Mean.Area = case_when(Size_Fraction == "F" & Mean.Area > Mean.Area[Size_Fraction == "S"] ~ Mean.Area-Mean.Area[Size_Fraction == "S"],
  #                             Size_Fraction == "F" & Mean.Area < Mean.Area[Size_Fraction == "S"] ~ 1,
  #                             TRUE ~ Mean.Area)) %>%
  mutate(Size_Fraction = str_replace(Size_Fraction, "F", "L")) %>%
  mutate(Small_Frac_Perc = Mean.nM[Size_Fraction == "S"]/(Mean.nM[Size_Fraction == "S"] + Mean.nM[Size_Fraction == "L"])) %>%
  ungroup() %>%
  filter(!Compound %in% c("Trimethylamine N-oxide", "L-Cysteic acid", "L-Lysine", "L-Tyrosine", "L-Serine", "L-Methionine", "L-Arginine")) %>%
  filter(!str_detect(Compound, "tentative")) %>%
  left_join(., compound.order)



ggplot(sf.dat, aes(x = as.factor(Lat), y = Mean.nM, fill = Size_Fraction)) + 
  geom_col(position = "fill") +
  facet_wrap(.~compound.name.figure)

ggplot(sf.dat, aes(x = as.factor(Lat), y = Mean.nM, fill = Size_Fraction)) + 
  geom_col() +
  facet_wrap(.~compound.name.figure, scales = "free")
