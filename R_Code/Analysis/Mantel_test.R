




library(tidyverse)
library(patchwork)
library(vegan)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")


###define inputs
all.dat.file <- "Intermediates/Final_Osmo_Meta_Env_Dataframe.csv"



#Read in data
dat <- read_csv(all.dat.file) %>%
  filter(!Part.SampID == "220902_Smp_TN397_S11_600_U_C") %>%
  filter(!str_detect(Part.SampID, "_S4_C")) %>%
  filter(!station %in% c(1, 8)) %>%
  filter(depth_m < 10) %>%
  filter(Cruise %in% c("TN397", "KM1906", "RC078")) 



#particulate
dat.p <- dat %>%
  filter(!is.na(Diss.SampID)) %>%
  filter(!is.na(Part.SampID)) %>%
  filter(!class %in% c("Sugar", "Sulfonate")) %>%
  mutate(log10.nM.p = log10(Part.Conc.nM)) %>%
  filter(!Compound %in% c("Arsenobetaine", "L-Isoleucine", "beta-Alanine")) %>%
  select(Part.SampID, Compound, log10.nM.p) %>%
  unique() %>%
  pivot_wider(id_cols = Part.SampID, names_from = Compound, values_from = log10.nM.p) %>%
  column_to_rownames(var = "Part.SampID")


#dissolved
dat.d <- dat %>%
  filter(!is.na(Diss.SampID)) %>%
  filter(!is.na(Part.SampID)) %>%
  filter(!class %in% c("Sugar", "Sulfonate")) %>%
  mutate(log10.nM.d = log10(Diss.Conc.nM.adj)) %>%
  filter(!Compound %in% c("Arsenobetaine", "L-Isoleucine", "beta-Alanine")) %>%
  select(Part.SampID, Compound, log10.nM.d) %>%
  unique() %>%
  pivot_wider(id_cols = Part.SampID, names_from = Compound, values_from = log10.nM.d) %>%
  column_to_rownames(var = "Part.SampID")


# #dissolved
# o.dat.d <- osmo.dat.mantel %>%
#   mutate(log10.nM.d = log10(nM.in.smp.diss)) %>%
#   filter(!Compound %in% c("Arsenobetaine", "L-Isoleucine")) %>%
#   select(SampID.shared, Compound, log10.nM.d) %>%
#   pivot_wider(id_cols = SampID.shared, names_from = Compound, values_from = log10.nM.d) %>%
#   column_to_rownames(var = "SampID.shared")



#Standardize data
p.stand<- decostand(dat.p, method = "range", MARGIN = 2)
d.stand <- decostand(dat.d, method = "range", MARGIN = 2) 


#convert to distance matrices 
p.dist <- vegdist(p.stand, method = "euclidean")
d.dist <- vegdist(d.stand, method = "euclidean")


#attempt Mantel Test:
m.test <- mantel(p.dist, d.dist)

m.test



















































