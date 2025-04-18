




library(tidyverse)
library(patchwork)
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


###Determine presence/absence of compounds in samples:

###particulate
part.pa <- dat %>%
  filter(!is.na(Part.SampID)) %>%
 # filter(Compound == "L-Alanine")
  cross_join(., dat %>%
              filter(!is.na(Part.SampID)) %>%
              select(Part.SampID) %>%
              unique() %>%
              reframe(tot.samp.count = n())) %>%
  filter(is.na(Part.Flag)) %>%
  select(Part.SampID, Region, Compound, Part.Conc.nM, tot.samp.count) %>%
  unique() %>%
  group_by(Compound) %>%
  reframe(comp.count = n(),
          tot.samp.count = tot.samp.count,
          Rel.fraction.detected = comp.count/tot.samp.count) %>%
  unique()
  
####dissolved
diss.pa <- dat %>%
  filter(!is.na(Diss.SampID)) %>%
#   filter(Compound == "L-Alanine")
  cross_join(., dat %>%
               filter(!is.na(Diss.SampID)) %>%
               select(Diss.SampID) %>%
               unique() %>%
               reframe(tot.samp.count = n())) %>%
  filter(is.na(Diss.Flag)) %>%
  filter(!is.na(Diss.Conc.nM.adj)) %>%
  select(Diss.SampID, Region, Compound, Diss.Conc.nM.adj, tot.samp.count) %>%
  unique() %>%
  group_by(Compound) %>%
  reframe(comp.count = n(),
          tot.samp.count = tot.samp.count,
          Rel.fraction.detected = comp.count/tot.samp.count) %>%
  unique()




###Calculate relative fractions:

#particulate:
part.dat.class.sum <- dat %>%
  filter(!is.na(Part.SampID)) %>%
  filter(is.na(Part.Flag)) %>%
  select(Part.SampID, Region, class, Part.Conc.nM) %>%
  unique() %>%
  group_by(Part.SampID) %>%
  mutate(total.nM = sum(Part.Conc.nM)) %>%
  group_by(Part.SampID, Region, class, total.nM) %>%
  reframe(class.nM = sum(Part.Conc.nM),
          class.rel.perc = class.nM/total.nM) %>%
  unique() %>%
  group_by(class) %>%
  mutate(mean.class.rel.perc = mean(class.rel.perc),
         sd.class.rel.perc = sd(class.rel.perc))


#dissolved:
diss.dat.class.sum <- dat %>%
  filter(!is.na(Diss.SampID)) %>%
  filter(is.na(Diss.Flag)) %>%
  select(Diss.SampID, Region, compound.name.figure, class, Diss.Conc.nM.adj) %>%
  unique() %>%
  mutate(class = case_when(compound.name.figure == "TMAO" ~ "TMAO",
                           TRUE ~ class)) %>%
  group_by(Diss.SampID) %>%
  mutate(total.nM = sum(Diss.Conc.nM.adj, na.rm = TRUE)) %>%
  group_by(Diss.SampID, Region, class, total.nM) %>%
  reframe(class.nM = sum(Diss.Conc.nM.adj, na.rm = TRUE),
          class.rel.perc = class.nM/total.nM) %>%
  unique() %>%
  filter(!class %in% c("Sulfonate", "Sugar")) %>%
  group_by(class) %>%
  mutate(mean.class.rel.perc = mean(class.rel.perc),
         sd.class.rel.perc = sd(class.rel.perc))

#total:
tot.dat.class.sum <- dat %>%
  filter(!is.na(Diss.SampID)) %>%
  filter(!is.na(Part.SampID)) %>%
  filter(is.na(Tot.Flag)) %>%
  select(Part.SampID, Region, class, Total.Conc.nM) %>%
  unique() %>%
  group_by(Part.SampID) %>%
  mutate(total.nM = sum(Total.Conc.nM, na.rm = TRUE)) %>%
  group_by(Part.SampID, Region, class, total.nM) %>%
  reframe(class.nM = sum(Total.Conc.nM, na.rm = TRUE),
          class.rel.perc = class.nM/total.nM) %>%
  unique() %>%
  filter(!class %in% c("Sulfonate", "Sulfonium"))

###