




library(tidyverse)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")


#load in data:

######define inputs
all.dat.file <- "Intermediates/Final_Osmo_Meta_Env_Dataframe.csv"
cult.file <- "Intermediates/Culture_Quant_Output.csv"
cult.meta.file <- "Intermediates/All_culture_metadata.csv"




#Read in data
dat <- read_csv(all.dat.file) %>%
  filter(!Part.SampID == "220902_Smp_TN397_S11_600_U_C")



#### Look at Relative abundance by sample across region and get overall rank:
Rel.metab.dat <- dat %>%
  select(Part.SampID, Cruise, Region, Part.Conc.nM, Compound, compound.name.figure, class, depth_m) %>%
  unique() %>%
  filter(Cruise %in% c("TN397", "KM1906", "RC078")) %>%
#  filter(!Cruise %in% c("RC078")) %>%
  filter(depth_m < 16) %>%
  group_by(Part.SampID) %>%
  mutate(Tot.Part.Conc.nM = sum(Part.Conc.nM),
         Rel.Osmo.Conc = Part.Conc.nM/Tot.Part.Conc.nM) %>%
  group_by(Compound) %>%
  mutate(Comp.Mean.Rel.Conc = mean(Rel.Osmo.Conc),
         SD.Rel.Conc = sd(Rel.Osmo.Conc),
         RSD.Rel.Conc = SD.Rel.Conc/Comp.Mean.Rel.Conc,
         Variance.Rel.Conc = max(Rel.Osmo.Conc) - min(Rel.Osmo.Conc),
         Rel.Variance = Variance.Rel.Conc/Comp.Mean.Rel.Conc)
  

comp.rsd.summary <- Rel.metab.dat %>%
  select(Compound, compound.name.figure, class, Comp.Mean.Rel.Conc, SD.Rel.Conc, RSD.Rel.Conc, Variance.Rel.Conc, Rel.Variance) %>%
  unique()

  
  





#Culture #s data
###load in data:
cult.dat <- read_csv(cult.file)
meta.dat <- read_csv(cult.meta.file) 


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
 # filter(!Type %in% c("Bacteria")) %>%
  mutate(Total.Osmo.Conc.uM = sum(ave.conc.uM),
         Rel.Osmo.Conc = (ave.conc.uM/Total.Osmo.Conc.uM)*100)



cult.pres.one.percent <- Cult.dat.relabun %>%
  filter(Rel.Osmo.Conc >= 1) %>%
  mutate(Presence = 1) %>%
  group_by(Compound) %>%
  mutate(num.present = sum(Presence))


cult.compound.sum <- cult.pres.one.percent %>%
  dplyr::select(compound.name.figure, num.present) %>%
  unique()

library(viridis)


###Combine the two datasets:
combined.dat <- left_join(comp.rsd.summary, cult.compound.sum) %>%
  mutate(Euk.only = case_when(class %in% c("Sulfonium", "Sulfonate") ~ "yes",
                              TRUE ~ "no")) %>%
  mutate(numb_bin = as.factor(case_when(num.present < 6 ~ "1-5",
                              num.present > 5 && num.present < 11 ~ "6-10",
                              num.present > 10 && num.present < 16 ~ "11-15",
                              num.present > 15 && num.present < 21 ~ "16-20",
                              num.present > 20 ~ ">20",
                              TRUE ~ "not detected"))) %>%
  mutate(numb_bin = fct_relevel(numb_bin, c("not detected", "1-5", "6-10", "11-15", "16-20", ">20")))
  
 # filter(!Comp.Mean.Rel.Conc <= 0.01)

ggplot(combined.dat %>% filter(!numb_bin == "not detected"), aes(x = numb_bin, y = RSD.Rel.Conc)) +
  geom_boxplot() +
  geom_jitter(width = 0.1)



ggplot(combined.dat, aes(x = num.present, y = RSD.Rel.Conc)) +
  geom_point(aes(color = class), size = 3) +
  scale_color_viridis(discrete = TRUE) +
  xlab("Number of cultures") +
  ylab("Relative Standard Deviation of Relative Abundance")
#  geom_smooth(method = "lm", formula = y ~ log10(x)) +
#  geom_text(aes(label = compound.name.figure)) 
ggplot(combined.dat, aes(x = num.present, y = Rel.Variance)) +
  geom_point(aes(color = Comp.Mean.Rel.Conc), size = 3) +
  scale_color_viridis() +
  geom_text(aes(label = compound.name.figure))




ggplot(combined.dat, aes(x = Euk.only, y = RSD.Rel.Conc)) +
  geom_boxplot() + 
  geom_point() 


ggplot(combined.dat, aes(x = num.present, y = Comp.Mean.Rel.Conc)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_text(aes(label = compound.name.figure))


ggplot(combined.dat, aes(x = Comp.Mean.Rel.Conc, y = SD.Rel.Conc)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
#  geom_smooth(method = "lm") +
  geom_text(aes(label = compound.name.figure))









  
  
  




















































































































































































