
library(tidyverse)

#Finalizing Script to organize, apply qc, and combine distribution datasets:

#inputs

#particulate
p.quant.file <- "Intermediates/Particulate_Quant_Output.csv"
p.qc.file <- "Intermediates/Particulate_QCdat.csv"
p.impute.file <- "Intermediates/Particulate_Quantified_BlkAve.csv"
  
#dissolved
d.quant.file <- "Intermediates/Dissolved_Quantified_Data.csv"
d.qc.file <- "Intermediates/Dissolved_QCdat.csv"
d.lod.file <- "Intermediates/Dissolved_Blk_LOD_Concentrations.csv"

#Cultures
c.quant.file <- "Intermediates/Culture_Quant_Output.csv"
c.qc.file <- "Intermediates/Culture_QCdat.csv"

#G2SF
g2.dat.file <- "Intermediates/G2SF_Quant_Output.csv"
g2.qc.file <- "Intermediates/G2SF_QCdat.csv"



#Finalize datasets: 

#Particulate (apply QC)

#load in datasets
p.q.dat <- read_csv(p.quant.file) %>%
  rename("Compound" = Name)

p.qc <- read_csv(p.qc.file) %>%
  rename("SampID" = Rep)

p.qc.impute.values <- read_csv(p.impute.file) %>%
  rename("Compound" = Name)

#Make final quant and QC particulate dataset:
p.q.qc.dat <- p.q.dat %>%
  left_join(., p.qc) %>%
  left_join(., p.qc.impute.values) %>%
  filter(!SampID == "221006_Smp_S7_C1_D1_A") %>%
  rename(Part.Conc.Vial.uM = umol.in.vial.ave,
         Part.Conc.nM = nM.in.smp,
         Part.Conc.C.nM = nM_C,
         Part.Conc.N.nM = nM_N,
         Part.Conc.S.nM = nM_S) 

write_csv(p.q.qc.dat, file = "Intermediates/Particulate_Final_Quant_QCed.csv")






#Dissolved (apply QC and blank subtraction)

#load in datasets
d.q.dat <- read_csv(d.quant.file) %>%
  rename("Compound" = Name)

d.qc.dat <- read_csv(d.qc.file) %>%
  rename(SampID = Rep) 

d.qc.impute.values <- read_csv(d.lod.file) %>%
  rename("Compound" = Name) 

#Make final quant and qc dissolved dataset and calculate blank subtracted values:
d.q.qc.dat <- d.q.dat %>%
  left_join(., d.qc.dat) %>%
  left_join(., d.qc.impute.values) %>%
  mutate(Diss.Conc.nM = EE.adjust.conc - EE.adjust.Blk.Av,
         Diss.Conc.C.nM = Diss.Conc.nM*C,
         Diss.Conc.N.nM = Diss.Conc.nM*N,
         Diss.Conc.S.nM = Diss.Conc.nM*S,
         impute.conc.nM = EE.adjust.lod - EE.adjust.Blk.Av,
         LOD.nM = EE.adjust.lod - EE.adjust.Blk.Av,
         Diss.Conc.no.blk.sub.nM = EE.adjust.conc,
         LOD.no.blk.sub.nM = EE.adjust.lod) %>%
  mutate(detected = case_when(Diss.Conc.nM < 0 ~ "No",
                              TRUE ~ detected)) %>%
  filter(!str_detect(SampID, "Std")) %>%
  filter(!str_detect(SampID, "Blk")) %>%
  filter(!str_detect(SampID, "Poo")) %>%
  filter(!Compound == "L-Arginine") %>%
  rename(Diss.Conc.Vial.uM = conc.in.vial.uM) %>%
  select(Cruise, SampID, Compound, detected, Diss.Conc.Vial.uM, Diss.Conc.nM, Diss.Conc.C.nM, Diss.Conc.N.nM, Diss.Conc.S.nM, impute.conc.nM, LOD.nM, Diss.Conc.no.blk.sub.nM, LOD.no.blk.sub.nM)

write_csv(d.q.qc.dat, file = "Intermediates/Dissolved_Final_Quant_QCed.csv")




#Culture (apply QC)

#load in datasets
c.q.dat <- read_csv(c.quant.file) %>%
  rename("Compound" = Name)
c.qc <- read_csv(c.qc.file)

#samples removed qc 
c.q.qc.dat <- c.q.dat %>%
  left_join(., c.qc) %>%
  rename(Part.Conc.Vial.uM = uM.in.vial.ave,
         Part.Conc.uM = uM_in_samp,
         Part.Conc.C.uM = uM_C_smp,
         Part.Conc.N.uM = uM_N_smp,
         Part.Conc.S.uM = uM_S_smp) %>%
  select(Batch, Type, Organism, SampID, Compound, Part.Conc.Vial.uM, Part.Conc.uM, Part.Conc.C.uM, Part.Conc.N.uM, Part.Conc.S.uM, Detected)


write_csv(c.q.qc.dat, file = "Intermediates/Culture_Final_Quant_QCed.csv")





#G2_Size_Fractionated (apply QC)

#load in datasets
g2.q.dat <- read_csv(g2.dat.file) %>%
  rename("Compound" = Name,
         "Part.Conc.Vial.uM" = umol.in.vial.ave)

g2.qc.dat <- read_csv(g2.qc.file)  %>%
  rename("SampID" = Rep)

#apply sample imputation qc 
g2.q.qc.dat <- g2.q.dat %>%
  left_join(., g2.qc.dat) %>%
  mutate(impute.conc.nM = 0) %>%
  rename(Part.Conc.nM = nM.in.smp,
         Part.Conc.C.nM = nM_C,
         Part.Conc.N.nM = nM_N,
         Part.Conc.S.nM = nM_S) %>%
  select(SampID, Compound, Part.Conc.Vial.uM, Part.Conc.nM, Part.Conc.C.nM, Part.Conc.N.nM, Part.Conc.S.nM, detected, impute.conc.nM)
  

write_csv(g2.q.qc.dat, file = "Intermediates/G2SF_Final_Quant_QCed.csv")














# Match Particulate and Dissolved Samples ---------------------------------


####Create Sample Matching Key for Dissolved and Particulate Data:

p.ids <- p.q.dat %>%
  filter(Cruise %in% c("TN397", "KM1906", "RC078", "PERIFIX")) %>%
  filter(!str_detect(SampID, "Blk"),
         !str_detect(SampID, "blk"),
         !str_detect(SampID, "Poo")) %>%
  mutate(Parent_ID = str_remove(SampID, "220902_Smp_")) %>%
  mutate(Parent_ID = str_remove(Parent_ID, "220628_Smp_")) %>%
  mutate(Parent_ID = str_remove(Parent_ID, "221006_Smp_")) %>%
  mutate(Parent_ID = str_remove(Parent_ID, "230213_Smp_")) %>%
  rename(Part.SampID = SampID) %>%
  select(Cruise, Parent_ID, Part.SampID) %>% 
  unique()



#get dissolved sample key
d.ids <- d.q.dat %>%
  filter(Cruise %in% c("TN397", "KM1906", "RC078", "PERIFIX")) %>%
  filter(!str_detect(SampID, "Blk"),
         !str_detect(SampID, "blk"),
         !str_detect(SampID, "Poo")) %>%
  mutate(Parent_ID = str_remove(SampID, "220623_Smp_")) %>%
  mutate(Parent_ID = str_remove(Parent_ID, "240415_Smp_")) %>%
  mutate(Parent_ID = str_remove(Parent_ID, "220602_Smp_")) %>%
  rename(Diss.SampID = SampID) %>%
  select(Cruise, Parent_ID, Diss.SampID) %>%
  unique()



#combine sample keys
Part.Diss.Samp.Key <- full_join(p.ids, d.ids)

#write to csv
write_csv(Part.Diss.Samp.Key, file = "Intermediates/Distributions_Part_Diss_Sample_Key.csv")




####Match up particulate and dissolved measurements:
part.dat.match <- p.q.qc.dat %>%
  rename(Part.SampID = SampID,
         Part.detected = detected,
         Part.Impute.Conc.nM = impute_conc_nM) %>%
  select(-Vol_L)

diss.dat.match <- d.q.qc.dat %>%
  rename(Diss.SampID = SampID,
         Diss.detected = detected,
         Diss.Impute.Conc.nM = impute.conc.nM,
         Diss.LOD.nM = LOD.nM,
         Diss.LOD.no.blk.sub.nM = LOD.no.blk.sub.nM) %>%
  filter(Diss.SampID %in% d.ids$Diss.SampID)

p.d.dat <- left_join(Part.Diss.Samp.Key, part.dat.match) %>%
  full_join(., diss.dat.match)


write_csv(p.d.dat, file = "Intermediates/Full_Particulate_Dissolved_Osmolome_Dat_100725.csv")








































