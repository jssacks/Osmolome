



library(tidyverse)
library(patchwork)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")






###define inputs
all.dat.file <- "Intermediates/Enviro_Osmo_Final_Dataset_with_metadata.csv"


#Read in data 
dat <- read_csv(all.dat.file) %>%
  left_join(., compound.order) %>%
  filter(!str_detect(Part.SampID, "Blk")) %>%
  filter(!Parent_ID == "TN397_S11_600_U_C") %>%
  filter(!is.na(compound.name.figure))

#Define regions:
dat.region <- dat %>%
  mutate(Region = case_when(Cruise == "KM1906" & Lat >= 35 ~ "NPTZ",
                            Cruise == "KM1906" & Lat < 35 ~ "NPSG",
                            Cruise == "TN397" & Long > -122 ~ "CUCP",
                            Cruise == "TN397" & Long < -122 & Lat > 10.5 ~ "NPSG",
                            Cruise == "TN397" & Lat < 10.5 & Lat > 4.5 ~ "NPEC",
                            Cruise == "TN397" & Lat < 4.5 ~ "PEDP",
                            Cruise == "RC078" ~ "SS"))



#Calculate Overall Particulate and Dissolved Concentrations by region

#particulate
Part.Conc.Sum <- dat.region %>%
  filter(Part.detected == "Yes") %>%
  group_by(Parent_ID) %>%
  mutate(Sum.Part.Conc.nM = sum(Part.Conc.nM)) %>%
  group_by(Cruise, Region) %>%
  reframe(mean.part.conc.nM = mean(Sum.Part.Conc.nM),
          sd.part.conc.nM = sd(Sum.Part.Conc.nM),
          max.part.conc.nM = max(Sum.Part.Conc.nM),
          min.part.conc.nM = min(Sum.Part.Conc.nM))

#dissolved
Diss.Conc.Sum <- dat.region %>%
  filter(Diss.detected == "Yes") %>%
  group_by(Parent_ID) %>%
  mutate(Sum.Diss.Conc.nM = sum(Diss.Conc.nM)) %>%
  group_by(Cruise, Region) %>%
  reframe(mean.diss.conc.nM = mean(Sum.Diss.Conc.nM),
          sd.diss.conc.nM = sd(Sum.Diss.Conc.nM),
          max.diss.conc.nM = max(Sum.Diss.Conc.nM),
          min.diss.conc.nM = min(Sum.Diss.Conc.nM))




###_Calculate C/N/S ratios of particulate osmolomes 
part.stoich <- dat.region %>%
  filter(Part.detected == "Yes") %>%
  group_by(Parent_ID) %>%
  mutate(Sum.Part.C.nM = sum(Part.Conc.C.nM, na.rm = TRUE),
         Sum.Part.N.nM = sum(Part.Conc.N.nM, na.rm = TRUE),
         Sum.Part.S.nM = sum(Part.Conc.S.nM, na.rm = TRUE),
         Part.C.N.ratio = Sum.Part.C.nM/Sum.Part.N.nM,
         Part.C.S.ratio = Sum.Part.C.nM/Sum.Part.S.nM,
         Part.N.S.ratio = Sum.Part.N.nM/Sum.Part.S.nM) %>%
  select(Cruise, Parent_ID, Region, Lat, Long, Part.C.N.ratio, Part.C.S.ratio, Part.N.S.ratio)


part.stoich.sum <- part.stoich %>%
  group_by(Cruise, Region) %>%
  reframe(Mean.C.N.ratio = mean(Part.C.N.ratio),
          SD.C.N.ratio = sd(Part.C.N.ratio),
          Mean.C.S.ratio = mean(Part.C.S.ratio),
          SD.C.S.ratio = sd(Part.C.S.ratio),
          Mean.N.S.ratio = mean(Part.N.S.ratio),
          SD.N.S.ratio = sd(Part.N.S.ratio))

part.stoich.overall.sum <- part.stoich %>%
  ungroup() %>%
  reframe(Mean.C.N.ratio = mean(Part.C.N.ratio),
          SD.C.N.ratio = sd(Part.C.N.ratio),
          Mean.C.S.ratio = mean(Part.C.S.ratio),
          SD.C.S.ratio = sd(Part.C.S.ratio),
          Mean.N.S.ratio = mean(Part.N.S.ratio),
          SD.N.S.ratio = sd(Part.N.S.ratio))








#Calculate Percent of organic matter
percent.dat <- dat.region %>%
  filter(Part.detected == "Yes") %>%
  mutate(pos.uM.est = poc*(1.3/124),
         osmo.perc.c = (Part.Conc.C.nM/(poc*1000))*100,
         osmo.perc.n = (Part.Conc.N.nM/(pn*1000))*100,
         osmo.perc.s = (Part.Conc.S.nM/(pos.uM.est*1000))*100) %>%
  filter(!is.na(poc)) %>%
  select(Cruise, Part.SampID, Region, Lat, Long, compound.name.figure, osmo.perc.c, osmo.perc.n, osmo.perc.s, poc, pn, pos.uM.est) %>%
  group_by(Cruise, Part.SampID, Region, Lat, Long, poc, pn, pos.uM.est) %>%
  reframe(sum.osmo.perc.c = sum(osmo.perc.c, na.rm = TRUE),
          sum.osmo.perc.n = sum(osmo.perc.n, na.rm = TRUE),
          sum.osmo.perc.s = sum(osmo.perc.s, na.rm = TRUE)
          )


#remove extreme outliers from SS samples (221006_Smp_S6_C1_D1_A, 221006_Smp_S7_C3_D5_B)

percent.dat.summary <- percent.dat %>%
  filter(!Part.SampID %in% c("221006_Smp_S6_C1_D1_A", "221006_Smp_S7_C3_D5_B")) %>%  #major outliers with estiamtes of % pos > than 100% indicating incorrect filtering, etc.
  group_by(Cruise, Region) %>%
  reframe(mean.percent.c = mean(sum.osmo.perc.c),
          sd.percent.c = sd(sum.osmo.perc.c),
          max.percent.c = max(sum.osmo.perc.c),
          min.percent.c = min(sum.osmo.perc.c),
          mean.percent.n = mean(sum.osmo.perc.n),
          sd.percent.n = sd(sum.osmo.perc.n),
          max.percent.n = max(sum.osmo.perc.n),
          min.percent.n = min(sum.osmo.perc.n),
          mean.percent.s = mean(sum.osmo.perc.s),
          sd.percent.s = sd(sum.osmo.perc.s),
          max.percent.s = max(sum.osmo.perc.s),
          min.percent.s = min(sum.osmo.perc.s),
          mean.poc.um = mean(poc),
          sd.poc.um = sd(poc),
          mean.pn.um = mean(pn),
          sd.pn.um = sd(pn),
          mean.pos.um = mean(pos.uM.est),
          sd.pos.um = sd(pos.uM.est))



percent.dat.openocean.summary <- percent.dat %>%
  filter(!Part.SampID %in% c("221006_Smp_S6_C1_D1_A", "221006_Smp_S7_C3_D5_B")) %>%  #major outliers with estiamtes of % pos > than 100% indicating incorrect filtering, etc.
  filter(!Cruise == "RC078") %>%
  ungroup() %>%
  reframe(mean.percent.c = mean(sum.osmo.perc.c),
          sd.percent.c = sd(sum.osmo.perc.c),
          max.percent.c = max(sum.osmo.perc.c),
          min.percent.c = min(sum.osmo.perc.c),
          mean.percent.n = mean(sum.osmo.perc.n),
          sd.percent.n = sd(sum.osmo.perc.n),
          max.percent.n = max(sum.osmo.perc.n),
          min.percent.n = min(sum.osmo.perc.n),
          mean.percent.s = mean(sum.osmo.perc.s),
          sd.percent.s = sd(sum.osmo.perc.s),
          max.percent.s = max(sum.osmo.perc.s),
          min.percent.s = min(sum.osmo.perc.s))



##Calculate Percent of Dissolved organic matter in dissolved metabolites
Perc.DOC.Sum <- dat.region %>%
  filter(Diss.detected == "Yes") %>%
  group_by(Parent_ID) %>%
  mutate(Sum.Diss.Conc.C.nM = sum(Diss.Conc.C.nM, na.rm = TRUE)) %>%
  mutate(doc.um = case_when(Cruise == "RC078" ~ doc,
                            TRUE ~ 80)) %>%
  mutate(osmo.perc.doc = Sum.Diss.Conc.C.nM/(doc.um*1000)*100) %>%
  group_by(Cruise, Region) %>%
  reframe(mean.perc.doc = mean(osmo.perc.doc),
          sd.perc.doc = sd(osmo.perc.doc),
          max.perc.doc = max(osmo.perc.doc),
          min.perc.doc = min(osmo.perc.doc),
          mean.osmo.C.nM = mean(Sum.Diss.Conc.C.nM),
          sd.osmo.C.nM = sd(Sum.Diss.Conc.C.nM),
          mean.doc.uM = mean(doc.um),
          sd.doc.uM = sd(doc.um))


##open ocean perc doc summary
openocean.perc.doc.sum <- dat.region %>%
  filter(Diss.detected == "Yes") %>%
  group_by(Parent_ID) %>%
  mutate(Sum.Diss.Conc.C.nM = sum(Diss.Conc.C.nM, na.rm = TRUE)) %>%
  mutate(doc.um = case_when(Cruise == "RC078" ~ doc,
                            TRUE ~ 80)) %>%
  mutate(osmo.perc.doc = Sum.Diss.Conc.C.nM/(doc.um*1000)*100) %>%
  ungroup() %>%
  filter(!Cruise %in% "RC078") %>%
  reframe(mean.perc.doc = mean(osmo.perc.doc),
          sd.perc.doc = sd(osmo.perc.doc),
          max.perc.doc = max(osmo.perc.doc),
          min.perc.doc = min(osmo.perc.doc),
          mean.osmo.C.nM = mean(Sum.Diss.Conc.C.nM),
          sd.osmo.C.nM = sd(Sum.Diss.Conc.C.nM),
          mean.doc.uM = mean(doc.um),
          sd.doc.uM = sd(doc.um))


###__DON___

Perc.DON.Sum <- dat.region %>%
  filter(Diss.detected == "Yes") %>%
  group_by(Parent_ID) %>%
  mutate(Sum.Diss.Conc.N.nM = sum(Diss.Conc.N.nM, na.rm = TRUE)) %>%
  mutate(don.um = case_when(Cruise == "RC078" ~ 10.8,
                            TRUE ~ 5.6)) %>%
  mutate(osmo.perc.don = Sum.Diss.Conc.N.nM/(don.um*1000)*100) %>%
  group_by(Cruise, Region) %>%
  reframe(mean.perc.don = mean(osmo.perc.don),
          sd.perc.don = sd(osmo.perc.don),
          max.perc.don = max(osmo.perc.don),
          min.perc.don = min(osmo.perc.don),
          mean.osmo.n.nM = mean(Sum.Diss.Conc.N.nM),
          sd.osmo.n.nM = sd(Sum.Diss.Conc.N.nM),
          mean.don.uM = mean(don.um),
          sd.don.uM = sd(don.um))

##open ocean perc don summary
openocean.perc.don.sum <- dat.region %>%
  filter(Diss.detected == "Yes") %>%
  group_by(Parent_ID) %>%
  mutate(Sum.Diss.Conc.N.nM = sum(Diss.Conc.N.nM, na.rm = TRUE)) %>%
  mutate(don.um = case_when(Cruise == "RC078" ~ 10.8,
                            TRUE ~ 5.6)) %>%
  mutate(osmo.perc.don = Sum.Diss.Conc.N.nM/(don.um*1000)*100) %>%
  filter(!Cruise == "RC078") %>%
  ungroup() %>%
  reframe(mean.perc.don = mean(osmo.perc.don),
          sd.perc.don = sd(osmo.perc.don),
          max.perc.don = max(osmo.perc.don),
          min.perc.don = min(osmo.perc.don),
          mean.osmo.n.nM = mean(Sum.Diss.Conc.N.nM),
          sd.osmo.n.nM = sd(Sum.Diss.Conc.N.nM),
          mean.don.uM = mean(don.um),
          sd.don.uM = sd(don.um))

































