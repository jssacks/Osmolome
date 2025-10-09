







### Osmolome Stoichiomety Statistics:






###
library(tidyverse)
library(patchwork)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")



###define inputs
all.dat.file <- "Intermediates/Final_Osmo_Meta_Env_Dataframe.csv"



#Read in data
dat <- read_csv(all.dat.file) %>%
  filter(!Part.SampID == "220902_Smp_TN397_S11_600_U_C") %>%
  filter(!str_detect(Part.SampID, "Smp_S4_C1"),
         !str_detect(Part.SampID, "Smp_S4_C1"))



##Make plots with all samples:

#Organize Perifix data for statistics:
dat.perifix.stoich <- dat %>%
  filter(Cruise %in% c("PERIFIX")) %>%
  filter(!is.na(Part.SampID)) %>%
  mutate(compound.stoich.N = case_when(class %in% c("Betaine", "AA") ~ 1,
                                       compound.name.figure %in% c("Ectoine", "Hydroxyectoine", "TMAO", "Taurine") ~ 1,
                                       TRUE ~ 0)) %>%
  mutate(compound.stoich.C = case_when(class %in% c("Sugar") ~ 1,
                                       TRUE ~ 0)) %>%
  mutate(compound.stoich.S = case_when(class %in% c("Sulfonium", "Sulfonate") ~ 1,
                                       TRUE ~ 0))  %>%
  group_by(Part.SampID) %>%
  mutate(Compound.N.nM = sum(Part.Conc.nM*compound.stoich.N),
         Compound.C.nM = sum(Part.Conc.nM*compound.stoich.C),
         Compound.S.nM = sum(Part.Conc.nM*compound.stoich.S)) %>%
  select(Part.SampID, Cruise, Part.Conc.nM, Part.C.nM, Part.N.nM, Part.S.nM, Compound.N.nM, Compound.C.nM, Compound.S.nM, Treatment) %>%
  unique() %>%
  group_by(Part.SampID) %>%
  mutate(Sum.Part.Conc.nM = sum(Part.Conc.nM, na.rm = TRUE),
         Sum.Part.C.nM = sum(Part.C.nM, na.rm = TRUE),
         Sum.Part.N.nM = sum(Part.N.nM, na.rm = TRUE),
         Sum.Part.S.nM = sum(Part.S.nM, na.rm = TRUE),
         Part.CN = Sum.Part.C.nM/Sum.Part.N.nM,
         Part.CS = Sum.Part.C.nM/Sum.Part.S.nM,
         Part.NS = Sum.Part.N.nM/Sum.Part.S.nM,
         Comp.CN = Compound.C.nM/Compound.N.nM,
         Comp.CS = Compound.C.nM/Compound.S.nM,
         Comp.NS = Compound.N.nM/Compound.S.nM) %>%
  mutate(Treatment = as.factor(Treatment),
         Treatment = fct_relevel(Treatment, c("Tote", "C", "P", "F", "PF", "NPF", "NP", "NF", "N")),
         N = as.factor(case_when(Treatment %in% c("N", "NP", "NF", "NPF") ~ 1,
                                 TRUE ~ 0)), 
         P = as.factor(case_when(Treatment %in% c("P", "NP", "PF", "NPF") ~ 1,
                                 TRUE ~ 0)), 
         Fe = as.factor(case_when(Treatment %in% c("F", "PF", "NF", "NPF") ~ 1,
                                  TRUE ~ 0))) %>%
  filter(!Treatment == "Tote") %>%
  select(Part.SampID, Treatment, N, P, Fe, Sum.Part.Conc.nM, Part.CN, Part.CS, Part.NS, Comp.CN, Comp.CS, Comp.NS) %>%
  unique() %>%
  mutate(N_status = case_when(Treatment %in% c("N", "NP", "NF", "NPF") ~ "+N",
                              TRUE ~ "-N")) 



####
###Do PERIFIX ANOVAs:

#Part - C/N

#ANOVA:
peri.anova.C.N.part <- aov(Part.CN ~ N*P*Fe, data = dat.perifix.stoich)  
summary(peri.anova.C.N.part)

#Post-Hoc Test:
peri.tuk.C.N.part <- TukeyHSD(peri.anova.C.N.part, which = "N")
peri.tuk.C.N.part

#Comp - C/N

#ANOVA:
peri.anova.C.N.comp <- aov(Comp.CN ~ N*P*Fe, data = dat.perifix.stoich)  
summary(peri.anova.C.N.comp)

#Post-Hoc Test:
peri.tuk.C.N.comp <- TukeyHSD(peri.anova.C.N.comp, which = "N")
peri.tuk.C.N.comp


#C/S:
#Part - C/S

#ANOVA:
peri.anova.C.S.part <- aov(Part.CS ~ N*P*Fe, data = dat.perifix.stoich)  
summary(peri.anova.C.S.part)

#Post-Hoc Test:
peri.tuk.C.S.part <- TukeyHSD(peri.anova.C.S.part, which = "N")
peri.tuk.C.S.part

#Comp - C/S

#ANOVA:
peri.anova.C.S.comp <- aov(Comp.CS ~ N*P*Fe, data = dat.perifix.stoich)  
summary(peri.anova.C.S.comp)

#Post-Hoc Test:
peri.tuk.C.S.comp <- TukeyHSD(peri.anova.C.S.comp, which = "N")
peri.tuk.C.S.comp



#N/S:
#Part - N/S

#ANOVA:
peri.anova.N.S.part <- aov(Part.NS ~ N*P*Fe, data = dat.perifix.stoich)  
summary(peri.anova.N.S.part)

#Post-Hoc Test:
peri.tuk.N.S.part <- TukeyHSD(peri.anova.N.S.part, which = "N")
peri.tuk.N.S.part

#Comp - N/S

#ANOVA:
peri.anova.N.S.comp <- aov(Comp.NS ~ N*P*Fe, data = dat.perifix.stoich)  
summary(peri.anova.C.S.comp)

#Post-Hoc Test:
peri.tuk.N.S.comp <- TukeyHSD(peri.anova.N.S.comp, which = "N")
peri.tuk.N.S.comp




#perifix summary values 
peri.sum <- dat.perifix.stoich %>%
  ungroup() %>%
  group_by(N_status) %>%
  reframe(mean_CN_part = mean(Part.CN),
          sd_CN_part = sd(Part.CN),
          mean_CN_comp = mean(Comp.CN),
          sd_CN_comp = sd(Comp.CN),
          mean_CS_part = mean(Part.CS),
          sd_CS_part = sd(Part.CS),
          mean_CS_comp = mean(Comp.CS),
          sd_CS_comp = sd(Comp.CS),
          mean_NS_part = mean(Part.NS),
          sd_NS_part = sd(Part.NS),
          mean_NS_comp = mean(Part.NS),
          sd_NS_comp = sd(Part.NS))














































































