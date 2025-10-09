





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


###Dat.TN397"
g4.dat <- dat %>%
  filter(Cruise %in% c("TN397")) %>%
  group_by(Part.SampID) %>%
  mutate(tot.part.conc.nM = sum(Part.Conc.nM),
         rel.part.conc = Part.Conc.nM/tot.part.conc.nM) %>%
  group_by(compound.name.figure, Region) %>%
  reframe(mean.region.conc = mean(rel.part.conc)) %>%
  pivot_wider(id_cols = compound.name.figure, names_from = "Region", values_from = mean.region.conc) %>%
  mutate(FC_Eq_Gyre = Equator/NPSG,
         log2fc_Eq_Gyre = log2(FC_Eq_Gyre))




###Dat.KM1906"
g3.dat <- dat %>%
  filter(Cruise %in% c("KM1906")) %>%
  group_by(Part.SampID) %>%
  mutate(tot.part.conc.nM = sum(Part.Conc.nM),
         rel.part.conc = Part.Conc.nM/tot.part.conc.nM) %>%
  group_by(compound.name.figure, Region) %>%
  reframe(mean.region.conc = mean(rel.part.conc)) %>%
  pivot_wider(id_cols = compound.name.figure, names_from = "Region", values_from = mean.region.conc) %>%
  mutate(FC_TZ_Gyre = NPTZ/NPSG,
         log2fc_TZ_Gyre = log2(FC_TZ_Gyre))


pelag.dat <- left_join(g3.dat, g4.dat %>% select(compound.name.figure, log2fc_Eq_Gyre))

ggplot(pelag.dat, aes(x = log2fc_TZ_Gyre, y = log2fc_Eq_Gyre)) +
  geom_text(aes(label = compound.name.figure)) +
  geom_point() +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")




####T-tests

###TN397 data:
###Dat.TN397"
g4.dat <- dat %>%
  filter(Cruise %in% c("TN397")) %>%
  group_by(Part.SampID) %>%
  mutate(tot.part.conc.nM = sum(Part.Conc.nM),
         rel.part.conc = Part.Conc.nM/tot.part.conc.nM) %>%
  filter(Region %in% c("NPSG", "Equator"))

library(rstatix)

g4.ttest.dat <- g4.dat %>%
  select(Part.SampID, compound.name.figure, rel.part.conc, Region) 

g4.ttest <- g4.ttest.dat %>%
  group_by(compound.name.figure) %>%
  t_test(rel.part.conc ~ Region) %>%
  select(compound.name.figure, p) %>%
  rename(g4.p = p)



###KM1906 data:
###Dat.TN397"
g3.dat <- dat %>%
  filter(Cruise %in% c("KM1906")) %>%
  group_by(Part.SampID) %>%
  mutate(tot.part.conc.nM = sum(Part.Conc.nM),
         rel.part.conc = Part.Conc.nM/tot.part.conc.nM) %>%
  filter(Region %in% c("NPSG", "NPTZ"))


g3.ttest.dat <- g3.dat %>%
  select(Part.SampID, compound.name.figure, rel.part.conc, Region) 

g3.ttest <- g3.ttest.dat %>%
  group_by(compound.name.figure) %>%
  t_test(rel.part.conc ~ Region) %>%
  select(compound.name.figure, p) %>%
  rename(g3.p = p)




###Combine data:
dat.comb.stat <- left_join(pelag.dat, g3.ttest) %>%
  left_join(g4.ttest) %>%
  mutate(stat_sig = case_when(g3.p <= 0.01 & g4.p <= 0.01 ~ "Both < 0.01",
                              g3.p <= 0.01 & g4.p > 0.01 ~ "Just G3 < 0.01",
                              g3.p > 0.01 & g4.p <= 0.01 ~ "Just G4 < 0.01",
                              TRUE ~ "Neither < 0.01")) %>%
  mutate(behavior.g3 = case_when(log2fc_TZ_Gyre < 0 & g3.p <= 0.01 ~ "Sig_Depleted",
                                 log2fc_TZ_Gyre > 0 & g3.p <= 0.01 ~ "Sig_Enriched",
                                 TRUE ~ "Not Sig"),
         behavior.g4 = case_when(log2fc_Eq_Gyre < 0 & g4.p <= 0.01 ~ "Sig_Depleted",
                                 log2fc_Eq_Gyre > 0 & g4.p <= 0.01 ~ "Sig_Enriched",
                                 TRUE ~ "Not Sig"),
         overall.behavior = case_when(behavior.g3 == "Sig_Enriched" & behavior.g4 == "Sig_Enriched" ~ "Both_Enriched",
                                      behavior.g3 == "Sig_Depleted" & behavior.g4 == "Sig_Depleted" ~ "Both_Depleted",
                                      TRUE ~ "Neither")
         )


ggplot(dat.comb.stat, aes(x = log2fc_TZ_Gyre, y = log2fc_Eq_Gyre)) +
  geom_label(aes(label = compound.name.figure)) +
  geom_point(aes(color = overall.behavior), size = 2) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  theme_test()







####PERIFIX:
pfix.dat <- dat %>%
  filter(Cruise %in% c("PERIFIX")) %>%
  group_by(Part.SampID) %>%
  mutate(tot.part.conc.nM = sum(Part.Conc.nM),
         rel.part.conc = Part.Conc.nM/tot.part.conc.nM) %>%
  mutate(Treatment = as.factor(Treatment),
         Treatment = fct_relevel(Treatment, c("Tote", "C", "P", "F", "PF", "NPF", "NP", "NF", "N")),
         N = as.factor(case_when(Treatment %in% c("N", "NP", "NF", "NPF") ~ 1,
                                 TRUE ~ 0)), 
         P = as.factor(case_when(Treatment %in% c("P", "NP", "PF", "NPF") ~ 1,
                                 TRUE ~ 0)), 
         Fe = as.factor(case_when(Treatment %in% c("F", "PF", "NF", "NPF") ~ 1,
                                  TRUE ~ 0))) %>%
  filter(!Treatment == "Tote") %>%
  mutate(N_status = case_when(Treatment %in% c("N", "NP", "NF", "NPF") ~ "plus_N",
                              TRUE ~ "minus_N")) %>%
  group_by(compound.name.figure, N_status) %>%
  reframe(mean.treat.conc = mean(rel.part.conc)) %>%
  pivot_wider(id_cols = compound.name.figure, names_from = "N_status", values_from = mean.treat.conc) %>%
  mutate(FC_plus_minus_N = plus_N/minus_N,
         log2fc_plus_minus_N = log2(FC_plus_minus_N)) %>%
  select(compound.name.figure,  FC_plus_minus_N, log2fc_plus_minus_N)


###Dat.TN397"
pfix.dat.ttest <- pfix.dat <- dat %>%
  filter(Cruise %in% c("PERIFIX")) %>%
  group_by(Part.SampID) %>%
  mutate(tot.part.conc.nM = sum(Part.Conc.nM),
         rel.part.conc = Part.Conc.nM/tot.part.conc.nM) %>%
  mutate(Treatment = as.factor(Treatment),
         Treatment = fct_relevel(Treatment, c("Tote", "C", "P", "F", "PF", "NPF", "NP", "NF", "N")),
         N = as.factor(case_when(Treatment %in% c("N", "NP", "NF", "NPF") ~ 1,
                                 TRUE ~ 0)), 
         P = as.factor(case_when(Treatment %in% c("P", "NP", "PF", "NPF") ~ 1,
                                 TRUE ~ 0)), 
         Fe = as.factor(case_when(Treatment %in% c("F", "PF", "NF", "NPF") ~ 1,
                                  TRUE ~ 0))) %>%
  filter(!Treatment == "Tote") %>%
  mutate(N_status = case_when(Treatment %in% c("N", "NP", "NF", "NPF") ~ "plus_N",
                              TRUE ~ "minus_N")) %>%
  select(Part.SampID, compound.name.figure, rel.part.conc, N_status) 

###
pfix.ttest <- pfix.dat.ttest %>%
  group_by(compound.name.figure) %>%
  t_test(rel.part.conc ~ N_status) %>%
  select(compound.name.figure, p) %>%
  rename(pfix.p = p)

##P-fix Fold Change analysis:
pfix.fc.analysis <- left_join(pfix.dat, pfix.ttest)


ggplot(pfix.fc.analysis, aes(x = log2fc_plus_minus_N, y = -log10(pfix.p))) +
  geom_point() +
  geom_text(aes(label = compound.name.figure)) +
  geom_hline(yintercept = -log10(0.01), linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed")







###define inputs
diss.dat.g3 <- dat %>%
  filter(Cruise == "KM1906")

write_csv(diss.dat.g3, file = "Intermediates/G3_Metabolite_Data_for_Elaina.csv")



























