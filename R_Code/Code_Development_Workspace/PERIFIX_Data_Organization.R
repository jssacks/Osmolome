





###PERIFIX Data Organization and Metadata management:


#define inputs:
dat.file <- "Intermediates/Full_Particulate_Dissolved_Osmolome_Dat_100725.csv"
peri.colab.file <- "Collaborator_Data/PERIFIX/PERIFIX_POC_Community_Comp_Data.csv"



#load in data:
peri.dat <- read_csv(dat.file) %>%
  filter(Cruise == "PERIFIX")

peri.colab.dat <- read_csv(peri.colab.file) %>%
  rename(Part.SampID = SampID) %>%
  select(Part.SampID:Other)



#Combine datasets:
peri.dat.comb <- peri.dat %>%
  select(Cruise, Parent_ID, Part.SampID, Compound, Part.detected, Part.Conc.Vial.uM, 
         Part.Conc.nM, Part.Conc.C.nM, Part.Conc.N.nM, Part.Conc.S.nM, Part.Impute.Conc.nM) %>%
  left_join(., peri.colab.dat) %>%
  mutate(Treatment = as.factor(Treatment),
         Treatment = fct_relevel(Treatment, c("Tote", "C", "P", "F", "PF", "NPF", "NP", "NF", "N")),
         N = as.factor(case_when(Treatment %in% c("N", "NP", "NF", "NPF") ~ 1,
                                 TRUE ~ 0)), 
         P = as.factor(case_when(Treatment %in% c("P", "NP", "PF", "NPF") ~ 1,
                                 TRUE ~ 0)), 
         Fe = as.factor(case_when(Treatment %in% c("F", "PF", "NF", "NPF") ~ 1,
                                  TRUE ~ 0))) %>%
  mutate(N_status = case_when(Treatment %in% c("N", "NP", "NF", "NPF") ~ "plus_N",
                              TRUE ~ "minus_N")) 


#Export:
write_csv(peri.dat.comb, file = "Intermediates/PERIFIX_Osmo_Meta_Dat_101325.csv")






































































