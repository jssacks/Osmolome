

library(tidyverse)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")




##Define inputs:
part.file <- "Intermediates/Particulate_Quant_Output.csv"
meta.file <- "Intermediates/All_metadata_information.csv"

###load in data and combine with metadata
part.dat <- read_csv(part.file)
meta.dat <- read_csv(meta.file)


####
dat.all <- left_join(part.dat, meta.dat) %>%
  rename("Compound" = Name)


gradients <- dat.all %>%
  filter(Cruise %in% c("TN397", "KM1906")) %>%
  filter(Compound %in% compound.order$Compound) %>%
  left_join(., compound.order)


gradients.stoich <- gradients %>%
  group_by(SampID) %>%
  mutate(C.tot = sum(nM_C, na.rm = TRUE),
         N.tot = sum(nM_N, na.rm = TRUE),
         S.tot = sum(nM_S, na.rm = TRUE)) %>%
  mutate(C.N.tot = C.tot/N.tot,
         C.S.tot = C.tot/S.tot,
         N.S.tot = N.tot/S.tot) %>%
  mutate(Group_Carbon = case_when(class %in% c("Sugar") ~ "Yes",
                                TRUE ~ "No"),
         Group_Nitrogen = case_when(class %in% c("AA", "Betaine", "TMAO", "Taurine") ~ "Yes",
                                TRUE ~ "No"),
         Group_Sulfur = case_when(class %in% c("Sulfonium", "Sulfonate", "Taurine") ~ "Yes",
                                TRUE ~ "No"))  %>%
  mutate(C.comp = sum(nM.in.smp[Group_Carbon == "Yes"]),
         N.comp = sum(nM.in.smp[Group_Nitrogen == "Yes"]),
         S.comp = sum(nM.in.smp[Group_Sulfur == "Yes"]),
         C.N.comp = C.comp/N.comp,
         C.S.comp = C.comp/S.comp,
         N.S.comp = N.comp/S.comp) %>%
  filter(!Long > -125)




















#Total Stoichiometry Plots
ggplot(gradients.stoich, aes(x = Lat, y = C.N.tot, color = as.numeric(Local_Time))) + 
  geom_point() 

ggplot(gradients.stoich, aes(x = Lat, y = C.S.tot, color = as.numeric(Local_Time))) + 
  geom_point()

ggplot(gradients.stoich, aes(x = Lat, y = N.S.tot, color = as.numeric(Local_Time))) + 
  geom_point()


#Compound Stoichiometry Plots:
ggplot(gradients.stoich, aes(x = Lat, y = C.N.comp, color = as.numeric(Local_Time))) + 
  geom_point() 

ggplot(gradients.stoich, aes(x = Lat, y = C.S.comp, color = as.numeric(Local_Time))) + 
  geom_point()

ggplot(gradients.stoich, aes(x = Lat, y = N.S.comp, color = as.numeric(Local_Time))) + 
  geom_point()






####
  group_by(Cruise, Compound, depth_m) %>%
  reframe(Mean_nM = mean(nM.in.smp),
          Mean_nM_C = mean(nM_C),
          Mean_nM_N = mean(nM_N))# %>%
  mutate(station.name = case_when(Cruise == "G4_DepthProfiles" & station == 4 ~ "G4 Station 04 (19.6 N)",
                                  Cruise == "G4_DepthProfiles" & station == 7 ~ "G4 Station 07 (10.7 N)",
                                  Cruise == "G4_DepthProfiles" & station == 9 ~ "G4 Station 09 (4.7 N)",
                                  Cruise == "G4_DepthProfiles" & station == 11 ~ "G4 Station 11 (0.14 N)",
                                  Cruise == "G3_DepthProfiles" & station == 4 ~ "G3 Station 04 (41.4 N)",
                                  Cruise == "G3_DepthProfiles" & station == 5 ~ "G3 Station 05 (37.0 N)",
                                  Cruise == "G3_DepthProfiles" & station == 6 ~ "G3 Station 06 (33.0 N)")) %>%
  left_join(., compound.order) %>%
  filter(!is.na(station.name))































































