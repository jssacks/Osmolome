






##Make data of Taxonomic Biomarkers for Steph:


library(tidyverse)
library(patchwork)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")





###define inputs
all.dat.file <- "Intermediates/Enviro_Osmo_Final_Dataset_with_metadata.csv"






#Read in data 
dat <- read_csv(all.dat.file) %>%
  left_join(., compound.order) %>%
  filter(!str_detect(Part.SampID, "Blk")) %>%
  mutate(Part.Conc.nM = case_when(Part.detected == "No" ~ Part.Impute.Conc.nM,
                                  TRUE ~ Part.Conc.nM)) #%>%
  # filter(compound.name.figure %in% c("Trigonelline", "Trehalose", "TMAB",
  #                                    "Sucrose", "Proline", "Isethionic acid",
  #                                    "Homarine", "Gonyol", "GG", "DMSA", "DHPS",
  #                                    "Carnitine", "beta_Alanine betaine", "Asparagine",
  #                                    "(Iso)leucine"))





#Data for Steph:

#Organize G3 data by Leg
# g3.dat.totosmo <- dat %>%
#   filter(Cruise %in% c("KM1906")) %>%
#   group_by(Part.SampID, Diss.SampID, Cruise, Lat, Long, station, poc) %>%
#   reframe(Part.Osmo.Conc.nM = sum(Part.Conc.nM, na.rm = TRUE),
#           Diss.Osmo.Conc.nM = sum(Diss.Conc.nM.adj, na.rm = TRUE),
#           Total.Osmo.Conc.nM = sum(Total.Conc.nM, na.rm = TRUE)) %>%
#   mutate(G3_station = str_remove(Part.SampID, "220628_Smp_MU"),
#          G3_station = str_remove(G3_station, "_A"),
#          G3_station = str_remove(G3_station, "_B"),
#          G3_station = str_remove(G3_station, "_C"),
#          G3_station = as.numeric(G3_station),
#          Leg = case_when(G3_station < 12 ~ "Leg_1",
#                          G3_station > 16 ~ "Leg_3",
#                          TRUE ~ "Leg_2")) %>%
#   mutate(lat_bin = cut(Lat, breaks = lat.breaks, include.lowest = TRUE, right = FALSE)) %>%
#   group_by(Leg, lat_bin) %>%
#   reframe(mean_part_conc = mean(Part.Osmo.Conc.nM, na.rm = TRUE),
#           mean_diss_conc = mean(Diss.Osmo.Conc.nM, na.rm = TRUE),
#           mean_total_conc = mean(Total.Osmo.Conc.nM, na.rm = TRUE),
#           sd_part_conc = sd(Part.Osmo.Conc.nM, na.rm = TRUE),
#           sd_diss_conc = sd(Diss.Osmo.Conc.nM, na.rm = TRUE),
#           sd_tot_conc = sd(Total.Osmo.Conc.nM, na.rm = TRUE),
#           mean_poc = mean(poc, na.rm = TRUE),
#           sd_poc = sd(poc, na.rm = TRUE),
#           mean_lat = mean(Lat)) %>%
#   mutate(Compound = "Total") %>%
#   select(Compound, Leg, lat_bin, mean_lat, mean_part_conc, sd_part_conc)

g3.dat <- dat %>%
  filter(Cruise %in% c("KM1906")) %>%
  mutate(G3_station = str_remove(Part.SampID, "220628_Smp_MU"),
         G3_station = str_remove(G3_station, "_A"),
         G3_station = str_remove(G3_station, "_B"),
         G3_station = str_remove(G3_station, "_C"),
         G3_station = as.numeric(G3_station),
         Leg = case_when(G3_station < 12 ~ "Leg_1",
                         G3_station > 16 ~ "Leg_3",
                         TRUE ~ "Leg_2")) %>%
  mutate(lat_bin = cut(Lat, breaks = lat.breaks, include.lowest = TRUE, right = FALSE)) %>%
  group_by(compound.name.figure, Leg, lat_bin) %>%
  reframe(mean_part_conc = mean(Part.Conc.nM, na.rm = TRUE),
          sd_part_conc = sd(Part.Conc.nM, na.rm = TRUE),
          mean_poc = mean(poc, na.rm = TRUE),
          sd_poc = sd(poc, na.rm = TRUE),
          mean_lat = mean(Lat)) %>%
  select(compound.name.figure, Leg, lat_bin, mean_lat, mean_part_conc, sd_part_conc)






#Organize G4 data by Leg
# g4.dat <- dat %>%
#   filter(Cruise %in% c("TN397")) %>%
#   group_by(Part.SampID, Diss.SampID, Cruise, Lat, Long, station, poc) %>%
#   filter(!Part.SampID == "220902_Smp_TN397_U8_BB") %>%
#   reframe(Part.Osmo.Conc.nM = sum(Part.Conc.nM, na.rm = TRUE),
#           Diss.Osmo.Conc.nM = sum(Diss.Conc.nM.adj, na.rm = TRUE),
#           Total.Osmo.Conc.nM = sum(Total.Conc.nM, na.rm = TRUE)) %>%
#   mutate(Leg = case_when(Long > -139.5 ~ "Leg_1",
#                          Long < -140.5 ~ "Leg_3",
#                          TRUE ~ "Leg_2")) %>%
#   mutate(lat_bin = cut(Lat, breaks = lat.breaks, include.lowest = TRUE, right = FALSE)) %>%
#   group_by(Leg, lat_bin) %>%
#   reframe(mean_part_conc = mean(Part.Osmo.Conc.nM, na.rm = TRUE),
#           mean_diss_conc = mean(Diss.Osmo.Conc.nM, na.rm = TRUE),
#           mean_total_conc = mean(Total.Osmo.Conc.nM, na.rm = TRUE),
#           sd_part_conc = sd(Part.Osmo.Conc.nM, na.rm = TRUE),
#           sd_diss_conc = sd(Diss.Osmo.Conc.nM, na.rm = TRUE),
#           sd_tot_conc = sd(Total.Osmo.Conc.nM, na.rm = TRUE),
#           mean_poc = mean(poc, na.rm = TRUE),
#           sd_poc = sd(poc, na.rm = TRUE),
#           mean_lat = mean(Lat)) %>%
#   mutate(Compound = "Total") %>%
#   select(Compound, Leg, lat_bin, mean_lat, mean_part_conc, sd_part_conc)


g4.dat <- dat %>%
  filter(Cruise %in% c("TN397")) %>%
  filter(!Part.SampID == "220902_Smp_TN397_U8_BB") %>%
#  reframe(Part.Osmo.Conc.nM = sum(Part.Conc.nM, na.rm = TRUE)) %>%
  mutate(Leg = case_when(Long > -139.5 ~ "Leg_1",
                         Long < -140.5 ~ "Leg_3",
                         TRUE ~ "Leg_2")) %>%
  mutate(lat_bin = cut(Lat, breaks = lat.breaks, include.lowest = TRUE, right = FALSE)) %>%
  group_by(compound.name.figure, Leg, lat_bin) %>%
  reframe(mean_part_conc = mean(Part.Conc.nM, na.rm = TRUE),
          sd_part_conc = sd(Part.Conc.nM, na.rm = TRUE),
          mean_poc = mean(poc, na.rm = TRUE),
          sd_poc = sd(poc, na.rm = TRUE),
          mean_lat = mean(Lat)) %>%
  select(compound.name.figure, Leg, lat_bin, mean_lat, mean_part_conc, sd_part_conc)






#separate data by leg for export:

#G3 data for export:
g3.leg1.dat <- g3.dat %>% filter(Leg == "Leg_1")
g3.leg2.dat <- g3.dat %>% filter(Leg == "Leg_2")
g3.leg3.dat <- g3.dat %>% filter(Leg == "Leg_3")

#G4 data for export:
g4.leg1.dat <- g4.dat %>% filter(Leg == "Leg_1")
g4.leg2.dat <- g4.dat %>% filter(Leg == "Leg_2")
g4.leg3.dat <- g4.dat %>% filter(Leg == "Leg_3")

#Write as separate files:
write_csv(g3.leg1.dat, file = "Intermediates/Biogeography_Synthesis/G3_Leg1_Osmo.csv")
write_csv(g3.leg2.dat, file = "Intermediates/Biogeography_Synthesis/G3_Leg2_Osmo.csv")
write_csv(g3.leg3.dat, file = "Intermediates/Biogeography_Synthesis/G3_Leg3_Osmo.csv")
write_csv(g4.leg1.dat, file = "Intermediates/Biogeography_Synthesis/G4_Leg1_Osmo.csv")
write_csv(g4.leg2.dat, file = "Intermediates/Biogeography_Synthesis/G4_Leg2_Osmo.csv")
write_csv(g4.leg3.dat, file = "Intermediates/Biogeography_Synthesis/G4_Leg3_Osmo.csv")


































































