




##load packages
library(tidyverse)

##Functions:
source("R_Code/Functions.R")


##Define input files:


#________________Particulate___________
#G3 Particulate
g3p.pos.file <- "Raw_Data/Distribution_Data/Particulate/G3_particulate_HILICPos_Jan25.csv"
g3p.neg.file <- "Raw_Data/Distribution_Data/Particulate/G3_particulate_HILICNeg_Oct24.csv"

#G4 Particulate
g4p.pos.file <- "Raw_Data/Distribution_Data/Particulate/G4_particulate_HILICPos_Jan25.csv"
g4p.neg.file <- "Raw_Data/Distribution_Data/Particulate/G4_particulate_HILICNeg_Oct24.csv"

#D1 Particulate
d1p.pos.file <- "Raw_Data/Distribution_Data/Particulate/D1_particulate_HILICPos_Jan25.csv"
d1p.neg.file <- "Raw_Data/Distribution_Data/Particulate/D1_particulate_HILICNeg_Oct24.csv"

#G4 Depth Profiles
g4depth.pos.file <- "Raw_Data/Distribution_Data/Particulate/G4_depthprofiles_particulate_HILICPos_Jan25.csv"
g4depth.neg.file <- "Raw_Data/Distribution_Data/Particulate/G4_depthprofiles_particulate_HILICNeg_Oct24.csv"

#G4 Depth Profiles
g3depth.pos.file <- "Raw_Data/Distribution_Data/Particulate/G3_depthprofiles_particulate_HILICPos_Jan25.csv"
g3depth.neg.file <- "Raw_Data/Distribution_Data/Particulate/G3_depthprofiles_particulate_HILICNeg_Jan25.csv"

#PERIFIX
perifix.pos.file <- "Raw_Data/Distribution_Data/Particulate/PERIFIX_particulate_HILICPos_Jan25.csv"
perifix.neg.file <- "Raw_Data/Distribution_Data/Particulate/PERIFIX_particulate_HILICNeg_Oct24.csv"


#_________________Dissolved_____________
#G3 Dissolved
g3d.file <- "Raw_Data/Distribution_Data/Dissolved/G3_dissolved_HILICPos_Jan25.csv"

#G4 Dissolved
g4d.file.0 <-  "Raw_Data/Distribution_Data/Dissolved/G4_dissolved_HILICPos_Jan25_pooled.csv"
g4d.file.1 <-  "Raw_Data/Distribution_Data/Dissolved/G4_dissolved_HILICPos_Jan25_file1.csv"
g4d.file.2 <-  "Raw_Data/Distribution_Data/Dissolved/G4_dissolved_HILICPos_Jan25_file2.csv"

#D1 Dissolved
d1d.file <- "Raw_Data/Distribution_Data/Dissolved/D1_dissolved_HILICPos_Jan25.csv"

#Kinetics Experiments Dissolved
ked.file <-  "Raw_Data/Distribution_Data/Dissolved/KinExp_dissolved_HILICPos_Jan25.csv"


#_________________Culture_____________
bact.pos.file <- "Raw_Data/Culture_Data/culture_bacteria_particulate_HILICPos_Jan25.csv"
bact.neg.file <- "Raw_Data/Culture_Data/culture_bacteria_particulate_HILICNeg_Oct24.csv"

cyano.pos.file <- "Raw_Data/Culture_Data/culture_cyanos_particulate_HILICPos_Jan25.csv"
cyano.neg.file <- "Raw_Data/Culture_Data/culture_cyanos_particulate_HILICNeg_Oct24.csv"

diatom.pos.file <- "Raw_Data/Culture_Data/culture_diatom_particulate_HILICPos_Jan25.csv"
diatom.neg.file <- "Raw_Data/Culture_Data/culture_diatom_particulate_HILICNeg_Oct24.csv"

dinogreen.pos.file <- "Raw_Data/Culture_Data/culture_dinosgreens_particulate_HILICPos_Jan25.csv"
dinogreen.neg.file <- "Raw_Data/Culture_Data/culture_dinosgreens_particulate_HILICNeg_Oct24.csv"

hapto.pos.file <- "Raw_Data/Culture_Data/culture_haptophyte_particulate_HILICPos_Jan25.csv"
hapto.neg.file <- "Raw_Data/Culture_Data/culture_haptophyte_particulate_HILICNeg_Oct24.csv"


#_________G2 Size Fraction Data
g2.pos.file <- "Raw_Data/Size_Fraction_Data/G2_particulate_HILICPos_Jan25.csv"
g2.neg.file <- "Raw_Data/Size_Fraction_Data/G2_particulate_HILICNeg_Oct24.csv"




# Organize Particulate Data -----------------------------------------------

#Organize G3-particulate
g3p.pos <- sky_read(g3p.pos.file) %>%
  select(Rep, Compound, Area) %>%
  mutate(Cruise = "KM1906")

g3p.neg <- sky_read(g3p.neg.file) %>%
  select(Rep, Compound, Area) %>%
  mutate(Cruise = "KM1906")

#Organize G4-Particulate
g4p.pos <- sky_read(g4p.pos.file) %>% 
  select(Rep, Compound, Area) %>%
  mutate(Cruise = "TN397")

g4p.neg <- sky_read(g4p.neg.file) %>% 
  select(Rep, Compound, Area) %>%
  mutate(Cruise = "TN397")

#Organize D1-Particulate
d1p.pos <- sky_read(d1p.pos.file) %>% 
  select(Rep, Compound, Area) %>%
  mutate(Cruise = "RC078")

d1p.neg <- sky_read(d1p.neg.file) %>% 
  select(Rep, Compound, Area) %>%
  mutate(Cruise = "RC078")

#Organize G4 Depth Profiles
g4.depthprofile.pos <- sky_read(g4depth.pos.file) %>% 
  select(Rep, Compound, Area) %>%
  mutate(Cruise = "G4_DepthProfiles")

g4.depthprofile.neg <- sky_read(g4depth.neg.file) %>% 
  select(Rep, Compound, Area) %>%
  mutate(Cruise = "G4_DepthProfiles")

#Organize G3 Depth Profiles
g3.depthprofile.pos <- sky_read(g3depth.pos.file) %>% 
  select(Rep, Compound, Area) %>%
  mutate(Cruise = "G3_DepthProfiles")

g3.depthprofile.neg <- sky_read(g3depth.neg.file) %>% 
  select(Rep, Compound, Area) %>%
  mutate(Cruise = "G3_DepthProfiles")

#Organize PERIFIX
perifix.pos <- sky_read(perifix.pos.file) %>% 
  select(Rep, Compound, Area) %>%
  mutate(Cruise = "PERIFIX") %>%
  filter(!str_detect(Rep, "241031"))

perifix.neg <- sky_read(perifix.neg.file) %>% 
  select(Rep, Compound, Area) %>%
  mutate(Cruise = "PERIFIX") %>%
  filter(!str_detect(Rep, "241031"))



####Pull together all particulate data:
all.p.dat <- rbind(g3p.pos, g3p.neg, g4p.pos, g4p.neg, d1p.pos, d1p.neg, 
                   g4.depthprofile.pos, g4.depthprofile.neg, g3.depthprofile.pos, 
                   g3.depthprofile.neg, perifix.pos, perifix.neg)

is.p.dat <- all.p.dat %>%
  filter(str_detect(Compound, ", ")) %>%
  filter(!Compound %in% c("Cys-Gly, oxidized"))

#grab just osmolytes
osmo.p.dat <- all.p.dat %>%
  filter(Compound %in% 
               c("beta-Alaninebetaine", "Glycine betaine", "Proline betaine", "Homarine",
                 "Trigonelline", "Betonicine", "Dimethylsulfonioacetate", "Dimethylsulfoniopropionate",
                 "Gonyol", "Trimethylamine N-oxide", "(3-Carboxypropyl)trimethylammonium", "2-O-alpha-D-Glucosylglycerol",
                 "5-Hydroxyectoine", "Carnitine", "Ectoine", "Hydroxyisoleucine", "L-Alanine", "L-Aspartic acid", "L-Glutamic acid",
                 "L-Glutamine", "L-Proline", "L-Serine", "L-Threonine", "Sarcosine", "Threonine Betaine (tentative)",
                 "Homoserine Betaine (tentative)", "L-Cysteic acid", "(R)-2,3-Dihydroxypropane-1-sulfonate", "Isethionic acid",
                 "Sucrose", "Taurine", "Trehalose", "Arsenobetaine", "Glycine", "L-Isoleucine", "L-Asparagine", "L-Lysine",
                 "beta-Glutamic acid", "L-Methionine", "L-Tyrosine", "L-Arginine"))

#make sample list:
p.smp.list <- osmo.p.dat %>%
  select(Rep, Cruise) %>%
  unique() %>%
  mutate(Injec_vol = case_when(str_detect(Rep, "Half") ~ 1,
                             TRUE ~ 2))

####Write cleaned up IS and betaine data to .csvs 
write_csv(is.p.dat, file = "Intermediates/particulate_IS_data_raw.csv")

write_csv(osmo.p.dat, file = "Intermediates/particulate_osmo_data_raw.csv")

write_csv(p.smp.list, file = "Intermediates/particulate_smp_list.csv")




# Organize Dissolved Data ---------------------------------------------

#Organize G3-dissolved
g3d <- sky_read(g3d.file) %>%
  select(Rep, Compound, Area) %>%
  mutate(Cruise = "KM1906")

#Organize G4-dissolved
g4d.0 <- sky_read(g4d.file.0) %>%
  select(Rep, Compound, Area) %>%
  mutate(Cruise = "TN397")

g4d.1 <- sky_read(g4d.file.1) %>%
  select(Rep, Compound, Area) %>%
  mutate(Cruise = "TN397")  %>%
  filter(!str_detect(Rep, "_Std_"))

g4d.2 <- sky_read(g4d.file.2) %>%
  select(Rep, Compound, Area) %>%
  mutate(Cruise = "TN397")%>%
  filter(!str_detect(Rep, "_Std_"))

g4d <- rbind(g4d.0, g4d.1, g4d.2)


###Organize D1 Dissolved
d1d <- sky_read(d1d.file) %>%
  select(Rep, Compound, Area) %>%
  mutate(Cruise = "RC078")


#Organize Kinetics Experiment Dissolved
ked <- sky_read(ked.file) %>%
  select(Rep, Compound, Area) %>%
  mutate(Cruise = "KinExp") 

####Pull together all dissolved data:
all.d.dat <- rbind(g3d, g4d, ked, d1d)

###Grab Just IS:
is.d.dat <- all.d.dat %>%
  filter(str_detect(Compound, ", ")) %>%
  filter(!Compound %in% c("Cys-Gly, oxidized"))


###Grab Just osmolytes: 
osmo.d.dat <- all.d.dat %>%
  filter(Compound %in% 
           c("beta-Alaninebetaine", "Glycine betaine", "Proline betaine", "Homarine",
             "Trigonelline", "Betonicine", "Dimethylsulfonioacetate", "Dimethylsulfoniopropionate",
             "Gonyol", "Trimethylamine N-oxide", "(3-Carboxypropyl)trimethylammonium", "2-O-alpha-D-Glucosylglycerol",
             "5-Hydroxyectoine", "Carnitine", "Ectoine", "Hydroxyisoleucine", "L-Alanine", "L-Aspartic acid", "L-Glutamic acid",
             "L-Glutamine", "L-Proline", "L-Serine", "L-Threonine", "Sarcosine", "Threonine Betaine (tentative)",
             "Homoserine Betaine (tentative)", "L-Cysteic acid", "(R)-2,3-Dihydroxypropane-1-sulfonate", "Isethionic acid",
             "Sucrose", "Taurine", "Trehalose", "Arsenobetaine", "Glycine", "L-Isoleucine", "L-Asparagine", "L-Lysine",
             "beta-Glutamic acid", "L-Methionine", "L-Tyrosine", "L-Arginine"))

#make sample list:
d.smp.list <- osmo.d.dat %>%
  select(Rep, Cruise) %>%
  unique() %>%
  mutate(Injec_vol = case_when(str_detect(Rep, "Half") ~ 1,
                             TRUE ~ 2))

####Write cleaned up IS and betaine data to .csvs 
write_csv(is.d.dat, file = "Intermediates/dissolved_IS_data_raw.csv")

write_csv(osmo.d.dat, file = "Intermediates/dissolved_osmo_data_raw.csv")

write_csv(d.smp.list, file = "Intermediates/dissolved_smp_list.csv")



# Organize Culture Data _____________________________________

#Organize data from each batch

#bacteria
bact.dat.pos <- sky_read(bact.pos.file) %>%
  select(Rep, Compound, Area) %>%
  mutate(Batch = "Bacteria")

bact.dat.neg <- sky_read(bact.neg.file) %>%
  select(Rep, Compound, Area) %>%
  mutate(Batch = "Bacteria")

#cyanos
cyano.dat.pos <- sky_read(cyano.pos.file) %>%
  select(Rep, Compound, Area) %>%
  mutate(Batch = "Cyano") %>%
  mutate(Rep = str_replace(Rep, "211102", "211221"))

cyano.dat.neg <- sky_read(cyano.neg.file) %>%
  select(Rep, Compound, Area) %>%
  mutate(Batch = "Cyano") %>%
  mutate(Rep = str_replace(Rep, "211102", "211221"))

#Diatoms
diatom.dat.pos <- sky_read(diatom.pos.file) %>%
  select(Rep, Compound, Area) %>%
  mutate(Batch = "Diatom")

diatom.dat.neg <- sky_read(diatom.neg.file) %>%
  select(Rep, Compound, Area) %>%
  mutate(Batch = "Diatom") %>%
  mutate(Rep = str_replace(Rep, "211102", "211215"))

#Dinos and Greens
dinogreen.dat.pos <- sky_read(dinogreen.pos.file) %>%
  select(Rep, Compound, Area) %>%
  mutate(Batch = "Dino_Green") 

dinogreen.dat.neg <- sky_read(dinogreen.neg.file) %>%
  select(Rep, Compound, Area) %>%
  mutate(Batch = "Dino_Green") %>%
  mutate(Rep = str_replace(Rep, "218", "211218")) %>%
  mutate(Rep = str_replace(Rep, "102", "211218"))

#Haptophytes
hapto.dat.pos <- sky_read(hapto.pos.file) %>%
  select(Rep, Compound, Area) %>%
  mutate(Batch = "Haptophyte")

hapto.dat.neg <- sky_read(hapto.neg.file) %>%
  select(Rep, Compound, Area) %>%
  mutate(Batch = "Haptophyte")

####Pull together all datasets together:
all.c.dat <- rbind(bact.dat.pos, bact.dat.neg, cyano.dat.pos, cyano.dat.neg,
                   diatom.dat.pos, diatom.dat.neg, dinogreen.dat.pos, dinogreen.dat.neg, 
                   hapto.dat.pos, hapto.dat.neg) %>% 
  mutate(Rep = str_remove(Rep, "_pos")) %>%
  mutate(Rep = str_remove(Rep, "_neg"))

###Grab Just IS:
is.c.dat <- all.c.dat %>%
  filter(str_detect(Compound, ", ")) %>%
  filter(!Compound %in% c("Cys-Gly, oxidized"))

###Grab Just osmolytes: 
osmo.c.dat <- all.c.dat %>%
  filter(Compound %in% 
           c("beta-Alaninebetaine", "Glycine betaine", "Proline betaine", "Homarine",
             "Trigonelline", "Betonicine", "Dimethylsulfonioacetate", "Dimethylsulfoniopropionate",
             "Gonyol", "Trimethylamine N-oxide", "(3-Carboxypropyl)trimethylammonium", "2-O-alpha-D-Glucosylglycerol",
             "5-Hydroxyectoine", "Carnitine", "Ectoine", "Hydroxyisoleucine", "L-Alanine", "L-Aspartic acid", "L-Glutamic acid",
             "L-Glutamine", "L-Proline", "L-Serine", "L-Threonine", "Sarcosine", "Threonine Betaine (tentative)",
             "Homoserine Betaine (tentative)", "L-Cysteic acid", "(R)-2,3-Dihydroxypropane-1-sulfonate", "Isethionic acid",
             "Sucrose", "Taurine", "Trehalose", "Arsenobetaine", "Glycine", "L-Isoleucine", "L-Asparagine", "L-Lysine",
             "beta-Glutamic acid", "L-Methionine", "L-Tyrosine", "L-Arginine"))

#make sample list:
c.smp.list <- osmo.c.dat %>%
  select(Rep, Batch) %>%
  unique() %>%
  mutate(Injec_vol = case_when(str_detect(Rep, "Half") ~ 1,
                               TRUE ~ 2))

####Write cleaned up IS and betaine data to .csvs 
write_csv(is.c.dat, file = "Intermediates/culture_IS_data_raw.csv")

write_csv(osmo.c.dat, file = "Intermediates/culture_osmo_data_raw.csv")

write_csv(c.smp.list, file = "Intermediates/culture_smp_list.csv")



#________Organize Size Fractionated G2 Data

#organize data:
g2.dat.pos <- sky_read(g2.pos.file) %>%
  select(Rep, Compound, Area) %>%
  mutate(Batch = "G2")

g2.dat.neg <- sky_read(g2.neg.file) %>%
  select(Rep, Compound, Area) %>%
  mutate(Batch = "G2")

#combine
g2.dat <- rbind(g2.dat.pos, g2.dat.neg)

###Grab Just IS:
is.g2.dat <- g2.dat %>%
  filter(str_detect(Compound, ", ")) %>%
  filter(!Compound %in% c("Cys-Gly, oxidized"))

###Grab just betaines: (no TMAO because instrument not set to detect low masses)
osmo.g2.dat <- g2.dat %>%
  filter(Compound %in% 
           c("beta-Alaninebetaine", "Glycine betaine", "Proline betaine", "Homarine",
             "Trigonelline", "Betonicine", "Dimethylsulfonioacetate", "Dimethylsulfoniopropionate",
             "Gonyol", "Trimethylamine N-oxide", "(3-Carboxypropyl)trimethylammonium", "2-O-alpha-D-Glucosylglycerol",
             "5-Hydroxyectoine", "Carnitine", "Ectoine", "Hydroxyisoleucine", "L-Alanine", "L-Aspartic acid", "L-Glutamic acid",
             "L-Glutamine", "L-Proline", "L-Serine", "L-Threonine", "Sarcosine", "Threonine Betaine (tentative)",
             "Homoserine Betaine (tentative)", "L-Cysteic acid", "(R)-2,3-Dihydroxypropane-1-sulfonate", "Isethionic acid",
             "Sucrose", "Taurine", "Trehalose", "Arsenobetaine", "Glycine", "L-Isoleucine", "L-Asparagine", "L-Lysine",
             "beta-Glutamic acid", "L-Methionine", "L-Tyrosine", "L-Arginine"))

#make sample list:
g2.smp.list <- osmo.g2.dat %>%
  select(Rep, Batch) %>%
  unique() %>%
  mutate(Injec_vol = case_when(str_detect(Rep, "Half") ~ 1,
                               TRUE ~ 2))

####Write cleaned up IS and betaine data to .csvs 
write_csv(is.g2.dat, file = "Intermediates/G2_IS_data_raw.csv")

write_csv(osmo.g2.dat, file = "Intermediates/G2_osmo_data_raw.csv")

write_csv(g2.smp.list, file = "Intermediates/G2_smp_list.csv")















