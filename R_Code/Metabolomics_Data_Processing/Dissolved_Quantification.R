
#
#This script....


#####
library(readr)
library(tidyverse)


###Define inputs
norm.dat.file <- "Intermediates/Dissolved_HILIC_Pos_Osmo_BMISed_dat.csv"
rf.file <- "Intermediates/Dissolved_Stds_RFs_RFratios.csv"
EE.file <- "Meta_Data/CXSPE_EEs.csv"
Std.file <- "Meta_Data/Ingalls_Lab_Standards_03172023.csv"
IS.names.file <- "Meta_Data/CXSPE_IS_List_JSS.csv" 
HILIC.raw.dat <- "Intermediates/dissolved_osmo_data_raw.csv"
Blk.LOD.dat <- "Intermediates/Dissolved_Blk_LOD.csv"
IS.raw.file <- "Intermediates/dissolved_final_IS_peaklist.csv"

###HILIC Dat
hilic.dat <- read_csv(norm.dat.file) %>%
  rename("Name" = MF)

###Join HILIC Dat and RFs and RF ratios
hilic.rfs <- read_csv(rf.file) 
dat <- left_join(hilic.dat, hilic.rfs) %>%
  filter(!is.na(RF))

###Calculate environmental concentration using RFs
dat.conc <- dat %>%
  rowwise() %>%
  mutate(RF = as.numeric(RF)) %>%
  mutate(nmol.conc = Adjusted_Area/RF/RFratio*10^-6*400/(40*10^-3)*1000)

###Adjust values using extraction efficiencies and remove compound not extracted by CX-SPE
EE.vals <- read_csv(EE.file) %>%
  rename("Name" = Compound,
         "EE" = `Extraction Efficiency (%)`) %>%
  select(Name, EE) %>%
  rbind(tibble(Name = c("Carnitine", "Homoserine Betaine (tentative)", "Threonine Betaine (tentative)"), EE = c(100, 100, 100))) %>%
  mutate(Name = case_when(Name == "(Iso)leucine" ~ "L-Isoleucine",
                          TRUE ~ Name))

dat.conc.EE <- left_join(dat.conc, EE.vals, by = c("Name")) %>%
  mutate(EE.adjust.conc = nmol.conc/(EE/100)) %>%
  unique()

####Calculate LODs in Concentration Space 
blk.lod.dat <- read_csv(Blk.LOD.dat) %>%
  rename("Name" = MF)
#  mutate(MF = str_replace_all(.$MF, "butyric_acid_Neg", "butyric acid_Neg")) %>%
 # separate(MF, into = c("Compound", "ion_mode"), sep = "_") %>%
#  mutate(z = ifelse(ion_mode == "Neg", -1, 1)) %>%
#  mutate(LOQ.test = Blk.Av + (10 * (Blk.sd/sqrt(15)))) %>%
#  mutate(Compound = str_replace_all(.$Compound, "Isoleucine", "(Iso)leucine")) %>%
#  filter(!Compound == "leucine")

lod.dat.2 <- left_join(blk.lod.dat, hilic.rfs) %>%
  filter(!is.na(RF))


lod.conc <- lod.dat.2 %>%
  rowwise() %>%
  mutate(RF = as.numeric(RF)) %>%
  mutate(lod.nmol.conc = Blk.LD/RF/RFratio*10^-6*400/(40*10^-3)*1000,
         Blk.av.nmol.conc = Blk.Av/RF/RFratio*10^-6*400/(40*10^-3)*1000)
       #  loq.nmol.conc = LOQ.test/RF/RFratio*10^-6*400/(40*10^-3)*1000)

lod.conc.EE <-  left_join(lod.conc, EE.vals) %>%
 # filter(!is.na(Overall.Mean.EE)) %>%
  mutate(EE.adjust.lod = lod.nmol.conc/(EE/100),
         EE.adjust.Blk.Av = Blk.av.nmol.conc/(EE/100)) %>%
  select(Name, Cruise, EE.adjust.lod, EE.adjust.Blk.Av) %>%
  unique()






####Calculate better concentrations using Internal standards
IS_names <- read_csv(IS.names.file) %>%
  rename(Compound = Match.New,
         IS = IS.Name.New) %>%
  mutate(IS = str_replace(IS, "DL-", "L-")) %>%
  select(Compound, IS, Spike.Fraction, Conc.in.vial.uM)
  

#bring in raw IS data and combine with IS names and matching keys
IS.dat.full <- read_csv(IS.raw.file) %>%
  rename(IS = MF,
         IS_Area = Area,
         Rep = SampID) %>%
  mutate(IS = str_replace(IS, "DL-", "L-"))
IS.dat.named <- left_join(IS.dat.full, IS_names)




###_________Spike Before:_______________________________

#select just spike before is data
IS.Spike.Before <- IS.dat.named %>%
  filter(Spike.Fraction == "Spike_Before")

#pull in raw, not normalized data
raw.dat <- read_csv(HILIC.raw.dat) %>%
  select(Cruise, Rep, Compound, Area)

#Combine IS data with raw peak area data and calculate concentrations
SBe.Matched <- left_join(IS.Spike.Before, raw.dat, by = c("Compound", "Rep", "Cruise")) %>%
  filter(!str_detect(.$Rep, "Std")) %>%
  filter(!str_detect(.$Rep, "Blk")) %>%
  filter(!str_detect(.$Rep, "_C_")) %>%
  filter(!str_detect(.$Rep, "PPL")) %>%
  filter(!str_detect(.$Rep, "Poo")) %>%
  filter(!is.na(Area)) %>%
  select(Cruise, Rep, Compound, Area, IS_Area, Conc.in.vial.uM) %>%
  mutate(Nmol.in.vial_IS = Area/IS_Area*Conc.in.vial.uM*1000,
         Nmol.in.Samp_IS = Nmol.in.vial_IS*10^-6*400/(40*10^-3))

####
SBe.add <- SBe.Matched %>%
  rename(EE.adjust.conc = Nmol.in.Samp_IS) %>%
  select(Compound, Rep, EE.adjust.conc)


###_________Spike After:_______________________________

#select just spike before is data
IS.Spike.After <- IS.dat.named %>%
  filter(Spike.Fraction == "Spike_After")

#Combine IS data with raw peak area data and calculate concentrations
SAf.Matched <- left_join(IS.Spike.After, raw.dat, by = c("Compound", "Rep", "Cruise")) %>%
  filter(!str_detect(.$Rep, "Std")) %>%
  filter(!str_detect(.$Rep, "Blk")) %>%
  filter(!str_detect(.$Rep, "_C_")) %>%
  filter(!str_detect(.$Rep, "PPL")) %>%
  filter(!str_detect(.$Rep, "Poo")) %>%
  filter(!is.na(Area)) %>%
  select(Cruise, Rep, Compound, Area, IS_Area, Conc.in.vial.uM) %>%
  mutate(Nmol.in.vial_IS = Area/IS_Area*Conc.in.vial.uM*1000,
         Nmol.in.Samp_IS = Nmol.in.vial_IS*10^-6*400/(40*10^-3)) %>%
  left_join(., EE.vals %>% rename("Compound" = Name)) %>%
  mutate(EE.adjust.conc = Nmol.in.Samp_IS*(EE/100))

####select just final pieces for comining with other datasets
SAf.add <- SAf.Matched %>%
  select(Compound, Rep, EE.adjust.conc)




#______________combine SAf and SBe IS
IS.adjus.dat <- rbind(SBe.add, SAf.add) %>%
  separate(Rep, 
           c("runDate",
             "type","samp","replicate"),"_", remove = FALSE)






#####____Recalculate LODs based on IS___________________

LOD.Matched <- left_join(IS.Spike.Before, raw.dat, by = c("Compound", "Rep", "Cruise")) %>%
  filter(!str_detect(.$Rep, "Std")) %>%
  filter(str_detect(.$Rep, "Blk")) %>%
  filter(!str_detect(.$Rep, "MQBlk_")) %>%
  filter(!str_detect(.$Rep, "TN397_MQBlk")) %>%
  filter(!str_detect(.$Rep, "FilterBlk")) %>%
  filter(!str_detect(.$Rep, "BottleBlk")) %>%
  filter(!str_detect(.$Rep, "CXC_Blk")) %>%    
 # filter(!str_detect(.$Rep, "Smp")) %>%
  filter(!str_detect(.$Rep, "_C_")) %>%
  filter(!str_detect(.$Rep, "PPL")) %>%
  filter(!str_detect(.$Rep, "Poo")) %>%
  filter(!is.na(Area)) %>%
  select(Cruise, Rep, Compound, Area, IS_Area, Conc.in.vial.uM) %>%
  mutate(Nmol.in.vial_IS = Area/IS_Area*Conc.in.vial.uM*1000,
         Nmol.in.Samp_IS = Nmol.in.vial_IS*10^-6*400/(40*10^-3))



##Count number of blanks and assign student's t-value
Blk.sum <- LOD.Matched %>%
  select(Rep, Cruise) %>%
  unique() %>%
  group_by(Cruise) %>%
  summarize(count = n()) %>%
  mutate(t_val = case_when(count == 12 ~ 1.782,
                           count == 14 ~ 1.761,
                           count == 15 ~ 1.753)) %>%
  ungroup()


###
Blk.ave.dat <- LOD.Matched %>%
  group_by(Compound, Cruise) %>%
  left_join(., Blk.sum) %>%
  summarize(Blk.Av = mean(Nmol.in.Samp_IS),
            Blk.sd = sd(Nmol.in.Samp_IS),
            Blk.max = max(Nmol.in.Samp_IS),
            Blk.LD = Blk.Av + (t_val * (Blk.sd/sqrt(count)))) %>%
  unique() %>%
  rename("Name" = Compound) %>%
  rename("EE.adjust.lod" = Blk.LD,
         "EE.adjust.Blk.Av" = Blk.Av)


############LODs:
LOD.IS.replaced <- lod.conc.EE %>%
  filter(!Name %in% Blk.ave.dat$Name) %>%
  rbind(., Blk.ave.dat %>%
          select(Name, Cruise, EE.adjust.lod, EE.adjust.Blk.Av))










#####______________Perform final calculations and assemble final dataset__________________

#combine IS quantified values with metadata
IS.dat.QCed <- rbind(SBe.add, SAf.add) %>%
  rename("SampID" = Rep,
         "Name" = Compound) %>%
  left_join(., dat.conc.EE %>%
              select(SampID, Cruise) %>%
              unique()) %>%
  unite(c(Cruise, Name), remove = FALSE, col = "cruise_comp") 

####______Add in IS normalized dat__________

#Remove compounds from main dataset that are quantified using IS
no.IS.dat <- dat.conc.EE %>%
  select(Name, SampID, Cruise, EE.adjust.conc)  %>%
  unite(c(Cruise, Name), remove = FALSE, col = "cruise_comp") %>%
  filter(!cruise_comp %in% IS.dat.QCed$cruise_comp) 

##
final.conc.dat <- rbind(no.IS.dat, IS.dat.QCed) %>%
  select(-cruise_comp)

#####Get mols C and N per mol Compound from empirical formula in standard sheet
std.info <- read_csv(Std.file)

std.formula <- read_csv(Std.file) %>%
  select(Compound_Name, Empirical_Formula) %>%
  rename("Name" = Compound_Name) %>%
  unique() %>%
  mutate(C = ifelse(is.na(str_extract(Empirical_Formula, "^C\\d\\d")),
                    str_extract(Empirical_Formula, "^C\\d"), 
                    str_extract(Empirical_Formula, "^C\\d\\d"))) %>%
  mutate(C = as.numeric(str_replace_all(C, "C", ""))) %>%
  mutate(N = ifelse(str_detect(Empirical_Formula, "N\\D"),
                    1, str_extract(Empirical_Formula, "N\\d")))%>%
  mutate(N = as.numeric(str_replace_all(N, "N", ""))) %>%
  mutate(S = case_when(str_detect(Empirical_Formula, "S$") ~ "1",
                       str_detect(Empirical_Formula, "S\\d") ~ str_extract(Empirical_Formula, "S\\d"))) %>%
  mutate(S = as.numeric(str_replace_all(S, "S", "")))

###Conc Data with C and N mol space data
final.dat <- left_join(final.conc.dat, std.formula) %>%
  mutate(C = case_when(Name %in% c("Homoserine Betaine (tentative)", "Threonine Betaine (tentative)") ~ 7,
                       TRUE ~ C),
         N = case_when(Name %in% c("Homoserine Betaine (tentative)", "Threonine Betaine (tentative)") ~ 1,
                       TRUE ~ N)) %>%
  rowwise() %>%
  mutate(Nmol.in.smp = EE.adjust.conc,
         Nmol.C = C*EE.adjust.conc,
         Nmol.N = N*EE.adjust.conc,
         Nmol.S = S*EE.adjust.conc) %>%
  #filter(!sample == "TruePoo") %>%
  unique()

###Write Environmental Concentraions and LOD Concentrations to a csv:
#Enviro.Concs:
write_csv(final.dat, file = "Intermediates/Dissolved_Quantified_Data.csv")

#LODs:
write_csv(LOD.IS.replaced, file = "Intermediates/Dissolved_Blk_LOD_Concentrations.csv")

