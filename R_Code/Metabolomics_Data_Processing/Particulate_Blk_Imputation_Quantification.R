




library(readr)
library(tidyverse)

#Inputs

#Stds and RFs and RFratios
stds.file <- "Meta_Data/Ingalls_Lab_Standards_03172023.csv" 
rf.file <- "Intermediates/Particulate_Stds_RFs_RFratios.csv"

#IS
hilic.is.file <- "Intermediates/particulate_final_IS_peaklist.csv"

### area dat
hilic.file <- "Intermediates/Particulate_osmo_HILIC_Pos_BMISed_dat.csv"
hilic.file.notnorm <- "Intermediates/particulate_osmo_data_raw.csv"


###Sample volume dat:
vol.file <- "Intermediates/all_vol_filt_data.csv"



#Quantify values using standards 
rf.dat <- read_csv(rf.file)



###
vol.filt.dat <- read_csv(vol.file)


##load in BMISed data:
bmis.dat <- read_csv(hilic.file)


###Pull out blanks and calculate mean blanks for each compound:
blk.dat <- bmis.dat %>%
  mutate(Adjusted_Area = replace_na(Adjusted_Area, 0)) %>%
  rename(Rep = SampID) %>%
  mutate(keep.blk = case_when(Cruise == "TN397" & str_detect(Rep, "MQBlk") ~ "Yes",
                              Cruise == "KM1906" & str_detect(Rep, "MMQBlk") ~ "Yes",
                              Cruise == "KM1906" & str_detect(Rep, "_MBlk1_B") ~ "Yes",
                              Cruise == "RC078" & str_detect(Rep, "Blk") ~ "Yes",
                              Cruise == "G4_DepthProfiles" & str_detect(Rep, "Blk") ~ "Yes",
                              Cruise == "G3_DepthProfiles" & str_detect(Rep, "Blk") ~ "Yes",
                              Cruise == "PERIFIX" & str_detect(Rep, "Blk_Blk") ~ "Yes",
                              Cruise == "RR" & str_detect(Rep, "Blk") ~ "Yes",
                              Cruise == "PNT" & str_detect(Rep, "FltBlk") ~ "Yes",
                              # Cruise == "PERIFIX" & str_detect(Rep, "Blk") ~ "Yes",
                              TRUE ~ NA)) %>%
  filter(keep.blk == "Yes")  %>%
  group_by(MF, Cruise) %>%
  reframe(blk.ave = mean(Adjusted_Area+1)) %>%
  rename(Name = MF)


##Replace each sample with blank area and quantify using external and internal standards to determine imputation value:
vial.quant.dat <- read_csv(hilic.file)  %>%
  rename("Name" = MF) %>%
  select(-Adjusted_Area) %>%
  left_join(., blk.dat %>%
              rename(Adjusted_Area = blk.ave)) %>%
  left_join(., vol.filt.dat) %>%
  filter(!is.na(Vol_L)) %>%
  left_join(., rf.dat) %>%
  mutate(umol.in.vial.ave = Adjusted_Area/RF/RFratio,
         umol.in.vial.max = Adjusted_Area/RFmin/RFratio,
         umol.in.vial.min = Adjusted_Area/RFmax/RFratio) 




#Quantify compounds with matched IS
hilic.is.dat <- read_csv(hilic.is.file) %>%
  rename("IS" = MF,
         "IS.area" = Area) %>%
  unique() %>%
  filter(!SampID == "221006_Smp_S7_C1_D1_A")



#get internal standard concentration values 
is.std <- read_csv(stds.file) %>%
  mutate(Compound_Name = case_when(Compound_Name == "Sucrose, 13C12" ~ "Sucrose, 13C",
                                   Compound_Name == "Trehalose, 13C12" ~ "Trehalose, 13C", 
                                   TRUE ~ Compound_Name)) %>%
  filter(Compound_Name %in% hilic.is.dat$IS) %>%
  select(Compound_Name, Concentration_uM) %>%
  rename("IS" = Compound_Name,
         "IS.Conc.uM" = Concentration_uM) 


####Pull in not-normalized hilic data 
hilic.notnorm.dat <- read_csv(hilic.file.notnorm) %>%
  rename("SampID" = Rep,
         "Name" = Compound) 
  
#quantify blk imputation value using IS
hilic.notnorm.dat <- read_csv(hilic.file)  %>%
  rename("Name" = MF) %>%
  select(Name, Cruise, SampID) %>%
  left_join(., blk.dat %>%
              rename(Area = blk.ave)) %>%
  select(Name, SampID, Area) %>%
  unique() %>%
  full_join(., hilic.is.dat)  %>%
  mutate(match.name = str_extract(.$IS, Name)) %>%
  filter(!is.na(match.name)) %>%
  filter(!str_detect(.$SampID, "Poo")) %>%
  filter(!str_detect(.$SampID, "Std")) %>%
  # filter(!str_detect(.$SampID, "Blk")) %>%
  filter(!SampID == "221006_Smp_S7_C1_D1_A") %>%
  mutate(IS = case_when(IS == "Sucrose, 13C12" ~ "Sucrose, 13C",
                        IS == "Trehalose, 13C12" ~ "Trehalose, 13C", 
                        IS == "L-Proline, 2H7" ~ "DL-Proline, 2H7",
                        TRUE ~ IS)) 

#Quantify using internal standard
vial.is.quant.dat <- left_join(hilic.notnorm.dat, is.std) %>%
  mutate(umol.in.vial.ave = Area*IS.Conc.uM/IS.area) %>%
  left_join(., vol.filt.dat) %>%
  filter(!is.na(Vol_L)) %>%
  unite(c(Cruise, Name), remove = FALSE, col = "cruise_comp")


#combine external and internal standard data and quantify all imputation values:
smp.quant.dat.all <- vial.quant.dat %>%
  select(Name, SampID, Cruise, umol.in.vial.ave, Vol_L) %>%
  #  mutate(remove = case_when(SampID %in% vial.is.quant.dat$SampID & Name %in% vial.is.quant.dat$Name ~ 1)) %>%
  #  filter(is.na(remove)) %>%
  #  select(-remove) %>%
  unite(c(Cruise, Name), remove = FALSE, col = "cruise_comp") %>%
  filter(!cruise_comp %in% vial.is.quant.dat$cruise_comp) %>%
  rbind(., vial.is.quant.dat %>% 
          select(Name, SampID, Cruise, umol.in.vial.ave, Vol_L, cruise_comp)) %>%
  mutate(dilution.factor = case_when(Cruise %in% c("KM1906", "G3_DepthProfiles") ~ 2,
                                     TRUE ~ 1)) %>%
  mutate(nM.in.smp = umol.in.vial.ave*10^-6*400/(Vol_L)*1000*dilution.factor)%>%
  unique()


##organize into final CSV:
dat.imputation.values <- smp.quant.dat.all %>%
  select(Cruise, SampID, Name, nM.in.smp) %>%
  rename(impute_conc_nM = nM.in.smp)

##Export:
write_csv(dat.imputation.values, file = "Intermediates/Particulate_Quantified_BlkAve.csv")










































































