

#
#This script....


#####
library(readr)
library(tidyverse)

#Inputs

#Stds and RFs and RFratios
stds.file <- "Meta_Data/Ingalls_Lab_Standards_03172023.csv" 
rf.file <- "Intermediates/Culture_Stds_RFs_RFratios.csv"

#IS
hilic.is.file <- "Intermediates/culture_final_IS_peaklist.csv"

### area dat
hilic.file <- "Intermediates/Culture_HILIC_Pos_BMISed_dat.csv"
hilic.file.notnorm <- "Intermediates/culture_osmo_data_raw.csv"


###Sample volume dat:
vol.file <- "Intermediates/All_culture_metadata.csv"



#Quantify values using standards 
rf.dat <- read_csv(rf.file)


##Pull in and combine volume filtered data:
#WILL DO THIS WHEN I ACTUALLY HAVE VOLUME DATA FOR EVERYTHING


### combine metab area data with vol.filt.data and 
## calculate concentration in Vial using normal RF and RFratio approach 
vial.quant.dat <- read_csv(hilic.file) %>%
  rename("Name" = MF) %>%
  filter(!str_detect(.$SampID, "Poo")) %>%
  filter(!str_detect(.$SampID, "Std")) %>% 
 # left_join(., vol.filt.dat) %>%
 # filter(!is.na(Vol_L)) %>%
  left_join(., rf.dat) %>%
  mutate(uM.in.vial.ave = Adjusted_Area/RF/RFratio,
         uM.in.vial.max = Adjusted_Area/RFmin/RFratio,
         uM.in.vial.min = Adjusted_Area/RFmax/RFratio) %>%
  unite(c(Batch, Name), remove = FALSE, col = "batch_comp")



#Quantify compounds with matched IS
hilic.is.dat <- read_csv(hilic.is.file) %>%
  rename("IS" = MF,
         "IS.area" = Area) %>%
  unique() 


#get internal standard concentration values 
is.std <- read_csv(stds.file) %>%
  mutate(Compound_Name = case_when(Compound_Name == "Sucrose, 13C12" ~ "Sucrose, 13C",
                   Compound_Name == "Trehalose, 13C12" ~ "Trehalose, 13C",
                   TRUE ~ Compound_Name)) %>%
  filter(Compound_Name %in% hilic.is.dat$IS) %>%
  select(Compound_Name, Concentration_uM) %>%
  rename("IS" = Compound_Name,
         "IS.Conc.uM" = Concentration_uM) 


####Pull in unnormalized hilic data 
hilic.notnorm.dat <- read_csv(hilic.file.notnorm) %>%
  rename("SampID" = Rep,
         "Name" = Compound) %>%
  select(Name, SampID, Area) %>%
  unique() %>%
  full_join(., hilic.is.dat)  %>%
  mutate(match.name = str_extract(.$IS, Name)) %>%
  filter(!is.na(match.name)) %>%
  filter(!str_detect(.$SampID, "Poo")) %>%
  filter(!str_detect(.$SampID, "Std")) %>%
  filter(!str_detect(.$SampID, "SAR11")) %>%
  mutate(Area = replace_na(Area, 0))



#Quantify using internal standard
vial.is.quant.dat <- left_join(hilic.notnorm.dat, is.std) %>%
  mutate(uM.in.vial.ave = Area*IS.Conc.uM/IS.area) %>%
  unite(c(Batch, Name), remove = FALSE, col = "batch_comp")


smp.quant.dat.all <- vial.quant.dat %>%
  select(Name, SampID, Batch, uM.in.vial.ave, batch_comp) %>%
  filter(!batch_comp %in% vial.is.quant.dat$batch_comp) %>%
  rbind(., vial.is.quant.dat %>% 
          select(Name, SampID, Batch, uM.in.vial.ave, batch_comp)) %>%
  unique()



#load in volume data
vol.dat <- read_csv(vol.file) %>%
  select(SampID, Vol_mL)


#Calculate nM C and N per sample
std.formula <- read_csv(stds.file) %>%
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

#calculate nM, nM-C, nM-N, and nM-S in sample for all compounds
samp.quant.dat <- left_join(smp.quant.dat.all, std.formula) %>%
  mutate(C = case_when(Name %in% c("Homoserine Betaine (tentative)", "Threonine Betaine (tentative)") ~ 7,
                       TRUE ~ C),
         N = case_when(Name %in% c("Homoserine Betaine (tentative)", "Threonine Betaine (tentative)") ~ 1,
                       TRUE ~ N)) %>%
  unique() %>%
  left_join(., vol.dat) %>%
  mutate(uM_in_samp = uM.in.vial.ave*400*(1/1E6)*(1/Vol_mL)*1000) %>%
  mutate(uM_C_smp = uM_in_samp*C,
         uM_N_smp = uM_in_samp*N,
         uM_S_smp = uM_in_samp*S) %>%
  select(Name, SampID, Batch, uM.in.vial.ave, uM_in_samp, uM_C_smp, uM_N_smp, uM_S_smp)


write_csv(samp.quant.dat, file = "Intermediates/Culture_Quant_Output.csv")

