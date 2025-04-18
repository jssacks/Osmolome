






#
#This script....


#####
library(readr)
library(tidyverse)

#Inputs

#Stds and RFs and RFratios
stds.file <- "Meta_Data/Ingalls_Lab_Standards_03172023.csv" 
rf.file <- "Intermediates/Particulate_Stds_RFs_RFratios.csv"

#IS
hilic.is.file <- "Intermediates/G2SF_final_IS_peaklist.csv"

### area dat
hilic.file <- "Intermediates/G2_osmo_BMISed_dat.csv"
hilic.file.notnorm <- "Intermediates/g2_osmo_data_raw.csv"


###Sample volume dat:
vol.file <- "Meta_Data/G2_SF_Sample_MetaData.csv"

#Quantify values using standards 
rf.dat <- read_csv(rf.file)



###
vol.filt.dat <- read_csv(vol.file) %>%
  select(SampID, Vol_L)

### combine metab area data with vol.filt.data and 
## calculate concentration in Vial using normal RF and RFratio approach 
# USE RFs and RFratios from all cruises and take the median of the values
vial.quant.dat <- read_csv(hilic.file)  %>%
  rename("Name" = MF) %>%
  left_join(., vol.filt.dat) %>%
  filter(!is.na(Vol_L)) %>%
  left_join(., rf.dat) %>%
  mutate(umol.in.vial.ave = Adjusted_Area/RF/RFratio,
         umol.in.vial.max = Adjusted_Area/RFmin/RFratio,
         umol.in.vial.min = Adjusted_Area/RFmax/RFratio) %>%
  group_by(Name, SampID, Vol_L) %>%
  reframe(umol.in.vial.ave = mean(umol.in.vial.ave))





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
         "Name" = Compound) %>%
  select(Name, SampID, Area) %>%
  unique() %>%
  full_join(., hilic.is.dat)  %>%
  mutate(match.name = str_extract(.$IS, Name)) %>%
  filter(!is.na(match.name)) %>%
  filter(!str_detect(.$SampID, "Poo")) %>%
  filter(!str_detect(.$SampID, "Std")) %>%
  filter(!str_detect(.$SampID, "Blk")) %>%
  filter(!SampID == "221006_Smp_S7_C1_D1_A")



#Quantify using internal standard
vial.is.quant.dat <- left_join(hilic.notnorm.dat, is.std) %>%
  mutate(umol.in.vial.ave = Area*IS.Conc.uM/IS.area) %>%
  left_join(., vol.filt.dat) %>%
  filter(!is.na(Vol_L))


smp.quant.dat.all <- vial.quant.dat %>%
  select(Name, SampID, umol.in.vial.ave, Vol_L) %>%
  #  mutate(remove = case_when(SampID %in% vial.is.quant.dat$SampID & Name %in% vial.is.quant.dat$Name ~ 1)) %>%
  #  filter(is.na(remove)) %>%
  #  select(-remove) %>%
 # unite(c(Cruise, Name), remove = FALSE, col = "cruise_comp") %>%
  filter(!Name %in% vial.is.quant.dat$Name) %>%
  rbind(., vial.is.quant.dat %>% 
          select(Name, SampID, umol.in.vial.ave, Vol_L)) %>%
  mutate(dilution.factor = 2) %>%
  mutate(nM.in.smp = umol.in.vial.ave*10^-6*400/(Vol_L)*1000*dilution.factor)%>%
  unique()




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
  mutate(N = as.numeric(str_replace_all(N, "N", "")))  %>%
  mutate(S = case_when(str_detect(Empirical_Formula, "S$") ~ "1",
                       str_detect(Empirical_Formula, "S\\d") ~ str_extract(Empirical_Formula, "S\\d"))) %>%
  mutate(S = as.numeric(str_replace_all(S, "S", "")))


samp.quant.dat <- left_join(smp.quant.dat.all, std.formula) %>%
  mutate(C = case_when(Name %in% c("Homoserine Betaine (tentative)", "Threonine Betaine (tentative)") ~ 7,
                       TRUE ~ C),
         N = case_when(Name %in% c("Homoserine Betaine (tentative)", "Threonine Betaine (tentative)") ~ 1,
                       TRUE ~ N)) %>%
  unique() %>%
  mutate(nM_C = nM.in.smp*C,
         nM_N = nM.in.smp*N,
         nM_S = nM.in.smp*S) %>%
  select(Name, SampID, Vol_L, umol.in.vial.ave, nM.in.smp, nM_C, nM_N, nM_S)

write_csv(samp.quant.dat, file = "Intermediates/G2SF_Quant_Output.csv")









