


#library
library(tidyverse)


#Define inputs
raw.file <- "Intermediates/particulate_osmo_data_raw.csv"


#Define QC thresholds
min.area.threshold <- 40000 #minimum peak area threshold 
min.blk.ratio <- 3  # minimum signal above average blk that a sample must have to pass QC
min_perc_replicates <- 0.5 #minimum percentage of replicates that a compound must pass QC in to be considered present   


#Load in dataset
raw.dat <- read_csv(raw.file)


###Calculate Blk thresholds
blk.dat <- raw.dat %>%
  mutate(Area = replace_na(Area, 0)) %>%
  mutate(keep.blk = case_when(Cruise == "TN397" & str_detect(Rep, "MQBlk") ~ "Yes",
                              Cruise == "KM1906" & str_detect(Rep, "MMQBlk") ~ "Yes",
                              Cruise == "KM1906" & str_detect(Rep, "_MBlk") ~ "Yes",
                              Cruise == "RC078" & str_detect(Rep, "Blk") ~ "Yes",
                              Cruise == "G4_DepthProfiles" & str_detect(Rep, "Blk") ~ "Yes",
                              Cruise == "G3_DepthProfiles" & str_detect(Rep, "Blk") ~ "Yes",
                             # Cruise == "PERIFIX" & str_detect(Rep, "Blk") ~ "Yes",
                              TRUE ~ NA)) %>%
  filter(keep.blk == "Yes")  %>%
  group_by(Compound) %>%
  mutate(overall.mean.area = mean(Area)) %>%
  group_by(Compound, Cruise, overall.mean.area) %>%
  reframe(blk.sd = sd(Area),
          blk.ave = mean(Area),
          blk.threshold = blk.ave*min.blk.ratio) 

#visualize areas
ggplot(blk.dat, aes(x = blk.ave, y = Cruise)) + 
  geom_point(color = "blue", size = 2.5) +
  geom_vline(aes(xintercept = overall.mean.area)) +
  facet_wrap(.~Compound) +
  scale_x_log10()


###Perform minimum area and blk QC 
qc.dat <- raw.dat %>%
  left_join(., blk.dat %>% select(Compound, Cruise, blk.threshold, blk.ave)) %>%
  filter(!str_detect(Rep, "Std")) %>%
  filter(!str_detect(Rep, "Blk")) %>%
  filter(!str_detect(Rep, "Poo")) %>%
  mutate(min.area.flag = case_when(Area <= min.area.threshold ~ "Flag",
                                   TRUE ~ NA),
         blk.ratio.flag = case_when(Area <= blk.threshold ~ "Flag",
                                    TRUE ~ NA))

###Perform replicated QC

#wrangle and tidy sample IDs into sample names and replicates
samp.id.key <- raw.dat %>%
  filter(!str_detect(Rep, "Std")) %>%
  filter(!str_detect(Rep, "Blk")) %>%
  filter(!str_detect(Rep, "Poo")) %>%
  mutate(SampID = str_remove(Rep, "220628_Smp_"),
         SampID = str_remove(SampID, "220902_Smp_TN397_"),
         SampID = str_remove(SampID, "U_"),
         SampID = str_remove(SampID, "221006_Smp_")) %>%
  mutate(replicate = case_when(str_detect(SampID, "_A$") ~ "A",
                               str_detect(SampID, "_B$") ~ "B",
                               str_detect(SampID, "_C$") ~ "C")) %>%
  mutate(SampID = str_remove(SampID, "_A$"),
         SampID = str_remove(SampID, "_B$"),
         SampID = str_remove(SampID, "_C$")) %>%
  select(-Area)
  
###Combine replicate information with QC information
dat.qc.rep <- left_join(samp.id.key, qc.dat) %>%
  group_by(Compound, Cruise, SampID) %>%
  mutate(rep.count = n()) %>%
  ungroup() %>%
  filter(min.area.flag == "Flag" | blk.ratio.flag == "Flag") %>%
  group_by(Compound, Cruise, SampID) %>%
  mutate(min.area.flag.count = sum(!is.na(min.area.flag))) %>%
  mutate(blk.flag.count = sum(!is.na(blk.ratio.flag))) %>%
  mutate(min.area.flag.ratio = min.area.flag.count/rep.count,
         blk.flag.ratio = blk.flag.count/rep.count) %>%
  mutate(smp.remove = case_when(min.area.flag.ratio >= min_perc_replicates | 
                                  blk.flag.ratio >= min_perc_replicates ~ "Smp_Remove",
                                TRUE ~ NA)) %>%
  mutate(blk.imputed.value = blk.ave/2) %>%
  ungroup()

###Create QCed dataset where samples not passing QC are flagged for removal
dat.qc.remove <- samp.id.key %>%
  left_join(., raw.dat) %>%
  left_join(., dat.qc.rep) %>%
 # filter(is.na(smp.remove)) %>%
  select(Rep, SampID, replicate, Cruise, Compound, Area, min.area.flag, blk.ratio.flag, smp.remove)
  

###Create blk_imputed_value dataset where samples not passing QC have values equal to 1/2 the blank imputed
dat.qc.impute <- raw.dat %>%
  left_join(., samp.id.key) %>%
  left_join(., dat.qc.rep) %>%
  mutate(Area = case_when(blk.ratio.flag == "Flag" | min.area.flag == "Flag" ~ blk.imputed.value,
                          TRUE ~ Area))  %>%
  select(Rep, SampID, replicate, Cruise, Compound, Area, min.area.flag, blk.ratio.flag, smp.remove)


###Export both QCed datasets:
write_csv(dat.qc.remove, file = "Intermediates/Particulate_QCdat_samplesremoved.csv")
write_csv(dat.qc.impute, file = "Intermediates/Particulate_QCdat_blanksimputed.csv")









#visualize removals:
dat.viz <- raw.dat %>%
  mutate(Area = replace_na(Area, 0)) %>%
  mutate(keep.blk = case_when(Cruise == "TN397" & str_detect(Rep, "MQBlk") ~ "Yes",
                              Cruise == "KM1906" & str_detect(Rep, "MMQBlk") ~ "Yes",
                              Cruise == "KM1906" & str_detect(Rep, "_MBlk") ~ "Yes",
                              Cruise == "RC078" & str_detect(Rep, "Blk") ~ "Yes",
                              
                              Cruise == "G3_DepthProfiles" & str_detect(Rep, "Blk") ~ "Yes",
                              Cruise == "G4_DepthProfiles" & str_detect(Rep, "Blk") ~ "Yes")) %>%
  filter(Compound == "L-Serine")

ggplot(dat.viz, aes(x = Rep, fill = keep.blk, y = Area)) +
  geom_col() +
  facet_wrap(.~Cruise, scales = "free")






























