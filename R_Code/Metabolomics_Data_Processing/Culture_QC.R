
#load packages
library(tidyverse)

#define inputs
raw.file <- "Intermediates/culture_osmo_data_raw.csv"
cult.info.file <- "Meta_Data/Culture_Meta_Data.csv"
blk.key.file <- "Meta_Data/Culture_Blk_Match_Key.csv"

#Define QC thresholds
min.area.threshold <- 40000 #minimum peak area threshold 
min.blk.ratio <- 3  # minimum signal above average blk that a sample must have to pass QC
min_perc_replicates <- 1 #minimum percentage of replicates that a compound must pass QC in to be considered present


#####
raw.dat <- read_csv(raw.file)
cult.info <- read_csv(cult.info.file) %>%
  mutate(Samp_ID = str_remove(Samp_ID, "_pos"))
blk.key <- read_csv(blk.key.file)


#tidy and organize data
dat.sml <- raw.dat %>%
  filter(!str_detect(Rep, "Std")) %>%
  filter(!str_detect(Rep, "Blk")) %>%
  filter(!str_detect(Rep, "Poo")) %>%
  filter(!str_detect(Rep, "SAR11")) %>%
  rename("Samp_ID" = Rep) %>%
  left_join(., cult.info) %>%
  rename("SampID" = Samp_ID) %>%
  mutate(Area = replace_na(Area, 0)) 


###calculate blank threshold 
dat.blks <- raw.dat %>%
  rename("SampID" = Rep) %>%
  filter(str_detect(SampID, "Blk")) %>%
  rename("Blk_ID" = SampID) %>%
  left_join(., blk.key %>%
              mutate(Blk_ID = str_remove(Blk_ID, "_pos")))  %>%
  group_by(Compound, Organism) %>%
  mutate(Area = replace_na(Area, 0)) %>%
  mutate(mean.blk = mean(Area),
         blk.threshold = 3*mean.blk) %>%
  select(Compound, Organism, blk.threshold, mean.blk) %>%
  rename(blk.ave = mean.blk) %>%
  filter(!is.na(Organism)) %>%
  unique() %>%
  ungroup()


###Apply minimum peak area thresholds and blk_qc thresholds
dat.qc <- dat.sml %>%
  left_join(., dat.blks) %>%
  mutate(min.area.flag = case_when(Area <= min.area.threshold ~ "Flag",
                                   TRUE ~ NA),
         blk.flag = case_when(Area <= blk.threshold ~ "Flag",
                              TRUE ~ NA)) %>%
  mutate(detected = case_when(is.na(min.area.flag) & is.na(blk.flag) ~ "Yes",
                              TRUE ~ "No")) %>%
  mutate(detected.numeric = case_when(detected == "Yes" ~ 1,
                                      TRUE ~ 0))

###make sure a compound is detected in all replicats of a culture
dat.qc.org <- dat.qc %>%
  group_by(Compound, Organism) %>%
  mutate(rep.count = n(),
         detect.count = sum(detected.numeric),
         detected.all.reps = case_when(detect.count == rep.count ~ "Yes",
                                       TRUE ~ "No"))

#simplify and organize data for export
dat.qc.export <- dat.qc.org %>%
  select(Batch, Type, Organism, SampID, Compound, detected.all.reps) %>%
  rename(Detected = detected.all.reps)


# write to a csv:
write_csv(dat.qc.export, file = "Intermediates/Culture_QCdat.csv")

##Create SampID Key:
#samp.id.key <- dat.sml %>%
#  mutate(Samp_info = str_remove(SampID, "220111_Smp_"),
#         Samp_info = str_remove(Samp_info, "211221_Smp_"),
#         Samp_info = str_remove(Samp_info, "211215_Smp_"),
#         Samp_info = str_remove(Samp_info, "211218_Smp_"),
##         Samp_info = str_remove(Samp_info, "220104_Smp_"),
#         Samp_info = str_remove(Samp_info, "_pos"),
#         Samp_info = str_remove(Samp_info, "-1")) %>%
# #  separate(Samp_info, c("org_code", "replicate"), sep = "_") 
# 
# #Identify number of reps per organism a compound fails qc in
# dat.qc.rep <- dat.qc %>%
#   group_by(Compound, Organism) %>%
#   mutate(rep.count = n()) %>%
#   ungroup() %>%
#   filter(min.area.flag == "Flag" | blk.flag == "Flag") %>%
#   group_by(Compound, Organism) %>%
#   mutate(min.area.flag.count = sum(!is.na(min.area.flag))) %>%
#   mutate(blk.flag.count = sum(!is.na(blk.flag))) %>%
#   mutate(min.area.flag.ratio = min.area.flag.count/rep.count,
#          blk.flag.ratio = blk.flag.count/rep.count) %>%
#   mutate(smp.remove = case_when(min.area.flag.ratio <= min_perc_replicates | 
#                                   blk.flag.ratio <= min_perc_replicates ~ "Smp_Remove",
#                                 TRUE ~ NA)) %>%
#   mutate(blk.imputed.value = blk.ave/2) %>%
#   ungroup() 
# 
# 
# ###Create QCed dataset where samples not passing QC are removed
# dat.qc.remove <- dat.sml %>%
#   left_join(., dat.qc.rep) %>%
#   filter(is.na(smp.remove)) %>%
#   select(SampID, Organism, Batch, Compound, Area, min.area.flag, blk.flag)
# 
# ###Create blk_imputed_value dataset where samples not passing QC have values equal to 1/2 the blank imputed
# dat.qc.impute <- dat.sml %>%
#   left_join(., dat.qc.rep) %>%
#   mutate(Area = case_when(blk.flag == "Flag" | min.area.flag == "Flag" ~ blk.imputed.value,
#                           TRUE ~ Area))  %>%
#   select(SampID, Batch, Organism, Compound, Area, min.area.flag, blk.flag, smp.remove)
# 
# ###Export both QCed datasets:
# write_csv(dat.qc.remove, file = "Intermediates/Culture_osmo_QCdat_samplesremoved.csv")
# write_csv(dat.qc.impute, file = "Intermediates/Culture_osmo_QCdat_blanksimputed.csv")
# 



















































































