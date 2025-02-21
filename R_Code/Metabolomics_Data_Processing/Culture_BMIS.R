
#packages
library(tidyverse)


##Functions:
source("R_Code/Functions.R")

#Define all your inputs here
is.file <- "Intermediates/culture_IS_data_raw.csv"
hilic.file <- "Intermediates/culture_osmo_data_raw.csv"
smp.key.file <-  "Intermediates/culture_smp_list.csv"
#Cutoffs 

#Minimum RSD improvement for BMIS to be accepted and applied 
cut.off <- 0.2

#Minimum RSD of pooled samples above which BMIS-normalization will be applied (ie. if RSDpoo is above cut.off2, BMIS will be applied)
cut.off2 <- 0.1


#Import sample key----
samp.key <- read_csv(smp.key.file) 


# Import HILIC data and remove samples with no/wrong internal standards added
hilic.dat <- read_csv(hilic.file) %>%
  rename("MF" = Compound,
         "SampID" = Rep) %>%
  select(MF, SampID, Area, Batch) %>%
  filter(!str_detect(.$SampID, "Std")) %>%
  filter(!str_detect(.$SampID, "DDA")) %>%
 # filter(!str_detect(.$SampID, "Blk")) %>%
  filter(!str_detect(.$SampID, "SAR11")) %>%
  mutate(Area = replace_na(Area, 0))



#Import and clean up the Internal standard data---- and remove sample with no internal standards added
is.dat.full <- read_csv(is.file) %>%
  rename(SampID = `Rep`,
         MF = `Compound`) %>% 
  select(SampID, MF, Area, Batch) %>%
  filter(!str_detect(.$SampID, "Std")) %>%
  filter(!str_detect(.$SampID, "DDA")) %>%
 # filter(!str_detect(.$SampID, "Blk")) %>%
  filter(!str_detect(.$SampID, "SAR11")) %>%
  filter(!SampID == "221006_Smp_S7_C1_D1_A")




#Read in sample key, add in injec_volume data from sample key----
samp.key.2 <- samp.key %>%
  filter(Rep %in% is.dat.full$SampID) %>%
  select(Rep, Injec_vol, Batch) %>%
  filter(!is.na(Injec_vol))%>%
  mutate(MF = "Inj_vol",
         Area = Injec_vol,
         SampID = Rep) %>%
  select(SampID, MF, Area, Batch) %>%
  mutate(Area = case_when(str_detect(.$SampID, "Half") ~ 1,
                          TRUE ~ 2))

is.dat.full.with.samp <- rbind(is.dat.full, samp.key.2) 



# Look at extraction replication of the Internal Standards by Batch----
# Make list of IS to use for BMIS and make separate list of IS to use for quant


## Bacteria
bacteria.is.dat.full.with.samp <- is.dat.full.with.samp %>%
  filter(Batch == "Bacteria")

bacteria.IS_inspectPlot <- ggplot(bacteria.is.dat.full.with.samp, aes(x=SampID, y=Area)) + 
  geom_bar(stat="identity") + 
  facet_wrap(.~MF, scales="free_y")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.5, size = 5), 
        axis.text.y = element_text(size = 10),
        legend.position = "top",
        strip.text = element_text(size = 10))+
  ggtitle("IS Raw Areas")
bacteria.IS_inspectPlot

bacteria.is.dat.full.with.samp.edited.BMIS <- bacteria.is.dat.full.with.samp %>%
  filter(!str_detect(MF, "AMP")) %>%
  filter(!str_detect(MF, "GMP")) %>%
  filter(!str_detect(MF, "Isethionic")) %>%
  filter(!str_detect(MF, "Succinic")) %>%
  filter(!str_detect(MF, "Sucrose")) %>%
  filter(!str_detect(MF, "Sulfoacetic")) %>%
  filter(!str_detect(MF, "Guanine")) %>%
  filter(!str_detect(MF, "Taurine")) %>%
  filter(!str_detect(MF, "Trehalose")) 

bacteria.is.dat.full.with.samp.edited.quant <- bacteria.is.dat.full.with.samp %>%
  filter(!str_detect(MF, "AMP")) %>%
  filter(!str_detect(MF, "GMP")) %>%
  filter(!str_detect(MF, "Isethionic")) %>%
  filter(!str_detect(MF, "Succinic")) %>%
  filter(!str_detect(MF, "Sucrose")) %>%
  filter(!str_detect(MF, "Sulfoacetic")) %>%
  filter(!str_detect(MF, "Guanine")) %>%
  filter(!str_detect(MF, "Taurine")) %>%
  filter(!str_detect(MF, "Trehalose")) 

## Cyano
cyano.is.dat.full.with.samp <- is.dat.full.with.samp %>%
  filter(Batch == "Cyano")

cyano.IS_inspectPlot <- ggplot(cyano.is.dat.full.with.samp, aes(x=SampID, y=Area)) + 
  geom_bar(stat="identity") + 
  facet_wrap(.~MF, scales="free_y")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.5, size = 5), 
        axis.text.y = element_text(size = 10),
        legend.position = "top",
        strip.text = element_text(size = 10))+
  ggtitle("IS Raw Areas")
cyano.IS_inspectPlot

cyano.is.dat.full.with.samp.edited.BMIS <- cyano.is.dat.full.with.samp %>%
  filter(!str_detect(MF, "AMP")) %>%
  filter(!str_detect(MF, "Arsenobetaine")) %>%
  filter(!str_detect(MF, "Glycine betaine")) %>%
  filter(!str_detect(MF, "GMP")) %>%
  filter(!str_detect(MF, "Guanine")) %>%
  filter(!str_detect(MF, "Homarine")) %>%
  filter(!str_detect(MF, "Isethionic")) %>%
  filter(!str_detect(MF, "Succinic")) %>%
  filter(!str_detect(MF, "Sucrose")) %>%
  filter(!str_detect(MF, "Sulfoacetic")) %>%
  filter(!str_detect(MF, "Taurine")) %>%
  filter(!str_detect(MF, "Trehalose")) 

cyano.is.dat.full.with.samp.edited.quant <- cyano.is.dat.full.with.samp %>%
  filter(!str_detect(MF, "AMP")) %>%
  filter(!str_detect(MF, "GMP")) %>%
  filter(!str_detect(MF, "Succinic")) %>%
  filter(!str_detect(MF, "Sucrose")) %>%
  filter(!str_detect(MF, "Trehalose")) 

## Diatom
diatom.is.dat.full.with.samp <- is.dat.full.with.samp %>%
  filter(Batch == "Diatom")

diatom.IS_inspectPlot <- ggplot(diatom.is.dat.full.with.samp, aes(x=SampID, y=Area)) + 
  geom_bar(stat="identity") + 
  facet_wrap(.~MF, scales="free_y")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.5, size = 5), 
        axis.text.y = element_text(size = 10),
        legend.position = "top",
        strip.text = element_text(size = 10))+
  ggtitle("IS Raw Areas")
diatom.IS_inspectPlot

diatom.is.dat.full.with.samp.edited.BMIS <- diatom.is.dat.full.with.samp %>%
  filter(!str_detect(MF, "sulfolactate")) %>%
  filter(!str_detect(MF, "AMP")) %>%
  filter(!str_detect(MF, "Glycine betaine")) %>%
  filter(!str_detect(MF, "Isethionic")) %>%
  filter(!str_detect(MF, "Sucrose")) %>%
  filter(!str_detect(MF, "Sulfoacetic")) %>%
  filter(!str_detect(MF, "Taurine")) %>%
  filter(!str_detect(MF, "Trehalose")) 

diatom.is.dat.full.with.samp.edited.quant <- diatom.is.dat.full.with.samp %>%
  filter(!str_detect(MF, "sulfolactate")) %>%
  filter(!str_detect(MF, "AMP")) %>%
 # filter(!str_detect(MF, "Glycine betaine")) %>%
 # filter(!str_detect(MF, "Isethionic")) %>%
  filter(!str_detect(MF, "Sucrose")) %>%
  filter(!str_detect(MF, "Sulfoacetic")) %>%
 # filter(!str_detect(MF, "Taurine")) %>%
  filter(!str_detect(MF, "Trehalose")) 


## Dino_Green
dinogreen.is.dat.full.with.samp <- is.dat.full.with.samp %>%
  filter(Batch == "Dino_Green")

dinogreen.IS_inspectPlot <- ggplot(dinogreen.is.dat.full.with.samp, aes(x=SampID, y=Area)) + 
  geom_bar(stat="identity") + 
  facet_wrap(.~MF, scales="free_y")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.5, size = 5), 
        axis.text.y = element_text(size = 10),
        legend.position = "top",
        strip.text = element_text(size = 10))+
  ggtitle("IS Raw Areas")
dinogreen.IS_inspectPlot

dinogreen.is.dat.full.with.samp.edited.BMIS <- dinogreen.is.dat.full.with.samp %>%
  filter(!str_detect(MF, "Alanine")) %>%
  filter(!str_detect(MF, "Guanine")) %>%
  filter(!str_detect(MF, "Isethionic")) 

dinogreen.is.dat.full.with.samp.edited.quant <- dinogreen.is.dat.full.with.samp 


## Haptophyte
hapto.is.dat.full.with.samp <- is.dat.full.with.samp %>%
  filter(Batch == "Haptophyte")

hapto.IS_inspectPlot <- ggplot(hapto.is.dat.full.with.samp, aes(x=SampID, y=Area)) + 
  geom_bar(stat="identity") + 
  facet_wrap(.~MF, scales="free_y")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.5, size = 5), 
        axis.text.y = element_text(size = 10),
        legend.position = "top",
        strip.text = element_text(size = 10))+
  ggtitle("IS Raw Areas")
hapto.IS_inspectPlot

hapto.is.dat.full.with.samp.edited.BMIS <- hapto.is.dat.full.with.samp %>%
  filter(!str_detect(MF, "AMP")) %>%
  filter(!str_detect(MF, "Histidine")) %>%
  filter(!str_detect(MF, "Glycine betaine")) %>%
  filter(!str_detect(MF, "GMP")) %>%
  filter(!str_detect(MF, "Guanine")) %>%
  filter(!str_detect(MF, "Isethionic")) %>%
  filter(!str_detect(MF, "Succinic")) %>%
  filter(!str_detect(MF, "Sucrose")) %>%
  filter(!str_detect(MF, "Sulfoacetic")) %>%
  filter(!str_detect(MF, "Taurine")) %>%
  filter(!str_detect(MF, "Trehalose")) 


hapto.is.dat.full.with.samp.edited.quant <- hapto.is.dat.full.with.samp %>%
  filter(!str_detect(MF, "AMP")) %>%
#  filter(!str_detect(MF, "Histidine")) %>%
#  filter(!str_detect(MF, "Glycine betaine")) %>%
  filter(!str_detect(MF, "GMP")) %>%
#  filter(!str_detect(MF, "Guanine")) %>%
#  filter(!str_detect(MF, "Isethionic")) %>%
  filter(!str_detect(MF, "Succinic")) %>%
  filter(!str_detect(MF, "Sucrose")) %>%
 # filter(!str_detect(MF, "Sulfoacetic")) %>%
 # filter(!str_detect(MF, "Taurine")) %>%
  filter(!str_detect(MF, "Trehalose")) 


####______XXX____________________________________

###Combine all batches together for BMIS:
is.dat.full.with.samp.edited.BMIS <- rbind(
  bacteria.is.dat.full.with.samp.edited.BMIS,
  cyano.is.dat.full.with.samp.edited.BMIS,
  diatom.is.dat.full.with.samp.edited.BMIS,
  dinogreen.is.dat.full.with.samp.edited.BMIS,
  hapto.is.dat.full.with.samp.edited.BMIS
)


###Combine all batches together for Quant:
is.dat.full.with.samp.edited.quant <- rbind(
  bacteria.is.dat.full.with.samp.edited.quant,
  cyano.is.dat.full.with.samp.edited.quant,
  diatom.is.dat.full.with.samp.edited.quant,
  dinogreen.is.dat.full.with.samp.edited.quant,
  hapto.is.dat.full.with.samp.edited.quant
)

#export peak list of good IS:
write_csv(is.dat.full.with.samp.edited.quant, file = "Intermediates/culture_final_IS_peaklist.csv")







####
hilic.long <- hilic.dat

#Calculate mean values for each IS----
is.means <- is.dat.full.with.samp.edited.BMIS %>% 
  left_join(samp.key %>%
              mutate(SampID = Rep) %>% 
              select(SampID), by = "SampID") %>%
  group_by(MF, Batch) %>%
  summarise(ave = mean(as.numeric(Area), na.rm = TRUE)) %>%
  mutate(ave = ifelse(MF == "Inj_vol", 1, ave))
#REMOVED SAMPLE.TYPE



####
######## Make IS Key
is.info <- is.dat.full.with.samp.edited.BMIS %>%
  select(MF, Batch) %>%
  rename("IS" = MF) %>%
  unique() %>%
  mutate(match.key = "x")

IS.dat.key.check <- read_csv(hilic.file) %>%
  rename("MF" = Compound,
         "SampID" = Rep) %>%
  select(MF) %>%
  unique() %>%
  mutate(match.key = "x") %>%
  full_join(., is.info) %>%
  select(-match.key) %>%
  mutate(match.name = str_extract(.$IS, MF)) %>%
  filter(!is.na(match.name))

IS.key <- IS.dat.key.check %>%
  select(MF, Batch, IS) %>%
  rename("MIS" = IS)





#Normalize to each internal Standard----
binded <- rbind(is.dat.full.with.samp.edited.BMIS, hilic.long) %>%
  left_join(samp.key %>%
              mutate(SampID = Rep) %>%
              select(SampID), by = "SampID") 
split.dat <- list()
for (i in 1:length(unique(is.dat.full.with.samp.edited.BMIS$MF))){
  split.dat[[i]] <- binded %>% mutate(MIS = unique(is.dat.full.with.samp.edited.BMIS$MF)[i]) %>%
    left_join(is.dat.full.with.samp.edited.BMIS %>% 
                rename(MIS = MF, IS_Area = Area) %>% 
                select(MIS, SampID, IS_Area)) %>%
    left_join(is.means %>% 
                rename(MIS = MF)) %>%
    mutate(Adjusted_Area = Area/IS_Area*ave)
}
area.norm <- do.call(rbind, split.dat) %>% select(-IS_Area, -ave)


#Break Up the Names (Name structure must be:  Date_type_ID_replicate_anythingextraOK)----
area.norm.2 <- area.norm %>% separate(SampID, 
                                      c("runDate",
                                        "type","samp","replicate"),"_", remove = FALSE)

#Find the B-MIS for each MassFeature----
#Look only the Pooled samples, to get a lowest RSD of the pooled possible (RSD_ofPoo), 
#then choose which IS reduces the RSD the most (Poo.Picked.IS) 
poodat <- area.norm.2 %>%
  filter(type == "Poo")%>% 
  group_by(samp, MF, MIS, Batch) %>%
  summarise(RSD_ofPoo_IND = sd(Adjusted_Area, 
                               na.rm = TRUE)/mean(Adjusted_Area+1, na.rm = TRUE)) %>%
  mutate(RSD_ofPoo_IND = ifelse(RSD_ofPoo_IND == "NaN", NA, RSD_ofPoo_IND)) %>%
  group_by(MF, MIS, Batch) %>%
  summarise(RSD_ofPoo =  mean(RSD_ofPoo_IND, na.rm = TRUE)) 

poodat.lowest.RSD <- poodat %>%
  ungroup() %>%
  filter(!RSD_ofPoo == "NaN") %>%
  group_by(MF, Batch) %>%
  mutate(min.rsd = min(RSD_ofPoo)) %>%
  filter(min.rsd == RSD_ofPoo) %>%
  ungroup() %>%
  rename("Poo.Picked.IS" = MIS) %>%
  select(MF, Batch, Poo.Picked.IS)

poodat <- left_join(poodat, poodat.lowest.RSD)

#Get the starting point of the RSD (Orig_RSD), calculate the change in the RSD, say if the MIS is acceptable----
poodat.2 <- left_join(poodat, poodat %>%
                        group_by(MF, Batch) %>%
                        filter(MIS == "Inj_vol" ) %>%
                        mutate(Orig_RSD = RSD_ofPoo) %>%
                        select(-RSD_ofPoo, -MIS)) %>%
  mutate(del_RSD = (Orig_RSD - RSD_ofPoo)) %>%
  mutate(percentChange = del_RSD/Orig_RSD) %>%
  mutate(accept_MIS = (percentChange > cut.off & Orig_RSD > cut.off2)) 


###
Matched.IS.dat <- left_join(IS.key, poodat.2) %>%
  mutate(FinalBMIS = MIS,
         FinalRSD = RSD_ofPoo) %>%
  unite("batch_compound", c(Batch, MF), remove = FALSE) %>%
  filter(!is.na(accept_MIS))


#Change the BMIS to "Inj_vol" if the BMIS is not an acceptable -----
#Adds a column that has the BMIS, not just Poo.picked.IS
#Changes the finalBMIS to inject_volume if its no good
# 
#Also force compounds with MIS to pick their MIS to preserve consistency with quantification
fixedpoodat <- poodat.2 %>%
  filter(MIS == Poo.Picked.IS)%>%
  mutate(FinalBMIS = ifelse((accept_MIS == "FALSE"), "Inj_vol", Poo.Picked.IS), 
         FinalRSD = RSD_ofPoo) %>%
  unite("batch_compound", c(Batch, MF), remove = FALSE) %>%
  filter(!batch_compound %in% Matched.IS.dat$batch_compound) %>%
  rbind(., Matched.IS.dat) #%>%
 # unite("batch_compound", c(Batch, MF), remove = FALSE)

fixedpoodat.2 <- fixedpoodat %>%
  ungroup() %>%
  select(MF, Batch, FinalBMIS) 

###
newpoodat <- poodat.2 %>% 
  ungroup() %>%
  left_join(., fixedpoodat.2) %>%
  filter(MIS == FinalBMIS) %>%
  mutate(FinalRSD = RSD_ofPoo)
report.text <- newpoodat %>% filter(FinalBMIS != "Inj_vol")
QuickReport <- paste("% of MFs that picked a BMIS", 
                     length(report.text$MF) / length(newpoodat$MF), 
                     "RSD improvement cutoff", cut.off,
                     "RSD minimum cutoff", cut.off2,
                     sep = " ")
QuickReport


#Get all the data back - and keep only the MF-MIS match set for the BMIS----
#Add a column to the longdat that has important information from the FullDat_fixed, 
#then only return data that is normalized via B-MIS normalization
BMIS_normalizedData <- newpoodat %>% select(MF, FinalBMIS, Orig_RSD, Batch, FinalRSD) %>%
  left_join(area.norm.2 %>% rename(FinalBMIS = MIS)) %>% unique() %>%
  filter(!MF %in% is.dat.full.with.samp.edited.BMIS$MF)

QuickReport

#Fix Inj_vol adjusted areas 
BMIS_normalizedData.2 <- BMIS_normalizedData %>%
  mutate(Adjusted_Area = case_when(FinalBMIS == "Inj_vol" ~ 2*Adjusted_Area,
                                   !FinalBMIS == "Inj_vol" ~ Adjusted_Area)) 
write_csv(BMIS_normalizedData.2, "Intermediates/Culture_HILIC_Pos_BMISed_dat.csv")

#BMISlist <- list(QuickReport, BMIS_normalizedData.2)

#Removes all intermediate variables :)
rm(list=setdiff(ls(), c("BMISlist")))

