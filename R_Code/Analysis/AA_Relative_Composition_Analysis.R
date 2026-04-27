



library(tidyverse)
library(vegan)
library(viridis)
library(rstatix)
library(ggforce)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")




###define inputs
all.dat.file <- "Intermediates/Enviro_Osmo_Final_Dataset_with_metadata.csv"






#Read in data 
dat <- read_csv(all.dat.file) %>%
  left_join(., compound.order) %>%
  filter(!str_detect(Part.SampID, "Blk")) %>%
  filter(!Parent_ID == "TN397_S11_600_U_C") %>%
  filter(!is.na(compound.name.figure))

#Define regions:
dat.region <- dat %>%
  mutate(Region = case_when(Cruise == "KM1906" & Lat >= 35 ~ "NPTZ",
                            Cruise == "KM1906" & Lat < 35 ~ "NPSG",
                            Cruise == "TN397" & Long > -122 ~ "CUCP",
                            Cruise == "TN397" & Long < -122 & Lat > 10.5 ~ "NPSG",
                            Cruise == "TN397" & Lat < 10.5 & Lat > 4.5 ~ "NPEC",
                            Cruise == "TN397" & Lat < 4.5 ~ "PEDP",
                            Cruise == "RC078" ~ "SS"))





### Calculate BRR Stuff:

#organize particulate surface data:
p.AA.dat <- dat.region %>%
  filter(Cruise %in% c( "KM1906", "TN397", "RC078")) %>%
  select(Compound, compound.name.figure, order, class, Part.SampID, Cruise, Part.Conc.nM, Region) %>% 
  unique() %>%
  filter(class == "AA") %>%
  group_by(Part.SampID) %>%
  mutate(AAs.detected = n()) %>%
  filter(AAs.detected == 12) %>%
  mutate(AA.MaxNorm.Val = Part.Conc.nM/max(Part.Conc.nM),
         AA.Tot.Conc = sum(Part.Conc.nM),
         AA.Rank = rank(1/Part.Conc.nM),
         count = n()) %>%
  ungroup() %>%
  group_by(Compound) %>%
  mutate(Mean.AA.Rank = mean(AA.Rank),
         SD.AA.Rank = sd(AA.Rank),
         Mean.AAMaxNorm.Val = mean(AA.MaxNorm.Val),
         SD.AAMaxNorm.Val = sd(AA.MaxNorm.Val)) %>%
  ungroup() 


#organize dissolved surface data:
d.AA.dat <- dat.region %>%
  filter(Cruise %in% c("TN397", "KM1906", "RC078")) %>%
  select(Compound, compound.name.figure, class, Diss.SampID, Cruise, Diss.Conc.nM, Diss.LOD.nM, order, Region) %>% 
  unique() %>%
  filter(!is.na(Diss.SampID)) %>%
  mutate(Diss.Conc.nM = case_when(Diss.Conc.nM < Diss.LOD.nM ~ Diss.LOD.nM,
                                  TRUE ~ Diss.Conc.nM)) %>%
  filter(class == "AA") %>%
  group_by(Diss.SampID)  %>%
  mutate(AAs.detected = n()) %>%
  filter(AAs.detected == 12) %>%
  mutate(AA.MaxNorm.Val = Diss.Conc.nM/max(Diss.Conc.nM),
         AA.Tot.Conc = sum(Diss.Conc.nM),
         AA.Rank = rank(1/Diss.Conc.nM),
         count = n()) %>%
  ungroup() %>%
  group_by(Compound) %>%
  mutate(Mean.AA.Rank = mean(AA.Rank),
         SD.AA.Rank = sd(AA.Rank),
         Mean.AAMaxNorm.Val = mean(AA.MaxNorm.Val),
         SD.AAMaxNorm.Val = sd(AA.MaxNorm.Val)) %>%
  ungroup() 






###______Calculate Regional correlations in particulate betaine data_____________________

#_________Particulate:_________________________________

#_________Rank:
p.AA.region.rank.dat <- p.AA.dat %>%
  group_by(Region, compound.name.figure, order) %>%
  reframe(mean.rank = mean(AA.Rank))


#run correlation analysis comparing the mean rank of a compound in one region to the
# mean rank of a compound in a second region
p.AA.region.rank.comparison <- p.AA.region.rank.dat %>%
  rename(Region.1 = Region,
         mean.rank.1 = mean.rank) %>%
  full_join(p.AA.region.rank.dat %>%
              rename(Region.2 = Region,
                     mean.rank.2 = mean.rank)) 

#Calculate correlations
p.AA.region.rank.cor <- p.AA.region.rank.comparison %>%
  group_by(Region.1, Region.2) %>%
  cor_test(vars = c(mean.rank.1, mean.rank.2)) %>%
  filter(Region.1 < Region.2) 




#_____________Relative abundance:____________________________
p.AA.region.relabun.dat <- p.AA.dat %>%
  group_by(Region, compound.name.figure, order) %>%
  reframe(mean.relabun = log10(mean(AA.MaxNorm.Val)))

#run correlation analysis comparing the mean relative abundance of a compound in one region to the
# mean relative abundance of a compound in a second region
p.AA.region.relabun.comparison <- p.AA.region.relabun.dat %>%
  rename(Region.1 = Region,
         mean.relabun.1 = mean.relabun) %>%
  full_join(p.AA.region.relabun.dat %>%
              rename(Region.2 = Region,
                     mean.relabun.2 = mean.relabun)) 

#Calculate correlations
p.AA.relabun.cor <- p.AA.region.relabun.comparison %>%
  group_by(Region.1, Region.2) %>%
  cor_test(vars = c(mean.relabun.1, mean.relabun.2)) %>%
  filter(Region.1 < Region.2) 







###______Calculate regional correlations in dissolved betaine data_____________________

#_________Dissolved:_________________________________

#_________Rank:
d.AA.region.rank.dat <- d.AA.dat %>%
  group_by(Region, compound.name.figure, order) %>%
  reframe(mean.rank = mean(AA.Rank))


#run correlation analysis comparing the mean rank of a compound in one region to the
# mean rank of a compound in a second region
d.AA.region.rank.comparison <- d.AA.region.rank.dat %>%
  rename(Region.1 = Region,
         mean.rank.1 = mean.rank) %>%
  full_join(d.AA.region.rank.dat %>%
              rename(Region.2 = Region,
                     mean.rank.2 = mean.rank)) 

#Calculate correlations
d.AA.region.rank.cor <- d.AA.region.rank.comparison %>%
  group_by(Region.1, Region.2) %>%
  cor_test(vars = c(mean.rank.1, mean.rank.2)) %>%
  filter(Region.1 < Region.2) 




#_____________Relative abundance:____________________________
d.AA.region.relabun.dat <- d.AA.dat %>%
  group_by(Region, compound.name.figure, order) %>%
  reframe(mean.relabun = log10(mean(AA.MaxNorm.Val)))

#run correlation analysis comparing the mean relative abundance of a compound in one region to the
# mean relative abundance of a compound in a second region
d.AA.region.relabun.comparison <- d.AA.region.relabun.dat %>%
  rename(Region.1 = Region,
         mean.relabun.1 = mean.relabun) %>%
  full_join(d.AA.region.relabun.dat %>%
              rename(Region.2 = Region,
                     mean.relabun.2 = mean.relabun)) 

#Calculate correlations
d.AA.relabun.cor <- d.AA.region.relabun.comparison %>%
  group_by(Region.1, Region.2) %>%
  cor_test(vars = c(mean.relabun.1, mean.relabun.2)) %>%
  filter(Region.1 < Region.2) 





###_________Compare rank abundance to MW_____________________

####Get compound properties
mw.dat <- tibble(
  compound.name.figure = c("Glutamic acid", "Aspartic acid", "Alanine", "Threonine", "beta-Alanine", "Proline", "(Iso)leucine", "Hydroxyisoleucine",
                           "Sarcosine", "Glutamine", "Asparagine", "beta-Glutamic acid"),
  mw = c(147.13, 133.10, 119.12, 89.09, 89.09, 115.13, 131.17, 147.17, 89.09, 146.14, 132.12, 147.13))

# 



#summarize and combine data:
p.sum <- p.AA.dat %>%
  select(compound.name.figure, Mean.AA.Rank, SD.AA.Rank) %>%
  unique() %>%
  rename(P.Rank = Mean.AA.Rank,
         P.Rank.SD = SD.AA.Rank)

d.sum <- d.AA.dat %>%
  select(compound.name.figure, Mean.AA.Rank, SD.AA.Rank) %>%
  unique() %>%
  rename(D.Rank = Mean.AA.Rank,
         D.Rank.SD = SD.AA.Rank)

sum.comb <- left_join(p.sum, d.sum) %>%
  left_join(., mw.dat) %>%
  left_join(., compound.order)



###Run linear models 
lm.p.mw <- lm(mw~P.Rank, data = sum.comb)
summary(lm.p.mw)

lm.d.mw <- lm(mw~D.Rank, data = sum.comb)
summary(lm.d.mw)



