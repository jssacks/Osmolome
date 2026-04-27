



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
p.betaine.dat <- dat.region %>%
  filter(Cruise %in% c( "KM1906", "TN397", "RC078")) %>%
  select(Compound, compound.name.figure, order, class, Part.SampID, Cruise, Part.Conc.nM, Region, Part.detected) %>% 
  unique() %>%
  filter(class == "Betaine") %>%
  group_by(Part.SampID) %>%
  filter(Part.detected == "Yes") %>%
  mutate(Betaines.detected = n()) %>%
  filter(Betaines.detected == 8) %>%
  mutate(Betaine.MaxNorm.Val = Part.Conc.nM/max(Part.Conc.nM),
         Betaine.Tot.Conc = sum(Part.Conc.nM),
         Betaine.Rank = rank(1/Part.Conc.nM),
         count = n()) %>%
  ungroup() %>%
  group_by(Compound) %>%
  mutate(Mean.Betaine.Rank = mean(Betaine.Rank),
         SD.Betaine.Rank = sd(Betaine.Rank),
         Mean.BetaineMaxNorm.Val = mean(Betaine.MaxNorm.Val),
         SD.BetaineMaxNorm.Val = sd(Betaine.MaxNorm.Val)) %>%
  ungroup() %>%
  mutate(Betaine.Redfield.Ratio = Mean.BetaineMaxNorm.Val/Mean.BetaineMaxNorm.Val[Compound == "Glycine betaine"])




#organize dissolved surface data:
d.betaine.dat <- dat.region %>%
  filter(Cruise %in% c("TN397", "RC078")) %>%
  select(Compound, compound.name.figure, class, Diss.SampID, Cruise, Diss.Conc.nM, Diss.LOD.nM, order, Region, Diss.detected) %>% 
  unique() %>%
  filter(!is.na(Diss.SampID)) %>%
  mutate(Diss.Conc.nM = case_when(Diss.Conc.nM < Diss.LOD.nM ~ Diss.LOD.nM,
                                  TRUE ~ Diss.Conc.nM)) %>%
  filter(class == "Betaine") %>%
  group_by(Diss.SampID) %>%
  filter(Diss.detected == "Yes") %>%
  mutate(Betaines.detected = n()) %>%
  filter(Betaines.detected == 8) %>%
  mutate(Betaine.MaxNorm.Val = Diss.Conc.nM/max(Diss.Conc.nM),
         Betaine.Tot.Conc = sum(Diss.Conc.nM),
         Betaine.Rank = rank(1/Diss.Conc.nM),
         count = n()) %>%
  ungroup() %>%
  group_by(Compound) %>%
  mutate(Mean.Betaine.Rank = mean(Betaine.Rank),
         SD.Betaine.Rank = sd(Betaine.Rank),
         Mean.BetaineMaxNorm.Val = mean(Betaine.MaxNorm.Val),
         SD.BetaineMaxNorm.Val = sd(Betaine.MaxNorm.Val)) %>%
  ungroup() %>%
  mutate(Betaine.Redfield.Ratio = Mean.BetaineMaxNorm.Val/Mean.BetaineMaxNorm.Val[Compound == "Glycine betaine"])







###______Calculate Regional correlations in particulate betaine data_____________________

#_________Particulate:_________________________________

#_________Rank:
p.betaine.region.rank.dat <- p.betaine.dat %>%
  group_by(Region, compound.name.figure, order) %>%
  reframe(mean.rank = mean(Betaine.Rank))

  
#run correlation analysis comparing the mean rank of a compound in one region to the
# mean rank of a compound in a second region
p.betaine.region.rank.comparison <- p.betaine.region.rank.dat %>%
  rename(Region.1 = Region,
         mean.rank.1 = mean.rank) %>%
  full_join(p.betaine.region.rank.dat %>%
              rename(Region.2 = Region,
                     mean.rank.2 = mean.rank)) 

#Calculate correlations
p.betaine.region.rank.cor <- p.betaine.region.rank.comparison %>%
  group_by(Region.1, Region.2) %>%
  cor_test(vars = c(mean.rank.1, mean.rank.2)) %>%
  filter(Region.1 < Region.2) 




#_____________Relative abundance:____________________________
p.betaine.region.relabun.dat <- p.betaine.dat %>%
  group_by(Region, compound.name.figure, order) %>%
  reframe(mean.relabun = log10(mean(Betaine.MaxNorm.Val)))

#run correlation analysis comparing the mean relative abundance of a compound in one region to the
# mean relative abundance of a compound in a second region
p.betaine.region.relabun.comparison <- p.betaine.region.relabun.dat %>%
  rename(Region.1 = Region,
         mean.relabun.1 = mean.relabun) %>%
  full_join(p.betaine.region.relabun.dat %>%
              rename(Region.2 = Region,
                     mean.relabun.2 = mean.relabun)) 

#Calculate correlations
p.betaine.relabun.cor <- p.betaine.region.relabun.comparison %>%
  group_by(Region.1, Region.2) %>%
  cor_test(vars = c(mean.relabun.1, mean.relabun.2)) %>%
  filter(Region.1 < Region.2) 







###______Calculate regional correlations in dissolved betaine data_____________________

#_________Dissolved:_________________________________

#_________Rank:
d.betaine.region.rank.dat <- d.betaine.dat %>%
  group_by(Region, compound.name.figure, order) %>%
  reframe(mean.rank = mean(Betaine.Rank))


#run correlation analysis comparing the mean rank of a compound in one region to the
# mean rank of a compound in a second region
d.betaine.region.rank.comparison <- d.betaine.region.rank.dat %>%
  rename(Region.1 = Region,
         mean.rank.1 = mean.rank) %>%
  full_join(d.betaine.region.rank.dat %>%
              rename(Region.2 = Region,
                     mean.rank.2 = mean.rank)) 

#Calculate correlations
d.betaine.region.rank.cor <- d.betaine.region.rank.comparison %>%
  group_by(Region.1, Region.2) %>%
  cor_test(vars = c(mean.rank.1, mean.rank.2)) %>%
  filter(Region.1 < Region.2) 




#_____________Relative abundance:____________________________
d.betaine.region.relabun.dat <- d.betaine.dat %>%
  group_by(Region, compound.name.figure, order) %>%
  reframe(mean.relabun = log10(mean(Betaine.MaxNorm.Val)))

#run correlation analysis comparing the mean relative abundance of a compound in one region to the
# mean relative abundance of a compound in a second region
d.betaine.region.relabun.comparison <- d.betaine.region.relabun.dat %>%
  rename(Region.1 = Region,
         mean.relabun.1 = mean.relabun) %>%
  full_join(d.betaine.region.relabun.dat %>%
              rename(Region.2 = Region,
                     mean.relabun.2 = mean.relabun)) 

#Calculate correlations
d.betaine.relabun.cor <- d.betaine.region.relabun.comparison %>%
  group_by(Region.1, Region.2) %>%
  cor_test(vars = c(mean.relabun.1, mean.relabun.2)) %>%
  filter(Region.1 < Region.2) 





###_________Compare rank abundance to MW_____________________

####Get compound properties
mw.dat <- tibble(
  compound.name.figure = c("GBT", "beta-Alanine betaine", "Homarine", "Trigonelline", "Carnitine", "Proline betaine", "Betonicine", "TMAB"),
  mw = c(117.15, 131.17, 137.14, 137.14, 161.20, 143.18, 159.18, 145.20))



#summarize and combine data:
p.sum <- p.betaine.dat %>%
  select(compound.name.figure, Mean.Betaine.Rank, SD.Betaine.Rank) %>%
  unique() %>%
  rename(P.Rank = Mean.Betaine.Rank,
         P.Rank.SD = SD.Betaine.Rank)

d.sum <- d.betaine.dat %>%
  select(compound.name.figure, Mean.Betaine.Rank, SD.Betaine.Rank) %>%
  unique() %>%
  rename(D.Rank = Mean.Betaine.Rank,
         D.Rank.SD = SD.Betaine.Rank)

sum.comb <- left_join(p.sum, d.sum) %>%
  left_join(., mw.dat) %>%
  left_join(., compound.order)



###Run linear models 
lm.p.mw <- lm(mw~P.Rank, data = sum.comb)
summary(lm.p.mw)

lm.d.mw <- lm(mw~D.Rank, data = sum.comb)
summary(lm.d.mw)



















# 
# 
# 
# 
# 
# ggplot(p.betaine.region.rank.dat, aes(x = mean.rank, y = reorder(compound.name.figure, order))) +
#   geom_point()
# 
# 
#   
#   part.dat %>%
#   group_by(Part.SampID) %>%
#   mutate(comp.rank = rank(1/Part.Conc.nM)) %>%
#   group_by(Region, compound.name.figure) %>%
#   reframe(mean.rank = mean(comp.rank))
# 
# #export
# write_csv(p.region.rank.dat, file = "Intermediates/Part_Region_Mean_Rank.csv")
# 
# #run correlation analysis comparing the mean rank of a compound in one region to the
# # mean rank of a compound in a second region
# p.region.rank.comparison <- p.region.rank.dat %>%
#   rename(Region.1 = Region,
#          mean.rank.1 = mean.rank) %>%
#   full_join(p.region.rank.dat %>%
#               rename(Region.2 = Region,
#                      mean.rank.2 = mean.rank)) 
# 
# 
# p.region.rank.cor <- p.region.rank.comparison %>%
#   group_by(Region.1, Region.2) %>%
#   cor_test(vars = c(mean.rank.1, mean.rank.2))
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #####__Make BRR Plots:
# 
# #organize particulate surface data:
# p.betaine.dat <- dat.region %>%
#   filter(Cruise %in% c( "KM1906", "TN397", "RC078")) %>%
#   select(Compound, compound.name.figure, order, class, Part.SampID, Cruise, Part.Conc.nM, Region) %>% 
#   unique() %>%
#   filter(class == "Betaine") %>%
#   group_by(Part.SampID) %>%
#   mutate(Betaine.Norm.Val = Part.Conc.nM/max(Part.Conc.nM),
#          Betaine.Tot.Conc = sum(Part.Conc.nM),
#          Betaine.Rank = rank(1/Part.Conc.nM),
#          count = n()) %>%
#   ungroup() %>%
#   mutate(Betaine.Norm.Val.2 = Part.Conc.nM/Betaine.Tot.Conc) %>%
#   group_by(Compound) %>%
#   mutate(Mean.Betaine.Rank = mean(Betaine.Rank),
#          SD.Betaine.Rank = sd(Betaine.Rank),
#          Mean.Betaine.Val = mean(Betaine.Norm.Val),
#          SD.Betaine.Val = sd(Betaine.Norm.Val),
#          Betaine.Val.Max = Mean.Betaine.Val + SD.Betaine.Val,
#          Betaine.Val.Min = case_when(Mean.Betaine.Val - SD.Betaine.Val > 0 ~ Mean.Betaine.Val - SD.Betaine.Val,
#                                      Mean.Betaine.Val - SD.Betaine.Val < 0 ~ 0.01)) %>%
#   ungroup() %>%
#   mutate(Betaine.Redfield.Ratio = Mean.Betaine.Val/Mean.Betaine.Val[Compound == "Glycine betaine"])
# 
# 
# 
# #organize dissolved surface data:
# d.betaine.dat <- dat.region %>%
#   filter(Cruise %in% c("TN397", "RC078")) %>%
#   select(Compound, compound.name.figure, class, Diss.SampID, Cruise, Diss.Conc.nM, order, Region) %>% 
#   unique() %>%
#   filter(!is.na(Diss.SampID)) %>%
#   filter(!is.na(Diss.Conc.nM)) %>%
#   filter(class == "Betaine") %>%
#   group_by(Diss.SampID) %>%
#   mutate(Betaine.Norm.Val = Diss.Conc.nM/max(Diss.Conc.nM),
#          Betaine.Tot.Conc = sum(Diss.Conc.nM),
#          Betaine.Rank = rank(1/Diss.Conc.nM),
#          count = n()) %>%
#   ungroup() %>%
#   mutate(Betaine.Norm.Val.2 = Diss.Conc.nM/Betaine.Tot.Conc) %>%
#   group_by(Compound) %>%
#   mutate(Mean.Betaine.Rank = mean(Betaine.Rank),
#          SD.Betaine.Rank = sd(Betaine.Rank),
#          Mean.Betaine.Val = mean(Betaine.Norm.Val),
#          SD.Betaine.Val = sd(Betaine.Norm.Val),
#          Betaine.Val.Max = Mean.Betaine.Val + SD.Betaine.Val,
#          Betaine.Val.Min = case_when(Mean.Betaine.Val - SD.Betaine.Val > 0 ~ Mean.Betaine.Val - SD.Betaine.Val,
#                                      Mean.Betaine.Val - SD.Betaine.Val < 0 ~ 0.01)) %>%
#   ungroup() %>%
#   mutate(Betaine.Redfield.Ratio = Mean.Betaine.Val/Mean.Betaine.Val[Compound == "Glycine betaine"])
# 
