



library(tidyverse)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")

##________Define inputs:

#particulate
part.file <- "Intermediates/Particualte_Final_Quant_QCed.csv"

#dissolved:
diss.file <- "Intermediates/Dissolved_Final_Quant_QCed.csv"
diss.GBT.supp.file <- "Intermediates/KM1906_Predicted_GBT_values.csv"

#metadata
meta.file <- "Intermediates/All_metadata_information.csv"
kinexp.meta.file <- "Meta_Data/KinExp_MetaData.csv"
match.file <- "Intermediates/Particulate_Dissolved_Matching_Key.csv"


###_________load in data and combine with metadata

#laod meta data files and match key
match.key <- read_csv(match.file)
meta.dat <- read_csv(meta.file) %>%
  mutate(Diss.SampID = str_replace(Diss.SampID, "nm", "nM"),
         Diss.SampID = str_replace(Diss.SampID, "2202602", "220602"))

#particulate
part.dat <- read_csv(part.file) %>%
  select(-SampID) %>%
  rename("Part.SampID" = Rep, 
         "Part.Conc.nM" = nM.in.smp,
         "Part.C.nM" = nM_C,
         "Part.N.nM" = nM_N,
         "Part.S.nM" = nM_S,
         Part.Cruise = Cruise) %>%
  mutate(Part.Flag = case_when(!is.na(min.area.flag) | !is.na(blk.ratio.flag) ~ "Flag",
                                 TRUE ~ NA)) %>%
  select(Part.SampID, Part.Cruise, Compound, Part.Conc.nM, Part.C.nM, Part.N.nM, Part.S.nM, Part.Flag)
  
  


#organize predicted GBT values for KM1906
diss.gbt.pred <- read_csv(diss.GBT.supp.file) %>%
  select(-SampID) %>%
  rename("Diss.SampID" = Rep) %>%
  select(Diss.SampID, replicate, Cruise, GBT_nM_pred) %>%
  rename(Diss.Conc.nM.adj = GBT_nM_pred) %>%
  mutate(Compound = "Glycine betaine",
         smp.remove = NA,
         Diss.Nmol.C.adj = Diss.Conc.nM.adj*5,
         Diss.Nmol.N.adj = Diss.Conc.nM.adj, 
         Diss.Nmol.S.adj = NA,
         LOD.nM.adj = NA,
         LOD.Flag.2 = NA)

#dissolved data + add in predicted GBT values
diss.dat <- read_csv(diss.file) %>%
  select(-SampID) %>%
  rename("Diss.SampID" = Rep) %>%
  mutate(Remove = case_when(Compound == "Glycine betaine" & Cruise == "KM1906" ~ "Yes",
                            TRUE ~ "No")) %>%
  filter(!Remove == "Yes") %>%
  select(-Remove) %>%
  rbind(., diss.gbt.pred) %>%
  rename(Diss.Conc.nM.adj = Diss.Conc.nM.adj,
         Diss.C.nM.adj = Diss.Nmol.C.adj,
         Diss.N.nM.adj = Diss.Nmol.N.adj,
         Diss.S.nM.adj = Diss.Nmol.S.adj,
         Diss.LOD.nM.adj = LOD.nM.adj,
         Diss.Cruise = Cruise) %>%
  mutate(Diss.Flag = case_when(!is.na(smp.remove) | !is.na(LOD.Flag.2) ~ "Flag",
                               TRUE ~ NA)) %>%
  select(Diss.SampID, Diss.Cruise, Compound, Diss.Conc.nM.adj, Diss.C.nM.adj, Diss.N.nM.adj, Diss.S.nM.adj, Diss.LOD.nM.adj, Diss.Flag)




###Combine Datasets:
all.dat <- full_join(part.dat, match.key) %>%
  full_join(., diss.dat) %>%
  mutate(Cruise = case_when(!is.na(Part.Cruise) ~ Part.Cruise,
                            TRUE ~ Diss.Cruise)) %>%
  select(-Part.Cruise, -Diss.Cruise) %>% 
  left_join(., meta.dat %>% select(-Diss.SampID) %>% filter(!is.na(Part.SampID))) 

diss.only.dat <- all.dat %>% 
  filter(is.na(Part.SampID)) %>%
  select(-Lat, -Long, -Local_Date, -Local_Time, -depth_m, -station, -Treatment) %>%
  left_join(., meta.dat %>% filter(is.na(Part.SampID)) %>% select(-Part.SampID, -Cruise))

all.dat.final <- all.dat %>%
  filter(!is.na(Part.SampID)) %>%
  rbind(., diss.only.dat) %>%
  filter(Compound %in% compound.order$Compound) %>%
  left_join(., compound.order) %>%
  filter(!Compound == "Threonine Betaine (tentative)") %>%
  filter(!Compound == "Homoserine Betaine (tentative)") %>%
  mutate(Total.Conc.nM = Part.Conc.nM + Diss.Conc.nM.adj,
         Total.C.nM = Part.C.nM + Diss.C.nM.adj,
         Total.N.nM = Part.N.nM + Diss.N.nM.adj,
         Total.S.nM = Part.S.nM + Diss.S.nM.adj,
         Tot.Flag = case_when(!is.na(Part.Flag) | !is.na(Diss.Flag) ~ "Flag",
                              TRUE ~ NA)) %>%
  mutate(Region = as.factor(case_when(Lat > 45 ~ "PS",
                                      Lat > 36 & Lat < 45 ~ "NPTZ",
                                      Lat < 7 ~ "Equator",
                                      Cruise == "TN397" & Lat > 29 ~ "CC",
                                      TRUE ~ "NPSG"))) %>% 
  mutate(Region = fct_relevel(Region, c("CC", "Equator", "NPSG", "NPTZ", "PS"))) %>%
  select(Part.SampID, Diss.SampID, Cruise, Compound, compound.name.figure, order, class, Part.Conc.nM, Part.C.nM, Part.N.nM, Part.S.nM,
         Diss.Conc.nM.adj, Diss.Conc.nM.adj, Diss.N.nM.adj, Diss.S.nM.adj, Total.Conc.nM, Total.C.nM, Total.N.nM, Total.S.nM, Part.Flag, Diss.Flag, Tot.Flag, Diss.LOD.nM.adj,
         Lat, Long, depth_m, Local_Date, Local_Time, station, Region, Treatment)



write_csv(all.dat.final, file = "Intermediates/Combined_Final_Osmolyte_Dataset.csv")








# 
# 
# 
# ###________Organize Datasets for PCA Analysis:
# 
# ####Particulate:
# dat.p <- left_join(part.dat, meta.dat) %>%
#   filter(Compound %in% compound.order$Compound) %>%
#   left_join(., compound.order) %>%
#   filter(!SampID == "220902_Smp_TN397_S11_600_U_C") %>%
#   unique() %>%
#   filter(Cruise %in% c( "KM1906", "TN397", "RC078")) %>%
#   filter(!Compound == "Threonine Betaine (tentative)") %>%
#   filter(!Compound == "Homoserine Betaine (tentative)") %>%
#   filter(depth_m < 10) %>%
#   mutate(sampID.part = SampID) %>%
#   left_join(., match.key)
# 
# 
# 
# ####Dissolved:
# dat.d <- diss.dat %>%
#   filter(Compound %in% compound.order$Compound) %>%
#   left_join(., compound.order) %>%
#   unique() %>%
#   filter(Cruise %in% c( "KM1906", "TN397", "RC078", "KinExp")) %>%
#   filter(!Compound == "Threonine Betaine (tentative)") %>%
#   filter(!Compound == "Homoserine Betaine (tentative)") %>%
#   mutate(sampID.diss = SampID) %>%
#   left_join(., match.key) %>%
#   left_join(., dat.p %>%
#               select(sampID.diss, Lat, Long, Local_Date, Local_Time, station) %>% unique()) 
# 
# 
# kin.exp.dat.d <- dat.d %>%
#   select(-Lat, -Long, -station) %>%
#   filter(SampID %in% kinexp.meta.dat$SampID) %>%
#   left_join(., kinexp.meta.dat %>% rename(sampID.diss = SampID))
# 
# 
# dat.d.final <- dat.d %>%
#   filter(!sampID.diss %in% kin.exp.dat.d$sampID.diss) %>%
#   rbind(., kin.exp.dat.d)
# 
# ##k
# 
# 
# 
# 
# 
# #Make a combined dataset:
# dat.total <- dat.p %>%
#   select(SampID, Cruise, Compound, min.area.flag, blk.ratio.flag, nM.in.smp, Lat, Long, Local_Date, Local_Time, depth_m, order, class, compound.name.figure, sampID.diss) %>%
#   rename(p.area.flag = min.area.flag,
#          p.blk.flag = blk.ratio.flag,
#          Part.Conc.nM = nM.in.smp) %>%
#   left_join(dat.d %>%
#               select(Cruise, Compound, smp.remove, LOD.Flag.2, Diss.Conc.nM.adj, Lat, Long, Local_Date, Local_Time, order, class, compound.name.figure, sampID.diss) %>%
#               rename(d.qc.flag.1 = smp.remove,
#                      d.qc.flag.2 = LOD.Flag.2))# %>%
#   rbind(dat.d %>% filter(!sampID.diss %in% dat.p$sampID.diss) %>%
#         select(Cruise, Compound, smp.remove, LOD.Flag.2, Diss.Conc.nM.adj, Lat, Long, Local_Date, Local_Time, order, class, compound.name.figure, sampID.diss) %>%
#         rename(d.qc.flag.1 = smp.remove,
#                d.qc.flag.2 = LOD.Flag.2) %>%
#         mutate(SampID = NA, p.area.flag = NA, p.blk.flag = NA, Part.Conc.nM = NA))
# 
# x <- dat.d %>% filter(!sampID.diss %in% dat.p$sampID.diss) %>%
#   select(Cruise, Compound, smp.remove, LOD.Flag.2, Diss.Conc.nM.adj, Lat, Long, Local_Date, Local_Time, order, class, compound.name.figure, sampID.diss) %>%
#   rename(d.qc.flag.1 = smp.remove,
#          d.qc.flag.2 = LOD.Flag.2) %>%
#   mutate(SampID = NA, p.area.flag = NA, p.blk.flag = NA, Part.Conc.nM = NA)
# 
# ###Combined dataset:
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
# #meta data
# samp.meta.dat <- dat.total %>%
#   select(SampID, sampID.diss, Cruise, Lat, Long) %>%
#   unique() %>%
#   mutate(Region = as.factor(case_when(Cruise == "RC078" ~ "PS",
#                             Lat > 36 ~ "NPTZ",
#                             Lat < 7 ~ "Equator",
#                             Cruise == "TN397" & Lat > 29 ~ "CC",
#                             TRUE ~ "NPSG"))) %>% 
#   mutate(Region = fct_relevel(Region, c("CC", "Equator", "NPSG", "NPTZ", "PS")))
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
# #####Run Particulate PCA Analysis
# 
# #Change data into a matrix
# osmo.matrix.p <- dat.p %>%
#   mutate(log10.nM.in.smp = log10(nM.in.smp)) %>%
#   select(SampID, Compound, log10.nM.in.smp) %>%
#   pivot_wider(id_cols = SampID, names_from = Compound, values_from = log10.nM.in.smp) %>%
#   column_to_rownames(var = "SampID")
# 
# 
# 
# 
# ###standardize data and make NMDS plot:
# library(vegan)
# 
# osmo.matrix.stand.p <- decostand(osmo.matrix.p, method = "range", MARGIN = 2)
# 
# #osmo.dist.matrix <- vegdist(osmo.matrix.stand, method = "euclidean")
# 
# osmo.pca.p <- rda(osmo.matrix.p, scale = TRUE)
# print(osmo.pca.p)
# summary(osmo.pca.p)
# pca.comp.p <- data.frame(osmo.pca.p[["CA"]][["v"]])
# pca.samp.p <- data.frame(osmo.pca.p[["CA"]][["u"]])
# 
# pca.samp.plot.p <- pca.samp.p %>%
#   rownames_to_column(var = "SampID") %>%
#   left_join(samp.meta.dat)
# 
# 
# 
# ##To move to Figure Script Later
# region.pca.p <- ggplot(pca.samp.plot.p, aes(x = PC1, y = PC2, shape = Cruise)) +
#   geom_point(aes(fill = Region), stroke = 0.15, size = 2.5, shape = 21) +
#   #  scale_shape_manual(values = c(22, 21, 23)) +
#   scale_fill_manual(values = region.palette) +
#   ylab("PC2 (9%)") +
#   theme_test() +
#   theme(panel.border = element_blank(), axis.line = element_line(), 
#         axis.text.x = element_blank(), axis.title.x = element_blank()) 
# region.pca.p
# 
# 
# 
# 
# 




