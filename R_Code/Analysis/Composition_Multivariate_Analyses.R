





library(tidyverse)
library(vegan)
library(viridis)
library(rstatix)
library(Hmisc)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")



###define inputs
env.dat.file <- "Intermediates/Enviro_Osmo_Final_Dataset_with_metadata.csv"
peri.dat.file <- "Intermediates/PERIFIX_Osmo_Meta_Dat_101325.csv"





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


#Organize Datasets for PCA Analysis:

#Particulate data: 
part.dat <- dat.region %>%
  mutate(Part.Conc.nM = case_when(Part.detected == "No" ~ Part.Impute.Conc.nM, 
                                  TRUE ~ Part.Conc.nM)) %>%
  select(Part.SampID, Cruise, Lat, Long, Region, compound.name.figure, Part.Conc.nM) %>%
  filter(!is.na(Part.Conc.nM)) %>%
  mutate(log10.part.conc.nM = log10(Part.Conc.nM)) %>%
  unique()

part.matrix <- part.dat %>%
  rename(SampID = Part.SampID) %>%
  select(SampID, compound.name.figure, log10.part.conc.nM) %>%
  pivot_wider(id_cols = SampID, names_from = compound.name.figure, values_from = log10.part.conc.nM) %>%
  column_to_rownames(var = "SampID")

part.metadata <- dat.region %>%
  filter(!is.na(Part.Conc.nM)) %>%
  rename(SampID = Part.SampID) %>%
  select(SampID, Cruise, Lat, Long, Region, chla, poc, pn, sst, sss, N_N, SRP) %>%
  group_by(SampID, Cruise, Lat, Long, Region, chla, poc, pn, sst, N_N, SRP) %>%
  reframe(sss = mean(sss))




#Dissolved data: 
diss.dat <- dat.region %>%
  mutate(Diss.Conc.nM = case_when(Diss.detected == "No" ~ Diss.Impute.Conc.nM, 
                                  TRUE ~ Diss.Conc.nM)) %>%
  select(Diss.SampID, Cruise, Lat, Long, Region, compound.name.figure, Diss.Conc.nM) %>%
  filter(!compound.name.figure == "Arsenobetaine") %>%
  filter(!is.na(Diss.Conc.nM)) %>%
  mutate(log10.diss.conc.nM = log10(Diss.Conc.nM)) %>%
  unique()

diss.matrix <- diss.dat %>%
  rename(SampID = Diss.SampID) %>%
  select(SampID, compound.name.figure, log10.diss.conc.nM) %>%
  pivot_wider(id_cols = SampID, names_from = compound.name.figure, values_from = log10.diss.conc.nM) %>%
  column_to_rownames(var = "SampID")

diss.metadata <- dat.region %>%
  filter(!is.na(Diss.Conc.nM)) %>%
  rename(SampID = Diss.SampID) %>%
  select(SampID, Cruise, Lat, Long, Region, chla, poc, pn, sst, sss, N_N, SRP) %>%
  group_by(SampID, Cruise, Lat, Long, Region, chla, poc, pn, sst, N_N, SRP) %>%
  reframe(sss = mean(sss))










#________________________Run PCA:

###Particulate:
p.matrix.stand <- decostand(part.matrix, method = "range", MARGIN = 2)

p.pca <- rda(p.matrix.stand, scale = TRUE)

print(p.pca)
summary(p.pca)
p.pca.comp <- data.frame(p.pca[["CA"]][["v"]])
p.pca.samp <- data.frame(p.pca[["CA"]][["u"]])


#combine PCA with metadata and reverse PC1 (multiply by -1) to make more interpretable
p.pca.samp.plot <- p.pca.samp %>%
  rownames_to_column(var = "SampID") %>%
  left_join(part.metadata) %>%
  mutate(PC1 = PC1*-1)

p.pca.comp.plot <- p.pca.comp %>%
  rownames_to_column(var = "Compound") 

#export particulate pca
write_csv(p.pca.samp.plot, file = "Intermediates/Part_PCA_with_metadata.csv")







###Dissolved:
d.matrix.stand <- decostand(diss.matrix, method = "range", MARGIN = 2)

d.pca <- rda(d.matrix.stand, scale = TRUE)

print(d.pca)
summary(d.pca)
d.pca.comp <- data.frame(d.pca[["CA"]][["v"]])
d.pca.samp <- data.frame(d.pca[["CA"]][["u"]])



#combine PCA with metadata and reverse PC1 (multiply by -1) to make more interpretable
d.pca.samp.plot <- d.pca.samp %>%
  rownames_to_column(var = "SampID") %>%
  left_join(diss.metadata)%>%
  mutate(PC1 = PC1*-1)

d.pca.comp.plot <- d.pca.comp %>%
  rownames_to_column(var = "Compound") 

#export dissolved pca
write_csv(d.pca.samp.plot, file = "Intermediates/Diss_PCA_with_metadata.csv")







###________PCA Regression________________________

#_______Particulate:

#Organize Particulate Data - PC1 vs everything:
# also log10 scale all variables except for sss and sst
p.pca.reg.dat <- p.pca.samp.plot %>%
  select(Region, SampID, PC1, poc, pn, chla, sst, sss, N_N, SRP) %>%
  pivot_longer(cols = poc:SRP, values_to = "val", names_to = "param") %>%
  mutate(val = case_when(param %in% c("sst", "sss") ~ val,
                         TRUE ~ log10(val))) %>%
  group_by(param) 


p.pca.reg.count <- p.pca.reg.dat %>%
  filter(!is.na(val)) %>%
  group_by(param) %>%
  reframe(count = n()) 

##Run PCA regressions
p.pca.reg.out <- p.pca.reg.dat %>%
  group_by(param) %>%
  cor_test(vars = c(PC1, val)) %>%
  left_join(., p.pca.reg.count)

#export
write_csv(p.pca.reg.out, file = "Intermediates/Part_PCA_Regression_Output.csv")






#____Dissolved

#Organize Data - PC1 vs everything:
# also log10 scale all variables except for sss and sst
d.pca.reg.dat <- d.pca.samp.plot %>%
  select(Region, SampID, PC1, poc, pn, chla, sst, sss, N_N, SRP) %>%
  pivot_longer(cols = poc:SRP, values_to = "val", names_to = "param") %>%
  mutate(val = case_when(param %in% c("sst", "sss") ~ val,
                         TRUE ~ log10(val))) %>%
  group_by(param) 


d.pca.reg.count <- d.pca.reg.dat %>%
  filter(!is.na(val)) %>%
  group_by(param) %>%
  reframe(count = n()) 

##Run PCA regressions
d.pca.reg.out <- d.pca.reg.dat %>%
  group_by(param) %>%
  cor_test(vars = c(PC1, val)) %>%
  left_join(., d.pca.reg.count)


#export
write_csv(d.pca.reg.out, file = "Intermediates/Diss_PCA_Regression_Output.csv")







####______________Correlation of all compounds against eachother and against POC:

#___Particulate 

#compare all compounds to POC
p.comp.poc.cor <- part.dat %>%
  rename(SampID = Part.SampID) %>%
  left_join(., part.metadata) %>%
  select(compound.name.figure, SampID, poc, log10.part.conc.nM) %>%
  mutate(log10.poc = log10(poc)) %>%
  group_by(compound.name.figure) %>%
  cor_test(., vars = c(log10.part.conc.nM, log10.poc))

#export
write_csv(p.comp.poc.cor, file = "Intermediates/Part_POC_Osmo_Regression_Output.csv")






#compare all compounds to each other
p.comp.cor.all.dat <- part.dat %>%
  rename(SampID = Part.SampID) %>%
  select(compound.name.figure, SampID, log10.part.conc.nM) %>%
  pivot_wider(id_cols = SampID, names_from = compound.name.figure, values_from = log10.part.conc.nM) %>%
  column_to_rownames(var = "SampID")

p.comp.cor.all.out <- rcorr(as.matrix(p.comp.cor.all.dat), type = "pearson")

p.r  <- p.comp.cor.all.out$r
p.p  <- p.comp.cor.all.out$P

p.comp.cor.all.out.tidy <- tibble(
  var1 = rownames(p.r)[row(p.r)],
  var2 = colnames(p.r)[col(p.r)],
  cor  = as.vector(p.r),
  pval = as.vector(p.p)) %>%
  filter(var1 != var2) %>%             # remove diagonal
  distinct() %>%                       # remove lower triangle duplicates
  mutate(padj = p.adjust(pval, method = "fdr"))

p.comp.cor.all.sum <- p.comp.cor.all.out.tidy %>%
  mutate(cor.direction = case_when(cor > 0 & cor < 0.25 ~ "pos_0.0-0.25",
                                   cor >= 0.25 & cor < 0.5 ~ "pos_0.25-0.5",
                                   cor >= 0.50 & cor < 0.75 ~ "pos_0.5-0.75",
                                   cor >= 0.75 ~ "pos_0.75-1.0",
                                   cor < 0 & cor > -0.25 ~ "neg_0.0--0.25",
                                   cor <= -0.25 & cor > -0.5 ~ "neg_-0.25--0.5",
                                   cor <= -0.50 & cor > -0.75 ~ "neg_-0.5--0.75",
                                   cor <= -0.75 ~ "neg_-0.75--1.0")) %>%
  mutate(cor.sig = case_when(padj < 0.01 ~ "Sig",
                             TRUE ~ "Not_Sig")) %>%
  group_by(cor.direction, cor.sig) %>%
  reframe(count = n())





#___Dissolved

#compare all compounds to POC
d.comp.poc.cor <- diss.dat %>%
  rename(SampID = Diss.SampID) %>%
  left_join(., diss.metadata) %>%
  select(compound.name.figure, SampID, poc, log10.diss.conc.nM) %>%
  mutate(log10.poc = log10(poc)) %>%
  group_by(compound.name.figure) %>%
  cor_test(., vars = c(log10.diss.conc.nM, log10.poc))


#export
write_csv(d.comp.poc.cor, file = "Intermediates/Diss_POC_Osmo_Regression_Output.csv")






#compare all compounds to each other
d.comp.cor.all.dat <- diss.dat %>%
  rename(SampID = Diss.SampID) %>%
  select(compound.name.figure, SampID, log10.diss.conc.nM) %>%
  pivot_wider(id_cols = SampID, names_from = compound.name.figure, values_from = log10.diss.conc.nM) %>%
  column_to_rownames(var = "SampID")

d.comp.cor.all.out <- rcorr(as.matrix(d.comp.cor.all.dat), type = "pearson")

d.r  <- d.comp.cor.all.out$r
d.p  <- d.comp.cor.all.out$P

d.comp.cor.all.out.tidy <- tibble(
  var1 = rownames(d.r)[row(d.r)],
  var2 = colnames(d.r)[col(d.r)],
  cor  = as.vector(d.r),
  pval = as.vector(d.p)) %>%
  filter(var1 != var2) %>%             # remove diagonal
  distinct() %>%                       # remove lower triangle duplicates
  mutate(padj = p.adjust(pval, method = "fdr"))

d.comp.cor.all.sum <- d.comp.cor.all.out.tidy %>%
  mutate(cor.direction = case_when(cor > 0 & cor < 0.25 ~ "pos_0.0-0.25",
                                   cor >= 0.25 & cor < 0.5 ~ "pos_0.25-0.5",
                                   cor >= 0.50 & cor < 0.75 ~ "pos_0.5-0.75",
                                   cor >= 0.75 ~ "pos_0.75-1.0",
                                   cor < 0 & cor > -0.25 ~ "neg_0.0--0.25",
                                   cor <= -0.25 & cor > -0.5 ~ "neg_-0.25--0.5",
                                   cor <= -0.50 & cor > -0.75 ~ "neg_-0.5--0.75",
                                   cor <= -0.75 ~ "neg_-0.75--1.0")) %>%
  mutate(cor.sig = case_when(padj < 0.01 ~ "Sig",
                             TRUE ~ "Not_Sig")) %>%
  group_by(cor.direction, cor.sig) %>%
  reframe(count = n())









###_________PERMANOVAs of Rank and Relative Abundance Data:

###__________Particulate____________________

#Rank Order:
p.matrix.regularscaling <- part.dat %>%
  rename(SampID = Part.SampID) %>%
  select(SampID, compound.name.figure, Part.Conc.nM) %>%
  pivot_wider(id_cols = SampID, names_from = compound.name.figure, values_from = Part.Conc.nM) %>%
  column_to_rownames(var = "SampID")

p.matrix.rank <- decostand(p.matrix.regularscaling, method = "rank", MARGIN = 1)

##metadata matrix
p.meta.region <- part.dat %>%
  rename(SampID = Part.SampID) %>%
  select(SampID, Region, Cruise) %>%
  unique()


#run PERMANOVA
p.per.output.rank <- adonis2(p.matrix.rank ~ Region + Cruise, data = p.meta.region, method = "euclidean")




#Relative Abundance:
p.matrix.relabun <- decostand(p.matrix.regularscaling, method = "range", MARGIN = 1)

#run PERMANOVA
p.per.output.relabun <- adonis2(p.matrix.relabun ~ Region + Cruise, data = p.meta.region, method = "euclidean")



###__________Dissolved____________________

#Rank Order:
d.matrix.regularscaling <- diss.dat %>%
  rename(SampID = Diss.SampID) %>%
  select(SampID, compound.name.figure, Diss.Conc.nM) %>%
  pivot_wider(id_cols = SampID, names_from = compound.name.figure, values_from = Diss.Conc.nM) %>%
  column_to_rownames(var = "SampID")

d.matrix.rank <- decostand(d.matrix.regularscaling, method = "rank", MARGIN = 1)

##metadata matrix
d.meta.region <- diss.dat %>%
  rename(SampID = Diss.SampID) %>%
  select(SampID, Region, Cruise) %>%
  unique()

d.per.output.rank <- adonis2(d.matrix.rank ~ Region + Cruise, data = d.meta.region, method = "euclidean")



#Relative Abundance:
d.matrix.relabun <- decostand(d.matrix.regularscaling, method = "range", MARGIN = 1)

#run PERMANOVA
d.per.output.relabun <- adonis2(d.matrix.relabun ~ Region + Cruise, data = d.meta.region, method = "euclidean")










###_________Correlations of region mean rank and mean relative abundance data 


#_________Particulate:

#Rank:
p.region.rank.dat <- part.dat %>%
  group_by(Part.SampID) %>%
  mutate(comp.rank = rank(1/Part.Conc.nM)) %>%
  group_by(Region, compound.name.figure) %>%
  reframe(mean.rank = mean(comp.rank))

#export
write_csv(p.region.rank.dat, file = "Intermediates/Part_Region_Mean_Rank.csv")

#run correlation analysis comparing the mean rank of a compound in one region to the
# mean rank of a compound in a second region
p.region.rank.comparison <- p.region.rank.dat %>%
  rename(Region.1 = Region,
         mean.rank.1 = mean.rank) %>%
  full_join(p.region.rank.dat %>%
               rename(Region.2 = Region,
               mean.rank.2 = mean.rank)) 


p.region.rank.cor <- p.region.rank.comparison %>%
  group_by(Region.1, Region.2) %>%
  cor_test(vars = c(mean.rank.1, mean.rank.2))

#export
write_csv(p.region.rank.cor, file = "Intermediates/Part_Region_Rank_Corr_Output.csv")




##____________Relative abundance:
p.region.relabun.dat <- part.dat %>%
  group_by(Part.SampID) %>%
  mutate(comp.relabun = Part.Conc.nM/max(Part.Conc.nM)) %>%
  group_by(Region, compound.name.figure) %>%
  reframe(mean.relabun = log10(mean(comp.relabun)))

#export
write_csv(p.region.relabun.dat, file = "Intermediates/Part_Region_Mean_RelAbun.csv")


#run correlation analysis comparing the mean relative abundance of a compound in one region to the
# mean relative abundance of a compound in a second region
p.region.relabun.comparison <- p.region.relabun.dat %>%
  rename(Region.1 = Region,
         mean.relabun.1 = mean.relabun) %>%
  full_join(p.region.relabun.dat %>%
              rename(Region.2 = Region,
                     mean.relabun.2 = mean.relabun)) 


p.region.relabun.cor <- p.region.relabun.comparison %>%
  group_by(Region.1, Region.2) %>%
  cor_test(vars = c(mean.relabun.1, mean.relabun.2))

#export
write_csv(p.region.relabun.cor, file = "Intermediates/Part_Region_RelAbun_Corr_Output.csv")





#_________Dissolved:

#Rank:
d.region.rank.dat <- diss.dat %>%
  group_by(Diss.SampID) %>%
  mutate(comp.rank = rank(1/Diss.Conc.nM)) %>%
  group_by(Region, compound.name.figure) %>%
  reframe(mean.rank = mean(comp.rank))

#export
write_csv(d.region.rank.dat, file = "Intermediates/Diss_Region_Mean_Rank.csv")

#run correlation analysis comparing the mean rank of a compound in one region to the
# mean relative abundance of a compound in a second region
d.region.rank.comparison <- d.region.rank.dat %>%
  rename(Region.1 = Region,
         mean.rank.1 = mean.rank) %>%
  full_join(d.region.rank.dat %>%
              rename(Region.2 = Region,
                     mean.rank.2 = mean.rank)) 


d.region.rank.cor <- d.region.rank.comparison %>%
  group_by(Region.1, Region.2) %>%
  cor_test(vars = c(mean.rank.1, mean.rank.2))

#export
write_csv(d.region.rank.cor, file = "Intermediates/Diss_Region_Rank_Corr_Output.csv")




##____________Relative abundance:
d.region.relabun.dat <- diss.dat %>%
  group_by(Diss.SampID) %>%
  mutate(comp.relabun = Diss.Conc.nM/max(Diss.Conc.nM)) %>%
  group_by(Region, compound.name.figure) %>%
  reframe(mean.relabun = log10(mean(comp.relabun)))

#export
write_csv(d.region.relabun.dat, file = "Intermediates/Diss_Region_Mean_RelAbun.csv")

#run correlation analysis comparing the mean relative abundance of a compound in one region to the
# mean rank of a compound in a second region
d.region.relabun.comparison <- d.region.relabun.dat %>%
  rename(Region.1 = Region,
         mean.relabun.1 = mean.relabun) %>%
  full_join(d.region.relabun.dat %>%
              rename(Region.2 = Region,
                     mean.relabun.2 = mean.relabun)) 


d.region.relabun.cor <- d.region.relabun.comparison %>%
  group_by(Region.1, Region.2) %>%
  cor_test(vars = c(mean.relabun.1, mean.relabun.2))

#export
write_csv(d.region.relabun.cor, file = "Intermediates/Diss_Region_RelAbun_Corr_Output.csv")



#







#___________Old Code:

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
# #
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
# p.pca <- rda(p.matrix.stand, scale = TRUE)
# 
# print(p.pca)
# summary(p.pca)
# p.pca.comp <- data.frame(p.pca[["CA"]][["v"]])
# p.pca.samp <- data.frame(p.pca[["CA"]][["u"]])
# 
# 
# #combine PCA with metadata and reverse PC1 (multiply by -1) to make more interpretable
# p.pca.samp.plot <- p.pca.samp %>%
#   rownames_to_column(var = "SampID") %>%
#   left_join(part.metadata) %>%
#   mutate(PC1 = PC1)
# 
# p.pca.comp.plot <- p.pca.comp %>%
#   rownames_to_column(var = "Compound") 
# 
# #Particulate:
# p.pca.plot <- ggplot(p.pca.samp.plot, aes(x = PC1, y = PC2)) +
#   geom_point(aes(fill = Region, shape = Cruise), stroke = 0.15, size = 3, shape = 21) +
#   # scale_shape_manual(values = c(22, 24, 21)) +
#   #  scale_shape_manual(values = c(22, 21, 23)) +
#   scale_fill_manual(values = region.palette.7) +
#   ylab("PC2 (11%)") +
#   xlab("PC1 (68%)") +
#   theme_test() +
#   # theme(panel.border = element_blank(), axis.line = element_line(), 
#   #       axis.text.x = element_blank(), axis.title.x = element_blank(), 
#   #      plot.title = element_text(hjust = 0.5)) +
#   ggtitle("Particulate") +
#   theme(plot.title = element_text(hjust = 0.5)) 
# 
# p.pca.plot
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
# ####Plot all of the PCA plots:
# 
# #Particulate:
# p.pca.plot <- ggplot(p.pca.samp.plot, aes(x = PC1, y = PC2)) +
#   geom_point(aes(fill = Region, shape = Cruise), stroke = 0.15, size = 3, shape = 21) +
#   # scale_shape_manual(values = c(22, 24, 21)) +
#   #  scale_shape_manual(values = c(22, 21, 23)) +
#   scale_fill_manual(values = region.palette.7) +
#   ylab("PC2 (11%)") +
#   xlab("PC1 (68%)") +
#   theme_test() +
#   # theme(panel.border = element_blank(), axis.line = element_line(), 
#   #       axis.text.x = element_blank(), axis.title.x = element_blank(), 
#   #      plot.title = element_text(hjust = 0.5)) +
#   ggtitle("Particulate") +
#   theme(plot.title = element_text(hjust = 0.5)) 
# 
# p.pca.plot
# 
# 
# 
# #Dissolved:
# d.pca.plot <- ggplot(d.pca.samp.plot, aes(x = PC1, y = PC2)) +
#   geom_point(aes(fill = Region, shape = Cruise), stroke = 0.15, size = 3, shape = 21) +
#   # scale_shape_manual(values = c(22, 24, 21)) +
#   #  scale_shape_manual(values = c(22, 21, 23)) +
#   scale_fill_manual(values = region.palette.7) +
#   ylab("PC2 (10%)") +
#   xlab("PC1 (50%)") +
#   theme_test() +
#   # theme(panel.border = element_blank(), axis.line = element_line(), 
#   #       axis.text.x = element_blank(), axis.title.x = element_blank(), 
#   #      plot.title = element_text(hjust = 0.5)) +
#   ggtitle("Dissolved") +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# d.pca.plot
# 
# 
# 
# 
# 
# 
# 
# ###________PCA Regression________________________
# 
# ####Make Plots of correlation of each compound and PC1 with POC
# 
# #Particulate - PC1 vs everything:
# p.pca.reg.dat <- p.pca.samp.plot %>%
#   select(Region, SampID, PC1, poc, pn, chla, sst, sss, N_N, SRP) %>%
#   pivot_longer(cols = poc:SRP, values_to = "val", names_to = "param") %>%
#   group_by(param) %>%
#   mutate(cor_pc1 = cor(PC1*-1, log10(val), use = "complete.obs")) 
# 
# ##Run PCA regressions
# p.pca.reg.out <- p.pca.reg.dat %>%
#   mutate(val = log10(val)) %>%
#   #  filter(!is.na(val)) %>%
#   group_by(param) %>%
#   cor_test(vars = c(PC1, val))
# 
# 
# ggplot(p.pca.reg.dat, aes(x = val, y = PC1, color = Region)) +
#   geom_point() +
#   facet_wrap(.~param, scales = "free") +
#   scale_x_log10()
# 
# 
# 
# 
# 
# 
# p.pca.reg.sum <- p.pca.reg.dat %>%
#   filter(!is.na(val)) %>%
#   group_by(param, cor_pc1) %>%
#   reframe(count = n()) 
# 
# 
# 
# 
# #Dissolved - PC1 vs everything:
# d.pca.reg.dat <- d.pca.samp.plot %>%
#   select(Region, SampID, PC1, poc, pn, chla, sst, sss, N_N, SRP) %>%
#   pivot_longer(cols = poc:SRP, values_to = "val", names_to = "param") %>%
#   group_by(param) %>%
#   mutate(cor_pc1 = cor(PC1*-1, log10(val), use = "complete.obs"))
# 
# ggplot(d.pca.reg.dat, aes(x = val, y = PC1, color = Region)) +
#   geom_point() +
#   facet_wrap(.~param, scales = "free") +
#   scale_x_log10()
# 
# 
# d.pca.reg.sum <- d.pca.reg.dat %>%
#   filter(!is.na(val)) %>%
#   group_by(param, cor_pc1) %>%
#   reframe(count = n()) 
# 




