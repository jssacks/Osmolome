





library(tidyverse)
library(vegan)
library(viridis)
library(rstatix)
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










#Run PCA:

###Particulate:
p.matrix.stand <- decostand(part.matrix, method = "range", MARGIN = 2)

p.pca <- rda(p.matrix.stand, scale = TRUE)

print(p.pca)
summary(p.pca)
p.pca.comp <- data.frame(p.pca[["CA"]][["v"]])
p.pca.samp <- data.frame(p.pca[["CA"]][["u"]])

p.pca.samp.plot <- p.pca.samp %>%
  rownames_to_column(var = "SampID") %>%
  left_join(part.metadata)

p.pca.comp.plot <- p.pca.comp %>%
  rownames_to_column(var = "Compound") 



###Dissolved:
d.matrix.stand <- decostand(diss.matrix, method = "range", MARGIN = 2)

d.pca <- rda(d.matrix.stand, scale = TRUE)

print(d.pca)
summary(d.pca)
d.pca.comp <- data.frame(d.pca[["CA"]][["v"]])
d.pca.samp <- data.frame(d.pca[["CA"]][["u"]])

d.pca.samp.plot <- d.pca.samp %>%
  rownames_to_column(var = "SampID") %>%
  left_join(diss.metadata)

d.pca.comp.plot <- d.pca.comp %>%
  rownames_to_column(var = "Compound") 




####Plot all of the PCA plots:

#Particulate:
p.pca.plot <- ggplot(p.pca.samp.plot, aes(x = PC1, y = PC2)) +
  geom_point(aes(fill = Region, shape = Cruise), stroke = 0.15, size = 3, shape = 21) +
 # scale_shape_manual(values = c(22, 24, 21)) +
  #  scale_shape_manual(values = c(22, 21, 23)) +
  scale_fill_manual(values = region.palette.7) +
  ylab("PC2 (11%)") +
  xlab("PC1 (68%)") +
  theme_test() +
 # theme(panel.border = element_blank(), axis.line = element_line(), 
 #       axis.text.x = element_blank(), axis.title.x = element_blank(), 
  #      plot.title = element_text(hjust = 0.5)) +
  ggtitle("Particulate") +
  theme(plot.title = element_text(hjust = 0.5)) 
  
p.pca.plot



#Dissolved:
d.pca.plot <- ggplot(d.pca.samp.plot, aes(x = PC1, y = PC2)) +
  geom_point(aes(fill = Region, shape = Cruise), stroke = 0.15, size = 3, shape = 21) +
  # scale_shape_manual(values = c(22, 24, 21)) +
  #  scale_shape_manual(values = c(22, 21, 23)) +
  scale_fill_manual(values = region.palette.7) +
  ylab("PC2 (10%)") +
  xlab("PC1 (50%)") +
  theme_test() +
  # theme(panel.border = element_blank(), axis.line = element_line(), 
  #       axis.text.x = element_blank(), axis.title.x = element_blank(), 
  #      plot.title = element_text(hjust = 0.5)) +
  ggtitle("Dissolved") +
  theme(plot.title = element_text(hjust = 0.5))

d.pca.plot







###________PCA Regression________________________

####Make Plots of correlation of each compound and PC1 with POC

#Particulate - PC1 vs everything:
p.pca.reg.dat <- p.pca.samp.plot %>%
  select(Region, SampID, PC1, poc, pn, chla, sst, sss, N_N, SRP) %>%
  pivot_longer(cols = poc:SRP, values_to = "val", names_to = "param") %>%
  group_by(param) %>%
  mutate(cor_pc1 = cor(PC1*-1, log10(val), use = "complete.obs")) 

##Run PCA regressions
p.pca.reg.out <- p.pca.reg.dat %>%
  mutate(val = log10(val)) %>%
#  filter(!is.na(val)) %>%
  group_by(param) %>%
  cor_test(vars = c(PC1, val))


ggplot(p.pca.reg.dat, aes(x = val, y = PC1, color = Region)) +
  geom_point() +
  facet_wrap(.~param, scales = "free") +
  scale_x_log10()






p.pca.reg.sum <- p.pca.reg.dat %>%
  filter(!is.na(val)) %>%
  group_by(param, cor_pc1) %>%
  reframe(count = n()) 




#Dissolved - PC1 vs everything:
d.pca.reg.dat <- d.pca.samp.plot %>%
  select(Region, SampID, PC1, poc, pn, chla, sst, sss, N_N, SRP) %>%
  pivot_longer(cols = poc:SRP, values_to = "val", names_to = "param") %>%
  group_by(param) %>%
  mutate(cor_pc1 = cor(PC1*-1, log10(val), use = "complete.obs"))

ggplot(d.pca.reg.dat, aes(x = val, y = PC1, color = Region)) +
  geom_point() +
  facet_wrap(.~param, scales = "free") +
  scale_x_log10()


d.pca.reg.sum <- d.pca.reg.dat %>%
  filter(!is.na(val)) %>%
  group_by(param, cor_pc1) %>%
  reframe(count = n()) 

lm.dat 






#####_________________Plots showing conserved rank order across regions____________

######______Heatmaps________
part.dat.heatmap <- dat.region %>%
  mutate(Part.Conc.nM = case_when(Part.detected == "No" ~ Part.Impute.Conc.nM, 
                                  TRUE ~ Part.Conc.nM)) %>%
  select(Part.SampID, Region, compound.name.figure, Part.Conc.nM) %>%
  group_by(Region, compound.name.figure) %>%
  reframe(mean.part.conc.nM = mean(Part.Conc.nM)) %>%
  group_by(Region) %>%
  mutate(region.rank = rank (1/mean.part.conc.nM)) %>%
  group_by(compound.name.figure) %>%
  mutate(overall.rank = mean(region.rank))

hm.plot <- ggplot(part.dat.heatmap, aes(x = Region, y = reorder(compound.name.figure, -overall.rank), fill = region.rank)) +
  geom_tile(color = "black", size = 0.05) +
  scale_fill_viridis(direction = -1, option = "D") +
  coord_fixed() +
  labs(fill = "Rank") +
  ylab("Compound") +
  scale_x_discrete(position = "top") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0))
hm.plot






#####__Make BRR Plots:

#organize particulate surface data:
p.betaine.dat <- dat.region %>%
  filter(Cruise %in% c( "KM1906", "TN397", "RC078")) %>%
  select(Compound, compound.name.figure, order, class, Part.SampID, Cruise, Part.Conc.nM, Region) %>% 
  unique() %>%
  filter(class == "Betaine") %>%
  group_by(Part.SampID) %>%
  mutate(Betaine.Norm.Val = Part.Conc.nM/max(Part.Conc.nM),
         Betaine.Tot.Conc = sum(Part.Conc.nM),
         Betaine.Rank = rank(1/Part.Conc.nM),
         count = n()) %>%
  ungroup() %>%
  mutate(Betaine.Norm.Val.2 = Part.Conc.nM/Betaine.Tot.Conc) %>%
  group_by(Compound) %>%
  mutate(Mean.Betaine.Rank = mean(Betaine.Rank),
         SD.Betaine.Rank = sd(Betaine.Rank),
         Mean.Betaine.Val = mean(Betaine.Norm.Val),
         SD.Betaine.Val = sd(Betaine.Norm.Val),
         Betaine.Val.Max = Mean.Betaine.Val + SD.Betaine.Val,
         Betaine.Val.Min = case_when(Mean.Betaine.Val - SD.Betaine.Val > 0 ~ Mean.Betaine.Val - SD.Betaine.Val,
                                     Mean.Betaine.Val - SD.Betaine.Val < 0 ~ 0.01)) %>%
  ungroup() %>%
  mutate(Betaine.Redfield.Ratio = Mean.Betaine.Val/Mean.Betaine.Val[Compound == "Glycine betaine"])



#organize dissolved surface data:
d.betaine.dat <- dat.region %>%
  filter(Cruise %in% c("TN397", "RC078")) %>%
  select(Compound, compound.name.figure, class, Diss.SampID, Cruise, Diss.Conc.nM, order, Region) %>% 
  unique() %>%
  filter(!is.na(Diss.SampID)) %>%
  filter(!is.na(Diss.Conc.nM)) %>%
  filter(class == "Betaine") %>%
  group_by(Diss.SampID) %>%
  mutate(Betaine.Norm.Val = Diss.Conc.nM/max(Diss.Conc.nM),
         Betaine.Tot.Conc = sum(Diss.Conc.nM),
         Betaine.Rank = rank(1/Diss.Conc.nM),
         count = n()) %>%
  ungroup() %>%
  mutate(Betaine.Norm.Val.2 = Diss.Conc.nM/Betaine.Tot.Conc) %>%
  group_by(Compound) %>%
  mutate(Mean.Betaine.Rank = mean(Betaine.Rank),
         SD.Betaine.Rank = sd(Betaine.Rank),
         Mean.Betaine.Val = mean(Betaine.Norm.Val),
         SD.Betaine.Val = sd(Betaine.Norm.Val),
         Betaine.Val.Max = Mean.Betaine.Val + SD.Betaine.Val,
         Betaine.Val.Min = case_when(Mean.Betaine.Val - SD.Betaine.Val > 0 ~ Mean.Betaine.Val - SD.Betaine.Val,
                                     Mean.Betaine.Val - SD.Betaine.Val < 0 ~ 0.01)) %>%
  ungroup() %>%
  mutate(Betaine.Redfield.Ratio = Mean.Betaine.Val/Mean.Betaine.Val[Compound == "Glycine betaine"])



#sumarize BRR data:
##Redfield.sum 
p.red <- p.betaine.dat %>%
  select(compound.name.figure, Betaine.Redfield.Ratio, Mean.Betaine.Rank, SD.Betaine.Rank) %>%
  unique() %>%
  rename(P.Ratio = Betaine.Redfield.Ratio,
         P.Rank = Mean.Betaine.Rank,
         P.Rank.SD = SD.Betaine.Rank)

d.red <- d.betaine.dat %>%
  select(compound.name.figure, Betaine.Redfield.Ratio, Mean.Betaine.Rank, SD.Betaine.Rank) %>%
  unique() %>%
  rename(D.Ratio = Betaine.Redfield.Ratio,
         D.Rank = Mean.Betaine.Rank,
         D.Rank.SD = SD.Betaine.Rank)


####Get compound properties
std.formula <- read_csv(stds.file) %>%
  select(Compound_Name, mz) %>%
  rename("Compound" = Compound_Name) %>%
  unique() 

comp.info <- dat %>%
  select(Compound, compound.name.figure, class) %>%
  unique() %>%
  filter(class == "Betaine") %>%
  left_join(., std.formula) %>%
  select(compound.name.figure, mz)

###
red.comb <- left_join(p.red, d.red) %>%
  left_join(., comp.info) %>%
  left_join(., compound.order)


###Run quick linear models 
lm.p.mz <- lm(mz~P.Rank, data = red.comb)
summary(lm.p.mz)

lm.d.mz <- lm(mz~D.Rank, data = red.comb)
summary(lm.d.mz)






#Make Plots:


#########Maek Relative Concentration Plot:

#particulate
p.rel.conc.plot <- ggplot(p.betaine.dat, aes(x = Betaine.Norm.Val, y = reorder(compound.name.figure, -Mean.Betaine.Rank))) +
  geom_jitter(height = 0.12, width = 0, alpha = 0.6, shape = 21, color = "black", 
              size = 2, stroke = 0.1, aes(fill = reorder(compound.name.figure, order))) +
  labs(fill = "Comopund") +
  geom_boxplot(alpha = 0.7, width = 0.3) +
  #  geom_point(aes(x = Mean.Betaine.Val, y = reorder(compound.name.figure, -Mean.Betaine.Rank)), color = "black", size = 2, shape = 21, fill = "white") +
  scale_fill_manual(values = compound.pal.fig) +
  scale_x_continuous(trans = trans_reverser('log10'), breaks = c(1, 0.5, 0.25, 0.1, 0.05, 0.01, 0.001), limits = c(1.5, 0.001)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  xlab("Relative Abundance") +
  ylab("Compound") +
  ggtitle("Particulate")
p.rel.conc.plot

#diss
d.rel.conc.plot <- ggplot(d.betaine.dat, aes(x = Betaine.Norm.Val, y = reorder(compound.name.figure, -Mean.Betaine.Rank))) +
  geom_jitter(height = 0.12, width = 0, alpha = 0.6, shape = 21, color = "black", 
              size = 2, stroke = 0.1, aes(fill = reorder(compound.name.figure, order))) +
  labs(fill = "Comopund") +
  geom_boxplot(alpha = 0.7, width = 0.3) +
  #  geom_point(aes(x = Mean.Betaine.Val, y = reorder(compound.name.figure, -Mean.Betaine.Rank)), color = "black", size = 2, shape = 21, fill = "white") +
  scale_fill_manual(values = compound.pal.fig) +
  scale_x_continuous(trans = trans_reverser('log10'), breaks = c(1, 0.5, 0.25, 0.1, 0.05, 0.01, 0.001), limits = c(1.5, 0.001)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  xlab("Relative Abundance") +
  ylab("Compound") +
  ggtitle("Dissolved")
d.rel.conc.plot


## Make rank vs. mz plots:
#Particulate
mz.p.plot <- ggplot(red.comb, aes(x = P.Rank, y = mz)) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgray") +
  geom_point(size = 3, shape = 21, stroke = .15, aes(fill = reorder(compound.name.figure, order))) +
  theme_bw() +
  scale_fill_manual(values = compound.pal.fig) +
  labs(fill = "Comopund") +
  xlab("Mean Rank in Particulate") +
  annotate("text", x = 6, y = 130, 
           label = expression(R^2~"="~0.71),
           size = 4) 
mz.p.plot

#Dissolved
mz.d.plot <- ggplot(red.comb, aes(x = D.Rank, y = mz)) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgray") +
  geom_point(size = 3, shape = 21, stroke = .15, aes(fill = reorder(compound.name.figure, order))) +
  theme_bw() +
  scale_fill_manual(values = compound.pal.fig) +
  labs(fill = "Comopund") +
  xlab("Mean Rank in Dissolved") +
  annotate("text", x = 6, y = 130, 
           label = expression(R^2~"="~0.70),
           size = 4) +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank())
mz.d.plot




###combine all of the plots together 
comb.plot.v1 <- (p.pca.plot + d.pca.plot + plot_layout(nrow = 1, guides = "collect")) /
  (hm.plot + (p.rel.conc.plot + d.rel.conc.plot + 
                mz.p.plot + mz.d.plot + plot_layout(ncol = 2, nrow = 2, heights = c(2,1))) +
     plot_layout(guides = "collect", nrow = 1, widths = c(1,2.5))) +
  plot_layout(heights = c(1,2), guides = "collect")
comb.plot.v1

ggsave(comb.plot.v1, filename = "Figures/Output_Oct25/PCA_HM_BRR_Plot.png", dpi = 900,
       height = 7, width = 8, scale = 1.4)








