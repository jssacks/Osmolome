



#install.packages("GGally")\

##Organize data:
library(patchwork)
library(viridis)
library(GGally)
library(ggforce)


library(tidyverse)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")

##Define inputs:
part.file <- "Intermediates/Particulate_Quant_Output.csv"
meta.file <- "Intermediates/All_metadata_information.csv"

###load in data and combine with metadata
part.dat <- read_csv(part.file)
meta.dat <- read_csv(meta.file)



#### Gather Surface Data:
dat.surface <- left_join(part.dat, meta.dat) %>%
  rename("Compound" = Name) %>%
  filter(Compound %in% compound.order$Compound) %>%
  left_join(., compound.order) %>%
  # filter(!station == 4) %>%
  filter(!SampID == "220902_Smp_TN397_S11_600_U_C") %>%
  unique() %>%
  filter(Cruise %in% c( "KM1906", "TN397", "RC078")) %>%
  filter(!Compound == "Threonine Betaine (tentative)") %>%
  filter(!Compound == "Homoserine Betaine (tentative)") %>%
  filter(depth_m < 10)



#####Run PCA Analysis
osmo.matrix <- dat.surface %>%
  mutate(log10.nM.in.smp = log10(nM.in.smp)) %>%
  select(SampID, Compound, log10.nM.in.smp) %>%
  pivot_wider(id_cols = SampID, names_from = Compound, values_from = log10.nM.in.smp) %>%
  column_to_rownames(var = "SampID")

#meta data
samp.meta.dat <- dat.surface %>%
  select(SampID, Cruise, Lat, Long, depth_m) %>%
  unique() %>%
  mutate(Region = case_when(Cruise == "RC078" ~ "PS",
                            Lat > 36 ~ "NPTZ",
                            Lat < 7 ~ "Equator",
                            Cruise == "TN397" & Lat > 29 ~ "CC",
                            TRUE ~ "NPSG"))


###standardize data and make NMDS plot:
library(vegan)

osmo.matrix.stand <- decostand(osmo.matrix, method = "range", MARGIN = 2)

#osmo.dist.matrix <- vegdist(osmo.matrix.stand, method = "euclidean")

osmo.pca <- rda(osmo.matrix, scale = TRUE)

print(osmo.pca)
summary(osmo.pca)
pca.comp <- data.frame(osmo.pca[["CA"]][["v"]])
pca.samp <- data.frame(osmo.pca[["CA"]][["u"]])

pca.samp.plot <- pca.samp %>%
  rownames_to_column(var = "SampID") %>%
  left_join(samp.meta.dat)


#Perform PCA Regressions:

##Pull in metadata 
g.env.dat <- read_csv("Intermediates/Gradients_Matched_Environmental_Metadata.csv") 
d.env.dat <- read_csv("Intermediates/RC078_MetaData_Compiled.csv") %>%
  filter(!is.na(depth_m),
         !is.na(station)) %>%
  # select(sample_id, parent_id, station, depth_m) %>%
  rename("SampID" = sample_id) %>%
  mutate(SampID = paste("221006_Smp_", SampID, sep = ""),
         Cruise = "RC078") %>%
  #  select(SampID, Cruise, station, depth_m, Cruise) %>%
  filter(!str_detect(SampID, "-")) %>%
  unique() %>%
  rename("poc" = "POC_uM",
         "sss" = sal,
         "sst" = temp,
         "chla" = Chl_fluor)

###Organize all enviro dat
all.env.dat <- g.env.dat %>%
  select(SampID, sst, sss, chla_interp, pc_interp) %>%
  rename("chla" = chla_interp,
         "poc" = pc_interp) %>%
  rbind(., d.env.dat %>% select(SampID, sst, sss, chla, poc)) 






### Combine PCA data with environmental Data:
pca.env.dat <- pca.samp.plot %>%
  select(SampID, PC1, PC2, Cruise) %>%
  left_join(., all.env.dat) %>%
  filter(Cruise %in% c("TN397", "KM1906", "RC078")) %>%
  left_join(., samp.meta.dat)

##PCA Regression of PC1 vs. POC
poc.lm <- lm(PC1 ~ log10(poc), pca.env.dat)
summary(poc.lm)


##PCA Regression of PC1 vs. salinity
sss.lm <- lm(PC1 ~ sss, pca.env.dat)
summary(sss.lm)




## Look at Relative abundance by sample across region:
Rel.metab.dat <- dat.surface %>%
  left_join(samp.meta.dat %>% select(SampID, Region)) %>%
  group_by(SampID) %>%
  mutate(tot.osmo.nM = sum(nM.in.smp),
         max.osmo.nM = max(nM.in.smp),
         rel.osmo.conc = nM.in.smp/max.osmo.nM,
         comp.rank = rank(nM.in.smp)) 


Overall.comp.rank <- Rel.metab.dat %>%
  group_by(Compound) %>%
  mutate(Mean.rel.osmo.conc = mean(rel.osmo.conc)) %>%
  ungroup() %>%
  select(Compound, Mean.rel.osmo.conc) %>%
  unique() %>%
  mutate(Rank = rank(1/Mean.rel.osmo.conc))


##Region.metabs
Region.metab.dat <- Rel.metab.dat %>%
  group_by(Compound, Region) %>%
  reframe(mean.rel.conc = mean(rel.osmo.conc),
          median.rel.conc = median(rel.osmo.conc),
          sd.rel.conc = sd(rel.osmo.conc),
          mean.comp.rank = mean(comp.rank),
          sd.comp.rank = sd(comp.rank)) %>%
  left_join(., Overall.comp.rank) %>%
  ungroup() %>%
  left_join(., compound.order)


###
Region.metab.all <- Rel.metab.dat %>%
  left_join(., Overall.comp.rank) %>%
  ungroup() %>%
  left_join(., compound.order) %>%
  filter(!compound.name.figure %in% c("Arsenobetaine", "Hydroxyectoine"))


###Plot Relative concentrations:
ggplot(Region.metab.all, aes(x = rel.osmo.conc, y = reorder(compound.name.figure, -Rank))) +
  geom_jitter(aes(color = Region), width = 0, height = 0.15, alpha = 0.5) +
  scale_color_manual(values = region.palette) +
  geom_boxplot(alpha = 0.3, width = 0.5) +
#  facet_wrap(.~Region, nrow = 1) +
  scale_x_continuous(trans = trans_reverser('log10'), breaks = c(1, 0.5, 0.25, 0.1, 0.05, 0.01)) 
 # scale_x_log10()#+

#By compound class
ggplot(Region.metab.all %>%
         filter(class %in% c("AA", "Betaine")), aes(x = rel.osmo.conc, y = reorder(compound.name.figure, -Rank))) +
  geom_jitter(aes(color = Region), width = 0, height = 0.15, alpha = 0.5) +
  scale_color_manual(values = region.palette) +
  geom_boxplot(alpha = 0.3, width = 0.5) +
  facet_wrap(.~class, nrow = 1, scales = "free") +
  scale_x_continuous(trans = trans_reverser('log10'), breaks = c(1, 0.5, 0.25, 0.1, 0.05, 0.01)) 
# scale_x_log10()#+


###Plot Rank concentrations
ggplot(Region.metab.all, aes(x = comp.rank, y = reorder(compound.name.figure,-Rank))) +
  geom_jitter(aes(color = Region), width = 0, height = 0.15, alpha = 0.5) +
  scale_color_manual(values = region.palette) +
  facet_wrap(.~Region, nrow = 1) +
  geom_boxplot(alpha = 0.3, width = 0.5) 



##Plot Region metab concenrations
region.metab.fig <- ggplot(Region.metab.dat, aes(x = reorder(compound.name.figure, -Rank), y = median.rel.conc, group = Region)) +
  geom_line(aes(color = Region)) +
  geom_point(aes(color = Region), shape = 21, fill = "white") +
  theme_test() + 
 # scale_y_log10() +
  scale_color_manual(values = region.palette) +
  xlab("Compound") +
  ylab("Median Normalized Concentration") +
  theme(panel.border = element_blank(), axis.line = element_line(),
        legend.position = "none") +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))
  #+
 # geom_errorbar(aes(ymin = mean.rel.conc-sd.rel.conc, ymax = mean.rel.conc+sd.rel.conc, color = Region), width = 0)# +
 # scale_y_log10()
region.metab.fig

r.metab.dat.sml <- Region.metab.dat %>%
  filter(!compound.name.figure %in% c("Arsenobetaine", "Hydroxyectoine"))

library(cmocean)

ggplot(r.metab.dat.sml, 
       aes(y = reorder(compound.name.figure, -Rank), x = Region, 
           fill = mean.rel.conc
           #fill = sqrt(mean.rel.conc)
           )) +
  geom_tile(color = "black") +
  scale_fill_viridis(option = "F", end = 0.85, trans = "sqrt", breaks = c(0.01, 0.1, 0.25, 0.5, 0.75)) +
  coord_fixed()

ggplot(r.metab.dat.sml, 
       aes(y = reorder(compound.name.figure, -Rank), x = Region, fill = mean.comp.rank)) +
  geom_tile(color = "black") +
  scale_fill_viridis(option = "F", end = 0.85)




  scale_fill_cmocean(name = "matter")
  
  
  scale_fill_gradientn(colors = c("#082741", "#8ba7b7", "#df725d", "#f9f2d5"))
    
    low = "#082741", mid = "#df725d", high = "#f9f2d5")
  
  scale_fill_viridis(option = "F")
  scale_fill_cmocean(name = "thermal")

#color 1: 
  #f9f2d5
  #f8f1d5
  #082741
  #df725d
  
  
  



#####
region.metab.fig.rank <- ggplot(Region.metab.dat, aes(x = reorder(compound.name.figure, -Rank), y = mean.comp.rank, group = Region)) +
  geom_line(aes(color = Region)) +
  geom_point(aes(color = Region), shape = 21, fill = "white") +
  theme_test() + 
  # scale_y_log10() +
  scale_color_manual(values = region.palette) +
  xlab("Compound") +
  ylab("Mean Rank") +
  theme(panel.border = element_blank(), axis.line = element_line(),
        legend.position = "none") +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))
#+
# geom_errorbar(aes(ymin = mean.rel.conc-sd.rel.conc, ymax = mean.rel.conc+sd.rel.conc, color = Region), width = 0)# +
# scale_y_log10()
region.metab.fig.rank








#Plot PCA and RCA-R

#Full PCA
region.pca <- ggplot(pca.samp.plot, aes(x = PC1, y = PC2, shape = Cruise)) +
  geom_point(aes(fill = Region), stroke = 0.15, size = 2.5, shape = 21) +
  #  scale_shape_manual(values = c(22, 21, 23)) +
  scale_fill_manual(values = region.palette) +
  ylab("PC2 (9%)") +
  theme_test() +
  theme(panel.border = element_blank(), axis.line = element_line(), 
        axis.text.x = element_blank(), axis.title.x = element_blank()) 
region.pca

#PCA Regression 
poc.pca.fig <- ggplot(pca.env.dat, aes(x = PC1, y = poc)) +
  geom_smooth(method = "lm", color = "black") +
  geom_point(shape = 21, aes(fill = Region), size =2.5, stroke = 0.15) +
  scale_y_log10() +
  scale_fill_manual(values = region.palette) +
  theme_test() +
  ylab("POC (uM)") +
  xlab("PC1 (71%)") +
  theme(panel.border = element_blank(), axis.line = element_line()) +
  annotate("text", x = 0.15, y = 2.5, 
           label = expression(R^2~"="~0.89),
           size = 4) 
poc.pca.fig



##Combine plots
MT.Fig2 <- (region.pca + poc.pca.fig + region.metab.fig) +
  plot_layout(ncol = 1, heights = c(3,3,4), guides = "collect") +
  plot_annotation(tag_levels = "A")
MT.Fig2

ggsave(MT.Fig2, file = "R_Code/Code_Development_Workspace/MT_Fig2_PCA.pdf", height = 6.5, width = 4.75, scale = 1.3)


##Combine plots
MT.Fig2.rank <- (region.pca + poc.pca.fig + region.metab.fig.rank) +
  plot_layout(ncol = 1, heights = c(3,3,4), guides = "collect") +
  plot_annotation(tag_levels = "A")
MT.Fig2.rank

ggsave(MT.Fig2.rank, file = "R_Code/Code_Development_Workspace/MT_Fig2_PCA_rank.pdf", height = 6.5, width = 4.75, scale = 1.3)








##Do stats on Relative Metab data:
library(rstatix)

rel.stats.dat <- Rel.metab.dat

#Try welch's ANOVA
w.anova.out <- rel.stats.dat %>%
  group_by(Compound) %>%
  welch_anova_test(., rel.osmo.conc ~ Region)

comp.out <- rel.stats.dat %>%
  group_by(Compound, Region) %>%
  get_comparisons("rel.osmo.conc", ref.group = "all")

