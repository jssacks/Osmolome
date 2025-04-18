



library(patchwork)
library(tidyverse)
library(viridis)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")

#load in data:

######define inputs
all.dat.file <- "Intermediates/Final_Osmo_Meta_Env_Dataframe.csv"
cult.file <- "Intermediates/Culture_Final_Quant_QCed.csv"
cult.meta.file <- "Intermediates/All_culture_metadata.csv"
osmo.file <- "Intermediates/G2SF_Quant_Output.csv"
meta.file <- "Meta_Data/G2_SF_Sample_MetaData.csv"



#Read in data
dat <- read_csv(all.dat.file) %>%
  filter(!Part.SampID == "220902_Smp_TN397_S11_600_U_C")



#### Look at Relative abundance by sample across region and get overall rank:
Rel.metab.dat <- dat%>%
  select(Part.SampID, Cruise, Region, Part.Conc.nM, Compound, depth_m) %>%
  unique() %>%
  filter(Cruise %in% c("TN397", "KM1906", "RC078")) %>%
  filter(depth_m < 16) %>%
  group_by(Part.SampID) %>%
  mutate(max.osmo.nM = max(Part.Conc.nM),
         rel.osmo.conc = Part.Conc.nM/max.osmo.nM,
         comp.rank = rank(Part.Conc.nM)) 


##Get overall compound rank
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

Region.metab.all <- Rel.metab.dat %>%
  left_join(., Overall.comp.rank) %>%
  ungroup() %>%
  left_join(., compound.order) %>%
  filter(!compound.name.figure %in% c("Arsenobetaine", "Hydroxyectoine"))


####
r.metab.dat.sml <- Region.metab.dat %>%
  filter(!compound.name.figure %in% c("Arsenobetaine"))



##Make Environmental Heatmap:
enviro.hm <- ggplot(r.metab.dat.sml, 
                    aes(y = reorder(compound.name.figure, -Rank), x = Region, 
                        fill = mean.rel.conc
                        #fill = sqrt(mean.rel.conc)
                    )) +
  geom_tile(color = "black") +
  theme_test() +
  scale_fill_viridis(option = "F", end = 0.85, trans = "sqrt", breaks = c(0.01, 0.1, 0.25, 0.5, 0.75)) +
  coord_fixed() +
  scale_x_discrete(position = "top") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0),
        axis.title.y = element_blank()) +
  labs(fill = "Normalized \nConcentration") 
enviro.hm





# Culture Heatmap ---------------------------------------------------------
dat.cult <- read_csv(cult.file)
meta.cult <- read_csv(cult.meta.file) %>%
  mutate(org.group = as.factor(case_when(Organism == "Nmar" ~ "Archaea",
                                         Type == "Bacteria" & !Organism == "Nmar" ~ "Bacteria",
                                         Organism %in% c("WH8501") ~ "N-fixer",
                                         Organism %in% c("1314P", "As9601", "MED4", "NATL2A") ~ "Pro",
                                         Organism %in% c("8102", "7803") ~ "Syn",
                                         Type == "Diatom" ~ "Diatom",
                                         Type == "Dino" ~ "Dino",
                                         Type == "Prasinophyte" ~ "Prasino",
                                         Type == "Haptophyte" ~ "Hapto"))) %>%
  mutate(org.group = fct_relevel(org.group, c("Archaea", "Bacteria", "Pro", "Syn", "N-fixer", "Prasino", "Diatom", "Hapto", "Dino")))

dat.cult.clean <- dat.cult %>%
  left_join(., meta.cult) 

dat.cult.mean <- dat.cult.clean %>%
  group_by(SampID) %>%
  mutate(max.norm.conc = uM.in.vial.ave/max(uM.in.vial.ave, na.rm = TRUE)) %>%
  group_by(org.group, Compound) %>%
  reframe(mean.norm.conc = mean(max.norm.conc, na.rm = TRUE)) %>%
  # reframe(mean.conc = mean(uM_in_samp, na.rm = TRUE)) %>%
  # group_by(Batch) %>%
  # mutate(rank.comp = rank(mean.conc),
  #        rel.mean.conc = mean.conc/max(mean.conc)) %>%
  left_join(., Overall.comp.rank) %>%
  filter(!is.na(Rank)) %>%
  left_join(., compound.order) %>%
  ungroup() #%>%
# filter(!compound.name.figure %in% c("Arsenobetaine", "Hydroxyectoine"))



c.hm <- ggplot(dat.cult.mean, aes(y = reorder(compound.name.figure, -Rank), x = org.group, fill = mean.norm.conc)) +
  # geom_tile(color = "black", aes(fill = NA))# +
  geom_tile(color = "black") +
  scale_fill_viridis(option = "F", end = 0.85, trans = "sqrt", breaks = c(0.01, 0.1, 0.25, 0.5, 0.75)) +
  coord_fixed() +
  theme_test() +
  scale_x_discrete(position = "top") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        #    axis.ticks.y = element_blank(),
        legend.position = "none") +
  xlab("Taxonomic Group")
c.hm

##Combined environmental/culture plot
env.cult.hm <- (enviro.hm | c.hm) + 
  plot_layout(guides = "collect")
env.cult.hm



# Make G2SF Heatmaps ------------------------------------------------------



###load in data
g2.dat <- read_csv(osmo.file)
meta.dat <- read_csv(meta.file)



###
sf.dat <- g2.dat %>%
  left_join(., meta.dat)  %>%
  rename(Compound = Name) %>%
  group_by(Compound, Station, Lat, Size_Fraction) %>%
  reframe(Mean.nM = mean(nM.in.smp),
          Mean.nM.C = mean(nM_C),
          Mean.nM.N = mean(nM_N),
          Mean.nM.S = mean(nM_S)) %>%
  group_by(Compound, Station, Lat) %>%
  # mutate(Mean.Area = case_when(Size_Fraction == "F" & Mean.Area > Mean.Area[Size_Fraction == "S"] ~ Mean.Area-Mean.Area[Size_Fraction == "S"],
  #                             Size_Fraction == "F" & Mean.Area < Mean.Area[Size_Fraction == "S"] ~ 1,
  #                             TRUE ~ Mean.Area)) %>%
  mutate(Size_Fraction = str_replace(Size_Fraction, "F", "L")) %>%
  mutate(Small_Frac_Perc = Mean.nM[Size_Fraction == "S"]/(Mean.nM[Size_Fraction == "S"] + Mean.nM[Size_Fraction == "L"])) %>%
  ungroup() %>%
  filter(!Compound %in% c("Trimethylamine N-oxide", "L-Cysteic acid", "L-Lysine", "L-Tyrosine", "L-Serine", "L-Methionine", "L-Arginine")) %>%
  filter(!str_detect(Compound, "tentative")) %>%
  left_join(., compound.order)



##
# metab.clust.dat <- sf.dat %>%
#   select(Compound, Lat, Small_Frac_Perc) %>%
#   unique() %>%
#   pivot_wider(id_cols = Compound, names_from = Lat, values_from = Small_Frac_Perc) %>%
#   column_to_rownames(var = "Compound")
# 
# metab.dist <- vegdist(metab.clust.dat, method = "euclidean")
# 
# clust.out <- hclust(metab.dist, method = "average")
# dend <- as.dendrogram(clust.out)
# dend.dat <- dendro_data(dend)
# dend.order <- dend.dat$labels %>%
#   rename("order" = x, 
#          "Compound" = label) %>%
#   select(Compound, order)







###Overall Relative Abundance:
sf.dat.relabun <- sf.dat %>%
  filter(Size_Fraction == "L") %>%
  group_by(Station, Lat) %>%
  mutate(max.osmo.nM = max(Mean.nM),
         rel.osmo.conc = Mean.nM/max.osmo.nM,
         comp.rank = rank(Mean.nM)) %>%
#  left_join(dend.order) %>%
  left_join(., compound.order %>%
              select(-order)) 

g2.relabun.hm <- ggplot(sf.dat.relabun, aes(x = as.factor(Lat), y = reorder(compound.name.figure, rel.osmo.conc), fill = rel.osmo.conc)) +
  geom_tile(color = "black") +
  scale_fill_viridis(option = "F", end = 0.85, trans = "sqrt", breaks = c(0.01, 0.1, 0.25, 0.5, 0.75)) +
 # scale_fill_viridis(option = "G", begin = 0) +
  # facet_wrap(.~Class, scales = "free") +
  theme_bw() +
  xlab("Latitude") +
  ylab("Compound") +
  coord_fixed() +
  scale_x_discrete(position = "top") +
  theme(axis.text.x = element_text(angle = 60, hjust = 0),
        axis.title.y = element_blank(), 
        legend.position = "none")
  
g2.relabun.hm


###Size Fraction Relative Abundance 
metab.heatmap.dat <- sf.dat %>%
  select(Compound, Lat, Small_Frac_Perc) %>%
#  left_join(dend.order) %>%
  left_join(., compound.order %>%
              select(-order)) %>%
  unique() %>%
  left_join(., sf.dat.relabun %>% select(Compound, Station, rel.osmo.conc))

#heatmap showing % of signal in small size fraction 
g2sf.hm <- ggplot(metab.heatmap.dat, aes(x = as.factor(Lat), y = reorder(compound.name.figure, rel.osmo.conc), fill = Small_Frac_Perc)) +
  geom_tile(color = "black") +
  scale_fill_viridis(option = "G", begin = 0) +
  # facet_wrap(.~Class, scales = "free") +
  xlab("Latitude") +
  ylab("Compound") +
  coord_fixed() +
  theme_bw() +
  scale_x_discrete(position = "top") +
  theme(axis.text.x = element_text(angle = 60, hjust = 0)) +
  theme(axis.title.y = element_blank(), axis.text.y = element_blank()) +
  labs(fill = "Fraction \nless than \n3 microns") 
g2sf.hm


###Make plot of range of relative abundance vs. range of small fraction percentage:
rel.sf.dat <- metab.heatmap.dat %>%
  group_by(Compound) %>%
  mutate(Range.rel.abun = max(rel.osmo.conc) - min(rel.osmo.conc),
         RSD.rel.abun = sd(rel.osmo.conc)/mean(rel.osmo.conc),
         Range.SFP = max(Small_Frac_Perc) - min(Small_Frac_Perc),
         RSD.SFP = sd(Small_Frac_Perc)/mean(Small_Frac_Perc)) %>%
  select(Compound, Range.rel.abun, Range.SFP, RSD.rel.abun, RSD.SFP) %>%
  unique() %>%
  left_join(compound.order)

range.sf.plot <- ggplot(rel.sf.dat, aes(x = Range.rel.abun, y = Range.SFP)) + 
  geom_point(size = 3, shape = 21, aes(fill = reorder(compound.name.figure, order))) +
  scale_fill_manual(values = compound.pal.fig) +
  labs(fill = "Compound") +
#  geom_abline(intercept = 0, slope = 1) +
  xlab("Range of Max Normalized Abundance Values") + 
  ylab("Range of Small Fraction Percentage Values") + 
  theme_test()
range.sf.plot

ggsave(range.sf.plot, file = "Figures/Outputs/G2SF_Supplemental_range_plot.png", height = 5, width = 7, dpi = 600,
       scale = 1.3)

##Relative abundance sum:
rel.sf.sum <- rel.sf.dat %>%
  mutate(category = case_when(Range.rel.abun > Range.SFP ~ "RelAbun_Greater",
                   TRUE ~ "SFP_Greater")) %>%
  group_by(category) %>%
  mutate(count = n())


###Put G2 SF heatmaps together:
g2.all.hms <- g2.relabun.hm + g2sf.hm + plot_layout(guides = "collect")
g2.all.hms


###Put all 4 heatmaps together: 
heatmaps <- ((enviro.hm | c.hm) | (g2.relabun.hm | g2sf.hm)) + plot_layout(guides = "collect") +
  plot_annotation(tag_levels = list(c("A", "", "B", ""))) 
heatmaps

#Export:
ggsave(heatmaps, file = "Figures/Outputs/Region_Culture_G2SF_Heatmaps.png",
          height = 6, width = 8, dpi = 600, scale = 1.2)


##



































