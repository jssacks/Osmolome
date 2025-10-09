





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
                    aes(x = reorder(compound.name.figure, Rank), y = Region, 
                        fill = mean.rel.conc
                        #fill = sqrt(mean.rel.conc)
                    )) +
  geom_tile(color = "black") +
  theme_test() +
  scale_fill_viridis(option = "F", end = 0.85, trans = "sqrt", breaks = c(0.01, 0.1, 0.25, 0.5, 0.75)) +
  coord_fixed() +
  scale_x_discrete(position = "top") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0),
        axis.title.x = element_blank()) +
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



c.hm <- ggplot(dat.cult.mean, aes(x = reorder(compound.name.figure, Rank), y = org.group, fill = mean.norm.conc)) +
  # geom_tile(color = "black", aes(fill = NA))# +
  geom_tile(color = "black") +
  scale_fill_viridis(option = "F", end = 0.85, trans = "sqrt", breaks = c(0.01, 0.1, 0.25, 0.5, 0.75)) +
  coord_fixed() +
  theme_test() +
  scale_x_discrete(position = "top") +
  theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        #    axis.ticks.y = element_blank(),
        legend.position = "none") +
  ylab("Taxonomic Group")
c.hm

##Combined environmental/culture plot
env.cult.hm.pres <- (enviro.hm / c.hm) + 
  plot_layout(guides = "collect")
env.cult.hm.pres

ggsave(env.cult.hm.pres, file = "Figures/Outputs/Pres_Region_Culture_Heatmaps.pdf",
       height = 5, width = 8, dpi = 600, scale = 1.2)
