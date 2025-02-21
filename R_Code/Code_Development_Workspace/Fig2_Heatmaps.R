




library(patchwork)
library(tidyverse)
library(viridis)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")



#define inputs:
cult.file <- "Intermediates/Culture_Final_Quant_QCed.csv"
cult.meta.file <- "Intermediates/All_culture_metadata.csv"
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


#meta data
samp.meta.dat <- dat.surface %>%
  select(SampID, Cruise, Lat, Long, depth_m) %>%
  unique() %>%
  mutate(Region = case_when(Cruise == "RC078" ~ "PS",
                            Lat > 36 ~ "NPTZ",
                            Lat < 7 ~ "Equator",
                            Cruise == "TN397" & Lat > 29 ~ "CC",
                            TRUE ~ "NPSG"))









## Look at Relative abundance by sample across region and get overall rank:
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

Region.metab.all <- Rel.metab.dat %>%
  left_join(., Overall.comp.rank) %>%
  ungroup() %>%
  left_join(., compound.order) %>%
  filter(!compound.name.figure %in% c("Arsenobetaine", "Hydroxyectoine"))


####
r.metab.dat.sml <- Region.metab.dat %>%
  filter(!compound.name.figure %in% c("Arsenobetaine"))
#
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


ggplot(r.metab.dat.sml, 
       aes(y = reorder(compound.name.figure, -Rank), x = Region, fill = mean.comp.rank)) +
  geom_tile(color = "black") +
  scale_fill_viridis(option = "F", end = 0.85) +
  coord_fixed()






###_________Load in Data from Culutres:
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
all.hm <- (enviro.hm | plot_spacer() | c.hm) + 
  plot_layout(guides = "collect", widths = c(2, -1.26, 2))
all.hm

ggsave(all.hm, filename = "R_Code/Code_Development_Workspace/Region_Culture_Heatmap.pdf",
       height = 6, width = 5, scale = 1.15)
