




#load packages and source code:
library(tidyverse)
library(viridis)
library(patchwork)
library(ggsci)
library(rstatix)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")

##Define inputs:

#metabolomic data:
cult.metab <- "Intermediates/Culture_Final_Quant_QCed.csv"

#meta data
cult.meta <- "Intermediates/All_culture_metadata.csv"


#____________________Organize dataset:_________________________

#load and organize data:
meta.data <- read_csv(cult.meta)

dat.cult <- read_csv(cult.metab) %>%
  left_join(., compound.order) %>%
  filter(!str_detect(SampID, "Blk")) %>%
  mutate(Part.Conc.Vial.uM = case_when(Detected == "No" ~ 0,
                                       TRUE ~ Part.Conc.Vial.uM)) %>%
  mutate(Part.Conc.Vial.uM = replace_na(Part.Conc.Vial.uM, 0)) %>%
  filter(!is.na(compound.name.figure)) %>%
  filter(!is.na(Type)) %>%
  group_by(SampID) %>%
  left_join(., meta.data)


#Redefine organism classes:
dat.cult.new <- dat.cult %>%
  mutate(org_class = case_when(Organism == "Nmar" ~ "Archaea",
                               Type == "Bacteria" & !Organism == "Nmar" ~ "Bacteria",
                               Type == "Dino" ~ "Dinoflagellate",
                               Type == "Diatom" ~ "Diatom",
                               Type == "Haptophyte" ~ "Haptophyte",
                               Type == "Prasinophyte" ~ "Prasinophyte",
                               Organism %in% c("WH8501") ~ "Croco",
                               Organism %in% c("1314P", "As9601", "MED4", "NATL2A") ~ "Pro",
                               Organism %in% c("8102", "7803") ~ "Syn")) %>%
  mutate(org_class = as.factor(org_class)) %>%
  mutate(org_class = fct_relevel(org_class, c("Archaea", "Bacteria", "Pro", "Syn", "Croco",
                                              "Prasinophyte", "Diatom", "Haptophyte", "Dinoflagellate"))) %>%
  mutate()



#Define dataset for total osmolyte analyses 
dat.cult.total<-  dat.cult.new %>%
  filter(Detected == "Yes") %>%
 # filter(!org_class == "Archaea") %>%
  group_by(SampID) %>%
  mutate(uM.in.samp = sum(Part.Conc.uM),
         umol.in.samp = uM.in.samp*Vol_mL*(1/1000),
         umol.per.cell = umol.in.samp/tot_cells_filt,
         cell_vol_on_filter_um3 = Cell_Volume_um3*tot_cells_filt) %>%
  ungroup() %>%
  select(SampID, org_class, Organism, uM.in.samp, umol.in.samp, umol.per.cell, 
         cell_volume_on_filter_uL, Cell_Volume_um3, cell_vol_on_filter_um3) %>%
  unique() 




#### Organize data to determine osmolyte accumulation:
dat.cult.organism <- dat.cult.new %>%
  group_by(Organism, org_class, class, compound.name.figure, Detected) %>%
  reframe(mean.conc.uM = mean(Part.Conc.uM)) %>%
  group_by(org_class, Organism) %>%
  mutate(rel.conc = mean.conc.uM/sum(mean.conc.uM)*100) %>%
  ungroup() %>%
  mutate(rel.conc = replace_na(rel.conc, 0)) %>%
  mutate(Detected_0.1_threshold = case_when(rel.conc >= 0.1 ~ "Yes",
                                            TRUE ~ "No")) %>%
  filter(rel.conc > 0.1) %>%
  mutate(cell_type = case_when(org_class %in% c("Archaea", "Bacteria", "Pro", "Syn", "Croco") ~ "Prokaryote",
                               TRUE ~ "Eukaryote"))
  


##Summarize number of osmolytes accumulated by each organism
dat.cult.sum.by.org <- dat.cult.organism %>%
  group_by(Organism, org_class, cell_type) %>%
  reframe(count = n())

#summarize number of organsims accumulating each compound
dat.cult.sum.by.compound.org <- dat.cult.organism %>%
  group_by(compound.name.figure) %>%
  reframe(count = n())

#summarize number of org_classes accumulating each compound
dat.cult.sum.by.compound.orgclass <- dat.cult.organism %>%
  select(compound.name.figure, org_class) %>%
  unique() %>%
  group_by(compound.name.figure) %>%
  reframe(count = n())






#____________Perform Analyses:_____________________

#__Total Concentration:____

#relationship between cell volume and summed osmo amount per cell
cell.vol.osmo.lm <- lm(log10(umol.per.cell)~log10(Cell_Volume_um3), data = dat.cult.total)
summary(cell.vol.osmo.lm)


#relationship between volume on filter and amount of osmo on filter
filter.vol.osmo.lm <- lm(log10(umol.in.samp)~log10(cell_vol_on_filter_um3), data = dat.cult.total)
summary(filter.vol.osmo.lm)



#__Accumulation by euks vs. proks____
w.anova.cell.type <- dat.cult.sum.by.org %>%
  welch_anova(count~cell_type)
  
cell.type.accumulation.sum <- dat.cult.sum.by.org %>%
  group_by(cell_type) %>%
  reframe(mean.count = mean(count),
          sd.count = sd(count),
          n = n())



#relationship 





#Figures:

#figure 1
cult.supfig.1 <- ggplot(dat.cult.total, aes(x = Cell_Volume_um3, y = umol.per.cell)) +
  geom_smooth(method = "lm", color = "black") +
  geom_point(aes(fill = org_class), shape = 21, size = 3, stroke = 0.15) +
  scale_fill_manual(values = org.palette) +
  scale_x_log10() + 
  scale_y_log10() +
  theme_test() +
  labs(fill = "Taxonomic Group",
       x = expression(Estimated~Cell~Volume~(mu*m^3)),
       y = expression(Osmolyte~Amount~per~Cell~(mu*mol))) +
  annotate("text", x = 0.2, y = 1e-7, 
           label = expression(atop(R^2 < 0.92, italic(p) < 0.001)),
           hjust = 0)

cult.supfig.1

##figure 2
cult.supfig.2 <- ggplot(dat.cult.total, aes(x = cell_vol_on_filter_um3, y = umol.in.samp)) +
  geom_smooth(method = "lm", color = "black") +
  geom_point(aes(fill = org_class), shape = 21, size = 3, stroke = 0.15) +
  scale_fill_manual(values = org.palette) +
  scale_x_log10() + 
  scale_y_log10() +
  theme_test() +
  labs(fill = "Taxonomic Group",
       x = expression(Estimated~Total~Cell~Volume~on~Filter~(mu*m^3)),
       y = expression(Osmolyte~Amount~on~Filter~(mu*mol))) +
  annotate("text", x = 3e7, y = 0.8, 
           label = expression(atop(R^2 < 0.44, italic(p) < 0.001)),
           hjust = 0)


cult.supfig.2

##Culture Supplemental Figure 1:
cult.tot.sup.fig.comb <- cult.supfig.1 + cult.supfig.2 + plot_layout(guides = "collect")
cult.tot.sup.fig.comb

##export
ggsave(cult.tot.sup.fig.comb, filename = "Figures/Output_Oct25/Culture_TotOsmo_Supp_Fig.png", 
       height = 3, width = 7, dpi = 600, scale = 1.3)


