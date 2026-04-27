




#load packages and source code:
library(tidyverse)
library(viridis)
library(patchwork)
library(ggsci)
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



#Define data set for total osmolyte analyses and figures
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


#Define data set for mean relative concentration analysis and figures:
dat.cult.rel <- dat.cult.new %>%
  filter(Detected == "Yes") %>%
  filter(!is.na(Vol_mL)) %>%
  group_by(Organism, org_class, compound.name.figure, order) %>%
  reframe(mean.part.conc.uM = mean(Part.Conc.uM, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(org_class_fig = case_when(org_class == "Archaea" ~ "Ar",
                                   org_class == "Croco" ~ "Cr",
                                   org_class == "Prasinophyte" ~ "Pr",
                                   org_class == "Haptophyte" ~ "Ha",
                                   org_class == "Dinoflagellate" ~ "Dino",
                                   TRUE ~ org_class)) %>%
  mutate(org_class_fig = as.factor(org_class_fig)) %>%
  mutate(org_class_fig = fct_relevel(org_class_fig, c("Ar", "Bacteria", "Pro", "Syn", "Cr",
                                              "Pr", "Diatom", "Ha", "Dino"))) %>%
  mutate()









#Perform Analyses:

#relationship between cell volume and summed osmo amount per cell
cell.vol.osmo.lm <- lm(log10(umol.per.cell)~log10(Cell_Volume_um3), data = dat.cult.total)
summary(cell.vol.osmo.lm)


#relationship between volume on filter and amount of osmo on filter
filter.vol.osmo.lm <- lm(log10(umol.in.samp)~log10(cell_vol_on_filter_um3), data = dat.cult.total)
summary(filter.vol.osmo.lm)



#relationship 





#Figures:

##################_______Make Supplemental Figure of Total Concentrations_________

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
cult.tot.sup.fig.comb <- cult.supfig.1 + cult.supfig.2 + plot_layout(guides = "collect") + plot_annotation(tag_levels = 'a')
cult.tot.sup.fig.comb

##export
ggsave(cult.tot.sup.fig.comb, filename = "Figures/Output_Oct25/Culture_TotOsmo_Supp_Fig.png", 
       height = 3, width = 7, dpi = 600, scale = 1.3)





##################_______Make Supplemental Figure of Mean Relative Concentrations In each species_________
cult.comp.fig <- ggplot(dat.cult.rel, aes(x = Organism, y = mean.part.conc.uM, fill = reorder(compound.name.figure, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15, position = "fill") +
  scale_fill_manual(values = compound.pal.fig) +
  facet_grid(.~org_class_fig, scales = "free", space = "free") +
  scale_y_continuous(expand = c(0,NA,NA,NA)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = "Relative Concentration (mole fraction)", 
       fill = "Compound")
cult.comp.fig



ggsave(cult.comp.fig, filename = "Figures/Output_Oct25/Culture_Relative_Composition_Supp_Fig.png", 
       height = 4, width = 9, dpi = 600, scale = 1.4)


























