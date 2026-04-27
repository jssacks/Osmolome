


#load packages and source code:
library(tidyverse)
library(viridis)
library(patchwork)
library(ggsci)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")




cult.dat.file <- "Intermediates/Culture_Final_Quant_QCed.csv"
cult.meta.file <- "Intermediates/All_culture_metadata.csv"





#load and organize data:
dat.cult <- read_csv(cult.dat.file) %>%
  left_join(., read_csv(cult.meta.file)) %>%
  left_join(., compound.order) %>%
  filter(!str_detect(SampID, "Blk")) %>%
  mutate(Part.Conc.Vial.uM = case_when(Detected == "No" ~ 0,
                                       TRUE ~ Part.Conc.Vial.uM)) %>%
  mutate(Part.Conc.Vial.uM = replace_na(Part.Conc.Vial.uM, 0)) %>%
  filter(!is.na(compound.name.figure)) %>%
  filter(!is.na(Type))


## Redefine Organism Classes:
dat.org.class.new <- dat.cult %>%
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
                                              "Prasinophyte", "Diatom", "Haptophyte", "Dinoflagellate")))




#total osmolyte concentration:
osmo.sum.cult <- dat.org.class.new %>%
  mutate(Part.amount.osmo.umol = (Vol_mL/1000)*Part.Conc.uM) %>%
  group_by(SampID, org_class, tot_cells_filt, Cell_Volume_um3, cell_volume_on_filter_uL) %>%
  reframe(sum.osmo.amount.umol = sum(Part.amount.osmo.umol),
          sum.osmo.conc.uM = sum(Part.Conc.uM)) %>%
  mutate(osmo.amount.per.cell.umol = sum.osmo.amount.umol/tot_cells_filt) %>%
  mutate(osmo.conc.per.cell.uM = osmo.amount.per.cell.umol/Cell_Volume_um3) %>%
  filter(!org_class == "Bacteria")


#plot 1
ggplot(osmo.sum.cult, aes(x = Cell_Volume_um3, y = osmo.amount.per.cell.umol)) +
  geom_smooth(method = "lm", color = "black") +
  geom_point(shape = 21, size = 2.5, stroke = 0.15, aes(fill = org_class)) +
  scale_x_log10() +
  scale_y_log10() +
  scale_fill_manual(values = org.palette) +
  theme_bw()

#plot 2
ggplot(osmo.sum.cult, aes(x = cell_volume_on_filter_uL, y = sum.osmo.amount.umol))  +
  geom_smooth(method = "lm", color = "black") +
  geom_point(shape = 21, size = 2.5, stroke = 0.15, aes(fill = org_class)) +
  scale_x_log10() +
  scale_y_log10() +
  scale_fill_manual(values = org.palette) +
  theme_bw()
  
#plot 3
ggplot(osmo.sum.cult, aes(x = Cell_Volume_um3, y = osmo.conc.per.cell.uM))  +
  geom_smooth(method = "lm", color = "black") +
  geom_point(shape = 21, size = 2.5, stroke = 0.15, aes(fill = org_class)) +
  scale_x_log10() +
  scale_y_log10() +
  scale_fill_manual(values = org.palette) +
  theme_bw()

ggplot(osmo.sum.cult, aes(x = org_class, y = osmo.conc.per.cell.uM))  +
  geom_boxplot() +
#  geom_smooth(method = "lm", color = "black") +
  geom_point(shape = 21, size = 2.5, stroke = 0.15, aes(fill = org_class)) +
 # scale_x_log10() +
  scale_y_log10() +
  scale_fill_manual(values = org.palette) +
  theme_bw()


###stats:
lm1 <- lm(log10(Cell_Volume_um3) ~ log10(osmo.amount.per.cell.umol), data = osmo.sum.cult)
summary(lm1)

lm2 <- lm(log10(cell_volume_on_filter_uL) ~ log10(sum.osmo.amount.umol), data = osmo.sum.cult)
summary(lm2)
