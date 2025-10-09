


library(tidyverse)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")



#define inputs:
cult.file <- "Intermediates/Culture_Quant_Output.csv"
meta.file <- "Intermediates/All_culture_metadata.csv"
  
  
  
###load in data:
cult.dat <- read_csv(cult.file)
meta.dat <- read_csv(meta.file) 
  
  
###combine data with meta data
cult.meta.dat <- left_join(cult.dat, meta.dat) %>%
  filter(!str_detect(SampID, "Blk"))


cult.fig <- cult.meta.dat %>%
  rename("Compound" = Name) %>%
  filter(Compound %in% compound.order$Compound) %>%
  left_join(., compound.order) %>%
  filter(!is.na(Type)) %>%
  mutate(SampID.fig = str_remove(SampID, "220111_Smp_"),
         SampID.fig = str_remove(SampID.fig, "211221_Smp_"),
         SampID.fig = str_remove(SampID.fig, "211215_Smp_"),
         SampID.fig = str_remove(SampID.fig, "211218_Smp_"),
         SampID.fig = str_remove(SampID.fig, "220104_Smp_"))

#absolute concentrations
cult.fig1 <- ggplot(cult.fig, aes(y = uM_in_samp, x = SampID.fig, fill = reorder(Compound, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15, position = "fill") +
  scale_fill_manual(values = compound.pal) +
  #  scale_fill_manual(values = stepped2(n = 20)) +
  theme_test() +
#  coord_flip() +
  guides(fill=guide_legend(ncol=1)) +
  theme(legend.position = "right", strip.text = element_text(size = 12),
        axis.text.x = element_text(angle = 90)) +
  #  geom_point() +
  # scale_y_reverse() +
  facet_grid(.~Type, scales = "free_x", space = "free_x") +
  ylab("Relative Concentration") +
  xlab("Sample") +
  labs(fill = "Compound") 
cult.fig1

ggsave(cult.fig1, filename = "R_Code/Code_Development_Workspace/Culture_Fig1.pdf", height = 9, width = 17)





###Make means dataset:
cult.fig.means <- cult.fig %>%
  group_by(Type, Organism, Compound) %>%
  reframe(Mean_uMol = mean(uM.in.vial.ave)) %>%
  left_join(., compound.order) 


cult.fig2 <- ggplot(cult.fig.means, aes(y = Mean_uMol, x = Organism, fill = reorder(Compound, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15, position = "fill") +
  scale_fill_manual(values = compound.pal) +
  #  scale_fill_manual(values = stepped2(n = 20)) +
  theme_test() +
  #  coord_flip() +
  guides(fill=guide_legend(ncol=2)) +
  theme(legend.position = "right", strip.text = element_text(size = 12),
        axis.text.x = element_text(angle = 90)) +
  #  geom_point() +
  # scale_y_reverse() +
  facet_grid(.~Type, scales = "free_x", space = "free_x") +
  ylab("Relative Concentration") +
  xlab("Organism") +
  labs(fill = "Compound") 
cult.fig2


ggsave(cult.fig2, filename = "R_Code/Code_Development_Workspace/Culture_Fig2.pdf", height = 6, width = 15)





###Make means dataset:
cult.fig.means.betaines <- cult.fig.means %>%
  filter(class == "Betaine")


cult.fig3 <- ggplot(cult.fig.means.betaines, aes(y = Mean_uMol, x = Organism, fill = reorder(Compound, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15, position = "fill") +
  scale_fill_manual(values = compound.pal) +
  #  scale_fill_manual(values = stepped2(n = 20)) +
  theme_test() +
  #  coord_flip() +
  guides(fill=guide_legend(ncol=2)) +
  theme(legend.position = "right", strip.text = element_text(size = 12),
        axis.text.x = element_text(angle = 90)) +
  #  geom_point() +
  # scale_y_reverse() +
  facet_grid(.~Type, scales = "free_x", space = "free_x") +
  ylab("Relative Concentration") +
  xlab("Organism") +
  labs(fill = "Compound") 
cult.fig3













####Normailze things to cell volume:
cult.fig.norm <- cult.fig %>%
  mutate(umol_in_samp = uM_in_samp*Vol_mL*(1/1000),
         intracellular_conc_uM = umol_in_samp*(1/cell_volume_on_filter_uL)*1e6,
         Carbon_per_Cell_fg = 0.261*Cell_Volume_um3^0.86,
         Cell_Carbon_on_filter_fg = Carbon_per_Cell_fg*tot_cells_filt)


norm.cult.fig1 <- ggplot(cult.fig.norm, aes(y = intracellular_conc_uM, x = SampID.fig, fill = reorder(Compound, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15) +
  scale_fill_manual(values = compound.pal) +
  #  scale_fill_manual(values = stepped2(n = 20)) +
  theme_test() +
  #  coord_flip() +
  guides(fill=guide_legend(ncol=1)) +
  theme(legend.position = "right", strip.text = element_text(size = 12),
        axis.text.x = element_text(angle = 90)) +
  #  geom_point() +
  # scale_y_reverse() +
  facet_grid(.~Type, scales = "free_x", space = "free_x") +
  ylab("Relative Concentration") +
  xlab("Sample") +
  labs(fill = "Compound") 
norm.cult.fig1









cult.norm.sum <- cult.fig.norm %>%
  filter(!is.na(Vol_mL)) %>%
  group_by(SampID.fig, cell_volume_on_filter_uL, Organism, Type, Cell_Volume_um3, tot_cells_filt, Carbon_per_Cell_fg, Cell_Carbon_on_filter_fg) %>%
  reframe(Tot.Osmo.umol = sum(umol_in_samp, na.rm = TRUE),
          Osmo.per.cell.umol = Tot.Osmo.umol/tot_cells_filt,
          Intracellular_Conc_uM = Tot.Osmo.umol/cell_volume_on_filter_uL*1e6)
  
ggplot(cult.norm.sum, aes(x = cell_volume_on_filter_uL, y = Tot.Osmo.umol)) +
  geom_smooth(method = "lm") +
  geom_point(aes(color = Type)) +
  scale_y_log10() +
  scale_x_log10() +
  xlab("Cell volume on filter (uL)") +
  ylab("Total Osmolyte Amount on filter (umol)")

ggplot(cult.norm.sum, aes(x = Cell_Volume_um3, y = Osmo.per.cell.umol)) +
  geom_smooth(method = "lm") +
  geom_point(aes(color = Type)) +
  scale_y_log10() +
  scale_x_log10() +
  xlab("Cell Volume (um^3)") +
  ylab("Osmolyte Amount per Cell (umol)")

ggplot(cult.norm.sum, aes(y = Carbon_per_Cell_fg, x = Osmo.per.cell.umol)) +
  geom_smooth(method = "lm") +
  geom_point(aes(color = Type)) +
  scale_y_log10() +
  scale_x_log10() +
  ylab("Qc (fg C)") +
  xlab("Osmolyte Amount per Cell (umol)")

ggplot(cult.norm.sum, aes(x = Carbon_per_Cell_fg, y = Osmo.per.cell.umol)) +
  geom_smooth(method = "lm") +
  geom_point(aes(color = Type)) +
  # xlab("Cell Volume (um^3)") +
  ylab("Osmolyte Amount per Cell (umol)")




ggplot(cult.norm.sum, aes(x = Cell_Volume_um3, y = Carbon_per_Cell_fg)) +
  geom_smooth(method = "lm") +
  scale_y_log10() +
  scale_x_log10() +
  geom_point(aes(color = Type))

ggplot(cult.norm.sum %>% filter(!Type == "Bacteria"), aes(x = Cell_Carbon_on_filter_fg, y = Tot.Osmo.umol)) +
  geom_smooth(method = "lm") +
  geom_point(aes(color = Type)) +
  xlab("Cell carbon on filter (fg)") +
  ylab("Total Osmolyte Amount on filter (umol)")



osmo.calibration <- cult.norm.sum %>%
  mutate(Osmo.per.cell.nmol = 1000*Osmo.per.cell.umol,
         Carbon_per_Cell_ng = Carbon_per_Cell_fg/1000) 


ggplot(osmo.calibration, aes(y = Carbon_per_Cell_ng, x = Osmo.per.cell.nmol)) +
  geom_smooth(method = "lm") +
  geom_point(aes(color = Type)) +
  scale_y_log10() +
  scale_x_log10() +
  ylab("Qc (ng C)") +
  xlab("Osmolyte Amount per Cell (nmol)")

ggplot(osmo.calibration, aes(y = Carbon_per_Cell_ng, x = Osmo.per.cell.nmol)) +
  geom_smooth(method = "lm") +
  geom_point(aes(color = Type)) +
  ylab("Qc (ng C)") +
  xlab("Osmolyte Amount per Cell (nmol)")
model.norm <- lm(Carbon_per_Cell_ng ~ Osmo.per.cell.nmol, data = osmo.calibration)
summary(model.norm)

model.log <- lm(log10(Carbon_per_Cell_ng) ~ log10(Osmo.per.cell.nmol), data = osmo.calibration)
summary(model.log)


ggplot(cult.norm.sum, aes(x = Cell_Volume_um3, y = Intracellular_Conc_uM)) +
  #geom_smooth(method = "lm") +
  geom_point(aes(color = Type)) +
#  scale_y_log10() +
  scale_x_log10() 


#
cult.stoich <- cult.meta.dat %>%
  rename("Compound" = Name) %>%
  filter(Compound %in% compound.order$Compound) %>%
  left_join(., compound.order) %>%
  group_by(SampID) %>%
  mutate(C.tot = sum(uM_C_vial, na.rm = TRUE),
         N.tot = sum(uM_N_vial, na.rm = TRUE),
         S.tot = sum(uM_S_vial, na.rm = TRUE)) %>%
  mutate(C.N.tot = C.tot/N.tot,
         C.S.tot = C.tot/S.tot,
         N.S.tot = N.tot/S.tot) %>%
  mutate(Group_Carbon = case_when(class %in% c("Sugar") ~ "Yes",
                                  TRUE ~ "No"),
         Group_Nitrogen = case_when(class %in% c("AA", "Betaine", "TMAO", "Taurine") ~ "Yes",
                                    TRUE ~ "No"),
         Group_Sulfur = case_when(class %in% c("Sulfonium", "Sulfonate", "Taurine") ~ "Yes",
                                  TRUE ~ "No"))  %>%
  mutate(C.comp = sum(uM.in.vial.ave[Group_Carbon == "Yes"]),
         N.comp = sum(uM.in.vial.ave[Group_Nitrogen == "Yes"]),
         S.comp = sum(uM.in.vial.ave[Group_Sulfur == "Yes"]),
         C.N.comp = C.comp/N.comp,
         C.S.comp = C.comp/S.comp,
         N.S.comp = N.comp/S.comp) %>%
  select(SampID, Type, Organism, C.tot:N.S.comp) %>%
  unique() %>%
  filter(!is.na(Type))





#######tot comparisons:
ggplot(cult.stoich, aes(x = Organism, y = C.N.tot)) +
  geom_boxplot() +
  geom_point() +
  facet_grid(.~Type, scales = "free_x", space = "free_x") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_log10()

ggplot(cult.stoich %>% filter(!Type %in% c("Bacteria", "Cyano")), aes(x = Organism, y = C.S.tot)) +
  geom_boxplot() +
  geom_point() +
  facet_grid(.~Type, scales = "free_x", space = "free_x") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_log10()

ggplot(cult.stoich %>% filter(!Type %in% c("Bacteria", "Cyano")), aes(x = Organism, y = N.S.tot)) +
  geom_boxplot() +
  geom_point() +
  facet_grid(.~Type, scales = "free_x", space = "free_x") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_log10()


#######compound comparisons:
ggplot(cult.stoich, aes(x = Organism, y = C.N.comp)) +
  geom_boxplot() +
  geom_point() +
  facet_grid(.~Type, scales = "free_x", space = "free_x") +
  theme(axis.text.x = element_text(angle = 90))

ggplot(cult.stoich %>% filter(!Type %in% c("Bacteria", "Cyano")), aes(x = Organism, y = C.S.tot)) +
  geom_boxplot() +
  geom_point() +
  facet_grid(.~Type, scales = "free_x", space = "free_x") +
  theme(axis.text.x = element_text(angle = 90))

ggplot(cult.stoich %>% filter(!Type %in% c("Bacteria", "Cyano")), aes(x = Organism, y = N.S.tot)) +
  geom_boxplot() +
  geom_point() +
  facet_grid(.~Type, scales = "free_x", space = "free_x") +
  theme(axis.text.x = element_text(angle = 90))







































































