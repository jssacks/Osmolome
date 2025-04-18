







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
         SampID.fig = str_remove(SampID.fig, "220104_Smp_")) %>%
  filter(!is.na(cell_volume_on_filter_uL)) %>%
  filter(!is.na(Vol_mL))

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

#ggsave(cult.fig1, filename = "R_Code/Code_Development_Workspace/Culture_Fig1.pdf", height = 9, width = 17)

###____Average Metabolome for Each Culutre Dataset:

###Make means dataset:
cult.fig.means <- cult.fig %>%
  group_by(Type, Organism, Compound) %>%
  reframe(Mean_uMol = mean(uM.in.vial.ave)) %>%
  left_join(., compound.order) %>%
  mutate(Type.short = case_when(Type == "Bacteria" ~ "Bact. and Arch.",
                                Type == "Cyano" ~ "Cyano.",
                                Type == "Diatom" ~ "Diatom",
                                Type == "Dino" ~ "Dino.",
                                Type == "Prasinophyte" ~ "P.",
                                Type == "Haptophyte" ~ "H."))


cult.fig2 <- ggplot(cult.fig.means, aes(y = Mean_uMol, x = Organism, fill = reorder(Compound, order))) +
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
  facet_grid(.~Type.short, scales = "free_x", space = "free_x") +
  ylab("Relative Concentration") +
  xlab("Organism") +
  labs(fill = "Compound") 
cult.fig2


ggsave(cult.fig2, filename = "Figures/Outputs/Culutre_Rel_Abun_Fig.png", height = 6.5, width = 9,
       scale = 1.4, dpi = 600)

















