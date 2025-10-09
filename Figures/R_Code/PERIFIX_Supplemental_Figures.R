






library(tidyverse)
library(patchwork)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")



###define inputs
all.dat.file <- "Intermediates/Final_Osmo_Meta_Env_Dataframe.csv"



#Read in data
dat <- read_csv(all.dat.file) %>%
  filter(!Part.SampID == "220902_Smp_TN397_S11_600_U_C") %>%
  filter(!str_detect(Part.SampID, "Smp_S4_C1"),
         !str_detect(Part.SampID, "Smp_S4_C1"))







###############______PERIFIX

##Make plots with all samples:
dat.perifix <- dat %>%
  filter(Cruise %in% c("PERIFIX")) %>%
  group_by(Compound, compound.name.figure, Treatment, order, class) %>%
  reframe(Mean_nM = mean(Part.Conc.nM)) %>%
  mutate(Treatment = as.factor(Treatment)) %>%
  mutate(Treatment = fct_relevel(Treatment, c("Tote", "C", "F", "P", "PF", "N", 
                                              "NF", "NP", "NPF")))


dat.peri.carbon <- dat %>%
  filter(Cruise %in% c("PERIFIX")) %>%
 # filter(!Compound == "Isethionic acid") %>%
  group_by(Part.SampID, Treatment) %>%
  summarise(Tot.nM.osmo = sum(Part.Conc.nM),
            Carbon_estiamte_osmo_ng_L = Tot.nM.osmo^0.976*660)


##relative plot with means:
perifix.fig <- ggplot(dat.perifix, aes(y = Mean_nM, x = Treatment, fill = reorder(compound.name.figure, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15, position = "fill") +
  scale_fill_manual(values = compound.pal.fig) +
  #  scale_fill_manual(values = stepped2(n = 20)) +
  theme_test() +
  # coord_flip() +
  guides(fill=guide_legend(ncol=2)) +
  theme(legend.position = "right", 
        axis.text.x = element_text(angle = 90)) +
  
  scale_y_continuous(expand = c(0,NA,NA,NA)) +
  #  geom_point() +
  # scale_y_reverse() +
  #  facet_wrap(.~station.name, scales = "free_y", ncol = 2) +
  ylab("Mole Fraction") +
  xlab("Treatment") +
  labs(fill = "Compound") 
perifix.fig

ggsave(perifix.fig, file = "Figures/Outputs/PERIFIX_RelativeConcentration_Plots.png",
       height = 5, width = 7, scale = 1.3, dpi = 600)






###Make plot for presentation:



dat.perifix.pres <- dat %>%
  filter(Cruise %in% c("PERIFIX")) %>%
  group_by(Compound, compound.name.figure, Treatment, order, class) %>%
  reframe(Mean_nM = mean(Part.Conc.nM)) %>%
  mutate(Treatment = as.factor(Treatment)) %>%
  mutate(Treatment = fct_relevel(Treatment, c("Tote", "C", "P", "F", "PF", "NPF", "NP", "NF", "N"))) %>%
  filter(!Treatment == "Tote") %>%
  mutate(N_status = case_when(Treatment %in% c("N", "NP", "NF", "NPF") ~ "+N",
                              TRUE ~ "-N")) %>%
  group_by(compound.name.figure, order, N_status) %>%
  reframe(Mean_nM = mean(Mean_nM)) 


##relative plot with means for PhD presentation:
perifix.fig.pres <- ggplot(dat.perifix.pres, aes(y = Mean_nM, x = N_status, fill = reorder(compound.name.figure, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15, position = "fill") +
  scale_fill_manual(values = compound.pal.fig) +
  #  scale_fill_manual(values = stepped2(n = 20)) +
  theme_test() +
  # coord_flip() +
  guides(fill=guide_legend(ncol=2)) +
  scale_y_continuous(expand = c(0,NA,NA,NA)) +
  #  geom_point() +
  # scale_y_reverse() +
  #  facet_wrap(.~station.name, scales = "free_y", ncol = 2) +
  ylab("Mole Fraction") +
  xlab("Treatment") +
  labs(fill = "Compound") 
perifix.fig.pres

ggsave(perifix.fig.pres, file = "Figures/Outputs/Defense_PERIFIX_RelativeConcentration_Plots.pdf",
       height = 4, width = 4, scale = 1.2, dpi = 600)
