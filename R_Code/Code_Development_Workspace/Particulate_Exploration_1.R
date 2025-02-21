



library(tidyverse)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")


##Define inputs:
part.file <- "Intermediates/Particulate_Quant_Output.csv"
meta.file <- "Intermediates/All_metadata_information.csv"

###load in data and combine with metadata
part.dat <- read_csv(part.file)
meta.dat <- read_csv(meta.file)


####
dat.all <- left_join(part.dat, meta.dat) %>%
  rename("Compound" = Name)


dat.depth.profile <- dat.all %>%
  filter(Cruise %in% c("G4_DepthProfiles", "G3_DepthProfiles")) %>%
  filter(Compound %in% compound.order$Compound) %>%
  group_by(Cruise, station, Compound, depth_m) %>%
  reframe(Mean_nM = mean(nM.in.smp),
          Mean_nM_C = mean(nM_C),
          Mean_nM_N = mean(nM_N)) %>%
  mutate(station.name = case_when(Cruise == "G4_DepthProfiles" & station == 4 ~ "G4 Station 04 (19.6 N)",
                             Cruise == "G4_DepthProfiles" & station == 7 ~ "G4 Station 07 (10.7 N)",
                             Cruise == "G4_DepthProfiles" & station == 9 ~ "G4 Station 09 (4.7 N)",
                             Cruise == "G4_DepthProfiles" & station == 11 ~ "G4 Station 11 (0.14 N)",
                             Cruise == "G3_DepthProfiles" & station == 4 ~ "G3 Station 04 (41.4 N)",
                             Cruise == "G3_DepthProfiles" & station == 5 ~ "G3 Station 05 (37.0 N)",
                             Cruise == "G3_DepthProfiles" & station == 6 ~ "G3 Station 06 (33.0 N)")) %>%
  left_join(., compound.order) %>%
  filter(!is.na(station.name))


###Depth Profile Plot:

#absolute concentrations
g4dp.fig1 <- ggplot(dat.depth.profile, aes(y = Mean_nM, x = reorder(as.factor(depth_m), -depth_m), fill = reorder(Compound, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15) +
  scale_fill_manual(values = compound.pal) +
  #  scale_fill_manual(values = stepped2(n = 20)) +
  theme_test() +
  coord_flip() +
  guides(fill=guide_legend(ncol=1)) +
  theme(legend.position = "right", strip.text = element_text(size = 18)) +
  #  geom_point() +
  # scale_y_reverse() +
  facet_wrap(.~station.name, scales = "free_y", ncol = 2) +
  ylab("Concentration (nM Compound)") +
  xlab("Depth (m)") +
  labs(fill = "Compound") 
g4dp.fig1
ggsave(g4dp.fig1, filename = "R_Code/Code_Development_Workspace/DepthProfile_Fig1.pdf", height = 13, width = 10)


#relative concentrations
g4dp.fig2 <- ggplot(dat.depth.profile, aes(y = Mean_nM, x = reorder(as.factor(depth_m), -depth_m), fill = reorder(Compound, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15, position = "fill") +
  scale_fill_manual(values = compound.pal) +
  #  scale_fill_manual(values = stepped2(n = 20)) +
  theme_test() +
  coord_flip() +
  guides(fill=guide_legend(ncol=1)) +
  theme(legend.position = "right", strip.text = element_text(size = 18)) +
  #  geom_point() +
  # scale_y_reverse() +
  facet_wrap(.~station.name, scales = "free_y", ncol = 2) +
  ylab("Concentration (nM Compound)") +
  xlab("Depth (m)") +
  labs(fill = "Compound") 
g4dp.fig2
ggsave(g4dp.fig2, filename = "R_Code/Code_Development_Workspace/DepthProfile_Fig2.pdf", height = 13, width = 10)


##Make plots with all samples:
dat.depth.profile.all.smps <- dat.all %>%
  filter(Cruise %in% c("G4_DepthProfiles", "G3_DepthProfiles")) %>%
  filter(Compound %in% compound.order$Compound) %>%
#  group_by(Cruise, station, Compound, depth_m) %>%
#  reframe(Mean_nM = mean(nM.in.smp),
#          Mean_nM_C = mean(nM_C),
#          Mean_nM_N = mean(nM_N)) %>%
  mutate(station.name = case_when(Cruise == "G4_DepthProfiles" & station == 4 ~ "G4 Station 04 (19.6 N)",
                                  Cruise == "G4_DepthProfiles" & station == 7 ~ "G4 Station 07 (10.7 N)",
                                  Cruise == "G4_DepthProfiles" & station == 9 ~ "G4 Station 09 (4.7 N)",
                                  Cruise == "G4_DepthProfiles" & station == 11 ~ "G4 Station 11 (0.14 N)",
                                  Cruise == "G3_DepthProfiles" & station == 4 ~ "G3 Station 04 (41.4 N)",
                                  Cruise == "G3_DepthProfiles" & station == 5 ~ "G3 Station 05 (37.0 N)",
                                  Cruise == "G3_DepthProfiles" & station == 6 ~ "G3 Station 06 (33.0 N)")) %>%
  left_join(., compound.order) %>%
  filter(!is.na(station.name)) %>%
  mutate(SampID.fig = str_remove(SampID, "190715_Smp_"),
         SampID.fig = str_remove(SampID.fig, "240801_Smp_"))

##relative plot with all samples:
g4dp.fig3 <- ggplot(dat.depth.profile.all.smps, aes(y = nM.in.smp, x = reorder(SampID.fig, -depth_m), fill = reorder(Compound, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15, position = "fill") +
  scale_fill_manual(values = compound.pal) +
  #  scale_fill_manual(values = stepped2(n = 20)) +
  theme_test() +
  coord_flip() +
  guides(fill=guide_legend(ncol=1)) +
  theme(legend.position = "right", strip.text = element_text(size = 12)) +
  #  geom_point() +
  # scale_y_reverse() +
  facet_wrap(.~station.name, scales = "free_y", ncol = 2) +
  ylab("Concentration (nM Compound)") +
  xlab("Depth (m)") +
  labs(fill = "Compound") 
g4dp.fig3
ggsave(g4dp.fig3, filename = "R_Code/Code_Development_Workspace/DepthProfile_Fig3.pdf", height = 17, width = 12)

##absolute plot with all samples:
g4dp.fig4 <- ggplot(dat.depth.profile.all.smps, aes(y = nM.in.smp, x = reorder(SampID.fig, -depth_m), fill = reorder(Compound, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15) +
  scale_fill_manual(values = compound.pal) +
  #  scale_fill_manual(values = stepped2(n = 20)) +
  theme_test() +
  coord_flip() +
  guides(fill=guide_legend(ncol=1)) +
  theme(legend.position = "right", strip.text = element_text(size = 12)) +
  #  geom_point() +
  # scale_y_reverse() +
  facet_wrap(.~station.name, scales = "free_y", ncol = 2) +
  ylab("Concentration (nM Compound)") +
  xlab("Depth (m)") +
  labs(fill = "Compound") 
g4dp.fig4
ggsave(g4dp.fig4, filename = "R_Code/Code_Development_Workspace/DepthProfile_Fig4.pdf", height = 17, width = 12)





###############______PERIFIX

##Make plots with all samples:
dat.perifix <- dat.all %>%
  filter(Cruise %in% c("PERIFIX")) %>%
  filter(Compound %in% compound.order$Compound) %>%
  left_join(., compound.order) %>%
#  filter(!is.na(station.name)) %>%
  mutate(SampID.fig = str_remove(SampID, "230213_Smp_"))

##absolute plot with all samples:
perifix.fig1 <- ggplot(dat.perifix, aes(y = nM.in.smp, x = SampID.fig, fill = reorder(Compound, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15) +
  scale_fill_manual(values = compound.pal) +
  #  scale_fill_manual(values = stepped2(n = 20)) +
  theme_test() +
 # coord_flip() +
  guides(fill=guide_legend(ncol=2)) +
  theme(legend.position = "right", 
        axis.text.x = element_text(angle = 90)) +
  #  geom_point() +
  # scale_y_reverse() +
#  facet_wrap(.~station.name, scales = "free_y", ncol = 2) +
  ylab("Concentration (nM Compound)") +
  xlab("Sample") +
  labs(fill = "Compound") 
perifix.fig1
ggsave(perifix.fig1, filename = "R_Code/Code_Development_Workspace/PERIFIX_fig1.pdf", height = 8, width = 12)


##absolute plot with all samples:
perifix.fig2 <- ggplot(dat.perifix, aes(y = nM.in.smp, x = SampID.fig, fill = reorder(Compound, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15, position = "fill") +
  scale_fill_manual(values = compound.pal) +
  #  scale_fill_manual(values = stepped2(n = 20)) +
  theme_test() +
  # coord_flip() +
  guides(fill=guide_legend(ncol=2)) +
  theme(legend.position = "right", 
        axis.text.x = element_text(angle = 90)) +
  #  geom_point() +
  # scale_y_reverse() +
  #  facet_wrap(.~station.name, scales = "free_y", ncol = 2) +
  ylab("Concentration (nM Compound)") +
  xlab("Sample") +
  labs(fill = "Compound") 
perifix.fig2

ggsave(perifix.fig2, filename = "R_Code/Code_Development_Workspace/PERIFIX_fig2.pdf", height = 8, width = 12)



dat.perifix.sum <- dat.perifix %>%
  group_by(Compound, Treatment, order, class) %>%
  reframe(Mean_nM = mean(nM.in.smp),
          Mean_nM_C = mean(nM_C),
          Mean_nM_N = mean(nM_N)) 

##absolute plot with means:
perifix.fig3 <- ggplot(dat.perifix.sum, aes(y = Mean_nM, x = Treatment, fill = reorder(Compound, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15) +
  scale_fill_manual(values = compound.pal) +
  #  scale_fill_manual(values = stepped2(n = 20)) +
  theme_test() +
  # coord_flip() +
  guides(fill=guide_legend(ncol=2)) +
  theme(legend.position = "right", 
        axis.text.x = element_text(angle = 90)) +
  #  geom_point() +
  # scale_y_reverse() +
  #  facet_wrap(.~station.name, scales = "free_y", ncol = 2) +
  ylab("Concentration (nM Compound)") +
  xlab("Sample") +
  labs(fill = "Compound") 
perifix.fig3

ggsave(perifix.fig3, filename = "R_Code/Code_Development_Workspace/PERIFIX_fig3.pdf", height = 8, width = 12)



##relative plot with means:
perifix.fig4 <- ggplot(dat.perifix.sum, aes(y = Mean_nM, x = Treatment, fill = reorder(Compound, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15, position = "fill") +
  scale_fill_manual(values = compound.pal) +
  #  scale_fill_manual(values = stepped2(n = 20)) +
  theme_test() +
  # coord_flip() +
  guides(fill=guide_legend(ncol=2)) +
  theme(legend.position = "right", 
        axis.text.x = element_text(angle = 90)) +
  #  geom_point() +
  # scale_y_reverse() +
  #  facet_wrap(.~station.name, scales = "free_y", ncol = 2) +
  ylab("Concentration (nM Compound)") +
  xlab("Sample") +
  labs(fill = "Compound") 
perifix.fig4

ggsave(perifix.fig4, filename = "R_Code/Code_Development_Workspace/PERIFIX_fig4.pdf", height = 8, width = 12)


#Just the betaines
perifix.betaines.sum <- dat.perifix.sum %>%
  filter(class == "Betaine")

perifix.fig5 <- ggplot(perifix.betaines.sum, aes(y = Mean_nM, x = Treatment, fill = reorder(Compound, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15, position = "fill") +
  scale_fill_manual(values = compound.pal) +
  #  scale_fill_manual(values = stepped2(n = 20)) +
  theme_test() +
  # coord_flip() +
  guides(fill=guide_legend(ncol=2)) +
  theme(legend.position = "right", 
        axis.text.x = element_text(angle = 90)) +
  #  geom_point() +
  # scale_y_reverse() +
  #  facet_wrap(.~station.name, scales = "free_y", ncol = 2) +
  ylab("Concentration (nM Compound)") +
  xlab("Sample") +
  labs(fill = "Compound") 
perifix.fig5


dat.perifix.betaines <- dat.perifix %>%
  filter(class == "AA")

##absolute plot with all samples:
perifix.fig6 <- ggplot(dat.perifix.betaines, aes(y = nM.in.smp, x = SampID.fig, fill = reorder(Compound, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15, position = "fill") +
  scale_fill_manual(values = compound.pal) +
  #  scale_fill_manual(values = stepped2(n = 20)) +
  theme_test() +
  # coord_flip() +
  guides(fill=guide_legend(ncol=2)) +
  theme(legend.position = "right", 
        axis.text.x = element_text(angle = 90)) +
  #  geom_point() +
  # scale_y_reverse() +
  #  facet_wrap(.~station.name, scales = "free_y", ncol = 2) +
  ylab("Concentration (nM Compound)") +
  xlab("Sample") +
  labs(fill = "Compound") 
perifix.fig6







##################
perifix.stoich <- dat.perifix %>%
  group_by(SampID) %>%
  mutate(C.tot = sum(nM_C, na.rm = TRUE),
         N.tot = sum(nM_N, na.rm = TRUE),
         S.tot = sum(nM_S, na.rm = TRUE)) %>%
  mutate(C.N.tot = C.tot/N.tot,
         C.S.tot = C.tot/S.tot,
         N.S.tot = N.tot/S.tot) %>%
  mutate(Group_Carbon = case_when(class %in% c("Sugar") ~ "Yes",
                                  TRUE ~ "No"),
         Group_Nitrogen = case_when(class %in% c("AA", "Betaine", "TMAO", "Taurine") ~ "Yes",
                                    TRUE ~ "No"),
         Group_Sulfur = case_when(class %in% c("Sulfonium", "Sulfonate", "Taurine") ~ "Yes",
                                  TRUE ~ "No"))  %>%
  mutate(C.comp = sum(nM.in.smp[Group_Carbon == "Yes"]),
         N.comp = sum(nM.in.smp[Group_Nitrogen == "Yes"]),
         S.comp = sum(nM.in.smp[Group_Sulfur == "Yes"]),
         C.N.comp = C.comp/N.comp,
         C.S.comp = C.comp/S.comp,
         N.S.comp = N.comp/S.comp) %>%
  select(SampID, Treatment, C.tot:N.S.comp) %>%
  unique()

#Total stoichiometry plots:
ggplot(perifix.stoich, aes(x = Treatment, y = C.N.tot)) +
  geom_boxplot() + 
  geom_point(size = 3)

ggplot(perifix.stoich, aes(x = Treatment, y = C.S.tot)) +
  geom_boxplot() + 
  geom_point(size = 3)

ggplot(perifix.stoich, aes(x = Treatment, y = N.S.tot)) +
  geom_boxplot() + 
  geom_point(size = 3)


#Compoound stoichiometry plots:
ggplot(perifix.stoich, aes(x = Treatment, y = C.N.comp)) +
  geom_boxplot() + 
  geom_point(size = 3)

ggplot(perifix.stoich, aes(x = Treatment, y = C.S.comp)) +
  geom_boxplot() + 
  geom_point(size = 3)

ggplot(perifix.stoich, aes(x = Treatment, y = N.S.comp)) +
  geom_boxplot() + 
  geom_point(size = 3)













































































































