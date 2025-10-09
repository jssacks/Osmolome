




library(tidyverse)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")


##Define inputs:
part.file <- "Intermediates/Particulate_Quant_Output.csv"
meta.file <- "Intermediates/All_metadata_information.csv"

###load in data and combine with metadata
part.dat <- read_csv(part.file) %>%
  filter(Cruise %in% c("G4_DepthProfiles", "G3_DepthProfiles"))
meta.dat <- read_csv(meta.file) %>%
  filter(Cruise %in% c("G4_DepthProfiles", "G3_DepthProfiles")) %>%
  rename(SampID = Part.SampID)


####
dat.all <- left_join(part.dat, meta.dat) %>%
  rename("Compound" = Name)


dat.depth.profile <- dat.all %>%
  filter(Cruise %in% c("G4_DepthProfiles", "G3_DepthProfiles")) %>%
  filter(Compound %in% compound.order$Compound) %>%
  group_by(Cruise, station, Compound, depth_m) %>%
  reframe(Mean_nM = mean(nM.in.smp),
          Mean_nM_C = mean(nM_C),
          Mean_nM_N = mean(nM_N))  %>%
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
ggsave(g4dp.fig1, filename = "Figures/Depth_Profiles/DepthProfile_Fig1.pdf", height = 13, width = 10)


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
ggsave(g4dp.fig2, filename = "Figures/Depth_Profiles/DepthProfile_Fig2.pdf", height = 13, width = 10)


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
ggsave(g4dp.fig3, filename = "Figures/Depth_Profiles/DepthProfile_Fig3.pdf", height = 17, width = 12)

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
ggsave(g4dp.fig4, filename = "Figures/Depth_Profiles/DepthProfile_Fig4.pdf", height = 17, width = 12)




#######depth profiles by total abundance:

#All Osmolytes:
sum.osmo.dat <- dat.depth.profile.all.smps %>%
  group_by(SampID, station.name, depth_m) %>%
  reframe(samp.osmo.nM = sum(nM.in.smp),
          samp.osmo.C.nM = sum(nM_C),
          samp.osmo.N.nM = sum(nM_N, na.rm = TRUE),
          samp.osmo.S.nM = sum(nM_S, na.rm = TRUE),
          samp.CN.ratio = samp.osmo.C.nM/samp.osmo.N.nM,
          samp.CS.ratio = samp.osmo.C.nM/samp.osmo.S.nM,
          samp.NS.ratio = samp.osmo.N.nM/samp.osmo.S.nM) %>%
  group_by(station.name, depth_m) %>%
  mutate(mean.osmo.nM = mean(samp.osmo.nM),
         mean.osmo.C.nM = mean(samp.osmo.C.nM),
         mean.osmo.N.nM = mean(samp.osmo.N.nM),
         mean.osmo.S.nM = mean(samp.osmo.S.nM),
         mean.CN.ratio = mean(samp.CN.ratio),
         mean.CS.ratio = mean(samp.CS.ratio),
         mean.NS.ratio = mean(samp.NS.ratio))


#total concentration:
tot.fig <- ggplot(sum.osmo.dat) +
  geom_point(aes(y = samp.osmo.nM, x = depth_m), alpha = 0.5) +
  geom_line(aes(y = mean.osmo.nM, x = depth_m), size = 0.5) +
  geom_point(aes(y = mean.osmo.nM, x = depth_m), size = 2) +
  scale_x_reverse() +
  coord_flip() +
  facet_wrap(.~station.name) +
  theme_bw() 
tot.fig
ggsave(tot.fig, filename = "Figures/Depth_Profiles/DepthProfile_total_conc.pdf", height = 8, width = 8)

#CN ratio
cn.plot <- ggplot(sum.osmo.dat) +
  geom_point(aes(y = samp.CN.ratio, x = depth_m), alpha = 0.5) +
  geom_line(aes(y = mean.CN.ratio, x = depth_m), size = 0.5, color = "darkred") +
  geom_point(aes(y = mean.CN.ratio, x = depth_m), size = 2, color = "darkred") +
  scale_x_reverse() +
  coord_flip() +
  facet_wrap(.~station.name) +
  theme_bw() 
cn.plot
ggsave(cn.plot, filename = "Figures/Depth_Profiles/DepthProfile_cn_ratio.pdf", height = 8, width = 8)


#CS ratio
cs.plot <- ggplot(sum.osmo.dat) +
  geom_point(aes(y = samp.CS.ratio, x = depth_m), alpha = 0.5) +
  geom_line(aes(y = mean.CS.ratio, x = depth_m), size = 0.5, color = "darkblue") +
  geom_point(aes(y = mean.CS.ratio, x = depth_m), size = 2, color = "darkblue") +
  scale_x_reverse() +
  coord_flip() +
  facet_wrap(.~station.name) +
  theme_bw() 
cs.plot
ggsave(cs.plot, filename = "Figures/Depth_Profiles/DepthProfile_cs_ratio.pdf", height = 8, width = 8)


#NS ratio
ns.plot <- ggplot(sum.osmo.dat) +
  geom_point(aes(y = samp.NS.ratio, x = depth_m), alpha = 0.5) +
  geom_line(aes(y = mean.NS.ratio, x = depth_m), size = 0.5, color = "#BA8E23") +
  geom_point(aes(y = mean.NS.ratio, x = depth_m), size = 2, color = "#BA8E23") +
  scale_x_reverse() +
  coord_flip() +
  facet_wrap(.~station.name) +
  theme_bw() 
ns.plot
ggsave(ns.plot, filename = "Figures/Depth_Profiles/DepthProfile_ns_ratio.pdf", height = 8, width = 8)





#Individual Compounds:

dat.depth.profile.2 <- dat.depth.profile %>%
  group_by(station.name, depth_m) %>%
  mutate(tot.nM = sum(Mean_nM),
         rel.abun = Mean_nM/tot.nM)


depth.profile.individual.compounds.1 <- ggplot(dat.depth.profile.2) +
  geom_line(aes(y = Mean_nM, x = depth_m, color = station.name), size = 0.5, alpha = 0.5) +
  geom_point(aes(y = Mean_nM, x = depth_m, color = station.name), size = 2, alpha = 0.5) +
  scale_x_reverse() +
  coord_flip() +
  facet_wrap(.~Compound, scales = "free", ncol = 5) +
  theme_bw() +
  theme(legend.position = "bottom") 
depth.profile.individual.compounds.1
ggsave(depth.profile.individual.compounds.1, filename = "Figures/Depth_Profiles/DepthProfile_indcomps_absolute_conc.pdf", height = 10, width = 8, scale = 1.25)




depth.profile.individual.compounds.2 <-ggplot(dat.depth.profile.2) +
  geom_line(aes(y = rel.abun, x = depth_m, color = station.name), size = 0.5, alpha = 0.5) +
  geom_point(aes(y = rel.abun, x = depth_m, color = station.name), size = 2, alpha = 0.5) +
  scale_x_reverse() +
  coord_flip() +
  facet_wrap(.~Compound, scales = "free", ncol = 5) +
  theme_bw() +
  theme(legend.position = "bottom") 
depth.profile.individual.compounds.2
ggsave(depth.profile.individual.compounds.2, filename = "Figures/Depth_Profiles/DepthProfile_indcomps_rel_conc.pdf", height = 10, width = 8, scale = 1.25)


dat.depth.profile.2.200m <- dat.depth.profile.2 %>%
  filter(depth_m < 201)

depth.profile.individual.compounds.200m <-ggplot(dat.depth.profile.2.200m) +
  geom_line(aes(y = rel.abun, x = depth_m, color = station.name), size = 0.5, alpha = 0.5) +
  geom_point(aes(y = rel.abun, x = depth_m, color = station.name), size = 2, alpha = 0.5) +
  scale_x_reverse() +
  coord_flip() +
  facet_wrap(.~Compound, scales = "free", ncol = 5) +
  theme_bw() +
  theme(legend.position = "bottom") 
depth.profile.individual.compounds.200m

depth.profile.individual.compounds.200m <-ggplot(dat.depth.profile.2.200m) +
  geom_line(aes(y = Mean_nM, x = depth_m, color = station.name), size = 0.5, alpha = 0.5) +
  geom_point(aes(y = Mean_nM, x = depth_m, color = station.name), size = 2, alpha = 0.5) +
  scale_x_reverse() +
  coord_flip() +
  facet_wrap(.~Compound, scales = "free", ncol = 5) +
  theme_bw() +
  theme(legend.position = "bottom") 
depth.profile.individual.compounds.200m
#ggsave(depth.profile.individual.compounds.2, filename = "Figures/Depth_Profiles/DepthProfile_indcomps_rel_conc.pdf", height = 10, width = 8, scale = 1.25)

#ggsave(depth.profile.individual.compounds.2, filename = "Figures/Depth_Profiles/DepthProfile_indcomps_rel_conc.pdf", height = 10, width = 8, scale = 1.25)








#### Pull out just a single Depth Profile:
g4.s9.dat <- dat.depth.profile %>%
  filter(station %in% c(6, 9)) #%>%
 # filter(Cruise == "G4_DepthProfiles")


#absolute concentrations
g4.s9.dp.fig1 <- ggplot(g4.s9.dat, aes(y = Mean_nM, x = reorder(as.factor(depth_m), -depth_m), fill = reorder(compound.name.figure, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15) +
  scale_fill_manual(values = compound.pal.fig) +
  #  scale_fill_manual(values = stepped2(n = 20)) +
  theme_test() +
  coord_flip() +
  guides(fill=guide_legend(ncol=1)) +
  theme(legend.position = "right", strip.text = element_text(size = 12)) +
  #  geom_point() +
  # scale_y_reverse() +
  facet_wrap(station.name~., scales = "free_y", ncol = 1) +
  ylab("Concentration (nM Compound)") +
  xlab("Depth (m)") +
  labs(fill = "Compound") 
g4.s9.dp.fig1
ggsave(g4.s9.dp.fig1, filename = "Figures/Depth_Profiles/Pres_DepthProfile_Fig1.pdf", height = 7, width = 6, scale = 1.1)


#relative concentrations
g4.s9.dp.fig2 <- ggplot(g4.s9.dat, aes(y = Mean_nM, x = reorder(as.factor(depth_m), -depth_m), fill = reorder(compound.name.figure, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15, position = "fill") +
  scale_fill_manual(values = compound.pal.fig) +
  #  scale_fill_manual(values = stepped2(n = 20)) +
  theme_test() +
  coord_flip() +
  guides(fill=guide_legend(ncol=1)) +
  theme(legend.position = "right", strip.text = element_text(size = 12)) +
  #  geom_point() +
  # scale_y_reverse() +
  facet_wrap(.~station.name, scales = "free_y", ncol = 1) +
  ylab("Concentration (nM Compound)") +
  xlab("Depth (m)") +
  labs(fill = "Compound") 
g4.s9.dp.fig2
ggsave(g4.s9.dp.fig2, filename = "Figures/Depth_Profiles/Pres_DepthProfile_Fig2.pdf", height = 7, width = 6, scale = 1.1)




