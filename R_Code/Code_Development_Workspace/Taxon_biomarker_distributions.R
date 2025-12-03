


library(tidyverse)
library(patchwork)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")




###define inputs
all.dat.file <- "Intermediates/Enviro_Osmo_Final_Dataset_with_metadata.csv"






#Read in data 
dat <- read_csv(all.dat.file) %>%
  left_join(., compound.order) %>%
  filter(!str_detect(Part.SampID, "Blk")) %>%
  filter()


#Define latitudinal breaks:
lat.breaks <- seq(-3,50, by = 1)





#Organize data into different panels 
dat.panel <- dat %>%
  mutate(panel = case_when(Cruise == "TN397" & Long > -136 ~ "p3",
                           Cruise == "TN397" & Long < -136 ~ "p1",
                           Cruise == "KM1906" ~ "p2",
                           Cruise == "RC078" ~ "p4")) 


#Bin each panel into latitudinal or station Bins:

#Particulate: 

p1.dat.p <- dat.panel %>%
  filter(panel == "p1") %>%
  filter(Part.detected == "Yes") %>%
  mutate(lat_bin = cut(Lat, breaks = lat.breaks, include.lowest = TRUE, right = FALSE)) %>%
  group_by(Part.SampID) %>%
  mutate(sum.part.conc = sum(Part.Conc.nM),
         rel.part.conc = Part.Conc.nM/sum.part.conc) %>%
  filter(compound.name.figure %in% c(
    "Trigonelline", "Trehalose", "Sucrose", "Proline", "Isethionic acid", "Homarine",
    "Gonyol", "GG", "DMSA", "DHPS", "TMAB", "Proline betaine"))

p2.dat.p <- dat.panel %>%
  filter(panel == "p2") %>%
  filter(Part.detected == "Yes") %>%
  mutate(lat_bin = cut(Lat, breaks = lat.breaks, include.lowest = TRUE, right = FALSE)) %>%
  group_by(Part.SampID) %>%
  mutate(sum.part.conc = sum(Part.Conc.nM),
         rel.part.conc = Part.Conc.nM/sum.part.conc) %>%
  filter(compound.name.figure %in% c(
    "Trigonelline", "Trehalose", "Sucrose", "Proline", "Isethionic acid", "Homarine",
    "Gonyol", "GG", "DMSA", "DHPS", "TMAB", "Proline betaine"))

p3.dat.p <- dat.panel %>%
  filter(panel == "p3") %>%
  filter(Part.detected == "Yes") %>%
  mutate(Station = case_when(str_detect(Part.SampID, "U1") ~ "U1",
                             str_detect(Part.SampID, "U2") ~ "U2",
                             str_detect(Part.SampID, "U3") ~ "U3",
                             str_detect(Part.SampID, "U4") ~ "U4",
                             str_detect(Part.SampID, "U5") ~ "U5",
                             str_detect(Part.SampID, "U6") ~ "U6",
                             str_detect(Part.SampID, "S1") ~ "S1",
                             str_detect(Part.SampID, "S2") ~ "S2",
                             str_detect(Part.SampID, "U7") ~ "U7",
                             str_detect(Part.SampID, "S3") ~ "S3")) %>%
  group_by(Part.SampID) %>%
  mutate(sum.part.conc = sum(Part.Conc.nM),
         rel.part.conc = Part.Conc.nM/sum.part.conc) %>%
  filter(compound.name.figure %in% c(
    "Trigonelline", "Trehalose", "Sucrose", "Proline", "Isethionic acid", "Homarine",
    "Gonyol", "GG", "DMSA", "DHPS", "TMAB", "Proline betaine"))




##Plot absolute concentrations:
p1.plot.abs <- ggplot(p1.dat.p, aes(x = Lat, y = Part.Conc.nM)) +
  geom_smooth() +
  geom_point(shape = 21, fill = "white") +
  facet_wrap(.~compound.name.figure, scales = "free_y")
p1.plot.abs

p2.plot.abs <- ggplot(p2.dat.p, aes(x = Lat, y = Part.Conc.nM)) +
  geom_smooth() +
  geom_point(shape = 21, fill = "white") +
  facet_wrap(.~compound.name.figure, scales = "free_y")
p2.plot.abs

p3.plot.abs <- ggplot(p3.dat.p, aes(x = Lat, y = Part.Conc.nM)) +
  geom_smooth() +
  geom_point(shape = 21, fill = "white") +
  facet_wrap(.~compound.name.figure, scales = "free_y")
p3.plot.abs




##Plot Relative Concentrations::
p1.plot.rel <- ggplot(p1.dat.p, aes(x = Lat, y = rel.part.conc)) +
  geom_smooth() +
  geom_point(shape = 21, fill = "white") +
  facet_wrap(.~compound.name.figure, scales = "free_y")
p1.plot.rel

p2.plot.rel <- ggplot(p2.dat.p, aes(x = Lat, y = rel.part.conc)) +
  geom_smooth() +
  geom_point(shape = 21, fill = "white") +
  facet_wrap(.~compound.name.figure, scales = "free_y")
p2.plot.rel

p3.plot.rel <- ggplot(p3.dat.p, aes(x = Lat, y = rel.part.conc)) +
  geom_smooth() +
  geom_point(shape = 21, fill = "white") +
  facet_wrap(.~compound.name.figure, scales = "free_y")
p3.plot.rel




###figures for presentation: 

##Gradients 3:
g3.sml.dat <- p2.dat.p %>%
  filter(compound.name.figure %in% c("DMSA", "Homarine"))


g3.plot.abs <- ggplot(g3.sml.dat, aes(x = Lat, y = Part.Conc.nM)) +
  geom_vline(xintercept = 35, linetype = "dashed", color = "darkred") +
  geom_smooth(alpha = 0.2) +
  geom_point(shape = 21, fill = "white", size = 3) +
  facet_wrap(.~compound.name.figure, scales = "free_y", ncol = 1) +
  ylab("Particulate Concentration (nM)") 
 # theme_bw()
g3.plot.abs


ggsave(g3.plot.abs, file = "Figures/SCOPE_2025/biogeography_abs_plot.png", dpi = 800, height = 5, width = 5, scale = 0.85)

g3.plot.rel <- ggplot(g3.sml.dat, aes(x = Lat, y = rel.part.conc)) +
  geom_vline(xintercept = 35, linetype = "dashed", color = "darkred") +
  geom_smooth() +
  geom_point(shape = 21, fill = "white", size = 3) +
  facet_wrap(.~compound.name.figure, scales = "free_y", ncol = 1) +
  ylab("Relative Abundance")
g3.plot.rel


ggsave(g3.plot.rel, file = "Figures/SCOPE_2025/biogeography_rel_plot.png", dpi = 800, height = 5, width = 5, scale = 0.85)



#Gradients 4
g4.sml.dat <- p1.dat.p %>%
  filter(compound.name.figure %in% c("Proline")) %>%
  mutate(lt = hour(Local_Time),
         TOD = as.factor(case_when(lt > 3 & lt < 11 ~ "morning",
                                   lt > 10 & lt < 17 ~ "midday",
                                   lt > 16 & lt < 22 ~ "evening",
                                   TRUE ~ "night")),
         TOD = fct_relevel(TOD, c("morning", "midday", "evening", "night")))


g4.plot.abs <- ggplot(g4.sml.dat, aes(x = Lat, y = Part.Conc.nM)) +
  scale_fill_viridis(discrete = TRUE, direction = -1, option = "G", end = 0.95) +
  #geom_vline(xintercept = 35, linetype = "dashed", color = "darkred") +
  geom_smooth(alpha = 0.2) +
  geom_point(shape = 21, aes(fill = TOD), size = 3) +
#  facet_wrap(.~compound.name.figure, scales = "free_y", ncol = 1) +
  ylab("Particulate Concentration (nM)") 
# theme_bw()
g4.plot.abs

ggsave(g4.plot.abs, file = "Figures/SCOPE_2025/biogeography_proline_plot.png", dpi = 800, height = 4, width = 6)






















