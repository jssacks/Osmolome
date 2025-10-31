

















library(tidyverse)
library(patchwork)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")





###define inputs
all.dat.file <- "Intermediates/Enviro_Osmo_Final_Dataset_with_metadata.csv"
fc.analysis.file <- "Intermediates/Fold_Change_Analysis_Stats.csv"



#Read in data 
dat <- read_csv(all.dat.file) %>%
  left_join(., compound.order) %>%
  filter(!str_detect(Part.SampID, "Blk"))


#Define regions:
dat.region <- dat %>%
  mutate(Region = case_when(Cruise == "KM1906" & Lat >= 35 ~ "NPTZ",
                            Cruise == "KM1906" & Lat < 35 ~ "NPSG",
                            Cruise == "TN397" & Long > -122 ~ "CUCP",
                            Cruise == "TN397" & Long < -122 & Lat > 10.5 ~ "NPSG",
                            Cruise == "TN397" & Lat < 10.5 & Lat > 4.5 ~ "NPEC",
                            Cruise == "TN397" & Lat < 4.5 ~ "PEDP",
                            Cruise == "RC078" ~ "SS"))


##Pull out gradients data and calculate average sample values of each osmolyte class:
dat.gradients <- dat.region %>%
  filter(Cruise %in% c("KM1906", "TN397")) %>%
  filter(!Region == "CUCP") %>%
  filter(Part.detected == "Yes") %>%
  group_by(Part.SampID, Cruise, Lat, Long, Region, class) %>%
  reframe(sum.part.conc = sum(Part.Conc.nM, na.rm = TRUE)) %>%
  pivot_wider(id_cols = c(Part.SampID, Lat, Long, Region, Cruise), names_from = class, values_from = sum.part.conc) %>%
  mutate(N_Sugar_ratio = Sugar/(AA+Betaine),
         N_Sulfonate_ratio = Sulfonate/(AA+Betaine),
         N_Sulfonium_ratio = Sulfonium/(AA+Betaine)) %>%
  mutate(Cruise = as.factor(Cruise)) %>%
  mutate(Cruise = fct_relevel(Cruise, c("TN397", "KM1906")))



##Make Plots:
ggplot(dat.gradients, aes(x = Lat, y = N_Sugar_ratio)) +
  geom_smooth(color = "black", fill = "lightgray", alpha = 0.3, size = 0.2)+
  geom_point(aes(fill = Region), shape = 21, stroke = 0.15, size = 2, alpha = 0.9) +
  facet_wrap(.~Cruise, scales = "free") +
  scale_fill_manual(values = region.palette.7) +
  theme_test() +
  ylab("Sugar/(AA+Betaine) Ratio") #+
 # geom_segment(aes(x = -2, y = 0.2, xend = 3, yend = 0.2), size = 0.2)

ggplot(dat.gradients, aes(x = Lat, y = N_Sulfonate_ratio)) +
  geom_smooth(color = "black", fill = "lightgray", alpha = 0.3, size = 0.2)+
  geom_point(aes(fill = Region), shape = 21, stroke = 0.1, size = 2, color = "black") +
  facet_wrap(.~Cruise, scales = "free") +
  scale_fill_manual(values = region.palette.7) +
  theme_test() +
  ylab("Sulfonate/(AA+Betaine) Ratio")

ggplot(dat.gradients, aes(x = Lat, y = N_Sulfonium_ratio)) +
  geom_smooth(color = "black", fill = "lightgray", alpha = 0.2, size = 0.2)+
  geom_point(aes(fill = Region), shape = 21, stroke = 0.1, size = 2, color = "black") +
  facet_wrap(.~Cruise, scales = "free") +
  scale_fill_manual(values = region.palette.7) +
  theme_test() +
  ylab("Sulfonium/(AA+Betaine) Ratio") +
 geom_segment(aes(x = -2.5, y = 0.16, xend = 3.5, yend = 0.16), size = 0.2) +
  geom_segment(aes(x = 4.5, y = 0.15, xend = 9, yend = 0.15), size = 0.2) +
  geom_segment(aes(x = 11, y = 0.07, xend = 30, yend = 0.07), size = 0.2)




###Fold Change Analysis
fc.dat <- read_csv(fc.analysis.file)




##Break up data into each comparison:
peri.fc.dat <- fc.dat %>%
  select(compound.name.figure, peri.log2fc, peri.p, peri.comparison, peri_behavior, behavior_sum)
  
g4.fc.dat <- fc.dat %>%
  select(compound.name.figure, g4.log2fc, g4.p, g4.comparison, g4_behavior, behavior_sum)
  
g3.fc.dat <- fc.dat %>%
  select(compound.name.figure, g3.log2fc, g3.p, g3.comparison, g3_behavior, behavior_sum)


###Pull out compounds for labels:
label.dat <- fc.dat %>%
  filter(compound.name.figure %in% c("Homarine", "Isethionic acid", "Taurine", "DHPS", "GG", "Sucrose", "Trehalose", "Betonicine", 
                                     "Arsenobetaine", "Proline betaine", "DMSA"))

#make individual labels for each comparison:
label.dat.peri <- fc.dat %>%
  filter(compound.name.figure %in% c("Isethionic acid", "GG", 
                                     "Arsenobetaine", "Proline betaine"))

label.dat.g4 <- fc.dat %>%
  filter(compound.name.figure %in% c("Homarine", "Isethionic acid", "Taurine", "DHPS", "GG", "Trehalose", "Betonicine", 
                                     "DMSA"))

label.dat.g3 <- fc.dat %>%
  filter(compound.name.figure %in% c("Homarine", "Isethionic acid", "Taurine", "DHPS", "GG", "Sucrose", "Trehalose", "Betonicine", 
                                      "DMSA"))





library(ggrepel)

##Make Base Plots:
peri.fc.plot <- ggplot(peri.fc.dat, aes(x = peri.log2fc, y = -log10(peri.p))) +
  geom_vline(xintercept = 0, color = "gray20", size = 0.3, linetype = "dashed") +
  geom_hline(yintercept = -log10(0.05), color = "gray20", linetype = "dashed", size = 0.3) +
  geom_point(shape = 21, aes(fill = behavior_sum), stroke = 0.2, size = 3) +
  scale_fill_manual(values = volcano.palette) +
  theme_test() +
  xlim(c(-5, 5)) +
  geom_text_repel(data = label.dat.peri, aes(x = peri.log2fc, y = -log10(peri.p), label = compound.name.figure),
                        label.size = 0.1,
                        size = 3,
                        min.segment.length = 0.01, 
                        point.padding = 0, 
                        box.padding = 1,
                        segment.size = 0.25,
                        force = 5)
peri.fc.plot


##Make Base Plots:
g4.fc.plot <- ggplot(g4.fc.dat, aes(x = g4.log2fc, y = -log10(g4.p))) +
  geom_vline(xintercept = 0, color = "gray20", size = 0.3, linetype = "dashed") +
  geom_hline(yintercept = -log10(0.05), color = "gray20", linetype = "dashed", size = 0.3) +
  geom_point(shape = 21, aes(fill = behavior_sum), stroke = 0.2, size = 3) +
  scale_fill_manual(values = volcano.palette) +
  theme_test() +
  xlim(c(-3.5, 3.5)) +
  geom_text_repel(data = label.dat.g4, aes(x = g4.log2fc, y = -log10(g4.p), label = compound.name.figure),
                   label.size = 0.1,
                   size = 3,
                   min.segment.length = 0.01, 
                   point.padding = 0.5, 
                   box.padding = 1,
                   segment.size = 0.25,
                   force = 0.1)
g4.fc.plot


##Make Base Plots:
g3.fc.plot <- ggplot(g3.fc.dat, aes(x = g3.log2fc, y = -log10(g3.p))) +
  geom_vline(xintercept = 0, color = "gray20", size = 0.3, linetype = "dashed") +
  geom_hline(yintercept = -log10(0.05), color = "gray20", linetype = "dashed", size = 0.3) +
  geom_point(shape = 21, aes(fill = behavior_sum), stroke = 0.2, size = 3) +
  scale_fill_manual(values = volcano.palette) +
  theme_test()  +
  xlim(c(-3.5, 3.5)) +
  geom_text_repel(data = label.dat.g3, aes(x = g3.log2fc, y = -log10(g3.p), label = compound.name.figure),
                   size = 3,
                   min.segment.length = 0.1, 
                   point.padding = 1, 
                   box.padding = 1,
                   segment.size = 0.25,
                   force = 0.1,
                   nudge_y = 0.5) +
  scale_color_manual(values = volcano.palette) 
g3.fc.plot

































































































