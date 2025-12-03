

















library(tidyverse)
library(patchwork)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")





###define inputs
all.dat.file <- "Intermediates/Enviro_Osmo_Final_Dataset_with_metadata.csv"
fc.analysis.file <- "Intermediates/Fold_Change_Analysis_Stats.csv"
peri.file <- "Intermediates/PERIFIX_Osmo_Meta_Dat_101325.csv"


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


peri.dat <- read_csv(peri.file)%>%
  left_join(., compound.order) %>%
  filter(!str_detect(Part.SampID, "Blk")) %>%
  filter(!is.na(compound.name.figure)) %>%
  mutate(Part.Conc.nM = case_when(Part.detected == "No" ~ Part.Impute.Conc.nM,
                                  TRUE ~ Part.Conc.nM)) %>%
  group_by(Part.SampID, Treatment, N_status, class, N, P, Fe) %>%
  reframe(sum.part.conc = sum(Part.Conc.nM, na.rm = TRUE)) %>%
  pivot_wider(id_cols = c(Part.SampID, Treatment, N_status, N, P, Fe), names_from = class, values_from = sum.part.conc) %>%
  mutate(N_Sugar_ratio = Sugar/(AA+Betaine),
         N_Sulfonate_ratio = Sulfonate/(AA+Betaine),
         N_Sulfonium_ratio = Sulfonium/(AA+Betaine)) %>%
  mutate(Treatment = as.factor(Treatment),
         N_status = as.factor(N_status),
         N = as.factor(N),
         P = as.factor(P),
         Fe = as.factor(Fe)) %>%
  filter(!Treatment == "Tote") %>%
  mutate(N_status_fig = case_when(N_status == "plus_N" ~ "+N",
                                  N_status == "minus_N" ~ "-N"))





#prepare G3 and G4 data for plotting:
dat.g3 <- dat.gradients %>%
  filter(Cruise == "KM1906") 

dat.g4 <- dat.gradients %>%
  filter(Cruise == "TN397") %>%
  full_join(., dat.g3 %>% select(Region) %>% unique())




##Make Plots:

#Sugars:

#Sug-G4
g4.sug.plot <- ggplot(dat.g4, aes(x = Lat, y = N_Sugar_ratio)) +
  geom_smooth(color = "black", fill = "lightgray", alpha = 0.4, size = 0.2)+
  geom_point(aes(fill = Region), shape = 21, stroke = 0.15, size = 3, alpha = 0.9) +
  # facet_wrap(.~Cruise, scales = "free") +
  scale_fill_manual(values = region.palette.7) +
  theme_test() +
  ylim(c(0, 0.325)) +
  ylab("Sugar/(AA+Betaine)")  +
  annotate("segment", x = -2.5, xend = 3.5, y = 0.22, yend = 0.22, 
           size = 0.2, color = "black") +
  annotate("segment", x = 4.5, xend = 9, y = 0.22, yend = 0.22, 
           size = 0.2, color = "black") +
  annotate("segment", x = 11, xend = 30, y = 0.22, yend = 0.22, 
           size = 0.2, color = "black") +
  annotate("text", x = 0, y = 0.24, label = "a",fontface = "italic") +
  annotate("text", x = 6.75, y = 0.24, label = "a", fontface = "italic") +
  annotate("text", x = 20, y = 0.24, label = "a", fontface = "italic") +
  annotate("text", x = 0, y = 0.31, label = "p = 0.11", fontface = "italic") +
  theme(axis.title.x = element_blank(),
        legend.position = "bottom") +
  ggtitle("TN397") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(fill = "Region:")
g4.sug.plot


#Sug-G3
g3.sug.plot <- ggplot(dat.g3, aes(x = Lat, y = N_Sugar_ratio)) +
  geom_smooth(color = "black", fill = "lightgray", alpha = 0.4, size = 0.2)+
  geom_point(aes(fill = Region), shape = 21, stroke = 0.15, size = 3, alpha = 0.9) +
  # facet_wrap(.~Cruise, scales = "free") +
  scale_fill_manual(values = region.palette.7) +
  theme_test() +
  ylab("Sugar/(AA+Betaine)") +
  annotate("segment", x = 22, xend = 34, y = 0.25, yend = 0.25, 
           size = 0.2, color = "black") +
  annotate("segment", x = 36, xend = 41, y = 0.32, yend = 0.32, 
           size = 0.2, color = "black") +
  annotate("text", x = 28, y = 0.275, label = "a", fontface = "italic") +
  annotate("text", x = 38.5, y = 0.345, label = "b", fontface = "italic") +
  annotate("text", x = 24, y = 0.4, label = "p = 0.002", fontface = "bold.italic") +
  theme(legend.position = "none", 
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  ggtitle("KM1906") +
  theme(plot.title = element_text(hjust = 0.5))
g3.sug.plot


#Sug-Peri
peri.sug.plot <- ggplot(peri.dat, aes(x = N_status_fig, y = N_Sugar_ratio)) +
  geom_boxplot(aes(color = N_status), width = 0.4) +
  geom_jitter(shape = 21, fill = "white", width = 0.2, size = 2, stroke = 0.5) +
  scale_color_manual(values = c("gray50", "black")) +
  theme_test() +
  theme(legend.position = "none") +
  xlab("Treatment") +
  ylab("Sugar/(AA+Betaine)") +
  annotate("text", x = 1.1, y = 1.4, label = "p = 0.0091", fontface = "bold.italic")  +
  theme(legend.position = "none", 
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  ggtitle("PERIFIX") +
  theme(plot.title = element_text(hjust = 0.5))
peri.sug.plot




###########________Sulfonate Plots__________

#Sulfonate-G4
g4.sulfonate.plot <- ggplot(dat.g4, aes(x = Lat, y = N_Sulfonate_ratio)) +
  geom_smooth(color = "black", fill = "lightgray", alpha = 0.4, size = 0.2)+
  geom_point(aes(fill = Region), shape = 21, stroke = 0.15, size = 3, alpha = 0.9) +
  # facet_wrap(.~Cruise, scales = "free") +
  scale_fill_manual(values = region.palette.7) +
  theme_test() +
  ylim(c(0, 0.5)) +
  ylab("Sulfonate/(AA+Betaine)")  +
  annotate("segment", x = -2.5, xend = 3.5, y = 0.38, yend = 0.38, 
           size = 0.2, color = "black") +
  annotate("segment", x = 4.5, xend = 9, y = 0.32, yend = 0.32, 
           size = 0.2, color = "black") +
  annotate("segment", x = 11, xend = 30, y = 0.22, yend = 0.22, 
           size = 0.2, color = "black") +
  annotate("text", x = 0, y = 0.41, label = "a",fontface = "italic") +
  annotate("text", x = 6.75, y = 0.35, label = "b", fontface = "italic") +
  annotate("text", x = 20, y = 0.25, label = "c", fontface = "italic") +
  annotate("text", x = 26, y = 0.48, label = "p = 5.27e-21", fontface = "bold.italic") +
  theme(legend.position = "none", 
        axis.title.x = element_blank())

g4.sulfonate.plot


#Sulfonate-G3
g3.sulfonate.plot <- ggplot(dat.g3, aes(x = Lat, y = N_Sulfonate_ratio)) +
  geom_smooth(color = "black", fill = "lightgray", alpha = 0.4, size = 0.2)+
  geom_point(aes(fill = Region), shape = 21, stroke = 0.15, size = 3, alpha = 0.9) +
  # facet_wrap(.~Cruise, scales = "free") +
  scale_fill_manual(values = region.palette.7) +
  theme_test() +
  ylab("Sulfonate/(AA+Betaine)") +
  annotate("segment", x = 22, xend = 34, y = 0.5, yend = 0.5, 
           size = 0.2, color = "black") +
  annotate("segment", x = 36, xend = 41, y = 1, yend = 1, 
           size = 0.2, color = "black") +
  annotate("text", x = 28, y = 0.59, label = "a", fontface = "italic") +
  annotate("text", x = 38.5, y = 1.09, label = "b", fontface = "italic") +
  annotate("text", x = 24, y = 1.6, label = "p = 1.18e-7", fontface = "bold.italic") +
  theme(legend.position = "none", 
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
g3.sulfonate.plot


#Sulfonate-G3
peri.sulfonate.plot <- ggplot(peri.dat, aes(x = N_status_fig, y = N_Sulfonate_ratio)) +
  geom_boxplot(aes(color = N_status), width = 0.5) +
  geom_jitter(shape = 21, fill = "white", width = 0.2, size = 2, stroke = 0.5) +
  scale_color_manual(values = c("gray50", "black")) +
  theme_test() +
  theme(legend.position = "none") +
  xlab("Treatment") +
  ylab("Sulfonate/(AA+Betaine) Ratio") +
  annotate("text", x = 1.1, y = 7, label = "p = 0.001", fontface = "bold.italic") +
  theme(legend.position = "none", 
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
peri.sulfonate.plot







###########________Sulfonium Plots__________

#Sulfonium-G4
g4.sulfonium.plot <- ggplot(dat.g4, aes(x = Lat, y = N_Sulfonium_ratio)) +
  geom_smooth(color = "black", fill = "lightgray", alpha = 0.4, size = 0.2)+
  geom_point(aes(fill = Region), shape = 21, stroke = 0.15, size = 3, alpha = 0.9) +
  # facet_wrap(.~Cruise, scales = "free") +
  scale_fill_manual(values = region.palette.7) +
  theme_test() +
  ylim(c(0, 0.45)) +
  ylab("Sulfonium/(AA+Betaine)")  +
  annotate("segment", x = -2.5, xend = 3.5, y = 0.2, yend = 0.2, 
           size = 0.2, color = "black") +
  annotate("segment", x = 4.5, xend = 9, y = 0.22, yend = 0.22, 
           size = 0.2, color = "black") +
  annotate("segment", x = 11, xend = 30, y = 0.38, yend = 0.38, 
           size = 0.2, color = "black") +
  annotate("text", x = 0, y = 0.23, label = "a",fontface = "italic") +
  annotate("text", x = 6.75, y = 0.25, label = "a", fontface = "italic") +
  annotate("text", x = 20, y = 0.41, label = "b", fontface = "italic") +
  annotate("text", x = 1, y = 0.44, label = "p = 2.53e-14", fontface = "bold.italic") +
  theme(legend.position = "none")
g4.sulfonium.plot



#Sulfoium-G3
g3.sulfonium.plot <- ggplot(dat.g3, aes(x = Lat, y = N_Sulfonium_ratio)) +
  geom_smooth(color = "black", fill = "lightgray", alpha = 0.4, size = 0.2)+
  geom_point(aes(fill = Region), shape = 21, stroke = 0.15, size = 3, alpha = 0.9) +
  # facet_wrap(.~Cruise, scales = "free") +
  scale_fill_manual(values = region.palette.7) +
  theme_test() +
  ylab("Sulfonium/(AA+Betaine)") +
  annotate("segment", x = 22, xend = 34, y = 0.8, yend = 0.8, 
           size = 0.2, color = "black") +
  annotate("segment", x = 36, xend = 41, y = 1.1, yend = 1.1, 
           size = 0.2, color = "black") +
  annotate("text", x = 28, y = 0.85, label = "a", fontface = "italic") +
  annotate("text", x = 38.5, y = 1.15, label = "a", fontface = "italic") +
  annotate("text", x = 24, y = 1.1, label = "p = 0.21", fontface = "italic") +
  theme(legend.position = "none", 
        axis.title.y = element_blank())
g3.sulfonium.plot


#Sulfonate-G3
peri.sulfonium.plot <- ggplot(peri.dat, aes(x = N_status_fig, y = N_Sulfonium_ratio)) +
  geom_boxplot(aes(color = N_status), width = 0.5) +
  geom_jitter(shape = 21, fill = "white", width = 0.2, size = 2, stroke = 0.5) +
  scale_color_manual(values = c("gray50", "black")) +
  theme_test() +
  theme(legend.position = "none") +
  xlab("Treatment") +
  ylab("Sulfonium/(AA+Betaine)") +
  annotate("text", x = 1.1, y = 0.7, label = "p = 0.0086", fontface = "bold.italic") +
  theme(legend.position = "none", 
        axis.title.y = element_blank())
peri.sulfonium.plot







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

#Perifix
label.dat.peri.pos <- fc.dat %>%
  filter(compound.name.figure %in% c("Isethionic acid", "GG"))

label.dat.peri.neg <- fc.dat %>%
  filter(compound.name.figure %in% c("Arsenobetaine", "Proline betaine"))

#Gradients 4
label.dat.g4.pos <- fc.dat %>%
  filter(compound.name.figure %in% c("Homarine", "Isethionic acid", "Taurine", "DHPS", "GG"))

label.dat.g4.neg <- fc.dat %>%
  filter(compound.name.figure %in% c("Trehalose", "Arsenobetaine", "DMSA"))

#Gradients 3
label.dat.g3.pos <- fc.dat %>%
  filter(compound.name.figure %in% c("Homarine", "Isethionic acid", "Taurine", "DHPS", "GG"))
label.dat.g3.neg <- fc.dat %>%
  filter(compound.name.figure %in% c("Sucrose", "Trehalose", "Arsenobetaine", "DMSA"))




library(ggrepel)

##Make Base Plots:
peri.fc.plot <- ggplot(peri.fc.dat, aes(x = peri.log2fc, y = -log10(peri.p))) +
  geom_vline(xintercept = 0, color = "gray20", size = 0.3, linetype = "dashed") +
  geom_hline(yintercept = -log10(0.05), color = "gray20", linetype = "dashed", size = 0.3) +
  geom_point(shape = 21, aes(fill = behavior_sum), stroke = 0.2, size = 4) +
  scale_fill_manual(values = volcano.palette) +
  theme_test() +
  xlim(c(-5, 5)) +
  geom_text_repel(data = label.dat.peri.pos, aes(x = peri.log2fc, y = -log10(peri.p), label = compound.name.figure),
                  label.size = 0.1,
                  size = 3,
                  min.segment.length = 0.01, 
                  point.padding = 0.5, 
                  box.padding = 1,
                  segment.size = 0.25,
                  force = 0.1,
                  xlim = c(2.5, NA))  +
  geom_text_repel(data = label.dat.peri.neg, aes(x = peri.log2fc, y = -log10(peri.p), label = compound.name.figure),
                  label.size = 0.1,
                  size = 3,
                  min.segment.length = 0.01, 
                  point.padding = 0.5, 
                  box.padding = 1,
                  segment.size = 0.25,
                  force = 0.1,
                  xlim = c(NA, -2),
                  ylim = c(2, NA)) +
  ylab("-log10(p)") +
  xlab("Log2FC(+N/-N)") +
  theme(legend.position = "bottom") +
  ggtitle("PERIFIX") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(fill = "Behavior:")
peri.fc.plot


##Make Base Plots:
g4.fc.plot <- ggplot(g4.fc.dat, aes(x = g4.log2fc, y = -log10(g4.p))) +
  geom_vline(xintercept = 0, color = "gray20", size = 0.3, linetype = "dashed") +
  geom_hline(yintercept = -log10(0.05), color = "gray20", linetype = "dashed", size = 0.3) +
  geom_point(shape = 21, aes(fill = behavior_sum), stroke = 0.2, size = 4) +
  scale_fill_manual(values = volcano.palette) +
  theme_test() +
  xlim(c(-4, 4)) +
  geom_text_repel(data = label.dat.g4.pos, aes(x = g4.log2fc, y = -log10(g4.p), label = compound.name.figure),
                   label.size = 0.1,
                   size = 3,
                   min.segment.length = 0.01, 
                   point.padding = 0.5, 
                   box.padding = 1,
                   segment.size = 0.25,
                   force = 0.1,
                   xlim = c(2.5, NA),
                   ylim = c(2, NA))  +
  geom_text_repel(data = label.dat.g4.neg, aes(x = g4.log2fc, y = -log10(g4.p), label = compound.name.figure),
                  label.size = 0.1,
                  size = 3,
                  min.segment.length = 0.01, 
                  point.padding = 0.5, 
                  box.padding = 1,
                  segment.size = 0.25,
                  force = 0.1,
                  xlim = c(NA, -2),
                  ylim = c(2, NA)) +
  ylab("-log10(p)") +
  xlab("Log2FC(PEDP/NPSG)") +
  theme(legend.position = "bottom") +
  ggtitle("TN397") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(fill = "Behavior:")
g4.fc.plot


##Make Base Plots:
g3.fc.plot <- ggplot(g3.fc.dat, aes(x = g3.log2fc, y = -log10(g3.p))) +
  geom_vline(xintercept = 0, color = "gray20", size = 0.3, linetype = "dashed") +
  geom_hline(yintercept = -log10(0.05), color = "gray20", linetype = "dashed", size = 0.3) +
  geom_point(shape = 21, aes(fill = behavior_sum), stroke = 0.2, size = 4) +
  scale_fill_manual(values = volcano.palette) +
  theme_test()  +
  xlim(c(-4.5, 4.5)) +
  geom_text_repel(data = label.dat.g3.pos, aes(x = g3.log2fc, y = -log10(g3.p), label = compound.name.figure),
                  label.size = 0.1,
                  size = 3,
                  min.segment.length = 0.01, 
                  point.padding = 0.5, 
                  box.padding = 1,
                  segment.size = 0.25,
                  force = 0.1,
                  xlim = c(2.5, NA),
                  ylim = c(1.5, NA))  +
  geom_text_repel(data = label.dat.g3.neg, aes(x = g3.log2fc, y = -log10(g3.p), label = compound.name.figure),
                  label.size = 0.1,
                  size = 3,
                  min.segment.length = 0.01, 
                  point.padding = 0.5, 
                  box.padding = 1,
                  segment.size = 0.25,
                  force = 0.1,
                  xlim = c(NA, -2),
                  ylim = c(2, NA)) +
  scale_color_manual(values = volcano.palette) +
  ylab("-log10(p)") +
  xlab("Log2FC(NPTZ/NPSG)") +
  theme(legend.position = "bottom") +
  ggtitle("KM1906") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(fill = "Behavior:")
g3.fc.plot







###Work to combine all plots:

full.comb.plot <- ((g4.fc.plot + g3.fc.plot + peri.fc.plot)/guide_area() + plot_layout(guides = "collect")) /
                  plot_spacer() /
                  ((g4.sug.plot + g3.sug.plot + peri.sug.plot + 
                     plot_layout(nrow = 1, widths = c(0.4, 0.4, 0.15))) /
                  (g4.sulfonate.plot + g3.sulfonate.plot + peri.sulfonate.plot + 
                    plot_layout(nrow = 1, widths = c(0.4, 0.4, 0.15))) /
                  (g4.sulfonium.plot + g3.sulfonium.plot + peri.sulfonium.plot + 
                    plot_layout(nrow = 1, widths = c(0.4, 0.4, 0.15))) /
                    guide_area() + plot_layout(guides = "collect", heights = c(3, 3, 3, 1))) +
                  plot_layout(guides = "collect", heights = c(2.5, 0.2, 0.2, 7.5))
  
full.comb.plot  

ggsave(full.comb.plot, filename = "Figures/Output_Oct25/Osmo_N_Response_Figure.png", 
       dpi = 800, height = 8, width = 7, scale = 1.5)






















































































