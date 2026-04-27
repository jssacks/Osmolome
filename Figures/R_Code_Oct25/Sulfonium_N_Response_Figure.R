




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







#Make osmolyte class ratio plots:

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
  theme(legend.position = "bottom") +
  ggtitle("TN397") +
  theme(plot.title = element_text(hjust = 0.5))
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
        axis.title.y = element_blank()) +
  ggtitle("KM1906") +
  theme(plot.title = element_text(hjust = 0.5))
g3.sulfonium.plot


#Sulfonium-PERIFIX
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
        axis.title.y = element_blank()) +
  ggtitle("PERIFIX") +
  theme(plot.title = element_text(hjust = 0.5))
peri.sulfonium.plot



## Make suppelmental plot for Sulfoniums:
sulfonium.sup.plot <- 
  (g4.sulfonium.plot + g3.sulfonium.plot + peri.sulfonium.plot +
     plot_layout(nrow = 1, widths = c(0.4, 0.4, 0.15))) /
  guide_area() +
  plot_layout(guides = "collect", heights = c(5,1)) +
  plot_annotation(tag_levels = 'a')
sulfonium.sup.plot

ggsave(sulfonium.sup.plot, filename = "Figures/Output_Oct25/Sulfonium_N_Response_Supp_Figure.png", 
       dpi = 900, height = 2.75, width = 7, scale = 1.5)
