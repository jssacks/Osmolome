
library(tidyverse)
library(patchwork)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")





###define inputs
all.dat.file <- "Intermediates/Enviro_Osmo_Final_Dataset_with_metadata.csv"






#Read in data 
dat <- read_csv(all.dat.file) %>%
  left_join(., compound.order) %>%
  filter(!str_detect(Part.SampID, "Blk")) %>%
  filter(!is.na(compound.name.figure))


#Define regions:
dat.region <- dat %>%
  mutate(Region = case_when(Cruise == "KM1906" & Lat >= 35 ~ "NPTZ",
                            Cruise == "KM1906" & Lat < 35 ~ "NPSG",
                            Cruise == "TN397" & Long > -122 ~ "CUCP",
                            Cruise == "TN397" & Long < -122 & Lat > 10.5 ~ "NPSG",
                            Cruise == "TN397" & Lat < 10.5 & Lat > 4.5 ~ "NPEC",
                            Cruise == "TN397" & Lat < 4.5 ~ "PEDP",
                            Cruise == "RC078" ~ "SS")) %>%
  mutate(Region = as.factor(Region)) %>%
  mutate(Region = fct_relevel(Region, c("PEDP", "NPEC", "NPSG", "NPTZ", "CUCP", "SS"))) %>%
  filter(!is.na(class)) %>%
  mutate(class = as.factor(class)) %>%
  mutate(class = fct_relevel(class, c("AA", "Betaine", "Sugar", "Sulfonium", "Sulfonate", "Other")))


# 
# #Summarize values by class:
# 
# #Particulate:
# Part.sum.dat <- dat.region %>%
#   filter(Part.detected == "Yes") %>%
#   group_by(Part.SampID) %>%
#   mutate(sum.part.conc = sum(Part.Conc.nM)) %>%
#   group_by(Part.SampID, class, Region) %>%
#   reframe(class.sum.conc = sum(Part.Conc.nM),
#           class.rel.conc = class.sum.conc/sum.part.conc) %>%
#   unique() 
# 
# 
# ##Dissolved:
# Diss.sum.dat <- dat.region %>%
#   filter(Diss.detected == "Yes") %>%
#   group_by(Diss.SampID) %>%
#   mutate(sum.diss.conc = sum(Diss.Conc.nM)) %>%
#   group_by(Diss.SampID, class, Region) %>%
#   reframe(class.sum.conc = sum(Diss.Conc.nM),
#           class.rel.conc = class.sum.conc/sum.diss.conc) %>%
#   unique()
# 
# 
# 
# #Make Relative Composition Plots:
# part.class.region.plot <- ggplot(Part.sum.dat, aes(x = class, y = class.rel.conc)) +
#   geom_boxplot(aes(color = Region)) +
#   geom_point(aes(fill = Region), position = position_jitterdodge(jitter.width = 0.2), 
#              shape = 21, stroke = 0.1, color = "black") +
#   
#   # geom_jitter(aes(color = Region), shape = 21, width = 0.2) +
#   scale_color_manual(values = region.palette.7) +
#   scale_fill_manual(values = region.palette.7) +
#   theme_bw() +
#   ylab("Particulate Fraction")
# part.class.region.plot
# 
# 
# diss.class.region.plot <- ggplot(Diss.sum.dat, aes(x = class, y = class.rel.conc)) +
#   geom_boxplot(aes(color = Region)) +
#   geom_point(aes(fill = Region), position = position_jitterdodge(jitter.width = 0.1), 
#              shape = 21, stroke = 0.1, color = "black") +
#   
#   # geom_jitter(aes(color = Region), shape = 21, width = 0.2) +
#   scale_color_manual(values = region.palette.7) +
#   scale_fill_manual(values = region.palette.7) +
#   theme_bw() +
#   ylab("Dissolved Fraction")
# 
# diss.class.region.plot
# 


###__________Make Binned Latitude Relative Abundance Plots:_____________

####Gradient Survey Plot
lat.breaks <- seq(-3,50, by = 1)

#Organize data into different panels 
dat.panel <- dat %>%
  mutate(panel = case_when(Cruise == "TN397"  ~ "p1",
                           Cruise == "KM1906" ~ "p2",
                           Cruise == "RC078" ~ "p3"))


#Bin each panel into latitudinal or station Bins:

#Particulate: 
p1.dat.p <- dat.panel %>%
  filter(panel == "p1") %>%
  filter(Part.detected == "Yes") %>%
  mutate(lat_bin = cut(Lat, breaks = lat.breaks, include.lowest = TRUE, right = FALSE)) %>%
  group_by(lat_bin, compound.name.figure, order) %>%
  reframe(Mean.Part.nM = mean(Part.Conc.nM, na.rm = TRUE),
          Mean.Diss.nM = mean(Diss.Conc.nM, na.rm = TRUE))

p2.dat.p <- dat.panel %>%
  filter(panel == "p2") %>%
  filter(Part.detected == "Yes") %>%
  mutate(lat_bin = cut(Lat, breaks = lat.breaks, include.lowest = TRUE, right = FALSE)) %>%
  group_by(lat_bin, compound.name.figure, order) %>%
  reframe(Mean.Part.nM = mean(Part.Conc.nM, na.rm = TRUE),
          Mean.Diss.nM = mean(Diss.Conc.nM, na.rm = TRUE))

# p3.dat.p <- dat.panel %>%
#   filter(panel == "p3") %>%
#   filter(Part.detected == "Yes") %>%
#   mutate(Station = case_when(str_detect(Part.SampID, "U1") ~ "U1",
#                              str_detect(Part.SampID, "U2") ~ "U2",
#                              str_detect(Part.SampID, "U3") ~ "U3",
#                              str_detect(Part.SampID, "U4") ~ "U4",
#                              str_detect(Part.SampID, "U5") ~ "U5",
#                              str_detect(Part.SampID, "U6") ~ "U6",
#                              str_detect(Part.SampID, "S1") ~ "S1",
#                              str_detect(Part.SampID, "S2") ~ "S2",
#                              str_detect(Part.SampID, "U7") ~ "U7",
#                              str_detect(Part.SampID, "S3") ~ "S3")) %>%
#   group_by(Station, Long, compound.name.figure, order) %>%
#   reframe(Mean.Part.nM = mean(Part.Conc.nM, na.rm = TRUE),
#           Mean.Diss.nM = mean(Diss.Conc.nM, na.rm = TRUE))

p3.dat.p <- dat.panel %>%
  filter(panel == "p3") %>%
  filter(Part.detected == "Yes") %>%
  group_by(Station, compound.name.figure, order) %>%
  reframe(Mean.Part.nM = mean(Part.Conc.nM, na.rm = TRUE),
          Mean.Diss.nM = mean(Diss.Conc.nM, na.rm = TRUE))




##________________________Make particulate stacked bar charts:__________________

#panel 1
p.p1 <- ggplot(p1.dat.p, aes(x = lat_bin, y=Mean.Part.nM, fill = reorder(compound.name.figure, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15, position = "fill") +
  scale_fill_manual(values = compound.pal.fig)+
  guides(fill = guide_legend(ncol = 1)) +
  # scale_fill_manual(values = stepped2(n = 20)) +
  scale_y_continuous(expand = c(0,NA,NA,NA)) +
  theme_test() + 
  theme(axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  theme(legend.key.size = unit(0.4, "cm"), legend.text = element_text(size = 6),
        plot.title = element_text(hjust = 0.5)) +
  ylab("Particulate Mole Fraction") +
  xlab("Latitude") +
  labs(fill = "Compound") +
  ggtitle("TN397")
p.p1

#panel 2
p.p2 <- ggplot(p2.dat.p, aes(x = lat_bin, y=Mean.Part.nM, fill = reorder(compound.name.figure, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15, position = "fill") +
  scale_fill_manual(values = compound.pal.fig)+
  guides(fill = guide_legend(ncol = 1)) +
  #  scale_fill_manual(values = stepped2(n = 20)) +
  scale_y_continuous(expand = c(0,NA,NA,NA)) +
  theme_test() + 
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none") +
  theme(legend.key.size = unit(0.4, "cm"), legend.text = element_text(size = 6),
        plot.title = element_text(hjust = 0.5)) +
  ylab("Particulate Mole Fraction") +
  xlab("Latitude") +
  labs(fill = "Compound") +
  ggtitle("KM1906")
p.p2
# 
# #panel 3
# p.p3 <- ggplot(p3.dat.p, aes(x = reorder(Station, Lo), y=Mean.Part.nM, fill = reorder(compound.name.figure, order))) +
#   geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15, position = "fill") +
#   scale_fill_manual(values = compound.pal.fig)+
#   guides(fill = guide_legend(ncol = 1)) +
#   #  scale_fill_manual(values = stepped2(n = 20)) +
#   scale_y_continuous(expand = c(0,NA,NA,NA)) +
#   theme_test() + 
#   theme(axis.text.y = element_blank(),
#         axis.title.y = element_blank(),
#         axis.title.x = element_blank(),
#         axis.text.x = element_blank(),
#         legend.position = "none") +
#   theme(legend.key.size = unit(0.4, "cm"), legend.text = element_text(size = 6),
#         plot.title = element_text(hjust = 0.5)) +
#   ylab("Particulate Mole Fraction") +
#   xlab("Latitude") +
#   labs(fill = "Compound") +
#   ggtitle("TN397 - Leg 1")
# p.p3

#panel 3
p.p3 <- ggplot(p3.dat.p, aes(x = Station, y=Mean.Part.nM, fill = reorder(compound.name.figure, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15, position = "fill") +
  scale_fill_manual(values = compound.pal.fig)+
  guides(fill = guide_legend(ncol = 1)) +
  #  scale_fill_manual(values = stepped2(n = 20)) +
  scale_y_continuous(expand = c(0,NA,NA,NA)) +
  theme_test() + 
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none") +
  theme(legend.key.size = unit(0.4, "cm"), legend.text = element_text(size = 6),
        plot.title = element_text(hjust = 0.5)) +
  ylab("Particulate Mole Fraction") +
  xlab("Latitude") +
  labs(fill = "Compound") +
  ggtitle("RC078")
p.p3








###____________Organize Dissolved data: 

p1.dat.d <- dat.panel %>%
  filter(panel == "p1") %>%
  filter(!Diss.detected == "No") %>%
  mutate(lat_bin = cut(Lat, breaks = lat.breaks, include.lowest = TRUE, right = FALSE)) %>%
  group_by(lat_bin, compound.name.figure, order)  %>%
  reframe(Mean.Part.nM = mean(Part.Conc.nM, na.rm = TRUE),
          Mean.Diss.nM = mean(Diss.Conc.nM, na.rm = TRUE)) %>%
  full_join(., p1.dat.p %>% select(lat_bin) %>% unique())

p2.dat.d <- dat.panel %>%
  filter(panel == "p2") %>%
  filter(!Diss.detected == "No") %>%
  mutate(lat_bin = cut(Lat, breaks = lat.breaks, include.lowest = TRUE, right = FALSE)) %>%
  group_by(lat_bin, compound.name.figure, order)  %>%
  reframe(Mean.Part.nM = mean(Part.Conc.nM, na.rm = TRUE),
          Mean.Diss.nM = mean(Diss.Conc.nM, na.rm = TRUE)) %>%
  full_join(., p2.dat.p %>% select(lat_bin) %>% unique())
# 
# p3.dat.d <- dat.panel %>%
#   filter(panel == "p3") %>%
#   filter(!Diss.detected == "No") %>%
#   mutate(Station = case_when(str_detect(Part.SampID, "U1") ~ "U1",
#                              str_detect(Part.SampID, "U2") ~ "U2",
#                              str_detect(Part.SampID, "U3") ~ "U3",
#                              str_detect(Part.SampID, "U4") ~ "U4",
#                              str_detect(Part.SampID, "U5") ~ "U5",
#                              str_detect(Part.SampID, "U6") ~ "U6",
#                              str_detect(Part.SampID, "S1") ~ "S1",
#                              str_detect(Part.SampID, "S2") ~ "S2",
#                              str_detect(Part.SampID, "U7") ~ "U7",
#                              str_detect(Part.SampID, "S3") ~ "S3")) %>%
#   group_by(Station, Long, compound.name.figure, order)  %>%
#   reframe(Mean.Part.nM = mean(Part.Conc.nM, na.rm = TRUE),
#           Mean.Diss.nM = mean(Diss.Conc.nM, na.rm = TRUE)) %>%
#   full_join(., p3.dat.p %>% select(Station, Long) %>% unique())

p3.dat.d <- dat.panel %>%
  filter(panel == "p3") %>%
  filter(!Diss.detected == "No") %>%
  group_by(Station, compound.name.figure, order)  %>%
  reframe(Mean.Part.nM = mean(Part.Conc.nM, na.rm = TRUE),
          Mean.Diss.nM = mean(Diss.Conc.nM, na.rm = TRUE)) %>%
  full_join(., p3.dat.p %>% select(Station) %>% unique())






##________________________Make Dissolved stacked bar charts:__________________

#panel 1
d.p1 <- ggplot(p1.dat.d, aes(x = lat_bin, y=Mean.Diss.nM, fill = reorder(compound.name.figure, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15, position = "fill") +
  scale_fill_manual(values = compound.pal.fig)+
  guides(fill = guide_legend(ncol = 1)) +
  #  scale_fill_manual(values = stepped2(n = 20)) +
  scale_y_continuous(expand = c(0,NA,NA,NA)) +
  theme_test() + 
  theme(axis.text.y = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.key.size = unit(0.4, "cm"), legend.text = element_text(size = 6)) +
  ylab("Dissolved Mole Fraction") +
  xlab("Latitude") +
  labs(fill = "Compound")  
d.p1

#panel 2
d.p2 <- ggplot(p2.dat.d, aes(x = lat_bin, y=Mean.Diss.nM, fill = reorder(compound.name.figure, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15, position = "fill") +
  scale_fill_manual(values = compound.pal.fig)+
  guides(fill = guide_legend(ncol = 1)) +
  #  scale_fill_manual(values = stepped2(n = 20)) +
  scale_y_continuous(expand = c(0,NA,NA,NA)) +
  theme_test() + 
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.key.size = unit(0.4, "cm"), legend.text = element_text(size = 6)) +
  ylab("Particulate Mole Fraction") +
  xlab("Latitude") +
  labs(fill = "Compound") 
d.p2

# #panel 3
# d.p3 <- ggplot(p3.dat.d, aes(x = reorder(Station, Long), y=Mean.Diss.nM, fill = reorder(compound.name.figure, order))) +
#   geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15, position = "fill") +
#   scale_fill_manual(values = compound.pal.fig)+
#   guides(fill = guide_legend(ncol = 1)) +
#   #  scale_fill_manual(values = stepped2(n = 20)) +
#   scale_y_continuous(expand = c(0,NA,NA,NA)) +
#   theme_test() + 
#   theme(axis.text.y = element_blank(),
#         axis.title.y = element_blank(),
#         legend.position = "none",
#         axis.text.x = element_text(angle = 45, hjust = 1)) +
#   theme(legend.key.size = unit(0.4, "cm"), legend.text = element_text(size = 6)) +
#   #  ylab("Particulate Mole Fraction") +
#   xlab("Station") +
#   labs(fill = "Compound") 
# d.p3

#panel 4
d.p3 <- ggplot(p3.dat.d, aes(x = Station, y=Mean.Diss.nM, fill = reorder(compound.name.figure, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15, position = "fill") +
  scale_fill_manual(values = compound.pal.fig)+
  guides(fill = guide_legend(ncol = 1)) +
  #  scale_fill_manual(values = stepped2(n = 20)) +
  scale_y_continuous(expand = c(0,NA,NA,NA)) +
  theme_test() + 
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.key.size = unit(0.4, "cm"), legend.text = element_text(size = 6)) +
  xlab("Station") +
  labs(fill = "Compound") 
d.p3



comb.plot.1 <- (p.p1 + p.p2 + p.p3  + plot_layout(nrow = 1, widths = c(3.5, 2.5, 1))) / 
     (d.p1 + d.p2 + d.p3 + plot_layout(nrow = 1, widths = c(3.5, 2.5, 1))) + plot_layout(guides = "collect")
comb.plot.1

ggsave(comb.plot.1, filename = "Figures/Output_Oct25/Relative_Composition_Figure_Nov25.png",
       dpi = 1000, height = 4, width = 7.5, scale = 1.5)







