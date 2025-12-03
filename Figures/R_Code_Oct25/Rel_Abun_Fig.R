


library(tidyverse)
library(patchwork)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")





###define inputs
all.dat.file <- "Intermediates/Enviro_Osmo_Final_Dataset_with_metadata.csv"






#Read in data 
dat <- read_csv(all.dat.file) %>%
  left_join(., compound.order) %>%
  filter(!str_detect(Part.SampID, "Blk"))  %>%
  filter(!Parent_ID == "TN397_S11_600_U_C")
  

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
  group_by(lat_bin, compound.name.figure, order) %>%
  reframe(Mean.Part.nM = mean(Part.Conc.nM, na.rm = TRUE))

p2.dat.p <- dat.panel %>%
  filter(panel == "p2") %>%
  filter(Part.detected == "Yes") %>%
  mutate(lat_bin = cut(Lat, breaks = lat.breaks, include.lowest = TRUE, right = FALSE)) %>%
  group_by(lat_bin, compound.name.figure, order) %>%
  reframe(Mean.Part.nM = mean(Part.Conc.nM, na.rm = TRUE))

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
  group_by(Station, Long, compound.name.figure, order) %>%
  reframe(Mean.Part.nM = mean(Part.Conc.nM, na.rm = TRUE))
  
p4.dat.p <- dat.panel %>%
  filter(panel == "p4") %>%
  filter(Part.detected == "Yes") %>%
  group_by(Station, compound.name.figure, order) %>%
  reframe(Mean.Part.nM = mean(Part.Conc.nM, na.rm = TRUE))




#Dissolved

p1.dat.d <- dat.panel %>%
  filter(panel == "p1") %>%
  filter(Diss.detected == "Yes") %>%
  mutate(lat_bin = cut(Lat, breaks = lat.breaks, include.lowest = TRUE, right = FALSE)) %>%
  group_by(lat_bin, compound.name.figure, order) %>%
  reframe(Mean.Diss.nM = mean(Diss.Conc.nM, na.rm = TRUE)) 

p2.dat.d <- dat.panel %>%
  filter(panel == "p2") %>%
  filter(Diss.detected == "Yes") %>%
  mutate(lat_bin = cut(Lat, breaks = lat.breaks, include.lowest = TRUE, right = FALSE)) %>%
  group_by(lat_bin, compound.name.figure, order) %>%
  reframe(Mean.Diss.nM = mean(Diss.Conc.nM, na.rm = TRUE))

p3.dat.d <- dat.panel %>%
  filter(panel == "p3") %>%
  filter(Diss.detected == "Yes") %>%
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
  group_by(Station, Long, compound.name.figure, order) %>%
  reframe(Mean.Diss.nM = mean(Diss.Conc.nM, na.rm = TRUE))

p4.dat.d <- dat.panel %>%
  filter(panel == "p4") %>%
  filter(Diss.detected == "Yes") %>%
  group_by(Station, compound.name.figure, order) %>%
  reframe(Mean.Diss.nM = mean(Diss.Conc.nM, na.rm = TRUE))























##________________________Make particulate stacked bar charts:__________________

#panel 1
p.p1 <- ggplot(p1.dat.p, aes(x = lat_bin, y=Mean.Part.nM, fill = reorder(compound.name.figure, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15, position = "fill") +
  scale_fill_manual(values = compound.pal.fig)+
  guides(fill = guide_legend(ncol = 1)) +
  #  scale_fill_manual(values = stepped2(n = 20)) +
  scale_y_continuous(expand = c(0,NA,NA,NA)) +
  theme_test() + 
  theme(axis.text.y = element_blank(),
        axis.title.x = element_blank()) +
  theme(legend.key.size = unit(0.4, "cm"), legend.text = element_text(size = 6),
        plot.title = element_text(hjust = 0.5)) +
  ylab("Particulate Mole Fraction") +
  xlab("Latitude") +
  labs(fill = "Compound") +
  ggtitle("TN397 - Legs 2 and 3")
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
        axis.title.x = element_blank()) +
  theme(legend.key.size = unit(0.4, "cm"), legend.text = element_text(size = 6),
        plot.title = element_text(hjust = 0.5))  +
  ylab("Particulate Mole Fraction") +
  xlab("Latitude") +
  labs(fill = "Compound") +
  ggtitle("KM1906")
p.p2

#panel 3
p.p3 <- ggplot(p3.dat.p, aes(x = reorder(Station, Long), y=Mean.Part.nM, fill = reorder(compound.name.figure, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15, position = "fill") +
  scale_fill_manual(values = compound.pal.fig)+
  guides(fill = guide_legend(ncol = 1)) +
  #  scale_fill_manual(values = stepped2(n = 20)) +
  scale_y_continuous(expand = c(0,NA,NA,NA)) +
  theme_test() + 
  theme(axis.text.y = element_blank(),
        axis.title.x = element_blank()) +
  theme(legend.key.size = unit(0.4, "cm"), legend.text = element_text(size = 6),
        plot.title = element_text(hjust = 0.5)) +
  ylab("Particulate Mole Fraction") +
  xlab("Latitude") +
  labs(fill = "Compound")  +
  ggtitle("TN397 - Leg 1")
p.p3

#panel 4
p.p4 <- ggplot(p4.dat.p, aes(x = Station, y=Mean.Part.nM, fill = reorder(compound.name.figure, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15, position = "fill") +
  scale_fill_manual(values = compound.pal.fig)+
  guides(fill = guide_legend(ncol = 1)) +
  #  scale_fill_manual(values = stepped2(n = 20)) +
  scale_y_continuous(expand = c(0,NA,NA,NA)) +
  theme_test() + 
  theme(axis.text.y = element_blank(),
        axis.title.x = element_blank()) +
  theme(legend.key.size = unit(0.4, "cm"), legend.text = element_text(size = 6),
        plot.title = element_text(hjust = 0.5)) +
  ylab("Particulate Mole Fraction") +
  xlab("Latitude") +
  labs(fill = "Compound") +
  ggtitle("RC078")
p.p4






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
        axis.title.x = element_blank()) +
  theme(legend.key.size = unit(0.4, "cm"), legend.text = element_text(size = 6)) +
  ylab("Particulate Mole Fraction") +
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
        axis.title.x = element_blank()) +
  theme(legend.key.size = unit(0.4, "cm"), legend.text = element_text(size = 6)) +
  ylab("Particulate Mole Fraction") +
  xlab("Latitude") +
  labs(fill = "Compound") 
d.p2

#panel 3
d.p3 <- ggplot(p3.dat.d, aes(x = reorder(Station, Long), y=Mean.Diss.nM, fill = reorder(compound.name.figure, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15, position = "fill") +
  scale_fill_manual(values = compound.pal.fig)+
  guides(fill = guide_legend(ncol = 1)) +
  #  scale_fill_manual(values = stepped2(n = 20)) +
  scale_y_continuous(expand = c(0,NA,NA,NA)) +
  theme_test() + 
  theme(axis.text.y = element_blank(),
        axis.title.x = element_blank()) +
  theme(legend.key.size = unit(0.4, "cm"), legend.text = element_text(size = 6)) +
  ylab("Particulate Mole Fraction") +
  xlab("Latitude") +
  labs(fill = "Compound") 
d.p3

#panel 4
d.p4 <- ggplot(p4.dat.d, aes(x = Station, y=Mean.Diss.nM, fill = reorder(compound.name.figure, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15, position = "fill") +
  scale_fill_manual(values = compound.pal.fig)+
  guides(fill = guide_legend(ncol = 1)) +
  #  scale_fill_manual(values = stepped2(n = 20)) +
  scale_y_continuous(expand = c(0,NA,NA,NA)) +
  theme_test() + 
  theme(axis.text.y = element_blank(),
        axis.title.x = element_blank()) +
  theme(legend.key.size = unit(0.4, "cm"), legend.text = element_text(size = 6)) +
  ylab("Particulate Mole Fraction") +
  xlab("Latitude") +
  labs(fill = "Compound") 
d.p4
















##______________Extra Code:___________

##Make G3 dissolved figure in N-space:
G3.N.dat <- dat.panel %>%
  filter(!is.na(Diss.Conc.N.nM)) %>%
  filter(panel == "p2") %>%
  filter(Diss.detected == "Yes") %>%
  mutate(lat_bin = cut(Lat, breaks = lat.breaks, include.lowest = TRUE, right = FALSE)) %>%
  group_by(lat_bin, compound.name.figure, order) %>%
  filter(!is.na(compound.name.figure)) %>%
  reframe(Mean.Diss.N.nM = mean(Diss.Conc.N.nM, na.rm = TRUE))

#panel 2
g3.diss.N <- ggplot(G3.N.dat, aes(x = lat_bin, y=Mean.Diss.N.nM, fill = reorder(compound.name.figure, order))) +
  geom_col(alpha = 0.9, width = 0.6, color = "black", size = 0.15) +
  scale_fill_manual(values = compound.pal.fig)+
  guides(fill = guide_legend(ncol = 1)) +
  #  scale_fill_manual(values = stepped2(n = 20)) +
  scale_y_continuous(expand = c(0,NA,NA,NA), limits = c(0, 130)) +
  theme_test() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#        axis.title.x = element_blank()) +
  theme(legend.key.size = unit(0.4, "cm"), legend.text = element_text(size = 6)) +
  ylab("Dissolved Concentration (nmol N/L)") +
  xlab("Latitude") +
  labs(fill = "Compound") 
g3.diss.N

ggsave(g3.diss.N, filename = "Figures/SCOPE_2025/G3_Diss_N.png", dpi = 800, height = 5, width = 10,
       scale = 0.9)



#make the same plot without TMAO:
G3.N.dat.noTMAO <- G3.N.dat %>%
  filter(!compound.name.figure == "TMAO")

###
g3.diss.N.noTMAO <- ggplot(G3.N.dat.noTMAO, aes(x = lat_bin, y=Mean.Diss.N.nM, fill = reorder(compound.name.figure, order))) +
  geom_col(alpha = 0.9, width = 0.6, color = "black", size = 0.15) +
  scale_fill_manual(values = compound.pal.fig)+
  guides(fill = guide_legend(ncol = 1)) +
  scale_y_continuous(expand = c(0,NA,NA,NA), limits = c(0, 65)) +
  #  scale_fill_manual(values = stepped2(n = 20)) +
  # scale_y_continuous(expand = c(0,NA,NA,NA)) +
  theme_test() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  #        axis.title.x = element_blank()) +
  theme(legend.key.size = unit(0.4, "cm"), legend.text = element_text(size = 6)) +
  ylab("Dissolved Concentration (nmol N/L)") +
  xlab("Latitude") +
  labs(fill = "Compound") 
g3.diss.N.noTMAO

ggsave(g3.diss.N.noTMAO, filename = "Figures/SCOPE_2025/G3_Diss_N_noTMAO.png", dpi = 1000, height = 5, width = 10,
       scale = 0.9)




































































































































