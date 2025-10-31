


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



































