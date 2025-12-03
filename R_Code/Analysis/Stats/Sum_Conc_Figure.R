





library(tidyverse)
library(patchwork)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")





###______________define inputs____________________

#Osmolytes
all.dat.file <- "Intermediates/Enviro_Osmo_Final_Dataset_with_metadata.csv"

#environmental data:
g3.file <- "Intermediates/G3_metadata_with_interpolations.csv"
g4.file <- "Intermediates/G4_metadata_with_interpolations.csv"
d1.file <- "Intermediates/RC078_MetaData_Compiled.csv"

#atp data:
atp.dat.file <- "Intermediates/Gradients_ATP_Enviro_Meta_Data.csv"




#Define binning scheme for data: 

#Define latitudinal breaks:
lat.breaks <- seq(-3,50, by = 1)
long.breaks <- seq(-150, -100, by = 1)





####______Make plots of environmental data______________


##load in atp data 
atp.dat <- read_csv(atp.dat.file) %>%
  filter(is.na(ATP_Flag)) %>%
  mutate(living_biomass_umolC_L = 250*PATP_ng_L/12.01/1000,
         sd_living_biomass_umolC_L = 250*SD_PATP_ng_L/12.01/1000) %>%
  select(Cruise, Lat, Long, living_biomass_umolC_L, sd_living_biomass_umolC_L) %>%
  rename()




##Load in g4 data:
g4.env.dat <- read_csv(g4.file) %>%
  mutate(Cruise = "TN397") %>%
  rename(Lat = lat,
         Long = lon) %>%
  select(Cruise, Lat, Long, chla, pc)  %>%
  full_join(., atp.dat %>% filter(Cruise == "TN397")) %>%
  mutate(panel = case_when(Cruise == "TN397" & Long > -136 ~ "p3",
                           Cruise == "TN397" & Long < -136 ~ "p1",
                           Cruise == "KM1906" ~ "p2",
                           Cruise == "RC078" ~ "p4")) 


#load in g3 data:
g3.env.dat <- read_csv(g3.file) %>%
  mutate(Cruise = "KM1906") %>%
  rename(Lat = lat,
         Long = lon) %>%
  select(Cruise, Lat, Long, chla, pc)  %>%
  full_join(., atp.dat %>% filter(Cruise == "KM1906")) %>%
  mutate(panel = case_when(Cruise == "TN397" & Long > -136 ~ "p3",
                           Cruise == "TN397" & Long < -136 ~ "p1",
                           Cruise == "KM1906" ~ "p2",
                           Cruise == "RC078" ~ "p4"))



##Dinimite data
d1.env.dat <- read_csv(d1.file) %>%
  mutate(Cruise = "RC078") %>%
  filter(depth_m < 16) #%>%
  rename(Lat = lat,
         Long = lon,
         poc = POC_uM,
         chla = Chl_fluor) %>%
  select(Station, Cruise, Lat, Long, poc, chla)  %>%
  mutate(panel = case_when(Cruise == "TN397" & Long > -136 ~ "p3",
                           Cruise == "TN397" & Long < -136 ~ "p1",
                           Cruise == "KM1906" ~ "p2",
                           Cruise == "RC078" ~ "p4")) %>%
  filter(Station %in% c("S2", "S3", "S5", "S6", "S7"))






###Organize Environmental Data for each panel:

#_________Panel 1:
p1.env.dat <- g4.env.dat %>%
  filter(panel == "p1") %>%
  mutate(lat_bin = cut(Lat, breaks = lat.breaks, include.lowest = TRUE, right = FALSE)) %>%
  group_by(Cruise, lat_bin) %>%
  reframe(mean.poc = mean(pc, na.rm = TRUE),
          sd.poc = sd(pc, na.rm = TRUE),
          mean.live = mean(living_biomass_umolC_L, na.rm = TRUE),
          sd.live = sd(living_biomass_umolC_L, na.rm = TRUE),
          mean.lat = mean(Lat)) 

p1.chla.dat <- g4.env.dat %>%
  filter(panel == "p1")

p1.atp.dat <- g4.env.dat %>%
  filter(panel == "p1")


#_________Panel 2:
p2.env.dat <- g3.env.dat %>%
  filter(panel == "p2") %>%
  mutate(lat_bin = cut(Lat, breaks = lat.breaks, include.lowest = TRUE, right = FALSE)) %>%
  group_by(Cruise, lat_bin) %>%
  reframe(mean.poc = mean(pc, na.rm = TRUE),
          sd.poc = sd(pc, na.rm = TRUE),
          mean.live = mean(living_biomass_umolC_L, na.rm = TRUE),
          sd.live = sd(living_biomass_umolC_L, na.rm = TRUE),
          mean.lat = mean(Lat)) 

p2.chla.dat <- g3.env.dat %>%
  filter(panel == "p2")

p2.atp.dat <- g3.env.dat %>%
  filter(panel == "p2")





#________Panel 3_____________________
p3.env.dat <- g4.env.dat %>%
  filter(panel == "p3") %>%
  mutate(long_bin = cut(Long, breaks = long.breaks, include.lowest = TRUE, right = FALSE)) %>%
  group_by(Cruise, long_bin) %>%
  reframe(mean.poc = mean(pc, na.rm = TRUE),
          sd.poc = sd(pc, na.rm = TRUE),
          mean.live = mean(living_biomass_umolC_L, na.rm = TRUE),
          sd.live = sd(living_biomass_umolC_L, na.rm = TRUE),
          mean.long = mean(Long)) 

p3.chla.dat <- g4.env.dat %>%
  filter(panel == "p3")

p3.atp.dat <- g4.env.dat %>%
  filter(panel == "p3")



##________Panel_4___________
p4.env.dat <- d1.env.dat %>%
  mutate(chla = chla*30) %>%
  pivot_longer(cols = poc:chla, names_to = "Parameter", values_to = "Value") 












#try making first figure:

#P1
p1.env <- ggplot(p1.env.dat) +
  theme_bw() +
  geom_point(data = p1.chla.dat, aes(x = Lat, y = chla*15), color = "yellowgreen", alpha = 0.5, size = 1) +
  geom_errorbar(aes(x = mean.lat, ymin = mean.poc-sd.poc, ymax = mean.poc+sd.poc), size = 0.5, width = 0.2) +
  geom_line(aes(x = mean.lat, y = mean.poc, group = Cruise)) +
  geom_point(aes(x = mean.lat, y = mean.poc), shape = 21, fill = "gray30", size = 2) +
  geom_point(data = p1.atp.dat, aes(x = Lat, y = living_biomass_umolC_L), shape = 22, size = 2, fill = "red", stroke = 0.2) +
  # 
  # geom_errorbar(aes(x = mean.lat, ymin = mean.live-sd.live, ymax = mean.live+sd.live), size = 0.5, width = 0.2, color = "darkred") +
  # geom_point(aes(x = mean.lat, y = mean.live), 
  #            fill = "red", shape = 22, size = 2.5, stroke = 0.2) +
  scale_y_continuous(name = "POC (uM)", sec.axis = sec_axis(trans=~.*(1/15), name=NULL), limits = c(0,5)) +
  theme(axis.text.y.right = element_text(color = "chartreuse4")) +
  theme(axis.title.x = element_blank())

p1.env



###______P2______________
p2.env <- ggplot(p2.env.dat) +
  theme_bw() +
  geom_point(data = p2.chla.dat, aes(x = Lat, y = chla*15), color = "yellowgreen", alpha = 0.5, size = 1) +
  geom_errorbar(aes(x = mean.lat, ymin = mean.poc-sd.poc, ymax = mean.poc+sd.poc), size = 0.5, width = 0.2) +
  geom_line(aes(x = mean.lat, y = mean.poc, group = Cruise)) +
  geom_point(aes(x = mean.lat, y = mean.poc), shape = 21, fill = "gray30", size = 2) +
  geom_errorbar(data = p2.atp.dat, aes(x = Lat, ymin = living_biomass_umolC_L-sd_living_biomass_umolC_L,  ymax = living_biomass_umolC_L+sd_living_biomass_umolC_L),
                                       size = 0.5, width = 0.2, color = "black") + 
  geom_point(data = p2.atp.dat, aes(x = Lat, y = living_biomass_umolC_L), shape = 22, size = 2, fill = "red", stroke = 0.2) +
  # 
  # geom_errorbar(aes(x = mean.lat, ymin = mean.live-sd.live, ymax = mean.live+sd.live), size = 0.5, width = 0.2, color = "darkred") +
  # geom_point(aes(x = mean.lat, y = mean.live), 
  #            fill = "red", shape = 22, size = 2.5, stroke = 0.2) +
  scale_y_continuous(name = NULL, sec.axis = sec_axis(trans=~.*(1/15), name=NULL), limits = c(0,10.5)) +
  theme(axis.text.y.right = element_text(color = "chartreuse4")) +
  theme(axis.title.x = element_blank())

p2.env



###_________P3___
p3.env <- ggplot(p3.env.dat) +
  theme_bw() +
  geom_point(data = p3.chla.dat, aes(x = Long, y = chla*15), color = "yellowgreen", alpha = 0.5, size = 1.5) +
  geom_errorbar(aes(x = mean.long, ymin = mean.poc-sd.poc, ymax = mean.poc+sd.poc), size = 0.5, width = 0.2) +
  geom_line(aes(x = mean.long, y = mean.poc, group = Cruise)) +
  geom_point(aes(x = mean.long, y = mean.poc), shape = 21, fill = "gray30", size = 2) +
  geom_point(data = p3.atp.dat, aes(x = Long, y = living_biomass_umolC_L), shape = 22, size = 2, fill = "red", stroke = 0.2) +
  # 
  # geom_errorbar(aes(x = mean.lat, ymin = mean.live-sd.live, ymax = mean.live+sd.live), size = 0.5, width = 0.2, color = "darkred") +
  # geom_point(aes(x = mean.lat, y = mean.live), 
  #            fill = "red", shape = 22, size = 2.5, stroke = 0.2) +
  scale_y_continuous(name = NULL, sec.axis = sec_axis(trans=~.*(1/15), name=NULL), limits = c(0,5)) +
  theme(axis.text.y.right = element_text(color = "chartreuse4"))  +
  theme(axis.title.x = element_blank())
p3.env




###______P4 environment data:
p4.env <- ggplot(p4.env.dat, aes(x = Station, y = Value)) +
  theme_bw() +
  geom_boxplot(aes(color = Parameter)) +
  geom_point(aes(color = Parameter), 
             position = position_jitterdodge(jitter.width = 0.2, jitter.height = 1),
             alpha = 0.7
             ) +
  scale_color_manual(values = c("yellowgreen", "gray30")) +
  scale_fill_manual(values = c("yellowgreen", "gray30")) +
  scale_y_continuous(name = NULL, sec.axis = sec_axis(trans=~.*(1/30), name="Chla"), 
                     limits = c(0,NA)) +
  theme(axis.text.y.right = element_text(color = "chartreuse4"),
        axis.title.y.right = element_text(color = "chartreuse4"),
        legend.position = "none",
        axis.title.x = element_blank())
p4.env











###___________Make Total Osmolyte Figures__________________

#Read in data 
dat <- read_csv(all.dat.file) %>%
  left_join(., compound.order) %>%
  filter(!str_detect(Part.SampID, "Blk")) %>%
  filter(!is.na(class))





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

#Organize data into different panels 
dat.panel <- dat.region %>%
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
  group_by(Parent_ID) %>%
  mutate(Sum.Part.Conc.nM = sum(Part.Conc.nM)) %>%
  group_by(lat_bin, Region) %>%
  reframe(Mean.Part.nM = mean(Sum.Part.Conc.nM, na.rm = TRUE),
          SD.Part.nM = sd(Sum.Part.Conc.nM, na.rm = TRUE),
          Mean.Lat = mean(Lat)) %>%
  full_join(dat.panel %>% select(Region) %>% unique())

p2.dat.p <- dat.panel %>%
  filter(panel == "p2") %>%
  filter(Part.detected == "Yes") %>%
  mutate(lat_bin = cut(Lat, breaks = lat.breaks, include.lowest = TRUE, right = FALSE)) %>%
  group_by(Parent_ID) %>%
  mutate(Sum.Part.Conc.nM = sum(Part.Conc.nM)) %>%
  group_by(lat_bin, Region) %>%
  reframe(Mean.Part.nM = mean(Sum.Part.Conc.nM, na.rm = TRUE),
          SD.Part.nM = sd(Sum.Part.Conc.nM, na.rm = TRUE),
          Mean.Lat = mean(Lat))

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
  group_by(Parent_ID) %>%
  mutate(Sum.Part.Conc.nM = sum(Part.Conc.nM)) %>%
  group_by(Station, Long, Region) %>%
  reframe(Mean.Part.nM = mean(Sum.Part.Conc.nM, na.rm = TRUE),
          SD.Part.nM = sd(Sum.Part.Conc.nM, na.rm = TRUE),
          Mean.Long = mean(Long))

p4.dat.p <- dat.panel %>%
  filter(panel == "p4") %>%
  select(Parent_ID, Station, Part.Conc.nM, compound.name.figure, Part.detected, Region) %>%
  unique() %>%
  filter(Part.detected == "Yes") %>%
  group_by(Parent_ID, Station, Region) %>%
  reframe(Sum.Part.Conc.nM = sum(Part.Conc.nM)) 




#____Dissolved______

p1.dat.d <- dat.panel %>%
  filter(panel == "p1") %>%
  filter(Diss.detected == "Yes") %>%
  mutate(lat_bin = cut(Lat, breaks = lat.breaks, include.lowest = TRUE, right = FALSE)) %>%
  group_by(Parent_ID) %>%
  mutate(Sum.Diss.Conc.nM = sum(Diss.Conc.nM)) %>%
  group_by(lat_bin, Region) %>%
  reframe(Mean.Diss.nM = mean(Sum.Diss.Conc.nM , na.rm = TRUE),
          SD.Diss.nM = sd(Sum.Diss.Conc.nM , na.rm = TRUE),
          Mean.Lat = mean(Lat))

p2.dat.d <- dat.panel %>%
  filter(panel == "p2") %>%
  filter(Diss.detected == "Yes") %>%
  mutate(lat_bin = cut(Lat, breaks = lat.breaks, include.lowest = TRUE, right = FALSE)) %>%
  group_by(Parent_ID) %>%
  mutate(Sum.Diss.Conc.nM = sum(Diss.Conc.nM)) %>%
  group_by(lat_bin, Region) %>%
  reframe(Mean.Diss.nM = mean(Sum.Diss.Conc.nM , na.rm = TRUE),
          SD.Diss.nM = sd(Sum.Diss.Conc.nM , na.rm = TRUE),
          Mean.Lat = mean(Lat))

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
  group_by(Parent_ID) %>%
  mutate(Sum.Diss.Conc.nM = sum(Diss.Conc.nM)) %>%
  group_by(Station, Region) %>%
  reframe(Mean.Diss.nM = mean(Sum.Diss.Conc.nM , na.rm = TRUE),
          SD.Diss.nM = sd(Sum.Diss.Conc.nM , na.rm = TRUE),
          Mean.Long = mean(Long))

p4.dat.d <- dat.panel %>%
  filter(panel == "p4") %>%
  select(Parent_ID, Station, Diss.Conc.nM, compound.name.figure, Diss.detected, Region) %>%
  unique() %>%
  filter(Diss.detected == "Yes") %>%
  group_by(Parent_ID, Station, Region) %>%
  reframe(Sum.Diss.Conc.nM = sum(Diss.Conc.nM)) 






#########Make Plots

#Particulate:
p1.p <- ggplot(p1.dat.p, aes(x = Mean.Lat, y = Mean.Part.nM)) +
  geom_errorbar(aes(ymin = Mean.Part.nM-SD.Part.nM, ymax = Mean.Part.nM+SD.Part.nM, color = Region),
                wdith = 0.3) +
  geom_line() +
  geom_point(aes(fill = Region), shape = 21, size = 3, scale = 0.2) +
  theme_bw() +
  scale_fill_manual(values = region.palette.7) +
  scale_color_manual(values = region.palette.7) +
  ylim(c(0, 24)) +
  theme(axis.title.x = element_blank()) +
  ylab("Particulate Concentration (nM)")
p1.p

p2.p <- ggplot(p2.dat.p, aes(x = Mean.Lat, y = Mean.Part.nM)) +
  geom_errorbar(aes(ymin = Mean.Part.nM-SD.Part.nM, ymax = Mean.Part.nM+SD.Part.nM, color = Region),
                width = 0.3) +
  geom_line() +
  geom_point(aes(fill = Region), shape = 21, size = 3, stroke = 0.4) +
  theme_bw() +
  scale_fill_manual(values = region.palette.7) +
  scale_color_manual(values = region.palette.7) +
  ylim(c(0, NA)) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none")
p2.p

p3.p <- ggplot(p3.dat.p, aes(x = Mean.Long, y = Mean.Part.nM)) +
  geom_errorbar(aes(ymin = Mean.Part.nM-SD.Part.nM, ymax = Mean.Part.nM+SD.Part.nM, color = Region)) +
  geom_line() +
  geom_point(aes(fill = Region), shape = 21, size = 3, stroke = 0.4) +
  theme_bw() +
  scale_fill_manual(values = region.palette.7) +
  scale_color_manual(values = region.palette.7) +
  ylim(c(0, 24)) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none")
p3.p

p4.p <- ggplot(p4.dat.p, aes(x = Station, y = Sum.Part.Conc.nM)) +
  geom_boxplot(width = 0.4) +
  geom_jitter(width = 0.2, shape = 21, aes(fill = Region), size = 2, stroke = 0.3) +
  theme_bw() +
  scale_fill_manual(values = region.palette.7) +
  scale_color_manual(values = region.palette.7) +
  ylim(c(0, NA)) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none")
p4.p


#Dissolved:
p1.d <- ggplot(p1.dat.d, aes(x = Mean.Lat, y = Mean.Diss.nM)) +
  geom_errorbar(aes(ymin = Mean.Diss.nM-SD.Diss.nM, ymax = Mean.Diss.nM+SD.Diss.nM, color = Region),
                width = 0.3) +
  geom_line() +
  geom_point(aes(fill = Region), shape = 21, size = 3, stroke = 0.4) +
  theme_bw() +
  scale_fill_manual(values = region.palette.7) +
  scale_color_manual(values = region.palette.7) +
  ylim(c(0, 90)) +
  ylab("Dissolved Concentration (nM)") +
  theme(legend.position = "none") +
  xlab("Latitude")
p1.d

p2.d <- ggplot(p2.dat.d, aes(x = Mean.Lat, y = Mean.Diss.nM)) +
  geom_errorbar(aes(ymin = Mean.Diss.nM-SD.Diss.nM, ymax = Mean.Diss.nM+SD.Diss.nM, color = Region),
                width = 0.3) +
  geom_line() +
  geom_point(aes(fill = Region), shape = 21, size = 3, stroke = 0.4) +
  theme_bw() +
  scale_fill_manual(values = region.palette.7) +
  scale_color_manual(values = region.palette.7) +
  ylim(c(0, NA)) +
  theme(axis.title.y = element_blank(),
        legend.position = "none")  +
  xlab("Latitude")
p2.d

p3.d <- ggplot(p3.dat.d, aes(x = Mean.Long, y = Mean.Diss.nM)) +
  geom_errorbar(aes(ymin = Mean.Diss.nM-SD.Diss.nM, ymax = Mean.Diss.nM+SD.Diss.nM, color = Region),
                width = 0.3) +
  geom_line() +
  geom_point(aes(fill = Region), shape = 21, size = 3, stroke = 0.4) +
  theme_bw() +
  scale_fill_manual(values = region.palette.7) +
  scale_color_manual(values = region.palette.7) +
  ylim(c(0, 90)) +
  theme(axis.title.y = element_blank(),
        legend.position = "none") +
  xlab("Longitude")
p3.d

p4.d <- ggplot(p4.dat.d, aes(x = Station, y = Sum.Diss.Conc.nM)) +
  geom_boxplot(width = 0.4) +
  geom_jitter(width = 0.2, shape = 21, aes(fill = Region), size = 2, stroke = 0.3) +
  theme_bw() +
  scale_fill_manual(values = region.palette.7) +
  scale_color_manual(values = region.palette.7) +
  ylim(c(0, NA)) +
  theme(axis.title.y = element_blank(),
        legend.position = "none") +
  xlab("Station")
p4.d



#####Other Plots to Show:

#1. Correlation with POC

#Make POC plotting dataset:
poc.fig.dat <- dat.region %>%
  group_by(filter())



#2. Correlation with Chla



#3. Correlation with ATP



#4. Correlation with Sal 



######




###Work on comibining all plots:
# distribution.plots <- 
#   (p1.env + p2.env + p3.env + p4.env + 
#      plot_layout(nrow = 1, widths = c(4, 4, 4, 2.5))) /
#   (p1.p + p2.p + p3.p + p4.p + 
#   plot_layout(nrow = 1, widths = c(4, 4, 4, 2.5))) /
#   (p1.d + p2.d + p3.d + p4.d + 
#      plot_layout(nrow = 1, widths = c(4, 4, 4, 2.5))) + 
#   plot_layout(guides = "collect")
# distribution.plots


distribution.plots <- 
  p1.env + p3.env + p2.env + p4.env +
  p1.p + p3.p + p2.p + p4.p +
  p1.d + p3.d + p2.d + p4.d  + 
  plot_layout(guides = "collect", nrow = 3, ncol = 4,
              widths = c(4,3,4.5,2.5))
distribution.plots

ggsave(distribution.plots, filename = "Figures/Output_Oct25/Distribution_Plot.png",
       dpi = 800, width = 7.5, height = 4.5, scale = 1.5)








































