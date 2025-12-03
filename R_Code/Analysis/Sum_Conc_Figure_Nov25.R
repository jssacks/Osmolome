





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
atp.file <- "Intermediates/ATP_G3_G4_data.csv"




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
  mutate(panel = case_when(Cruise == "TN397" ~ "p1",
                           Cruise == "KM1906" ~ "p2",
                           Cruise == "RC078" ~ "p3")) 


#load in g3 data:
g3.env.dat <- read_csv(g3.file) %>%
  mutate(Cruise = "KM1906") %>%
  rename(Lat = lat,
         Long = lon) %>%
  select(Cruise, Lat, Long, chla, pc)  %>%
  full_join(., atp.dat %>% filter(Cruise == "KM1906")) %>%
  mutate(panel = case_when(Cruise == "TN397" ~ "p1",
                           Cruise == "KM1906" ~ "p2",
                           Cruise == "RC078" ~ "p3")) 



##Dinimite data
d1.env.dat <- read_csv(d1.file) %>%
  mutate(Cruise = "RC078") %>%
  filter(depth_m < 16) %>%
  filter(!Cast == "C9") %>%
  rename(Lat = lat,
         Long = lon,
         poc = POC_uM,
         chla = Chl_fluor) %>%
  select(Station, Cruise, Lat, Long, poc, chla)  %>%
  mutate(panel = case_when(Cruise == "TN397" ~ "p1",
                           Cruise == "KM1906" ~ "p2",
                           Cruise == "RC078" ~ "p3"))  %>%
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







##________Panel_3___________
p3.env.dat <- d1.env.dat %>%
  mutate(chla = chla*50) %>%
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
  scale_y_continuous(name = "POC (uM)", sec.axis = sec_axis(trans=~.*(1/15), name=NULL), limits = c(0,4.5),
                     expand = c(0,NA)) +
  theme(axis.text.y.right = element_text(color = "chartreuse4")) +
  theme(axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("TN397")

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
  scale_y_continuous(name = NULL, sec.axis = sec_axis(trans=~.*(1/15), name=NULL), limits = c(0,11),
                     expand = c(0,NA)) +
  theme(axis.text.y.right = element_text(color = "chartreuse4")) +
  theme(axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("KM1906")

p2.env







###______P4 environment data:
p3.env <- ggplot(p3.env.dat, aes(x = Station, y = Value)) +
  theme_bw() +
  geom_boxplot(aes(color = Parameter)) +
  geom_point(aes(color = Parameter), 
             position = position_jitterdodge(jitter.width = 0.2, jitter.height = 1),
             alpha = 0.7
  ) +
  scale_color_manual(values = c("yellowgreen", "gray30")) +
  scale_fill_manual(values = c("yellowgreen", "gray30")) +
  scale_y_continuous(name = NULL, sec.axis = sec_axis(trans=~.*(1/50), name="Chla"), 
                     limits = c(0,120), expand = c(0,NA)) +
  theme(axis.text.y.right = element_text(color = "chartreuse4"),
        axis.title.y.right = element_text(color = "chartreuse4"),
        legend.position = "none",
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("RC078")

p3.env











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
  mutate(panel = case_when(Cruise == "TN397" ~ "p1",
                           Cruise == "KM1906" ~ "p2",
                           Cruise == "RC078" ~ "p3")) 


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
  select(Parent_ID, Station, Diss.Conc.nM, compound.name.figure, Diss.detected, Region) %>%
  unique() %>%
  filter(Diss.detected == "Yes") %>%
  group_by(Parent_ID, Station, Region) %>%
  reframe(Sum.Diss.Conc.nM = sum(Diss.Conc.nM)) 






#########Make Plots

#Particulate:
p1.p <- ggplot(p1.dat.p, aes(x = Mean.Lat, y = Mean.Part.nM)) +
  geom_errorbar(aes(ymin = Mean.Part.nM-SD.Part.nM, ymax = Mean.Part.nM+SD.Part.nM, color = Region),
                width = 0.3) +
  geom_line() +
  geom_point(aes(fill = Region), shape = 21, size = 3, stroke = 0.4) +
  theme_bw() +
  scale_fill_manual(values = region.palette.7) +
  scale_color_manual(values = region.palette.7) +
  scale_y_continuous(limits = c(0,21), expand = c(0,NA)) +
#  ylim(c(0, 21)) +
  theme(axis.title.x = element_blank()) +
  ylab("Particulate Conc. (nM)")
p1.p

p2.p <- ggplot(p2.dat.p, aes(x = Mean.Lat, y = Mean.Part.nM)) +
  geom_errorbar(aes(ymin = Mean.Part.nM-SD.Part.nM, ymax = Mean.Part.nM+SD.Part.nM, color = Region),
                width = 0.3) +
  geom_line() +
  geom_point(aes(fill = Region), shape = 21, size = 3, stroke = 0.4) +
  theme_bw() +
  scale_fill_manual(values = region.palette.7) +
  scale_color_manual(values = region.palette.7) +
  scale_y_continuous(limits = c(0,45), expand = c(0,NA)) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none")
p2.p


p3.p <- ggplot(p3.dat.p, aes(x = Station, y = Sum.Part.Conc.nM)) +
  geom_boxplot(width = 0.4) +
  geom_jitter(width = 0.2, shape = 21, aes(fill = Region), size = 2, stroke = 0.3) +
  theme_bw() +
  scale_fill_manual(values = region.palette.7) +
  scale_color_manual(values = region.palette.7) +
  scale_y_continuous(limits = c(0,750), expand = c(0,NA)) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none")
p3.p


#Dissolved:
p1.d <- ggplot(p1.dat.d, aes(x = Mean.Lat, y = Mean.Diss.nM)) +
  geom_errorbar(aes(ymin = Mean.Diss.nM-SD.Diss.nM, ymax = Mean.Diss.nM+SD.Diss.nM, color = Region),
                width = 0.3) +
  geom_line() +
  geom_point(aes(fill = Region), shape = 21, size = 3, stroke = 0.4) +
  theme_bw() +
  scale_fill_manual(values = region.palette.7) +
  scale_color_manual(values = region.palette.7) +
  scale_y_continuous(limits = c(0,90), expand = c(0,NA)) +
  ylab("Dissolved Conc. (nM)") +
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
  scale_y_continuous(limits = c(0,180), expand = c(0,NA)) +
  theme(axis.title.y = element_blank(),
        legend.position = "none")  +
  xlab("Latitude")
p2.d

p3.d <- ggplot(p3.dat.d, aes(x = Station, y = Sum.Diss.Conc.nM)) +
  geom_boxplot(width = 0.4) +
  geom_jitter(width = 0.2, shape = 21, aes(fill = Region), size = 2, stroke = 0.4) +
  theme_bw() +
  scale_fill_manual(values = region.palette.7) +
  scale_color_manual(values = region.palette.7) +
  scale_y_continuous(limits = c(0,640), expand = c(0,NA)) +
  theme(axis.title.y = element_blank(),
        legend.position = "none") +
  xlab("Station")
p3.d



#####Other Plots to Show:


#Make POC plotting dataset:
correlation.fig.dat <- dat.region %>%
  filter(Part.detected == "Yes") %>%
  group_by(Parent_ID, Region, Cruise, sss, sst, poc, chla, pn, N_N, sum.median.biovol.ul.l) %>%
  reframe(Sum.Part.Conc.nM = sum(Part.Conc.nM))





#1. Correlation with POC
poc.cor.fig <- ggplot(correlation.fig.dat, aes(x = poc, y = Sum.Part.Conc.nM)) +
  geom_smooth(method = "lm", color = "black") +
  geom_point(aes(fill = Region), shape = 21, size = 2, stroke = 0.2) +
  theme_bw() +
  scale_fill_manual(values = region.palette.7) +
  scale_y_log10() +
  scale_x_log10() +
  theme(legend.position = "none") +
  ylab("Particulate Conc. (nM)") +
  xlab("POC (uM)") 
poc.cor.fig

#2. Correlation with Chla
chla.cor.fig <- ggplot(correlation.fig.dat %>%
         filter(!chla < 0.03), aes(x = chla, y = Sum.Part.Conc.nM)) +
  geom_smooth(method = "lm", color = "black") +
  geom_point(aes(fill = Region), shape = 21, size = 2, stroke = 0.2) +
  theme_bw() +
  scale_fill_manual(values = region.palette.7) +
  scale_y_log10() +
  scale_x_log10() +
  theme(legend.position = "none",
        axis.title.y = element_blank()) +
  ylab("Part. Conc. (nM)") +
  xlab("Chla (units?)") 
chla.cor.fig




#3. Correlation with ATP

#match up data based on 3 hour time window:
atp.dat.match <- read_csv(atp.file) %>%
  select(Cruise, Station, UTC.time.round, PATP_ng_L, SD_PATP_ng_L) %>%
  mutate(atp.timewindow.max = UTC.time.round + hours(3),
         atp.timewindow.min = UTC.time.round - hours(3))  %>%
  rename(UTC.time.round.ATP = UTC.time.round) %>%
  select(-Cruise)


osmo.dat.match <- dat.region %>%
  filter(Cruise %in% c("TN397", "KM1906")) %>%
  filter(Part.detected == "Yes") %>%
  select(Cruise, Region, Parent_ID, Lat, Long, UTC.time.round, Part.Conc.nM) %>%
  mutate(metab.sample = str_remove(Parent_ID, "_A"),
         metab.sample = str_remove(metab.sample, "_B"),
         metab.sample = str_remove(metab.sample, "_C")) %>%
  group_by(Cruise, Region, Parent_ID, metab.sample, Lat, Long, UTC.time.round) %>%
  reframe(Sum.Osmo.Conc.nM = sum(Part.Conc.nM)) %>%
  ungroup() %>%
  group_by(Cruise, Region, metab.sample, Lat, Long, UTC.time.round) %>%
  reframe(Mean.Osmo.Conc.nM = mean(Sum.Osmo.Conc.nM),
          SD.Osmo.Conc.nM = sd(Sum.Osmo.Conc.nM)) %>%
  rename(UTC.time.round.osmo = UTC.time.round) 


##combine data:
atp.osmo.dat <- cross_join(atp.dat.match, osmo.dat.match) %>%
  filter(UTC.time.round.osmo < atp.timewindow.max,
         UTC.time.round.osmo > atp.timewindow.min) %>%
  mutate(Mean.living.biomass.umolC.L = 250*PATP_ng_L/12.01/1000,
         SD.living.biomass.umolC.L = 250*SD_PATP_ng_L/12.01/1000) %>%
  unique()

#Plot data:
atp.cor.fig <- ggplot(atp.osmo.dat, aes(x = Mean.living.biomass.umolC.L, y = Mean.Osmo.Conc.nM)) +
  geom_smooth(method = "lm", color = "black", alpha = 0.2) +
  geom_errorbar(aes(ymin = Mean.Osmo.Conc.nM-SD.Osmo.Conc.nM, ymax = Mean.Osmo.Conc.nM+SD.Osmo.Conc.nM), size = 0.2) +
  geom_errorbarh(aes(xmin = Mean.living.biomass.umolC.L-SD.living.biomass.umolC.L, xmax = Mean.living.biomass.umolC.L+SD.living.biomass.umolC.L), size = 0.2) +
  geom_point(aes(fill = Region), shape = 21, stroke = 0.2, size = 2)  +
  theme_bw() +
  scale_fill_manual(values = region.palette.7) +
 # scale_fill_manual(values = c("darkgray", "cornflowerblue")) +
  # scale_y_continuous(expand = c(0,NA,NA,NA)) +
  # scale_x_continuous(expand = c(0,NA,NA,NA)) +
  ylab("Part. Conc. (nM)") +
  xlab("Living Biomass (uM C)") +
  theme(legend.position = "none",
        axis.title.y = element_blank()) 
 # annotate(geom = "text", x = 32, y = 18000, label = "y = 1681.3x + 161.3") +
 # annotate(geom = "text", x = 32, y = 14000, label = "R2 = 0.85") +
 # annotate(geom = "text", x = 32, y = 10000, label = "p = 2.2e-10") 
atp.cor.fig 



#4. Correlation with Biovolume
biovol.cor.fig <- ggplot(correlation.fig.dat %>%
         filter(!is.na(sum.median.biovol.ul.l)), aes(x = sum.median.biovol.ul.l, y = Sum.Part.Conc.nM)) +
  geom_smooth(method = "lm", color = "black") +
  geom_point(aes(fill = Region), shape = 21, size = 2, stroke = 0.2) +
  theme_bw() +
  scale_fill_manual(values = region.palette.7) +
  theme(legend.position = "none",
        axis.title.y = element_blank()) +
  scale_x_log10() +
  scale_y_log10() +
  ylab("Part. Conc. (nM)") +
  xlab("Biovolume (uL/L)") 
biovol.cor.fig

#  scale_y_log10() +
 # scale_x_log10()










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
design <- "AAAABBBBCC
           DDDDEEEEFF
           GGGGHHHHII
           ##########
           JJJJJJJJJJ"

distribution.plots <- 
  p1.env + p2.env + p3.env + 
  p1.p + p2.p + p3.p + 
  p1.d + p2.d + p3.d +
  (poc.cor.fig | chla.cor.fig | biovol.cor.fig | atp.cor.fig) +
    plot_layout(guides = "collect", design = design,
                heights = c(1,1,1,0.15,1.25)) + 
  plot_annotation(tag_levels = "a")

distribution.plots


ggsave(distribution.plots, filename = "Figures/Output_Oct25/Distribution_Plot.png",
       dpi = 1000, width = 7.5, height = 6.25, scale = 1.5)








#####Other Plots to Show:

#1. Correlation with POC

#Make POC plotting dataset:
poc.fig.dat <- dat.region %>%
  filter(Part.detected == "Yes") %>%
  group_by(Parent_ID, Region, Cruise, sss, sst, poc, chla, pn, N_N) %>%
  reframe(Sum.Part.Conc.nM = sum(Part.Conc.nM))

#sss
ggplot(poc.fig.dat, aes(x = sss, y = Sum.Part.Conc.nM)) +
  geom_smooth(method = "lm", color = "black") +
  geom_point(aes(fill = Region), shape = 21, size = 3, stroke = 0.4) +
  theme_bw() +
  scale_fill_manual(values = region.palette.7) +
  scale_y_log10() 

#sst
ggplot(poc.fig.dat, aes(x = sst, y = Sum.Part.Conc.nM)) +
  geom_point(aes(fill = Region), shape = 21, size = 3, stroke = 0.4) +
  theme_bw() +
  scale_fill_manual(values = region.palette.7) +
  scale_y_log10() 

#N_N
ggplot(poc.fig.dat, aes(x = N_N, y = Sum.Part.Conc.nM)) +
  geom_point(aes(fill = Region), shape = 21, size = 3, stroke = 0.4) +
  theme_bw() +
  scale_fill_manual(values = region.palette.7) +
  scale_y_log10() +
  scale_x_log10()






