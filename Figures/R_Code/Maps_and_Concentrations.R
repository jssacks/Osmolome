






##Load in packages and source code:
library(patchwork)
library(viridis)
library(tidyverse)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(maps)
library(mapdata)


######define inputs
all.dat.file <- "Intermediates/Final_Osmo_Meta_Env_Dataframe.csv"



#Read in data
dat <- read_csv(all.dat.file)


# 
# 
# ##Define inputs:
# part.file <- "Intermediates/Particulate_Quant_Output.csv"
# meta.file <- "Intermediates/All_metadata_information.csv"
# g.env.file <- "Intermediates/combined_gradients_enviromental_data.csv"
# 
# ###load in data and combine with metadata
# part.dat <- read_csv(part.file)
# meta.dat <- read_csv(meta.file)
# 
# 
# 
# 
# #### Gather Surface Data:
# dat.surface <- left_join(part.dat, meta.dat) %>%
#   rename("Compound" = Name) %>%
#   filter(Compound %in% compound.order$Compound) %>%
#   left_join(., compound.order) %>%
#   # filter(!station == 4) %>%
#   filter(!SampID == "220902_Smp_TN397_S11_600_U_C") %>%
#   unique() %>%
#   filter(Cruise %in% c( "KM1906", "TN397", "RC078")) %>%
#   filter(!Compound == "Threonine Betaine (tentative)") %>%
#   filter(!Compound == "Homoserine Betaine (tentative)") %>%
#   filter(depth_m < 10) %>%
#   mutate(Region = case_when(Cruise == "RC078" ~ "PS",
#                             Lat > 36 ~ "NPTZ",
#                             Lat < 7 ~ "Equator",
#                             Cruise == "TN397" & Lat > 29 ~ "CC",
#                             TRUE ~ "NPSG")) %>%
#   filter(!station %in% c(1, 8))
# 
# 
# ##Pull in environmental metadata 
# g.env.dat <- read_csv("Intermediates/Gradients_Matched_Environmental_Metadata.csv") 
# d.env.dat <- read_csv("Intermediates/RC078_MetaData_Compiled.csv") %>%
#   filter(!is.na(depth_m),
#          !is.na(station)) %>%
#   # select(sample_id, parent_id, station, depth_m) %>%
#   rename("SampID" = sample_id) %>%
#   mutate(SampID = paste("221006_Smp_", SampID, sep = ""),
#          Cruise = "RC078") %>%
#   #  select(SampID, Cruise, station, depth_m, Cruise) %>%
#   filter(!str_detect(SampID, "-")) %>%
#   unique() %>%
#   rename("poc" = "POC_uM",
#          "pn" = "PN_uM",
#          "sss" = sal,
#          "sst" = temp,
#          "chla" = Chl_fluor)
# 
# ###Organize all enviro dat
# all.env.dat <- g.env.dat %>%
#   select(SampID, sst, sss, chla_interp, pc_interp, pn_interp) %>%
#   rename("chla" = chla_interp,
#          "poc" = pc_interp,
#          "pn" = pn_interp) %>%
#   rbind(., d.env.dat %>% select(SampID, sst, sss, chla, poc, pn)) 
# 
# 
# 
# ###Combine enviro and surface data:
# osmo.enviro.dat <- left_join(dat.surface, all.env.dat) 
# 






# Make Maps of North Pacific and Puget Sound ------------------------------


#load in map data
world <- ne_countries(scale = "medium", returnclass = "sf")


#Get location data:
samp.loc.dat <- dat%>%
  select(Region, Lat, Long, station) %>%
  unique()

big.map <- ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-170, -110), ylim = c(-6, 53)) +
  geom_point(data = samp.loc.dat, 
             aes(x = Long, y = Lat, fill = Region), 
             shape = 21,
             size = 3,
             stroke = 0.15) +
  scale_y_continuous(labels = scales::label_number(accuracy = 1)) +
  scale_x_continuous(labels = scales::label_number(accuracy = 1)) +
  # scale_fill_manual(values = region.pal) +
  scale_fill_manual(values = region.palette.2) +
  #  geom_jitter(data = dat.3, aes(Long, Lat, color = Local_Date), size = 4, alpha = 0.3) + 
  #  scale_color_viridis() +
  theme_test() +
  geom_rect(xmin = -120, xmax = -125,
            ymin = 46, ymax = 51, alpha = 0, color = "black") #+
#  theme(legend.position = "none")
big.map



####Make map of Puget Sound Stations:

#get map data:
ps.map.data <- map_data("worldHires", c("Canada", "usa", "Mexico"))

#get just PS experiment locatin data:
samp.loc.dat.PS <- samp.loc.dat %>%
  filter(Region == "PS") %>%
  filter(!station %in% c(1, 8)) 


#Plot
PS.map <- ggplot() + 
  geom_polygon(data = ps.map.data, aes(x=long, y = lat, group = group),
               fill = "gray90", color = "black", linewidth = 0.2) +
  theme_bw()+
  coord_map("conic", lat0 = 18, xlim=c(-123.5, -122), ylim=c(47.5,49)) +
  geom_point(data = samp.loc.dat.PS, 
             aes(x = Long, y = Lat, fill = Region), 
             shape = 21,
             size = 3, 
             stroke = 0.15) +
  scale_fill_manual(values = region.palette.2) +
  # geom_jitter(data = exp.loc.dat.PS, 
  #             aes(x = Long, y = Lat, shape = Compound), 
  #             alpha = 0.9, 
  #             size = 5,
  #             fill = "#00A087FF", width = 0.03, height = 0.03) +
  # scale_shape_manual(values = c(21, 22)) +
  geom_label_repel(data = samp.loc.dat.PS, 
                   aes(x = Long, y = Lat, label = station),
                   box.padding = 0.5, min.segment.length = 0.1,
                   alpha = 1,
                   color = "black",
                   # point.padding = 0.5,
                   # ylim = c(-2, 48),
                   size = 3,
                   segment.size = 0.2,
                   segment.curvature = -1e-20) +
  theme_test() +
  theme(legend.position="none") + 
  ylab("Lat") +
  xlab("Long") 

PS.map




##__T+S diagram:

#collect TS data
t.s.dat <- dat %>%
  select(Part.SampID, Diss.SampID, sst, sss, Region, station) %>%
  filter(!station %in% c(1, 8)) %>%
  unique()

##get station labels for PS
t.s.dat.ps <- t.s.dat %>%
  filter(Region == "PS") %>%
  select(station, sst, sss, Region) %>%
  unique() %>%
  group_by(station, Region) %>%
  reframe(sst = mean(sst, na.rm = TRUE),
          sss = mean(sss, na.rm = TRUE))


t.s.plot <- ggplot(t.s.dat, aes(x = sst, y = sss, fill = Region)) +
  geom_jitter(shape = 21, aes(fill = Region), size = 3, stroke = 0.15, width = 0.5, height = 0.25) +
  theme_test() +
  scale_fill_manual(values = region.palette.2) +
  geom_label_repel(data = t.s.dat.ps, 
                   aes(x = sst, y = sss, label = station),
                   box.padding = 0.5, min.segment.length = 0.1,
                   alpha = 0.8,
                   color = "black",
                   fill = "white",
                   # point.padding = 0.5,
                   # ylim = c(-2, 48),
                   size = 3,
                   segment.size = 0,
                   segment.curvature = -1e-20) +
  xlab("Temperature (C)") + 
  ylab("Salinity (PSU)")
t.s.plot


all.maps <- big.map + PS.map + t.s.plot +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A")
all.maps

ggsave(all.maps, file = "R_Code/Code_Development_Workspace/maps_v1.pdf", 
       height = 3, width = 8, scale = 1.2)


#maps for presenatation:
all.maps.pres <- big.map + PS.map + plot_layout(guides = "collect") 
all.maps.pres 

ggsave(all.maps.pres, file = "Figures/Outputs/maps_for_pres.pdf", 
       height = 4, width = 7, scale = 1.1)

# Make POC and Concentration Plots ----------------------------------------

#g.env.dat <- read_csv(g.env.file)

#Bin POC by lat and calculate mean and sd:
lat.breaks <- seq(-3,50, by = 1)

# #POC data to plot
# g.env.dat.bin <- g.env.dat %>%
#   filter(lon < -124) %>%
#   mutate(lat_bin = cut(lat, breaks = lat.breaks, include.lowest = TRUE, right = FALSE)) %>%
#   group_by(lat_bin) %>%
#   reframe(mean_poc = mean(pc, na.rm = TRUE),
#           sd_poc = sd(pc, na.rm = TRUE),
#           mean_lat = mean(lat)) %>%
#   filter(!is.na(mean_poc))


###Pull in and orggnize osmolyte data:
g.tot.osmo.dat <- dat %>%
  filter(Cruise %in% c("TN397", "KM1906")) %>%
  group_by(Part.SampID, Diss.SampID, Cruise, Lat, Long, station, poc) %>%
  reframe(Part.Osmo.Conc.nM = sum(Part.Conc.nM, na.rm = TRUE),
          Diss.Osmo.Conc.nM = sum(Diss.Conc.nM.adj, na.rm = TRUE),
          Total.Osmo.Conc.nM = sum(Total.Conc.nM, na.rm = TRUE)) %>%
  filter(Long < -124) %>%
  mutate(lat_bin = cut(Lat, breaks = lat.breaks, include.lowest = TRUE, right = FALSE)) %>%
  group_by(lat_bin) %>%
  reframe(mean_part_conc = mean(Part.Osmo.Conc.nM, na.rm = TRUE),
          mean_diss_conc = mean(Diss.Osmo.Conc.nM, na.rm = TRUE),
          mean_total_conc = mean(Total.Osmo.Conc.nM, na.rm = TRUE),
          sd_part_conc = sd(Part.Osmo.Conc.nM, na.rm = TRUE),
          sd_diss_conc = sd(Diss.Osmo.Conc.nM, na.rm = TRUE),
          sd_tot_conc = sd(Total.Osmo.Conc.nM, na.rm = TRUE),
          mean_poc = mean(poc, na.rm = TRUE),
          sd_poc = sd(poc, na.rm = TRUE),
          mean_lat = mean(Lat)) 


#Attempt XYZ data
poc.lat <- ggplot(g.tot.osmo.dat, aes(x = mean_lat)) +
  geom_errorbar(aes(ymin = mean_poc-sd_poc, ymax = mean_poc+sd_poc), width = 0, color = "#1d3557") +
  geom_line(aes(y = mean_poc), linetype = "31", color = "#1d3557") +
  geom_point(aes(y = mean_poc), shape = 21, fill = "white", size = 2, color = "#1d3557") +
  geom_line(aes(x = mean_lat, y = mean_part_conc/3), color = "#219EBC") +
  geom_errorbar(aes(x = mean_lat, ymin = mean_part_conc/3-sd_part_conc/3, ymax = mean_part_conc/3+sd_part_conc/3), width = 0, color = "#219EBC") +
  geom_point(aes(x = mean_lat, y = mean_part_conc/3), color = "#219EBC", fill = "white", shape = 22, size = 2) +
  ylim(0,NA) +
  theme_test() +
  scale_y_continuous(name = "POC (uM)", sec.axis = sec_axis(trans=~.*(3), name=NULL)) +
  xlab("Lat") +
  theme(axis.title.y.right = element_text(color = "#219EBC"),
        axis.text.y.right = element_text(color = "#219EBC"))
poc.lat

###Make DINIMITE plot:
# d.env.dat.plot <- d.env.dat %>%
#   filter(station %in% c(2, 3, 5, 6, 7)) %>%
#   filter(depth_m < 10) %>%
#   select(SampID, poc, pn)
# 
# osmo.dat.d.cc <- tot.osmo.conc <- dat.surface %>%
#   group_by(SampID, Cruise, Lat, Long, station) %>%
#   reframe(Total.Osmo.Conc.nM = sum(nM.in.smp),
#           Total.Osmo.Carbon.nM = sum(nM_C),
#           Total.Osmo.Nitrogen.nM = sum(nM_N, na.rm = TRUE)) %>%
#   filter(Cruise == "RC078") %>%
#   left_join(., d.env.dat.plot) %>%
#   mutate(osmo_conc = Total.Osmo.Conc.nM/3,
#          station = as.factor(station)) %>%
#   select(station, osmo_conc, poc) %>%
#   pivot_longer(cols = c("osmo_conc", "poc"), names_to = "parameter")


d.tot.osmo.dat <- dat %>%
  filter(Cruise == "RC078") %>%
  select(Part.SampID, Compound, Diss.SampID, Cruise, Lat, Long, station, poc, Part.Conc.nM, Diss.Conc.nM.adj, Total.Conc.nM) %>%
  unique() %>%
  group_by(Part.SampID, Diss.SampID, station, poc) %>%
  reframe(Part.Osmo.Conc.nM = sum(Part.Conc.nM, na.rm = TRUE)/3,
          Diss.Osmo.Conc.nM = sum(Diss.Conc.nM.adj, na.rm = TRUE),
          Total.Osmo.Conc.nM = sum(Total.Conc.nM, na.rm = TRUE)) %>%
  select(-Diss.Osmo.Conc.nM, -Total.Osmo.Conc.nM) %>%
  rename("Osmolytes" = Part.Osmo.Conc.nM) %>%
  pivot_longer(cols = c("Osmolytes", "poc"), names_to = "parameter") %>%
  filter(!station %in% c(1, 8))



poc.box <- ggplot(d.tot.osmo.dat, aes(x = as.factor(station), y = value, color = parameter, shape = parameter)) +
  geom_boxplot(width = 0.7) +
  geom_jitter(fill = "white", position = position_jitterdodge(dodge.width = 0.7, jitter.width = 0.2)) +
  scale_y_continuous(name = NULL, sec.axis = sec_axis(trans=~.*(3), name="Part. Osmo. (nM)")) +
  theme_test() +
  xlab("Station") +
  scale_shape_manual(values = c(22,21)) +
  scale_color_manual(values = c("#219EBC", "#1d3557")) +
  theme(axis.title.y.right = element_text(color = "#219EBC"),
        axis.text.y.right = element_text(color = "#219EBC"))
poc.box


#combine plots for POC and total osmolyte concentration
poc.comb <- poc.lat + poc.box +
  plot_layout(guides = "collect", widths = c(6.5, 3.5)) +
  plot_annotation(tag_levels = "A")
poc.comb



##Combine all of the pieces so far:


all.maps <- big.map + PS.map + t.s.plot +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A")
all.maps


map.conc.plot <- all.maps/poc.comb + 
  plot_layout(guides = "collect", heights = c(0.6, 0.4)) +
  plot_annotation(tag_levels = "A") 

map.conc.plot

ggsave(map.conc.plot, file = "Figures/Outputs/MainTextFig1_Maps.png", dpi = 1200,
       height = 7, width = 10, scale = 1.3)














###Pull in and orggnize osmolyte data for Steph::
g.tot.osmo.dat.s <- dat %>%
  filter(Cruise %in% c("TN397", "KM1906")) %>%
  group_by(Part.SampID, Diss.SampID, Cruise, Lat, Long, station, poc) %>%
  reframe(Part.Osmo.Conc.nM = sum(Part.Conc.nM, na.rm = TRUE),
          Diss.Osmo.Conc.nM = sum(Diss.Conc.nM.adj, na.rm = TRUE),
          Total.Osmo.Conc.nM = sum(Total.Conc.nM, na.rm = TRUE)) %>%
  filter(Long < -124) %>%
  mutate(lat_bin = cut(Lat, breaks = lat.breaks, include.lowest = TRUE, right = FALSE)) %>%
  group_by(lat_bin) %>%
  reframe(mean_part_conc = mean(Part.Osmo.Conc.nM, na.rm = TRUE),
          mean_diss_conc = mean(Diss.Osmo.Conc.nM, na.rm = TRUE),
          mean_total_conc = mean(Total.Osmo.Conc.nM, na.rm = TRUE),
          sd_part_conc = sd(Part.Osmo.Conc.nM, na.rm = TRUE),
          sd_diss_conc = sd(Diss.Osmo.Conc.nM, na.rm = TRUE),
          sd_tot_conc = sd(Total.Osmo.Conc.nM, na.rm = TRUE),
          mean_poc = mean(poc, na.rm = TRUE),
          sd_poc = sd(poc, na.rm = TRUE),
          mean_lat = mean(Lat)) %>%
  mutate(Compound = "Total") %>%
  select(Compound, lat_bin, mean_lat, mean_part_conc, sd_part_conc)

ggplot(g.tot.osmo.dat.s, aes(x = mean_lat, y = mean_part_conc)) +
  geom_errorbar(aes(ymin = mean_part_conc-sd_part_conc, ymax = mean_part_conc+sd_part_conc), width = 0) +
  geom_line() + 
  geom_point() +
  labs(x = "mean latitiude", y = "Mean Particulate Concentration (nM)")



Specific.Compound.Osmo.dat.s <- dat %>%
  filter(Cruise %in% c("TN397", "KM1906")) %>%
  group_by(Compound, Part.SampID, Diss.SampID, Cruise, Lat, Long, station, poc) %>%
  reframe(Part.Osmo.Conc.nM = sum(Part.Conc.nM, na.rm = TRUE),
          Diss.Osmo.Conc.nM = sum(Diss.Conc.nM.adj, na.rm = TRUE),
          Total.Osmo.Conc.nM = sum(Total.Conc.nM, na.rm = TRUE)) %>%
  filter(Long < -124) %>%
  mutate(lat_bin = cut(Lat, breaks = lat.breaks, include.lowest = TRUE, right = FALSE)) %>%
  group_by(Compound, lat_bin) %>%
  reframe(mean_part_conc = mean(Part.Osmo.Conc.nM, na.rm = TRUE),
          mean_diss_conc = mean(Diss.Osmo.Conc.nM, na.rm = TRUE),
          mean_total_conc = mean(Total.Osmo.Conc.nM, na.rm = TRUE),
          sd_part_conc = sd(Part.Osmo.Conc.nM, na.rm = TRUE),
          sd_diss_conc = sd(Diss.Osmo.Conc.nM, na.rm = TRUE),
          sd_tot_conc = sd(Total.Osmo.Conc.nM, na.rm = TRUE),
          mean_poc = mean(poc, na.rm = TRUE),
          sd_poc = sd(poc, na.rm = TRUE),
          mean_lat = mean(Lat)) %>%
  filter(Compound %in% c("Sucrose", "Homarine")) %>%
  select(Compound, lat_bin, mean_lat, mean_part_conc, sd_part_conc)


ggplot(Specific.Compound.Osmo.dat.s, aes(x = mean_lat, y = mean_part_conc, color = Compound)) +
  geom_errorbar(aes(ymin = mean_part_conc-sd_part_conc, ymax = mean_part_conc+sd_part_conc), width = 0) +
  geom_line() + 
  geom_point() +
  labs(x = "mean latitiude", y = "Mean Particulate Concentration (nM)")



#Data for Steph:

#Organize G3 data by Leg
g3.dat <- dat %>%
  filter(Cruise %in% c("KM1906")) %>%
  group_by(Part.SampID, Diss.SampID, Cruise, Lat, Long, station, poc) %>%
  reframe(Part.Osmo.Conc.nM = sum(Part.Conc.nM, na.rm = TRUE),
          Diss.Osmo.Conc.nM = sum(Diss.Conc.nM.adj, na.rm = TRUE),
          Total.Osmo.Conc.nM = sum(Total.Conc.nM, na.rm = TRUE)) %>%
  mutate(G3_station = str_remove(Part.SampID, "220628_Smp_MU"),
         G3_station = str_remove(G3_station, "_A"),
         G3_station = str_remove(G3_station, "_B"),
         G3_station = str_remove(G3_station, "_C"),
         G3_station = as.numeric(G3_station),
         Leg = case_when(G3_station < 12 ~ "Leg_1",
                            G3_station > 16 ~ "Leg_3",
                            TRUE ~ "Leg_2")) %>%
  mutate(lat_bin = cut(Lat, breaks = lat.breaks, include.lowest = TRUE, right = FALSE)) %>%
  group_by(Leg, lat_bin) %>%
  reframe(mean_part_conc = mean(Part.Osmo.Conc.nM, na.rm = TRUE),
          mean_diss_conc = mean(Diss.Osmo.Conc.nM, na.rm = TRUE),
          mean_total_conc = mean(Total.Osmo.Conc.nM, na.rm = TRUE),
          sd_part_conc = sd(Part.Osmo.Conc.nM, na.rm = TRUE),
          sd_diss_conc = sd(Diss.Osmo.Conc.nM, na.rm = TRUE),
          sd_tot_conc = sd(Total.Osmo.Conc.nM, na.rm = TRUE),
          mean_poc = mean(poc, na.rm = TRUE),
          sd_poc = sd(poc, na.rm = TRUE),
          mean_lat = mean(Lat)) %>%
  mutate(Compound = "Total") %>%
  select(Compound, Leg, lat_bin, mean_lat, mean_part_conc, sd_part_conc)

g3.comp.dat <- dat %>%
  filter(Cruise %in% c("KM1906")) %>%
  group_by(Compound, Part.SampID, Diss.SampID, Cruise, Lat, Long, station, poc) %>%
  reframe(Part.Osmo.Conc.nM = sum(Part.Conc.nM, na.rm = TRUE),
          Diss.Osmo.Conc.nM = sum(Diss.Conc.nM.adj, na.rm = TRUE),
          Total.Osmo.Conc.nM = sum(Total.Conc.nM, na.rm = TRUE)) %>%
  mutate(G3_station = str_remove(Part.SampID, "220628_Smp_MU"),
         G3_station = str_remove(G3_station, "_A"),
         G3_station = str_remove(G3_station, "_B"),
         G3_station = str_remove(G3_station, "_C"),
         G3_station = as.numeric(G3_station),
         Leg = case_when(G3_station < 12 ~ "Leg_1",
                            G3_station > 16 ~ "Leg_3",
                            TRUE ~ "Leg_2")) %>%
  mutate(lat_bin = cut(Lat, breaks = lat.breaks, include.lowest = TRUE, right = FALSE)) %>%
  group_by(Compound, Leg, lat_bin) %>%
  reframe(mean_part_conc = mean(Part.Osmo.Conc.nM, na.rm = TRUE),
          mean_diss_conc = mean(Diss.Osmo.Conc.nM, na.rm = TRUE),
          mean_total_conc = mean(Total.Osmo.Conc.nM, na.rm = TRUE),
          sd_part_conc = sd(Part.Osmo.Conc.nM, na.rm = TRUE),
          sd_diss_conc = sd(Diss.Osmo.Conc.nM, na.rm = TRUE),
          sd_tot_conc = sd(Total.Osmo.Conc.nM, na.rm = TRUE),
          mean_poc = mean(poc, na.rm = TRUE),
          sd_poc = sd(poc, na.rm = TRUE),
          mean_lat = mean(Lat)) %>%
  filter(Compound %in% c("Sucrose", "Homarine")) %>%
  select(Compound, Leg, lat_bin, mean_lat, mean_part_conc, sd_part_conc)

g3.all.dat <- rbind(g3.dat, g3.comp.dat) 




#Organize G4 data by Leg
g4.dat <- dat %>%
  filter(Cruise %in% c("TN397")) %>%
  group_by(Part.SampID, Diss.SampID, Cruise, Lat, Long, station, poc) %>%
  filter(!Part.SampID == "220902_Smp_TN397_U8_BB") %>%
  reframe(Part.Osmo.Conc.nM = sum(Part.Conc.nM, na.rm = TRUE),
          Diss.Osmo.Conc.nM = sum(Diss.Conc.nM.adj, na.rm = TRUE),
          Total.Osmo.Conc.nM = sum(Total.Conc.nM, na.rm = TRUE)) %>%
  mutate(Leg = case_when(Long > -139.5 ~ "Leg_1",
                            Long < -140.5 ~ "Leg_3",
                            TRUE ~ "Leg_2")) %>%
  mutate(lat_bin = cut(Lat, breaks = lat.breaks, include.lowest = TRUE, right = FALSE)) %>%
  group_by(Leg, lat_bin) %>%
  reframe(mean_part_conc = mean(Part.Osmo.Conc.nM, na.rm = TRUE),
          mean_diss_conc = mean(Diss.Osmo.Conc.nM, na.rm = TRUE),
          mean_total_conc = mean(Total.Osmo.Conc.nM, na.rm = TRUE),
          sd_part_conc = sd(Part.Osmo.Conc.nM, na.rm = TRUE),
          sd_diss_conc = sd(Diss.Osmo.Conc.nM, na.rm = TRUE),
          sd_tot_conc = sd(Total.Osmo.Conc.nM, na.rm = TRUE),
          mean_poc = mean(poc, na.rm = TRUE),
          sd_poc = sd(poc, na.rm = TRUE),
          mean_lat = mean(Lat)) %>%
  mutate(Compound = "Total") %>%
  select(Compound, Leg, lat_bin, mean_lat, mean_part_conc, sd_part_conc)


g4.comp.dat <- dat %>%
  filter(Cruise %in% c("TN397")) %>%
  group_by(Compound, Part.SampID, Diss.SampID, Cruise, Lat, Long, station, poc) %>%
  filter(!Part.SampID == "220902_Smp_TN397_U8_BB") %>%
  reframe(Part.Osmo.Conc.nM = sum(Part.Conc.nM, na.rm = TRUE),
          Diss.Osmo.Conc.nM = sum(Diss.Conc.nM.adj, na.rm = TRUE),
          Total.Osmo.Conc.nM = sum(Total.Conc.nM, na.rm = TRUE)) %>%
  mutate(Leg = case_when(Long > -139.5 ~ "Leg_1",
                         Long < -140.5 ~ "Leg_3",
                         TRUE ~ "Leg_2")) %>%
  mutate(lat_bin = cut(Lat, breaks = lat.breaks, include.lowest = TRUE, right = FALSE)) %>%
  group_by(Compound, Leg, lat_bin) %>%
  reframe(mean_part_conc = mean(Part.Osmo.Conc.nM, na.rm = TRUE),
          mean_diss_conc = mean(Diss.Osmo.Conc.nM, na.rm = TRUE),
          mean_total_conc = mean(Total.Osmo.Conc.nM, na.rm = TRUE),
          sd_part_conc = sd(Part.Osmo.Conc.nM, na.rm = TRUE),
          sd_diss_conc = sd(Diss.Osmo.Conc.nM, na.rm = TRUE),
          sd_tot_conc = sd(Total.Osmo.Conc.nM, na.rm = TRUE),
          mean_poc = mean(poc, na.rm = TRUE),
          sd_poc = sd(poc, na.rm = TRUE),
          mean_lat = mean(Lat)) %>%
  filter(Compound %in% c("Sucrose", "Homarine")) %>%
  select(Compound, Leg, lat_bin, mean_lat, mean_part_conc, sd_part_conc)


g4.all.dat <- rbind(g4.dat, g4.comp.dat) 



#separate data by leg for export:

#G3 data for export:
g3.leg1.dat <- g3.all.dat %>% filter(Leg == "Leg_1")
g3.leg2.dat <- g3.all.dat %>% filter(Leg == "Leg_2")
g3.leg3.dat <- g3.all.dat %>% filter(Leg == "Leg_3")

#G4 data for export:
g4.leg1.dat <- g4.all.dat %>% filter(Leg == "Leg_1")
g4.leg2.dat <- g4.all.dat %>% filter(Leg == "Leg_2")
g4.leg3.dat <- g4.all.dat %>% filter(Leg == "Leg_3")

#Write as separate files:
write_csv(g3.leg1.dat, file = "Intermediates/Biogeography_Synthesis/G3_Leg1_Osmo.csv")
write_csv(g3.leg2.dat, file = "Intermediates/Biogeography_Synthesis/G3_Leg2_Osmo.csv")
write_csv(g3.leg3.dat, file = "Intermediates/Biogeography_Synthesis/G3_Leg3_Osmo.csv")
write_csv(g4.leg1.dat, file = "Intermediates/Biogeography_Synthesis/G4_Leg1_Osmo.csv")
write_csv(g4.leg2.dat, file = "Intermediates/Biogeography_Synthesis/G4_Leg2_Osmo.csv")
write_csv(g4.leg3.dat, file = "Intermediates/Biogeography_Synthesis/G4_Leg3_Osmo.csv")




##Combine together:
steph.dat <- rbind(g.tot.osmo.dat.s, Specific.Compound.Osmo.dat.s)
write_csv(steph.dat, file = "Intermediates/Osmolyte_Biogeography_Data_JSS.csv")












































































