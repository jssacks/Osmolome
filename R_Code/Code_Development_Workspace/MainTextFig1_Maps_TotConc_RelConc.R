




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




##Define inputs:
part.file <- "Intermediates/Particulate_Quant_Output.csv"
meta.file <- "Intermediates/All_metadata_information.csv"
g.env.file <- "Intermediates/combined_gradients_enviromental_data.csv"

###load in data and combine with metadata
part.dat <- read_csv(part.file)
meta.dat <- read_csv(meta.file)




#### Gather Surface Data:
dat.surface <- left_join(part.dat, meta.dat) %>%
  rename("Compound" = Name) %>%
  filter(Compound %in% compound.order$Compound) %>%
  left_join(., compound.order) %>%
  # filter(!station == 4) %>%
  filter(!SampID == "220902_Smp_TN397_S11_600_U_C") %>%
  unique() %>%
  filter(Cruise %in% c( "KM1906", "TN397", "RC078")) %>%
  filter(!Compound == "Threonine Betaine (tentative)") %>%
  filter(!Compound == "Homoserine Betaine (tentative)") %>%
  filter(depth_m < 10) %>%
  mutate(Region = case_when(Cruise == "RC078" ~ "PS",
                            Lat > 36 ~ "NPTZ",
                            Lat < 7 ~ "Equator",
                            Cruise == "TN397" & Lat > 29 ~ "CC",
                            TRUE ~ "NPSG")) %>%
  filter(!station %in% c(1, 8))


##Pull in environmental metadata 
g.env.dat <- read_csv("Intermediates/Gradients_Matched_Environmental_Metadata.csv") 
d.env.dat <- read_csv("Intermediates/RC078_MetaData_Compiled.csv") %>%
  filter(!is.na(depth_m),
         !is.na(station)) %>%
  # select(sample_id, parent_id, station, depth_m) %>%
  rename("SampID" = sample_id) %>%
  mutate(SampID = paste("221006_Smp_", SampID, sep = ""),
         Cruise = "RC078") %>%
  #  select(SampID, Cruise, station, depth_m, Cruise) %>%
  filter(!str_detect(SampID, "-")) %>%
  unique() %>%
  rename("poc" = "POC_uM",
         "pn" = "PN_uM",
         "sss" = sal,
         "sst" = temp,
         "chla" = Chl_fluor)

###Organize all enviro dat
all.env.dat <- g.env.dat %>%
  select(SampID, sst, sss, chla_interp, pc_interp, pn_interp) %>%
  rename("chla" = chla_interp,
         "poc" = pc_interp,
         "pn" = pn_interp) %>%
  rbind(., d.env.dat %>% select(SampID, sst, sss, chla, poc, pn)) 



###Combine enviro and surface data:
osmo.enviro.dat <- left_join(dat.surface, all.env.dat) 





# Make Maps of North Pacific and Puget Sound ------------------------------


#load in map data
world <- ne_countries(scale = "medium", returnclass = "sf")


#Get location data:
samp.loc.dat <- dat.surface %>%
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
  scale_fill_manual(values = region.palette) +
  #  geom_jitter(data = dat.3, aes(Long, Lat, color = Local_Date), size = 4, alpha = 0.3) + 
  #  scale_color_viridis() +
  theme_test() +
  geom_rect(xmin = -120, xmax = -125,
            ymin = 46, ymax = 51, alpha = 0, color = "black") +
  theme(legend.position = "none")
big.map



####Make map of Puget Sound Stations:

#get map data:
ps.map.data <- map_data("worldHires", c("Canada", "usa", "Mexico"))

#get just PS experiment locatin data:
samp.loc.dat.PS <- samp.loc.dat %>%
  filter(Region == "PS")


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
t.s.dat <- osmo.enviro.dat %>%
  select(SampID, sst, sss, Region, station) %>%
  unique()

##get station labels for PS
t.s.dat.ps <- t.s.dat %>%
  filter(Region == "PS") %>%
  select(station, sst, sss, Region) %>%
  unique() %>%
  group_by(station, Region) %>%
  reframe(sst = mean(sst),
          sss = mean(sss))


t.s.plot <- ggplot(t.s.dat, aes(x = sst, y = sss, fill = Region)) +
  geom_jitter(shape = 21, aes(fill = Region), size = 3, stroke = 0.15, width = 0.5, height = 0.25) +
  theme_test() +
  scale_fill_manual(values = region.palette) +
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


# Make POC and Concentration Plots ----------------------------------------

g.env.dat <- read_csv(g.env.file)

#Bin POC by lat and calculate mean and sd:
lat.breaks <- seq(-3,50, by = 1)

#POC data to plot
g.env.dat.bin <- g.env.dat %>%
  filter(lon < -124) %>%
  mutate(lat_bin = cut(lat, breaks = lat.breaks, include.lowest = TRUE, right = FALSE)) %>%
  group_by(lat_bin) %>%
  reframe(mean_poc = mean(pc, na.rm = TRUE),
          sd_poc = sd(pc, na.rm = TRUE),
          mean_lat = mean(lat)) %>%
  filter(!is.na(mean_poc))
  

###Pull in and orgnize osmolyte data:
tot.osmo.conc <- dat.surface %>%
  group_by(SampID, Cruise, Lat, Long, station) %>%
  reframe(Total.Osmo.Conc.nM = sum(nM.in.smp),
          Total.Osmo.Carbon.nM = sum(nM_C),
          Total.Osmo.Nitrogen.nM = sum(nM_N, na.rm = TRUE)) %>%
  filter(Long < -124) %>%
  mutate(lat_bin = cut(Lat, breaks = lat.breaks, include.lowest = TRUE, right = FALSE)) %>%
  group_by(lat_bin) %>%
  reframe(mean_tot_conc = mean(Total.Osmo.Conc.nM, na.rm = TRUE),
          sd_tot_conc = sd(Total.Osmo.Conc.nM, na.rm = TRUE),
          mean_lat_osmo = mean(Lat)) 

# gradients.osmo.conc <- tot.osmo.conc %>%
#   filter(Cruise %in% c("KM1906", "TN397"))


###Combine POC and total osmo conc datasets:
poc.osmo.dat <- left_join(g.env.dat.bin, tot.osmo.conc)

#Attempt XYZ data
poc.lat <- ggplot(poc.osmo.dat, aes(x = mean_lat)) +
  geom_errorbar(aes(ymin = mean_poc-sd_poc, ymax = mean_poc+sd_poc), width = 0) +
  geom_line(aes(y = mean_poc), linetype = "31") +
  geom_point(aes(y = mean_poc), shape = 21, fill = "white", size = 2) +
  geom_line(aes(x = mean_lat_osmo, y = mean_tot_conc/3), color = "#00a087") +
  geom_errorbar(aes(x = mean_lat_osmo, ymin = mean_tot_conc/3-sd_tot_conc/3, ymax = mean_tot_conc/3+sd_tot_conc/3), width = 0, color = "#00a087") +
  geom_point(aes(x = mean_lat_osmo, y = mean_tot_conc/3), color = "#00a087", fill = "white", shape = 22, size = 2) +
  ylim(0,NA) +
  theme_test() +
  scale_y_continuous(name = "POC (uM)", sec.axis = sec_axis(trans=~.*(3), name=NULL)) +
  xlab("Lat") +
  theme(axis.title.y.right = element_text(color = "#00a087"),
        axis.text.y.right = element_text(color = "#00a087"))
poc.lat

###Make DINIMITE plot:
d.env.dat.plot <- d.env.dat %>%
  filter(station %in% c(2, 3, 5, 6, 7)) %>%
  filter(depth_m < 10) %>%
  select(SampID, poc, pn)

osmo.dat.d.cc <- tot.osmo.conc <- dat.surface %>%
  group_by(SampID, Cruise, Lat, Long, station) %>%
  reframe(Total.Osmo.Conc.nM = sum(nM.in.smp),
          Total.Osmo.Carbon.nM = sum(nM_C),
          Total.Osmo.Nitrogen.nM = sum(nM_N, na.rm = TRUE)) %>%
  filter(Cruise == "RC078") %>%
  left_join(., d.env.dat.plot) %>%
  mutate(osmo_conc = Total.Osmo.Conc.nM/3,
         station = as.factor(station)) %>%
  select(station, osmo_conc, poc) %>%
  pivot_longer(cols = c("osmo_conc", "poc"), names_to = "parameter")


poc.box <- ggplot(osmo.dat.d.cc, aes(x = station, y = value, color = parameter, shape = parameter)) +
  geom_boxplot(width = 0.7) +
  geom_jitter(fill = "white", position = position_jitterdodge(dodge.width = 0.7, jitter.width = 0.2)) +
  scale_y_continuous(name = NULL, sec.axis = sec_axis(trans=~.*(3), name="Total Osmo Conc. (nM)")) +
  theme_test() +
  scale_shape_manual(values = c(22,21)) +
  scale_color_manual(values = c("#00a087", "black")) +
  theme(axis.title.y.right = element_text(color = "#00a087"),
        axis.text.y.right = element_text(color = "#00a087"))
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


x <- all.maps/poc.comb + 
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A")
x



# Relative Abundance Plots ------------------------------------------------
dat.all <- left_join(part.dat, meta.dat) %>%
  rename("Compound" = Name)


gradients <- dat.all %>%
  filter(Cruise %in% c("TN397", "KM1906")) %>%
  filter(Compound %in% compound.order$Compound) %>%
  left_join(., compound.order)



####Gradient Survey Plot
lat.breaks <- seq(-3,50, by = 1)

g3.g4.dat <- gradients %>%
  filter(Cruise %in% c("KM1906", "TN397")) %>%
  filter(Long < -124) %>%
  mutate(lat_bin = cut(Lat, breaks = lat.breaks, include.lowest = TRUE, right = FALSE)) 

g3.g4.transect.dat <- g3.g4.dat %>%
  group_by(lat_bin, Compound) %>%
  reframe(Mean_nM = mean(nM.in.smp, na.rm = TRUE),
          Mean_nM_C = mean(nM_C, na.rm = TRUE),
          Mean_nM_N = mean(nM_N, na.rm = TRUE),
          Mean_nM_S = mean(nM_S, na.rm = TRUE)) %>%
  left_join(., compound.order) 


#Make plot
g.plot.rel <- ggplot(g3.g4.transect.dat, aes(x = lat_bin, y=Mean_nM, fill = reorder(compound.name.figure, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15, position = "fill") +
  scale_fill_manual(values = compound.pal.fig)+
  guides(fill = guide_legend(ncol = 1)) +
  #  scale_fill_manual(values = stepped2(n = 20)) +
  scale_y_continuous(expand = c(0,NA,NA,NA)) +
  theme_test() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.8),
        axis.text.y = element_blank()) +
  theme(legend.key.size = unit(0.4, "cm"), legend.text = element_text(size = 6)) +
  ylab("Mole Fraction") +
  xlab("Latitude") +
  labs(fill = "Compound")
g.plot.rel




###DINIMITE 1 Surface Plots
d1.dat <- dat.all %>%
  filter(Cruise %in% c("RC078")) %>%
  filter(Compound %in% compound.order$Compound) %>%
  left_join(., compound.order) %>%
  filter(depth_m < 10)

###
d1.station.dat <- d1.dat %>%
  group_by(station, Compound) %>%
  reframe(Mean_nM = mean(nM.in.smp, na.rm = TRUE),
          Mean_nM_C = mean(nM_C, na.rm = TRUE),
          Mean_nM_N = mean(nM_N, na.rm = TRUE),
          Mean_nM_S = mean(nM_S, na.rm = TRUE)) %>%
  left_join(., compound.order) %>%
  filter(station %in% c(2, 3, 5, 6, 7)) %>%
  mutate(station = as.factor(station))

###
#####Plot
##DINIMITE Plot

d.plot.rel <- ggplot(d1.station.dat, aes(x = station, y=Mean_nM, fill = reorder(compound.name.figure, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15, position = "fill") +
  scale_fill_manual(values = compound.pal.fig)+
  guides(fill = guide_legend(ncol = 1)) +
  #  scale_fill_manual(values = stepped2(n = 20)) +
  scale_y_continuous(expand = c(0,NA,NA,NA)) +
  theme_test() + 
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  theme(legend.key.size = unit(0.4, "cm"), legend.text = element_text(size = 6)) +
  ylab("Mole Fraction") +
  xlab("Station") +
  labs(fill = "Compound")
d.plot.rel





##combine plots
rel.abun.plots <- g.plot.rel + d.plot.rel +
  plot_layout(guides = "collect", widths = c(0.65, 0.35)) +
  plot_annotation(tag_levels = "A")
rel.abun.plots


##Combine all pieces:

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


z <- (all.maps/poc.comb)/(rel.abun.plots) + 
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A")
z

z.2 <- ((big.map + PS.map + t.s.plot)/(poc.lat + poc.box +
  plot_layout(widths = c(7.5, 2.5))))/(g.plot.rel + d.plot.rel +
  plot_layout(widths = c(7.5, 2.5))) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A")

z.2

ggsave(z.2, file = "R_Code/Code_Development_Workspace/MT_Fig1_Map_Overview.pdf", 
       height = 6, width = 9, scale = 1.4)









#### Gather Surface Data:
dat.surface <- left_join(part.dat, meta.dat) %>%
  rename("Compound" = Name) %>%
  filter(Compound %in% compound.order$Compound) %>%
  left_join(., compound.order) %>%
  # filter(!station == 4) %>%
  filter(!SampID == "220902_Smp_TN397_S11_600_U_C") %>%
  unique() %>%
  filter(Cruise %in% c( "KM1906", "TN397", "RC078")) %>%
  filter(!Compound == "Threonine Betaine (tentative)") %>%
  filter(!Compound == "Homoserine Betaine (tentative)") %>%
  filter(depth_m < 10)




####Make Plot of POC and Total osmolyte C

tot.osmo.conc <- dat.surface %>%
  group_by(SampID, Cruise, Lat, Long, station) %>%
  reframe(Total.Osmo.Conc.nM = sum(nM.in.smp),
          Total.Osmo.Carbon.nM = sum(nM_C),
          Total.Osmo.Nitrogen.nM = sum(nM_N, na.rm = TRUE))





##Pull in environmental metadata 
g.env.dat <- read_csv("Intermediates/Gradients_Matched_Environmental_Metadata.csv") 
d.env.dat <- read_csv("Intermediates/RC078_MetaData_Compiled.csv") %>%
  filter(!is.na(depth_m),
         !is.na(station)) %>%
  # select(sample_id, parent_id, station, depth_m) %>%
  rename("SampID" = sample_id) %>%
  mutate(SampID = paste("221006_Smp_", SampID, sep = ""),
         Cruise = "RC078") %>%
  #  select(SampID, Cruise, station, depth_m, Cruise) %>%
  filter(!str_detect(SampID, "-")) %>%
  unique() %>%
  rename("poc" = "POC_uM",
         "pn" = "PN_uM",
         "sss" = sal,
         "sst" = temp,
         "chla" = Chl_fluor)

###Organize all enviro dat
all.env.dat <- g.env.dat %>%
  select(SampID, sst, sss, chla_interp, pc_interp, pn_interp) %>%
  rename("chla" = chla_interp,
         "poc" = pc_interp,
         "pn" = pn_interp) %>%
  rbind(., d.env.dat %>% select(SampID, sst, sss, chla, poc, pn)) 





##Combine osmo dat with environmental dat:
osmo.poc.dat <- left_join(tot.osmo.conc, all.env.dat) %>%
  filter(!Cruise == "RC078") %>%
  mutate("Perc_POC" = Total.Osmo.Carbon.nM/(poc*1000)*100,
         "Perc_PON" = Total.Osmo.Nitrogen.nM/(pn*1000)*100) %>%
  mutate(mean_perc_poc = mean(Perc_POC, na.rm = TRUE),
         sd_perc_poc = sd(Perc_POC, na.rm = TRUE),
         mean_perc_pon = mean(Perc_PON, na.rm = TRUE),
         sd_perc_pon = sd(Perc_PON, na.rm = TRUE))

poc.osmo.lm <- lm(Total.Osmo.Conc.nM~poc, data = osmo.poc.dat)
summary(poc.osmo.lm)


sss.osmo.lm <- lm(Total.Osmo.Conc.nM~sss, data = osmo.poc.dat)
summary(sss.osmo.lm)

ggplot(osmo.poc.dat, aes(x = Total.Osmo.Conc.nM, y = sss)) +
  geom_smooth(method = "lm") +
  geom_point()


ggplot(osmo.poc.dat, aes(x = Lat, y = Perc_POC)) +
  geom_smooth() +
  geom_point(size = 2) +
  ylab("Percent of POC in Osmolytes (%)")



ggplot(osmo.poc.dat) +
  geom_point(aes(x = Lat, y = poc)) +
  geom_point(aes(x = Lat, y = Total.Osmo.Conc.nM/5), color = "blue") +
  scale_y_continuous(name = "POC (uM)", sec.axis = sec_axis(trans=~.*(5), name="Total Osmo Conc. (nM)"))
  



g.sum <- g3.g4.transect.dat %>%
  ungroup() %>%
  mutate(total.conc = sum(Mean_nM)) %>%
  group_by(class) %>%
  reframe(class.conc = sum(Mean_nM),
          rel.conc = class.conc/total.conc) %>%
  unique()













































