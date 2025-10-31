




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
library(cmocean)




###define inputs
all.dat.file <- "Intermediates/Enviro_Osmo_Final_Dataset_with_metadata.csv"
g4.chl.file <- "Intermediates/MODIS_chla_data_g4.tsv"
g3.chl.file <- "Intermediates/MODIS_chla_data_g3.tsv"





#Read in data:

#Sample data
dat <- read_csv(all.dat.file) %>%
  left_join(., compound.order) %>%
  filter(!str_detect(Part.SampID, "Blk"))

#G4 Chla data:
g4.chla.dat <- read_csv(g4.chl.file)
g3.chla.dat <- read_csv(g3.chl.file)






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
  filter(!is.na(Region))


samp.locations <- dat.region %>% 
  select(Region, Cruise, Part.SampID, Diss.SampID, Lat, Long, chla, Station) %>%
  unique()

#Read in data
#dat <- read_csv(all.dat.file)




# Make Maps of North Pacific and Puget Sound ------------------------------


#load in map data
world <- ne_countries(scale = "medium", returnclass = "sf")


#Overview Map 
big.map <- ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-170, -110), ylim = c(-7, 57)) +
  geom_hline(yintercept = 0, size = 0.1, color = "black", linetype = "dashed") +
  geom_rect(xmin = -160.4, xmax = -110,
            ymin = -5, ymax = 35, alpha = 0, color = "black",
            size = 0.15) +
  geom_rect(xmin = -126.5, xmax = -121,
            ymin = 45.5, ymax = 51.5, alpha = 0, color = "black",
            size = 0.15) +
  geom_rect(xmin = -165, xmax = -150,
            ymin = 15, ymax = 50, alpha = 0, color = "black",
            size = 0.15)  + 
   geom_point(data = samp.locations, 
              aes(x = Long, y = Lat, fill = Region), 
              shape = 21,
              size = 3,
              stroke = 0.15) +
  scale_y_continuous(labels = scales::label_number(accuracy = 1)) +
  scale_x_continuous(labels = scales::label_number(accuracy = 1)) +
  # scale_fill_manual(values = region.pal) +
  scale_fill_manual(values = region.palette.7) +
  #  geom_jitter(data = dat.3, aes(Long, Lat, color = Local_Date), size = 4, alpha = 0.3) + 
  #  scale_color_viridis() +
  theme_test() +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(legend.position = "right")
  # annotate("segment", x = -117.5, xend = -138, y = 29.5, yend = 17, size = 0.15, color = "gray20") +
  # annotate("segment", x = -138, xend = -138, y = 15, yend = -3, size = 0.15, color = "gray20") +
  # annotate("segment", x = -142, xend = -154.5, y = -3, yend = 14, size = 0.15, color = "gray20") #+
 # geom_segment(aes(x = -117.5, xend = -138, y = 29.5, yend = 17), size = 0.15, color = "gray20")) +
 # geom_segment(aes(x = -138, xend = -138, y = 15, yend = -3), size = 0.15, color = "gray20") +
 # geom_segment(aes(x = -142, xend = -154.5, y = -3, yend = 14), size = 0.15, color = "gray20") +
 # theme(legend.position = "none")
big.map



# xcoord.g4 <- c(-160, -110)
# ycoord.g4 <- c(-5, 35)
#Make map of G4:

g4.samp.dat <- dat.region %>%
  filter(Cruise == "TN397") %>%
  select(Lat, Long, Region) %>%
  unique()

#plot map
g4.map <- ggplot(g4.chla.dat) +
  geom_raster(aes(x=lon, y=lat, fill = log(Chl))) +
  coord_fixed() + 
  scale_fill_cmocean(name = "delta", alpha = 0.70, limits = c(-3.5,1))  +
  geom_sf(data = world) +
  coord_sf(xlim = c(-160, -110), ylim = c(-5, 35)) +
  new_scale_fill() +
  geom_point(data = g4.samp.dat, aes(x = Long, y = Lat, fill = Region), 
             shape = 21, size = 3, stroke = 0.2) +
  scale_fill_manual(values = region.palette.7) +
  theme_test() +
  scale_y_continuous(expand = c(0,0), labels = scales::label_number(accuracy = 5)) +
  scale_x_continuous(expand = c(0,0), labels = scales::label_number(accuracy = 5)) +
  theme(legend.position = "none")+
  ylab("Latitude") + 
  xlab("Longitude") +
  annotate("segment", x = -117.5, xend = -138, y = 30, yend = 17, 
           size = 0.5, color = "black") +
  annotate("segment", x = -138, xend = -138, y = 15, yend = -3, 
           size = 0.5, color = "black") +
  annotate("text", x = -127, y = 22, label = "Leg 1", angle = 35) +
  annotate("text", x = -130, y = 0, label = "Legs 2 and 3") 
g4.map




#Make map of G3:
g3.samp.dat <- dat.region %>%
  filter(Cruise == "KM1906") %>%
  select(Lat, Long, Region) %>%
  unique()

#plot map
g3.map <- ggplot(g3.chla.dat) +
  geom_raster(aes(x=lon, y=lat, fill = log(Chl))) +
  coord_fixed() + 
  scale_fill_cmocean(name = "delta", alpha = 0.70, limits = c(-3.5,1))  +
  new_scale_fill() +
  geom_sf(data = world) +
  coord_sf(xlim = c(-167, -148), ylim = c(15, 50)) +
  geom_point(data = g3.samp.dat, aes(x = Long, y = Lat, fill = Region), 
             shape = 21, size = 3, stroke = 0.2) +
  scale_fill_manual(values = region.palette.7) +
  theme_test() +
  scale_y_continuous(expand = c(0,0), labels = scales::label_number(accuracy = 5)) +
  scale_x_continuous(expand = c(0,0), labels = scales::label_number(accuracy = 5)) +
  theme(legend.position = "none") +
  ylab("Latitude") + 
  xlab("Longitude") 
g3.map






####Make map of Puget Sound Stations:

#get map data:
ps.map.data <- map_data("worldHires", c("Canada", "usa", "Mexico"))

#get just PS experiment locatin data:
samp.locations.PS <- samp.locations %>%
  filter(Region == "SS") %>%
  filter(!Station %in% c(1, 8)) %>%
  group_by(Lat, Long, Station) %>%
  reframe(chla = mean(chla))
  


#Plot
PS.map <- ggplot() + 
  geom_polygon(data = ps.map.data, aes(x=long, y = lat, group = group),
               fill = "gray90", color = "black", linewidth = 0.2) +
  theme_bw()+
  coord_map("conic", lat0 = 18, xlim=c(-124.5, -122), ylim=c(47,49.75)) +
  geom_point(data = samp.locations.PS,
             aes(x = Long, y = Lat),
             fill = "white",
             shape = 21,
             size = 6,
             stroke = 0.25) +
  geom_point(data = samp.locations.PS,
             aes(x = Long, y = Lat, fill = log(chla)),
             shape = 21,
             size = 6,
             stroke = 0.25) +
  scale_fill_cmocean(name = "delta", 
                     limits = c(-3.5, 1),
                     alpha = 0.75) +
  geom_label_repel(data = samp.locations.PS,
                   aes(x = Long, y = Lat, label = Station),
                   box.padding = 1, min.segment.length = 0.1,
                   alpha = 1,
                   color = "black",
                   point.padding = 0.1,
                   # ylim = c(-2, 48),
                   size = 4,
                   segment.size = 0.2,
                   segment.curvature = -1e-20) +
  theme_test() +
  theme(axis.title.y = element_blank()) +
  scale_y_continuous(expand = c(0,0), labels = scales::label_number(accuracy = 1)) +
  scale_x_continuous(expand = c(0,0), labels = scales::label_number(accuracy = 1), breaks = c(-122, -123, -124)) +
  ylab("Lat") +
  xlab("Longitude") +
  theme(legend.position = "right")
  # theme(legend.position = "inside", legend.position.inside = c(0.2, 0.2),
  #       legend.box.background = element_rect(linewidth = 0.2, color = "black"),
  #       legend.box.margin = margin(10, 10, 10, 10))

PS.map



##Put all of these figures together:
# full.fig <- big.map + g4.map + g3.map + PS.map +
#   plot_layout(guides = "collect", nrow = 1)
# full.fig
# 
# ggsave(full.fig, filename = "Figures/Output_Oct25/Map_Figure.png",
#        dpi = 1200, height = 6, width = 8, scale = 1.3)

##arrange figures vertically
full.fig.vert <- big.map + g4.map + (g3.map + PS.map + plot_layout(nrow = 1)) +
  plot_layout(ncol = 1, guides = "collect") #&
 # theme(legend.position = "bottom")
full.fig.vert

ggsave(full.fig.vert, filename = "Figures/Output_Oct25/Map_Figure_Vertical.png",
       dpi = 600, height = 9, width = 4, scale = 1.35)














































































