







library(tidyverse)
library(lubridate)
library(patchwork)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")




###______________define inputs____________________

#Osmolytes
all.dat.file <- "Intermediates/Enviro_Osmo_Final_Dataset_with_metadata.csv"

#environmental data:
g3.file <- "Intermediates/G3_metadata_with_interpolations.csv"
g4.file <- "Intermediates/G4_metadata_with_interpolations.csv"

#atp data:
atp.file <- "Intermediates/ATP_G3_G4_data.csv"

#create extra region:
region.add <- data.frame(Cruise = c("TN397"), 
                         Region = c("NPTZ"))




##load in and organize environmental data:
g3.dat <- read_csv(g3.file)
g4.dat <- read_csv(g4.file)

gradients.enviro.dat <- full_join(g3.dat, g4.dat) %>%
  rename("UTC.time.round" = time) 



#load in ATP data:
atp.dat <- read_csv(atp.file)

#Combine gradients data with atp data
gradients.atp.dat <- left_join(atp.dat, gradients.enviro.dat) %>%
  select(Cruise, Station, Lat, Long, UTC.time.round, PATP_ng_L, SD_PATP_ng_L, ATP_Flag, sst, sss, chla_interp, pc_interp, pn_interp, N_N_interp) %>%
  rename("chla" = chla_interp,
         "poc" = pc_interp,
         "pn" = pn_interp,
         "N_N" = N_N_interp) 


#ATP living biomass data:
atp.live.bio <- gradients.atp.dat %>%
  mutate(Mean.living.biomass.umolC.L = 250*PATP_ng_L/12.01/1000,
         SD.living.biomass.umolC.L = 250*SD_PATP_ng_L/12.01/1000,
         Mean.Percent.Living= Mean.living.biomass.umolC.L/poc*100,
         SD.Percent.Living = SD.living.biomass.umolC.L/poc*100) %>%
  mutate(cruise.order = case_when(Cruise == "TN397" ~ 1,
                                  TRUE ~ 2))


#Load in osmolyte data:

#_____________Read in data________________________ 
osmo.dat <- read_csv(all.dat.file) %>%
  left_join(., compound.order) %>%
  filter(!str_detect(Part.SampID, "Blk")) %>%
  filter(!is.na(class)) %>%
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
  mutate(class = fct_relevel(class, c("AA", "Betaine", "Sugar", "Sulfonium", "Sulfonate", "Other"))) %>%
  filter(Part.detected == "Yes") %>%
  group_by(Parent_ID, Region, Cruise, Lat, Long, sss, sst, poc, chla, pn, N_N, SRP, sum.median.biovol.ul.l) %>%
  reframe(Sum.Part.Conc.nM = sum(Part.Conc.nM)) %>%
  mutate(Osmo.Poc.Ratio = Sum.Part.Conc.nM/poc) 


osmo.ratio.sum <- osmo.dat %>%
  group_by(Region, Cruise, Lat, Long, sss, sst, poc, chla) %>%
  reframe(Mean.osmo.poc.ratio = mean(Osmo.Poc.Ratio, na.rm = TRUE),
          SD.osmo.poc.ratio = sd(Osmo.Poc.Ratio, na.rm = TRUE)) %>%
  filter(!Cruise == "RC078") %>%
  mutate(cruise.order = case_when(Cruise == "TN397" ~ 1,
                                  TRUE ~ 2)) %>%
  filter(!Mean.osmo.poc.ratio == "NaN") %>%
  full_join(., region.add)
  



###Make plot:
g4.plot <- ggplot(osmo.ratio.sum %>% filter(Cruise == "TN397")) +
  geom_errorbar(aes(ymin = Mean.osmo.poc.ratio-SD.osmo.poc.ratio, ymax = Mean.osmo.poc.ratio+SD.osmo.poc.ratio, x = Lat), size = 0.2) +
  geom_point(aes(x = Lat, y = Mean.osmo.poc.ratio, fill = Region), shape = 21, size = 3, stroke = 0.25) +
  scale_fill_manual(values = region.palette.7) +
  scale_y_continuous(name = expression(Osmolyte/POC~Ratio~(nM~C/mu*M~C)), sec.axis = sec_axis(trans=~.*15, name=NULL), limits = c(0,7), expand = c(0,NA)) +
  geom_errorbar(data = atp.live.bio %>% filter(Cruise == "TN397"), 
                aes(ymin = Mean.Percent.Living/15-SD.Percent.Living/15, ymax = Mean.Percent.Living/15+SD.Percent.Living/15, x = Lat), size = 0.2) +
  geom_point(data = atp.live.bio %>% filter(Cruise == "TN397"), aes(x = Lat, y = Mean.Percent.Living/15), 
             shape = 22, stroke = 0.6, size = 2.5, fill = "gray90", alpha = 0.7)  +
  theme_bw() +
  annotate(geom = "text", x = 15, y = 6, label = "ATP-derived estimate") + 
  annotate(geom = "segment", y = 5.7, x = 15, xend = 17.5, yend = 47/15, linewidth = 0.2) +
  xlab("Latitude") +
  ggtitle("TN397") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y.right = element_blank()) 
g4.plot

g3.plot <- ggplot(osmo.ratio.sum %>% filter(Cruise == "KM1906")) +
  geom_errorbar(aes(ymin = Mean.osmo.poc.ratio-SD.osmo.poc.ratio, ymax = pmin(Mean.osmo.poc.ratio+SD.osmo.poc.ratio, 7), x = Lat), size = 0.2) +
  geom_point(aes(x = Lat, y = Mean.osmo.poc.ratio, fill = Region), shape = 21, size = 3, stroke = 0.25) +
  scale_fill_manual(values = region.palette.7) +
  scale_y_continuous(name = NULL, sec.axis = sec_axis(trans=~.*15, name= "Percent Living Biomass (%)"), limits = c(0,7), expand = c(0,NA)) +
  geom_errorbar(data = atp.live.bio %>% filter(Cruise == "KM1906"), 
                aes(ymin = Mean.Percent.Living/15-SD.Percent.Living/15, ymax = Mean.Percent.Living/15+SD.Percent.Living/15, x = Lat), size = 0.2) +
  geom_point(data = atp.live.bio %>% filter(Cruise == "KM1906"), aes(x = Lat, y = Mean.Percent.Living/15), 
             shape = 22, stroke = 0.6, size = 2.5, fill = "gray90", alpha = 0.7)  +
  theme_bw() +
 # scale_y_continuous(limits = c(0,7), expand = c(0,NA)) +
  theme(legend.position = "none",
        axis.text.y.left = element_blank()) +
        #axis.title.y = element_blank(),
       # axis.text.y = element_blank()) +
  xlab("Latitude") +
  ggtitle("KM1906") +
  theme(plot.title = element_text(hjust = 0.5))
g3.plot

comb.ratio.plot <- g4.plot + g3.plot + plot_layout(guides = "collect")
comb.ratio.plot


ggsave(comb.ratio.plot, dpi = 600, file = "Figures/Output_Oct25/Osmo_POC_Ratio_Figure.png",
       height = 3, width = 7.5, scale = 1.2)
