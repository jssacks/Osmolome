




library(tidyverse)
library(rstatix)





###______________define inputs____________________

#Osmolytes and enviro data:
all.dat.file <- "Intermediates/Enviro_Osmo_Final_Dataset_with_metadata.csv"

#atp 
atp.file <- "Intermediates/ATP_G3_G4_data.csv"






#_____________Read in data________________________ 
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


###__XXX___
dat.comp.paired <- dat.region %>%
  filter(Part.detected == "Yes",
         Diss.detected == "Yes") %>%
  select(Cruise, Region, Parent_ID, compound.name.figure, Part.Conc.nM, Diss.Conc.nM) %>%
  mutate(Log10.Part.Conc.nM = log10(Part.Conc.nM),
         Log10.Diss.Conc.nM = log10(Diss.Conc.nM))


#visualize relationships
part.diss.fig <- ggplot(dat.comp.paired, aes(x = Part.Conc.nM, Diss.Conc.nM)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", color = "black") +
  geom_point(shape = 21, stroke = 0.1, aes(fill = Region)) +
  scale_fill_manual(values = region.palette.7) +
  facet_wrap(.~compound.name.figure, scales = "free", ncol = ) +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw() +
  xlab("Particulate Concentration (nM)") +
  ylab("Dissolved Concentration (nM)")

ggsave(part.diss.fig, file = "Figures/Output_Oct25/Part_Diss_Supplemental_Figure.png",
       dpi = 900, height = 10, width = 8, scale = 1.1)
