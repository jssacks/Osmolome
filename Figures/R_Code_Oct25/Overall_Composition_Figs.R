





library(tidyverse)
library(patchwork)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")





###define inputs
all.dat.file <- "Intermediates/Enviro_Osmo_Final_Dataset_with_metadata.csv"






#Read in data 
dat <- read_csv(all.dat.file) %>%
  left_join(., compound.order) %>%
  filter(!str_detect(Part.SampID, "Blk"))


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









##Particulate
# Part.dat.all <- dat.region %>%
  # filter(Part.detected == "Yes") %>%
  # group_by(Part.SampID) %>%
  # mutate(sum.part.conc = sum(Part.Conc.nM)) %>%
  # group_by(Part.SampID, class) %>%
  # reframe(class.sum.conc = sum(Part.Conc.nM),
  #         class.rel.conc = class.sum.conc/sum.part.conc) %>%
  # unique() %>%
  # mutate(Region = "All")

Part.sum.dat <- dat.region %>%
  filter(Part.detected == "Yes") %>%
  group_by(Part.SampID) %>%
  mutate(sum.part.conc = sum(Part.Conc.nM)) %>%
  group_by(Part.SampID, class, Region) %>%
  reframe(class.sum.conc = sum(Part.Conc.nM),
          class.rel.conc = class.sum.conc/sum.part.conc) %>%
  unique() 

ggplot(Part.sum.dat, aes(x = class, y = class.rel.conc)) +
  geom_boxplot(aes(color = Region)) +
  geom_point(aes(fill = Region), position = position_jitterdodge(jitter.width = 0.2), 
             shape = 21, stroke = 0.1, color = "black") +
  
 # geom_jitter(aes(color = Region), shape = 21, width = 0.2) +
  scale_color_manual(values = region.palette.7) +
  scale_fill_manual(values = region.palette.7) +
  theme_test()



##Dissolved
Diss.sum.dat <- dat.region %>%
  filter(Diss.detected == "Yes") %>%
  group_by(Diss.SampID) %>%
  mutate(sum.diss.conc = sum(Diss.Conc.nM)) %>%
  group_by(Diss.SampID, class, Region) %>%
  reframe(class.sum.conc = sum(Diss.Conc.nM),
          class.rel.conc = class.sum.conc/sum.diss.conc) %>%
  unique()
# 
# ggplot(Diss.sum.dat, aes(x = class, y = class.rel.conc)) +
#   geom_boxplot(aes(color = Region)) +
#   scale_color_manual(values = region.palette.7) +
#   geom_point(aes(color = Region))
#   
#   geom_jitter(aes(color = Region, position = position_dodge(width = 0.1)), shape = 21, width = 0.2)

ggplot(Diss.sum.dat, aes(x = class, y = class.rel.conc)) +
    geom_boxplot(aes(color = Region)) +
    geom_point(aes(fill = Region), position = position_jitterdodge(jitter.width = 0.1), 
               shape = 21, stroke = 0.1, color = "black") +
    
    # geom_jitter(aes(color = Region), shape = 21, width = 0.2) +
    scale_color_manual(values = region.palette.7) +
    scale_fill_manual(values = region.palette.7) +
    theme_test()






###Rank relationship data:
dat.rank.p <- dat.region %>%
  group_by(compound.name.figure, Region) %>%
  reframe(mean.conc = mean(Part.Conc.nM, na.rm = TRUE)) %>%
  group_by(Region) %>%
  mutate(rel.conc = mean.conc/sum(mean.conc),
         rank = rank(1/mean.conc))


ggplot(dat.rank.p, aes(x = rank, y = rel.conc, color = Region)) +
  geom_line() +
  geom_point() 

ggplot(dat.rank.p, aes(rel.conc, color = Region)) +
  stat_ecdf()


















