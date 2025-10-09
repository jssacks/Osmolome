






#load pacakges:
library(tidyverse)
library(viridis)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")


#load in dataset:
pnt.dat <- read_csv("Intermediates/particulate_osmo_data_raw.csv") %>%
  filter(Cruise == "PNT")


pnt.dat.sml <- pnt.dat %>%
  filter(!str_detect(Rep, "Std_")) %>%
  filter(!str_detect(Rep, "Poo_")) %>%
  filter(!str_detect(Rep, "Blk_")) %>%
  mutate(depth_m = case_when(str_detect(Rep, "150m") ~ 150,
                             str_detect(Rep, "175m") ~ 175,
                             str_detect(Rep, "200m") ~ 200,
                             str_detect(Rep, "300m") ~ 300)) %>%
  group_by(depth_m, Compound) %>%
  mutate(mean_area = mean(Area)) %>%
  left_join(., compound.order) %>%
  filter(!is.na(compound.name.figure))


ggplot(pnt.dat.sml, aes(x = depth_m, y = Area)) +
  geom_line(aes(y = mean_area, x = depth_m)) +
  geom_point() +
  coord_flip() +
  scale_x_reverse() +
  facet_wrap(.~compound.name.figure, scales = "free") +
  ylab("Peak Area/m2/day")


###Normalize PNT data to total signal in each sample:
pnt.rel <- pnt.dat.sml %>%
  group_by(Rep) %>%
  mutate(sum_samp_area = sum(Area)) %>%
  group_by(Rep, Compound) %>%
  mutate(rel_area = Area/sum_samp_area)



ggplot(pnt.rel, aes(x = depth_m, y = rel_area)) +
  #geom_line(aes(y = mean_area, x = depth_m)) +
  geom_point() +
  coord_flip() +
  scale_x_reverse() +
  facet_wrap(.~compound.name.figure, scales = "free")

pnt.rel.sml <- pnt.rel %>%
  filter(compound.name.figure %in% c("DHPS", "Glutamic acid", "TMAO", "Hydroxyectoine")) %>%
  group_by(depth_m, Compound) %>%
  mutate(mean_rel_area = mean(rel_area)) 

ggplot(pnt.rel.sml, aes(x = depth_m, y = rel_area)) +
  geom_line(aes(y = mean_rel_area, x = depth_m, color = Compound)) +
  geom_point(aes(fill = Compound), shape = 21, stroke = 0.5, size = 2.5) +
  scale_color_viridis(discrete = TRUE, end = 0.8) +
  scale_fill_viridis(discrete = TRUE, end = 0.8) +
  coord_flip() +
  scale_x_reverse() +
  facet_wrap(.~compound.name.figure, scales = "free") +
  theme_test() +
  theme(legend.position = "none",
        strip.text = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)
        ) +
  xlab("Depth (m)") +
  ylab("Relative Peak Area")















































