



##Organize data:
library(patchwork)
library(viridis)


library(tidyverse)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")

##Define inputs:
part.file <- "Intermediates/Particulate_Quant_Output.csv"
meta.file <- "Intermediates/All_metadata_information.csv"

###load in data and combine with metadata
part.dat <- read_csv(part.file)
meta.dat <- read_csv(meta.file)



####
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



#  filter(!depth_m == 0)


osmo.matrix <- dat.all %>%
  mutate(log10.nM.in.smp = log10(nM.in.smp)) %>%
  select(SampID, Compound, log10.nM.in.smp) %>%
  pivot_wider(id_cols = SampID, names_from = Compound, values_from = log10.nM.in.smp) %>%
  column_to_rownames(var = "SampID")

#meta data
samp.meta.dat <- dat.all %>%
  select(SampID, Cruise, Lat, Long, depth_m) %>%
  unique() %>%
  mutate(Region = case_when(Cruise == "RC078" ~ "PS",
                            Lat > 36 ~ "NPTZ",
                            Lat < 7 ~ "Equator",
                            Cruise == "TN397" & Lat > 29 ~ "CC",
                            TRUE ~ "NPSG"))


###standardize data and make NMDS plot:
library(vegan)

osmo.matrix.stand <- decostand(osmo.matrix, method = "range", MARGIN = 2)

#osmo.dist.matrix <- vegdist(osmo.matrix.stand, method = "euclidean")

osmo.pca <- rda(osmo.matrix, scale = TRUE)

print(osmo.pca)

plot(osmo.pca)

summary(osmo.pca)


pca.comp <- data.frame(osmo.pca[["CA"]][["v"]])

pca.samp <- data.frame(osmo.pca[["CA"]][["u"]])

pca.samp.plot <- pca.samp %>%
  rownames_to_column(var = "SampID") %>%
  left_join(samp.meta.dat)

ggplot(pca.samp.plot, aes(x = PC1, y = PC2)) +
  geom_point(shape = 21, aes(fill = Lat), stroke = 0.15, size = 3) +
  scale_fill_viridis() +
  xlab("PC1 (68%)") +
  ylab("PC1 (11%)")

ggplot(pca.samp.plot, aes(x = PC1, y = PC2)) +
  geom_point(shape = 21, aes(fill = Long), stroke = 0.15, size = 3) +
  scale_fill_viridis() +
  xlab("PC1 (68%)") +
  ylab("PC1 (11%)")

ggplot(pca.samp.plot, aes(x = PC1, y = PC2)) +
  geom_point(shape = 21, aes(fill = Cruise), stroke = 0.15, size = 3) +
  scale_fill_viridis(discrete = TRUE) +
  xlab("PC1 (68%)") +
  ylab("PC1 (11%)")

region.pca <- ggplot(pca.samp.plot, aes(x = PC1, y = PC2, shape = Cruise)) +
  geom_point(aes(fill = Region), stroke = 0.15, size = 2.5, shape = 21) +
#  scale_shape_manual(values = c(22, 21, 23)) +
  scale_fill_manual(values = region.palette) +
  xlab("PC1 (70%)") +
  ylab("PC1 (9%)") +
  theme_test()


##Pull in metadata 
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
         "sss" = sal,
         "sst" = temp,
         "chla" = Chl_fluor)
  
  

###Make all enviro dat
all.env.dat <- g.env.dat %>%
  select(SampID, sst, sss, chla_interp, pc_interp) %>%
  rename("chla" = chla_interp,
         "poc" = pc_interp) %>%
  rbind(., d.env.dat %>% select(SampID, sst, sss, chla, poc)) 










###pca.enviro.dat

pca.env.dat <- pca.samp.plot %>%
  select(SampID, PC1, PC2, Cruise) %>%
  left_join(., all.env.dat) %>%
  filter(Cruise %in% c("TN397", "KM1906", "RC078")) %>%
  left_join(., samp.meta.dat)



##__T+S diagram:
t.s.plot <- ggplot(pca.env.dat, aes(x = sst, y = sss, fill = Region)) +
  geom_jitter(shape = 21, aes(fill = Region), size = 3, stroke = 0.15, width = 0.5, height = 0.25) +
  theme_test() +
  scale_fill_manual(values = region.palette) 
t.s.plot




#poc normal
ggplot(pca.env.dat, aes(x = PC1, y = poc)) +
  geom_smooth(method = "lm") +
  geom_point(shape = 21, fill = "darkgray") 

#poc log10
poc.fig <- ggplot(pca.env.dat, aes(x = PC1, y = poc)) +
  geom_smooth(method = "lm", color = "black") +
  geom_point(shape = 21, aes(fill = Region), size =2.5, stroke = 0.15) +
  scale_y_log10() +
  scale_fill_manual(values = region.palette) +
  theme_test() +
  theme(panel.border = element_blank(), axis.line = element_line()) +
  annotate("text", x = 0.15, y = 2.5, 
           label = expression(R^2~"="~0.89),
           size = 4) 
poc.fig


poc.lm <- lm(PC1 ~ log10(poc), pca.env.dat)
summary(poc.lm)

#sss
sss.fig <- ggplot(pca.env.dat, aes(x = PC1, y = sss)) +
  geom_smooth(method = "lm", color = "black") +
  geom_point(shape = 21, aes(fill = Region), size =2.5, stroke = 0.15) +
#  scale_y_log10() +
  scale_fill_manual(values = region.palette) +
  theme_test() +
  theme(panel.border = element_blank(), axis.line = element_line()) +
  annotate("text", x = 0.15, y = 34, 
           label = expression(R^2~"="~0.77),
           size = 4) 
sss.fig

#Put plots together:
library(patchwork)

full.plot <- region.pca + (poc.fig/sss.fig) +
  plot_layout(guides = 'collect', widths = c(0.65, 0.35)) +
  plot_annotation(tag_levels = "A") 

full.plot

ggsave(full.plot, file = "R_Code/Code_Development_Workspace/PCA_Plot.pdf", 
          height = 4, width = 9)

















#sss
ggplot(pca.env.dat, aes(x = PC1, y = sss)) +
  geom_smooth(method = "lm") +
  geom_point(shape = 21, fill = "darkgray") 

sss.lm <- lm(PC1 ~ sss, pca.env.dat)
summary(sss.lm)


#sst
ggplot(pca.env.dat, aes(x = PC2, y = sst)) +
  geom_smooth(method = "lm") +
  geom_point(shape = 21, fill = "darkgray") 


#chla
ggplot(pca.env.dat, aes(x = PC1, y = chla)) +
  geom_smooth(method = "lm") +
  geom_point(shape = 21, fill = "darkgray") +
  scale_y_log10()

chla.lm <- lm(PC1 ~ log10(chla), pca.env.dat)
summary(chla.lm)

ggplot(pca.env.dat, aes(x = poc, y = chla)) +
  geom_point()


###Estimate % of POC that is osmolyte:
all.biomass.dat <- all.env.dat %>%
  left_join(part.dat %>% 
              group_by(SampID) %>%
              reframe(tot.nM.C = sum(nM_C))) %>%
  mutate(Perc.POC = tot.nM.C/(poc*1000)*100) %>%
  filter(Perc.POC < 10) %>%
  left_join(., meta.dat) %>%
  filter(depth_m < 10)

ggplot(all.biomass.dat, aes(y = Perc.POC, x = Lat, fill = Long)) +
  geom_point(shape = 21, stroke = 0.15, size = 3) + 
  scale_fill_viridis() +
  ylab("Percent of POC in Osmolytes (%)")


ggplot(all.biomass.dat, aes(y = tot.nM.C, x = poc, fill = Lat)) +
  geom_point(shape = 21, stroke = 0.15, size = 3) + 
  scale_fill_viridis() +
  scale_x_log10() +
  scale_y_log10() + 
  xlab("POC (uM)") +
  ylab("Osmolyte C (nM)")
