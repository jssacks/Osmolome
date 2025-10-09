





















library(tidyverse)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")

##Define inputs:
part.file <- "Intermediates/Particulate_Quant_Output.csv"
meta.file <- "Intermediates/All_metadata_information.csv"

###load in data and combine with metadata
part.dat <- read_csv(part.file)
meta.dat <- read_csv(meta.file) 


####
dat.all <- left_join(part.dat, meta.dat) %>%
  rename("Compound" = Name)


gradients <- dat.all %>%
  filter(Cruise %in% c("TN397", "KM1906")) %>%
  filter(Compound %in% compound.order$Compound) %>%
  left_join(., compound.order) %>%
 # filter(!station == 4) %>%
  filter(!SampID == "220902_Smp_TN397_S11_600_U_C") %>%
  unique() 
#  filter(!depth_m == 0)


g.matrix <- gradients %>%
  select(SampID, Compound, nM.in.smp) %>%
  pivot_wider(id_cols = SampID, names_from = Compound, values_from = nM.in.smp) %>%
  column_to_rownames(var = "SampID")

#meta data
samp.meta.dat <- gradients %>%
  select(SampID, Cruise, Lat, Long, depth_m) %>%
  unique()



###standardize data and make NMDS plot:
library(vegan)

g.matrix.stand <- decostand(g.matrix, method = "total", MARGIN = 1)

nmds.dist <- vegdist(x = g.matrix.stand, method = "bray")

nmds.out <- metaMDS(nmds.dist, k=2, trymax = 50)



###
nmds.out.plot <- data.frame(nmds.out$points) %>%
  rownames_to_column(var = "SampID") %>%
  left_join(., samp.meta.dat) %>%
  mutate(depth_bin = case_when(depth_m < 51 ~ "0-50",
                               depth_m < 101 & depth_m > 50 ~ "50-100",
                               depth_m < 151 & depth_m > 101 ~ "100-150",
                               depth_m < 201 & depth_m > 151 ~ "150-200",
                               depth_m < 301 & depth_m > 201 ~ "200-300",
                               depth_m < 501 & depth_m > 301 ~ "300-500",
                               depth_m > 501 ~ "1000")) %>%
  mutate(depth_bin = as.factor(depth_bin)) %>%
  mutate(depth_bin = fct_relevel(depth_bin, c("0-50", "50-100", "100-150", "150-200",
                                              "200-300", "300-500", "1000")))


ggplot(nmds.out.plot, aes(x = MDS1, y = MDS2, fill = Lat)) +
  geom_point(shape = 21, size = 3, stroke = 0.15) +
  scale_fill_viridis()

ggplot(nmds.out.plot, aes(x = MDS1, y = MDS2, fill = Long)) +
  geom_point(shape = 21, size = 3, stroke = 0.15) +
  scale_fill_viridis()


ggplot(nmds.out.plot, aes(x = MDS1, y = MDS2, fill = Cruise)) +
  geom_point(shape = 21, size = 3, stroke = 0.15) +
  scale_fill_viridis(discrete = TRUE, begin = 0.5)

ggplot(nmds.out.plot, aes(x = MDS1, y = MDS2, fill = depth_bin)) +
  geom_point(shape = 21, size = 3, stroke = 0.15) #+ 
  scale_fill_viridis(discrete = TRUE)
  #scale_fill_stepsn(colours = c("darkred","white", "steelblue"), n.breaks = 8)
  
  scale_fill_binned(type = "viridis", n.breaks = 20,)



##Try with Depth Profiles
###standardize data and make NMDS plot:

###standardize data and make NMDS plot:
dp.dat <- dat.all %>%
  filter(Cruise %in% c("G3_DepthProfiles", "G4_DepthProfiles")) %>%
  filter(Compound %in% compound.order$Compound) %>%
  left_join(., compound.order) %>%
  # filter(!station == 4) %>%
  filter(!SampID == "220902_Smp_TN397_S11_600_U_C") %>%
  unique() 
#  filter(!depth_m == 0)


d.matrix <- dp.dat %>%
  select(SampID, Compound, nM.in.smp) %>%
  pivot_wider(id_cols = SampID, names_from = Compound, values_from = nM.in.smp) %>%
  column_to_rownames(var = "SampID")

#meta data
samp.meta.dat <- dp.dat %>%
  select(SampID, Cruise, Lat, Long, depth_m) %>%
  unique()


d.matrix.stand <- decostand(d.matrix, method = "total", MARGIN = 1)

nmds.dist <- vegdist(x = d.matrix.stand, method = "bray")

nmds.out <- metaMDS(nmds.dist, k=2, trymax = 50)




