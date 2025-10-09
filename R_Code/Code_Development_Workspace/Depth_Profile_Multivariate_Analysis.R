







library(tidyverse)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")


##Define inputs:
part.file <- "Intermediates/Particulate_Quant_Output.csv"
meta.file <- "Intermediates/All_metadata_information.csv"

###load in data and combine with metadata
part.dat <- read_csv(part.file) %>%
  filter(Cruise %in% c("G4_DepthProfiles", "G3_DepthProfiles"))
meta.dat <- read_csv(meta.file) %>%
  filter(Cruise %in% c("G4_DepthProfiles", "G3_DepthProfiles")) %>%
  rename(SampID = Part.SampID)


####
dat.all <- left_join(part.dat, meta.dat) %>%
  rename("Compound" = Name)


dat.depth.profile <- dat.all %>%
  filter(Cruise %in% c("G4_DepthProfiles", "G3_DepthProfiles")) %>%
  filter(Compound %in% compound.order$Compound) %>%
  group_by(Cruise, station, Compound, depth_m) %>%
  reframe(Mean_nM = mean(nM.in.smp),
          Mean_nM_C = mean(nM_C),
          Mean_nM_N = mean(nM_N))  %>%
  mutate(station.name = case_when(Cruise == "G4_DepthProfiles" & station == 4 ~ "G4 Station 04 (19.6 N)",
                                  Cruise == "G4_DepthProfiles" & station == 7 ~ "G4 Station 07 (10.7 N)",
                                  Cruise == "G4_DepthProfiles" & station == 9 ~ "G4 Station 09 (4.7 N)",
                                  Cruise == "G4_DepthProfiles" & station == 11 ~ "G4 Station 11 (0.14 N)",
                                  Cruise == "G3_DepthProfiles" & station == 4 ~ "G3 Station 04 (41.4 N)",
                                  Cruise == "G3_DepthProfiles" & station == 5 ~ "G3 Station 05 (37.0 N)",
                                  Cruise == "G3_DepthProfiles" & station == 6 ~ "G3 Station 06 (33.0 N)")) %>%
  left_join(., compound.order) %>%
  filter(!is.na(station.name))


dat.depth.profile.2 <- dat.depth.profile %>%
  group_by(station.name, depth_m) %>%
  mutate(tot.nM = sum(Mean_nM),
         rel.abun = Mean_nM/tot.nM)



dat.clust <- dat.all %>%
  filter(Cruise %in% c("G4_DepthProfiles", "G3_DepthProfiles")) %>%
  filter(Compound %in% compound.order$Compound) %>%
  mutate(station.name = case_when(Cruise == "G4_DepthProfiles" & station == 4 ~ "G4 Station 04 (19.6 N)",
                                  Cruise == "G4_DepthProfiles" & station == 7 ~ "G4 Station 07 (10.7 N)",
                                  Cruise == "G4_DepthProfiles" & station == 9 ~ "G4 Station 09 (4.7 N)",
                                  Cruise == "G4_DepthProfiles" & station == 11 ~ "G4 Station 11 (0.14 N)",
                                  Cruise == "G3_DepthProfiles" & station == 4 ~ "G3 Station 04 (41.4 N)",
                                  Cruise == "G3_DepthProfiles" & station == 5 ~ "G3 Station 05 (37.0 N)",
                                  Cruise == "G3_DepthProfiles" & station == 6 ~ "G3 Station 06 (33.0 N)")) %>%
  left_join(., compound.order) %>%
  select(SampID, compound.name.figure, nM.in.smp, depth_m, station.name) %>%
  filter(!is.na(station.name))



dat.rel.abun <- dat.clust %>%
  group_by(SampID) %>%
  mutate(rel.abun = nM.in.smp/sum(nM.in.smp)) %>%
  select(SampID, rel.abun, compound.name.figure) %>%
  pivot_wider(names_from = SampID, id_cols = compound.name.figure, values_from = rel.abun) %>%
  column_to_rownames(var = "compound.name.figure")




dat.dp.stand <- dat.clust %>%
  group_by(SampID) %>%
  mutate(rel.abun = nM.in.smp/sum(nM.in.smp)) %>%
  group_by(station.name, compound.name.figure) %>%
  mutate(norm.rel.abun = (rel.abun-min(rel.abun))/(max(rel.abun)-min(rel.abun))) %>%
  ungroup()

#visualize
ggplot(dat.dp.stand, aes(x = norm.rel.abun, y = depth_m, color = station.name)) +
  geom_point() + 
  facet_wrap(.~compound.name.figure) +
  scale_y_reverse() 


#change into a matrix:
dat.dp.stand.matrix <- dat.dp.stand %>%
  select(SampID, norm.rel.abun, compound.name.figure) %>%
  pivot_wider(names_from = SampID, id_cols = compound.name.figure, values_from = norm.rel.abun) %>%
  column_to_rownames(var = "compound.name.figure")


#####Run k-means clustering:


library(vegan)

library(factoextra)



#organize data
k.rel.dat <- decostand(dat.rel.abun, method = "max", MARGIN = 1)


cluster.comp <- fviz_nbclust(dat.dp.stand.matrix, FUNcluster = kmeans, nboot = 100)

cluster.comp

#4-5 clusters suggested... 

##run clustering algorithim 
kmeans.1 <- kmeans(dat.dp.stand.matrix, centers = 4, iter.max = 500, nstart = 500)


clust.dat <- data.frame(kmeans.1$cluster) %>%
  rownames_to_column(var = "compound.name.figure") %>%
  rename("cluster" = kmeans.1.cluster)


clust.abun.dat <- dat.dp.stand %>%
  left_join(., clust.dat)



clust.means.dat <- clust.abun.dat %>%
  group_by(station.name, depth_m, cluster) %>%
  mutate(mean.rel.abun = mean(norm.rel.abun, na.rm = TRUE),
        sd.rel.abun = sd(norm.rel.abun, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(station.name, depth_m, compound.name.figure) %>%
  mutate(comp.norm.rel.abun = mean(norm.rel.abun))


cluster.means.plot <- ggplot(clust.means.dat, aes(y = mean.rel.abun, x = depth_m, color = as.factor(cluster))) +
  geom_line(aes(y = comp.norm.rel.abun, x = depth_m, group = compound.name.figure), color = "black", alpha = 0.10) +
  geom_line() +
  geom_point(size = 3, shape = 21, stroke = 0.1, aes(fill = as.factor(cluster))) +
  coord_flip() +
  scale_x_reverse() +
  facet_grid(cluster~station.name) +
  theme_test()


ggsave(cluster.means.plot, filename = "Figures/Depth_Profiles/depthprofile_cluster_means.pdf", height = 8, width = 9, scale = 1.3)




###Overall Cluster behavior figure:
clust.means.dat.sml <- clust.means.dat %>%
  ungroup() %>%
  select(cluster, depth_m, station.name, mean.rel.abun) %>%
  group_by(cluster, depth_m) %>%
  mutate(cluster.overall.mean = mean(mean.rel.abun)) %>%
  unique()



#below figure not currently working because depths need to be grouped... 
ggplot(clust.means.dat.sml) +
  geom_line(aes(y = mean.rel.abun, x = depth_m, group = station.name), color = "black", alpha = 0.2) +
  geom_line(aes(y = cluster.overall.mean, x = depth_m), size = 1, color = "red") +
  coord_flip() +
  scale_x_reverse() +
  facet_wrap(.~cluster) +
  theme_test()












####NMDS Plots:
g.matrix <- gradients %>%
  select(SampID, Compound, nM.in.smp) %>%
  pivot_wider(id_cols = SampID, names_from = Compound, values_from = nM.in.smp) %>%
  column_to_rownames(var = "SampID")

#meta data
samp.meta.dat <- gradients %>%
  select(SampID, Cruise, Lat, Long, depth_m) %>%
  unique()



dat.ord <- dat.clust %>%
  select(SampID, nM.in.smp, compound.name.figure) %>%
  pivot_wider(id_cols = SampID, names_from = compound.name.figure, values_from = nM.in.smp) %>%
  column_to_rownames(var = "SampID")

  
ord.meta.dat <- dat.clust %>%
  select(SampID, depth_m, station.name) %>%
  unique()


###Standardize and calculate distance matrix:

dat.ord.norm <- decostand(dat.ord, method = "normalize", MARGIN = 1)

dat.ord.bray <- vegdist(dat.ord.norm)

nmds.out <- metaMDS(dat.ord.bray)

plot(nmds.out)



#collect data from NMDS plot:
nmds.out.plot <- data.frame(nmds.out$points) %>%
  rownames_to_column(var = "SampID") %>%
  left_join(., ord.meta.dat) %>%
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

library(viridis)

nmds.depth <- ggplot(nmds.out.plot, aes(y = MDS1, x = MDS2, fill = depth_bin)) +
  geom_point(size = 3.5, shape = 21, color = "black", stroke = 0.15, alpha = 0.8) +
  coord_fixed() +
  scale_y_reverse() +
  scale_fill_viridis(discrete = TRUE, begin = 1, end = 0) +
  theme_bw()
nmds.depth
ggsave(nmds.depth, filename = "Figures/Depth_Profiles/DepthProfile_NMDS_Depth.pdf", height = 6, width = 6)



nmds.station <- ggplot(nmds.out.plot, aes(y = MDS1, x = MDS2, fill = station.name)) +
  geom_point(size = 3.5, shape = 21, color = "black", stroke = 0.15, alpha = 0.9) +
  coord_fixed() +
  scale_y_reverse() +
  scale_fill_viridis(discrete = TRUE, begin = 1, end = 0, option = "G") +
  theme_bw()
nmds.station

ggsave(nmds.station, filename = "Figures/Depth_Profiles/DepthProfile_NMDS_station.pdf", height = 6, width = 6)


##PERMANOVA
p.1 <- adonis2(dat.ord.bray ~ station.name*depth_m, data = ord.meta.dat, permutations = 1000)
p.1






























































































