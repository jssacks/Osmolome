
library(tidyverse)
library(vegan)


osmo.dat <- read_csv("Intermediates/Diss_Part_Metab_Combined.csv")



osmo.dat.mantel <- osmo.dat %>%
  filter(!Cruise %in% c("KM1906", "G3_DepthProfiles")) %>%
  filter(!compound.name.figure %in% c("DMSP")) 

samp.meta.dat <- osmo.dat.mantel %>%
  select(SampID.shared, Cruise, Lat, Long, Region) %>%
  unique()


#particulate
o.dat.p <- osmo.dat.mantel %>%
  mutate(log10.nM.p = log10(nM.in.smp.part)) %>%
  filter(!Compound %in% c("Arsenobetaine", "L-Isoleucine")) %>%
  select(SampID.shared, Compound, log10.nM.p) %>%
  pivot_wider(id_cols = SampID.shared, names_from = Compound, values_from = log10.nM.p) %>%
  column_to_rownames(var = "SampID.shared")

#dissolved
o.dat.d <- osmo.dat.mantel %>%
  mutate(log10.nM.d = log10(nM.in.smp.diss)) %>%
  filter(!Compound %in% c("Arsenobetaine", "L-Isoleucine")) %>%
  select(SampID.shared, Compound, log10.nM.d) %>%
  pivot_wider(id_cols = SampID.shared, names_from = Compound, values_from = log10.nM.d) %>%
  column_to_rownames(var = "SampID.shared")



#Standardize data
p.stand<- decostand(o.dat.p, method = "range", MARGIN = 2)
d.stand <- decostand(o.dat.d, method = "range", MARGIN = 2) 


#convert to distance matrices 
p.dist <- vegdist(p.stand, method = "euclidean")
d.dist <- vegdist(d.stand, method = "euclidean")


#attempt Mantel Test:
m.test <- mantel(p.dist, d.dist)

m.test




####Run PCA on dissolved data:

d.pca <- rda(d.stand, scale = TRUE)

print(d.pca)
summary(d.pca)
pca.comp <- data.frame(d.pca[["CA"]][["v"]])
pca.samp <- data.frame(d.pca[["CA"]][["u"]])


pca.samp.plot <- pca.samp %>%
  rownames_to_column(var = "SampID.shared") %>%
  left_join(samp.meta.dat)

pca.comp.plot <- pca.comp %>%
  rownames_to_column(var = "Compound") %>%
  left_join(., compound.order)


diss.pca <- ggplot(pca.samp.plot, aes(x = PC1, y = PC2, fill = Region))  +
  geom_point(shape = 21, size = 3, stroke = 0.15) +
  scale_fill_manual(values = region.palette) +
  theme_test() +
  xlab("PC1 (54%)") + 
  ylab("PC2 (11%)") 
  
diss.pca

ggsave(diss.pca, file = "R_Code/Code_Development_Workspace/Diss_PCA.pdf",
       height = 3.5, width = 4.25, scale = 1.25)






# ggplot(pca.comp.plot, aes(x = PC1, y = PC2, fill = reorder(compound.name.figure, order))) +
#   geom_point(shape = 21, size = 3, stroke = 0.15) +
#   scale_fill_manual(values = compound.pal.fig) +
#   theme_test()









































































































