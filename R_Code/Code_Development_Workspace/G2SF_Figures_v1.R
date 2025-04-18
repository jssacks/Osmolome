



library(tidyverse)
library(ggdendro)
library(viridis)
library(vegan)

source("R_Code/Code_Development_Workspace/Figure_Palettes.R")



osmo.file <- "Intermediates/G2SF_Quant_Output.csv"
meta.file <- "Meta_Data/G2_SF_Sample_MetaData.csv"




###
g2.dat <- read_csv(osmo.file)
meta.dat <- read_csv(meta.file)







###
sf.dat <- g2.dat %>%
  left_join(., meta.dat)  %>%
  rename(Compound = Name) %>%
  group_by(Compound, Station, Lat, Size_Fraction) %>%
  reframe(Mean.nM = mean(nM.in.smp),
          Mean.nM.C = mean(nM_C),
          Mean.nM.N = mean(nM_N),
          Mean.nM.S = mean(nM_S)) %>%
  group_by(Compound, Station, Lat) %>%
 # mutate(Mean.Area = case_when(Size_Fraction == "F" & Mean.Area > Mean.Area[Size_Fraction == "S"] ~ Mean.Area-Mean.Area[Size_Fraction == "S"],
  #                             Size_Fraction == "F" & Mean.Area < Mean.Area[Size_Fraction == "S"] ~ 1,
  #                             TRUE ~ Mean.Area)) %>%
  mutate(Size_Fraction = str_replace(Size_Fraction, "F", "L")) %>%
  mutate(Small_Frac_Perc = Mean.nM[Size_Fraction == "S"]/(Mean.nM[Size_Fraction == "S"] + Mean.nM[Size_Fraction == "L"])) %>%
  ungroup() %>%
  filter(!Compound %in% c("Trimethylamine N-oxide", "L-Cysteic acid", "L-Lysine", "L-Tyrosine", "L-Serine", "L-Methionine", "L-Arginine")) %>%
  filter(!str_detect(Compound, "tentative")) %>%
  left_join(., compound.order)



##Make stacked bar chart:
ggplot(sf.dat, aes(x = as.factor(Lat), y = Mean.nM, fill = Size_Fraction)) +
  geom_col(width = 0.5) +
  facet_wrap(.~Compound, scales = "free") +
  xlab("Latitude") +
  ylab("Peak Area")


##Make stacked bar chart (fill) :
ggplot(sf.dat, aes(x = as.factor(Lat), y = Mean.nM, fill = Size_Fraction)) +
  geom_col(width = 0.5, position = "fill") +
  facet_wrap(.~Compound, scales = "free") +
  xlab("Latitude") +
  ylab("Peak Area")


##Make bar chart of each fraction:
ggplot(sf.dat, aes(x = as.factor(Size_Fraction), y = Mean.nM, fill = reorder(compound.name.figure, order))) +
  geom_col(width = 0.5, color = "black", linewidth = 0.05) +
  facet_wrap(.~as.factor(Lat),  scales = "free") +
  scale_fill_manual(values = compound.pal.fig)

##Make bar chart of each fraction:
ggplot(sf.dat, aes(x = as.factor(Size_Fraction), y = Mean.nM, fill = reorder(compound.name.figure, order))) +
  geom_col(width = 0.5, color = "black", linewidth = 0.05, position = "fill") +
  facet_wrap(.~as.factor(Lat)) +
  scale_fill_manual(values = compound.pal.fig)







##
metab.clust.dat <- sf.dat %>%
  select(Compound, Lat, Small_Frac_Perc) %>%
  unique() %>%
  pivot_wider(id_cols = Compound, names_from = Lat, values_from = Small_Frac_Perc) %>%
  column_to_rownames(var = "Compound")

metab.dist <- vegdist(metab.clust.dat, method = "euclidean")

clust.out <- hclust(metab.dist, method = "average")
dend <- as.dendrogram(clust.out)
dend.dat <- dendro_data(dend)
dend.order <- dend.dat$labels %>%
  rename("order" = x, 
         "Compound" = label) %>%
  select(Compound, order)


###
metab.heatmap.dat <- sf.dat %>%
  select(Compound, Lat, Small_Frac_Perc) %>%
  left_join(dend.order) %>%
  left_join(., compound.order %>%
              select(-order)) %>%
  unique()

#heatmap showing % of signal in small size fraction 
g2sf.hm <- ggplot(metab.heatmap.dat, aes(x = as.factor(Lat), y = reorder(compound.name.figure, -order), fill = Small_Frac_Perc)) +
  geom_tile(color = "black") +
  scale_fill_viridis(option = "G", begin = 0) +
 # facet_wrap(.~Class, scales = "free") +
  xlab("Latitude") +
  ylab("Compound") +
  coord_fixed() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
g2sf.hm


ggsave(g2sf.hm, file = "R_Code/Code_Development_Workspace/G2SF_Heatmap.pdf", 
       height = 5, width = 4, scale = 1.4)



ggplot(metab.heatmap.dat, aes(x = as.factor(Lat), y = Small_Frac_Perc, group = Compound)) +
  geom_point() +
  geom_line() +
  facet_wrap(.~reorder(Compound, -order))


g2sf.sum <- metab.heatmap.dat %>%
  group_by(Compound) %>%
  reframe(max_sf = max(Small_Frac_Perc),
          min_sf = min(Small_Frac_Perc),
          diff = max_sf-min_sf)











































































