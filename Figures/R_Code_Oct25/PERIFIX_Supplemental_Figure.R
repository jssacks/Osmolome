library(tidyverse)
library(rstatix)
library(vegan)
library(patchwork)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")


peri.dat.file <- "Intermediates/PERIFIX_Osmo_Meta_Dat_101325.csv"

#Load in PERIFIX Data:
peri.dat <- read_csv(peri.dat.file)%>%
  left_join(., compound.order) %>%
  filter(!str_detect(Part.SampID, "Blk")) %>%
  filter(!is.na(compound.name.figure))


##Make Plot of total concentration:


#calculate total concentrations
peri.tot.conc <- peri.dat %>%
  filter(Part.detected == "Yes") %>%
  group_by(Part.SampID, Treatment, Replicate, N, P, Fe) %>%
  reframe(tot.conc.nM = sum(Part.Conc.nM)) %>%
  mutate(Treatment = as.factor(Treatment)) %>%
  mutate(Treatment = fct_relevel(Treatment, "Tote", "C", "F", "P", "PF", "N", "NF", "NP", "NPF")) %>%
#  filter(!Treatment == "Tote") %>%
  # mutate(N = as.factor(N),
  #        P = as.factor(P),
  #        Fe = as.factor(Fe)) %>%
  mutate(tot.conc.nM.log10 = log10(tot.conc.nM))



#visualize total concentrations
tot.conc.fig <- ggplot(peri.tot.conc, aes(x = Treatment, y = tot.conc.nM)) +
  geom_boxplot(width = 0.6, aes(color = Treatment)) +
  scale_color_manual(values = peri.palette) +
  geom_point() +
  theme_test() +
  xlab("Treatment") +
  ylab("Particulate Osmolyte Concentration (nM)") +
  theme(legend.position = "none")

tot.conc.fig




#____NMDS Plots

#Organize Data set:
peri.matrix <- peri.dat %>%
  mutate(Part.Conc.nM = case_when(Part.detected == "No" ~ Part.Impute.Conc.nM,
                                  TRUE ~ Part.Conc.nM)) %>%
 # filter(!Treatment == "Tote") %>%
  rename(SampID = Part.SampID) %>%
  select(SampID, compound.name.figure, Part.Conc.nM) %>%
  pivot_wider(id_cols = SampID, names_from = compound.name.figure, values_from = Part.Conc.nM) %>%
  column_to_rownames(var = "SampID")

##metadata matrix
peri.meta <- peri.dat %>%
  rename(SampID = Part.SampID) %>%
  select(SampID, Treatment, N, P, Fe) %>%
  unique() %>%
  mutate(Treatment = as.factor(Treatment)) %>%
  mutate(Treatment = fct_relevel(Treatment, "Tote", "C", "F", "P", "PF", "N", "NF", "NP", "NPF"))



#___generate matrices

#Apply transformation 
peri.matrix.rank <- decostand(peri.matrix, method = "rank", MARGIN = 1)
peri.matrix.relabun <- decostand(peri.matrix, method = "range", MARGIN = 1)

#Create distance matrix
nmds.dist.rank <- vegdist(x = peri.matrix.rank, method = "euclidean")
nmds.dist.relabun <- vegdist(x = peri.matrix.relabun, method = "euclidean")



#Run NMDS 
set.seed(123)

nmds.rank <- metaMDS(nmds.dist.rank, k=2, trymax = 100)
nmds.relabun <- metaMDS(nmds.dist.relabun, k=2, trymax = 50)


#Tidy NMDS output:
nmds.out.rank <- data.frame(nmds.rank$points) %>%
  rownames_to_column(var = "SampID") %>%
  left_join(., peri.meta) 

nmds.out.relabun <- data.frame(nmds.relabun$points) %>%
  rownames_to_column(var = "SampID") %>%
  left_join(., peri.meta) 



##Make NMDS Plots

#Rank:
nmds.rank.plot <- ggplot(nmds.out.rank, aes(x = MDS1, y = MDS2, fill = reorder(Treatment, N))) +
  geom_point(shape = 21, size = 3, stroke = 0.15) +
  scale_fill_manual(values = peri.palette) +
  theme_test() +
  annotate("text", x = -20, y = 18, label = "Stress = 0.16") +
  labs(fill = "Treatment")
nmds.rank.plot

#Relabun:
nmds.relabun.plot <- ggplot(nmds.out.relabun, aes(x = MDS1, y = MDS2, fill = reorder(Treatment, N))) +
  geom_point(shape = 21, size = 3, stroke = 0.15)+
  scale_fill_manual(values = peri.palette) +
  theme_test()  +
  annotate("text", x = -0.55, y = 0.75, label = "Stress = 0.12") +
  labs(fill = "Treatment")
nmds.relabun.plot


##Combine plots:
peri.supp.fig <- tot.conc.fig / (nmds.rank.plot + nmds.relabun.plot) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a")
peri.supp.fig

ggsave(peri.supp.fig, filename = "Figures/Output_Oct25/PERIFIX_Supplemental_Figure.png",
       height = 6, width = 7, dpi = 600, scale = 1.2)





















































