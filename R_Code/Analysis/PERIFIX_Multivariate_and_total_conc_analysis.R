





library(tidyverse)
library(rstatix)
library(vegan)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")


peri.dat.file <- "Intermediates/PERIFIX_Osmo_Meta_Dat_101325.csv"

#Load in PERIFIX Data:
peri.dat <- read_csv(peri.dat.file)%>%
  left_join(., compound.order) %>%
  filter(!str_detect(Part.SampID, "Blk")) %>%
  filter(!is.na(compound.name.figure))



#_______calculate impact of each nutrient on total concentration:_____________

#calculate total concentrations
peri.tot.conc <- peri.dat %>%
  filter(Part.detected == "Yes") %>%
  group_by(Part.SampID, Treatment, Replicate, N, P, Fe) %>%
  reframe(tot.conc.nM = sum(Part.Conc.nM)) %>%
  filter(!Treatment == "Tote") %>%
  mutate(N = as.factor(N),
         P = as.factor(P),
         Fe = as.factor(Fe)) %>%
  mutate(tot.conc.nM.log10 = log10(tot.conc.nM))

#Summarize total concentrations
peri.tot.conc.sum.treatment <- peri.tot.conc %>%
  group_by(Treatment) %>%
  reframe(mean.tot.conc.nM = mean(tot.conc.nM),
          sd.tot.conc.nM = sd(tot.conc.nM))

peri.tot.conc.sum.Nstatus <- peri.tot.conc %>%
  group_by(N) %>%
  reframe(mean.tot.conc.nM = mean(tot.conc.nM),
          sd.tot.conc.nM = sd(tot.conc.nM),
          min.tot.conc.nM = min(tot.conc.nM),
          max.tot.conc.nM = max(tot.conc.nM))




#visualize total concentrations
ggplot(peri.tot.conc, aes(x = Treatment, y = tot.conc.nM)) +
  geom_boxplot() +
  geom_point() +
  scale_y_log10()

#Run ANOVA:
peri.anova <- aov(tot.conc.nM.log10 ~ N * P * Fe, dat = peri.tot.conc)
summary(peri.anova)




#____Multivariate Analysis

#Organize Data set:
peri.matrix <- peri.dat %>%
  mutate(Part.Conc.nM = case_when(Part.detected == "No" ~ Part.Impute.Conc.nM,
                                  TRUE ~ Part.Conc.nM)) %>%
  filter(!Treatment == "Tote") %>%
  rename(SampID = Part.SampID) %>%
  select(SampID, compound.name.figure, Part.Conc.nM) %>%
  pivot_wider(id_cols = SampID, names_from = compound.name.figure, values_from = Part.Conc.nM) %>%
  column_to_rownames(var = "SampID")

##metadata matrix
peri.meta <- peri.dat %>%
  rename(SampID = Part.SampID) %>%
  filter(!Treatment == "Tote") %>%
  mutate(N = as.factor(N),
         P = as.factor(P),
         Fe = as.factor(Fe)) %>%
  select(SampID, N, P, Fe) %>%
  unique()



#___Rank Order

#Apply transformation and calculate dissimilarity matrix
peri.matrix.rank <- decostand(peri.matrix, method = "rank", MARGIN = 1)

#Run PERMANOVA
rank.permanova <- adonis2(peri.matrix.rank ~ N * P * Fe, data = peri.meta, method = "euclidean")
rank.permanova


#___Relative Abundance

#Apply transformation and calculate dissimilarity matrix
peri.matrix.relabun <- decostand(peri.matrix, method = "range", MARGIN = 1)

#Run PERMANOVA
relabun.permanova <- adonis2(peri.matrix.relabun ~ N * P * Fe, data = peri.meta, method = "euclidean")
relabun.permanova







###Other:

# peri.community.comp <- peri.dat %>%
#   pivot_longer(cols = Chlorophyte:Metazoa, names_to = "Tax_Group", values_to = "Rel_Abun") %>%
#   select(Parent_ID, Treatment, N, P, Fe, Tax_Group, Rel_Abun) %>%
#   unique() %>%
#   group_by(Treatment, Tax_Group) %>%
#   mutate(Ave_Rel_Abun = mean(Rel_Abun, na.rm = TRUE))
# 
# ggplot(peri.community.comp, aes(x = Parent_ID, y = Rel_Abun, fill = Tax_Group)) +
#   geom_col(position = "fill")
# 
# ggplot(peri.community.comp, aes(x = Treatment, y = Ave_Rel_Abun, fill = Tax_Group)) +
#   geom_col(position = "fill")























































































