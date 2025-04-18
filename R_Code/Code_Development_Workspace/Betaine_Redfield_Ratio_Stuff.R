
library(tidyverse)
library(ggforce)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")


##Define inputs:
diss.file <- "Intermediates/Dissolved_Quantified_Data.csv"
part.file <- "Intermediates/Particulate_Quant_Output.csv"
meta.file <- "Intermediates/All_metadata_information.csv"
match.file <- "Intermediates/Particulate_Dissolved_Matching_Key.csv"
stds.file <- "Meta_Data/Ingalls_Lab_Standards_03172023.csv" 




###load in data and combine with metadata
diss.dat <- read_csv(diss.file)
part.dat <- read_csv(part.file)
meta.dat <- read_csv(meta.file)
match.key <- read_csv(match.file)




#organize particulate surface data:
p.dat <- left_join(part.dat, meta.dat) %>%
  rename("Compound" = Name) %>%
  filter(Compound %in% compound.order$Compound) %>%
  left_join(., compound.order) %>%
  # filter(!station == 4) %>%
  filter(!SampID == "220902_Smp_TN397_S11_600_U_C") %>%
  unique() %>%
  # filter(Cruise %in% c( "KM1906", "TN397", "RC078")) %>%
  filter(!Compound == "Threonine Betaine (tentative)") %>%
  filter(!Compound == "Homoserine Betaine (tentative)") %>%
  filter(depth_m < 16) %>%
  mutate(Region = case_when(Cruise == "RC078" ~ "PS",
                            Lat > 36 ~ "NPTZ",
                            Lat < 7 ~ "Equator",
                            Cruise == "TN397" & Lat > 29 ~ "CC",
                            TRUE ~ "NPSG")) %>%
  filter(!station %in% c(1, 8)) %>%
  select(Compound, SampID, Cruise, nM.in.smp, Lat, Long, station, depth_m, Treatment, order, class, compound.name.figure, Region) 
  # rename(sampID.part = SampID,
  #        nM.in.smp.part = nM.in.smp) %>%
  # full_join(., match.key)



#organize dissolved surface data:
d.dat <- diss.dat %>%
  filter(!str_detect(SampID, "Blk"),
         !str_detect(SampID, "blk"),
         !str_detect(SampID, "Poo"),
         !str_detect(SampID, "Mix"),
         !str_detect(SampID, "UKH"),
         !str_detect(SampID, "UKG")) %>%
  filter(!Cruise == "KM1906") %>%
  select(Name, SampID, Nmol.in.smp) %>%
  rename(Compound = Name,
         sampID.diss = SampID,
         nM.in.smp.diss = Nmol.in.smp) %>%
  full_join(., match.key) 



###Combine datasets:
combine.dat <- full_join(d.dat, p.dat) 




###Particulate betaines redfield ratios 
part.betaine.dat <- p.dat %>%
  filter(class == "Betaine") %>%
  filter(Cruise %in% c("KM1906", "TN397", "RC078")) %>%
  left_join(., meta.dat) %>%
  unique() %>%
  group_by(SampID) %>%
  mutate(Betaine.Norm.Val = nM.in.smp/max(nM.in.smp),
         Betaine.Tot.Conc = sum(nM.in.smp),
         Betaine.Rank = rank(1/nM.in.smp),
         count = n()) %>%
  ungroup() %>%
  mutate(Betaine.Norm.Val.2 = nM.in.smp/Betaine.Tot.Conc) %>%
  group_by(Compound) %>%
  mutate(Mean.Betaine.Rank = mean(Betaine.Rank),
         SD.Betaine.Rank = sd(Betaine.Rank),
         Mean.Betaine.Val = mean(Betaine.Norm.Val),
         SD.Betaine.Val = sd(Betaine.Norm.Val),
         Betaine.Val.Max = Mean.Betaine.Val + SD.Betaine.Val,
         Betaine.Val.Min = case_when(Mean.Betaine.Val - SD.Betaine.Val > 0 ~ Mean.Betaine.Val - SD.Betaine.Val,
                                     Mean.Betaine.Val - SD.Betaine.Val < 0 ~ 0.01)) %>%
  ungroup() %>%
  mutate(Betaine.Redfield.Ratio = Mean.Betaine.Val/Mean.Betaine.Val[Compound == "Proline betaine"])






#Concentration Plot
rel.conc.plot <- ggplot(part.betaine.dat, aes(x = Betaine.Norm.Val, y = reorder(compound.name.figure, -Mean.Betaine.Rank))) +
  geom_jitter(height = 0.12, width = 0, alpha = 0.4, shape = 21, stroke = 1, aes(color = Region)) +
  # geom_errorbarh(aes(xmin = Betaine.Val.Min, 
  #                    xmax = Betaine.Val.Max, 
  #                    y = reorder(compound.name.figure, -Mean.Betaine.Rank)), 
  #                color = "black", height = 0, size = 0.3) +
  geom_boxplot(alpha = 0.3, width = 0.2) +
#  geom_point(aes(x = Mean.Betaine.Val, y = reorder(compound.name.figure, -Mean.Betaine.Rank)), color = "black", size = 2, shape = 21, fill = "white") +
  scale_color_manual(values = region.palette) +
  scale_x_continuous(trans = trans_reverser('log10'), breaks = c(1, 0.5, 0.25, 0.1, 0.05, 0.01, 0.001), limits = c(1.5, 0.001)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  xlab("Relative Abundance") +
  ylab("Compound") +
  ggtitle("Particulate")
rel.conc.plot



##Get just meta-data from particulate betaine dat:
betaine.meta.dat <- part.betaine.dat %>%
  mutate(sampID.part = SampID) %>%
  left_join(., match.key) %>%
  select(sampID.diss, Region, Lat, Long) %>%
  rename(SampID = sampID.diss) %>%
  filter(!is.na(SampID)) %>%
  unique()
  





###Dissolved betaines redfield ratios 
diss.betaine.dat <- diss.dat %>%
  filter(!str_detect(SampID, "Blk"),
         !str_detect(SampID, "blk"),
         !str_detect(SampID, "Poo"),
         !str_detect(SampID, "Mix"),
         !str_detect(SampID, "UKH"),
         !str_detect(SampID, "GBT"),
         !str_detect(SampID, "UKG"),
         !str_detect(SampID, "UnfiltMQ1"),
         !str_detect(SampID, "TN397_S11_600_U_C"),
         ) %>%
  rename(Compound = Name,
         nM.in.smp = Nmol.in.smp) %>%
  left_join(., compound.order) %>% 
  filter(class == "Betaine") %>%
  filter(Cruise %in% c("TN397", "RC078")) %>%
  left_join(., betaine.meta.dat) %>%
  unique() %>%
  group_by(SampID) %>%
  mutate(Betaine.Norm.Val = nM.in.smp/max(nM.in.smp),
         Betaine.Tot.Conc = sum(nM.in.smp),
         Betaine.Rank = rank(1/nM.in.smp),
         count = n()) %>%
  ungroup() %>%
  mutate(Betaine.Norm.Val.2 = nM.in.smp/Betaine.Tot.Conc) %>%
  group_by(Compound) %>%
  mutate(Mean.Betaine.Rank = mean(Betaine.Rank),
         SD.Betaine.Rank = sd(Betaine.Rank),
         Mean.Betaine.Val = mean(Betaine.Norm.Val),
         SD.Betaine.Val = sd(Betaine.Norm.Val),
         Betaine.Val.Max = Mean.Betaine.Val + SD.Betaine.Val,
         Betaine.Val.Min = case_when(Mean.Betaine.Val - SD.Betaine.Val > 0 ~ Mean.Betaine.Val - SD.Betaine.Val,
                                     Mean.Betaine.Val - SD.Betaine.Val < 0 ~ 0.01)) %>%
  ungroup() %>%
  unique() %>%
  left_join(., meta.dat) %>%
  mutate(Betaine.Redfield.Ratio = Mean.Betaine.Val/Mean.Betaine.Val[Compound == "Proline betaine"]) 






#Concentration Plot
d.rel.conc.plot <- ggplot(diss.betaine.dat, aes(x = Betaine.Norm.Val, y = reorder(compound.name.figure, -Mean.Betaine.Rank))) +
  geom_jitter(height = 0.12, width = 0, alpha = 0.4, shape = 21, stroke = 1, aes(color = Region)) +
  # geom_errorbarh(aes(xmin = Betaine.Val.Min, 
  #                    xmax = Betaine.Val.Max, 
  #                    y = reorder(compound.name.figure, -Mean.Betaine.Rank)), 
  #                color = "black", height = 0, size = 0.3) +
  geom_boxplot(alpha = 0.3, width = 0.2) +
  #  geom_point(aes(x = Mean.Betaine.Val, y = reorder(compound.name.figure, -Mean.Betaine.Rank)), color = "black", size = 2, shape = 21, fill = "white") +
  scale_color_manual(values = region.palette) +
  scale_x_continuous(trans = trans_reverser('log10'), breaks = c(1, 0.5, 0.25, 0.1, 0.05, 0.01, 0.001), limits = c(1.5, 0.001)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  xlab("Relative Abundance") +
  ylab("Compound") +
  ggtitle("Dissolved")
d.rel.conc.plot











##Redfield.sum 
p.red <- part.betaine.dat %>%
  select(compound.name.figure, Betaine.Redfield.Ratio, Mean.Betaine.Rank) %>%
  unique() %>%
  rename(P.Ratio = Betaine.Redfield.Ratio,
         P.Rank = Mean.Betaine.Rank)

d.red <- diss.betaine.dat %>%
  select(compound.name.figure, Betaine.Redfield.Ratio, Mean.Betaine.Rank) %>%
  unique() %>%
  rename(D.Ratio = Betaine.Redfield.Ratio,
         D.Rank = Mean.Betaine.Rank)




#Get compound properties
std.formula <- read_csv(stds.file) %>%
  select(Compound_Name, mz) %>%
  rename("Compound" = Compound_Name) %>%
  unique() 

comp.info <- diss.dat %>%
  select(Name, Empirical_Formula, C, N) %>%
  unique() %>%
  rename(Compound = Name) %>%
  left_join(., compound.order) %>%
  filter(class == "Betaine") %>%
  left_join(., std.formula) %>%
  select(compound.name.figure, Empirical_Formula, C, mz)





###
red.comb <- left_join(p.red, d.red) %>%
  left_join(comp.info)

mz.p.plot <- ggplot(red.comb, aes(x = P.Rank, y = mz)) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgray") +
  geom_point(size = 2) +
  theme_bw() +
  xlab("Mean Rank in Particulate")
mz.p.plot


mz.d.plot <- ggplot(red.comb, aes(x = D.Rank, y = mz)) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgray") +
  geom_point(size = 2) +
  theme_bw() +
  xlab("Mean Rank in Dissolved") +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank())
mz.d.plot







#Combine plots:
library(patchwork)
comb.plot <- rel.conc.plot + d.rel.conc.plot + mz.p.plot + mz.d.plot +
  plot_layout(guides = "collect", heights = c(0.65, 0.35)) + 
  plot_annotation(tag_levels = 'A')
comb.plot

ggsave(comb.plot, file = "R_Code/Code_Development_Workspace/Betaine_Redfield_Ratio.pdf",
       height = 5, width = 6, scale = 1.5)


