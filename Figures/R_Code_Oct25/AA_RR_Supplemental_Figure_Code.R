









library(tidyverse)
library(vegan)
library(viridis)
library(rstatix)
library(ggforce)
library(patchwork)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")



###define inputs
env.dat.file <- "Intermediates/Enviro_Osmo_Final_Dataset_with_metadata.csv"
peri.dat.file <- "Intermediates/PERIFIX_Osmo_Meta_Dat_101325.csv"





###define inputs
all.dat.file <- "Intermediates/Enviro_Osmo_Final_Dataset_with_metadata.csv"






#Read in data 
dat <- read_csv(all.dat.file) %>%
  left_join(., compound.order) %>%
  filter(!str_detect(Part.SampID, "Blk")) %>%
  filter(!Parent_ID == "TN397_S11_600_U_C") %>%
  filter(!is.na(compound.name.figure))

#Define regions:
dat.region <- dat %>%
  mutate(Region = case_when(Cruise == "KM1906" & Lat >= 35 ~ "NPTZ",
                            Cruise == "KM1906" & Lat < 35 ~ "NPSG",
                            Cruise == "TN397" & Long > -122 ~ "CUCP",
                            Cruise == "TN397" & Long < -122 & Lat > 10.5 ~ "NPSG",
                            Cruise == "TN397" & Lat < 10.5 & Lat > 4.5 ~ "NPEC",
                            Cruise == "TN397" & Lat < 4.5 ~ "PEDP",
                            Cruise == "RC078" ~ "SS"))






##? Need to remove values below LOD? Maybe only include ones where all compounds are measured?




#####__Make BRR Plots:

#organize particulate surface data:
p.AA.dat <- dat.region %>%
  filter(Cruise %in% c( "KM1906", "TN397", "RC078")) %>%
  select(Compound, compound.name.figure, order, class, Part.SampID, Cruise, Part.Conc.nM, Region, Part.detected) %>% 
  unique() %>%
  filter(class == "AA") %>%
  group_by(Part.SampID) %>%
  filter(Part.detected == "Yes") %>%
  mutate(AAs.detected = n()) %>%
  filter(AAs.detected == 12) %>%
  mutate(AA.Norm.Val = Part.Conc.nM/max(Part.Conc.nM),
         AA.Tot.Conc = sum(Part.Conc.nM),
         AA.Rank = rank(1/Part.Conc.nM),
         count = n()) %>%
  ungroup() %>%
  mutate(AA.Norm.Val.2 = Part.Conc.nM/AA.Tot.Conc) %>%
  group_by(Compound) %>%
  mutate(Mean.AA.Rank = mean(AA.Rank),
         SD.AA.Rank = sd(AA.Rank),
         Mean.AA.Val = mean(AA.Norm.Val),
         SD.AA.Val = sd(AA.Norm.Val),
         AA.Val.Max = Mean.AA.Val + SD.AA.Val,
         AA.Val.Min = case_when(Mean.AA.Val - SD.AA.Val > 0 ~ Mean.AA.Val - SD.AA.Val,
                                     Mean.AA.Val - SD.AA.Val < 0 ~ 0.01)) %>%
  ungroup() %>%
  mutate(AA.Redfield.Ratio = Mean.AA.Val/Mean.AA.Val[compound.name.figure == "Glutamic acid"]) %>%
  group_by(Part.SampID) %>%
  mutate(overall.rank = rank(Mean.AA.Rank))



#organize dissolved surface data:
d.AA.dat <- dat.region %>%
  select(Compound, compound.name.figure, class, Diss.SampID, Cruise, Diss.Conc.nM, order, Region, Diss.detected) %>% 
  unique() %>%
  filter(!is.na(Diss.SampID)) %>%
  filter(!is.na(Diss.Conc.nM)) %>%
  filter(class == "AA") %>%
  group_by(Diss.SampID) %>%
  filter(Diss.detected == "Yes") %>%
  mutate(AAs.detected = n()) %>%
  filter(AAs.detected == 12) %>%
  mutate(AA.Norm.Val = Diss.Conc.nM/max(Diss.Conc.nM),
         AA.Tot.Conc = sum(Diss.Conc.nM),
         AA.Rank = rank(1/Diss.Conc.nM),
         count = n()) %>%
  ungroup() %>%
  mutate(AA.Norm.Val.2 = Diss.Conc.nM/AA.Tot.Conc) %>%
  group_by(Compound) %>%
  mutate(Mean.AA.Rank = mean(AA.Rank),
         SD.AA.Rank = sd(AA.Rank),
         Mean.AA.Val = mean(AA.Norm.Val),
         SD.AA.Val = sd(AA.Norm.Val),
         AA.Val.Max = Mean.AA.Val + SD.AA.Val,
         AA.Val.Min = case_when(Mean.AA.Val - SD.AA.Val > 0 ~ Mean.AA.Val - SD.AA.Val,
                                     Mean.AA.Val - SD.AA.Val < 0 ~ 0.01)) %>%
  ungroup() %>%
  mutate(AA.Redfield.Ratio = Mean.AA.Val/Mean.AA.Val[compound.name.figure == "Glutamic acid"]) %>%
  group_by(Diss.SampID) %>%
  mutate(overall.rank = rank(Mean.AA.Rank))




#sumarize BRR data:
##Redfield.sum 
p.red <- p.AA.dat %>%
  select(compound.name.figure, AA.Redfield.Ratio, Mean.AA.Rank, SD.AA.Rank) %>%
  unique() %>%
  rename(P.Ratio = AA.Redfield.Ratio,
         P.Rank = Mean.AA.Rank,
         P.Rank.SD = SD.AA.Rank)

d.red <- d.AA.dat %>%
  select(compound.name.figure, AA.Redfield.Ratio, Mean.AA.Rank, SD.AA.Rank) %>%
  unique() %>%
  rename(D.Ratio = AA.Redfield.Ratio,
         D.Rank = Mean.AA.Rank,
         D.Rank.SD = SD.AA.Rank)


####Get compound properties
mw.dat <- tibble(
  compound.name.figure = c("Glutamic acid", "Aspartic acid", "Alanine", "Threonine", "beta-Alanine", "Proline", "(Iso)leucine", "Hydroxyisoleucine",
                           "Sarcosine", "Glutamine", "Asparagine", "beta-Glutamic acid"),
  mw = c(147.13, 133.10, 119.12, 89.09, 89.09, 115.13, 131.17, 147.17, 89.09, 146.14, 132.12, 147.13))

# comp.info <- dat %>%
#   select(Compound, compound.name.figure, class) %>%
#   unique() %>%
#   filter(class == "Betaine") %>%
#   left_join(., mw.dat) %>%
#   select(compound.name.figure, mw)

###
red.comb <- left_join(p.red, d.red) %>%
  left_join(., mw.dat) %>%
  left_join(., compound.order)


###Run quick linear models 
lm.p.mw <- lm(mw~P.Rank, data = red.comb)
summary(lm.p.mw)

lm.d.mw <- lm(mw~D.Rank, data = red.comb)
summary(lm.d.mw)






#Make Plots:


#########Maek Relative Concentration Plot:

#particulate
AA.p.rel.conc.plot <- ggplot(p.AA.dat, aes(x = AA.Norm.Val, y = reorder(overall.rank, -overall.rank))) +
  geom_jitter(height = 0.12, width = 0, alpha = 0.6, shape = 21, color = "black", 
              size = 2, stroke = 0.1, aes(fill = reorder(compound.name.figure, order))) +
  labs(fill = "Comopund") +
  geom_boxplot(alpha = 0.7, width = 0.3) +
  #  geom_point(aes(x = Mean.Betaine.Val, y = reorder(compound.name.figure, -Mean.Betaine.Rank)), color = "black", size = 2, shape = 21, fill = "white") +
  scale_fill_manual(values = compound.pal.fig) +
  scale_x_continuous(trans = trans_reverser('log10'), breaks = c(1, 0.5, 0.25, 0.1, 0.05, 0.01, 0.001), limits = c(1.5, 0.001)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
      #  axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  xlab("Relative Abundance") +
  ylab("Overall Rank") +
  ggtitle("Particulate")
AA.p.rel.conc.plot

#diss
AA.d.rel.conc.plot <- ggplot(d.AA.dat, aes(x = AA.Norm.Val, reorder(overall.rank, -overall.rank))) +
  geom_jitter(height = 0.12, width = 0, alpha = 0.6, shape = 21, color = "black", 
              size = 2, stroke = 0.1, aes(fill = reorder(compound.name.figure, order))) +
  labs(fill = "Comopund") +
  geom_boxplot(alpha = 0.7, width = 0.3) +
  #  geom_point(aes(x = Mean.Betaine.Val, y = reorder(compound.name.figure, -Mean.Betaine.Rank)), color = "black", size = 2, shape = 21, fill = "white") +
  scale_fill_manual(values = compound.pal.fig) +
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
AA.d.rel.conc.plot


## Make rank vs. mz plots:
#Particulate
AA.mz.p.plot <- ggplot(red.comb, aes(x = P.Rank, y = mw)) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgray") +
  geom_point(size = 3, shape = 21, stroke = .15, aes(fill = reorder(compound.name.figure, order))) +
  theme_bw() +
  scale_fill_manual(values = compound.pal.fig) +
  labs(fill = "Comopund") +
  xlab("Mean Rank in Particulate") +
  annotate("text", x = 2.5, y = 100, 
           label = expression(atop(R^2 == 0.21, italic(p) == 0.075)),
           hjust = 0)

AA.mz.p.plot

#Dissolved
AA.mz.d.plot <- ggplot(red.comb, aes(x = D.Rank, y = mw)) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgray") +
  geom_point(size = 3, shape = 21, stroke = .15, aes(fill = reorder(compound.name.figure, order))) +
  theme_bw() +
  scale_fill_manual(values = compound.pal.fig) +
  labs(fill = "Comopund") +
  xlab("Mean Rank in Dissolved") +
  annotate("text", x = 8, y = 105, 
           label = expression(atop(R^2 < 0.01, italic(p) == 0.57)),
           hjust = 0)+
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank())
AA.mz.d.plot


AA.RR.plot <- AA.p.rel.conc.plot + AA.d.rel.conc.plot + 
  AA.mz.p.plot + AA.mz.d.plot + plot_layout(ncol = 2, nrow = 2, heights = c(2,1.2), guides = "collect") +
  plot_annotation(tag_levels = "a")

AA.RR.plot

ggsave(AA.RR.plot, filename = "Figures/Output_Oct25/AA_RR_Supplemental_Figure.png", dpi = 900,
       height = 5.25, width = 6, scale = 1.4)

