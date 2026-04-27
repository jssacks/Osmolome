









library(tidyverse)
library(vegan)
library(viridis)
library(rstatix)
library(ggforce)
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











#####__Make BRR Plots:

#organize particulate surface data:
p.betaine.dat <- dat.region %>%
  filter(Cruise %in% c( "KM1906", "TN397", "RC078")) %>%
  select(Compound, compound.name.figure, order, class, Part.SampID, Cruise, Part.Conc.nM, Region, Part.detected) %>% 
  unique() %>%
  filter(class == "Betaine") %>%
  group_by(Part.SampID) %>%
  filter(Part.detected == "Yes") %>%
  mutate(Betaines.detected = n()) %>%
  filter(Betaines.detected == 8) %>%
  mutate(Betaine.Norm.Val = Part.Conc.nM/max(Part.Conc.nM),
         Betaine.Tot.Conc = sum(Part.Conc.nM),
         Betaine.Rank = rank(1/Part.Conc.nM),
         count = n()) %>%
  ungroup() %>%
  mutate(Betaine.Norm.Val.2 = Part.Conc.nM/Betaine.Tot.Conc) %>%
  group_by(Compound) %>%
  mutate(Mean.Betaine.Rank = mean(Betaine.Rank),
         SD.Betaine.Rank = sd(Betaine.Rank),
         Mean.Betaine.Val = mean(Betaine.Norm.Val),
         SD.Betaine.Val = sd(Betaine.Norm.Val),
         Betaine.Val.Max = Mean.Betaine.Val + SD.Betaine.Val,
         Betaine.Val.Min = case_when(Mean.Betaine.Val - SD.Betaine.Val > 0 ~ Mean.Betaine.Val - SD.Betaine.Val,
                                     Mean.Betaine.Val - SD.Betaine.Val < 0 ~ 0.01)) %>%
  ungroup() %>%
  mutate(Betaine.Redfield.Ratio = Mean.Betaine.Val/Mean.Betaine.Val[Compound == "Glycine betaine"]) %>%
  group_by(Part.SampID) %>%
  mutate(overall.rank = rank(Mean.Betaine.Rank))



#organize dissolved surface data:
d.betaine.dat <- dat.region %>%
  filter(Cruise %in% c("TN397", "RC078")) %>%
  select(Compound, compound.name.figure, class, Diss.SampID, Cruise, Diss.Conc.nM, order, Region, Diss.detected) %>% 
  unique() %>%
  filter(!is.na(Diss.SampID)) %>%
  filter(!is.na(Diss.Conc.nM)) %>%
  filter(class == "Betaine") %>%
  group_by(Diss.SampID) %>%
  filter(Diss.detected == "Yes") %>%
  mutate(AAs.detected = n()) %>%
  filter(AAs.detected == 8) %>%
  mutate(Betaine.Norm.Val = Diss.Conc.nM/max(Diss.Conc.nM),
         Betaine.Tot.Conc = sum(Diss.Conc.nM),
         Betaine.Rank = rank(1/Diss.Conc.nM),
         count = n()) %>%
  ungroup() %>%
  mutate(Betaine.Norm.Val.2 = Diss.Conc.nM/Betaine.Tot.Conc) %>%
  group_by(Compound) %>%
  mutate(Mean.Betaine.Rank = mean(Betaine.Rank),
         SD.Betaine.Rank = sd(Betaine.Rank),
         Mean.Betaine.Val = mean(Betaine.Norm.Val),
         SD.Betaine.Val = sd(Betaine.Norm.Val),
         Betaine.Val.Max = Mean.Betaine.Val + SD.Betaine.Val,
         Betaine.Val.Min = case_when(Mean.Betaine.Val - SD.Betaine.Val > 0 ~ Mean.Betaine.Val - SD.Betaine.Val,
                                     Mean.Betaine.Val - SD.Betaine.Val < 0 ~ 0.01)) %>%
  ungroup() %>%
  mutate(Betaine.Redfield.Ratio = Mean.Betaine.Val/Mean.Betaine.Val[Compound == "Glycine betaine"])  %>%
  group_by(Diss.SampID) %>%
  mutate(overall.rank = rank(Mean.Betaine.Rank))



#sumarize BRR data:
##Redfield.sum 
p.red <- p.betaine.dat %>%
  select(compound.name.figure, Betaine.Redfield.Ratio, Mean.Betaine.Rank, SD.Betaine.Rank) %>%
  unique() %>%
  rename(P.Ratio = Betaine.Redfield.Ratio,
         P.Rank = Mean.Betaine.Rank,
         P.Rank.SD = SD.Betaine.Rank)

d.red <- d.betaine.dat %>%
  select(compound.name.figure, Betaine.Redfield.Ratio, Mean.Betaine.Rank, SD.Betaine.Rank) %>%
  unique() %>%
  rename(D.Ratio = Betaine.Redfield.Ratio,
         D.Rank = Mean.Betaine.Rank,
         D.Rank.SD = SD.Betaine.Rank)


####Get compound properties
mw.dat <- tibble(
  compound.name.figure = c("GBT", "beta-Alanine betaine", "Homarine", "Trigonelline", "Carnitine", "Proline betaine", "Betonicine", "TMAB"),
  mw = c(117.15, 131.17, 137.14, 137.14, 161.20, 143.18, 159.18, 145.20))

# 
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
p.rel.conc.plot <- ggplot(p.betaine.dat, aes(x = Betaine.Norm.Val, y = reorder(overall.rank, -overall.rank))) +
                                             #  reorder(Mean.Betaine.Rank, -Mean.Betaine.Rank)
                                              # )) +
  geom_jitter(height = 0.12, width = 0, alpha = 0.6, shape = 21, color = "black", 
              size = 2, stroke = 0.1, aes(fill = reorder(compound.name.figure, order))) +
  labs(fill = "Comopund") +
  geom_boxplot(alpha = 0.7, width = 0.3, aes(group = overall.rank)) +
  #  geom_point(aes(x = Mean.Betaine.Val, y = reorder(compound.name.figure, -Mean.Betaine.Rank)), color = "black", size = 2, shape = 21, fill = "white") +
  scale_fill_manual(values = compound.pal.fig) +
  scale_x_continuous(trans = trans_reverser('log10'), breaks = c(1, 0.5, 0.25, 0.1, 0.05, 0.01, 0.001), limits = c(1.5, 0.001)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
     #   axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  ylab("Overall Rank") +
  xlab("Relative Abundance") +
  ggtitle("Particulate")
p.rel.conc.plot

#diss
d.rel.conc.plot <- ggplot(d.betaine.dat, aes(x = Betaine.Norm.Val, y = reorder(Mean.Betaine.Rank, -Mean.Betaine.Rank))) +
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
d.rel.conc.plot


## Make rank vs. mz plots:
#Particulate
mz.p.plot <- ggplot(red.comb, aes(x = P.Rank, y = mw)) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgray") +
  geom_point(size = 3, shape = 21, stroke = .15, aes(fill = reorder(compound.name.figure, order))) +
  theme_bw() +
  scale_fill_manual(values = compound.pal.fig) +
  labs(fill = "Comopund") +
  xlab("Mean Rank in Particulate") +
  annotate("text", x = 5, y = 130, 
           label = expression(atop(R^2 == 0.71, italic(p) == 0.006)),
           hjust = 0)

mz.p.plot

#Dissolved
mz.d.plot <- ggplot(red.comb, aes(x = D.Rank, y = mw)) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgray") +
  geom_point(size = 3, shape = 21, stroke = .15, aes(fill = reorder(compound.name.figure, order))) +
  theme_bw() +
  scale_fill_manual(values = compound.pal.fig) +
  labs(fill = "Comopund") +
  xlab("Mean Rank in Dissolved") +
  annotate("text", x = 5, y = 130, 
           label = expression(atop(R^2 == 0.73, italic(p) == 0.004)),
           hjust = 0)+
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank())
mz.d.plot


BRR.plot <- p.rel.conc.plot + d.rel.conc.plot + 
  mz.p.plot + mz.d.plot + plot_layout(ncol = 2, nrow = 2, heights = c(2,1.2), guides = "collect") +
  plot_annotation(tag_levels = "a")

BRR.plot

ggsave(BRR.plot, filename = "Figures/Output_Oct25/BRR_Figure.png", dpi = 900,
       height = 5, width = 6, scale = 1.4)













































