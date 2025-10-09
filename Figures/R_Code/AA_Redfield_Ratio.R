


library(tidyverse)
library(ggforce)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")




###define inputs
all.dat.file <- "Intermediates/Final_Osmo_Meta_Env_Dataframe.csv"
stds.file <- "Meta_Data/Ingalls_Lab_Standards_03172023.csv" 


#Read in data
dat <- read_csv(all.dat.file) %>%
  filter(!Part.SampID == "220902_Smp_TN397_S11_600_U_C") %>%
  filter(!str_detect(Part.SampID, "Smp_S4_C1"),
         !str_detect(Part.SampID, "Smp_S4_C1"))




#Particulate Betaine Data:

#organize particulate surface data:
p.betaine.dat <- dat %>%
  filter(Cruise %in% c( "KM1906", "TN397", "RC078")) %>%
  select(Compound, Part.SampID, Cruise, Part.Conc.nM, Lat, Long, station, depth_m, order, class, compound.name.figure, Region) %>% 
  unique() %>%
  filter(depth_m < 10) %>%
  filter(!station %in% c(1, 8)) %>%
  filter(class == "AA") %>%
  group_by(Part.SampID) %>%
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
  ungroup()# %>%
  mutate(Betaine.Redfield.Ratio = Mean.Betaine.Val/Mean.Betaine.Val[Compound == "Proline betaine"])

#organize dissolved surface data:
d.betaine.dat <- dat %>%
  filter(Cruise %in% c("TN397", "RC078")) %>%
  select(Compound, Diss.SampID, Cruise, Diss.Conc.nM.adj, Lat, Long, station, depth_m, order, class, compound.name.figure, Region) %>% 
  unique() %>%
  filter(!is.na(Diss.SampID)) %>%
  filter(!is.na(Diss.Conc.nM.adj)) %>%
  filter(depth_m < 10) %>%
  filter(!station %in% c(1, 8)) %>%
  filter(class == "AA") %>%
  group_by(Diss.SampID) %>%
  mutate(Betaine.Norm.Val = Diss.Conc.nM.adj/max(Diss.Conc.nM.adj),
         Betaine.Tot.Conc = sum(Diss.Conc.nM.adj),
         Betaine.Rank = rank(1/Diss.Conc.nM.adj),
         count = n()) %>%
  ungroup() %>%
  mutate(Betaine.Norm.Val.2 = Diss.Conc.nM.adj/Betaine.Tot.Conc) %>%
  group_by(Compound) %>%
  mutate(Mean.Betaine.Rank = mean(Betaine.Rank),
         SD.Betaine.Rank = sd(Betaine.Rank),
         Mean.Betaine.Val = mean(Betaine.Norm.Val),
         SD.Betaine.Val = sd(Betaine.Norm.Val),
         Betaine.Val.Max = Mean.Betaine.Val + SD.Betaine.Val,
         Betaine.Val.Min = case_when(Mean.Betaine.Val - SD.Betaine.Val > 0 ~ Mean.Betaine.Val - SD.Betaine.Val,
                                     Mean.Betaine.Val - SD.Betaine.Val < 0 ~ 0.01)) %>%
  ungroup()# %>%
 # mutate(Betaine.Redfield.Ratio = Mean.Betaine.Val/Mean.Betaine.Val[Compound == "Proline betaine"])


#organize total surface data:
t.betaine.dat <- dat %>%
  filter(Cruise %in% c("TN397", "RC078")) %>%
  filter(!is.na(Part.SampID)) %>%
  filter(!is.na(Diss.SampID)) %>%
  select(Compound, Part.SampID, Cruise, Total.Conc.nM, Lat, Long, station, depth_m, order, class, compound.name.figure, Region) %>% 
  unique() %>%
  filter(depth_m < 10) %>%
  filter(!station %in% c(1, 8)) %>%
  filter(class == "Betaine") %>%
  group_by(Part.SampID) %>%
  mutate(Betaine.Norm.Val = Total.Conc.nM/max(Total.Conc.nM),
         Betaine.Tot.Conc = sum(Total.Conc.nM),
         Betaine.Rank = rank(1/Total.Conc.nM),
         count = n()) %>%
  ungroup() %>%
  mutate(Betaine.Norm.Val.2 = Total.Conc.nM/Betaine.Tot.Conc) %>%
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
t.betaine.dat







#########Maek Relative Concentration Plot:

#particulate
p.rel.conc.plot <- ggplot(p.betaine.dat, aes(x = Betaine.Norm.Val, y = reorder(compound.name.figure, -Mean.Betaine.Rank))) +
  geom_jitter(height = 0.12, width = 0, alpha = 0.6, shape = 21, color = "black", 
              size = 2, stroke = 0.1, aes(fill = reorder(compound.name.figure, order))) +
  labs(fill = "Comopund") +
  geom_boxplot(alpha = 0.7, width = 0.3) +
  #  geom_point(aes(x = Mean.Betaine.Val, y = reorder(compound.name.figure, -Mean.Betaine.Rank)), color = "black", size = 2, shape = 21, fill = "white") +
  scale_fill_manual(values = compound.pal.fig) +
 # scale_x_continuous(trans = trans_reverser('log10'), breaks = c(1, 0.5, 0.25, 0.1, 0.05, 0.01, 0.001), limits = c(1.5, 0.001)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  xlab("Relative Abundance") +
  ylab("Compound") +
  ggtitle("Particulate")
p.rel.conc.plot

#diss
d.rel.conc.plot <- ggplot(d.betaine.dat, aes(x = Betaine.Norm.Val, y = reorder(compound.name.figure, -Mean.Betaine.Rank))) +
  geom_jitter(height = 0.12, width = 0, alpha = 0.6, shape = 21, color = "black", 
              size = 2, stroke = 0.1, aes(fill = reorder(compound.name.figure, order))) +
  labs(fill = "Comopund") +
  geom_boxplot(alpha = 0.7, width = 0.3) +
  #  geom_point(aes(x = Mean.Betaine.Val, y = reorder(compound.name.figure, -Mean.Betaine.Rank)), color = "black", size = 2, shape = 21, fill = "white") +
  scale_fill_manual(values = compound.pal.fig) +
 # scale_x_continuous(trans = trans_reverser('log10'), breaks = c(1, 0.5, 0.25, 0.1, 0.05, 0.01, 0.001), limits = c(1.5, 0.001)) +
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


#total
t.rel.conc.plot <- ggplot(t.betaine.dat, aes(x = Betaine.Norm.Val, y = reorder(compound.name.figure, -Mean.Betaine.Rank))) +
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
  ggtitle("Total")
t.rel.conc.plot
