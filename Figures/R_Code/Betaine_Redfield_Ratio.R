





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
  filter(class == "Betaine") %>%
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
  ungroup() %>%
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
  filter(class == "Betaine") %>%
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
  ungroup() %>%
  mutate(Betaine.Redfield.Ratio = Mean.Betaine.Val/Mean.Betaine.Val[Compound == "Proline betaine"])


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
  geom_jitter(height = 0.12, width = 0, alpha = 0.6, shape = 21, color = "black", size = 2, stroke = 0.1, aes(fill = Region)) +
  # geom_errorbarh(aes(xmin = Betaine.Val.Min, 
  #                    xmax = Betaine.Val.Max, 
  #                    y = reorder(compound.name.figure, -Mean.Betaine.Rank)), 
  #                color = "black", height = 0, size = 0.3) +
  geom_boxplot(alpha = 0.7, width = 0.3) +
  #  geom_point(aes(x = Mean.Betaine.Val, y = reorder(compound.name.figure, -Mean.Betaine.Rank)), color = "black", size = 2, shape = 21, fill = "white") +
  scale_fill_manual(values = region.palette.2) +
  scale_x_continuous(trans = trans_reverser('log10'), breaks = c(1, 0.5, 0.25, 0.1, 0.05, 0.01, 0.001), limits = c(1.5, 0.001)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  xlab("Relative Abundance") +
  ylab("Compound") +
  ggtitle("Particulate")
p.rel.conc.plot

#diss
d.rel.conc.plot <- ggplot(d.betaine.dat, aes(x = Betaine.Norm.Val, y = reorder(compound.name.figure, -Mean.Betaine.Rank))) +
  geom_jitter(height = 0.12, width = 0, alpha = 0.6, shape = 21, color = "black", size = 2, stroke = 0.1, aes(fill = Region)) +
  # geom_errorbarh(aes(xmin = Betaine.Val.Min, 
  #                    xmax = Betaine.Val.Max, 
  #                    y = reorder(compound.name.figure, -Mean.Betaine.Rank)), 
  #                color = "black", height = 0, size = 0.3) +
  geom_boxplot(alpha = 0.7, width = 0.3) +
  #  geom_point(aes(x = Mean.Betaine.Val, y = reorder(compound.name.figure, -Mean.Betaine.Rank)), color = "black", size = 2, shape = 21, fill = "white") +
  scale_fill_manual(values = region.palette.2) +
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


#total
t.rel.conc.plot <- ggplot(t.betaine.dat, aes(x = Betaine.Norm.Val, y = reorder(compound.name.figure, -Mean.Betaine.Rank))) +
  geom_jitter(height = 0.12, width = 0, alpha = 0.6, shape = 21, color = "black", size = 2, stroke = 0.1, aes(fill = Region)) +
  geom_boxplot(alpha = 0.7, width = 0.3) +
  #  geom_point(aes(x = Mean.Betaine.Val, y = reorder(compound.name.figure, -Mean.Betaine.Rank)), color = "black", size = 2, shape = 21, fill = "white") +
  scale_fill_manual(values = region.palette.2) +
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



##Redfield.sum 
p.red <- p.betaine.dat %>%
  select(compound.name.figure, Betaine.Redfield.Ratio, Mean.Betaine.Rank) %>%
  unique() %>%
  rename(P.Ratio = Betaine.Redfield.Ratio,
         P.Rank = Mean.Betaine.Rank)

d.red <- d.betaine.dat %>%
  select(compound.name.figure, Betaine.Redfield.Ratio, Mean.Betaine.Rank) %>%
  unique() %>%
  rename(D.Ratio = Betaine.Redfield.Ratio,
         D.Rank = Mean.Betaine.Rank)

t.red <- t.betaine.dat %>%
  select(compound.name.figure, Betaine.Redfield.Ratio, Mean.Betaine.Rank) %>%
  unique() %>%
  rename(T.Ratio = Betaine.Redfield.Ratio,
         T.Rank = Mean.Betaine.Rank)

####Get compound properties
std.formula <- read_csv(stds.file) %>%
  select(Compound_Name, mz) %>%
  rename("Compound" = Compound_Name) %>%
  unique() 

comp.info <- dat %>%
  select(Compound, compound.name.figure, class) %>%
  unique() %>%
  filter(class == "Betaine") %>%
  left_join(., std.formula) %>%
  select(compound.name.figure, mz)

###
red.comb <- left_join(p.red, d.red) %>%
  left_join(., t.red) %>%
  left_join(., comp.info)

###Run quick linear models 
lm.p.mz <- lm(mz~P.Rank, data = red.comb)
summary(lm.p.mz)

lm.d.mz <- lm(mz~D.Rank, data = red.comb)
summary(lm.d.mz)

lm.t.mz <- lm(mz~T.Rank, data = red.comb)
summary(lm.t.mz)

#Particulate
mz.p.plot <- ggplot(red.comb, aes(x = P.Rank, y = mz)) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgray") +
  geom_point(size = 2) +
  theme_bw() +
  xlab("Mean Rank in Particulate") +
  annotate("text", x = 6, y = 130, 
           label = expression(R^2~"="~0.71),
           size = 4) 
mz.p.plot

#Dissolved
mz.d.plot <- ggplot(red.comb, aes(x = D.Rank, y = mz)) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgray") +
  geom_point(size = 2) +
  theme_bw() +
  xlab("Mean Rank in Dissolved") +
  annotate("text", x = 6, y = 130, 
           label = expression(R^2~"="~0.73),
           size = 4) +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank())
mz.d.plot

#Total:
mz.t.plot <- ggplot(red.comb, aes(x = T.Rank, y = mz)) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgray") +
  geom_point(size = 2) +
  theme_bw() +
  xlab("Mean Rank in Total") +
  annotate("text", x = 6, y = 130, 
           label = expression(R^2~"="~0.54),
           size = 4) 
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank())
mz.t.plot



##Put it all together:
brr.plot <- (p.rel.conc.plot | d.rel.conc.plot | t.rel.conc.plot)/(mz.p.plot | mz.d.plot | mz.t.plot) +
  plot_layout(guides = "collect", heights = c(0.65, 0.35)) + 
  plot_annotation(tag_levels = 'A')
brr.plot

ggsave(brr.plot, file = "Figures/Outputs/Betaine_Redfield_Ratio.png", 
       height = 5, width = 8, scale = 1.4, dpi = 600)












