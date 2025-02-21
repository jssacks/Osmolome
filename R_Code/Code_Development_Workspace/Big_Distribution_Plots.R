



library(patchwork)


library(tidyverse)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")

##Define inputs:
part.file <- "Intermediates/Particulate_Quant_Output.csv"
meta.file <- "Intermediates/All_metadata_information.csv"

###load in data and combine with metadata
part.dat <- read_csv(part.file)
meta.dat <- read_csv(meta.file)


####
dat.all <- left_join(part.dat, meta.dat) %>%
  rename("Compound" = Name)





gradients <- dat.all %>%
  filter(Cruise %in% c("TN397", "KM1906")) %>%
  filter(Compound %in% compound.order$Compound) %>%
  left_join(., compound.order)



####Gradient Survey Plot
lat.breaks <- seq(-3,50, by = 1)

g3.g4.dat <- gradients %>%
  filter(Cruise %in% c("KM1906", "TN397")) %>%
  filter(Long < -124) %>%
  mutate(lat_bin = cut(Lat, breaks = lat.breaks, include.lowest = TRUE, right = FALSE)) 

g3.g4.transect.dat <- g3.g4.dat %>%
  group_by(lat_bin, Compound) %>%
  reframe(Mean_nM = mean(nM.in.smp, na.rm = TRUE),
          Mean_nM_C = mean(nM_C, na.rm = TRUE),
          Mean_nM_N = mean(nM_N, na.rm = TRUE),
          Mean_nM_S = mean(nM_S, na.rm = TRUE)) %>%
  left_join(., compound.order) 


#####Plot
##Big Plot
bigplot.1 <- ggplot(g3.g4.transect.dat, aes(x = lat_bin, y=Mean_nM, fill = reorder(Compound, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15) +
  scale_fill_manual(values = compound.pal)+
  #  scale_fill_manual(values = stepped2(n = 20)) +
  scale_y_continuous(expand = c(0,NA,NA,NA), limits = c(0,35), breaks = c(0,5,10,15,20,25,30,35)) +
  theme_test() + 
  theme(axis.text = element_text(angle = 90, vjust = 0.8)) +
  ylab("Mean nM Compound") +
  xlab("Latitude") +
  labs(fill = "Compound")
bigplot.1

bigplot.2 <- ggplot(g3.g4.transect.dat, aes(x = lat_bin, y=Mean_nM, fill = reorder(Compound, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15, position = "fill") +
  scale_fill_manual(values = compound.pal)+
  #  scale_fill_manual(values = stepped2(n = 20)) +
  scale_y_continuous(expand = c(0,NA,NA,NA)) +
  theme_test() + 
  theme(axis.text = element_text(angle = 90, vjust = 0.8)) +
  ylab("Mean nM Compound") +
  xlab("Latitude") +
  labs(fill = "Compound")
bigplot.2

###Just the sulfur compounds:
sulf.g.dat <- g3.g4.transect.dat %>%
  filter(class %in% c("Sulfonate", "Sulfonium", "Taurine"))

g.plot.s <- ggplot(sulf.g.dat, aes(x = lat_bin, y=Mean_nM, fill = reorder(Compound, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15, position = "fill") +
  scale_fill_manual(values = compound.pal)+
  #  scale_fill_manual(values = stepped2(n = 20)) +
  scale_y_continuous(expand = c(0,NA,NA,NA)) +
  theme_test() + 
  theme(axis.text = element_text(angle = 90, vjust = 0.8)) +
  ylab("Mole Fraction") +
  xlab("Latitude") +
  labs(fill = "Compound")
g.plot.s




###DINIMITE 1 Surface Plots
d1.dat <- dat.all %>%
  filter(Cruise %in% c("RC078")) %>%
  filter(Compound %in% compound.order$Compound) %>%
  left_join(., compound.order) %>%
  filter(depth_m < 10)

###
d1.station.dat <- d1.dat %>%
  group_by(station, Compound) %>%
  reframe(Mean_nM = mean(nM.in.smp, na.rm = TRUE),
          Mean_nM_C = mean(nM_C, na.rm = TRUE),
          Mean_nM_N = mean(nM_N, na.rm = TRUE),
          Mean_nM_S = mean(nM_S, na.rm = TRUE)) %>%
  left_join(., compound.order) %>%
  filter(station %in% c(2, 3, 5, 6, 7)) %>%
  mutate(station = as.factor(station))

###
#####Plot
##DINIMITE Plot
d.plot.1 <- ggplot(d1.station.dat, aes(x = station, y=Mean_nM, fill = reorder(Compound, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15) +
  scale_fill_manual(values = compound.pal)+
  #  scale_fill_manual(values = stepped2(n = 20)) +
  scale_y_continuous(expand = c(0,NA,NA,NA), limits = c(0,550)) +
  theme_test() + 
  theme() +
  ylab("Mean nM Compound") +
  xlab("Station") +
  labs(fill = "Compound")
d.plot.1

d.plot.2 <- ggplot(d1.station.dat, aes(x = station, y=Mean_nM, fill = reorder(Compound, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15, position = "fill") +
  scale_fill_manual(values = compound.pal)+
  #  scale_fill_manual(values = stepped2(n = 20)) +
  scale_y_continuous(expand = c(0,NA,NA,NA)) +
  theme_test() + 
  theme(axis.text = element_text(angle = 90, vjust = 0.8)) +
  ylab("Mole Fraction") +
  xlab("Station") +
  labs(fill = "Compound")
d.plot.2

#Just sulfur compounds:
d1.station.dat.s <- d1.station.dat %>%
  filter(class %in% c("Sulfonate", "Sulfonium", "Taurine"))

d.plot.s <- ggplot(d1.station.dat.s, aes(x = station, y=Mean_nM, fill = reorder(Compound, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15, position = "fill") +
  scale_fill_manual(values = compound.pal)+
  #  scale_fill_manual(values = stepped2(n = 20)) +
  scale_y_continuous(expand = c(0,NA,NA,NA)) +
  theme_test() + 
  theme(axis.text = element_text(angle = 90, vjust = 0.8)) +
  ylab("Mole Fraction") +
  xlab("Station") +
  labs(fill = "Compound")
d.plot.s




###PERIFIX Plots
p.dat <- dat.all %>%
  filter(Cruise %in% c("PERIFIX")) %>%
  filter(Compound %in% compound.order$Compound) 

###
p.dat.mean <- p.dat %>%
  group_by(Treatment, Compound) %>%
  reframe(Mean_nM = mean(nM.in.smp, na.rm = TRUE),
          Mean_nM_C = mean(nM_C, na.rm = TRUE),
          Mean_nM_N = mean(nM_N, na.rm = TRUE),
          Mean_nM_S = mean(nM_S, na.rm = TRUE)) %>%
  left_join(., compound.order) %>%
  mutate(Treatment = as.factor(Treatment),
         Treatment = fct_relevel(Treatment, c("Tote", "C", "P", "F", "PF", "NPF", "NP", "NF", "N")),
         N_status = case_when(Treatment %in% c("N", "NP", "NF", "NPF") ~ "+N",
                             TRUE ~ "-N")) %>%
  filter(!Treatment == "Tote")

###
p.plot.1 <- ggplot(p.dat.mean, aes(x = Treatment, y=Mean_nM, fill = reorder(Compound, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15) +
  scale_fill_manual(values = compound.pal)+
  #  scale_fill_manual(values = stepped2(n = 20)) +
  scale_y_continuous(expand = c(0,NA,NA,NA), limits = c(0,135)) +
  theme_test() + 
  theme(axis.text = element_text(angle = 90, vjust = 0.8)) +
  ylab("Mean nM Compound") +
  xlab("Treatment") +
  labs(fill = "Compound")
p.plot.1

p.plot.2 <- ggplot(p.dat.mean, aes(x = Treatment, y=Mean_nM, fill = reorder(Compound, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15, position = "fill") +
  scale_fill_manual(values = compound.pal)+
  #  scale_fill_manual(values = stepped2(n = 20)) +
  scale_y_continuous(expand = c(0,NA,NA,NA)) +
  theme_test() + 
  theme(axis.text = element_text(angle = 90, vjust = 0.8)) +
  ylab("Mean nM Compound") +
  xlab("Treatment") +
  labs(fill = "Compound")
p.plot.2

#Just sulfur compounds:
p.dat.mean.s <- p.dat.mean %>%
  filter(class %in% c("Sulfonate", "Sulfonium", "Taurine"))

p.plot.s <- ggplot(p.dat.mean.s, aes(x = Treatment, y=Mean_nM, fill = reorder(Compound, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15, position = "fill") +
  scale_fill_manual(values = compound.pal)+
  #  scale_fill_manual(values = stepped2(n = 20)) +
  scale_y_continuous(expand = c(0,NA,NA,NA)) +
  theme_test() + 
  theme(axis.text = element_text(angle = 90, vjust = 0.8)) +
  ylab("Mole Fraction") +
  xlab("Treatment") +
  labs(fill = "Compound")
p.plot.s


#Put sulfur plots together:
sulf.plot <- (g.plot.s | (d.plot.s + p.plot.s)) +
  plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = "A")
sulf.plot
