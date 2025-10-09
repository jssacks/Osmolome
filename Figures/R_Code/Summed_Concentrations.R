










library(tidyverse)
library(patchwork)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")


###define inputs
all.dat.file <- "Intermediates/Final_Osmo_Meta_Env_Dataframe.csv"



#Read in data
dat <- read_csv(all.dat.file) %>%
  filter(!Part.SampID == "220902_Smp_TN397_S11_600_U_C") %>%
  filter(!station %in% c(1, 8)) %>%
  filter(depth_m < 10) %>%
  filter(Cruise %in% c("TN397", "KM1906", "RC078")) %>%
  filter(is.na(Tot.Flag))


###Summed Concentrations
summed.dat <- dat %>%
  select(Part.SampID, Compound, Diss.SampID, Region, poc, sss, Lat, Part.Conc.nM, Diss.Conc.nM.adj, Total.Conc.nM) %>%
  unique() %>%
  group_by(Part.SampID, Diss.SampID, Region, Lat, poc, sss) %>%
  reframe(Sum.Part.nM = sum(Part.Conc.nM, na.rm = TRUE),
          Sum.Diss.nM = sum(Diss.Conc.nM.adj, na.rm = TRUE),
          Sum.Tot.nM = sum(Total.Conc.nM, na.rm = TRUE)) %>%
  group_by(Region) %>%
  mutate(Sum.Part.nM = case_when(is.na(Part.SampID) ~ NA,
                                 TRUE ~ Sum.Part.nM)) %>%
  mutate(Sum.Diss.nM = case_when(is.na(Diss.SampID) ~ NA,
                                 TRUE ~ Sum.Diss.nM)) %>%
  mutate(Sum.Tot.nM = case_when(is.na(Diss.SampID) ~ NA,
                                 TRUE ~ Sum.Tot.nM)) %>%
  mutate(Sum.Tot.nM = case_when(is.na(Part.SampID) ~ NA,
                                TRUE ~ Sum.Tot.nM)) %>%
  mutate(Mean.Part.nM = mean(Sum.Part.nM, na.rm = TRUE),
         SD.Part.nM = sd(Sum.Part.nM, na.rm = TRUE),
         Mean.Diss.nM = mean(Sum.Diss.nM, na.rm = TRUE),
         SD.Diss.nM = sd(Sum.Diss.nM, na.rm = TRUE),
         Mean.Total.nM = mean(Sum.Tot.nM, na.rm = TRUE),
         SD.Total.nM = sd(Sum.Tot.nM, na.rm = TRUE))

  
g.sum.dat <- summed.dat %>%
  filter(Region %in% c("CC", "NPSG", "NPTZ", "Equator"))

d.sum.dat <- summed.dat %>%
  filter(Region %in% c("PS"))





####look at relationship of all parameters to POC

# Run linear models

#particulate:
poc.part.lm <- lm(log10(Sum.Part.nM) ~ log10(poc), summed.dat)
summary(poc.part.lm)

#dissolved:
poc.diss.lm <- lm(log10(Sum.Diss.nM) ~ log10(poc), summed.dat)
summary(poc.diss.lm)

#total:
poc.tot.lm <- lm(log10(Sum.Tot.nM) ~ log10(poc), summed.dat)
summary(poc.tot.lm)


#look at correlation of disslved and particulate and total and particulate

#dissolved:
part.diss.lm <- lm(log10(Sum.Diss.nM) ~ log10(Sum.Part.nM), summed.dat)
summary(part.diss.lm)

#total:
part.tot.lm <- lm(log10(Sum.Tot.nM) ~ log10(Sum.Part.nM), summed.dat)
summary(part.tot.lm)




#Visualize POC-Osmolyte relationships in scatter plots:

#Particulate
p.p.fig <- ggplot(summed.dat, aes(x = poc, y = Sum.Part.nM)) +
  geom_smooth(color = "black", method = "lm") +
  geom_point(shape = 21, stroke = 0.15, size = 2.5, aes(fill = Region)) +
  scale_x_log10() +
  scale_y_log10() +
  theme_test() +
  scale_fill_manual(values = region.palette.2) +
  xlab("POC (uM)") +
  ylab("Summed Particulate Osmolytes (nM)")
p.p.fig 

#Dissolved
p.d.fig <- ggplot(summed.dat, aes(x = poc, y = Sum.Diss.nM)) +
  geom_smooth(color = "black", method = "lm") +
  geom_point(shape = 21, stroke = 0.15, size = 2.5, aes(fill = Region)) +
  scale_x_log10() +
  scale_y_log10() +
  theme_test() +
  scale_fill_manual(values = region.palette.2) +
  xlab("POC (uM)") +
  ylab("Summed Dissolved Osmolytes (nM)")
p.d.fig

#Total
p.t.fig <- ggplot(summed.dat, aes(x = poc, y = Sum.Tot.nM)) +
  geom_smooth(color = "black", method = "lm") +
  geom_point(shape = 21, stroke = 0.15, size = 2.5, aes(fill = Region)) +
  scale_x_log10() +
  scale_y_log10() +
  theme_test() +
  scale_fill_manual(values = region.palette.2) +
  xlab("POC (uM)") +
  ylab("Summed Total Osmolytes (nM)")
p.t.fig



# SSS ---------------------------------------------------------------------
####look at relationship of all parameters to SSS

# Run linear models

#particulate:
sss.part.lm <- lm(log10(Sum.Part.nM) ~ sss, summed.dat)
summary(sss.part.lm)

#dissolved:
sss.diss.lm <- lm(log10(Sum.Diss.nM) ~ sss, summed.dat)
summary(sss.diss.lm)

#total:
sss.tot.lm <- lm(log10(Sum.Tot.nM) ~ sss, summed.dat)
summary(sss.tot.lm)


#Visualize SSS-Osmolyte relationships in scatter plots:

#Particulate
s.p.fig <- ggplot(summed.dat, aes(x = sss, y = Sum.Part.nM)) +
  geom_smooth(color = "black", method = "lm") +
  geom_point(shape = 21, stroke = 0.15, size = 2.5, aes(fill = Region)) +
#  scale_x_log10() +
  scale_y_log10() +
  theme_test() +
  scale_fill_manual(values = region.palette.2) +
  xlab("Salinity (PSU)") +
  ylab("Summed Particulate Osmolytes (nM)")
s.p.fig

#Dissolved
s.d.fig <- ggplot(summed.dat, aes(x = sss, y = Sum.Diss.nM)) +
  geom_smooth(color = "black", method = "lm") +
  geom_point(shape = 21, stroke = 0.15, size = 2.5, aes(fill = Region)) +
#  scale_x_log10() +
  scale_y_log10() +
  theme_test() +
  scale_fill_manual(values = region.palette.2) +
  xlab("Salinity (PSU)") +
  ylab("Summed Dissolved Osmolytes (nM)")
s.d.fig

#Total
s.t.fig <- ggplot(summed.dat, aes(x = sss, y = Sum.Tot.nM)) +
  geom_smooth(color = "black", method = "lm") +
  geom_point(shape = 21, stroke = 0.15, size = 2.5, aes(fill = Region)) +
#  scale_x_log10() +
  scale_y_log10() +
  theme_test() +
  scale_fill_manual(values = region.palette.2) +
  xlab("Salinity (PSU)") +
  ylab("Summed Total Osmolytes (nM)")
s.t.fig


#####Make combined figure
Summed.comb.fig <- (p.p.fig | p.d.fig | p.t.fig) / (s.p.fig | s.d.fig | s.t.fig) +
  plot_layout(guides = "collect") + 
  plot_annotation(tag_levels = "A")

Summed.comb.fig

ggsave(Summed.comb.fig, file = "Figures/Outputs/POC_Salinity_Correlation_Plots.png",
       height = 4, width = 7, dpi = 600, scale = 1.5)


ggsave(Summed.comb.fig, file = "Figures/Outputs/POC_Salinity_Correlation_Plots_Pres.pdf",
       height = 4, width = 7, dpi = 600, scale = 1.5)

###Comparing dissolved and particulate total concentrations:

# ggplot(summed.dat, aes(x = Sum.Part.nM, y = Sum.Diss.nM)) +
#   geom_smooth(color = "black", method = "lm") +
#   geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
#   geom_point(shape = 21, stroke = 0.15, size = 2.5, aes(fill = Region)) +
#   scale_x_log10() +
#   scale_y_log10() +
#   theme_test() +
#   scale_fill_manual(values = region.palette.2) +
#   xlab("Summed Particulate Osmolytes (nM)") +
#   ylab("Summed Dissolved Osmolytes (nM)")
