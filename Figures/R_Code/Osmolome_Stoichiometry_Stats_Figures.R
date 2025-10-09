

library(tidyverse)
library(patchwork)



###define inputs
all.dat.file <- "Intermediates/Final_Osmo_Meta_Env_Dataframe.csv"



#Read in data
dat <- read_csv(all.dat.file) %>%
  filter(!Part.SampID == "220902_Smp_TN397_S11_600_U_C") %>%
  filter(!str_detect(Part.SampID, "Smp_S4_C1"),
         !str_detect(Part.SampID, "Smp_S4_C1"))


#Pull in data and organize for PCA Analysis:

#Particulate Surface Data
env.stoich.dat <- dat %>%
  filter(Cruise %in% c("TN397", "KM1906", "RC078")) %>%
  filter(depth_m < 10) %>%
  filter(!station %in% c(1, 8)) %>%
  filter(!is.na(Part.SampID)) %>%
  select(Part.SampID, Cruise, Compound, Part.Conc.nM, Part.C.nM, Part.N.nM, Part.S.nM, Region, Lat, Long) %>%
  unique() %>%
  group_by(Part.SampID) %>%
  mutate(Sum.Part.Conc.nM = sum(Part.Conc.nM, na.rm = TRUE),
         Sum.Part.C.nM = sum(Part.C.nM, na.rm = TRUE),
         Sum.Part.N.nM = sum(Part.N.nM, na.rm = TRUE),
         Sum.Part.S.nM = sum(Part.S.nM, na.rm = TRUE),
         Part.CN = Sum.Part.C.nM/Sum.Part.N.nM,
         Part.CS = Sum.Part.C.nM/Sum.Part.S.nM,
         Part.NS = Sum.Part.N.nM/Sum.Part.S.nM) %>%
  select(Part.SampID, Lat, Long, Region, Sum.Part.Conc.nM, Part.CN, Part.CS, Part.NS) %>%
  unique() %>%
  filter(!Region == "PS")


#Organize Perifix data:
peri.dat <- dat %>%
  filter(Cruise %in% c("PERIFIX")) %>%
  filter(!is.na(Part.SampID)) %>%
  select(Part.SampID, Cruise, Part.Conc.nM, Part.C.nM, Part.N.nM, Part.S.nM, Treatment) %>%
  unique() %>%
  group_by(Part.SampID) %>%
  mutate(Sum.Part.Conc.nM = sum(Part.Conc.nM, na.rm = TRUE),
         Sum.Part.C.nM = sum(Part.C.nM, na.rm = TRUE),
         Sum.Part.N.nM = sum(Part.N.nM, na.rm = TRUE),
         Sum.Part.S.nM = sum(Part.S.nM, na.rm = TRUE),
         Part.CN = Sum.Part.C.nM/Sum.Part.N.nM,
         Part.CS = Sum.Part.C.nM/Sum.Part.S.nM,
         Part.NS = Sum.Part.N.nM/Sum.Part.S.nM) %>%
  mutate(Treatment = as.factor(Treatment),
         Treatment = fct_relevel(Treatment, c("Tote", "C", "P", "F", "PF", "NPF", "NP", "NF", "N")),
         N = as.factor(case_when(Treatment %in% c("N", "NP", "NF", "NPF") ~ 1,
                                 TRUE ~ 0)), 
         P = as.factor(case_when(Treatment %in% c("P", "NP", "PF", "NPF") ~ 1,
                                 TRUE ~ 0)), 
         Fe = as.factor(case_when(Treatment %in% c("F", "PF", "NF", "NPF") ~ 1,
                                  TRUE ~ 0))) %>%
  filter(!Treatment == "Tote") %>%
  select(Part.SampID, Treatment, N, P, Fe, Sum.Part.Conc.nM, Part.CN, Part.CS, Part.NS) %>%
  unique() %>%
  mutate(N_status = case_when(Treatment %in% c("N", "NP", "NF", "NPF") ~ "+N",
                       TRUE ~ "-N")) 


#####Gradients:
pelagic.stoich <- env.stoich.dat %>%
  ungroup() %>%
  select(Region, Sum.Part.Conc.nM, Part.NS) %>%
  unique() %>%
  filter(!is.na(Sum.Part.Conc.nM)) %>%
  group_by(Region)  %>%
  reframe(Mean.Part.Conc.nM = mean(Sum.Part.Conc.nM),
          SD.Osmo.nM = sd(Sum.Part.Conc.nM),
          SE.Osmo.nM = SD.Osmo.nM/n(),
          Median.Osmo.nM = median(Sum.Part.Conc.nM),
          Mean.NS = mean(Part.NS),
          Median.NS = median(Part.NS),
          SD.NS = sd(Part.NS),
          SE.NS = SD.NS/n())


###Do PERIFIX ANOVAs:

#C/N

#ANOVA:
peri.anova.C.N <- aov(Part.CN ~ N*P*Fe, data = peri.dat)  
summary(peri.anova.C.N)

#Post-Hoc Test:
peri.tuk.C.N <- TukeyHSD(peri.anova.C.N, which = "N")
peri.tuk.C.N

#C/S:

#ANOVA:
peri.anova.C.S <- aov(Part.CS ~ N*P*Fe, data = peri.dat)  
summary(peri.anova.C.S)

#Post-Hoc Test:
peri.tuk.C.S <- TukeyHSD(peri.anova.C.S, which = "N")
peri.tuk.C.S

#N/S:
#ANOVA:
peri.anova.N.S <- aov(Part.NS ~ N*P*Fe, data = peri.dat)  
summary(peri.anova.N.S)

peri.tuk.N.S <- TukeyHSD(peri.anova.N.S, which = "N")
peri.tuk.N.S

#peri.summary values 
peri.sum <- peri.dat %>%
  ungroup() %>%
  group_by(N_status) %>%
  reframe(mean_CN = mean(Part.CN),
         sd_CN = sd(Part.CN),
         mean_CS = mean(Part.CS),
         sd_CS = sd(Part.CS),
         mean_NS = mean(Part.NS),
         sd_NS = sd(Part.NS))







###Do Region ANOVAs:

#C/N:_____
env.anova.C.N <- aov(Part.CN ~ Region, data = env.stoich.dat)  
summary(env.anova.C.N)

#Post-Hoc Test:
env.tuk.C.N <- TukeyHSD(env.anova.C.N, which = "Region")
env.tuk.C.N

#C/S:____
env.anova.C.S <- aov(Part.CS ~ Region, data = env.stoich.dat)  
summary(env.anova.C.S)

#Post-Hoc Test:
env.tuk.C.S <- TukeyHSD(env.anova.C.S, which = "Region")
env.tuk.C.S

#N/S:___
env.anova.N.S <- aov(Part.NS ~ Region, data = env.stoich.dat)  
summary(env.anova.N.S)

#Post-Hoc Test:
env.tuk.N.S <- TukeyHSD(env.anova.N.S, which = "Region")
env.tuk.N.S



# Make Figures ------------------------------------------------------------

####Make Perifix Figure
#___C/N Ratio
c.n.p.fig <- ggplot(peri.dat, aes(x = N_status, y = Part.CN)) +
  geom_boxplot(aes(color = N_status), width = 0.5) +
  scale_color_manual(values = c("gray70", "gray10")) +
  geom_jitter(width = .1, height = 0, shape = 21, size = 3, stroke = 0.2, fill = "white", color = "black") +
  theme_test() +
  theme(legend.position = "none") +
  xlab("Treatment") +
  ylab("C/N Ratio")
#  ylim(0,17)
c.n.p.fig


#___N/S Ratio
n.s.p.fig <- ggplot(peri.dat, aes(x = N_status, y = Part.NS)) +
  geom_boxplot(aes(color = N_status), width = 0.5) +
  scale_color_manual(values = c("gray70", "gray10")) +
  geom_jitter(width = .1, height = 0, shape = 21, size = 3, stroke = 0.2, fill = "white", color = "black") +
  theme_test() +
  theme(legend.position = "none") +
  xlab("Treatment") +
  ylab("N/S Ratio")
#  ylim(0,17)
n.s.p.fig


#stoich plot with mean and SD
ocean.stoich.plot <- ggplot(pelagic.stoich , aes(x = Mean.NS, y = Mean.Part.Conc.nM, fill = Region)) +
  geom_errorbar(aes(x = Mean.NS, ymin = Mean.Part.Conc.nM-SD.Osmo.nM, ymax = Mean.Part.Conc.nM+SD.Osmo.nM), color = "gray80") +
  geom_errorbarh(aes(y = Mean.Part.Conc.nM, xmin = Mean.NS-SD.NS, xmax = Mean.NS+SD.NS), height = 0.5, color = "gray80") +
  geom_point(shape = 21, size = 5, stroke = 0.15, color = "black") +
  ylim(0, 32) +
  xlim(0.5, 5.25) +
  scale_fill_manual(values = region.palette.2) +
  theme_test() +
  theme(legend.position = "inside", legend.position.inside = c(0.8, 0.8)) +
  xlab("N/S Ratio") +
  ylab("Summed Particulate Osmolyte Concentration (nM)") 

ocean.stoich.plot




###Make Full Plot:
Stoich.fig <- (c.n.p.fig / n.s.p.fig | ocean.stoich.plot)  +
  plot_layout(widths = c(1,3)) +
  plot_annotation(tag_levels = "A")

Stoich.fig

ggsave(Stoich.fig, file = "Figures/Outputs/Stoichiometry_Figure.png" , height = 4.5, width = 5.25, dpi = 600, scale = 1.2)

ggsave(Stoich.fig, file = "Figures/Outputs/Pres_Stoichiometry_Figure.pdf" , height = 4.5, width = 6.5, dpi = 600, scale = 1.2)

###PERIFIX total osmolyte vs. N/S ratio 
peri.stoich.fig <- ggplot(peri.dat, aes(x = Part.NS, y = Sum.Part.Conc.nM, color = N_status)) +
  geom_point(shape = 21, stroke = 1, size = 2.5) +
  scale_color_manual(values = c("gray70", "gray10")) +
  theme_test() +
  theme(legend.position = "inside", legend.position.inside = c(0.8, 0.8)) +
  xlab("N/S Ratio") +
  ylab("Total Osmolyte Concentration (nM)") +
  labs(color = "Treatment")
peri.stoich.fig

ggsave(peri.stoich.fig, file = "Figures/Outputs/PERIFIX_Stoichiometry_Concentration_SuppFigure.png" , height = 3.5, width = 3.5, dpi = 600, scale = 1.2)












###Env Stoichiometry Figures:
gradients.env.stoich <- env.stoich.dat %>%
  filter(Region %in% c("NPSG", "NPTZ", "Equator"))

ggplot(gradients.env.stoich, aes(x = Lat, y = Part.NS)) +
  geom_point(aes(fill = Region), shape = 21)


ns.g.plot <- ggplot(gradients.env.stoich, aes(x = Lat, y = Part.NS)) +
  geom_smooth(color = "black") +
  geom_point(aes(fill = Region), shape = 21, size = 3.5, stroke = 0.2) +
  scale_fill_manual(values = region.palette.2) +
  theme_test() +
  ylab("Osmolome N/S Ratio")
ns.g.plot

cn.g.plot <- ggplot(gradients.env.stoich, aes(x = Lat, y = Part.CN)) +
  geom_smooth(color = "black") +
  geom_point(aes(fill = Region), shape = 21, size = 3.5, stroke = 0.2) +
  scale_fill_manual(values = region.palette.2) +
  theme_test() +
  ylab("Osmolome C/N Ratio") +
  ylim(c(0, 12))
cn.g.plot

cs.g.plot <- ggplot(gradients.env.stoich, aes(x = Lat, y = Part.CS)) +
  geom_smooth(color = "black") +
  geom_point(aes(fill = Region), shape = 21, size = 3.5, stroke = 0.2) +
  scale_fill_manual(values = region.palette.2) +
  theme_test() +
  ylab("Osmolome C/S Ratio")
cs.g.plot

g.stoich.plots <- cn.g.plot + ns.g.plot + cs.g.plot + 
  plot_layout(guides = "collect")
g.stoich.plots

ggsave(g.stoich.plots, filename = "Figures/Outputs/gradients_stoich_plots.jpg", dpi = 600,
       height = 4, width = 12)





ggplot(gradients.env.stoich, aes(x = Lat, y = Part.CN)) +
  geom_point(aes(fill = Region), shape = 21) +
  ylim(c(0, 12))

ggplot(gradients.env.stoich, aes(x = Part.NS, y = Sum.Part.Conc.nM)) +
  geom_point(aes(fill = Region), shape = 21)










