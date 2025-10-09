

####

library(tidyverse)
library(patchwork)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")



###define inputs
all.dat.file <- "Intermediates/Final_Osmo_Meta_Env_Dataframe.csv"



#Read in data
dat <- read_csv(all.dat.file) %>%
  filter(!Part.SampID == "220902_Smp_TN397_S11_600_U_C") %>%
  filter(!str_detect(Part.SampID, "Smp_S4_C1"),
         !str_detect(Part.SampID, "Smp_S4_C1"))



##Make plots with all samples:
G4.dat <- dat %>%
  filter(Cruise %in% c("TN397", "KM1906")) %>%
  group_by(Part.SampID) %>%
  mutate(sum.p.c.nM = sum(Part.C.nM),
         sum.p.n.nM = sum(Part.N.nM, na.rm = TRUE),
         sum.p.s.nM = sum(Part.S.nM, na.rm = TRUE),
         sum.d.n.nM = sum(Diss.N.nM.adj, na.rm = TRUE),
         sum.d.s.nM = sum(Diss.S.nM.adj, na.rm = TRUE),
         sum.tot.N.nM = sum.p.n.nM + sum.d.n.nM,
         sum.tot.S.nM = sum.p.s.nM + sum.d.s.nM, 
         sum.p.NS.ratio = sum.p.n.nM/sum.p.s.nM,
         sum.d.NS.ratio = sum.d.n.nM/sum.d.s.nM,
         sum.tot.NS.ratio = sum.tot.N.nM/sum.tot.S.nM) %>%
  select(Part.SampID, Region, Cruise, N_N, Lat, Long, 
         sum.p.NS.ratio, sum.p.n.nM, sum.p.s.nM,
         sum.d.NS.ratio, sum.d.n.nM, sum.d.s.nM,
         sum.tot.N.nM, sum.tot.S.nM, sum.tot.NS.ratio) %>%
  filter(sum.tot.NS.ratio < 10)
  

ggplot(G4.dat, aes(x = Lat, y = sum.tot.NS.ratio)) +
  geom_point(aes(color = Region)) +
  geom_smooth() +
  facet_wrap(.~Cruise, scales = "free_x")

ggplot(G4.dat, aes(x = Lat, y = sum.p.NS.ratio)) +
  geom_point(aes(color = Region)) +
  geom_smooth() +
  facet_wrap(.~Cruise, scales = "free_x")

ggplot(G4.dat, aes(x = Lat, y = sum.d.NS.ratio)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(.~Cruise, scales = "free_x")


ggplot(G4.dat, aes(x = Lat, y = sum.p.n.nM)) +
  geom_point()

ggplot(G4.dat, aes(x = Lat, y = sum.p.s.nM)) +
  geom_point()

ggplot(G4.dat, aes(x = Lat, y = sum.d.n.nM)) +
  geom_point()

ggplot(G4.dat, aes(x = Lat, y = sum.tot.N.nM)) +
  geom_point()

ggplot(G4.dat, aes(x = Lat, y = sum.tot.S.nM)) +
  geom_point()





#Compositional Data averaged by Treatment
dat.perifix <- dat %>%
  filter(Cruise %in% c("PERIFIX")) %>%
  group_by(Compound, compound.name.figure, Treatment, order, class) %>%
  reframe(Mean_nM = mean(Part.Conc.nM)) %>%
  mutate(Treatment = as.factor(Treatment)) %>%
  mutate(Treatment = fct_relevel(Treatment, c("Tote", "C", "F", "P", "PF", "N", 
                                              "NF", "NP", "NPF")))




dat.perifix.stoich.check <- dat %>%
  filter(Cruise %in% c("PERIFIX")) %>%
  filter(!is.na(Part.SampID)) %>%
  mutate(compound.stoich.N = case_when(class %in% c("Betaine", "AA") ~ 1,
                                       compound.name.figure %in% c("Ectoine", "Hydroxyectoine", "TMAO", "Taurine") ~ 1,
                                       TRUE ~ 0)) %>%
  mutate(compound.stoich.C = case_when(class %in% c("Sugar") ~ 1,
                                       TRUE ~ 0)) %>%
  mutate(compound.stoich.S = case_when(class %in% c("Sulfonium", "Sulfonate") ~ 1,
                                       TRUE ~ 0)) %>%
  select(Part.SampID, compound.name.figure, Part.Conc.nM, compound.stoich.N, compound.stoich.C, compound.stoich.S)






dat.perifix.stoich <- dat %>%
  filter(Cruise %in% c("PERIFIX")) %>%
  filter(!is.na(Part.SampID)) %>%
  mutate(compound.stoich.N = case_when(class %in% c("Betaine", "AA") ~ 1,
                                       compound.name.figure %in% c("Ectoine", "Hydroxyectoine", "TMAO", "Taurine") ~ 1,
                                       TRUE ~ 0)) %>%
  mutate(compound.stoich.C = case_when(class %in% c("Sugar") ~ 1,
                                       TRUE ~ 0)) %>%
  mutate(compound.stoich.S = case_when(class %in% c("Sulfonium", "Sulfonate") ~ 1,
                                       TRUE ~ 0))  %>%
  group_by(Part.SampID) %>%
  mutate(Compound.N.nM = sum(Part.Conc.nM*compound.stoich.N),
         Compound.C.nM = sum(Part.Conc.nM*compound.stoich.C),
         Compound.S.nM = sum(Part.Conc.nM*compound.stoich.S)) %>%
  select(Part.SampID, Cruise, Part.Conc.nM, Part.C.nM, Part.N.nM, Part.S.nM, Compound.N.nM, Compound.C.nM, Compound.S.nM, Treatment) %>%
  unique() %>%
  group_by(Part.SampID) %>%
  mutate(Sum.Part.Conc.nM = sum(Part.Conc.nM, na.rm = TRUE),
         Sum.Part.C.nM = sum(Part.C.nM, na.rm = TRUE),
         Sum.Part.N.nM = sum(Part.N.nM, na.rm = TRUE),
         Sum.Part.S.nM = sum(Part.S.nM, na.rm = TRUE),
         Part.CN = Sum.Part.C.nM/Sum.Part.N.nM,
         Part.CS = Sum.Part.C.nM/Sum.Part.S.nM,
         Part.NS = Sum.Part.N.nM/Sum.Part.S.nM,
         Comp.C.nM = Compound.C.nM,
         Comp.N.nM = Compound.N.nM,
         Comp.S.nM = Compound.S.nM,
         Comp.CN = Compound.C.nM/Compound.N.nM,
         Comp.CS = Compound.C.nM/Compound.S.nM,
         Comp.NS = Compound.N.nM/Compound.S.nM) %>%
  mutate(Treatment = as.factor(Treatment),
         Treatment = fct_relevel(Treatment, c("Tote", "C", "P", "F", "PF", "NPF", "NP", "NF", "N")),
         N = as.factor(case_when(Treatment %in% c("N", "NP", "NF", "NPF") ~ 1,
                                 TRUE ~ 0)), 
         P = as.factor(case_when(Treatment %in% c("P", "NP", "PF", "NPF") ~ 1,
                                 TRUE ~ 0)), 
         Fe = as.factor(case_when(Treatment %in% c("F", "PF", "NF", "NPF") ~ 1,
                                  TRUE ~ 0))) %>%
  filter(!Treatment == "Tote") %>%
  select(Part.SampID, Treatment, N, P, Fe, Sum.Part.Conc.nM, Part.CN, Part.CS, Part.NS, Comp.C.nM, Comp.N.nM, Comp.S.nM, Comp.CN, Comp.CS, Comp.NS) %>%
  unique() %>%
  mutate(N_status = case_when(Treatment %in% c("N", "NP", "NF", "NPF") ~ "+N",
                              TRUE ~ "-N")) 



###Make compositional figure
perifix.comp.fig <- ggplot(dat.perifix, aes(y = Mean_nM, x = Treatment, fill = reorder(compound.name.figure, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.1, position = "fill") +
  scale_fill_manual(values = compound.pal.fig) +
  theme_test() +
  guides(fill=guide_legend(ncol=2)) +
  theme(legend.position = "right") +
  scale_y_continuous(expand = c(0,NA,NA,NA)) +
  ylab("Mole Fraction") +
  xlab("Treatment") +
  labs(fill = "Compound") 
perifix.comp.fig

ggsave(perifix.comp.fig, filename = "Figures/Outputs/PERIFIX_Composition_Figure.jpg", dpi = 600, height = 4, width = 8, scale = 1.2)



Perifix.N.figs




#Make Plot of Just C/N and N/S Stoichiometry:

####Make Perifix Figure
#___C/N Ratio
part.c.n.fig <- ggplot(dat.perifix.stoich, aes(x = N_status, y = Part.CN)) +
  geom_boxplot(aes(color = N_status), width = 0.5) +
  scale_color_manual(values = c("gray70", "gray10")) +
  geom_jitter(width = .1, height = 0, shape = 21, size = 3, stroke = 0.2, fill = "white", color = "black") +
  theme_test() +
  theme(legend.position = "none") +
  xlab("Treatment") +
  ylab("C/N Ratio")
#  ylim(0,17)
part.c.n.fig

#___C/N Ratio
comp.c.n.fig <- ggplot(dat.perifix.stoich, aes(x = N_status, y = Comp.CN)) +
  geom_boxplot(aes(color = N_status), width = 0.5) +
  scale_color_manual(values = c("gray70", "gray10")) +
  geom_jitter(width = .1, height = 0, shape = 21, size = 3, stroke = 0.2, fill = "white", color = "black") +
  theme_test() +
  theme(legend.position = "none") +
  xlab("Treatment") +
  ylab("C/N Ratio")
#  ylim(0,17)
comp.c.n.fig



####Make Perifix Figure
#___C/N Ratio
part.c.s.fig <- ggplot(dat.perifix.stoich, aes(x = N_status, y = Part.CS)) +
  geom_boxplot(aes(color = N_status), width = 0.5) +
  scale_color_manual(values = c("gray70", "gray10")) +
  geom_jitter(width = .1, height = 0, shape = 21, size = 3, stroke = 0.2, fill = "white", color = "black") +
  theme_test() +
  theme(legend.position = "none") +
  xlab("Treatment") +
  ylab("C/S Ratio")
#  ylim(0,17)
part.c.s.fig

#___C/N Ratio
comp.c.s.fig <- ggplot(dat.perifix.stoich, aes(x = N_status, y = Comp.CS)) +
  geom_boxplot(aes(color = N_status), width = 0.5) +
  scale_color_manual(values = c("gray70", "gray10")) +
  geom_jitter(width = .1, height = 0, shape = 21, size = 3, stroke = 0.2, fill = "white", color = "black") +
  theme_test() +
  theme(legend.position = "none") +
  xlab("Treatment") +
  ylab("C/S Ratio")
#  ylim(0,17)
comp.c.s.fig





####Make Perifix Figure
#___C/N Ratio
part.n.s.fig <- ggplot(dat.perifix.stoich, aes(x = N_status, y = Part.NS)) +
  geom_boxplot(aes(color = N_status), width = 0.5) +
  scale_color_manual(values = c("gray70", "gray10")) +
  geom_jitter(width = .1, height = 0, shape = 21, size = 3, stroke = 0.2, fill = "white", color = "black") +
  theme_test() +
  theme(legend.position = "none") +
  xlab("Treatment") +
  ylab("N/S Ratio")
#  ylim(0,17)
part.n.s.fig

#___C/N Ratio
comp.n.s.fig <- ggplot(dat.perifix.stoich, aes(x = N_status, y = Comp.NS)) +
  geom_boxplot(aes(color = N_status), width = 0.5) +
  scale_color_manual(values = c("gray70", "gray10")) +
  geom_jitter(width = .1, height = 0, shape = 21, size = 3, stroke = 0.2, fill = "white", color = "black") +
  theme_test() +
  theme(legend.position = "none") +
  xlab("Treatment") +
  ylab("N/S Ratio")
#  ylim(0,17)
comp.n.s.fig








# Open Ocean:

#Particulate Surface Data
env.stoich.dat <- dat %>%
  filter(Cruise %in% c("TN397", "KM1906", "RC078")) %>%
  filter(depth_m < 10) %>%
  filter(!station %in% c(1, 8)) %>%
  filter(!is.na(Part.SampID)) %>%
  mutate(compound.stoich.N = case_when(class %in% c("Betaine", "AA") ~ 1,
                                       compound.name.figure %in% c("Ectoine", "Hydroxyectoine", "TMAO", "Taurine") ~ 1,
                                       TRUE ~ 0)) %>%
  mutate(compound.stoich.C = case_when(class %in% c("Sugar") ~ 1,
                                       TRUE ~ 0)) %>%
  mutate(compound.stoich.S = case_when(class %in% c("Sulfonium", "Sulfonate") ~ 1,
                                       TRUE ~ 0))  %>%
  group_by(Part.SampID) %>%
  mutate(Compound.N.nM = sum(Part.Conc.nM*compound.stoich.N),
         Compound.C.nM = sum(Part.Conc.nM*compound.stoich.C),
         Compound.S.nM = sum(Part.Conc.nM*compound.stoich.S)) %>%
  select(Part.SampID, Cruise, Part.Conc.nM, Part.C.nM, Part.N.nM, Part.S.nM, Compound.N.nM, Compound.C.nM, Compound.S.nM, Region, Lat, Long) %>%
  unique() %>%
  group_by(Part.SampID) %>%
  mutate(Sum.Part.Conc.nM = sum(Part.Conc.nM, na.rm = TRUE),
         Sum.Part.C.nM = sum(Part.C.nM, na.rm = TRUE),
         Sum.Part.N.nM = sum(Part.N.nM, na.rm = TRUE),
         Sum.Part.S.nM = sum(Part.S.nM, na.rm = TRUE),
         Part.CN = Sum.Part.C.nM/Sum.Part.N.nM,
         Part.CS = Sum.Part.C.nM/Sum.Part.S.nM,
         Part.NS = Sum.Part.N.nM/Sum.Part.S.nM,
         Comp.C.nM = Compound.C.nM,
         Comp.N.nM = Compound.N.nM,
         Comp.S.nM = Compound.S.nM,
         Comp.CN = Compound.C.nM/Compound.N.nM,
         Comp.CS = Compound.C.nM/Compound.S.nM,
         Comp.NS = Compound.N.nM/Compound.S.nM) %>%
  select(Part.SampID, Cruise, Region, Lat, Long, Sum.Part.Conc.nM, Part.CN, Part.CS, Part.NS, Comp.C.nM, Comp.N.nM, Comp.S.nM, Comp.CN, Comp.CS, Comp.NS) %>%
  unique() %>%
  filter(!Region == "PS")



#Bulk osmolyte
ggplot(env.stoich.dat, aes(x = Lat, y = Part.CN, color = Region)) + 
  geom_point() +
  facet_wrap(.~Cruise, scales = "free")

ggplot(env.stoich.dat, aes(x = Region, y = Part.CN)) +
  geom_boxplot() +
  facet_wrap(.~Cruise, scales = "free")

ggplot(env.stoich.dat, aes(x = Lat, y = Part.CS, color = Region)) + 
  geom_point()

ggplot(env.stoich.dat, aes(x = Lat, y = Part.NS, color = Region)) + 
  geom_point() +
  facet_wrap(.~Cruise, scales = "free")

ggplot(env.stoich.dat, aes(x = Region, y = Part.NS)) +
  geom_boxplot() +
  facet_wrap(.~Cruise, scales = "free")

ggplot(env.stoich.dat, aes(x = Long, y = Part.NS, color = Region)) + 
  geom_point()

#compound
ggplot(env.stoich.dat, aes(x = Lat, y = Comp.CN, color = Region)) + 
  geom_point()

ggplot(env.stoich.dat, aes(x = Lat, y = Comp.CS, color = Region)) + 
  geom_point()

ggplot(env.stoich.dat, aes(x = Lat, y = Comp.NS, color = Region)) + 
  geom_point()










sulfonate.dat <- dat %>%
  filter(compound.name.figure %in% c("Isethionic acid", "Taurine")) %>%
  filter(Cruise %in% c("KM1906", "TN397", "RC078"))

ggplot(sulfonate.dat, aes(x = Part.Conc.nM, fill = Cruise)) +
  geom_histogram(bins = 60) +
  scale_x_log10() +
  facet_wrap(.~compound.name.figure) +
  geom_vline(xintercept = 0.4)





























