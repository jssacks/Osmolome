
library(tidyverse)
library(patchwork)



###load in data and combine with metadata
part.dat <- read_csv(part.file)
meta.dat <- read_csv(meta.file)


####
dat.all <- left_join(part.dat, meta.dat) %>%
  rename("Compound" = Name)


surface.dat <- dat.all %>%
  filter(Cruise %in% c("TN397", "KM1906", "RC078", "PERIFIX")) %>%
  filter(Compound %in% compound.order$Compound) %>%
  left_join(., compound.order) %>%
  mutate(Local_Time = hms(Local_Time),
         hour_of_day = hour(Local_Time)) %>%
  mutate(tod = as.factor(case_when(hour_of_day > 4 & hour_of_day <= 10 ~ "morning",
                                   hour_of_day > 10 & hour_of_day <= 16 ~ "midday",
                                   hour_of_day > 16 & hour_of_day <= 21 ~ "evening",
                                   hour_of_day > 21 & hour_of_day <= 25 ~ "night"))) %>%
  filter(!is.na(tod)) %>%
  mutate(tod = fct_relevel(tod, c("morning", "midday", "evening", "night")))


stoich.dat <- surface.dat %>%
  filter(!compound.name.figure %in% c("DMSP")) %>%
  group_by(SampID) %>%
  mutate(C.tot = sum(nM_C, na.rm = TRUE),
         N.tot = sum(nM_N, na.rm = TRUE),
         S.tot = sum(nM_S, na.rm = TRUE)) %>%
  mutate(C.N.tot = C.tot/N.tot,
         C.S.tot = C.tot/S.tot,
         N.S.tot = N.tot/S.tot) %>%
  mutate(Group_Carbon = case_when(class %in% c("Sugar") ~ "Yes",
                                  TRUE ~ "No"),
         Group_Nitrogen = case_when(class %in% c("AA", "Betaine", "TMAO", "Taurine") ~ "Yes",
                                    TRUE ~ "No"),
         Group_Sulfur = case_when(class %in% c("Sulfonium", "Sulfonate", "Taurine") ~ "Yes",
                                  TRUE ~ "No"))  %>%
  mutate(C.comp = sum(nM.in.smp[Group_Carbon == "Yes"]),
         N.comp = sum(nM.in.smp[Group_Nitrogen == "Yes"]),
         S.comp = sum(nM.in.smp[Group_Sulfur == "Yes"]),
         C.N.comp = C.comp/N.comp,
         C.S.comp = C.comp/S.comp,
         N.S.comp = N.comp/S.comp) %>%
  group_by(SampID) %>%
  mutate(tot.osmo.nM = sum(nM.in.smp)) %>%
  ungroup()


stoich.dat.sum <- stoich.dat %>%
  filter(Cruise %in% c("TN397", "KM1906", "RC078")) %>%
  select(SampID, Cruise, C.tot, N.tot, S.tot, C.S.tot, N.S.tot) %>%
  unique() %>%
  reframe(mean.CS = mean(C.S.tot),
          mean.NS = mean(N.S.tot))



#####Gradients:
g.stoich <- stoich.dat %>%
  filter(Cruise %in% c("TN397", "KM1906")) %>%
  filter(!SampID == "220902_Smp_TN397_S11_600_U_C") %>%
  mutate(Region = case_when(Cruise == "RC078" ~ "PS",
                            Lat > 36 ~ "NPTZ",
                            Lat < 7 ~ "Equator",
                            Cruise == "TN397" & Lat > 29 ~ "CC",
                            TRUE ~ "NPSG")) %>%
  select(Region, tot.osmo.nM, N.S.tot) %>%
  unique() %>%
  filter(!is.na(tot.osmo.nM)) %>%
  group_by(Region)  %>%
  reframe(Mean.Osmo.nM = mean(tot.osmo.nM),
          SD.Osmo.nM = sd(tot.osmo.nM),
          SE.Osmo.nM = SD.Osmo.nM/n(),
          Median.Osmo.nM = median(tot.osmo.nM),
          Mean.NS = mean(N.S.tot),
          Median.NS = median(N.S.tot),
          SD.NS = sd(N.S.tot),
          SE.NS = SD.NS/n())

          











#Make PERIFIX Plots:
peri.dat <- stoich.dat %>%
  filter(Cruise %in% c("PERIFIX")) %>%
  dplyr::select(SampID, Cruise, Lat, Long, Local_Date, Local_Time, depth_m, tod, Treatment,
                C.tot, N.tot, S.tot, C.N.tot, C.S.tot, N.S.tot, 
                C.comp, N.comp, S.comp, C.N.comp, C.S.comp, N.S.comp) %>%
  unique() %>%
  mutate(Treatment = as.factor(Treatment),
         Treatment = fct_relevel(Treatment, c("Tote", "C", "P", "F", "PF", "NPF", "NP", "NF", "N")),
         N_status = case_when(Treatment %in% c("N", "NP", "NF", "NPF") ~ "+N",
                              TRUE ~ "-N")) %>%
  filter(!Treatment == "Tote")

peri.dat.sum <- peri.dat %>%
  group_by(N_status) %>%
  reframe(max.ns = max(N.S.tot),
          min.ns = min(N.S.tot))

#___C/N Ratio
c.n.p.fig <- ggplot(peri.dat, aes(x = N_status, y = C.N.tot)) +
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
n.s.p.fig <- ggplot(peri.dat, aes(x = N_status, y = N.S.tot)) +
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
ocean.stoich.plot <- ggplot(g.stoich, aes(x = Mean.NS, y = Mean.Osmo.nM, fill = Region)) +
  geom_errorbar(aes(x = Mean.NS, ymin = Mean.Osmo.nM-SD.Osmo.nM, ymax = Mean.Osmo.nM+SD.Osmo.nM), color = "gray80") +
  geom_errorbarh(aes(y = Mean.Osmo.nM, xmin = Mean.NS-SD.NS, xmax = Mean.NS+SD.NS), height = 0.5, color = "gray80") +
  geom_point(shape = 21, size = 5, stroke = 0.15, color = "black") +
  ylim(0, 32) +
  xlim(0.5, 5.25) +
  scale_fill_manual(values = region.palette) +
  theme_test() +
  theme(legend.position = "inside", legend.position.inside = c(0.8, 0.8)) +
  xlab("N/S Ratio") +
  ylab("Total Osmolyte Concentration (nM)") 

ocean.stoich.plot




library(patchwork)

###Make Full Plot:
Stoich.fig <- (c.n.p.fig / n.s.p.fig | ocean.stoich.plot)  +
  plot_layout(widths = c(1,3)) +
  plot_annotation(tag_levels = "A")

Stoich.fig


ggsave(Stoich.fig, file = "R_Code/Code_Development_Workspace/MT_Fig3_Stoich.pdf", height = 4.5, width = 5.25, scale = 1.2)














#####Puget Sound:
d.stoich <- stoich.dat %>%
  filter(Cruise %in% c("RC078")) %>%
  filter(!SampID == "220902_Smp_TN397_S11_600_U_C") %>%
  filter(depth_m < 10) %>%
  mutate(Region = case_when(Cruise == "RC078" ~ "PS",
                            Lat > 36 ~ "NPTZ",
                            Lat < 7 ~ "Equator",
                            Cruise == "TN397" & Lat > 29 ~ "CC",
                            TRUE ~ "NPSG")) %>%
  select(Region, station, tot.osmo.nM, N.S.tot) %>%
  unique() %>%
  filter(station %in% c(2,3,5,6,7)) %>%
  filter(!is.na(tot.osmo.nM)) %>%
  group_by(station)  %>%
  reframe(Mean.Osmo.nM = mean(tot.osmo.nM),
          SD.Osmo.nM = sd(tot.osmo.nM),
          SE.Osmo.nM = SD.Osmo.nM/n(),
          Median.Osmo.nM = median(tot.osmo.nM),
          Mean.NS = mean(N.S.tot),
          Median.NS = median(N.S.tot),
          SD.NS = sd(N.S.tot),
          SE.NS = SD.NS/n())


PS.stoich.plot <- ggplot(d.stoich, aes(x = Mean.NS, y = Mean.Osmo.nM, fill = as.factor(station))) +
  geom_errorbar(aes(x = Mean.NS, ymin = Mean.Osmo.nM-SD.Osmo.nM, ymax = Mean.Osmo.nM+SD.Osmo.nM), color = "gray80", width = 0.1) +
  geom_errorbarh(aes(y = Mean.Osmo.nM, xmin = Mean.NS-SD.NS, xmax = Mean.NS+SD.NS), height = 10, color = "gray80") +
  geom_point(shape = 21, size = 5, stroke = 0.15, color = "black") +
#  ylim(0, 32) +
#  xlim(0.5, 5.25) +
#  scale_fill_manual(values = region.palette) +
  theme_test() +
  xlab("N/S Ratio") +
  ylab("Total Osmolyte Concentration (nM)")
PS.stoich.plot






#stoich plot with mean and SE
ggplot(g.stoich, aes(x = Mean.NS, y = Mean.Osmo.nM, fill = Region)) +
  geom_errorbar(aes(x = Mean.NS, ymin = Mean.Osmo.nM-SE.Osmo.nM, ymax = Mean.Osmo.nM+SE.Osmo.nM)) +
  geom_errorbarh(aes(y = Mean.Osmo.nM, xmin = Mean.NS-SE.NS, xmax = Mean.NS+SE.NS), height = 0.5) +
  geom_point(shape = 21, size = 5, stroke = 0.1) +
  ylim(0, 35) +
  xlim(0, 5.5)





g.quantiles <- stoich.dat %>%
  filter(Cruise %in% c("TN397", "KM1906")) %>%
  filter(!SampID == "220902_Smp_TN397_S11_600_U_C") %>%
  mutate(Region = case_when(Cruise == "RC078" ~ "PS",
                            Lat > 36 ~ "NPTZ",
                            Lat < 7 ~ "Equator",
                            Cruise == "TN397" & Lat > 29 ~ "CC",
                            TRUE ~ "NPSG")) %>%
  select(Region, tot.osmo.nM, N.S.tot) %>%
  unique() %>%
  filter(!is.na(tot.osmo.nM)) %>%
  group_by(Region) %>%
  reframe(tot.osmo.nM = quantile(tot.osmo.nM, c(0.25, 0.5, 0.75)), q = c(0.25, 0.5, 0.75),
          NS = quantile(N.S.tot, c(0.25, 0.5, 0.75)), q = c(0.25, 0.5, 0.75)) %>%
  pivot_longer(cols = c(tot.osmo.nM, NS), names_to = "param", values_to = "value") %>%
  unite("param_q", c("param", "q")) %>%
  pivot_wider(id_cols = Region, names_from = param_q, values_from = value)




#stoich plot with median
ggplot(g.quantiles, aes(fill = Region)) +
  geom_errorbar(aes(x = NS_0.5, ymin = tot.osmo.nM_0.25, ymax = tot.osmo.nM_0.75)) +
  geom_errorbarh(aes(y = tot.osmo.nM_0.5, xmin = NS_0.25, xmax = NS_0.75), height = 0.5) +
  geom_point(aes(x = NS_0.5, y = tot.osmo.nM_0.5), shape = 21, size = 5, stroke = 0.1) +
  ylim(0, 30) +
  xlim(0, 5)

