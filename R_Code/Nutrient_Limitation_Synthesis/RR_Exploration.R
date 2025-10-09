



library(patchwork)


library(tidyverse)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")

##Define inputs:
part.file <- "Intermediates/Particulate_Quant_Output.csv"
meta.file <- "Meta_data/RR_MetaData.csv"


###load in data:
meta.dat <- read_csv(meta.file)

part.dat <- read_csv(part.file) %>%
  filter(Cruise == "RR") %>%
  left_join(., meta.dat) %>%
  filter(!is.na(Samp_Name)) %>%
  rename(Compound = Name) %>%
  filter(Compound %in% compound.order$Compound) %>%
  left_join(., compound.order)




###Summarize by bottle and treatment:
rr.sum.dat <- part.dat %>%
  group_by(SampID) %>%
  mutate(Tot.Conc.nM = sum(nM.in.smp, na.rm = TRUE),
         Tot.C.nM = sum(nM_C, na.rm = TRUE),
         Tot.N.nM = sum(nM_N, na.rm = TRUE),
         Tot.S.nM = sum(nM_S, na.rm = TRUE),
         C.N.ratio = Tot.C.nM/Tot.N.nM,
         C.S.ratio = Tot.C.nM/Tot.S.nM,
         N.S.ratio = Tot.N.nM/Tot.S.nM) %>%
  group_by(Experiment, Treatment) %>%
  mutate(Mean.Tot.Conc.nM = mean(Tot.Conc.nM),
         SD.Tot.Conc.nM = sd(Tot.Conc.nM),
         Mean.Tot.C.nM = mean(Tot.C.nM),
         SD.Tot.C.nM = sd(Tot.C.nM),
         Mean.Tot.N.nM = mean(Tot.N.nM),
         SD.Tot.N.nM = sd(Tot.N.nM),
         Mean.Tot.S.nM = mean(Tot.S.nM),
         SD.Tot.S.nM = sd(Tot.S.nM),
         Mean.C.N.ratio = mean(C.N.ratio),
         SD.C.N.ratio = sd(C.N.ratio),
         Mean.C.S.ratio = mean(C.S.ratio),
         SD.C.S.ratio = sd(C.S.ratio),
         Mean.N.S.ratio = mean(N.S.ratio),
         SD.N.S.ratio = sd(N.S.ratio))


###Orgnize Data for Plotting:
rr.treatment <- rr.sum.dat %>%
  select(Experiment, Treatment, Mean.Tot.Conc.nM, SD.Tot.Conc.nM, Mean.C.N.ratio, SD.C.N.ratio, Mean.C.S.ratio, SD.C.S.ratio, Mean.N.S.ratio, SD.N.S.ratio) %>%
  unique() 

rr.replicate <- rr.sum.dat %>%
  select(Samp_Name, Experiment, Treatment, Tot.Conc.nM, C.N.ratio, C.S.ratio, N.S.ratio) %>%
  unique()

rr.rep.compound <- rr.sum.dat %>%
  group_by(Compound, Experiment, Treatment) %>%
  summarize(Mean_nM = mean(nM.in.smp)) %>%
  left_join(., compound.order)



###Total Concentration:
ggplot(rr.treatment, aes(x = Treatment, y = Mean.Tot.Conc.nM)) +
  geom_col(width = 0.6, fill = "gray80", color = "black", size = 0.3) +
  geom_errorbar(aes(ymin = Mean.Tot.Conc.nM-SD.Tot.Conc.nM, ymax = Mean.Tot.Conc.nM+SD.Tot.Conc.nM), width = 0.1) +
  geom_point(data = rr.replicate, aes(x = Treatment, y = Tot.Conc.nM), shape = 21, fill = "white", stroke = 0.3, size = 3) +
  facet_wrap(.~Experiment, scales = "free_x") +
  ylab("Total Osmolyte Concentration (nM)") +
  theme_test()


#C/N ratio
ggplot(rr.treatment, aes(x = Treatment, y = Mean.C.N.ratio)) +
  geom_col(width = 0.6, fill = "gray80", color = "black", size = 0.3) +
  geom_errorbar(aes(ymin = Mean.C.N.ratio-SD.C.N.ratio, ymax = Mean.C.N.ratio+SD.C.N.ratio), width = 0.1) +
  geom_point(data = rr.replicate, aes(x = Treatment, y = C.N.ratio), shape = 21, fill = "white", stroke = 0.3, size = 3) +
  facet_wrap(.~Experiment, scales = "free_x") +
  ylab("Osmolyte C:N Ratio") +
  theme_test()


#C/S ratio
ggplot(rr.treatment, aes(x = Treatment, y = Mean.C.S.ratio)) +
  geom_col(width = 0.6, fill = "gray80", color = "black", size = 0.3) +
  geom_errorbar(aes(ymin = Mean.C.S.ratio-SD.C.S.ratio, ymax = Mean.C.S.ratio+SD.C.S.ratio), width = 0.1) +
  geom_point(data = rr.replicate, aes(x = Treatment, y = C.S.ratio), shape = 21, fill = "white", stroke = 0.3, size = 3) +
  facet_wrap(.~Experiment, scales = "free_x") +
  theme_test()


#N/S ratio
ggplot(rr.treatment, aes(x = Treatment, y = Mean.N.S.ratio)) +
  geom_col(width = 0.6, fill = "gray80", color = "black", size = 0.3) +
  geom_errorbar(aes(ymin = Mean.N.S.ratio-SD.N.S.ratio, ymax = Mean.N.S.ratio+SD.N.S.ratio), width = 0.1) +
  geom_point(data = rr.replicate, aes(x = Treatment, y = N.S.ratio), shape = 21, fill = "white", stroke = 0.3, size = 3) +
  facet_wrap(.~Experiment, scales = "free_x") +
  ylab("Osmolyte N:S Ratio") +
  theme_test()






###Total
RR.compound.plot <- ggplot(rr.rep.compound, aes(x = Treatment, y=Mean_nM, fill = reorder(Compound, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15) +
  scale_fill_manual(values = compound.pal)+
  theme_test() +
  facet_wrap(.~Experiment, scales = "free") +
  ylab("Mean nM Compound") +
  xlab("Treatment") +
  labs(fill = "Compound")
RR.compound.plot


###Fill
RR.compound.plot <- ggplot(rr.rep.compound, aes(x = Treatment, y=Mean_nM, fill = reorder(Compound, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15, position = "fill") +
  scale_fill_manual(values = compound.pal)+
  theme_test() +
  facet_wrap(.~Experiment, scales = "free") +
  ylab("Fraction") +
  xlab("Treatment") +
  labs(fill = "Compound")
RR.compound.plot



###Plot all in Total/N:S Ratio Space:
rr.sum.dat.sml <- rr.sum.dat %>%
  dplyr::select(Experiment, Treatment, Tot.Conc.nM, C.N.ratio, C.S.ratio, N.S.ratio) %>%
  unique()

ggplot(rr.sum.dat.sml, aes(x = N.S.ratio, y = Tot.Conc.nM)) +
  geom_point(aes(shape = as.factor(Experiment), fill = as.factor(Treatment), size = 2.5, stroke = 0.15)) +
  scale_shape_manual(values = c(21, 23, 25)) +
  scale_y_log10()







###Run stats on total concentration:

library(multcomp)
library(agricolae)

#Experiment 1
exp1.dat <- rr.sum.dat %>%
  filter(Experiment == 1) %>%
  ungroup() %>%
  dplyr::select(Treatment, Tot.Conc.nM, C.N.ratio, C.S.ratio, N.S.ratio) %>%
  filter(!Treatment == "T0") %>%
  unique() %>%
  mutate(Treatment = as.factor(Treatment))

#Total:
e1.anova.tot <- aov(Tot.Conc.nM ~ Treatment, data = exp1.dat)
summary(e1.anova.tot)

e1.tot.hsd.groups <- HSD.test(e1.anova.tot,
                            trt = "Treatment", 
                            alpha = 0.05,
                            console = TRUE)

#C:N
e1.anova.CN <- aov(C.N.ratio ~ Treatment, data = exp1.dat)
summary(e1.anova.CN)

e1.CN.hsd.groups <- HSD.test(e1.anova.CN,
                              trt = "Treatment", 
                              alpha = 0.05,
                              console = TRUE)

#N:S
e1.anova.NS <- aov(N.S.ratio ~ Treatment, data = exp1.dat)
summary(e1.anova.NS)

e1.NS.hsd.groups <- HSD.test(e1.anova.NS,
                             trt = "Treatment", 
                             alpha = 0.05,
                             console = TRUE)




####Experiment 2:
exp2.dat <- rr.sum.dat %>%
  filter(Experiment == 2) %>%
  ungroup() %>%
  dplyr::select(Treatment, Tot.Conc.nM, C.N.ratio, C.S.ratio, N.S.ratio) %>%
  filter(!Treatment == "T0") %>%
  unique() %>%
  mutate(Treatment = as.factor(Treatment))

#Total:
e2.anova.tot <- aov(Tot.Conc.nM ~ Treatment, data = exp2.dat)
summary(e2.anova.tot)

e2.tot.hsd.groups <- HSD.test(e2.anova.tot,
                              trt = "Treatment", 
                              alpha = 0.05,
                              console = TRUE)

#C:N
e2.anova.CN <- aov(C.N.ratio ~ Treatment, data = exp2.dat)
summary(e2.anova.CN)

e2.CN.hsd.groups <- HSD.test(e2.anova.CN,
                             trt = "Treatment", 
                             alpha = 0.05,
                             console = TRUE)

#N:S
e2.anova.NS <- aov(N.S.ratio ~ Treatment, data = exp2.dat)
summary(e1.anova.NS)

e2.NS.hsd.groups <- HSD.test(e2.anova.NS,
                             trt = "Treatment", 
                             alpha = 0.05,
                             console = TRUE)




####Experiment 3
exp3.dat <- rr.sum.dat %>%
  filter(Experiment == 3) %>%
  ungroup() %>%
  dplyr::select(Treatment, Tot.Conc.nM, C.N.ratio, C.S.ratio, N.S.ratio) %>%
  filter(!Treatment == "T0") %>%
  unique() %>%
  mutate(Treatment = as.factor(Treatment))

#Total:
e3.anova.tot <- aov(Tot.Conc.nM ~ Treatment, data = exp3.dat)
summary(e3.anova.tot)

e3.tot.hsd.groups <- HSD.test(e3.anova.tot,
                              trt = "Treatment", 
                              alpha = 0.05,
                              console = TRUE)

#C:N
e3.anova.CN <- aov(C.N.ratio ~ Treatment, data = exp3.dat)
summary(e3.anova.CN)

e3.CN.hsd.groups <- HSD.test(e3.anova.CN,
                             trt = "Treatment", 
                             alpha = 0.05,
                             console = TRUE)

#N:S
e3.anova.NS <- aov(N.S.ratio ~ Treatment, data = exp3.dat)
summary(e1.anova.NS)

e3.NS.hsd.groups <- HSD.test(e3.anova.NS,
                             trt = "Treatment", 
                             alpha = 0.05,
                             console = TRUE)













































