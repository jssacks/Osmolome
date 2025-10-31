
library(tidyverse)
library(patchwork)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")





###define inputs
enviro.file <- "Intermediates/Enviro_Osmo_Final_Dataset_with_metadata.csv"
peri.file <- "Intermediates/PERIFIX_Osmo_Meta_Dat_101325.csv"



#Read in data 
dat <- read_csv(enviro.file) %>%
  left_join(., compound.order) %>%
  filter(!str_detect(Part.SampID, "Blk")) %>%
  filter(!Part.SampID == "220902_Smp_TN397_S11_600_U_C")


#Define regions:
dat.region <- dat %>%
  mutate(Region = case_when(Cruise == "KM1906" & Lat >= 35 ~ "NPTZ",
                            Cruise == "KM1906" & Lat < 35 ~ "NPSG",
                            Cruise == "TN397" & Long > -122 ~ "CUCP",
                            Cruise == "TN397" & Long < -122 & Lat > 10.5 ~ "NPSG",
                            Cruise == "TN397" & Lat < 10.5 & Lat > 4.5 ~ "NPEC",
                            Cruise == "TN397" & Lat < 4.5 ~ "PEDP",
                            Cruise == "RC078" ~ "SS"))


##Pull out gradients data and calculate average sample values of each osmolyte class:
dat.gradients <- dat.region %>%
  filter(Cruise %in% c("KM1906", "TN397")) %>%
  filter(!Region == "CUCP") %>%
  filter(Part.detected == "Yes") %>%
  group_by(Part.SampID, Cruise, Lat, Long, Region, class) %>%
  reframe(sum.part.conc = sum(Part.Conc.nM, na.rm = TRUE)) %>%
  pivot_wider(id_cols = c(Part.SampID, Lat, Long, Region, Cruise), names_from = class, values_from = sum.part.conc) %>%
  mutate(Sugar_N_ratio = Sugar/(AA+Betaine),
         Sulfonate_N_ratio = Sulfonate/(AA+Betaine),
         Sulfonium_N_ratio = Sulfonium/(AA+Betaine)) %>%
  mutate(Cruise = as.factor(Cruise),
         Region = as.factor(Region)) %>%
  mutate(Cruise = fct_relevel(Cruise, c("TN397", "KM1906")))


###Perform Welch's ANOVAs and Games-Howell Post-Hoc Tests on each ratio:


#Sugar_N:
sug.gradients.w.anova <- dat.gradients %>%
  group_by(Cruise) %>%
  welch_anova_test(Sugar_N_ratio ~ Region)

sug.gradients.gh.test <- dat.gradients %>%
  group_by(Cruise) %>%
  games_howell_test(Sugar_N_ratio ~ Region)


#Sulfonate_N
sulfonate.gradients.w.anova <- dat.gradients %>%
  group_by(Cruise) %>%
  welch_anova_test(Sulfonate_N_ratio ~ Region)

sulfonate.gradients.gh.test <- dat.gradients %>%
  group_by(Cruise) %>%
  games_howell_test(Sulfonate_N_ratio ~ Region)


#Sulfonium_N
sulfonium.gradients.w.anova <- dat.gradients %>%
  group_by(Cruise) %>%
  welch_anova_test(Sulfonium_N_ratio ~ Region)

sulfonium.gradients.gh.test <- dat.gradients %>%
  group_by(Cruise) %>%
  games_howell_test(Sulfonium_N_ratio ~ Region)




############

#Load in PERIFIX Data:
peri.dat <- read_csv(peri.file)%>%
  left_join(., compound.order) %>%
  filter(!str_detect(Part.SampID, "Blk")) %>%
  filter(!is.na(compound.name.figure))

dat.peri.stat <- peri.dat %>%
  mutate(Part.Conc.nM = case_when(Part.detected == "No" ~ Part.Impute.Conc.nM,
                                  TRUE ~ Part.Conc.nM)) %>%
  group_by(Part.SampID, Treatment, N_status, class, N, P, Fe) %>%
  reframe(sum.part.conc = sum(Part.Conc.nM, na.rm = TRUE)) %>%
  pivot_wider(id_cols = c(Part.SampID, Treatment, N_status, N, P, Fe), names_from = class, values_from = sum.part.conc) %>%
  mutate(Sugar_N_ratio = Sugar/(AA+Betaine),
         Sulfonate_N_ratio = Sulfonate/(AA+Betaine),
         Sulfonium_N_ratio = Sulfonium/(AA+Betaine)) %>%
  mutate(Treatment = as.factor(Treatment),
         N_status = as.factor(N_status),
         N = as.factor(N),
         P = as.factor(P),
         Fe = as.factor(Fe)) %>%
  filter(!Treatment == "Tote")
         

ggplot(dat.peri.stat, aes(x = reorder(Treatment, N), y = Sugar_N_ratio, color = Treatment)) +
  geom_boxplot() +
  geom_point()

ggplot(dat.peri.stat, aes(x = reorder(Treatment, N), y = Sulfonate_N_ratio, color = Treatment)) +
  geom_boxplot() +
  geom_point()

ggplot(dat.peri.stat, aes(x = reorder(Treatment, N), y = Sulfonium_N_ratio, color = Treatment)) +
  geom_boxplot() +
  geom_point()



#Try linear mixed effects model:
#install.packages("lmerTest")

library(lme4)     
library(lmerTest)

###
mod1 <- lmer(Sugar_N_ratio ~ N + (1|P) + (1|Fe), data = dat.peri.stat)
summary(mod1)


mod2 <- lmer(Sulfonate_N_ratio ~ N + (1|P) + (1|Fe), data = dat.peri.stat)
summary(mod2)


mod3 <- lmer(Sulfonium_N_ratio ~ N + (1|P) + (1|Fe), data = dat.peri.stat)
summary(mod3)
###G3 - Welch's ANOVA (basically just a t-test)






###G4 - Welch's ANOVA

#define dataset:
g4.dat <- dat.gradients %>%
  filter(Cruise == "TN397")

#compare Sug_N_ratio:
g4.sug_N.anova <- aov(N_Sugar_ratio ~ Region, data = g4.dat)
summary(g4.sug_N.anova)


welch.g4.sug <- dat.gradients %>%
  group_by(Cruise) %>%
  welch_anova_test(N_Sulfonium_ratio ~ Region)


gh.g4.sug <- dat.gradients %>%
  group_by(Cruise) %>%
  games_howell_test(N_Sulfonium_ratio ~ Region)


g4.sug_N.anova <- oneway.test(N_Sugar_ratio ~ Region, data = g4.dat, var.equal = FALSE)
g4.sug_N.anova

#post_hoc test:
phoc <- g4.sug_N.anova <- TukeyHSD(g4.sug_N.anova)



  
  
  

peri.anova.C.N.comp <- aov(Comp.CN ~ N*P*Fe, data = dat.perifix.stoich)  
summary(peri.anova.C.N.comp)




###PERIFIX - Welch's ANOVA


