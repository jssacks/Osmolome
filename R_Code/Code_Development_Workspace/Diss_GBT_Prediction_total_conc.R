


#install.packages("randomForest")
install.packages("rsample")


library(tidyverse)


###
diss.dat <- read_csv("Intermediates/Dissolved_Final_Quant_QCed.csv")




dat.G3 <- diss.dat %>%
  filter(Cruise %in% c("KM1906")) %>% 
  filter(!Compound == "Glycine betaine") %>%
  group_by(Rep) %>%
  mutate(sum.diss.conc = sum(Diss.Conc.nM.adj)) %>%
  ungroup() %>%
  summarize(max.conc = max(sum.diss.conc),
            min.conc = min(sum.diss.conc))




#### Dissolved data:
gbt.dat <- diss.dat %>% 
  filter(Compound == "Glycine betaine") %>%
  filter(Cruise %in% c("TN397", "RC078")) %>%
  filter(!str_detect(SampID, "KM1906")) %>%
  mutate(GBT = Diss.Conc.nM.adj) %>%
  select(Rep, GBT)

all.other.dat <- diss.dat %>% 
  filter(!Compound == "Glycine betaine") %>%
  filter(Cruise %in% c("TN397", "RC078")) %>%
  filter(!str_detect(SampID, "KM1906")) %>%
  select(Compound, Rep, Diss.Conc.nM.adj) %>%
  mutate(Diss.Conc.nM.adj = replace_na(Diss.Conc.nM.adj, 0)) %>%
  filter(!Compound %in% c("Arsenobetaine", "L-Isoleucine")) %>%
  mutate(Compound = str_remove_all(Compound, "-"),
         Compound = str_remove_all(Compound, " "),
         Compound = str_remove_all(Compound, "\\(|\\)"),
         Compound = str_remove_all(Compound, "[0-9]"))

combine.dat <- left_join(gbt.dat, all.other.dat) %>%
  group_by(Rep) %>%
  mutate(sum.diss.conc = sum(Diss.Conc.nM.adj)) %>%
  select(-Compound, -Diss.Conc.nM.adj) %>%
  unique()


#Regular
ggplot(combine.dat, aes(x = sum.diss.conc, y = GBT)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y~poly(x, 2))

#Log Scaled
ggplot(combine.dat, aes(x = sum.diss.conc, y = GBT)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = "lm", formula = y~poly(x, 2))

lm.sum <- lm(formula = GBT ~ poly(sum.diss.conc, 2, raw = TRUE), data = combine.dat)
summary(lm.sum)

x 

lm.sum.log <- lm(log10(GBT) ~ poly(log10(sum.diss.conc), 2, raw = TRUE), data = combine.dat)
summary(lm.sum.log)




#GBT_Predict_Dat:
combine.dat.predict <- combine.dat %>%
  ungroup() %>%
  mutate(GBT_log10 = log10(GBT),
         sum.diss.conc.log10 = log10(sum.diss.conc)) %>%
  mutate(GBT_predict = 3.322e-01 + 1.743e-01*(sum.diss.conc) - -3.104e-05*(sum.diss.conc^2),
         GBT_predict_log10 = -2.0327 + 2.3652*(sum.diss.conc.log10) -0.3528*(sum.diss.conc.log10^2),
         GBT_predict_log10_convert = 10^GBT_predict_log10)

ggplot(combine.dat.predict, aes(x = GBT, y = GBT_predict)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)

ggplot(combine.dat.predict, aes(x = GBT, y = GBT_predict)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  scale_x_log10() +
  scale_y_log10()

lm.gbt.predict.reg <- lm(GBT_predict ~ GBT, dat = combine.dat.predict)
summary(lm.gbt.predict.reg)

ggplot(combine.dat.predict, aes(x = GBT_log10, y = GBT_predict_log10)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)

lm.gbt.predict.log <- lm(GBT_predict_log10 ~ GBT_log10, dat = combine.dat.predict)
summary(lm.gbt.predict.log)




ggplot(combine.dat.predict, aes(x = GBT, y = GBT_predict_log10_convert)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  scale_x_log10() +
  scale_y_log10()
  


