


#install.packages("randomForest")
#install.packages("rsample")


library(tidyverse)


###load in dissolved data:
diss.dat <- read_csv("Intermediates/Dissolved_Final_Quant_QCed.csv")



#pull out G3 data and remove incorrect GBT concentrations:
dat.G3 <- diss.dat %>%
  filter(Cruise %in% c("KM1906")) %>% 
  filter(!Compound == "Glycine betaine") %>%
  group_by(SampID) %>%
  filter(detected == "Yes") %>%
  mutate(sum.diss.conc = sum(Diss.Conc.nM)) %>%
  ungroup() 






#### calculate sum of dissolved data and match with GBT data:

#Determine GBT concentrations:
gbt.dat <- diss.dat %>% 
  filter(detected == "Yes") %>%
  filter(Compound == "Glycine betaine") %>%
  filter(Cruise %in% c("TN397", "RC078")) %>%
  filter(!str_detect(SampID, "KM1906")) %>%
  mutate(GBT = Diss.Conc.nM) %>%
  select(SampID, GBT)


#calculate the sum of all other compounds:
all.other.dat <- diss.dat %>% 
  filter(detected == "Yes") %>%
  filter(!Compound == "Glycine betaine") %>%
  filter(Cruise %in% c("TN397", "RC078")) %>%
  filter(!str_detect(SampID, "KM1906")) %>%
  select(Cruise, Compound, SampID, Diss.Conc.nM) 


combine.dat <- left_join(gbt.dat, all.other.dat) %>%
  group_by(SampID) %>%
  mutate(sum.diss.conc = sum(Diss.Conc.nM)) %>%
  select(-Compound, -Diss.Conc.nM) %>%
  unique()

#Visualize relationship between summed concentration and dissolved concentration

# Regular scaling 
ggplot(combine.dat, aes(x = sum.diss.conc, y = GBT)) +
  geom_point(aes(color = Cruise)) +
  geom_smooth(method = "lm", formula = y~poly(x, 2))



# Log10 Scaling
ggplot(combine.dat, aes(x = sum.diss.conc, y = GBT)) +
  geom_point(aes(color = Cruise)) +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = "lm", formula = y~poly(x, 2))



##Calculate polynomial regression models for predicting dGBT concentrations:

#regular 
lm.sum <- lm(formula = GBT ~ poly(sum.diss.conc, 2, raw = TRUE), data = combine.dat)
summary(lm.sum)


#log10 scaled
lm.sum.log <- lm(log10(GBT) ~ poly(log10(sum.diss.conc), 2, raw = TRUE), data = combine.dat)
summary(lm.sum.log)



#Calculate predicted GBT values and compare to actula GBT values:
combine.dat.predict <- combine.dat %>%
  ungroup() %>%
  mutate(GBT_log10 = log10(GBT),
         sum.diss.conc.log10 = log10(sum.diss.conc)) %>%
  mutate(GBT_predict = 1.381 + 1.482e-01*(sum.diss.conc) - -1.173e-05*(sum.diss.conc^2),
         GBT_predict_log10 = -1.22110 + 1.55206*(sum.diss.conc.log10) -0.15919*(sum.diss.conc.log10^2),
         GBT_predict_log10_convert = 10^GBT_predict_log10,
         Residual_reg_predict = GBT_predict - GBT,
         Residual_log10_predict = GBT_predict_log10_convert - GBT)


#Visualize Predictions:

#Regular scaled prediction:

#regular
ggplot(combine.dat.predict, aes(x = GBT, y = GBT_predict)) +
  geom_point(aes(color = Cruise)) +
  geom_abline(intercept = 0, slope = 1)

#log scaled
ggplot(combine.dat.predict, aes(x = GBT, y = GBT_predict)) +
  geom_point(aes(color = Cruise)) +
  geom_abline(intercept = 0, slope = 1) +
  scale_x_log10() +
  scale_y_log10()



#Log10 scaled prediction:

#regular
ggplot(combine.dat.predict, aes(x = GBT, y = GBT_predict_log10_convert)) +
  geom_point(aes(color = Cruise)) +
  geom_abline(intercept = 0, slope = 1) 

#log scaled
ggplot(combine.dat.predict, aes(x = GBT, y = GBT_predict_log10_convert)) +
  geom_point(aes(color = Cruise)) +
  geom_abline(intercept = 0, slope = 1) +
  scale_x_log10() +
  scale_y_log10()




##Compare distributions of residuals:

#regular
ggplot(combine.dat.predict, aes(x = sum.diss.conc, y = Residual_reg_predict)) +
  geom_point(aes(color = Cruise)) +
  scale_x_log10() +
  geom_hline(yintercept = 0)  

#log10
ggplot(combine.dat.predict, aes(x = sum.diss.conc, y = Residual_log10_predict)) +
  geom_point(aes(color = Cruise)) +
  scale_x_log10() +
  geom_hline(yintercept = 0) 


#Decide to use log10 approach as it has a more even distribution of residuals at lower concentrations 

#Predict G3 dissolved GBT concentrations from total concentration of dissolved metabolites:

g3.predict <- dat.G3 %>%
  select(Cruise, SampID, sum.diss.conc) %>%
  unique() %>%
  mutate(sum.diss.conc.log10 = log10(sum.diss.conc),
         gbt.predict.log10 = -1.22110 + 1.55206*(sum.diss.conc.log10) -0.15919*(sum.diss.conc.log10^2),
         gbt_predict_convert = 10^gbt.predict.log10)
  


#Export G3 predictions to incorporate into larger dissolved dataset: 
g3.predict.export <- g3.predict %>%
  select(Cruise, SampID, gbt_predict_convert) %>%
  rename(Diss.Conc.nM = gbt_predict_convert) %>%
  mutate(Compound = "Glycine betaine",
         detected = "Yes",
         Diss.Conc.Vial.uM = NA,
         Diss.Conc.C.nM = 5*Diss.Conc.nM,
         Diss.Conc.N.nM = Diss.Conc.nM,
         Diss.Conc.S.nM = NA,
         impute.conc.nM = NA,
         LOD.nM = NA,
         Diss.Conc.no.blk.sub.nM = NA,
         LOD.no.blk.sub.nM = NA,
         Predicted_Value = "Yes")

write_csv(g3.predict.export, file = "Intermediates/KM1906_Predicted_Dissolved_GBT_Concentrations.csv")






