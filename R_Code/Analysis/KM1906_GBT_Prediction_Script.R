




diss.dat <- read_csv("Intermediates/Dissolved_Final_Quant_QCed.csv")


#Examine correlation between GBT and Homarine
gbt.dat <- diss.dat %>%
  filter(Compound == "Glycine betaine") %>%
  filter(Cruise %in% c("TN397", "RC078", "KinExp")) %>%
  filter(!str_detect(SampID, "KM1906")) %>%
  mutate(GBT_nM = Diss.Conc.nM.adj,
         log10_GBT_nM = log10(GBT_nM)) %>%
  select(Rep, Cruise, GBT_nM, log10_GBT_nM)

hom.dat <- diss.dat %>%
  filter(Compound == "Homarine") %>%
  filter(Cruise %in% c("TN397", "RC078", "KinExp")) %>%
  filter(!str_detect(SampID, "KM1906")) %>%
  mutate(Hom_nM = Diss.Conc.nM.adj,
         log10_Hom_nM = log10(Hom_nM)) %>%
  select(Rep, Cruise, Hom_nM, log10_Hom_nM)


comb.dat <- left_join(gbt.dat, hom.dat)

ggplot(comb.dat, aes(x = log10_Hom_nM, y = log10_GBT_nM)) +
  geom_smooth(method = "lm") +
  geom_point() 



#Make Linear Model to predict log10 GBT Conc from log10 Homarine Conc
hom.lin.mod <- lm(log10_GBT_nM ~ log10_Hom_nM, comb.dat) 
summary(hom.lin.mod)  


#Pull out coefficient for predicting GBT concentration
slope <- hom.lin.mod[["coefficients"]][["log10_Hom_nM"]]
intercept <- hom.lin.mod[["coefficients"]][["(Intercept)"]]


#Use linear model to predict GBT Concentrations in KM1906 dataset
km1906.prediction.dat <- diss.dat %>%
  filter(Compound == "Homarine") %>%
  filter(Cruise %in% c("KM1906")) %>%
  mutate(Hom_nM = Diss.Conc.nM.adj,
         log10_Hom_nM = log10(Hom_nM)) %>%
  mutate(log10_GBT_nM_pred = log10_Hom_nM*slope+intercept,
         GBT_nM_pred = 10^log10_GBT_nM_pred)


####save predictions:
write_csv(km1906.prediction.dat, "Intermediates/KM1906_Predicted_GBT_values.csv")

















