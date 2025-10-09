


#install.packages("randomForest")
install.packages("rsample")


library(tidyverse)
library(randomForest)
library(rsample)




###
diss.dat <- read_csv("Intermediates/Dissolved_Final_Quant_QCed.csv")

#### Dissolved data:
gbt.dat <- diss.dat %>% 
  filter(Compound == "Glycine betaine") %>%
  filter(Cruise %in% c("TN397", "RC078", "KinExp")) %>%
  filter(!str_detect(SampID, "KM1906")) %>%
  mutate(GBT = Diss.Conc.nM.adj) %>%
  select(Rep, GBT)

all.other.dat <- diss.dat %>% 
  filter(!Compound == "Glycine betaine") %>%
  filter(Cruise %in% c("TN397", "RC078", "KinExp")) %>%
  filter(!str_detect(SampID, "KM1906")) %>%
  select(Compound, Rep, Diss.Conc.nM.adj) %>%
  mutate(replace_na(Diss.Conc.nM.adj, 0)) %>%
  filter(!Compound %in% c("Arsenobetaine", "L-Isoleucine")) %>%
  mutate(Compound = str_remove_all(Compound, "-"),
         Compound = str_remove_all(Compound, " "),
         Compound = str_remove_all(Compound, "\\(|\\)"),
         Compound = str_remove_all(Compound, "[0-9]")) %>%
 # filter(Compound %in% c("Homarine", "Trigonelline", "Sarcosine")) %>%
  pivot_wider(id_cols = Rep, names_from = Compound, values_from = Diss.Conc.nM.adj) 


just.betaines <- all.other.dat <- diss.dat %>% 
  filter(Compound %in% c("Homarine", "beta-Alanine betaine", "Trigonelline", "Carnitine", "Proline betaine", "Dimethylsulfoniopropionate")) %>%
  filter(Cruise %in% c("TN397", "RC078", "KinExp")) %>%
  filter(!str_detect(SampID, "KM1906")) %>%
  select(Compound, Rep, Diss.Conc.nM.adj) %>%
  mutate(replace_na(Diss.Conc.nM.adj, 0)) %>%
  filter(!Compound %in% c("Arsenobetaine", "L-Isoleucine")) %>%
  mutate(Compound = str_remove_all(Compound, "-"),
         Compound = str_remove_all(Compound, " "),
         Compound = str_remove_all(Compound, "\\(|\\)"),
         Compound = str_remove_all(Compound, "[0-9]")) %>%
  # filter(Compound %in% c("Homarine", "Trigonelline", "Sarcosine")) %>%
  pivot_wider(id_cols = Rep, names_from = Compound, values_from = Diss.Conc.nM.adj) 




combine.dat <- left_join(gbt.dat, all.other.dat) %>%
  select(-Rep)



##Split data into train and test datasets:

set.seed(456)
data_split <- initial_split(combine.dat, prop = 0.7)

train <- training(data_split)
  
test <- testing(data_split)
  

#Train model:
rf_mod <- randomForest(GBT ~ ., data = train, ntree = 1000, mtry= 3)
rf_mod
  
plot(rf_mod) 
#

##Run prediction:
gbt_pred <- tibble(gbt.pred = predict(rf_mod, test)) 


test.pred <- cbind(test, gbt_pred)

ggplot(test.pred, aes(x = GBT, y = gbt.pred)) +
  geom_point()

ggplot(test.pred, aes(x = GBT, y = gbt.pred)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)

ggplot(test.pred, aes(x = GBT, y = gbt.pred)) +
  geom_point()

gbt.lm <- lm(GBT ~ gbt.pred, data = test.pred)
summary(gbt.lm)

x <- tibble(v_import = rf_mod[["importance"]])



### Predict beta-alanine betaine values:

#### Dissolved data:
Hom.dat <- diss.dat %>% 
  filter(Compound == "L-Glutamic acid") %>%
  filter(Cruise %in% c("TN397", "RC078", "KinExp")) %>%
  filter(!str_detect(SampID, "KM1906")) %>%
  mutate(Hom = Diss.Conc.nM.adj) %>%
  select(Rep, Hom)

all.other.dat <- diss.dat %>% 
  filter(!Compound == "L-Glutamic acid") %>%
  filter(!Compound == "Glycine betaine") %>%
  filter(Cruise %in% c("TN397", "RC078", "KinExp")) %>%
  filter(!str_detect(SampID, "KM1906")) %>%
  select(Compound, Rep, Diss.Conc.nM.adj) %>%
  mutate(replace_na(Diss.Conc.nM.adj, 0)) %>%
  filter(!Compound %in% c("Arsenobetaine", "L-Isoleucine")) %>%
  mutate(Compound = str_remove_all(Compound, "-"),
         Compound = str_remove_all(Compound, " "),
         Compound = str_remove_all(Compound, "\\(|\\)"),
         Compound = str_remove_all(Compound, "[0-9]")) %>%
  # filter(Compound %in% c("Homarine", "Trigonelline", "Sarcosine")) %>%
  pivot_wider(id_cols = Rep, names_from = Compound, values_from = Diss.Conc.nM.adj) 


# just.betaines <- all.other.dat <- diss.dat %>% 
#   filter(Compound %in% c("Homarine", "beta-Alanine betaine", "Trigonelline", "Carnitine", "Proline betaine", "Dimethylsulfoniopropionate")) %>%
#   filter(Cruise %in% c("TN397", "RC078", "KinExp")) %>%
#   filter(!str_detect(SampID, "KM1906")) %>%
#   select(Compound, Rep, Diss.Conc.nM.adj) %>%
#   mutate(replace_na(Diss.Conc.nM.adj, 0)) %>%
#   filter(!Compound %in% c("Arsenobetaine", "L-Isoleucine")) %>%
#   mutate(Compound = str_remove_all(Compound, "-"),
#          Compound = str_remove_all(Compound, " "),
#          Compound = str_remove_all(Compound, "\\(|\\)"),
#          Compound = str_remove_all(Compound, "[0-9]")) %>%
#   # filter(Compound %in% c("Homarine", "Trigonelline", "Sarcosine")) %>%
#   pivot_wider(id_cols = Rep, names_from = Compound, values_from = Diss.Conc.nM.adj) 




combine.dat <- left_join(Hom.dat, all.other.dat) %>%
  select(-Rep)



##Split data into train and test datasets:

set.seed(456)
data_split <- initial_split(combine.dat, prop = 0.7)

train <- training(data_split)

test <- testing(data_split)


#Train model:
rf_mod.hom <- randomForest(Hom ~ ., data = train, ntree = 1000, mtry= 5)
rf_mod.hom


x <- tibble(v_import = rf_mod.hom[["importance"]])

plot(rf_mod.hom) 
#



##Run prediction:
hom_pred <- tibble(hom.pred = predict(rf_mod.hom, test)) 


test.pred <- cbind(test, hom_pred)

ggplot(test.pred, aes(x = Hom, y = hom.pred)) +
  geom_point()

ggplot(test.pred, aes(x = Hom, y = hom.pred)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)

ggplot(test.pred, aes(x = Hom, y = hom.pred)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  geom_abline(intercept = 0, slope = 1) +
  ylab("Predicted dGlu") +
  xlab("Measured dGlu")
  

hom.lm <- lm(Hom ~ hom.pred, data = test.pred)
summary(hom.lm)

x <- tibble(v_import = rf_mod[["importance"]])




####Can you use it on Km1906 dataset:

#### Dissolved data:
Hom.dat.KM1906 <- diss.dat %>% 
  filter(Compound == "L-Glutamic acid") %>%
  filter(Cruise %in% c("KM1906")) %>%
#  filter(!str_detect(SampID, "KM1906")) %>%
  mutate(Hom = Diss.Conc.nM.adj) %>%
  select(Rep, Hom)

all.other.dat.KM1906 <- diss.dat %>% 
  filter(!Compound == "L-Glutamic acid") %>%
  filter(Cruise %in% c("KM1906")) %>%
#  filter(!str_detect(SampID, "KM1906")) %>%
  select(Compound, Rep, Diss.Conc.nM.adj) %>%
  mutate(replace_na(Diss.Conc.nM.adj, 0)) %>%
  filter(!Compound %in% c("Arsenobetaine", "L-Isoleucine")) %>%
  mutate(Compound = str_remove_all(Compound, "-"),
         Compound = str_remove_all(Compound, " "),
         Compound = str_remove_all(Compound, "\\(|\\)"),
         Compound = str_remove_all(Compound, "[0-9]")) %>%
  # filter(Compound %in% c("Homarine", "Trigonelline", "Sarcosine")) %>%
  pivot_wider(id_cols = Rep, names_from = Compound, values_from = Diss.Conc.nM.adj) 


###
KM1906.combine.dat <- left_join(Hom.dat.KM1906, all.other.dat.KM1906)

##Run prediction:
hom_pred <- tibble(hom.pred = predict(rf_mod.hom, all.other.dat.KM1906)) 


hom_predict_km1906 <- cbind(hom_pred, KM1906.combine.dat)




###Plot and examine fit:
ggplot(hom_predict_km1906, aes(x = Hom, y = hom.pred)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) 
  
ggplot(hom_predict_km1906, aes(x = Hom, y = hom.pred)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  geom_abline(intercept = 0, slope = 1)  +
  ylab("Predicted dGlu") +
  xlab("Measured dGlu")

####




###Make just dissolved G3 data for Elaina:
diss.dat.g3 <- diss.dat %>%
  filter(Cruise == "KM1906") %>%
  filter(!Compound == "Glycine betaine") %>%
  filter(!str_detect(Compound, "tentative")) %>%
  select(Rep, Cruise, Compound, Diss.Conc.nM.adj, LOD.Flag.2)



















#Examine correlation between GBT and Homarine
#gbt.dat <- diss.dat %>%
  filter(Compound == "Glycine betaine") %>%
  filter(Cruise %in% c("TN397", "RC078", "KinExp")) %>%
  filter(!str_detect(SampID, "KM1906")) %>%
  mutate(GBT_nM = Diss.Conc.nM.adj,
         log10_GBT_nM = log10(GBT_nM)) %>%
  select(Rep, Cruise, GBT_nM, log10_GBT_nM)
