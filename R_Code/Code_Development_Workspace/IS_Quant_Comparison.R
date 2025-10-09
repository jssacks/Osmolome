


library(viridis)



std.quant <- read_csv("Intermediates/Part_Vial_Quant_Dat.csv") %>%
  select(Name, SampID, Cruise, umol.in.vial.ave) %>%
  rename(std_quant_uM = umol.in.vial.ave)



is.quant <- read_csv("Intermediates/Part_IS_vial_quant.csv") %>%
  select(Name, Cruise, SampID, umol.in.vial.ave) %>%
  rename(IS_quant_uM = umol.in.vial.ave)



##combine data:
comparison.dat <- is.quant %>%
  left_join(., std.quant)


is.ratio.dat <- comparison.dat %>%
  ungroup() %>%
  mutate(is.ratio = IS_quant_uM/std_quant_uM) %>%
  select(Name, Cruise, is.ratio) %>%
  unique() %>%
  group_by(Name, Cruise) %>%
  reframe(is.ratio = mean(is.ratio),
          log2.is.ratio = log2(is.ratio))

ggplot(is.ratio.dat, aes(x = Cruise, y = Name, fill = log2.is.ratio)) +
  geom_tile() + 
  geom_text(aes(label = is.ratio)) +
  scale_fill_viridis()





###Arsenobetaine:
abt.dat <- comparison.dat %>%
  filter(Name == "Arsenobetaine")

ggplot(abt.dat, aes(x = std_quant_uM, y = IS_quant_uM)) +
  geom_point(aes(color = Cruise)) +
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap(Cruise ~ Name, scales = "free")




###Glycine betiane
gbt.dat <- comparison.dat %>%
  filter(Name == "Glycine betaine")

ggplot(gbt.dat, aes(x = std_quant_uM, y = IS_quant_uM)) +
  geom_point(aes(color = Cruise)) +
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap(Cruise ~ Name, scales = "free")





###Homarine
hom.dat <- comparison.dat %>%
  filter(Name == "Homarine")

ggplot(hom.dat, aes(x = std_quant_uM, y = IS_quant_uM)) +
  geom_point(aes(color = Cruise)) +
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap(Cruise ~ Name, scales = "free")



###isethionic acid
ise.dat <- comparison.dat %>%
  filter(Name == "Isethionic acid")

ggplot(ise.dat, aes(x = std_quant_uM, y = IS_quant_uM)) +
  geom_point(aes(color = Cruise)) +
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap(Cruise ~ Name, scales = "free")


###alanine
ala.dat <- comparison.dat %>%
  filter(Name == "L-Alanine")

ggplot(ala.dat, aes(x = std_quant_uM, y = IS_quant_uM)) +
  geom_point(aes(color = Cruise)) +
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap(Cruise ~ Name, scales = "free")



###arginine
arg.dat <- comparison.dat %>%
  filter(Name == "L-Arginine")

ggplot(arg.dat, aes(x = std_quant_uM, y = IS_quant_uM)) +
  geom_point(aes(color = Cruise)) +
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap(Cruise ~ Name, scales = "free")


###isoleucine
iso.dat <- comparison.dat %>%
  filter(Name == "L-Isoleucine")

ggplot(iso.dat, aes(x = std_quant_uM, y = IS_quant_uM)) +
  geom_point(aes(color = Cruise)) +
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap(Cruise ~ Name, scales = "free")


###Methionine
met.dat <- comparison.dat %>%
  filter(Name == "L-Methionine")

ggplot(met.dat, aes(x = std_quant_uM, y = IS_quant_uM)) +
  geom_point(aes(color = Cruise)) +
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap(Cruise ~ Name, scales = "free")

##Proline
pro.dat <- comparison.dat %>%
  filter(Name == "L-Proline")

ggplot(pro.dat, aes(x = std_quant_uM, y = IS_quant_uM)) +
  geom_point(aes(color = Cruise)) +
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap(Cruise ~ Name, scales = "free")




###Sucrose
suc.dat <- comparison.dat %>%
  filter(Name == "Sucrose")

ggplot(suc.dat, aes(x = std_quant_uM, y = IS_quant_uM)) +
  geom_point(aes(color = Cruise)) +
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap(Cruise ~ Name, scales = "free")

###Taurine
tau.dat <- comparison.dat %>%
  filter(Name == "Taurine")

ggplot(tau.dat, aes(x = std_quant_uM, y = IS_quant_uM)) +
  geom_point(aes(color = Cruise)) +
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap(Cruise ~ Name, scales = "free")



###Trehalose
tre.dat <- comparison.dat %>%
  filter(Name == "Trehalose")

ggplot(tre.dat, aes(x = std_quant_uM, y = IS_quant_uM)) +
  geom_point(aes(color = Cruise)) +
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap(Cruise ~ Name, scales = "free")

###TMAO
tmao.dat <- comparison.dat %>%
  filter(Name == "Trimethylamine N-oxide")

ggplot(tmao.dat, aes(x = std_quant_uM, y = IS_quant_uM)) +
  geom_point(aes(color = Cruise)) +
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap(Cruise ~ Name, scales = "free")











#G4.dat 
g4.comp.dat <- comparison.dat %>%
  filter(Cruise == "TN397")

ggplot(g4.comp.dat, aes(x = std_quant_uM, y = IS_quant_uM)) +
  geom_point(aes(color = Name)) +
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap(. ~ Name, scales = "free")



#G4.dat 
g3.comp.dat <- comparison.dat %>%
  filter(Cruise == "KM1906")

ggplot(g3.comp.dat, aes(x = std_quant_uM, y = IS_quant_uM)) +
  geom_point(aes(color = Name)) +
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap(. ~ Name, scales = "free")


#G4.dat 
d1.comp.dat <- comparison.dat %>%
  filter(Cruise == "RC078")

ggplot(d1.comp.dat, aes(x = std_quant_uM, y = IS_quant_uM)) +
  geom_point(aes(color = Name)) +
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap(. ~ Name, scales = "free")















































