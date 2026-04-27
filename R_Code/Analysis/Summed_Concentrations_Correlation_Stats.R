

library(tidyverse)
library(rstatix)





###______________define inputs____________________

#Osmolytes and enviro data:
all.dat.file <- "Intermediates/Enviro_Osmo_Final_Dataset_with_metadata.csv"

#atp 
atp.file <- "Intermediates/ATP_G3_G4_data.csv"






#_____________Read in data________________________ 
dat <- read_csv(all.dat.file) %>%
  left_join(., compound.order) %>%
  filter(!str_detect(Part.SampID, "Blk")) %>%
  filter(!is.na(class))

#Define regions:
dat.region <- dat %>%
  mutate(Region = case_when(Cruise == "KM1906" & Lat >= 35 ~ "NPTZ",
                            Cruise == "KM1906" & Lat < 35 ~ "NPSG",
                            Cruise == "TN397" & Long > -122 ~ "CUCP",
                            Cruise == "TN397" & Long < -122 & Lat > 10.5 ~ "NPSG",
                            Cruise == "TN397" & Lat < 10.5 & Lat > 4.5 ~ "NPEC",
                            Cruise == "TN397" & Lat < 4.5 ~ "PEDP",
                            Cruise == "RC078" ~ "SS")) %>%
  mutate(Region = as.factor(Region)) %>%
  mutate(Region = fct_relevel(Region, c("PEDP", "NPEC", "NPSG", "NPTZ", "CUCP", "SS"))) %>%
  filter(!is.na(class)) %>%
  mutate(class = as.factor(class)) %>%
  mutate(class = fct_relevel(class, c("AA", "Betaine", "Sugar", "Sulfonium", "Sulfonate", "Other")))











##Created summed datasets for particulate and dissolved osmolytes:
part.cor.dat <- dat.region %>%
  filter(Part.detected == "Yes") %>%
  group_by(Parent_ID, Region, Cruise, sss, sst, poc, chla, pn, N_N, SRP, sum.median.biovol.ul.l) %>%
  reframe(Sum.Part.Conc.nM = sum(Part.Conc.nM)) %>%
  rename(biovol = sum.median.biovol.ul.l) %>%
  pivot_longer(cols = sss:biovol, names_to = "param", values_to = "value") %>%
  mutate(log10.Sum.Part.Conc.nM = log10(Sum.Part.Conc.nM),
         log10.value = log10(value))

diss.cor.dat <- dat.region %>%
  filter(Diss.detected == "Yes") %>%
  group_by(Parent_ID, Region, Cruise, sss, sst, poc, chla, pn, N_N, SRP, sum.median.biovol.ul.l) %>%
  reframe(Sum.Diss.Conc.nM = sum(Diss.Conc.nM)) %>%
  rename(biovol = sum.median.biovol.ul.l) %>%
  pivot_longer(cols = sss:biovol, names_to = "param", values_to = "value") %>%
  mutate(log10.Sum.Diss.Conc.nM = log10(Sum.Diss.Conc.nM),
         log10.value = log10(value))




##Run linear models on log scaled and regular values:

#Particulate
part.cor.output <- part.cor.dat %>%
  group_by(param) %>%
  cor_test(vars = c(Sum.Part.Conc.nM, value))
  
part.log.cor.output <- part.cor.dat %>%
  group_by(param) %>%
  cor_test(vars = c(log10.Sum.Part.Conc.nM, log10.value))

#Dissolved
diss.cor.output <- diss.cor.dat %>%
  group_by(param) %>%
  cor_test(vars = c(Sum.Diss.Conc.nM, value))

diss.log.cor.output <- diss.cor.dat %>%
  group_by(param) %>%
  cor_test(vars = c(log10.Sum.Diss.Conc.nM, log10.value))






###Read in and match up atp and osmolyte data:

#match up data based on 3 hour time window:
atp.dat.match <- read_csv(atp.file) %>%
  select(Cruise, Station, UTC.time.round, PATP_ng_L, SD_PATP_ng_L) %>%
  mutate(atp.timewindow.max = UTC.time.round + hours(3),
         atp.timewindow.min = UTC.time.round - hours(3))  %>%
  rename(UTC.time.round.ATP = UTC.time.round) %>%
  select(-Cruise)


osmo.dat.match <- dat.region %>%
  filter(Cruise %in% c("TN397", "KM1906")) %>%
  filter(Part.detected == "Yes") %>%
  select(Cruise, Region, Parent_ID, Lat, Long, UTC.time.round, Part.Conc.nM) %>%
  mutate(metab.sample = str_remove(Parent_ID, "_A"),
         metab.sample = str_remove(metab.sample, "_B"),
         metab.sample = str_remove(metab.sample, "_C")) %>%
  group_by(Cruise, Region, Parent_ID, metab.sample, Lat, Long, UTC.time.round) %>%
  reframe(Sum.Osmo.Conc.nM = sum(Part.Conc.nM)) %>%
  ungroup() %>%
  group_by(Cruise, Region, metab.sample, Lat, Long, UTC.time.round) %>%
  reframe(Mean.Osmo.Conc.nM = mean(Sum.Osmo.Conc.nM),
          SD.Osmo.Conc.nM = sd(Sum.Osmo.Conc.nM)) %>%
  rename(UTC.time.round.osmo = UTC.time.round) 


##combine data:
atp.osmo.dat <- cross_join(atp.dat.match, osmo.dat.match) %>%
  filter(UTC.time.round.osmo < atp.timewindow.max,
         UTC.time.round.osmo > atp.timewindow.min) %>%
  mutate(Mean.living.biomass.umolC.L = 250*PATP_ng_L/12.01/1000,
         SD.living.biomass.umolC.L = 250*SD_PATP_ng_L/12.01/1000) %>%
  unique()



###Run linear model to compare particulate osmolyte and living biomass data:
lb.osmo.lm <- lm(Mean.living.biomass.umolC.L~Mean.Osmo.Conc.nM, data = atp.osmo.dat)
summary(lb.osmo.lm)

#formula: y = 0.14388x + 0.00124


















