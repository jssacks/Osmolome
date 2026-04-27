




library(tidyverse)
library(rstatix)
library(vegan)





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



###__XXX___
dat.comp.paired <- dat.region %>%
  filter(Part.detected == "Yes",
         Diss.detected == "Yes") %>%
  select(Cruise, Region, Parent_ID, compound.name.figure, Part.Conc.nM, Diss.Conc.nM) %>%
  mutate(Log10.Part.Conc.nM = log10(Part.Conc.nM),
         Log10.Diss.Conc.nM = log10(Diss.Conc.nM)) %>%
  group_by(Parent_ID) %>%
  mutate(sum.part.conc.nM = sum(Part.Conc.nM),
         sum.diss.conc.nM = sum(Diss.Conc.nM))

##Summarize Correlation results:
dat.comp.cor <- dat.comp.paired %>%
  group_by(compound.name.figure) %>%
  cor_test(vars = c(Log10.Part.Conc.nM, Log10.Diss.Conc.nM))

comp.cor.summary <- dat.comp.cor %>%
  mutate(cor.direction = case_when(cor > 0 & cor < 0.25 ~ "pos_0.0-0.25",
                                   cor >= 0.25 & cor < 0.5 ~ "pos_0.25-0.5",
                                   cor >= 0.50 & cor < 0.75 ~ "pos_0.5-0.75",
                                   cor >= 0.75 ~ "pos_0.75-1.0",
                                   cor < 0 & cor > -0.25 ~ "neg_0.0--0.25",
                                   cor <= -0.25 & cor > -0.5 ~ "neg_-0.25--0.5",
                                   cor <= -0.50 & cor > -0.75 ~ "neg_-0.5--0.75",
                                   cor <= -0.75 ~ "neg_-0.75--1.0")) %>%
  mutate(cor.sig = case_when(p < 0.01 ~ "Sig",
                             TRUE ~ "Not_Sig")) %>%
  group_by(cor.direction, cor.sig) %>%
  reframe(count = n())


##Overall correlation between total pools:
dat.cor <- dat.comp.paired %>%
  ungroup() %>%
  select(Parent_ID, sum.part.conc.nM, sum.diss.conc.nM) %>%
  unique() %>%
  cor_test(vars = c(sum.part.conc.nM, sum.diss.conc.nM))




####_____Mantel Test______________

##organize paired data and impute missing values
dat.comp.paired.mantel <- dat.region %>%
  mutate(Diss.detected = case_when(is.na(Diss.detected) & !is.na(Diss.SampID) ~ "No",
                                   TRUE ~ Diss.detected)) %>%
  mutate(Diss.Impute.Conc.nM = case_when(is.na(Diss.Impute.Conc.nM) & Compound == "Arsenobetaine" ~ 1.421805e-05, #add in value to impute for missing arsenobetaine data
                                         TRUE ~ Diss.Impute.Conc.nM)) %>%
  mutate(Part.Conc.nM = case_when(Part.detected == "No" ~ Part.Impute.Conc.nM,
                                  TRUE ~ Part.Conc.nM)) %>%
  mutate(Diss.Conc.nM = case_when(Diss.detected == "No" ~ Diss.Impute.Conc.nM,
                                  TRUE ~ Diss.Conc.nM)) %>%
  filter(!is.na(Part.SampID)) %>%
  filter(!is.na(Diss.SampID)) %>%
  select(Cruise, Region, Parent_ID, compound.name.figure, Part.Conc.nM, Diss.Conc.nM) %>%
  mutate(Log10.Part.Conc.nM = log10(Part.Conc.nM),
         Log10.Diss.Conc.nM = log10(Diss.Conc.nM)) %>%
  filter(!Parent_ID == "TN397_S11_600_U_C")

###_organize data for test
dat.diss.mant <- dat.comp.paired.mantel %>%
  select(Parent_ID, compound.name.figure, Log10.Diss.Conc.nM) %>%
  filter(!compound.name.figure %in% c("GG", "Sucrose", "Trehalose", "DHPS", "Taurine", "Isethionic acid")) %>%
  pivot_wider(id_cols = Parent_ID, names_from = compound.name.figure, values_from = Log10.Diss.Conc.nM) %>%
  column_to_rownames(var = "Parent_ID")
#  group_by(Parent_ID) %>%
#  filter(!is.na(DMSP)) 

dat.part.mant <- dat.comp.paired.mantel %>%
  select(Parent_ID, compound.name.figure, Log10.Part.Conc.nM) %>%
  pivot_wider(id_cols = Parent_ID, names_from = compound.name.figure, values_from = Log10.Part.Conc.nM) %>%
  column_to_rownames(var = "Parent_ID")






###Standardize data and create dissimilarity matrix

#Standardize data
p.stand<- decostand(dat.part.mant, method = "range", MARGIN = 2)
d.stand <- decostand(dat.diss.mant, method = "range", MARGIN = 2) 


#convert to distance matrices 
p.dist <- vegdist(p.stand, method = "euclidean")
d.dist <- vegdist(d.stand, method = "euclidean")


#attempt Mantel Test:
m.test <- mantel(p.dist, d.dist)

m.test










#visualize relationships
ggplot(dat.comp.paired, aes(x = Part.Conc.nM, Diss.Conc.nM)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", color = "black") +
  geom_point(shape = 21, stroke = 0.1, aes(fill = Region)) +
  scale_fill_manual(values = region.palette.7) +
  facet_wrap(.~compound.name.figure, scales = "free") +
  scale_x_log10() +
  scale_y_log10() 














































