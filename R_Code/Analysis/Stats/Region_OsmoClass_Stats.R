




#install.packages("multcompView")

library(tidyverse)
library(patchwork)
library(rstatix)
library(multcompView)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")





###define inputs
all.dat.file <- "Intermediates/Enviro_Osmo_Final_Dataset_with_metadata.csv"






#Read in data 
dat <- read_csv(all.dat.file) %>%
  left_join(., compound.order) %>%
  filter(!str_detect(Part.SampID, "Blk"))


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



#Summarize values by class:

#Particulate:
Part.sum.dat <- dat.region %>%
  filter(Part.detected == "Yes") %>%
  group_by(Part.SampID) %>%
  mutate(sum.part.conc = sum(Part.Conc.nM)) %>%
  group_by(Part.SampID, class, Region) %>%
  reframe(class.sum.conc = sum(Part.Conc.nM),
          class.rel.conc = class.sum.conc/sum.part.conc) %>%
  unique() 


##Dissolved:
Diss.sum.dat <- dat.region %>%
  filter(Diss.detected == "Yes") %>%
  group_by(Diss.SampID) %>%
  mutate(sum.diss.conc = sum(Diss.Conc.nM)) %>%
  group_by(Diss.SampID, class, Region) %>%
  reframe(class.sum.conc = sum(Diss.Conc.nM),
          class.rel.conc = class.sum.conc/sum.diss.conc) %>%
  unique()




####Stats

#Particulate

#Run ANOVA on each class:
part.stats.dat <- Part.sum.dat 

#run Welch's anova grouped by class
part.w.anova.output <- part.stats.dat %>%
  group_by(class) %>%
  welch_anova_test(class.rel.conc ~ Region) %>%
  select(class, p) %>%
  rename("W_ANOVA_pval" = p)

part.gh.output <- part.stats.dat %>%
  group_by(class) %>%
  games_howell_test(class.rel.conc ~ Region, conf.level = 0.99)

##combine and summarize
part.wanova.sum <- part.stats.dat %>%
  group_by(class, Region) %>%
  reframe(mean.class.rel.conc = mean(class.rel.conc, na.rm = TRUE),
          sd.class.rel.conc = sd(class.rel.conc, na.rm = TRUE)) %>%
  left_join(., part.w.anova.output) 
  
part.gh.sum <- part.gh.output %>%
  select(class, group1, group2, p.adj, p.adj.signif)

###Export particulate stats results:
write_csv(part.wanova.sum, file = "Tables/Oct25_Stats_Tables/Class_RelComp_Particulate_WANOVA_results.csv")
write_csv(part.gh.sum, file = "Tables/Oct25_Stats_Tables/Class_RelComp_Particulate_GHTest_results.csv")




#Dissolved

#Run ANOVA on each class:
diss.stats.dat <- Diss.sum.dat 

#run Welch's anova grouped by class
diss.w.anova.output <- diss.stats.dat %>%
  group_by(class) %>%
  welch_anova_test(class.rel.conc ~ Region) %>%
  select(class, p) %>%
  rename("W_ANOVA_pval" = p)

diss.gh.output <- diss.stats.dat %>%
  group_by(class) %>%
  games_howell_test(class.rel.conc ~ Region, conf.level = 0.99)

##combine and 
diss.wanova.sum <- diss.stats.dat %>%
  group_by(class, Region) %>%
  reframe(mean.class.rel.conc = mean(class.rel.conc, na.rm = TRUE),
          sd.class.rel.conc = sd(class.rel.conc, na.rm = TRUE)) %>%
  left_join(., diss.w.anova.output) 

diss.gh.sum <- diss.gh.output %>%
  select(class, group1, group2, p.adj, p.adj.signif)

###Export dissolved stats results:
write_csv(diss.wanova.sum, file = "Tables/Oct25_Stats_Tables/Class_RelComp_Dissolved_WANOVA_results.csv")
write_csv(diss.gh.sum, file = "Tables/Oct25_Stats_Tables/Class_RelComp_Dissolved_GHTest_results.csv")







































