







library(tidyverse)
library(rstatix)
library(lme4)
library(lmerTest)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")



###define inputs
env.dat.file <- "Intermediates/Enviro_Osmo_Final_Dataset_with_metadata.csv"
peri.dat.file <- "Intermediates/PERIFIX_Osmo_Meta_Dat_101325.csv"



#Read in data 
dat <- read_csv(env.dat.file) %>%
  left_join(., compound.order) %>%
  filter(!str_detect(Part.SampID, "Blk")) %>%
  filter(!Parent_ID == "TN397_S11_600_U_C")

#Define regions:
dat.region <- dat %>%
  mutate(Region = case_when(Cruise == "KM1906" & Lat >= 35 ~ "NPTZ",
                            Cruise == "KM1906" & Lat < 35 ~ "NPSG",
                            Cruise == "TN397" & Long > -122 ~ "CUCP",
                            Cruise == "TN397" & Long < -122 & Lat > 10.5 ~ "NPSG",
                            Cruise == "TN397" & Lat < 10.5 & Lat > 4.5 ~ "NPEC",
                            Cruise == "TN397" & Lat < 4.5 ~ "PEDP",
                            Cruise == "RC078" ~ "SS"))

#Load in PERIFIX Data:
peri.dat <- read_csv(peri.dat.file)%>%
  left_join(., compound.order) %>%
  filter(!str_detect(Part.SampID, "Blk")) %>%
  filter(!is.na(compound.name.figure))









### ______________Normalize data to poc:

#G3 and G4:

#normalize to poc
region.norm.dat <- dat.region %>%
  filter(Cruise %in% c("KM1906", "TN397")) %>%
  mutate(Part.Conc.nM = case_when(Part.detected == "No" ~ Part.Impute.Conc.nM,
                                  TRUE ~ Part.Conc.nM)) %>%
  filter(!is.na(poc)) %>%
  mutate(poc.norm.conc = Part.Conc.nM/poc) %>%
  group_by(compound.name.figure, Cruise, Region) %>%
  mutate(region.mean.norm.conc = mean(poc.norm.conc)) %>%
  filter(!is.na(compound.name.figure))

#pull out just regional mean values
region.mean.conc <- region.norm.dat %>%
  select(Cruise, Region, compound.name.figure, region.mean.norm.conc) %>%
  unique()


#PERIFIX dat:

#normalize to POC
peri.dat.norm <- peri.dat %>%
  filter(!Treatment == "Tote") #%>%
  mutate(Part.Conc.nM = case_when(Part.detected == "No" ~ Part.Impute.Conc.nM,
                                  TRUE ~ Part.Conc.nM)) %>%
  filter(!is.na(POC_uM)) %>%
  mutate(poc.norm.conc = Part.Conc.nM/POC_uM) %>%
  group_by(compound.name.figure, N_status) %>%
  mutate(N_status.mean.norm.conc = mean(poc.norm.conc))

peri.mean.conc <- peri.dat.norm %>%
  select(N_status, compound.name.figure, N_status.mean.norm.conc) %>%
  unique()


peri.dat.check <- peri.dat %>%
  filter(!Treatment == "Tote") %>%
  group_by(compound.name.figure, Part.detected) %>%
  reframe(count = n())



#________________Calculate Mean Log2FC values for each compound normalized to POC___________ 
g3.log2fc <- region.mean.conc %>%
  filter(Cruise == "KM1906") %>%
  pivot_wider(id_cols = c(Cruise, compound.name.figure), names_from = Region, values_from = region.mean.norm.conc) %>%
  mutate(log2fc = log2(NPTZ/NPSG)) %>% 
  mutate(Comparison = "NPTZ/NPSG") %>%
  select(-NPTZ, -NPSG)

g4.log2fc <- region.mean.conc %>%
  filter(Cruise == "TN397") %>%
  pivot_wider(id_cols = c(Cruise, compound.name.figure), names_from = Region, values_from = region.mean.norm.conc) %>%
  mutate(log2fc = log2(PEDP/NPSG)) %>% 
  mutate(Comparison = "PEDP/NPSG") %>%
  select(-PEDP, -NPSG, -NPEC)


peri.log2fc <- peri.mean.conc  %>%
  pivot_wider(id_cols = c(compound.name.figure), names_from = N_status, values_from = N_status.mean.norm.conc) %>%
  mutate(log2fc = log2(plus_N/minus_N)) %>% 
  mutate(Comparison = "Plus_N/Minus_N",
         Cruise = "PERIFIX") %>%
  select(-plus_N, -minus_N) 





# Fold change t-test calculations -----------------------------------------

###g3 fold change analysis:
g3.ttest.dat <- region.norm.dat %>%
  filter(Cruise == "KM1906") %>%
  select(Cruise, Part.SampID, compound.name.figure, Region, poc.norm.conc)

g3.ttest.out <- g3.ttest.dat %>%
  group_by(Cruise, compound.name.figure) %>%
  t_test(poc.norm.conc ~ Region, var.equal = FALSE) %>%
  select(Cruise, compound.name.figure, p) %>%
  mutate(Comparison = "NPTZ/NPSG")


###g4 fold change analysis:
g4.ttest.dat <- region.norm.dat %>%
  filter(Cruise == "TN397") %>%
  filter(Region %in% c("PEDP", "NPSG")) %>%
  select(Part.SampID, compound.name.figure, Region, poc.norm.conc)

g4.ttest.out <- g4.ttest.dat %>%
  group_by(Cruise, compound.name.figure) %>%
  t_test(poc.norm.conc ~ Region, var.equal = FALSE) %>%
  select(Cruise, compound.name.figure, p) %>%
  mutate(Comparison = "PEDP/NPSG")


###peri fold change analysis:
peri.ttest.dat <- peri.dat.norm %>%
  select(Part.SampID, compound.name.figure, N_status, poc.norm.conc)

peri.ttest.out <- peri.ttest.dat %>%
  group_by(compound.name.figure) %>%
  t_test(poc.norm.conc ~ N_status, var.equal = FALSE) %>%
  select(compound.name.figure, p)  %>%
  mutate(Cruise = "PERIFIX",
         Comparison = "Plus_N/Minus_N")



####Try out a linear mixed effects model for perifix

#Create data frame 
peri.stat.dat <- peri.dat.norm %>%
  select(Part.SampID, compound.name.figure, N_status, N, P, Fe, poc.norm.conc) %>%
  mutate(N = as.factor(N),
         P = as.factor(P),
         Fe = as.factor(Fe))


#Write function to perform linear mixed effects model
peri.lme <- function(dataframe, compound) {
  
  dat.comp <- dataframe %>%
    filter(compound.name.figure == compound)
  
  lme.output <- lmer(poc.norm.conc ~ N + (1|P) + (1|Fe), dat = dat.comp)
  
  lme.sum <- summary(lme.output)
  
  p_val <- lme.sum$coefficients[2, "Pr(>|t|)"]
  
  comp.pval <- tibble(compound.name.figure = compound,
                      p = p_val)
  
  print(comp.pval)
}



#Create blank dataset 
peri.lme.pvals <- tibble(compound.name.figure = character(),
                         p = numeric())


#create compound list for for loop
comp.list <- peri.stat.dat %>%
  ungroup() %>%
  select(compound.name.figure) %>%
  unique()

#Use for loop to perform lme analysis
for (i in seq_along(comp.list$compound.name.figure)) {
  compound <-  comp.list$compound.name.figure[i]
  print(compound)
  comp.pval <- peri.lme(peri.stat.dat, compound)
  peri.lme.pvals <- rbind(peri.lme.pvals, comp.pval)
}

peri.lme.dat <- peri.lme.pvals

#perform p-value adjustment
peri.lme.p.adjust <- peri.lme.dat %>%
  adjust_pvalue(., p.col = "p", output.col = "p", method = "fdr") 


  
#prep data to combine with other analyses
peri.lme.out <- peri.lme.p.adjust %>%
  mutate(Cruise = "PERIFIX",
         Comparison = "Plus_N/Minus_N")




#____________Compile all results:

###Compile fold change results 
fc.dat <- rbind(g3.log2fc, g4.log2fc, peri.log2fc)

## compile ttest output
pval.dat <- rbind(g3.ttest.out, g4.ttest.out, peri.lme.out)

## combine ttest results with fc results
fc.p.dat <- fc.dat %>%
  left_join(., pval.dat) %>%
  ungroup()



#Compare fold change analysis across groups

#Define behavior in each cruise:

#G3
g3.fc.p <- fc.p.dat %>%
  filter(Cruise == "KM1906") %>%
  rename(g3.log2fc = log2fc,
         g3.p = p,
         g3.comparison = Comparison) %>%
  select(compound.name.figure, g3.log2fc, g3.p, g3.comparison) %>%
  mutate(g3_behavior = case_when(g3.log2fc < 0 & g3.p < 0.05 ~ "Sig_Decreased",
                                 g3.log2fc > 0 & g3.p < 0.05 ~ "Sig_Increased",
                                 TRUE ~ "Not_Sig_Different"))

#G4
g4.fc.p <- fc.p.dat %>%
  filter(Cruise == "TN397") %>%
  rename(g4.log2fc = log2fc,
         g4.p = p,
         g4.comparison = Comparison) %>%
  select(compound.name.figure, g4.log2fc, g4.p, g4.comparison) %>%
  mutate(g4_behavior = case_when(g4.log2fc < 0 & g4.p < 0.05 ~ "Sig_Decreased",
                                 g4.log2fc > 0 & g4.p < 0.05 ~ "Sig_Increased",
                                 TRUE ~ "Not_Sig_Different"))

#PERIFIX
peri.fc.p <- fc.p.dat %>%
  filter(Cruise == "PERIFIX") %>%
  rename(peri.log2fc = log2fc,
         peri.p = p, 
         peri.comparison = Comparison) %>%
  select(compound.name.figure, peri.log2fc, peri.p, peri.comparison) %>%
  mutate(peri_behavior = case_when(peri.log2fc < 0 & peri.p < 0.05 ~ "Sig_Decreased",
                                 peri.log2fc > 0 & peri.p < 0.05 ~ "Sig_Increased",
                                 TRUE ~ "Not_Sig_Different"))

##
fc.analysis.comparison <- left_join(g3.fc.p, g4.fc.p) %>%
  left_join(., peri.fc.p) %>%
  mutate(behavior_sum = case_when(g3_behavior == "Sig_Increased" & g4_behavior == "Sig_Increased" & peri_behavior == "Sig_Increased" ~ "Increased all three",
                                  g3_behavior == "Sig_Increased" & g4_behavior == "Sig_Increased" & !peri_behavior == "Sig_Increased" ~ "Increased G3 and G4",
                                  g3_behavior == "Sig_Decreased" & g4_behavior == "Sig_Decreased" & peri_behavior == "Sig_Decreased" ~ "Decreased all three",
                                  g3_behavior == "Sig_Decreased" & g4_behavior == "Sig_Decreased" & !peri_behavior == "Sig_Decreased" ~ "Decreased G3 and G4",
                                  TRUE ~ "Other")) %>%
  mutate(label_text = case_when(compound.name.figure %in% c("GG", "Isethionic acid", "DHPS", "Taurine", "Proline", "Homarine", "Betonicine", 
                                                            "Proline betaine", "Arsenobetaine", "DMSA", "Trehalose") ~ "Yes",
                                TRUE ~ "No"))



#Export comparisons:
write_csv(fc.analysis.comparison, file = "Intermediates/Fold_Change_Analysis_Stats.csv")





##Make Supplemen

 # %>%
 #  mutate(sig.enriched.NPTZ = case_when(Cruise == "KM1906" & log2fc > 0 & p < 0.05 ~ TRUE,
 #                                       TRUE ~ FALSE),
 #         sig.enriched.EQ = case_when(Cruise == "TN397" & log2fc > 0 & p < 0.05 ~ TRUE,
 #                                     TRUE ~ FALSE),
 #         sig.enriched.plus_N = case_when(Cruise == "PERIFIX" & log2fc > 0 & p < 0.05 ~ TRUE,
 #                                         TRUE ~ FALSE),
 #         sig.enriched.gyre.G3 = case_when(Cruise == "KM1906" & log2fc < 0 & p < 0.05 ~ TRUE,
 #                                          TRUE ~ FALSE),
 #         sig.enriched.gyre.G4 = case_when(Cruise == "TN397" & log2fc < 0 & p < 0.05 ~ TRUE,
 #                                          TRUE ~ FALSE),
 #         sig.enriched.minus_N = case_when(Cruise == "PERIFIX" & log2fc < 0 & p < 0.05 ~ TRUE,
 #                                          TRUE ~ FALSE)) #%>%
  # group_by(compound.name.figure) %>%
  # mutate(enriched.N.G3.G4.PERIFIX = case_when(sig.enriched.NPTZ == TRUE & sig.enriched.EQ == TRUE & sig.enriched.plus_N == TRUE ~ TRUE,
  #                                             TRUE ~ FALSE),
  #        enriched.N.G3.G4.noperi = case_when(sig.enriched.NPTZ == TRUE & sig.enriched.EQ == TRUE & sig.enriched.plus_N == FALSE ~ TRUE,
  #                                             TRUE ~ FALSE),
  #        depleted.N.G3.G4.PERIFI = case_when(sig.enriched.gyre.G3 == TRUE & sig.enriched.gyre.G4 == TRUE & sig.enriched.minus_N == TRUE ~ TRUE,
  #                                             TRUE ~ FALSE),
  #        depleted.N.G3.G4.noperi = case_when(sig.enriched.gyre.G3 == TRUE & sig.enriched.gyre.G4 == TRUE & sig.enriched.minus_N == FALSE ~ TRUE,
  #                                            TRUE ~ FALSE))
         
ggplot(fc.p.dat, aes(x = log2fc, y = -log10(p))) +
  geom_point(shape = 21, ) +
  facet_wrap(.~Cruise, scales = "free") +
  geom_hline(yintercept = -log10(0.05))




ggplot(fc.analysis.comparison, aes(x = g3.log2fc, y = -log10(g3.p))) +
  geom_point(aes(fill = behavior_sum), shape = 21, size = 2, stroke = 0.2)


ggplot(fc.analysis.comparison, aes(x = g4.log2fc, y = -log10(g4.p))) +
  geom_point(aes(fill = behavior_sum), shape = 21, size = 2, stroke = 0.2)


ggplot(fc.analysis.comparison, aes(x = peri.log2fc, y = -log10(peri.p))) +
  geom_point(aes(fill = behavior_sum), shape = 21, size = 2, stroke = 0.2)




























