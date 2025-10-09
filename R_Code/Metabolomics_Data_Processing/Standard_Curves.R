





##Determine Linear Ranges of Compounds using a standard Curve:

#Thresholds:

#minimum peak area
pa.threshold <- 40000

#blank ratio threshold
br.threshold <- 3 










##load packages
library(tidyverse)

##Functions:
source("R_Code/Functions.R")


#define inputs
pos.stdcurve.file <- "Raw_Data/Standard_Curves/QE_HILIC_Pos_standardcurves_Jan25.csv"
neg.stdcurve.file <- "Raw_Data/Standard_Curves/QE_HILIC_Neg_standardcurves_Jan25.csv"
stds.file <- "Meta_Data/Ingalls_Lab_Standards_09222025.csv"



#Load in data:
pos.dat <- sky_read(pos.stdcurve.file) %>%
  select(Rep, Compound, Area)
  
neg.dat <- sky_read(neg.stdcurve.file) %>%
  select(Rep, Compound, Area)

sc.dat <- rbind(pos.dat, neg.dat) %>%
  mutate(Rep_Conc = case_when(str_detect(Rep, "5000nM") ~ 5000,
                              str_detect(Rep, "2500nM") ~ 2500,
                              str_detect(Rep, "500nM") ~ 500,
                              str_detect(Rep, "250nM") ~ 250,
                              str_detect(Rep, "50nM") ~ 50,
                              str_detect(Rep, "10nM") ~ 10,
                              str_detect(Rep, "_5nM") ~ 5,
                              str_detect(Rep, "0-5nM") ~ 0.5,
                              str_detect(Rep, "0-05nM") ~ 0.05),
         Rep_Mix = case_when(str_detect(Rep, "Mix1") ~ "Mix1",
                             TRUE ~ "Mix2")) %>%
  filter(!Rep %in% c("250227_Std_4uMStdsInH2O_1", "241016_Std_4uMStdsMix1InH2O_1"))



#load in standards:
stds.info <- read_csv(stds.file) %>%
  select(Compound_Name, HILIC_Mix, Concentration_uM) %>%
  rename(Compound = Compound_Name,
         Rep_Mix = HILIC_Mix)


#Combine data
stds.dat <- left_join(stds.info, sc.dat) %>%
  mutate(vial_conc = Concentration_uM*2.5*Rep_Conc/5000) %>%
  filter(Compound %in% c("beta-Alaninebetaine", "Glycine betaine", "Proline betaine", "Homarine",
             "Trigonelline", "Betonicine", "Dimethylsulfonioacetate", "Dimethylsulfoniopropionate",
             "Gonyol", "Trimethylamine N-oxide", "(3-Carboxypropyl)trimethylammonium", "2-O-alpha-D-Glucosylglycerol",
             "5-Hydroxyectoine", "Carnitine", "Ectoine", "Hydroxyisoleucine", "L-Alanine", "L-Aspartic acid", "L-Glutamic acid",
             "L-Glutamine", "L-Proline", "L-Serine", "L-Threonine", "Sarcosine", "Threonine Betaine (tentative)",
             "Homoserine Betaine (tentative)", "L-Cysteic acid", "(R)-2,3-Dihydroxypropane-1-sulfonate", "Isethionic acid",
             "Sucrose", "Taurine", "Trehalose", "Arsenobetaine", "L-Isoleucine", "L-Asparagine", "L-Lysine",
             "beta-Glutamic acid", "L-Methionine", "L-Tyrosine", "L-Arginine", "beta-Alanine")) %>%
  mutate(Area = replace_na(Area, 0)) %>%
  filter(!is.na(Rep))


#Determine blanks for each treatment (the lowest concentration of the opposite mix... )

#create dataframe that pulls out the lowest concentration sample of the opposite mix to use as a blank

# For sucrose and L-Lysine, the lowest concentration of the other mix has an abnormally high signal at that compounds RT and mass
# (higher than the same concentration or the sample containing sucrose at that concentration which shows no signal), Therefore,
# the 0.05 nM sample for the same mix was used instead.

blk.dat <- stds.dat %>%
  mutate(o.mix = case_when(Rep_Mix == "Mix1" ~ "Mix2",
                           TRUE ~ "Mix1")) %>%
  mutate(o.mix = case_when(Compound == "Sucrose" ~ "Mix1", 
                           TRUE ~ o.mix)) %>%
  mutate(o.mix = case_when(Compound == "L-Lysine" ~ "Mix1", 
                           TRUE ~ o.mix)) %>%
  select(Compound, Rep_Mix, o.mix) %>%
  unique() %>%
  select(-Rep_Mix) %>%
  rename(Rep_Mix = o.mix) %>%
  left_join(sc.dat) %>%
  mutate(Area = replace_na(Area, 0)) %>%
  filter(Rep_Conc == 0.05) %>%
  group_by(Compound) %>%
  summarize(mean_blk = mean(Area),
            max_blk = max(Area),
            sd_blk = sd(Area),
            blk_sd_threshold = mean_blk +(3*sd_blk))


###Perform QC on stds data:
qc.dat <- stds.dat %>%
  left_join(., blk.dat) %>%
  mutate(min.area.flag = case_when(Area < pa.threshold ~ 1,
                                   TRUE ~ 0),
         blk.ratio.flag = case_when(Area < br.threshold*mean_blk ~ 1,
                                    TRUE ~ 0),
         blk.sd.flag = case_when(Area < blk_sd_threshold ~ 1,
                                 TRUE ~ 0),
         blk.max.flag = case_when(Area < max_blk ~ 1,
                                 TRUE ~ 0))

qc.count.dat <- qc.dat %>%
  group_by(Compound, Rep_Conc) %>%
  reframe(ma.flag.count = sum(min.area.flag),
          blksd.flag.count = sum(blk.sd.flag)) %>%
  mutate(total.flag.count = ma.flag.count + blksd.flag.count) %>%
  mutate(pass.qc = case_when(total.flag.count == 0 ~ "Yes",
                             TRUE ~ "No")) %>%
  mutate(pass.qc = case_when(Compound == "Isethionic acid" & Rep_Conc == 0.05 ~ "No",   ##Make isethionic acid 0.05 nM match qc status of 0.5 and 5 nM
                             TRUE ~ pass.qc))


#Establish lower and upper concentration limits for each compound
dynamic.range.dat <- qc.count.dat %>%
  filter(pass.qc == "Yes") %>%
  group_by(Compound) %>%
  mutate(lower_limit_nM = min(Rep_Conc),
         upper_limit_nM = max(Rep_Conc))
  

###Establish R2 of linear range
linear.dat <- stds.dat %>%
  left_join(dynamic.range.dat %>% select(Compound, lower_limit_nM, upper_limit_nM) %>% unique()) %>%
  filter(!Rep_Conc < lower_limit_nM) %>%
  filter(!Rep_Conc > upper_limit_nM) %>%
  group_by(Compound) %>%
  mutate(Area_Conc_Cor = cor(Area, vial_conc),
         R2 = Area_Conc_Cor^2)



###Summarize Dynamic Range Stuff:
dynamic.range.summary <- linear.dat %>%
  mutate(lower_limit_uM_in_vial = Concentration_uM*2.5*lower_limit_nM/5000,
         upper_limit_uM_in_vial = Concentration_uM*2.5*upper_limit_nM/5000)  %>%
  select(Compound, lower_limit_uM_in_vial, upper_limit_uM_in_vial, R2) %>%
  unique()

write_csv(dynamic.range.summary, file = "Intermediates/Dynamic_Range_Summary_v1.csv")







#Visualizations 
ggplot(qc.count.dat, aes(x = as.factor(Rep_Conc), y = Compound, fill = pass.qc)) +
  geom_tile()


ggplot(qc.count.dat, aes(x = as.factor(Rep_Conc), y = br.flag.count)) +
  geom_col() +
  facet_wrap(.~Compound)


ggplot(qc.count.dat, aes(x = as.factor(Rep_Conc), y = ma.flag.count)) +
  geom_col() +
  facet_wrap(.~Compound)


ggplot(stds.dat %>% left_join(., blk.dat), aes(x = vial_conc, y = Area)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_hline(aes(yintercept = 40000), color = "red") +
  geom_hline(aes(yintercept = 3*mean_blk), color = "green") +
  geom_hline(aes(yintercept = blk_sd_threshold), color = "purple") +
  geom_hline(aes(yintercept = max_blk), color = "black", linetype = "dashed") +
  geom_point() +
  facet_wrap(.~Compound, scales = "free") +
  scale_y_log10() +
  scale_x_log10() 






















