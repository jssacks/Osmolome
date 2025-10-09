






library(tidyverse)



###Define inputs:
lr.file <- "Intermediates/Dynamic_Range_Summary_v1.csv"
part.file <- "Intermediates/Part_Vial_Quant_Dat.csv"
#diss.file





###load and combine dataframes 
lin.range.dat <- read_csv(lr.file)

part.lr.dat <- read_csv(part.file) %>%
  filter(!str_detect(SampID, "Blk")) %>%
  select(Name, Cruise, SampID, umol.in.vial.ave) %>%
  unique() %>%
  rename(Compound = Name) %>%
  left_join(., lin.range.dat) %>%
  filter(!Compound %in% c("Homoserine Betaine (tentative)", "Threonine Betaine (tentative)", "L-Lysine",
                          "L-Cysteic acid", "L-Methionine", "L-Serine"))



#visualize
ggplot(part.lr.dat, aes(x = umol.in.vial.ave, fill = Cruise)) +
  geom_histogram(bins = 60) +
  facet_wrap(.~Compound, scales = "free_y") +
  scale_x_log10() +
  geom_vline(aes(xintercept = upper_limit_uM_in_vial), linetype = "dashed") +
  geom_vline(aes(xintercept = lower_limit_uM_in_vial))



#ggplot(part.lr.dat, aes(x = lower_limit_uM_in_vial, y = Compound)) +
#  geom_point()


##Figure out highest concentration and difference in highest concentrations and upper tested limit:
upper.conc.dat <- part.lr.dat %>%
  mutate(above.upper.lim = case_when(umol.in.vial.ave > upper_limit_uM_in_vial ~ 1,
                                     TRUE ~ 0)) %>%
  group_by(Compound) %>%
  mutate(num.measurements = n(),
         max.vial.conc = max(umol.in.vial.ave, na.rm = TRUE),
         Dat.Above.Upper.Lim = sum(above.upper.lim),
         Perc.Dat.Above.Upper.Lim = Dat.Above.Upper.Lim/num.measurements*100) 

upper.conc.dat.sum <- upper.conc.dat %>%
  select(Compound, upper_limit_uM_in_vial, max.vial.conc, Perc.Dat.Above.Upper.Lim) %>%
  mutate(Perc_Above = max.vial.conc/upper_limit_uM_in_vial) %>%
  unique()



















































































