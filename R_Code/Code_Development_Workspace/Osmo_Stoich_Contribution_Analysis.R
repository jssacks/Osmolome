

library(tidyverse)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")

##Define inputs:
part.file <- "Intermediates/Particulate_Quant_Output.csv"
meta.file <- "Intermediates/All_metadata_information.csv"

###load in data and combine with metadata
part.dat <- read_csv(part.file)
meta.dat <- read_csv(meta.file)


####
dat.all <- left_join(part.dat, meta.dat) %>%
  rename("Compound" = Name)


gradients <- dat.all %>%
  filter(Cruise %in% c("TN397", "KM1906")) %>%
  filter(Compound %in% compound.order$Compound) %>%
  left_join(., compound.order) %>%
  filter(!compound.name.figure == "DMSP")


gradients.stoich <- gradients %>%
  group_by(SampID) %>%
  mutate(C.tot = sum(nM_C, na.rm = TRUE),
         N.tot = sum(nM_N, na.rm = TRUE),
         S.tot = sum(nM_S, na.rm = TRUE)) %>%
  mutate(C.N.tot = C.tot/N.tot,
         C.S.tot = C.tot/S.tot,
         N.S.tot = N.tot/S.tot) %>%
  mutate(Group_Carbon = case_when(class %in% c("Sugar") ~ "Yes",
                                  TRUE ~ "No"),
         Group_Nitrogen = case_when(class %in% c("AA", "Betaine", "TMAO", "Taurine") ~ "Yes",
                                    TRUE ~ "No"),
         Group_Sulfur = case_when(class %in% c("Sulfonium", "Sulfonate", "Taurine") ~ "Yes",
                                  TRUE ~ "No"))  %>%
  mutate(C.comp = sum(nM.in.smp[Group_Carbon == "Yes"]),
         N.comp = sum(nM.in.smp[Group_Nitrogen == "Yes"]),
         S.comp = sum(nM.in.smp[Group_Sulfur == "Yes"]),
         C.N.comp = C.comp/N.comp,
         C.S.comp = C.comp/S.comp,
         N.S.comp = N.comp/S.comp) %>%
  filter(!Long > -125)








######test to see which compound causes the largest change in N/S ratio


#Calculate value with all compounds:
g.stoich.all <- gradients %>%
  group_by(SampID) %>%
  mutate(C.tot = sum(nM_C, na.rm = TRUE),
         N.tot = sum(nM_N, na.rm = TRUE),
         S.tot = sum(nM_S, na.rm = TRUE)) %>%
  mutate(C.N.tot = C.tot/N.tot,
         C.S.tot = C.tot/S.tot,
         N.S.tot = N.tot/S.tot) %>%
  mutate(Group_Carbon = case_when(class %in% c("Sugar") ~ "Yes",
                                  TRUE ~ "No"),
         Group_Nitrogen = case_when(class %in% c("AA", "Betaine", "TMAO", "Taurine") ~ "Yes",
                                    TRUE ~ "No"),
         Group_Sulfur = case_when(class %in% c("Sulfonium", "Sulfonate", "Taurine") ~ "Yes",
                                  TRUE ~ "No"))  %>%
  mutate(C.comp = sum(nM.in.smp[Group_Carbon == "Yes"]),
         N.comp = sum(nM.in.smp[Group_Nitrogen == "Yes"]),
         S.comp = sum(nM.in.smp[Group_Sulfur == "Yes"]),
         C.N.comp = C.comp/N.comp,
         C.S.comp = C.comp/S.comp,
         N.S.comp = N.comp/S.comp) %>%
  select(SampID, Cruise, Lat, Long, Local_Date, Local_Time,
         C.tot, N.tot, S.tot, C.N.tot, C.S.tot, N.S.tot, 
         C.comp, N.comp, S.comp, C.N.comp, C.S.comp, N.S.comp) %>% 
  unique() %>%
  mutate(compound.remove = "none")

#
#Turn calculation into a function

calc_osmo_stoich <- function(osmo.dat, comp.remove) {
  #
  osmo.dat.1 <- osmo.dat %>% filter(!Compound == comp.remove)
  #
  osmo.stoich.out <- osmo.dat.1 %>%
    group_by(SampID) %>%
    mutate(C.tot = sum(nM_C, na.rm = TRUE),
           N.tot = sum(nM_N, na.rm = TRUE),
           S.tot = sum(nM_S, na.rm = TRUE)) %>%
    mutate(C.N.tot = C.tot/N.tot,
           C.S.tot = C.tot/S.tot,
           N.S.tot = N.tot/S.tot) %>%
    mutate(Group_Carbon = case_when(class %in% c("Sugar") ~ "Yes",
                                    TRUE ~ "No"),
           Group_Nitrogen = case_when(class %in% c("AA", "Betaine", "TMAO", "Taurine") ~ "Yes",
                                      TRUE ~ "No"),
           Group_Sulfur = case_when(class %in% c("Sulfonium", "Sulfonate", "Taurine") ~ "Yes",
                                    TRUE ~ "No"))  %>%
    mutate(C.comp = sum(nM.in.smp[Group_Carbon == "Yes"]),
           N.comp = sum(nM.in.smp[Group_Nitrogen == "Yes"]),
           S.comp = sum(nM.in.smp[Group_Sulfur == "Yes"]),
           C.N.comp = C.comp/N.comp,
           C.S.comp = C.comp/S.comp,
           N.S.comp = N.comp/S.comp) %>%
    select(SampID, Cruise, Lat, Long, Local_Date, Local_Time,
           C.tot, N.tot, S.tot, C.N.tot, C.S.tot, N.S.tot, 
           C.comp, N.comp, S.comp, C.N.comp, C.S.comp, N.S.comp) %>% 
    unique() %>%
    mutate(compound.remove = comp.remove)
  #
  osmo.stoich.out
}



x <- calc_osmo_stoich(gradients, "none")
y <- calc_osmo_stoich(gradients, "L-Glutamic acid")


###Make list of all compounds
comp.list <- gradients %>% 
  select(Compound) %>% 
  unique() %>%
  rbind(., tibble(Compound = "none"))


#Run on all compounds:

out <- g.stoich.all[0,] %>% 
  select(SampID, Cruise, Lat, Long, Local_Date, Local_Time,
         C.tot, N.tot, S.tot, C.N.tot, C.S.tot, N.S.tot, 
         C.comp, N.comp, S.comp, C.N.comp, C.S.comp, N.S.comp) 

for (Compound in comp.list$Compound) {
  df.out <- calc_osmo_stoich(gradients, Compound)
  out <- rbind(out, df.out)
}

out.dat <- out

stoich.diff <- out.dat %>%
  group_by(SampID) %>%
  mutate(C.tot.diff = C.tot-C.tot[compound.remove == "none"],
         N.tot.diff = N.tot-N.tot[compound.remove == "none"],
         S.tot.diff = S.tot-S.tot[compound.remove == "none"],
         C.N.tot.diff = C.N.tot-C.N.tot[compound.remove == "none"],
         C.S.tot.diff = C.S.tot-C.S.tot[compound.remove == "none"],
         N.S.tot.diff = N.S.tot-N.S.tot[compound.remove == "none"],
         C.comp.diff = C.comp-C.comp[compound.remove == "none"],
         N.comp.diff = N.comp-N.comp[compound.remove == "none"],
         S.comp.diff = S.comp-S.comp[compound.remove == "none"],
         C.N.comp.diff = C.N.comp-C.N.comp[compound.remove == "none"],
         C.S.comp.diff = C.S.comp-C.S.comp[compound.remove == "none"],
         N.S.comp.diff = N.S.comp-N.S.comp[compound.remove == "none"],
         C.N.tot.perc.diff = (C.N.tot.diff/C.N.tot[compound.remove == "none"])*100,
         C.S.tot.perc.diff = (C.S.tot.diff/C.S.tot[compound.remove == "none"])*100,
         N.S.tot.perc.diff = (N.S.tot.diff/N.S.tot[compound.remove == "none"])*100,
         C.N.tot.none = C.N.tot[compound.remove == "none"],
         C.S.tot.none = C.S.tot[compound.remove == "none"],
         N.S.tot.none = N.S.tot[compound.remove == "none"],
         N.S.tot.quot = N.S.tot/N.S.tot[compound.remove == "none"])


ggplot(stoich.diff, aes(x = N.S.tot.perc.diff, y = reorder(compound.remove, N.S.tot.perc.diff))) +
         geom_boxplot() 


ggplot(stoich.diff, aes(x = Lat, y = N.S.tot.perc.diff)) +
  geom_hline(yintercept = 0, color = "blue") +
  geom_hline(yintercept = 100, color = "red") +
  geom_point(alpha = 0.5) +
  facet_wrap(.~compound.remove)

ggplot(stoich.diff, aes(x = N.S.tot.none, y = N.S.tot.perc.diff)) +
  geom_hline(yintercept = 0, color = "blue") +
  geom_hline(yintercept = 100, color = "red") +
  geom_point(alpha = 0.5) +
  facet_wrap(.~compound.remove)
      
#############


####### Stoich difference bins:x
stoich.diff.bins <- stoich.diff %>%
  mutate(N.S.tot.bin = case_when(N.S.tot.none < 1 ~ "0-1",
                                 N.S.tot.none < 1.5 & N.S.tot.none > 1 ~ "1-1.5",
                                 N.S.tot.none < 2 & N.S.tot.none > 1.5 ~ "1.5-2",
                                 N.S.tot.none < 2.5 & N.S.tot.none > 2 ~ "2-2.5",
                                 N.S.tot.none < 3 & N.S.tot.none > 2.5 ~ "2.5-3",
                                 N.S.tot.none > 3  ~ ">3")) %>%
  filter(compound.remove %in% c("Gonyol", "Dimethylsulfoniopropionate", "Isethionic acid",
                                "(R)-2,3-Dihydroxypropane-1-sulfonate", "Dimethylsulfonioacetate", "Taurine")) %>%
  rename(Compound = compound.remove) %>%
  left_join(., compound.order)
         
ggplot(stoich.diff.bins, aes(x = Lat, y = N.S.tot.perc.diff)) +
  geom_smooth(alpha = 0.3) +
  geom_hline(yintercept = 0, color = "blue") +
  geom_hline(yintercept = 100, color = "red") +
  geom_point(alpha = 0.5) +
  facet_wrap(.~compound.name.figure) +
  ylim(0, 150)

ggplot(stoich.diff.bins, aes(x = Lat, y = N.S.tot.quot)) +
  geom_smooth(alpha = 0.3) +
#  geom_hline(yintercept = 0, color = "blue") +
 # geom_hline(yintercept = 100, color = "red") +
  geom_point(alpha = 0.5)  +
  facet_wrap(.~compound.name.figure) 


ggplot(stoich.diff.bins, aes(x = reorder(compound.name.figure, -N.S.tot.perc.diff), y = N.S.tot.perc.diff)) +
  geom_boxplot() +
  geom_jitter(width = 0.05, height = 0, alpha = 0.5) +
  facet_grid(.~N.S.tot.bin) +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.7)) 



gradients.sulfur.percent <- gradients %>%
  group_by(SampID) %>%
  mutate(Perc_S = nM_S/sum(nM_S, na.rm = TRUE)*100) %>%
  filter(Compound %in% c("Gonyol", "Dimethylsulfoniopropionate", "Isethionic acid",
                                "(R)-2,3-Dihydroxypropane-1-sulfonate", "Dimethylsulfonioacetate", "Taurine")) %>%
  left_join(., compound.order) %>%
  filter(Cruise %in% c("TN397", "KM1906"))

ggplot(gradients.sulfur.percent, aes(x = Lat, y = Perc_S)) +
  geom_smooth(alpha = 0.3) +
  #  geom_hline(yintercept = 0, color = "blue") +
  # geom_hline(yintercept = 100, color = "red") +
  geom_point(alpha = 0.5)  +
  facet_wrap(.~compound.name.figure) 





stoich.diff.compare <- stoich.diff %>%
  left_join(stoich.diff %>%
              filter(compound.remove == "none") %>%
              select(-compound.remove) %>%
              rename(C.tot.none = C.tot,
                     N.tot.none = N.tot,
                     S.tot.none = S.tot,
                     C.N.tot.none = C.N.tot,
                     C.S.tot.none = C.S.tot,
                     N.S.tot.none = N.S.tot,
                     C.comp.none = C.comp,
                     N.comp.none = N.comp,
                     S.comp.none = S.comp,
                     C.N.comp.none = C.N.comp,
                     C.S.comp.none = C.S.comp,
                     N.S.comp.none = N.S.comp) %>%
              select(SampID, C.tot.none, N.tot.none, S.tot.none, C.N.tot.none, C.S.tot.none, N.S.tot.none,
                     C.comp.none, N.comp.none, C.N.comp.none, C.S.comp.none, N.S.comp.none) %>% unique())


ggplot(stoich.diff.compare, aes(x = N.S.tot.none, y = N.S.tot)) +
  geom_point() +
  facet_wrap(.~compound.remove)







