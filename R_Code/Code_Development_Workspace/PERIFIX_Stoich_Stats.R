


 

#________________Perform Statistics on PERIFIX DATA:_______________

###load in data and combine with metadata
part.dat <- read_csv(part.file)
meta.dat <- read_csv(meta.file)


####
dat.all <- left_join(part.dat, meta.dat) %>%
  rename("Compound" = Name)


surface.dat <- dat.all %>%
  filter(Cruise %in% c("TN397", "KM1906", "RC078", "PERIFIX")) %>%
  filter(Compound %in% compound.order$Compound) %>%
  left_join(., compound.order) %>%
  mutate(Local_Time = hms(Local_Time),
         hour_of_day = hour(Local_Time)) %>%
  mutate(tod = case_when(hour_of_day > 4 & hour_of_day <= 10 ~ "morning",
                         hour_of_day > 10 & hour_of_day <= 16 ~ "midday",
                         hour_of_day > 16 & hour_of_day <= 21 ~ "evening",
                         hour_of_day > 21 & hour_of_day <= 25 ~ "night")) %>%
  filter(!is.na(tod))


stoich.dat <- surface.dat %>%
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
         N.S.comp = N.comp/S.comp) 


#PERIFIX
peri.dat <- stoich.dat %>%
  filter(Cruise %in% c("PERIFIX")) %>%
  dplyr::select(SampID, Cruise, Lat, Long, Local_Date, Local_Time, depth_m, tod, Treatment,
         C.tot, N.tot, S.tot, C.N.tot, C.S.tot, N.S.tot, 
         C.comp, N.comp, S.comp, C.N.comp, C.S.comp, N.S.comp) %>%
  unique() %>%
  mutate(Treatment = as.factor(Treatment),
         Treatment = fct_relevel(Treatment, c("Tote", "C", "P", "F", "PF", "NPF", "NP", "NF", "N")),
         N = as.factor(case_when(Treatment %in% c("N", "NP", "NF", "NPF") ~ 1,
                              TRUE ~ 0)), 
         P = as.factor(case_when(Treatment %in% c("P", "NP", "PF", "NPF") ~ 1,
                              TRUE ~ 0)), 
         Fe = as.factor(case_when(Treatment %in% c("F", "PF", "NF", "NPF") ~ 1,
                              TRUE ~ 0))) %>%
  filter(!Treatment == "Tote")


#C/N

#ANOVA:
anova.C.N <- aov(C.N.tot ~ N*P*Fe, data = peri.dat)  
summary(anova.C.N)

#Post-Hoc Test:
tuk.C.N <- TukeyHSD(anova.C.N, which = "N")
tuk.C.N

#C/S:

#ANOVA:
anova.C.S <- aov(C.S.tot ~ N*P*Fe, data = peri.dat)  
summary(anova.C.S)

#Post-Hoc Test:
tuk.C.S <- TukeyHSD(anova.C.S, which = "N")
tuk.C.S

#N/S:
#ANOVA:
anova.N.S <- aov(N.S.tot ~ N*P*Fe, data = peri.dat)  
summary(anova.N.S)

tuk.N.S <- TukeyHSD(anova.N.S, which = "N")
tuk.N.S


####Assign groups for each analysis:
library(multcomp)

peri.C.N.groups <- cld(C.N.tot ~ N*P*Fe, data = peri.dat)







































































