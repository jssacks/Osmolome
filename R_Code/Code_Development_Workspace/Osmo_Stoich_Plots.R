

library(tidyverse)
library(lubridate)
library(patchwork)





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


surface.dat <- dat.all %>%
  filter(Cruise %in% c("TN397", "KM1906", "RC078", "PERIFIX")) %>%
  filter(Compound %in% compound.order$Compound) %>%
  left_join(., compound.order) %>%
  mutate(Local_Time = hms(Local_Time),
         hour_of_day = hour(Local_Time)) %>%
  mutate(tod = as.factor(case_when(hour_of_day > 4 & hour_of_day <= 10 ~ "morning",
                         hour_of_day > 10 & hour_of_day <= 16 ~ "midday",
                         hour_of_day > 16 & hour_of_day <= 21 ~ "evening",
                         hour_of_day > 21 & hour_of_day <= 25 ~ "night"))) %>%
  filter(!is.na(tod)) %>%
  mutate(tod = fct_relevel(tod, c("morning", "midday", "evening", "night")))


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


###RC078 Stoich plots:
d1.data <- stoich.dat %>%
  filter(Cruise == "RC078") %>% 
  select(SampID, Cruise, Lat, Long, Local_Date, Local_Time, depth_m, station,
         C.tot, N.tot, S.tot, C.N.tot, C.S.tot, N.S.tot, 
         C.comp, N.comp, S.comp, C.N.comp, C.S.comp, N.S.comp, tod) %>%
  unique() %>%
  filter(depth_m < 10) %>%
  filter(station %in% c(2, 3, 5, 6, 7)) %>%
  full_join(., tibble(Cruise = "RC078", tod = as.factor("night"), station = 2))
  # mutate(Local_Time = hms(Local_Time),
  #        hour_of_day = hour(Local_Time)) %>%
  # mutate(tod = case_when(hour_of_day > 4 & hour_of_day <= 10 ~ "morning",
  #                        hour_of_day > 10 & hour_of_day <= 16 ~ "mid-day",
  #                        hour_of_day > 16 & hour_of_day <= 21 ~ "evening",
  #                        hour_of_day > 21 & hour_of_day <= 25 ~ "night"))
  
  
ggplot(d1.data, aes(x = as.factor(station), y = C.N.tot)) +
  geom_boxplot() +
  geom_jitter(width = .1, height = 0, shape = 21, aes(fill = tod)) +
  ylim(0,NA)




######________Gradients

g.dat <- stoich.dat %>%
  filter(Cruise %in% c("TN397", "KM1906")) %>% 
  select(SampID, Cruise, Lat, Long, Local_Date, Local_Time, depth_m, tod, station,
         C.tot, N.tot, S.tot, C.N.tot, C.S.tot, N.S.tot, 
         C.comp, N.comp, S.comp, C.N.comp, C.S.comp, N.S.comp) %>%
  unique() 

g.dat.sum <- g.dat %>%
  ungroup() %>%
  reframe(mean.C.N = mean(C.N.tot),
          sd.C.N = sd(C.N.tot),
          mean.C.S = mean(C.S.tot),
          sd.C.S = sd(C.S.tot),
          mean.N.S = mean(N.S.tot),
          sd.N.S = sd(N.S.tot))



#PERIFIX
peri.dat <- stoich.dat %>%
  filter(Cruise %in% c("PERIFIX")) %>%
  dplyr::select(SampID, Cruise, Lat, Long, Local_Date, Local_Time, depth_m, tod, Treatment,
         C.tot, N.tot, S.tot, C.N.tot, C.S.tot, N.S.tot, 
         C.comp, N.comp, S.comp, C.N.comp, C.S.comp, N.S.comp) %>%
  unique() %>%
  mutate(Treatment = as.factor(Treatment),
         Treatment = fct_relevel(Treatment, c("Tote", "C", "P", "F", "PF", "NPF", "NP", "NF", "N")),
         N_status = case_when(Treatment %in% c("N", "NP", "NF", "NPF") ~ "+N",
                             TRUE ~ "-N")) %>%
  filter(!Treatment == "Tote")
  






library(viridis)

###Make plots of each gradients and PS data

#C/N
c.n.g.fig <- ggplot(g.dat, aes(x = Lat, y = C.N.tot)) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
  geom_vline(xintercept = 32.34, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 35, linetype = "dashed", color = "blue") +
  geom_smooth(color = "black") +
  geom_point(shape = 21, size = 3, stroke = 0.2, fill = "#ad494a") +
  ylim(0,16) +
 # scale_fill_viridis(discrete = TRUE, direction = -1, option = "G", begin = 0.2) +
  scale_fill_manual(values = tod.palette) +
  theme_test() +
  ylab("C/N")
c.n.g.fig

c.n.d.fig <- ggplot(d1.data, aes(x = as.factor(station), y = C.N.tot)) +
  geom_boxplot(width = 0.5) +
  geom_jitter(width = .1, height = 0, shape = 21, size = 3, stroke = 0.2, fill = "#ad494a") +
#  scale_fill_viridis(discrete = TRUE, direction = -1, option = "G", begin = 0.2) +
  scale_fill_manual(values = tod.palette) +
  theme_test()+
  ylim(0,16) +
  theme(axis.title.y = element_blank()) +
  xlab("Station")
c.n.d.fig

c.n.p.fig <- ggplot(peri.dat, aes(x = Treatment, y = C.N.tot)) +
  geom_boxplot(aes(color = N_status)) +
  scale_color_manual(values = c("gray60", "gray20")) +
  geom_jitter(width = .1, height = 0, shape = 21, fill = "#ad494a", size = 3, stroke = 0.2) +
#  scale_fill_viridis(discrete = TRUE, direction = -1, option = "G", begin = 0.2) +
  scale_fill_manual(values = tod.palette) +
  theme_test() +
  ylim(0,NA) +
  theme(axis.title.y = element_blank()) 
c.n.p.fig




#C/S
c.s.g.fig <- ggplot(g.dat, aes(x = Lat, y = C.S.tot)) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
  geom_vline(xintercept = 32.34, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 35, linetype = "dashed", color = "blue") +
  geom_smooth(color = "black") +
  geom_point(shape = 21, size = 3, stroke = 0.2, fill = "darkgray") +
  scale_fill_manual(values = tod.palette) +
  theme_test() +
  ylim(0,45) +
  ylab("C/S")
c.s.g.fig 

c.s.d.fig <- ggplot(d1.data, aes(x = as.factor(station), y = C.S.tot)) +
  geom_boxplot(width = 0.5) +
  geom_jitter(width = .1, height = 0, shape = 21, size = 3, stroke = 0.2, fill = "darkgray") +
  scale_fill_manual(values = tod.palette) +
  theme_test() +
  ylim(0,45) +
  theme(axis.title.y = element_blank()) +
  xlab("Station")
c.s.d.fig 

c.s.p.fig <- ggplot(peri.dat, aes(x = Treatment, y = C.S.tot)) +
  geom_boxplot(aes(color = N_status)) +
  scale_color_manual(values = c("gray60", "gray20")) +
  geom_jitter(width = .1, height = 0, shape = 21, size = 3, stroke = 0.2, fill = "darkgray") +
  scale_fill_manual(values = tod.palette) +
  theme_test() +
  ylim(0,NA) +
  theme(axis.title.y = element_blank()) 
c.s.p.fig

#N/S
n.s.g.fig <- ggplot(g.dat, aes(x = Lat, y = N.S.tot)) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
  geom_vline(xintercept = 32.34, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 35, linetype = "dashed", color = "blue") +
  geom_smooth(color = "black") +
  geom_point(shape = 21, size = 3, stroke = 0.2, fill = "#fdae6b") +
  scale_fill_manual(values = tod.palette) +
  theme_test() +
  ylim(0,8) +
  ylab("N/S")
n.s.g.fig 

n.s.d.fig <- ggplot(d1.data, aes(x = as.factor(station), y = N.S.tot)) +
  geom_boxplot(width = 0.5) +
  geom_jitter(width = .1, height = 0, shape = 21, size = 3, stroke = 0.2, fill = "#fdae6b") +
  scale_fill_manual(values = tod.palette) +
  theme_test() +
  theme(axis.title.y = element_blank()) +
  ylim(0,8) +
  xlab("Station")
n.s.d.fig 

###
n.s.p.fig <- ggplot(peri.dat, aes(x = Treatment, y = N.S.tot)) +
  geom_boxplot(aes(color = N_status)) +
  scale_color_manual(values = c("gray60", "gray20")) +
  geom_jitter(width = .1, height = 0, shape = 21, size = 3, stroke = 0.2, fill = "#fdae6b") +
  scale_fill_manual(values = tod.palette) +
  theme_test() +
  theme(axis.title.y = element_blank()) 
#  ylim(0,17)
n.s.p.fig



####Put plots together:
big.plot <- ((c.n.g.fig | (c.n.d.fig + c.n.p.fig)) /
            (c.s.g.fig | (c.s.d.fig + c.s.p.fig)) /
            (n.s.g.fig | (n.s.d.fig + n.s.p.fig))) +
  plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = "A")
big.plot

ggsave(big.plot, file = "R_Code/Code_Development_Workspace/Stoich_Big_Plot.pdf",
       height = 7, width = 11, scale = 1.2)



############Add in sulfur compounds:
####

##Define inputs:
part.file <- "Intermediates/Particulate_Quant_Output.csv"
meta.file <- "Intermediates/All_metadata_information.csv"

###load in data and combine with metadata
part.dat <- read_csv(part.file)
meta.dat <- read_csv(meta.file)


dat.all <- left_join(part.dat, meta.dat) %>%
  rename("Compound" = Name)


gradients <- dat.all %>%
  filter(Cruise %in% c("TN397", "KM1906")) %>%
  filter(Compound %in% compound.order$Compound) %>%
  left_join(., compound.order)



####Gradient Survey Plot
lat.breaks <- seq(-3,50, by = 1)

g3.g4.dat <- gradients %>%
  filter(Cruise %in% c("KM1906", "TN397")) %>%
  filter(Long < -124) %>%
  mutate(lat_bin = cut(Lat, breaks = lat.breaks, include.lowest = TRUE, right = FALSE)) 

g3.g4.transect.dat <- g3.g4.dat %>%
  group_by(lat_bin, Compound) %>%
  reframe(Mean_nM = mean(nM.in.smp, na.rm = TRUE),
          Mean_nM_C = mean(nM_C, na.rm = TRUE),
          Mean_nM_N = mean(nM_N, na.rm = TRUE),
          Mean_nM_S = mean(nM_S, na.rm = TRUE)) %>%
  left_join(., compound.order) 


###Just the sulfur compounds:
sulf.g.dat <- g3.g4.transect.dat %>%
  filter(class %in% c("Sulfonate", "Sulfonium", "Taurine"))

g.plot.s <- ggplot(g3.g4.transect.dat, aes(x = lat_bin, y=Mean_nM, fill = reorder(compound.name.figure, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15, position = "fill") +
  scale_fill_manual(values = compound.pal.fig)+
  guides(fill = guide_legend(ncol = 1)) +
  #  scale_fill_manual(values = stepped2(n = 20)) +
  scale_y_continuous(expand = c(0,NA,NA,NA)) +
  theme_test() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.8),
        axis.text.y = element_blank()) +
  ylab("Mole Fraction") +
  xlab("Latitude") +
  labs(fill = "Compound")
g.plot.s




###DINIMITE 1 Surface Plots
d1.dat <- dat.all %>%
  filter(Cruise %in% c("RC078")) %>%
  filter(Compound %in% compound.order$Compound) %>%
  left_join(., compound.order) %>%
  filter(depth_m < 10)

###
d1.station.dat <- d1.dat %>%
  group_by(station, Compound) %>%
  reframe(Mean_nM = mean(nM.in.smp, na.rm = TRUE),
          Mean_nM_C = mean(nM_C, na.rm = TRUE),
          Mean_nM_N = mean(nM_N, na.rm = TRUE),
          Mean_nM_S = mean(nM_S, na.rm = TRUE)) %>%
  left_join(., compound.order) %>%
  filter(station %in% c(2, 3, 5, 6, 7)) %>%
  mutate(station = as.factor(station))

###
#####Plot
##DINIMITE Plot
#Just sulfur compounds:
d1.station.dat.s <- d1.station.dat %>%
  filter(class %in% c("Sulfonate", "Sulfonium", "Taurine"))

d.plot.s <- ggplot(d1.station.dat, aes(x = station, y=Mean_nM, fill = reorder(compound.name.figure, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15, position = "fill") +
  scale_fill_manual(values = compound.pal.fig)+
  guides(fill = guide_legend(ncol = 1)) +
  #  scale_fill_manual(values = stepped2(n = 20)) +
  scale_y_continuous(expand = c(0,NA,NA,NA)) +
  theme_test() + 
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  ylab("Mole Fraction") +
  xlab("Station") +
  labs(fill = "Compound")
d.plot.s




###PERIFIX Plots
p.dat <- dat.all %>%
  filter(Cruise %in% c("PERIFIX")) %>%
  filter(Compound %in% compound.order$Compound) 

###
p.dat.mean <- p.dat %>%
  group_by(Treatment, Compound) %>%
  reframe(Mean_nM = mean(nM.in.smp, na.rm = TRUE),
          Mean_nM_C = mean(nM_C, na.rm = TRUE),
          Mean_nM_N = mean(nM_N, na.rm = TRUE),
          Mean_nM_S = mean(nM_S, na.rm = TRUE)) %>%
  left_join(., compound.order) %>%
  mutate(Treatment = as.factor(Treatment),
         Treatment = fct_relevel(Treatment, c("Tote", "C", "P", "F", "PF", "NPF", "NP", "NF", "N")),
         N_status = case_when(Treatment %in% c("N", "NP", "NF", "NPF") ~ "+N",
                              TRUE ~ "-N")) %>%
  filter(!Treatment == "Tote")


#Just sulfur compounds:
p.dat.mean.s <- p.dat.mean %>%
  filter(class %in% c("Sulfonate", "Sulfonium", "Taurine"))

p.plot.s <- ggplot(p.dat.mean, aes(x = Treatment, y=Mean_nM, fill = reorder(compound.name.figure, order))) +
  geom_col(alpha = 0.9, width = 0.7, color = "black", size = 0.15, position = "fill") +
  scale_fill_manual(values = compound.pal.fig)+
  guides(fill = guide_legend(ncol = 1)) +
  #  scale_fill_manual(values = stepped2(n = 20)) +
  scale_y_continuous(expand = c(0,NA,NA,NA)) +
  theme_test() + 
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  xlab("Treatment") +
  labs(fill = "Compound")
p.plot.s


#library(patchwork)


####Put plots together:
big.plot.s <- ((g.plot.s | (d.plot.s + p.plot.s)) /
               (c.n.g.fig | (c.n.d.fig + c.n.p.fig)) /
               (n.s.g.fig | (n.s.d.fig + n.s.p.fig))) +
  plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = "A")
big.plot.s

ggsave(big.plot.s, file = "R_Code/Code_Development_Workspace/Stoich_Big_Plot_with_comp.pdf",
       height = 9, width = 12, scale = 1.2)

















###Test: 


#define inputs:
g3.enviro.file <- "Intermediates/G3_metadata_with_interpolations.csv"
g4.enviro.file <- "Intermediates/G4_MetaData_Interpolated.csv"
meta.file <- "Intermediates/All_metadata_information.csv"



#Compile metadata and match with environmental information:



#work on just G4 to start
meta.dat.samp <- read_csv(meta.file) %>%
  #filter(Cruise %in) %>%
  unite(c("Local_Date", "Local_Time"), col = "Local_DT", remove = FALSE, sep = " ") %>%
  mutate(Local_DT_obj = dmy_hms(Local_DT)) %>%
  mutate(time.round = round_date(Local_DT_obj, unit = "hour"))

g4.enviro.dat <- read_csv(g4.enviro.file) %>%
  rename(time.round = time) 

ggplot(g4.enviro.dat, aes(x = lat, y = N_N)) +
  geom_point() +
  scale_y_log10()

g4.meta.dat <- left_join(meta.dat.g4, g4.enviro.dat) %>%
  mutate(c_n_ratio = pc_interp/pn_interp)

ggplot(g4.meta.dat, aes(x = Lat, y = c_n_ratio, color = Long)) +
  geom_jitter()


#
g.dat.nut <- left_join(g4.meta.dat, g.dat %>%
                         select(SampID, C.tot, S.tot, N.tot, C.N.tot, C.S.tot, N.S.tot)) #%>%
  filter(N.S.tot > 2)

  
g.dat.POS <- g.dat.nut %>%
  select(SampID, S.tot, pc_interp, pn_interp) %>%
  mutate(PS_est_c = pc_interp/95*1000,
         percent_S_osmo = (S.tot/PS_est_c)*100)
  
  
  



















