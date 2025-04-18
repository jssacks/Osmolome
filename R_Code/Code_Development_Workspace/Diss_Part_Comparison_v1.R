


library(tidyverse)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")


##Define inputs:
diss.file <- "Intermediates/Dissolved_Quantified_Data.csv"
part.file <- "Intermediates/Particulate_Quant_Output.csv"
meta.file <- "Intermediates/All_metadata_information.csv"
match.file <- "Intermediates/Particulate_Dissolved_Matching_Key.csv"




###load in data and combine with metadata
diss.dat <- read_csv(diss.file)
part.dat <- read_csv(part.file)
meta.dat <- read_csv(meta.file)
match.key <- read_csv(match.file)


#organize particulate surface data:
p.dat <- left_join(part.dat, meta.dat) %>%
  rename("Compound" = Name) %>%
  filter(Compound %in% compound.order$Compound) %>%
  left_join(., compound.order) %>%
  # filter(!station == 4) %>%
  filter(!SampID == "220902_Smp_TN397_S11_600_U_C") %>%
  unique() %>%
  # filter(Cruise %in% c( "KM1906", "TN397", "RC078")) %>%
  filter(!Compound == "Threonine Betaine (tentative)") %>%
  filter(!Compound == "Homoserine Betaine (tentative)") %>%
  filter(depth_m < 16) %>%
  mutate(Region = case_when(Cruise == "RC078" ~ "PS",
                            Lat > 36 ~ "NPTZ",
                            Lat < 7 ~ "Equator",
                            Cruise == "TN397" & Lat > 29 ~ "CC",
                            TRUE ~ "NPSG")) %>%
  filter(!station %in% c(1, 8)) %>%
  select(Compound, SampID, Cruise, nM.in.smp, Lat, Long, station, Treatment, order, class, compound.name.figure, Region) %>%
  rename(sampID.part = SampID,
         nM.in.smp.part = nM.in.smp) %>%
  full_join(., match.key)



#organize dissolved surface data:
d.dat <- diss.dat %>%
  filter(!str_detect(SampID, "Blk"),
         !str_detect(SampID, "blk"),
         !str_detect(SampID, "Poo"),
         !str_detect(SampID, "Mix"),
         !str_detect(SampID, "UKH"),
         !str_detect(SampID, "UKG")) %>%
  select(Name, SampID, Nmol.in.smp) %>%
  rename(Compound = Name,
         sampID.diss = SampID,
         nM.in.smp.diss = Nmol.in.smp) %>%
  full_join(., match.key)



###Combine datasets:
combine.dat <- full_join(d.dat, p.dat) %>%
  filter(!is.na(nM.in.smp.diss)) %>%
  filter(!is.na(nM.in.smp.part)) %>%
  mutate(remove = case_when(Compound == "Glycine betaine" & Cruise %in% c("KM1906", "G3_DepthProfiles") ~ "yes",
                             TRUE ~ "no")) %>%
  filter(remove == "no" ) %>%
  select(-remove)

write_csv(combine.dat, file = "Intermediates/Diss_Part_Metab_Combined.csv")

combine.dat.no.PS <- combine.dat %>%
  filter(!Region == "PS")

#All
ggplot(combine.dat, aes(x = nM.in.smp.part, y = nM.in.smp.diss)) +
  geom_smooth(method = "lm", color = "black") +
  geom_point(aes(fill = Region), shape = 21, stroke = 0.15) +
#  facet_wrap(.~compound.name.figure, scales = "free") +
  scale_fill_manual(values = region.palette) +
  scale_y_log10() +
  scale_x_log10() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  xlab("Particulate Concentration (nM)") +
  ylab("Dissolved Concentration (nM)")



ggplot(combine.dat, aes(x = reorder(SampID.shared, nM.in.smp.part))) +
  geom_point(aes(y = nM.in.smp.part), color = "darkgreen") +
  geom_point(aes(y = nM.in.smp.diss), color = "steelblue") +
  facet_wrap(.~compound.name.figure, scales = "free") +
  scale_y_log10()










#No PS
ggplot(combine.dat.no.PS, aes(x = nM.in.smp.part, y = nM.in.smp.diss)) +
  geom_smooth(method = "lm", color = "black") +
  geom_point(aes(fill = Region), shape = 21, stroke = 0.15) +
  facet_wrap(.~Compound, scales = "free") +
  scale_fill_manual(values = region.palette) +
  scale_y_log10() +
  scale_x_log10() +
  geom_abline(slope = 1, intercept = 0)



dmsp.dat <- combine.dat %>%
  filter(compound.name.figure == "DMSP")

ggplot(dmsp.dat, aes(x = nM.in.smp.part, y = nM.in.smp.diss)) +
  geom_smooth(method = "lm", color = "black") +
  geom_point(aes(fill = Region), shape = 21, stroke = 0.15, size = 3) +
  #facet_wrap(.~Compound, scales = "free") +
  scale_fill_manual(values = region.palette) +
#  scale_y_log10() +
#  scale_x_log10() +
  geom_abline(slope = 1, intercept = 0) +
  xlab("Particulate Concentration (nM)") +
  ylab("Dissolved Concentration (nM)")

###look at % dissolved vs. concentration:

perc.diss.dat <- combine.dat %>%
#  filter(!compound.name.figure == "DMSP") %>%
  mutate(Perc.diss = nM.in.smp.diss/(nM.in.smp.diss+nM.in.smp.part)*100)

#particulate
ggplot(perc.diss.dat, aes(x=nM.in.smp.part, y = Perc.diss)) +
  geom_point(aes(fill = Region), shape = 21, stroke = 0.15) +
  facet_wrap(.~Compound, scales = "free_x") +
  geom_hline(yintercept = 50, linetype = "dashed") +
  scale_fill_manual(values = region.palette) +
  scale_x_log10()

#dissolved
ggplot(perc.diss.dat, aes(x=nM.in.smp.diss, y = Perc.diss)) +
  geom_point(aes(fill = Region), shape = 21, stroke = 0.15) +
  facet_wrap(.~Compound, scales = "free_x") +
  geom_hline(yintercept = 50, linetype = "dashed") +
  scale_fill_manual(values = region.palette) +
  scale_x_log10()





##Cor values 
cor.analysis <- combine.dat %>%
  group_by(Compound) %>%
  reframe(dp.cor = cor(log10(nM.in.smp.diss), log10(nM.in.smp.part)),
          R2 = dp.cor^2)




###Make plot of total:
combine.dat.tot <- combine.dat %>%
  filter(!Cruise %in% c("KM1906", "G3_DepthProfiles")) %>%
  filter(!compound.name.figure == "DMSP") %>%
  group_by(SampID.shared, Region, Cruise) %>%
  reframe(tot.p.nM = sum(nM.in.smp.part),
         tot.d.nM = sum(nM.in.smp.diss),
         tot.sum.conc = tot.p.nM+tot.d.nM,
         perc.diss = (tot.d.nM/tot.sum.conc)*100) 

sum.tot <- combine.dat.tot %>%
  reframe(mean.perc = mean(perc.diss),
          mean.conc = mean(tot.d.nM))

total.plot <- ggplot(combine.dat.tot, aes(x = tot.p.nM, y = tot.d.nM)) +
  geom_abline(slope = 1, intercept = 0) +
  geom_smooth(color = "black", method = "lm") +
  geom_point(shape = 21, aes(fill = Region), stroke = 0.15, size = 3) +
  scale_fill_manual(values = region.palette) +
  scale_y_log10(limits = c(1,350)) +
  scale_x_log10(limits = c(1,350)) +
  theme_bw() +
  xlab("Total Particulate Concentration (nM)") +
  ylab("Total Dissolved Concentration (nM)")

ggsave(total.plot, file = "R_Code/Code_Development_Workspace/Total_Plot.pdf",
       height = 3.5, width = 4.25, scale = 1.25)





###Arrange total plot differently:
ggplot(combine.dat.tot, aes(x = reorder(SampID.shared, tot.p.nM))) +
  geom_point(aes(y=tot.p.nM), color = "darkgreen") +
  geom_point(aes(y=tot.d.nM), color = "steelblue") +
  scale_y_log10()

ggplot(combine.dat.tot, aes(x = tot.p.nM, y = perc.diss)) +
  geom_smooth(method = "lm", color = "black") +
  geom_point(aes(fill = Region), shape = 21, stroke = 0.15, size = 3) +
  scale_x_log10() +
  scale_fill_manual(values = region.palette) +
  scale_y_continuous(limits = c(1,100)) +
  theme_bw()


###Make relative abundance plots:
relabun.dat.tot <- combine.dat %>%
  filter(!Cruise %in% c("KM1906", "G3_DepthProfiles")) %>%
  filter(!compound.name.figure == "DMSP") %>%
  group_by(SampID.shared) %>%
  mutate(tot.p.nM = sum(nM.in.smp.part),
         tot.d.nM = sum(nM.in.smp.diss),
         Rel.abun.p = nM.in.smp.part/tot.p.nM,
         Rel.abun.d = nM.in.smp.diss/tot.d.nM) 

ggplot(relabun.dat.tot, aes(x = Rel.abun.p, y = Rel.abun.d)) +
  geom_abline(slope = 1, intercept = 0) +
#  geom_smooth(color = "black", method = "lm") +
  geom_point(shape = 21, aes(fill = compound.name.figure), stroke = 0.15, size = 3) +
#  facet_wrap(.~Compound) +
  scale_fill_manual(values = compound.pal.fig) +
  theme_bw() +
 # scale_x_log10() +
 # scale_y_log10() +
  xlab("Particulate Relative Abundance") +
  ylab("Dissolved Relative Abundance ") 


###Make relative abundance plots:
relabun.dat.sum <- combine.dat %>%
  filter(!compound.name.figure %in% c("DMSP", "Arsenobetaine", "Hydroxyectoine")) %>%
  filter(!Cruise %in% c("KM1906", "G3_DepthProfiles")) %>%
  group_by(SampID.shared) %>%
  mutate(tot.p.nM = sum(nM.in.smp.part),
         tot.d.nM = sum(nM.in.smp.diss),
         Rel.abun.p = nM.in.smp.part/tot.p.nM,
         Rel.abun.d = nM.in.smp.diss/tot.d.nM) %>%
  group_by(Compound, compound.name.figure) %>%
  reframe(mean.rel.abun.p = mean(Rel.abun.p),
          sd.rel.abun.p = sd(Rel.abun.p),
          mean.rel.abun.d = mean(Rel.abun.d),
          sd.rel.abun.d = sd(Rel.abun.d)) %>%
  left_join(., compound.order)

ggplot(relabun.dat.sum, aes(x = mean.rel.abun.p, y = mean.rel.abun.d, fill = reorder(compound.name.figure, order))) +
  geom_abline(slope = 1, intercept = 0, size = 1, linetype = "dashed") +
  geom_errorbar(aes(ymin = mean.rel.abun.d-sd.rel.abun.d, ymax = mean.rel.abun.d+sd.rel.abun.d, color = compound.name.figure),
                show.legend = FALSE) +
  geom_errorbarh(aes(xmin = mean.rel.abun.p-sd.rel.abun.p, xmax = mean.rel.abun.p+sd.rel.abun.p, color = compound.name.figure),
                 show.legend = FALSE) +
  geom_point(shape = 21, stroke = 0.15, size = 5) +
  #  facet_wrap(.~Compound) +
  scale_fill_manual(values = compound.pal.fig) +
  scale_color_manual(values = compound.pal.fig) +
  theme_test() +
  # scale_x_log10() +
  # scale_y_log10() +
  xlab("Relative Abundance in Particulate") +
  ylab("Relative Abundance in Dissolved") 


ggplot(relabun.dat.sum, aes(x = mean.rel.abun.p, y = mean.rel.abun.d, fill = reorder(compound.name.figure, order))) +
  geom_abline(slope = 1, intercept = 0, size = 1, linetype = "dashed") +
  geom_errorbar(aes(ymin = mean.rel.abun.d-sd.rel.abun.d, ymax = mean.rel.abun.d+sd.rel.abun.d, color = compound.name.figure),
                show.legend = FALSE) +
  geom_errorbarh(aes(xmin = mean.rel.abun.p-sd.rel.abun.p, xmax = mean.rel.abun.p+sd.rel.abun.p, color = compound.name.figure),
                 show.legend = FALSE) +
  geom_point(shape = 21, stroke = 0.15, size = 5) +
  #  facet_wrap(.~Compound) +
  scale_fill_manual(values = compound.pal.fig) +
  scale_color_manual(values = compound.pal.fig) +
  theme_test() +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Relative Abundance in Particulate") +
  ylab("Relative Abundance in Dissolved") 









###Make relative abundance plots:
enrichment.dat.sum <- combine.dat %>%
  filter(!compound.name.figure %in% c("DMSP")) %>%
  filter(!Cruise %in% c("KM1906", "G3_DepthProfiles")) %>%
  group_by(SampID.shared) %>%
  mutate(tot.p.nM = sum(nM.in.smp.part),
         tot.d.nM = sum(nM.in.smp.diss),
         Rel.abun.p = nM.in.smp.part/tot.p.nM,
         Rel.abun.d = nM.in.smp.diss/tot.d.nM,
         norm.abun.p = nM.in.smp.part/max(nM.in.smp.part),
         norm.abun.d = nM.in.smp.diss/max(nM.in.smp.diss),
         enrich.d = Rel.abun.d/Rel.abun.p,
         norm.enrich.d = norm.abun.d/norm.abun.p) %>%
  group_by(Compound, compound.name.figure, Region) %>%
  reframe(mean.rel.abun.p = mean(Rel.abun.p),
          sd.rel.abun.p = sd(Rel.abun.p),
          mean.rel.abun.d = mean(Rel.abun.d),
          sd.rel.abun.d = sd(Rel.abun.d),
          mean.norm.p = mean(norm.abun.p),
          mean.norm.d = mean(norm.abun.d),
          mean.enrich.d = mean(enrich.d),
          mean.norm.enrich.d = mean(norm.enrich.d)) %>%
  left_join(., compound.order)

enrich.hm <- ggplot(enrichment.dat.sum, aes(x = Region, y = reorder(compound.name.figure, mean.enrich.d), fill = log2(mean.enrich.d))) +
  geom_tile(color = "black") +
  scale_fill_steps2(low = "steelblue3", mid = "white", high = "red3", n.breaks = 12, limits = c(-3, 3)) +
  labs(fill = "Log2(D/P)") +
 # scale_fill_gradient2(low = "#9966CC", mid = "white", high = "#F28500", n.breaks = 12, limits = c(-6, 6)) +
  coord_fixed() +
  scale_x_discrete(position = "top") +
  theme(axis.text.x = element_text(angle = 60, hjust = 0),
        axis.title.y = element_blank(),
        legend.key.height = unit(1, "cm")
        )

ggsave(enrich.hm, file = "R_Code/Code_Development_Workspace/D_P_ratio_hm.pdf",
       height = 5.5, width = 3, scale = 1.3)




ggplot(enrichment.dat.sum, aes(fill = Region, y = reorder(compound.name.figure, mean.enrich.d), x = log2(mean.norm.enrich.d))) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(shape = 21, stroke = 0.15, size = 3) +
  scale_fill_manual(values = region.palette) +
  theme_bw()
#  scale_fill_viridis()

library(viridis)





###Rel abundance heatmaps for dissolved and particulate:
d.norm.hm <- ggplot(enrichment.dat.sum, aes(x = Region, y = reorder(compound.name.figure, mean.norm.d), fill = mean.norm.d)) +
  geom_tile(color = "black") +
  scale_fill_viridis(option = "F", end = 0.85, trans = "sqrt", breaks = c(0.01, 0.1, 0.25, 0.5, 0.75)) +
  coord_fixed() +
  theme_test() +
  scale_x_discrete(position = "top") +
  theme(axis.text.x = element_text(angle = 60, hjust = 0),
        axis.title.y = element_blank()) +
  labs(fill = "Normalized Abundance") +
  xlab("Dissolved")

p.norm.hm <- ggplot(enrichment.dat.sum, aes(x = Region, y = reorder(compound.name.figure, mean.norm.d), fill = mean.norm.p)) +
  geom_tile(color = "black") +
  scale_fill_viridis(option = "F", end = 0.85, trans = "sqrt", breaks = c(0.01, 0.1, 0.25, 0.5, 0.75)) +
  coord_fixed() +
  theme_test() +
  scale_x_discrete(position = "top") +
  theme(axis.text.x = element_text(angle = 60, hjust = 0),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none") +
  xlab("Particulate")

enrich.hm.2 <- ggplot(enrichment.dat.sum, aes(x = Region, y = reorder(compound.name.figure, mean.norm.d), fill = log2(mean.norm.enrich.d))) +
  geom_tile(color = "black") +
  scale_fill_steps2(low = "steelblue3", mid = "white", high = "red3", n.breaks = 8, limits = c(-3, 3)) +
  labs(fill = "Log2(D/P) Norm Abun") +
  # scale_fill_gradient2(low = "#9966CC", mid = "white", high = "#F28500", n.breaks = 12, limits = c(-6, 6)) +
  coord_fixed() +
  scale_x_discrete(position = "top") +
  xlab("Difference") +
  theme(axis.text.x = element_text(angle = 60, hjust = 0),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        legend.key.height = unit(1, "cm")
  )


norm.d.p.hm <- (d.norm.hm | p.norm.hm | enrich.hm.2) + 
  plot_layout(guides = "collect")

norm.d.p.hm

ggsave(norm.d.p.hm, file = "R_Code/Code_Development_Workspace/D_P_RelAbun_hm.pdf",
       height = 5.5, width = 6, scale = 1.3)



#############Try calculating diversity indicies for dissolved and particulate:
richness.vals <- combine.dat %>%
  select(SampID.shared, Compound) %>%
  unique() %>%
  group_by(SampID.shared) %>%
  reframe(richness = n())

diversity.dat.sum <- combine.dat %>%
  filter(!compound.name.figure %in% c("DMSP")) %>%
  filter(!Cruise %in% c("KM1906", "G3_DepthProfiles")) %>%
  left_join(., richness.vals) %>%
  group_by(SampID.shared) %>%
  mutate(tot.p.nM = sum(nM.in.smp.part),
         tot.d.nM = sum(nM.in.smp.diss),
         Rel.abun.p = nM.in.smp.part/tot.p.nM,
         Rel.abun.d = nM.in.smp.diss/tot.d.nM) %>%
  group_by(SampID.shared, Region) %>%
  reframe(shannon.p = sum(Rel.abun.p*log(Rel.abun.p))*-1,
         shannon.d = sum(Rel.abun.d*log(Rel.abun.d))*-1,
         evenness.p = shannon.p/log(25),
         evenness.d = shannon.d/log(25)) %>%
  pivot_longer(cols = c("shannon.p", "shannon.d", "evenness.p", "evenness.d"))

evenness.dat <- diversity.dat.sum %>%
  filter(name %in% c("evenness.p", "evenness.d"))


ggplot(evenness.dat, aes(x = Region, y = value, color = name)) +
  geom_boxplot() +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.1))



ggplot(diversity.dat.sum, aes(x = Region, y = shannon.p)) +
  geom_boxplot() +
  geom_jitter(width = 0.1)

ggplot(diversity.dat.sum, aes(x = Region, y = shannon.d)) +
  geom_boxplot() +
  geom_jitter(width = 0.1)

ggplot(diversity.dat.sum, aes(x = Region, y = evenness.p)) +
  geom_boxplot() +
  geom_jitter(width = 0.1)

ggplot(diversity.dat.sum, aes(x = Region, y = evenness.d)) +
  geom_boxplot() +
  geom_jitter(width = 0.1)














####Make Plots of all samples:
g4.dat <- combine.dat %>%
  filter(Cruise %in% c("TN397")) %>%
  mutate(sample_station = str_replace(SampID.shared, "_A", ""),
         sample_station = str_replace(sample_station, "_B", ""),
         sample_station = str_replace(sample_station, "_C", "")) %>%
  group_by(Compound, sample_station, Lat, Long, Region) %>%
  reframe(mean.diss.nM = mean(nM.in.smp.diss)) %>%
  left_join(., compound.order)
  

ggplot(g4.dat, aes(x = reorder(sample_station, Lat), y = mean.diss.nM, fill = reorder(compound.name.figure, order))) +
  geom_col(position = "fill", color = "black", width = 0.8, size = 0.1) +
  scale_fill_manual(values = compound.pal.fig)


#No DMSP
g4.dat <- combine.dat %>%
  filter(Cruise %in% c("TN397")) %>%
  filter(!compound.name.figure %in% c("DMSP")) %>%
  mutate(sample_station = str_replace(SampID.shared, "_A", ""),
         sample_station = str_replace(sample_station, "_B", ""),
         sample_station = str_replace(sample_station, "_C", ""),
         sample_station = str_replace(sample_station, "_U", ""),
         sample_station = str_replace(sample_station, "_1800", "_PM"),
         sample_station = str_replace(sample_station, "_600", "_AM"),
         sample_station = str_replace(sample_station, "_800", "_AM2"),
         sample_station = str_replace(sample_station, "_2400", "_N"),) %>%
  group_by(Compound, sample_station, Lat, Long, Region) %>%
  reframe(mean.diss.nM = mean(nM.in.smp.diss),
          mean.part.nM = mean(nM.in.smp.part)) %>%
  left_join(., compound.order)



g4.d.plot <- ggplot(g4.dat, aes(x = reorder(sample_station, Lat), y = mean.diss.nM, fill = reorder(compound.name.figure, order))) +
  geom_col(position = "fill", color = "black", width = 0.8, size = 0.1) +
  scale_fill_manual(values = compound.pal.fig) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Dissolved Mole Fraction")
g4.d.plot

g4.p.plot <- ggplot(g4.dat, aes(x = reorder(sample_station, Lat), y = mean.part.nM, fill = reorder(compound.name.figure, order))) +
  geom_col(position = "fill", color = "black", width = 0.8, size = 0.1) +
  scale_fill_manual(values = compound.pal.fig) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Station") +
  ylab("Particulate Mole Fraction")
g4.p.plot


#No DMSP
d1.dat <- combine.dat %>%
  filter(Cruise %in% c("RC078")) %>%
  filter(!compound.name.figure %in% c("DMSP")) %>%
  mutate(sample_station = str_replace(SampID.shared, "_A", ""),
         sample_station = str_replace(sample_station, "_B", ""),
         sample_station = str_replace(sample_station, "_C", "")) %>%
  group_by(Compound, station, Lat, Long, Region) %>%
  reframe(mean.diss.nM = mean(nM.in.smp.diss),
          mean.part.nM = mean(nM.in.smp.part)) %>%
  left_join(., compound.order)


d1.d.plot <- ggplot(d1.dat, aes(x = as.factor(station), y = mean.diss.nM, fill = reorder(compound.name.figure, order))) +
  geom_col(position = "fill", color = "black", width = 0.8, size = 0.1) +
  scale_fill_manual(values = compound.pal.fig) +
  guides(fill = guide_legend(ncol = 1)) +
  labs(fill = "Compound") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank())
d1.d.plot

d1.p.plot <- ggplot(d1.dat, aes(x = as.factor(station), y = mean.part.nM, fill = reorder(compound.name.figure, order))) +
  geom_col(position = "fill", color = "black", width = 0.8, size = 0.1) +
  scale_fill_manual(values = compound.pal.fig) +
  theme_minimal() +
  theme(legend.position = "none") +
  xlab("Station") +
  theme(axis.title.y = element_blank())
  
  
d1.p.plot


####
library(patchwork)

d.p.comparison.fig <- g4.d.plot + d1.d.plot + g4.p.plot + d1.p.plot + 
  plot_layout(guides = "collect", widths = c(0.8, 0.2)) +
  plot_annotation(tag_levels = "A")

d.p.comparison.fig

ggsave(d.p.comparison.fig, 
       file = "R_Code/Code_Development_Workspace/Dissolved_Particulate_Compositional_Compariosn.pdf",
       height = 6, width = 9, scale = 1.35)


















































