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
dat.betaines <- left_join(part.dat, meta.dat) %>%
  rename("Compound" = Name) %>%
  filter(Cruise %in% c("KM1906", "RC078", "TN397")) %>%
  filter(Compound %in% compound.order$Compound) %>%
  left_join(., compound.order) %>%
  filter(class == "Betaine") 

dat.betaines.plot <- dat.betaines %>%
  left_join(dat.betaines %>%
              filter(Compound == "Glycine betaine") %>%
              select(SampID, nM.in.smp) %>%
              rename(GBT.in.smp = nM.in.smp)) %>%
  group_by(Compound) %>%
  mutate(gbt.cor = cor(nM.in.smp, GBT.in.smp), 
         gbt.cor.log = cor(log10(nM.in.smp), log10(GBT.in.smp)))

ggplot(dat.betaines.plot, aes(x = GBT.in.smp, y = nM.in.smp)) +
  geom_point(aes(fill = Cruise), shape = 21) +
  geom_smooth(method = "lm", color = "black") +
  facet_wrap(.~Compound, scales = "free") +
  scale_x_log10() +
  scale_y_log10()




####Dat.all
####
dat.all <- left_join(part.dat, meta.dat) %>%
  rename("Compound" = Name) %>%
  filter(Cruise %in% c("KM1906", "TN397", "RC078")) %>%
  filter(Compound %in% compound.order$Compound) %>%
  left_join(., compound.order)

dat.all.plot <- dat.all %>%
  left_join(dat.betaines %>%
              filter(Compound == "Glycine betaine") %>%
              select(SampID, nM.in.smp) %>%
              rename(GBT.in.smp = nM.in.smp)) %>%
  group_by(Compound) %>%
  mutate(gbt.cor = cor(nM.in.smp, GBT.in.smp), 
         gbt.cor.log = cor(log10(nM.in.smp), log10(GBT.in.smp), use = "pairwise.complete.obs"),
         gbt.log.r2 = gbt.cor.log^2,
         median.nM = median(nM.in.smp, na.rm = TRUE))

ggplot(dat.all.plot, aes(x = GBT.in.smp, y = nM.in.smp)) +
  geom_point(aes(fill = Cruise), shape = 21) +
  geom_smooth(method = "lm", color = "black") +
  facet_wrap(.~Compound, scales = "free") +
  scale_x_log10() +
  scale_y_log10()

ggplot(dat.all.plot, aes(x = GBT.in.smp, y = nM.in.smp)) +
  geom_point(aes(fill = reorder(Compound, order)), shape = 21, size = 2, stroke = 0.15) +
  geom_smooth(method = "lm", aes(color = reorder(Compound, order))) +
  scale_fill_manual(values = compound.pal) +
  scale_color_manual(values = compound.pal) +
  facet_wrap(.~class, scales = "free") +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw() 


dat.plot.cor <- dat.all.plot %>%
  select(Compound, gbt.cor, gbt.cor.log, median.nM, gbt.log.r2) %>%
  unique() %>%
  left_join(., compound.order) %>%
  filter(!is.na(class)) %>%
  mutate(cor_bin = case_when(gbt.cor.log > 0.8 ~ "1.0-0.8",
                             gbt.cor.log > 0.6 & gbt.cor.log < 0.8 ~ "0.6-0.8",
                             gbt.cor.log > 0.4 & gbt.cor.log < 0.6 ~ "0.4-0.6",
                             gbt.cor.log > 0.2 & gbt.cor.log < 0.4 ~ "0.2-0.4",
                             TRUE ~ "0-0.2"),
         r2_bin = case_when(gbt.log.r2 > 0.8 ~ "1.0-0.8",
                             gbt.log.r2 > 0.6 & gbt.log.r2 < 0.8 ~ "0.6-0.8",
                             gbt.log.r2 > 0.4 & gbt.log.r2 < 0.6 ~ "0.4-0.6",
                             gbt.log.r2 > 0.2 & gbt.log.r2 < 0.4 ~ "0.2-0.4",
                             TRUE ~ "0-0.2"))

ggplot(dat.plot.cor, aes(y = gbt.cor.log, x = reorder(compound.name.figure, -gbt.cor.log), fill = reorder(compound.name.figure, order)))  +
  geom_segment( aes(x=compound.name.figure, xend=compound.name.figure, y=0, yend=gbt.cor.log, color = compound.name.figure), show.legend = FALSE) +
  geom_point(color = "black", stroke = 0.1, shape = 21, size = 3.5) +
  theme_test() +
  scale_y_continuous(expand = c(0,NA,NA,NA), limits = c(0,1.1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9),
        panel.border = element_blank(), 
        axis.line = element_line()) +
  xlab("Compound") +
  ylab("Correlation with GBT") +
  labs(fill = "Compound") +
  scale_fill_manual(values = compound.pal.fig) +
  scale_color_manual(values = compound.pal.fig)


cor.median.plot <- ggplot(dat.plot.cor, aes(y = gbt.cor.log, x = median.nM, fill = reorder(compound.name.figure, order))) +
  geom_point(shape = 21, size = 3.5, stroke = 0.15) +
  scale_fill_manual(values = compound.pal.fig) +
  scale_x_log10() +
  theme_test() +
  xlab("Median Concentration (nM)") +
  ylab("Correlation with GBT") 
cor.median.plot
ggsave(cor.median.plot, file = "R_Code/Code_Development_Workspace/Corr_Median_Plot_v1.pdf",
       height = 3.5, width = 7)


ggplot(dat.plot.cor, aes(x = cor_bin, fill = class)) +
  geom_bar(width = 0.7) +
  theme_test() +
#  scale_fill_manual(values = compound.pal.fig) +
  scale_y_continuous(expand = c(0,NA,NA,NA), limits = c(0,25)) +
  xlab("Pearson Correlation Coefficient") +
  ylab("Count")
  