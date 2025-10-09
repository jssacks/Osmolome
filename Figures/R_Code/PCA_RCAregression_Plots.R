


library(tidyverse)
library(patchwork)
library(vegan)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")



###define inputs
all.dat.file <- "Intermediates/Final_Osmo_Meta_Env_Dataframe.csv"



#Read in data
dat <- read_csv(all.dat.file) %>%
  filter(!Part.SampID == "220902_Smp_TN397_S11_600_U_C") %>%
  filter(!str_detect(Part.SampID, "Smp_S4_C1"),
         !str_detect(Part.SampID, "Smp_S4_C1"))


#Pull in data and organize for PCA Analysis:

#Particulate Surface Data
part.surf.dat <- dat %>%
  filter(Cruise %in% c("TN397", "KM1906", "RC078")) %>%
  filter(depth_m < 10) %>%
  filter(!station %in% c(1, 8)) %>%
  select(Part.SampID, Compound, Part.Conc.nM) %>%
  unique()

part.matrix <- part.surf.dat  %>%
  mutate(log10.nM.in.smp = log10(Part.Conc.nM)) %>%
  rename(SampID = Part.SampID) %>%
  select(SampID, Compound, log10.nM.in.smp) %>%
  pivot_wider(id_cols = SampID, names_from = Compound, values_from = log10.nM.in.smp) %>%
  column_to_rownames(var = "SampID")

#.    Organize Metadata:
p.metadata <- dat %>%
  filter(Cruise %in% c("TN397", "KM1906", "RC078")) %>%
  filter(depth_m < 10) %>%
  select(Part.SampID, Region, poc, sss, sst, chla, N_N) %>%
  unique() %>%
  rename(SampID = Part.SampID) 



#Dissolved Data
diss.dat <- dat %>%
  filter(Cruise %in% c("TN397", "KM1906", "RC078")) %>%
  filter(depth_m < 10) %>%
  select(Diss.SampID, Compound, Diss.Conc.nM.adj) %>%
  unique() %>%
  filter(!is.na(Diss.SampID)) %>%
  filter(!is.na(Diss.Conc.nM.adj)) %>%
  filter(!Compound == "L-Isoleucine") %>%
  filter(!Compound == "Arsenobetaine") 

diss.matrix <- diss.dat  %>%
  mutate(log10.nM.in.smp = log10(Diss.Conc.nM.adj)) %>%
  rename(SampID = Diss.SampID) %>%
  select(SampID, Compound, log10.nM.in.smp) %>%
  pivot_wider(id_cols = SampID, names_from = Compound, values_from = log10.nM.in.smp) %>%
  column_to_rownames(var = "SampID")

#.    Organize Metadata:
d.metadata <- dat %>%
  filter(Cruise %in% c("TN397", "KM1906", "RC078")) %>%
  filter(depth_m < 10) %>%
  select(Diss.SampID, Region, poc, sss, sst, chla, N_N) %>%
  unique() %>%
  rename(SampID = Diss.SampID) 




#Total Data:
tot.dat <- dat %>%
  filter(Cruise %in% c("TN397", "KM1906", "RC078")) %>%
  filter(depth_m < 10) %>%
  select(Part.SampID, Compound, Total.Conc.nM) %>%
  unique() %>%
  filter(!is.na(Part.SampID)) %>%
  filter(!is.na(Total.Conc.nM)) %>%
  filter(!Compound == "L-Isoleucine") %>%
  filter(!Compound == "Arsenobetaine") 

tot.matrix <- tot.dat  %>%
  mutate(log10.nM.in.smp = log10(Total.Conc.nM)) %>%
  rename(SampID = Part.SampID) %>%
  select(SampID, Compound, log10.nM.in.smp) %>%
  pivot_wider(id_cols = SampID, names_from = Compound, values_from = log10.nM.in.smp) %>%
  column_to_rownames(var = "SampID")

t.metadata <- dat %>%
  filter(Cruise %in% c("TN397", "KM1906", "RC078")) %>%
  filter(depth_m < 10) %>%
  select(Part.SampID, Region, poc, sss, sst, chla, N_N) %>%
  unique() %>%
  rename(SampID = Part.SampID) 


# Run PCA Analysis --------------------------------------------------------

###Particulate:

p.matrix.stand <- decostand(part.matrix, method = "range", MARGIN = 2)

p.pca <- rda(p.matrix.stand, scale = TRUE)

print(p.pca)
summary(p.pca)
p.pca.comp <- data.frame(p.pca[["CA"]][["v"]])
p.pca.samp <- data.frame(p.pca[["CA"]][["u"]])

p.pca.samp.plot <- p.pca.samp %>%
  rownames_to_column(var = "SampID") %>%
  left_join(p.metadata)



###Dissolved:
d.matrix.stand <- decostand(diss.matrix, method = "range", MARGIN = 2)

d.pca <- rda(d.matrix.stand, scale = TRUE)

print(d.pca)
summary(d.pca)
d.pca.comp <- data.frame(d.pca[["CA"]][["v"]])
d.pca.samp <- data.frame(d.pca[["CA"]][["u"]])

d.pca.samp.plot <- d.pca.samp %>%
  rownames_to_column(var = "SampID") %>%
  left_join(d.metadata)



###Total:
t.matrix.stand <- decostand(tot.matrix, method = "range", MARGIN = 2)

t.pca <- rda(t.matrix.stand, scale = TRUE)

print(t.pca)
summary(t.pca)
t.pca.comp <- data.frame(t.pca[["CA"]][["v"]])
t.pca.samp <- data.frame(t.pca[["CA"]][["u"]])

t.pca.samp.plot <- t.pca.samp %>%
  rownames_to_column(var = "SampID") %>%
  left_join(t.metadata)


##Perform PCA Regressions:

##PCA Regression of PC1 vs. POC

#particulate:
p.poc.lm <- lm(PC1 ~ log10(poc), p.pca.samp.plot)
summary(p.poc.lm)

#dissolved:
d.poc.lm <- lm(PC1 ~ log10(poc), d.pca.samp.plot)
summary(d.poc.lm)

#total
t.poc.lm <- lm(PC1 ~ log10(poc), t.pca.samp.plot)
summary(t.poc.lm)







####Plot all of the PCA plots:

#Particulate:
p.pca.plot <- ggplot(p.pca.samp.plot, aes(x = PC1, y = PC2, shape = Cruise)) +
  geom_point(aes(fill = Region), stroke = 0.15, size = 2.5, shape = 21) +
  #  scale_shape_manual(values = c(22, 21, 23)) +
  scale_fill_manual(values = region.palette.2) +
  ylab("PC2 (8%)") +
  theme_test() +
  theme(panel.border = element_blank(), axis.line = element_line(), 
        axis.text.x = element_blank(), axis.title.x = element_blank(), 
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Particulate")
p.pca.plot


#Dissolved:
d.pca.plot <- ggplot(d.pca.samp.plot, aes(x = PC1, y = PC2, shape = Cruise)) +
  geom_point(aes(fill = Region), stroke = 0.15, size = 2.5, shape = 21) +
  #  scale_shape_manual(values = c(22, 21, 23)) +
  scale_fill_manual(values = region.palette.2) +
  ylab("PC2 (6%)") +
  theme_test() +
  theme(panel.border = element_blank(), axis.line = element_line(), 
        axis.text.x = element_blank(), axis.title.x = element_blank(), 
  plot.title = element_text(hjust = 0.5)) +
  ggtitle("Dissolved")
d.pca.plot


#Total:
t.pca.plot <- ggplot(t.pca.samp.plot, aes(x = PC1, y = PC2, shape = Cruise)) +
  geom_point(aes(fill = Region), stroke = 0.15, size = 2.5, shape = 21) +
  #  scale_shape_manual(values = c(22, 21, 23)) +
  scale_fill_manual(values = region.palette.2) +
  ylab("PC2 (7%)") +
  theme_test() +
  theme(panel.border = element_blank(), axis.line = element_line(), 
        axis.text.x = element_blank(), axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Total")
t.pca.plot


####Plot PC1 vs. POC:

#particulate:
p.poc.pca.fig <- ggplot(p.pca.samp.plot, aes(x = PC1, y = poc)) +
  geom_smooth(method = "lm", color = "black") +
  geom_point(shape = 21, aes(fill = Region), size =2.5, stroke = 0.15) +
  scale_y_log10() +
  scale_fill_manual(values = region.palette.2) +
  theme_test() +
  ylab("POC (uM)") +
  xlab("PC1 (70%)") +
  theme(panel.border = element_blank(), axis.line = element_line()) +
  annotate("text", x = 0.15, y = 2.5, 
           label = expression(R^2~"="~0.89),
           size = 4) 
p.poc.pca.fig

#dissolved
d.poc.pca.fig <- ggplot(d.pca.samp.plot, aes(x = PC1, y = poc)) +
  geom_smooth(method = "lm", color = "black") +
  geom_point(shape = 21, aes(fill = Region), size =2.5, stroke = 0.15) +
  scale_y_log10() +
  scale_fill_manual(values = region.palette.2) +
  theme_test() +
  ylab("POC (uM)") +
  xlab("PC1 (52%)") +
  theme(panel.border = element_blank(), axis.line = element_line()) +
  annotate("text", x = 0.15, y = 2.5, 
           label = expression(R^2~"="~0.81),
           size = 4) 
d.poc.pca.fig

#total
t.poc.pca.fig <- ggplot(t.pca.samp.plot, aes(x = PC1, y = poc)) +
  geom_smooth(method = "lm", color = "black") +
  geom_point(shape = 21, aes(fill = Region), size =2.5, stroke = 0.15) +
  scale_y_log10() +
  scale_fill_manual(values = region.palette.2) +
  theme_test() +
  ylab("POC (uM)") +
  xlab("PC1 (63%)") +
  theme(panel.border = element_blank(), axis.line = element_line()) +
  annotate("text", x = 0.15, y = 2.5, 
           label = expression(R^2~"="~0.87),
           size = 4) 
t.poc.pca.fig



###Run correlations plots for each compound:



##particulate:
part.corr.dat <- part.surf.dat %>%
  group_by(Part.SampID) %>%
  mutate(Glu.Conc = Part.Conc.nM[Compound == "L-Glutamic acid"]) %>%
  group_by(Compound) %>%
  mutate(glu_cor = cor(log10(Part.Conc.nM), log10(Glu.Conc))) %>%
  left_join(., compound.order)

part.corr.dat.sml <- part.corr.dat %>% 
  filter(!Compound == "L-Glutamic acid") %>%
  select(compound.name.figure, class, glu_cor) %>%
  unique() 

part.corr.histo <- ggplot(part.corr.dat.sml, aes(x = glu_cor, fill = class)) + 
  geom_histogram(binwidth = 0.1, color = "black") +
  scale_fill_manual(values = class.pal) +
  theme_test() +
  scale_y_continuous(expand = c(0, NA, NA, 14), limits = c(0,14)) +
  theme() + 
  xlab("Correlation with Glu (r)") + 
  ylab("Count")
part.corr.histo

part.corr.scatter <- ggplot(part.corr.dat, aes(x = Glu.Conc, y = Part.Conc.nM)) +
  geom_smooth(method = "lm", aes(color = reorder(compound.name.figure, order)), se = FALSE) +
  geom_point(shape = 21, stroke = 0.15, aes(fill = reorder(compound.name.figure, order)), show.legend = FALSE) +
  facet_wrap(.~class, scales = "free") +
  scale_fill_manual(values = compound.pal.fig) +
  scale_color_manual(values = compound.pal.fig) +
  scale_x_log10() +
  scale_y_log10() +
  theme_test() +
  theme(strip.background = element_blank()) +
  xlab("Glutamic Acid Concentration (nM)") +
  ylab("Osmolyte Concentration (nM)") +
  labs(color = "Compound") + 
  guides(color = guide_legend(ncol = 2))
part.corr.scatter




##dissolved:
diss.corr.dat <- diss.dat %>%
  group_by(Diss.SampID) %>%
  mutate(Glu.Conc = Diss.Conc.nM.adj[Compound == "L-Glutamic acid"]) %>%
  group_by(Compound) %>%
  mutate(glu_cor = cor(log10(Diss.Conc.nM.adj), log10(Glu.Conc))) %>%
  left_join(., compound.order)

diss.corr.dat.sml <- diss.corr.dat %>% 
  filter(!Compound == "L-Glutamic acid") %>%
  select(compound.name.figure, class, glu_cor) %>%
  unique() 

diss.corr.histo <- ggplot(diss.corr.dat.sml, aes(x = glu_cor, fill = class)) + 
  geom_histogram(binwidth = 0.1, color = "black") +
  scale_fill_manual(values = class.pal) +
  theme_test() +
  scale_y_continuous(expand = c(0, NA, NA, 10), limits = c(0,8)) +
  theme() + 
  xlab("Correlation with Glu (r)") + 
  ylab("Count") +
  theme(legend.position = "none")
diss.corr.histo

diss.corr.scatter <- ggplot(diss.corr.dat, aes(x = Glu.Conc, y = Diss.Conc.nM.adj)) +
  geom_smooth(method = "lm", aes(color = reorder(compound.name.figure, order)), se = FALSE) +
  geom_point(shape = 21, stroke = 0.15, aes(fill = reorder(compound.name.figure, order)), show.legend = FALSE) +
  facet_wrap(.~class, scales = "free") +
  scale_fill_manual(values = compound.pal.fig) +
  scale_color_manual(values = compound.pal.fig) +
  scale_x_log10() +
  scale_y_log10() +
  theme_test() +
  theme(strip.background = element_blank()) +
  xlab("Glutamic Acid Concentration (nM)") +
  ylab("Osmolyte Concentration (nM)") +
  labs(color = "Compound") + 
  guides(color = guide_legend(ncol = 2))
diss.corr.scatter




##total:
total.corr.dat <- tot.dat %>%
  group_by(Part.SampID) %>%
  mutate(Glu.Conc = Total.Conc.nM[Compound == "L-Glutamic acid"]) %>%
  group_by(Compound) %>%
  mutate(glu_cor = cor(log10(Total.Conc.nM), log10(Glu.Conc))) %>%
  left_join(., compound.order)

total.corr.dat.sml <- total.corr.dat %>% 
  filter(!Compound == "L-Glutamic acid") %>%
  select(compound.name.figure, class, glu_cor) %>%
  unique() 

total.corr.histo <- ggplot(total.corr.dat.sml, aes(x = glu_cor, fill = class)) + 
  geom_histogram(binwidth = 0.1, color = "black") +
  scale_fill_manual(values = class.pal) +
  theme_test() +
  scale_y_continuous(expand = c(0, NA, NA, 10), limits = c(0,6)) +
  theme() + 
  xlab("Correlation with Glu (r)") + 
  ylab("Count") +
  theme(legend.position = "none")
total.corr.histo

total.corr.scatter <- ggplot(total.corr.dat, aes(x = Glu.Conc, y = Total.Conc.nM)) +
  geom_smooth(method = "lm", aes(color = reorder(compound.name.figure, order)), se = FALSE) +
  geom_point(shape = 21, stroke = 0.15, aes(fill = reorder(compound.name.figure, order)), show.legend = FALSE) +
  facet_wrap(.~class, scales = "free") +
  scale_fill_manual(values = compound.pal.fig) +
  scale_color_manual(values = compound.pal.fig) +
  scale_x_log10() +
  scale_y_log10() +
  theme_test() +
  theme(strip.background = element_blank()) +
  xlab("Glutamic Acid Concentration (nM)") +
  ylab("Osmolyte Concentration (nM)") +
  labs(color = "Compound") + 
  guides(color = guide_legend(ncol = 2)) 
total.corr.scatter











####Combine all plots:
pca.big.plot <- (p.pca.plot | d.pca.plot | t.pca.plot)/(p.poc.pca.fig | d.poc.pca.fig | t.poc.pca.fig)/
  (part.corr.histo | diss.corr.histo | total.corr.histo) +
  plot_layout(guides = "collect", heights = c(0.38, 0.38, 0.24)) + 
  plot_annotation(tag_levels = "A")
pca.big.plot


##Save:
ggsave(pca.big.plot, file = "Figures/Outputs/PCA_Part_Diss_Total_Regression.png", 
       height = 6, width = 7, scale = 1.4, dpi = 600)


###Save scatter plots:
ggsave(part.corr.scatter, file = "Figures/Outputs/Part_Glu_Cor_Scatter.png", 
       height = 6, width = 8, scale = 1.2, dpi = 600)


ggsave(diss.corr.scatter, file = "Figures/Outputs/Diss_Glu_Cor_Scatter.png", 
       height = 6, width = 8, scale = 1.2, dpi = 600)


ggsave(total.corr.scatter, file = "Figures/Outputs/Total_Glu_Cor_Scatter.png", 
       height = 6, width = 8, scale = 1.2, dpi = 600)





#######Figures for presentation:
#Particulate:
p.pca.plot.pres <- ggplot(p.pca.samp.plot, aes(x = PC1, y = PC2, shape = Cruise)) +
  geom_point(aes(fill = Region), stroke = 0.15, size = 2.5, shape = 21) +
  #  scale_shape_manual(values = c(22, 21, 23)) +
  scale_fill_manual(values = region.palette.2) +
  ylab("PC2 (8%)") +
  xlab("PC1 (70%)") +
  theme_test() +
  theme(panel.border = element_blank(), axis.line = element_line(), 
       # axis.text.x = element_blank(), axis.title.x = element_blank(), 
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Particulate")
p.pca.plot.pres


#Dissolved:
d.pca.plot.pres <- ggplot(d.pca.samp.plot, aes(x = PC1, y = PC2, shape = Cruise)) +
  geom_point(aes(fill = Region), stroke = 0.15, size = 2.5, shape = 21) +
  #  scale_shape_manual(values = c(22, 21, 23)) +
  scale_fill_manual(values = region.palette.2) +
  ylab("PC2 (6%)") +
  xlab("PC1 (52%)") +
  theme_test() +
  theme(panel.border = element_blank(), axis.line = element_line(), 
       # axis.text.x = element_blank(), axis.title.x = element_blank(), 
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Dissolved")
d.pca.plot.pres


pres.pca <- p.pca.plot.pres + d.pca.plot.pres + plot_layout(guides = "collect")
pres.pca

ggsave(pres.pca, file = "Figures/Outputs/pres_pca.pdf", 
       height = 3.5, width = 7, scale = 1, dpi = 600)


###Save scatter plots for presentation:
ggsave(part.corr.scatter, file = "Figures/Outputs/Pres_Part_Glu_Cor_Scatter.pdf", 
       height = 4.5, width = 9, scale = 1.2, dpi = 600)

