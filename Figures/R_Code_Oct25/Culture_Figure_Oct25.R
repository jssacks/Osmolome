













#load packages and source code:
library(tidyverse)
library(viridis)
library(patchwork)
library(ggsci)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")





#load and organize data:
dat.cult <- read_csv("Intermediates/Culture_Final_Quant_QCed.csv") %>%
  left_join(., compound.order) %>%
  filter(!str_detect(SampID, "Blk")) %>%
  mutate(Part.Conc.Vial.uM = case_when(Detected == "No" ~ 0,
                                       TRUE ~ Part.Conc.Vial.uM)) %>%
  mutate(Part.Conc.Vial.uM = replace_na(Part.Conc.Vial.uM, 0)) %>%
  filter(!is.na(compound.name.figure)) %>%
  filter(!is.na(Type))


#Summarize data by different groups: 
dat.cult.organism <- dat.cult %>%
  group_by(Organism, Type, class, compound.name.figure, Detected) %>%
  reframe(mean.conc.uM = mean(Part.Conc.Vial.uM)) %>%
  group_by(Type, Organism) %>%
  mutate(rel.conc = mean.conc.uM/sum(mean.conc.uM)*100) %>%
  ungroup() %>%
  mutate(rel.conc = replace_na(rel.conc, 0)) %>%
  ungroup() 



## Redefine Organism Classes:
dat.org.class.new <- dat.cult.organism %>%
  mutate(org_class = case_when(Organism == "Nmar" ~ "Archaea",
                              Type == "Bacteria" & !Organism == "Nmar" ~ "Bacteria",
                              Type == "Dino" ~ "Dinoflagellate",
                              Type == "Diatom" ~ "Diatom",
                              Type == "Haptophyte" ~ "Haptophyte",
                              Type == "Prasinophyte" ~ "Prasinophyte",
                              Organism %in% c("WH8501") ~ "Croco",
                              Organism %in% c("1314P", "As9601", "MED4", "NATL2A") ~ "Pro",
                              Organism %in% c("8102", "7803") ~ "Syn")) %>%
  mutate(org_class = as.factor(org_class)) %>%
  mutate(org_class = fct_relevel(org_class, c("Archaea", "Bacteria", "Pro", "Syn", "Croco",
                                              "Prasinophyte", "Diatom", "Haptophyte", "Dinoflagellate")))
  


## Redefine Organism Classes:





###_ Summarize relative abundances by compound:
comp.org.relabun <- dat.org.class.new %>%
  group_by(compound.name.figure, org_class, class) %>%
  reframe(mean.rel.conc = mean(rel.conc),
          max.rel.conc = max(rel.conc),
          median.rel.conc = median(rel.conc)) %>%
  mutate(mean.rel.conc.group = case_when(mean.rel.conc == 0 ~ "0",
                                         mean.rel.conc > 0 & mean.rel.conc < 0.1 ~ "< 0.1",
                                         mean.rel.conc >= 0.1 & mean.rel.conc < 1 ~ "0.1 - 1",
                                         mean.rel.conc >= 1 & mean.rel.conc < 10 ~ "1 - 10",
                                         mean.rel.conc >= 10 & mean.rel.conc < 25 ~ "10 - 25",
                                         mean.rel.conc >= 25 & mean.rel.conc < 50 ~ "25 - 50",
                                         mean.rel.conc >= 50 ~ "> 50")) %>%
  left_join(., compound.order) %>%
  mutate(mean.rel.conc.group = fct_relevel(mean.rel.conc.group, c("0", "< 0.1", "0.1 - 1", "1 - 10", "10 - 25", "25 - 50", "> 50")))


### Summarize Number of producers by organism class
comp.org.count <- dat.org.class.new %>%
  filter(Detected == "Yes") %>%
  group_by(compound.name.figure, org_class, class) %>%
  reframe(count = n()) %>%
  full_join(., tibble(compound.name.figure = "Arsenobetaine",
                      class = "Other",
                      org_class = NA,
                      count = 0)) %>%
  left_join(., compound.order) 

comp.org.count.sum <- comp.org.count %>%
  group_by(compound.name.figure) %>%
  mutate(count.sum = sum(count)) %>%
  filter(!compound.name.figure == "Arsenobetaine")

comp.org.class.count <- comp.org.count %>%
  select(compound.name.figure, org_class) %>%
  unique() %>%
  group_by(compound.name.figure) %>%
  reframe(org_class.count = n()) 
  
comp.org.count.text <- comp.org.count.sum %>%
  left_join(., comp.org.class.count) %>%
  mutate(fig.text = paste0(count.sum, " (", org_class.count, ")")) %>%
  ungroup() %>%
  select(compound.name.figure, order, class, count.sum, fig.text) %>%
  unique() %>%
  full_join(., tibble(compound.name.figure = "Arsenobetaine",
                      order = 33,
                      class = "Other",
                      count.sum = 0,
                      fig.text = "0 (0)"))






##Make Test Plots
ggplot(comp.org.relabun, aes(x = org_class, y = reorder(compound.name.figure, -order), fill = mean.rel.conc.group)) +
  geom_tile(color = "black") +
  facet_wrap(.~class, scales = "free_y", ncol = 1) + 
  scale_fill_viridis(discrete = TRUE, option = "G")

ggplot(comp.org.count) +
  geom_col(aes(x = count, y = reorder(compound.name.figure, -order), fill = org_class), color = "black", linewidth = 0.1) +
  geom_text(data = comp.org.count.text, aes(x = count.sum+5, y = reorder(compound.name.figure, -order), label = fig.text)) +
  facet_wrap(.~class, scales = "free_y") +
  theme_minimal() 






######_________Make Plots for each Organism group:_______________

###AA Data:
aa.relabun <- comp.org.relabun %>% filter(class == "AA")
aa.count <- comp.org.count %>% filter(class == "AA")
aa.count.text <- comp.org.count.text %>% filter(class == "AA")

##Plot AA Data:
aa.rel.plot <- ggplot(aa.relabun, aes(x = org_class, y = reorder(compound.name.figure, -order), fill = mean.rel.conc.group)) +
  geom_tile(color = "black") +
  coord_fixed() +
  scale_fill_viridis(discrete = TRUE, option = "G") +
  theme_test()  +
  scale_x_discrete(position = "top") +
  theme(axis.text.x = element_text(angle = 320, hjust = 1), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
 #       legend.position = "none", 
        panel.border = element_blank(),
        axis.ticks = element_blank(), 
        plot.margin = unit(c(0,0,0,0), units = "cm")) +
  geom_vline(color = "gray", xintercept = 5.5) +
  labs(fill = "Mean Percent of \nOsmolyte Pool")
aa.rel.plot

scale_fill_material_

aa.count.plot <- ggplot(aa.count) +
  geom_col(aes(x = count, y = reorder(compound.name.figure, -order), 
               fill = org_class), color = "black", linewidth = 0.1) +
  geom_text(data = aa.count.text, aes(x = count.sum+5, y = reorder(compound.name.figure, -order), label = fig.text)) +
  theme_test() +
  scale_x_continuous(position = "top", limits = c(0,50)) +
  xlab("Number of cultures") +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
#        legend.position = "none", 
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(0,0,0,0), units = "cm")) +
  scale_fill_manual(values = org.palette) +
  labs(fill = "Taxonomic Group")
aa.count.plot

#combine plots:
aa.comb.plot <- aa.rel.plot + aa.count.plot + plot_layout(guides = "collect")
aa.comb.plot






###Betaine Data:
bet.relabun <- comp.org.relabun %>% filter(class == "Betaine")
bet.count <- comp.org.count %>% filter(class == "Betaine")
bet.count.text <- comp.org.count.text %>% filter(class == "Betaine")

##Plot Betaine Data:
bet.rel.plot <- ggplot(bet.relabun, aes(x = org_class, y = reorder(compound.name.figure, -order), fill = mean.rel.conc.group)) +
  geom_tile(color = "black") +
  coord_fixed() +
  scale_fill_viridis(discrete = TRUE, option = "G") +
  theme_test()  +
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none", 
        panel.border = element_blank(),
        axis.ticks = element_blank(), 
        plot.margin = unit(c(0,0,0,0), units = "cm")) +
  geom_vline(color = "gray", xintercept = 5.5)
bet.rel.plot

bet.count.plot <- ggplot(bet.count) +
  geom_col(aes(x = count, y = reorder(compound.name.figure, -order), fill = org_class), color = "black", linewidth = 0.1) +
  geom_text(data = bet.count.text, aes(x = count.sum+5, y = reorder(compound.name.figure, -order), label = fig.text)) +
  theme_test() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none", 
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(0,0,0,0), units = "cm")) +
  xlim(0, 50) +
  scale_fill_manual(values = org.palette)
bet.count.plot

#combine plots:
bet.comb.plot <- bet.rel.plot + bet.count.plot + plot_layout(guides = "collect")
bet.comb.plot






###Sugar Data:
sug.relabun <- comp.org.relabun %>% filter(class == "Sugar")
sug.count <- comp.org.count %>% filter(class == "Sugar")
sug.count.text <- comp.org.count.text %>% filter(class == "Sugar")

##Plot Sugar Data:
sug.rel.plot <- ggplot(sug.relabun, aes(x = org_class, y = reorder(compound.name.figure, -order), fill = mean.rel.conc.group)) +
  geom_tile(color = "black") +
  coord_fixed() +
  scale_fill_viridis(discrete = TRUE, option = "G") +
  theme_test()  +
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none", 
        panel.border = element_blank(),
        axis.ticks = element_blank(), 
        plot.margin = unit(c(0,0,0,0), units = "cm")) +
  geom_vline(color = "gray", xintercept = 5.5)
sug.rel.plot

sug.count.plot <- ggplot(sug.count) +
  geom_col(aes(x = count, y = reorder(compound.name.figure, -order), fill = org_class), color = "black", linewidth = 0.1) +
  geom_text(data = sug.count.text, aes(x = count.sum+5, y = reorder(compound.name.figure, -order), label = fig.text)) +
  theme_test() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none", 
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(0,0,0,0), units = "cm")) +
  xlim(0, 50) +
  scale_fill_manual(values = org.palette)
sug.count.plot

#combine plots:
sug.comb.plot <- sug.rel.plot + sug.count.plot + plot_layout(guides = "collect")
sug.comb.plot







###Sulfonium Data:
sulfonium.relabun <- comp.org.relabun %>% filter(class == "Sulfonium")
sulfonium.count <- comp.org.count %>% filter(class == "Sulfonium") 
sulfonium.count.text <- comp.org.count.text %>% filter(class == "Sulfonium")

##Plot Sulfonium Data:
sulfonium.rel.plot <- ggplot(sulfonium.relabun, aes(x = org_class, y = reorder(compound.name.figure, -order), fill = mean.rel.conc.group)) +
  geom_tile(color = "black") +
  coord_fixed() +
  scale_fill_viridis(discrete = TRUE, option = "G") +
  theme_test() +
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none", 
        panel.border = element_blank(),
        axis.ticks = element_blank(), 
        plot.margin = unit(c(0,0,0,0), units = "cm")) +
  geom_vline(color = "gray", xintercept = 5.5)
sulfonium.rel.plot

sulfonium.count.plot <- ggplot(sulfonium.count) +
  geom_col(aes(x = count, y = reorder(compound.name.figure, -order), fill = org_class), color = "black", linewidth = 0.1) +
  geom_text(data = sulfonium.count.text, aes(x = count.sum+5, y = reorder(compound.name.figure, -order), label = fig.text)) +
  theme_test() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none", 
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(0,0,0,0), units = "cm")) +
  xlim(0, 50) +
  scale_fill_manual(values = org.palette)
sulfonium.count.plot

#combine plots:
sulfonium.comb.plot <- sulfonium.rel.plot + sulfonium.count.plot + plot_layout(guides = "collect")
sulfonium.comb.plot





###Sulfonate Data:
sulfonate.relabun <- comp.org.relabun %>% filter(class == "Sulfonate")
sulfonate.count <- comp.org.count %>% filter(class == "Sulfonate")
sulfonate.count.text <- comp.org.count.text %>% filter(class == "Sulfonate")

##Plot Sulfonate Data:
sulfonate.rel.plot <- ggplot(sulfonate.relabun, aes(x = org_class, y = reorder(compound.name.figure, -order), fill = mean.rel.conc.group)) +
  geom_tile(color = "black") +
  coord_fixed() +
  scale_fill_viridis(discrete = TRUE, option = "G") +
  theme_test() +
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none", 
        panel.border = element_blank(),
        axis.ticks = element_blank(), 
        plot.margin = unit(c(0,0,0,0), units = "cm")) +
  geom_vline(color = "gray", xintercept = 5.5)
sulfonate.rel.plot

sulfonate.count.plot <- ggplot(sulfonate.count) +
  geom_col(aes(x = count, y = reorder(compound.name.figure, -order), fill = org_class), color = "black", linewidth = 0.1) +
  geom_text(data = sulfonate.count.text, aes(x = count.sum+5, y = reorder(compound.name.figure, -order), label = fig.text)) +
  theme_test() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none", 
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(0,0,0,0), units = "cm")) +
  xlim(0, 50) +
  scale_fill_manual(values = org.palette)
sulfonate.count.plot

#combine plots:
sulfonate.comb.plot <- sulfonate.rel.plot + sulfonate.count.plot + plot_layout(guides = "collect")
sulfonate.comb.plot






###Other Data:
other.relabun <- comp.org.relabun %>% filter(class == "Other")
other.count <- comp.org.count %>% filter(class == "Other")
other.count.text <- comp.org.count.text %>% filter(class == "Other")

##Plot Betaine Data:
other.rel.plot <- ggplot(other.relabun, aes(x = org_class, y = reorder(compound.name.figure, -order), fill = mean.rel.conc.group)) +
  geom_tile(color = "black") +
  coord_fixed() +
  scale_fill_viridis(discrete = TRUE, option = "G") +
  theme_test() +
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),
        plot.margin = unit(c(0,0,0,0), units = "cm")) +
  geom_vline(color = "gray", xintercept = 5.5)
other.rel.plot

other.count.plot <- ggplot(other.count) +
  geom_col(aes(x = count, y = reorder(compound.name.figure, -order), fill = org_class), color = "black", linewidth = 0.1) +
  geom_text(data = other.count.text, aes(x = count.sum+5, y = reorder(compound.name.figure, -order), label = fig.text)) +
  theme_test() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(), 
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0,0,0,0), units = "cm")) +
  xlim(0, 50) +
  scale_fill_manual(values = org.palette)
other.count.plot

#combine plots:
other.comb.plot <- other.rel.plot + other.count.plot + plot_layout(guides = "collect")
other.comb.plot


##Work to combine all plots:
# full.comb.plot <- aa.comb.plot + bet.comb.plot + sug.comb.plot + 
#   sulfonium.comb.plot + sulfonate.comb.plot + other.comb.plot +
#   plot_layout(guides = "collect", ncol = 1)
# full.comb.plot <- aa.rel.plot + plot_spacer() + aa.count.plot +
#   bet.rel.plot + plot_spacer() + bet.count.plot +
#   sug.rel.plot + plot_spacer() + sug.count.plot +
#   sulfonium.rel.plot + plot_spacer() + sulfonium.count.plot +
#   sulfonate.rel.plot + plot_spacer() + sulfonate.count.plot +
#   other.rel.plot + plot_spacer() + other.count.plot +
#   plot_layout(guides = "collect", ncol = 3, nrow = 6, widths = c(NA, 0, NA))
  

full.comb.plot <- aa.rel.plot + aa.count.plot +
  bet.rel.plot  + bet.count.plot +
  sug.rel.plot  + sug.count.plot +
  sulfonium.rel.plot  + sulfonium.count.plot +
  sulfonate.rel.plot  + sulfonate.count.plot +
  other.rel.plot  + other.count.plot +
  plot_layout(guides = "collect", ncol = 2, nrow = 6, widths = c(NA, NA))



full.comb.plot

ggsave(full.comb.plot, filename = "Figures/Output_Oct25/Culture_Fig.png", 
       height = 9, width = 6, dpi = 1000, scale = 1.3)
