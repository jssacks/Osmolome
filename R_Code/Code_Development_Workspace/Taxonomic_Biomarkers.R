






#load packages and source code:
library(tidyverse)
library(viridis)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")





#load and organize data:
dat.cult <- read_csv("Intermediates/Culture_Final_Quant_QCed.csv") %>%
  left_join(., compound.order) %>%
  filter(!str_detect(SampID, "Blk")) %>%
  mutate(Part.Conc.Vial.uM = case_when(Detected == "No" ~ 0,
                                  TRUE ~ Part.Conc.Vial.uM)) %>%
  filter(!is.na(compound.name.figure)) %>%
  filter(!is.na(Type))


#Summarize data by different groups: 
dat.cult.organism <- dat.cult %>%
  group_by(Organism, Type, class, compound.name.figure, Detected) %>%
  reframe(mean.conc.uM = mean(Part.Conc.Vial.uM)) %>%
  group_by(Type, Organism) %>%
  mutate(Rel.Conc = mean.conc.uM/sum(mean.conc.uM)*100) 
  
  
  
#Redefine organism classes
dat.org.class.new <- dat.cult.organism %>%
    filter(!Type == "Bacteria") %>%
    mutate(org_type = case_when(Type == "Dino" ~ "Dinoflagellate",
                                 Type == "Diatom" ~ "Diatom",
                                 Type == "Haptophyte" ~ "Haptophyte",
                                 Type == "Prasinophyte" ~ "Prasinohpyte",
                                 Organism %in% c("WH8501") ~ "Croco",
                                 Organism %in% c("1314P", "As9601", "MED4", "NATL2A") ~ "Pro",
                                 Organism %in% c("8102", "7803") ~ "Syn"))



# Identify compounds unique to a small number of organisms/organism groups:


#sumarize organism compounds:
org.group.comps <- dat.org.class.new %>%
  filter(Detected == "Yes") %>%
  filter(Rel.Conc > 0.1)


#compound count by number of organism groups that produce it:
org.group.sum <- dat.org.class.new %>%
  filter(Detected == "Yes") %>%
  filter(Rel.Conc > 0.1) %>%
  select(compound.name.figure, org_type) %>%
  unique() %>%
  group_by(compound.name.figure) %>%
  mutate(count = n())
  

#Settle on final "Taxonomic indicator" compounds:
tax.comps <- org.group.sum %>%
  filter(count < 4) %>%
  filter(!compound.name.figure %in% c("beta-Glutamic acid"))





## Pull out data for figure:
tax.comp.dat <- dat.org.class.new %>%
  filter(compound.name.figure %in% tax.comps$compound.name.figure) %>%
  filter(Detected == "Yes") %>%
  filter(Rel.Conc > 1) %>%
  group_by(compound.name.figure, org_type) %>%
  reframe(count = n(),
         mean.rel.conc = mean(Rel.Conc),
         max.rel.conc = max(Rel.Conc),
         median.rel.conc = median(Rel.Conc)) 


ggplot(tax.comp.dat, aes(y = compound.name.figure, x = org_type, fill = mean.rel.conc)) +
  geom_tile(color = "black") +
  coord_fixed() +
  scale_fill_viridis(option = "H") +
  theme_test() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(tax.comp.dat, aes(y = compound.name.figure, x = org_type, fill = max.rel.conc)) +
  geom_tile(color = "black") +
  coord_fixed() +
  scale_fill_viridis(option = "H") +
  theme_test() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(tax.comp.dat, aes(y = compound.name.figure, x = org_type, fill = median.rel.conc)) +
  geom_tile(color = "black") +
  coord_fixed() +
  scale_fill_viridis(option = "H") +
  theme_test() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))







dat.org.review <- dat.cult.organism %>%
  filter(!Type == "Bacteria") %>%
  filter(Detected == "Yes") %>%
  filter(Rel.Conc > 1) %>%
  group_by(compound.name.figure) %>%
  mutate(count = n())

dat.org.review.classcount <- dat.org.review %>%
  select(compound.name.figure, Type) %>%
  unique() %>%
  group_by(compound.name.figure) %>%
  mutate(count = n())




ggplot(dat.cult.organism %>%
         filter(Rel.Conc >= 5), aes(x = reorder(Organism, Type), y = compound.name.figure, fill = Rel.Conc)) +
  geom_tile() +
  coord_fixed()
  

dat.cult.organism.sum <- 



dat.cult.group <- 

























