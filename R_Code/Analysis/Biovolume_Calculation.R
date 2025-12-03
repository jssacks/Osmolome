




library(tidyverse)



###define inputs:

#seaflow
seaflow.file <- "Collaborator_Data/Seaflow_all/tblSeaFlow_v1_5_data.csv"


#ifcb
g3.ifcb.file <- "Collaborator_Data/IFCB_all/G3_IFCB_Biovolume.csv"
g4.ifcb.file <- "Collaborator_Data/IFCB_all/G4_IFCB_Biovolume.csv"




###load data and calculate biovolume from seaflow
sf.dat <- read_csv(seaflow.file) %>%
  filter(cruise %in% c("TN397", "KM1906")) %>%
  mutate(pro.biovol.ul.l = abundance_prochloro*(pi*diam_prochloro^3)/6*1e-9*1e6,
         syn.biovol.ul.l = abundance_synecho*(pi*diam_synecho^3)/6*1e-9*1e6,
         picoeuk.biovol.ul.l = abundance_picoeuk*(pi*diam_picoeuk^3)/6*1e-9*1e6,
         total.sf.biovol.ul.l = pro.biovol.ul.l+syn.biovol.ul.l+picoeuk.biovol.ul.l) %>%
  mutate(time.round = round_date(time, unit = "hour")) 





###load and calculate total biovolume from IFCB:
g3.ifcb.dat <- read_csv(g3.ifcb.file) %>%
  pivot_longer(cols = Arthropoda:Unknown, names_to = "category", values_to = "biovol.ul.l") %>%
  group_by(GMT) %>%
  mutate(total.ifcb.biovol.ul.l = sum(biovol.ul.l)) %>%
  ungroup() %>%
  mutate(time = mdy_hm(GMT)) %>%
  mutate(time.round = round_date(time, unit = "hour"))

g4.ifcb.dat <- read_csv(g4.ifcb.file) %>%
  pivot_longer(cols = Arthropoda:Unknown, names_to = "category", values_to = "biovol.ul.l")  %>%
  group_by(GMT) %>%
  mutate(total.ifcb.biovol.ul.l = sum(biovol.ul.l)) %>%
  mutate(time = mdy_hm(GMT)) %>%
  mutate(time.round = round_date(time, unit = "hour"))




####Get hourly median values for seaflow and ifcb datasets
sf.median.dat <- sf.dat %>%
  group_by(time.round, cruise) %>%
  reframe(median.totsf.biovol.ul.l = median(total.sf.biovol.ul.l),
          median.pro.biovol.ul.l = median(pro.biovol.ul.l),
          median.syn.biovol.ul.l = median(syn.biovol.ul.l),
          median.picoeuk.biovol.ul.l = median(picoeuk.biovol.ul.l))
  
   
g3.ifcb.median.dat <- g3.ifcb.dat %>%
  mutate(cruise = "KM1906") %>%
  group_by(time.round, cruise) %>%
  reframe(median.totifcb.biovol.ul.l = median(total.ifcb.biovol.ul.l))


g4.ifcb.median.dat <- g4.ifcb.dat %>%
  mutate(cruise = "TN397") %>%
  group_by(time.round, cruise) %>%
  reframe(median.totifcb.biovol.ul.l = median(total.ifcb.biovol.ul.l))



####Combine seaflow and ifcb data to calculate a summed median biovolume value

#combine ifcb datasets:
all.ifcb.median.dat <- rbind(g3.ifcb.median.dat, g4.ifcb.median.dat)


#### combine seaflow and ifcb data
all.biovol.dat <- left_join(sf.median.dat, all.ifcb.median.dat) %>%
  mutate(sum.median.biovol.ul.l = median.totsf.biovol.ul.l + median.totifcb.biovol.ul.l,
         fraction.biovol.seaflow = median.totsf.biovol.ul.l/sum.median.biovol.ul.l)



#export biovolume data:
write_csv(all.biovol.dat, file = "Intermediates/G3_G4_IFCB_Seaflow_Biovolume_Data.csv")






#visualize data:
ggplot(all.biovol.dat, aes(x = time.round, y = sum.median.biovol.ul.l)) +
  geom_line() +
  facet_wrap(.~cruise, scales = "free")

ggplot(all.biovol.dat, aes(x = time.round, y = fraction.biovol.seaflow)) +
  geom_line() +
  facet_wrap(.~cruise, scales = "free")





















