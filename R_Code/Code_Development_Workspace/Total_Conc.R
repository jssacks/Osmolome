



library(tidyverse)
library(lubridate)
library(patchwork)



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


tot.dat <- surface.dat %>%
  group_by(SampID) %>%
  mutate(tot.osmo.nM = sum(nM.in.smp))

#Gradients tot dat:
g.dat <- tot.dat %>%
  filter(Cruise %in% c("TN397", "KM1906")) %>% 
  select(SampID, Cruise, Lat, Long, Local_Date, Local_Time, depth_m, station,
         tot.osmo.nM, tod) %>%
  unique() %>%
  filter(Long < -125)



#D1 tot dat:
###RC078 Stoich plots:
d1.data <- tot.dat %>%
  filter(Cruise == "RC078") %>% 
  select(SampID, Cruise, Lat, Long, Local_Date, Local_Time, depth_m, station,
         tot.osmo.nM, tod) %>%
  unique() %>%
  filter(depth_m < 10) %>%
  filter(station %in% c(2, 3, 5, 6, 7)) %>%
  full_join(., tibble(Cruise = "RC078", tod = as.factor("night"), station = 2))


#PERIFIX tot dat:
peri.dat <- tot.dat %>%
  filter(Cruise %in% c("PERIFIX")) %>%
  select(SampID, Cruise, Lat, Long, Local_Date, Local_Time, depth_m, station,
         tot.osmo.nM, tod, Treatment) %>%
  unique() %>%
  mutate(Treatment = as.factor(Treatment),
         Treatment = fct_relevel(Treatment, c("Tote", "C", "P", "F", "PF", "NPF", "NP", "NF", "N")),
         N_status = case_when(Treatment %in% c("N", "NP", "NF", "NPF") ~ "+N",
                              TRUE ~ "-N")) %>%
  filter(!Treatment == "Tote")




###Make Plots:
t.g.fig <- ggplot(g.dat, aes(x = Lat, y = tot.osmo.nM)) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
  geom_vline(xintercept = 32.34, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 35, linetype = "dashed", color = "blue") +
  geom_smooth(color = "black") +
  geom_point(shape = 21, aes(fill = tod), size = 3, stroke = 0.2) +
#  ylim(0,16) +
  # scale_fill_viridis(discrete = TRUE, direction = -1, option = "G", begin = 0.2) +
  scale_fill_manual(values = tod.palette) +
  theme_test() +
  ylab("Total Osmolyte Conc (nM)")
t.g.fig


t.d.fig <- ggplot(d1.data, aes(x = as.factor(station), y = tot.osmo.nM)) +
  geom_boxplot(width = 0.5) +
  geom_jitter(width = .1, height = 0, shape = 21, aes(fill = tod), size = 3, stroke = 0.2) +
  #  scale_fill_viridis(discrete = TRUE, direction = -1, option = "G", begin = 0.2) +
  scale_fill_manual(values = tod.palette) +
  theme_test()+
 # ylim(0,16) +
  theme(axis.title.y = element_blank()) +
  xlab("Station")
t.d.fig

t.p.fig <- ggplot(peri.dat, aes(x = Treatment, y = tot.osmo.nM)) +
  geom_boxplot(aes(color = N_status)) +
  scale_color_manual(values = c("gray60", "gray20")) +
  geom_jitter(width = .1, height = 0, shape = 21, aes(fill = tod), size = 3, stroke = 0.2) +
  #  scale_fill_viridis(discrete = TRUE, direction = -1, option = "G", begin = 0.2) +
  scale_fill_manual(values = tod.palette) +
  theme_test() +
  ylim(0,NA) +
  theme(axis.title.y = element_blank()) 
t.p.fig


####Put plots together:
big.plot <- ((t.g.fig | (t.d.fig + t.p.fig))) +
  plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = "A")
big.plot


