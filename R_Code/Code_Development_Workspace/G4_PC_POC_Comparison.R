







###G4 POC data from Angel 
g4.poc.dat <- read_csv("Intermediates/G4_metadata_with_interpolations.csv")


##G4 POC data from Karl Lab
karl.lab.dat <- read_csv("Collaborator_Data/ATP_all/ATP_G3_G4.csv") %>%
  filter(as.numeric(Depth) < 10) %>%
  select(Lat, Long, PC) 


comparison.dat <- g

ggplot(g4.poc.dat, aes(x = lat, y = pc)) +
  geom_point(size = 2) +
  geom_point(data = karl.lab.dat, aes(x = Lat, y = PC), color = "red", size = 2) +
  xlab("Latitude") +
  ylab("Particulate Organic Carbon (umol/L)")



#latitude 







































