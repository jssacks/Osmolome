







library(tidyverse)
library(patchwork)
source("R_Code/Code_Development_Workspace/Figure_Palettes.R")


###define inputs
all.dat.file <- "Intermediates/Final_Osmo_Meta_Env_Dataframe.csv"



#Read in data
dat <- read_csv(all.dat.file)



#sample locations/identities:
samp.info <- dat %>%
  filter(Cruise %in% c("KM1906", "TN397", "RC078", "PERIFIX", "K")) %>%
  filter(depth_m < 10) %>%
  select(Part.SampID, Diss.SampID, Cruise, Region, Lat, Long, depth_m, station, Treatment, sst, sss, poc, pn) %>%
  unique()

write_csv(samp.info, file = "Tables/Outputs/Environmental_Sample_Details_Supplemental_Table.csv")





#metabolite data for Supplemental Table:
metab.dat <- dat %>%
  filter(depth_m < 10) %>%
  select(Part.SampID, Diss.SampID, Cruise, Region, Compound, compound.name.figure, Part.Conc.nM, Diss.Conc.nM.adj, Total.Conc.nM,
         Part.Flag, Diss.Flag, Tot.Flag) %>%
  unique() %>%
  filter(Cruise %in% c("KM1906", "TN397", "RC078", "PERIFIX"))

write_csv(metab.dat, file = "Tables/Outputs/Environmental_Osmolyte_Concentrations_Supplemental_Table.csv")








































