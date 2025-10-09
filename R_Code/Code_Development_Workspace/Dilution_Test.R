







##load packages
library(tidyverse)

##Functions:
source("R_Code/Functions.R")


#define inputs
pos.stdcurve.file <- "Raw_Data/Standard_Curves/QE_HILIC_Pos_standardcurves_Jan25.csv"
neg.stdcurve.file <- "Raw_Data/Standard_Curves/QE_HILIC_Neg_standardcurves_Jan25.csv"
stds.file <- "Meta_Data/Ingalls_Lab_Standards_09222025.csv"



#Load in data:
pos.dat <- sky_read(pos.stdcurve.file) %>%
  select(Rep, Compound, Area)

neg.dat <- sky_read(neg.stdcurve.file) %>%
  select(Rep, Compound, Area)




























