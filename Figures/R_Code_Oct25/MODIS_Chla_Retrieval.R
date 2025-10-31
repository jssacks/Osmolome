





#load packages:
packages.required <- c("rerddap","plotdap","rerddapXtracto", "lubridate", "maps",
                       "mapdata", "mapproj", "ncdf4", "data.table", "tidyverse",
                       "sp", "ggplot2", "gridExtra", "cmocean", "ggnewscale")

lapply(packages.required, require, character.only = TRUE)




####Perform G4 (TN397) Retrieval: 


#Define time, lat, and lon range:

#Time:
tcoord.g4 <- c("2021-11-19", "2021-12-15")

#Lat and Long
xcoord.g4 <- c(-160, -110)
ycoord.g4 <- c(-5, 35)
ttext.g4 <-paste(paste(abs(xcoord), collapse="-"),"W, ", paste(ycoord, collapse="-"),"N")



###Get MODIS data:
dataInfo.g4 <- rerddap::info('erdMH1chlamday')

# Extract the parameter name from the metadata in dataInfo
parameter.g4 <- dataInfo.g4$variable$variable_name

#Extract the start and end times of the dataset from the metadata in dataInfo
global.g4 <- dataInfo.g4$alldata$NC_GLOBAL

# Run rxtracto_3D
chlMODIS.g4 <- rxtracto_3D(dataInfo.g4, parameter=parameter,
                      tcoord=tcoord.g4,
                      xcoord=xcoord.g4, ycoord=ycoord.g4)

chlMODIS.g4$avgmap <- apply(chlMODIS.g4$chlorophyll,c(1,2),function(x) mean(x,na.rm=TRUE))

#extract chlorophyll data and pair with lat and long

#chlorophyll 
chl.wide.g4 <- as.data.table(chlMODIS.g4$avgmap) 

chl.long.g4 <- tibble(melt(chl.wide.g4)) %>%
  select(value) %>%
  rename("Chl" = value)

#lat + lon
lat.g4 <- as.data.table(tibble(lat = chlMODIS.g4$latitude)) 
lon.g4 <- as.data.table(tibble(lon = chlMODIS.g4$longitude))

chlmap.g4 <- cross_join(lat.g4, lon.g4) %>%
  cbind(., chl.long.g4) %>%
  mutate(lat = as.numeric(lat)) %>%
  mutate(lon = as.numeric(lon)) 



print(chlmap.g4)

###Export G4 Chla data:
write_csv(chlmap.g4, file = "Intermediates/MODIS_chla_data_g4.tsv")






####__________Perform G3 (KM1906) Retrieval: 


#Define time, lat, and lon range:

#Time:
tcoord.g3 <- c("2019-04-10", "2019-04-29")

#Lat and Long
xcoord.g3 <- c(-167, -148)
ycoord.g3 <- c(15, 50)
ttext.g3 <-paste(paste(abs(xcoord), collapse="-"),"W, ", paste(ycoord, collapse="-"),"N")




###Get MODIS data:
dataInfo.g3 <- rerddap::info('erdMH1chlamday')

# Extract the parameter name from the metadata in dataInfo
parameter.g3 <- dataInfo.g3$variable$variable_name

#Extract the start and end times of the dataset from the metadata in dataInfo
global.g3 <- dataInfo.g3$alldata$NC_GLOBAL

# Run rxtracto_3D
chlMODIS.g3 <- rxtracto_3D(dataInfo.g3, parameter=parameter,
                           tcoord=tcoord.g3,
                           xcoord=xcoord.g3, ycoord=ycoord.g3)

chlMODIS.g3$avgmap <- apply(chlMODIS.g3$chlorophyll,c(1,2),function(x) mean(x,na.rm=TRUE))

#extract chlorophyll data and pair with lat and long

#chlorophyll 
chl.wide.g3 <- as.data.table(chlMODIS.g3$avgmap) 

chl.long.g3 <- tibble(melt(chl.wide.g3)) %>%
  select(value) %>%
  rename("Chl" = value)

#lat + lon
lat.g3 <- as.data.table(tibble(lat = chlMODIS.g3$latitude)) 
lon.g3 <- as.data.table(tibble(lon = chlMODIS.g3$longitude))

chlmap.g3 <- cross_join(lat.g3, lon.g3) %>%
  cbind(., chl.long.g3) %>%
  mutate(lat = as.numeric(lat)) %>%
  mutate(lon = as.numeric(lon)) 



print(chlmap.g3)

###Export G4 Chla data:
write_csv(chlmap.g3, file = "Intermediates/MODIS_chla_data_g3.tsv")






































