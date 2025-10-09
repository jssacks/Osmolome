


#load packages:
packages.required <- c("rerddap","plotdap","rerddapXtracto", "lubridate", "maps",
                       "mapdata", "mapproj", "ncdf4", "data.table", "tidyverse",
                       "sp", "ggplot2", "gridExtra", "cmocean", "ggnewscale")

lapply(packages.required, require, character.only = TRUE)



##Define inputs:
#sample.loc.file <- 
#exp.loc.file <- 



#Define time, lat, and lon range:

#Time:
tcoord <- c("2019-04-10", "2019-04-29")

#Lat and Long
xcoord <- c(-165, -150)
ycoord <- c(15, 50)
ttext<-paste(paste(abs(xcoord), collapse="-"),"W, ", paste(ycoord, collapse="-"),"N")



###Get MODIS data:
dataInfo <- rerddap::info('erdMH1chlamday')
dataInfo

# Extract the parameter name from the metadata in dataInfo
parameter <- dataInfo$variable$variable_name

#Extract the start and end times of the dataset from the metadata in dataInfo
global <- dataInfo$alldata$NC_GLOBAL

# Run rxtracto_3D
chlMODIS<-rxtracto_3D(dataInfo,parameter=parameter,
                      tcoord=tcoord,
                      xcoord=xcoord,ycoord=ycoord)

chlMODIS$avgmap <- apply(chlMODIS$chlorophyll,c(1,2),function(x) mean(x,na.rm=TRUE))

#extract chlorophyll data and pair with lat and long

#chlorophyll 
chl.wide <- as.data.table(chlMODIS$avgmap) 

chl.long <- tibble(melt(chl.wide)) %>%
  select(value) %>%
  rename("Chl" = value)

#lat + lon
lat <- as.data.table(tibble(lat = chlMODIS$latitude)) 
lon <- as.data.table(tibble(lon = chlMODIS$longitude))

chlmap <- cross_join(lat, lon) %>%
  cbind(., chl.long) %>%
  mutate()

library(viridis)
library(cmocean)
###########XXXXXXX
ggplot(chlmap, aes(x=lon, y=lat, fill = log(Chl))) +
  geom_raster() +
  scale_fill_cmocean(name = "delta", alpha = 0.75, limits = c(-3.5,1)) +
  coord_fixed() +
  theme_test() +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0))
  

