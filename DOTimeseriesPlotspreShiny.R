setwd('C:/Users/ml1451/OneDrive - USNH/PREP/SOEE 2022/SOOE2022Scripts/PREP_SOOE_2022_Scripts/Shiny')
# print(getwd())
source('./helpers.R')
library(leaflet)

# GRBUPR = 647
# GRBGB=279
# GRBLR=274
# GRBSQ = 281
# GRBSF = 697
# GRBOR = 280 
# HHHR = 958
# GRBCML = 285
# locs <- c('Great Bay', 'Lamprey River', 'Upper Piscataqua River', 
#           'Squamscott River', 'Salmon Falls', 'Oyster River', 
#           'Hampton River', 'Portsmouth Harbor')
sfids = '647,279,274,281,697,280,958,285'
sfs <- prepsfdbdata(sfids)

#access coordinates
sfs$featuregeometry.coordinates[[1]][1] # long
sfs$featuregeometry.coordinates[[8]][2] #lat
sfs$long <- NA
sfs$lat <- NA

for (i in 1:nrow(sfs)){
  sfs$long[i] <- sfs$featuregeometry.coordinates[[i]][1]
  sfs$lat[i] <- sfs$featuregeometry.coordinates[[i]][2]
}

leaflet(data=sfs) %>% addTiles() %>% 
  addMarkers(~long, ~lat, popup=~as.character(samplingfeaturecode))