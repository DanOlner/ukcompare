#Misc geography checks
library(tidyverse)
library(sf)

#Check NUTS2 vs ITL2 are actually the same
itl2.geo <- st_read('data/geographies/International_Territorial_Level_2_January_2021_UK_BFE_V2_2022_-4735199360818908762/ITL2_JAN_2021_UK_BFE_V2.shp')

nuts2.geo <- st_read('../../../MapPolygons/England/2001/England_nuts2_2001/england_nuts2_2001.shp')

#Nearly, not quite
plot(st_geometry(itl2.geo), lty = 3)
plot(st_geometry(nuts2.geo), add = T, border = 'green')

#Looking in QIGS: local/qgis/NUTS_ITL_comparison:
#ITL and NUTS 2001: 2001 has only inner/outer london. ITL London zones match whole of London but not within
#Plus small chunk of border of Cheshire / Merseyside changed.

#Next questions:
#1. What NUTS year does BRES 2015-2021 data actually use? What options does it have?
#At the moment, for BRES open 2015-21 I've downloaded NUTS2 2016, the most recent available there
#And of course that's the last one the UK will be using, now ITL

#See here?
#https://ec.europa.eu/eurostat/web/nuts/history
#NUTS 2016 was released in 2018. So almost certainly when geoportal says "NUTS2018" they mean NUTS 2016
#All perfectly logical
#https://geoportal.statistics.gov.uk/datasets/nuts-level-2-january-2018-full-extent-boundaries-in-the-united-kingdom-1/explore

#On the website, it's looking like it matches ITL2. Checking in QGIS... tick, matches current 2023 ITL2

#Ah, this looking better than geoportal
#https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/nuts

#No data in NUTS2 2010 geographies for earlier BRES either.
#It does have data for CAS wards 2003. Do they tesselate by any chance?
#These are (I think) English census area stats wards for 2001.
engcas2001 <- st_read('../../../MapPolygons/England/2001/England_caswa_2001_clipped/england_caswa_2001_clipped.shp')


#Actually, checked in QGIS and yes they do tesselate (or appear to, would need to do full area match check here)