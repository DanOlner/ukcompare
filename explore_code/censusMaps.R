#Census maps
#Regional GVA per sector explore
library(tidyverse)
library(sf)
library(tmap)
source('functions/misc_functions.R')
options(scipen = 99)


#2011 MSOA workplace population
c2011w <- read_csv('data/workplace_popCensus2011_MSOA_bySICcode_20sections.csv') %>% 
  mutate(code = substr(`2011 super output area - middle layer`, start = 1, stop = 9))

#2011 MSOAs
msoa11 <- st_read('../../../MapPolygons/England/2011/England_msoa_2011_gen_clipped/england_msoa_2011_gen_clipped.shp')

#Data is for England... and wales? Doesn't really matter, we want SY
table(msoa11$code %in% c2011w$code)

msoa11 <- msoa11 %>% left_join(c2011w, by = 'code')

msoa11 <- msoa11 %>% 
  # mutate_at(vars(`A Agriculture, forestry and fishing`:`U Activities of extraterritorial organisations and bodies`), funs( (. / 'All categories: Industry')*100 ))
  mutate(across(c("A Agriculture, forestry and fishing":"U Activities of extraterritorial organisations and bodies"), .names = '{.col}_percent',
                function(x) ( (x/`All categories: Industry`)*100 )))


#intersection for SY geography
# itl2.geo <- st_read('data/geographies/International_Territorial_Level_2_January_2021_UK_BFE_V2_2022_-4735199360818908762/ITL2_JAN_2021_UK_BFE_V2.shp')
# intersects = st_intersects(msoa11, itl2.geo %>% filter(ITL221NM == 'South Yorkshire'), sparse = FALSE)

# Filter small_zones based on the intersection
# msoa.sy = msoa11[rowSums(intersects) > 0, ]

#Ah ha, we can just filter on name!
msoa.sy <- msoa11 %>% filter(grepl(x = name, pattern = 'Sheffield|Doncaster|Rotherham|Barnsley', ignore.case = T))

plot(st_geometry(msoa.sy))

#Too big but will do for now... lookup would be better
plot(itl2.geo %>% filter(ITL221NM == 'South Yorkshire'), border = 'blue', add=T)

itl3.geo <- st_read('data/geographies/International_Territorial_Level_3_January_2021_UK_BUC_V3_2022_6920195468392554877/ITL3_JAN_2021_UK_BUC_V3.shp')

#Map plz
tmap_mode('view')

names(msoa.sy)
sector = names(msoa.sy)[grepl(x  = names(msoa.sy), pattern = 'health', ignore.case=T) & grepl(x  = names(msoa.sy), pattern = 'percent')]
sector = names(msoa.sy)[grepl(x  = names(msoa.sy), pattern = 'education', ignore.case=T) & grepl(x  = names(msoa.sy), pattern = 'percent')]
sector = names(msoa.sy)[grepl(x  = names(msoa.sy), pattern = 'construction', ignore.case=T) & grepl(x  = names(msoa.sy), pattern = 'percent')]
sector = names(msoa.sy)[grepl(x  = names(msoa.sy), pattern = 'wholesale', ignore.case=T) & grepl(x  = names(msoa.sy), pattern = 'percent')]
sector = names(msoa.sy)[grepl(x  = names(msoa.sy), pattern = '', ignore.case=T) & grepl(x  = names(msoa.sy), pattern = 'percent')]

tm_shape(msoa.sy) +
  tm_polygons(col = sector, style = 'fisher', alpha = 0.3, n = 10, palette = 'RdYlGn') +
  tm_shape(itl3.geo %>% filter(grepl(x = ITL321NM, pattern = 'sheffield|rotherham', ignore.case=T))) +
  tm_borders(lwd = 3, col = 'black') +
  # tm_polygons(col = c("A Agriculture, forestry and fishing_percent","U Activities of extraterritorial organisations and bodies_percent" )) +
  # tm_facets(as.layers = T) +
  tmap_options(check.and.fix = TRUE)



#Just gimme crude correlation matrix of %s please
#plot(msoa.sy %>% st_set_geometry(NULL) %>% select(contains('percent')))
cormatrix <- cor(msoa.sy %>% st_set_geometry(NULL) %>% select(contains('percent')), method = 'pearson')

cormatrix.long <- 
  cormatrix %>% as_tibble() %>% 
  pivot_longer(cols = contains('percent'),
                        names_to = 'sector', values_to = 'percent'
                        )


ggplot(cormatrix, aes())
