#Business counts via NOMIS
#BRES explore
library(tidyverse)
library(zoo)
library(sf)
library(tmap)
library(plotly)
library(magick)
library(nomisr)
library(pryr)
options(scipen = 99)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Tracking down in the API----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~

a <- nomis_search(name = '*business*')

atts <- a %>% tidyr::unnest(components.attribute) %>% glimpse()
unique(atts$name.value)

#Business counts: enterprises will probably have larger businesses in SY, so counts will be higher. Need to compare both.
atts %>% filter(name.value == "UK Business Counts - local units by industry and employment size band")
atts %>% filter(name.value == "UK Business Counts - enterprises by industry and employment size band")

#local units
#This is saying it was last updated 2019, don't think that's accurate. Website has more recent data.
q <- nomis_overview("NM_141_1")
q %>% tidyr::unnest(name) %>% View

y <- nomis_data_info("NM_141_1")
glimpse(y)
y %>% tidyr::unnest(components.dimension) %>% View
y %>% tidyr::unnest(components.dimension) %>% select(conceptref) %>% pull()

print(nomis_get_metadata(id = "NM_141_1", concept = "GEOGRAPHY", type = 'type'), n = 50)
nomis_get_metadata(id = "NM_141_1", concept = "INDUSTRY")#As for BRES, does not work!
nomis_get_metadata(id = "NM_141_1", concept = "EMPLOYMENT_SIZEBAND")
nomis_get_metadata(id = "NM_141_1", concept = "LEGAL_STATUS")
nomis_get_metadata(id = "NM_141_1", concept = "MEASURES")
nomis_get_metadata(id = "NM_141_1", concept = "FREQ")
nomis_get_metadata(id = "NM_141_1", concept = "TIME")#2010-2022

#Compare number/name of zones in NUTS2010 vs 2013
#2013: for "2016 onwards" data, which should mean it matches ITL2
#2010 probably won't.
countries <- nomis_get_metadata(id = "NM_141_1", concept = "geography", type = "TYPE499")

#NUTS2 2013: think there’s a Scotland difference with the ITL2 regions. This might not matter if linking e.g. just with England ITL2 in the GVA data, but note.
#May need to aggregate Scotland, if want to keep comparison
nuts2010 <- nomis_get_metadata(id = "NM_141_1", concept = "geography", type = "TYPE455")
nuts2013 <- nomis_get_metadata(id = "NM_141_1", concept = "geography", type = "TYPE450")

nuts2013$description.en[!nuts2013$description.en %in% nuts2010$description.en]
nuts2010$description.en[!nuts2010$description.en %in% nuts2013$description.en]
#Yes, difference is London - those extra 3 London zones. So that's summable if we want a larger time series 
#Though suspect the actual geography will have that little alteration to Cheshire, that might be bearable if noted - not the most important comparison to SY

#This is just a text difference: "Bristol/Bath" vs "Bath/Bristol"
nuts2010$description.en[grepl('Gloucester',nuts2010$description.en)]

#Confirm 2013 match to ITL2 again
itl2.geo <- st_read('data/geographies/International_Territorial_Level_2_January_2021_UK_BFE_V2_2022_-4735199360818908762/ITL2_JAN_2021_UK_BFE_V2.shp')

table(nuts2013$description.en %in% itl2$GEOGRAPHY_NAME)
#itl2 geodata there is England only
nuts2013$description.en[!nuts2013$description.en %in% itl2$GEOGRAPHY_NAME]
unique(itl2$GEOGRAPHY_NAME)[!unique(itl2$GEOGRAPHY_NAME) %in% nuts2013$description.en]


#Note the following (from text included in CSV export) on 2015 change:
#•	“In 2015, ONS extended the coverage of businesses to include a population of solely PAYE based businesses that were previously excluded because of the risk of duplication. In total, in 2015, 105,000 businesses have been added.Improvements in matching of administrative data and research into those units excluded has indicated that the risk of duplication is very small. The addition of these businesses brings the publication in line with Business Demography and the BIS Business Population Estimates, both of which include these businesses. For more information, see http://www.nomisweb.co.uk/articles/news/files/UKBusinessCoverage.pdf”

#enterprises: structure will be (should be!) the same as above
y <- nomis_data_info("NM_142_1")
glimpse(y)

print(nomis_get_metadata(id = "NM_142_1", concept = "GEOGRAPHY", type = 'type'), n = 50)

#So, summary:
#2010-2015 data is available at NUTS2 2010 geography. It nearly matches but I can't confirm the geography, the shapefile is highly elusive. It does have data though.
#2015-2022 data - NUTS2 2013 geography. Scots mismatch with ITL2 possibly, but should match BRES data


#Test reducing the query to get a year at a time rather than doing one geography at a time

#Check variable names with random file
chk <- readRDS('local/data/BusinessCountsByNUTS2/BUSINESSCOUNT_ENTERPRISE_NUTS2_Inner London - East_2016.rds')

#First, dropping half the size because don't need % - just measures = value
x <- proc.time()

#If select is done after download, this isn't going to make any difference... no, it's faster
z <- nomis_get_data(id = "NM_142_1", time = 2022, geography = "TYPE450", measures = 20100, 
                    select = c(
                      'DATE',
                      'GEOGRAPHY_NAME',
                      'INDUSTRY_NAME',
                      'INDUSTRY_CODE',
                      'INDUSTRY_TYPE',
                      'GEOGRAPHY_CODE',
                      'EMPLOYMENT_SIZEBAND_NAME',
                      'LEGAL_STATUS_NAME',
                      'OBS_VALUE'
                      )
                    )
proc.time()-x

#8 minutes for a year
#Check that worked... tick
unique(z$GEOGRAPHY_NAME)
unique(z$INDUSTRY_TYPE)

#688mb
object_size(z)

#Save that one before moving on to rest
saveRDS(z,'local/data/BusinessCountsByNUTS2/BUSINESSCOUNT_ENTERPRISE_NUTS2_2022.rds')


#Getting business count data 2015-2022 then...

#Test removing forward slashes to get filenames to work below
#https://stackoverflow.com/a/33312609
#Run both
gsub('(?<![0-9])/(?![0-9])', '_', "Bath/Bristol", perl=TRUE)
#gsub('(?<![0-9])/|/(?![0-9])','_', "Bath/Bristol", perl=TRUE)

#Looks like bath/bristol is the only one with a problem





#~~~~~~~~~~~~~~~~~~~~~~~~~~~
#BUSINESS COUNTS: ITL2 / NUTS2 BULK DOWNLOAD----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Break down by NUTS2 geography to enable bulk download. Combine/reduce after.
# nuts2013 <- nomis_get_metadata(id = "NM_142_1", concept = "geography", type = "TYPE450")
# 
# geographies_id <- nuts2013 %>% select(id) %>% pull
# geographies_name <- nuts2013 %>% select(label.en) %>% pull
# 
# years = c(2016:2022)
# 
# download_all_BUSINESSCOUNT_ENTERPRISE <- function(year){
#   
#     for(i in 1:nrow(nuts2013)){
#   # for(i in 1:1){
#     
#       cat('Getting ',geographies_name[i],", ",year,'\n')
#         
#       #For any failed downloads
#       #Check if file already exists in the folder. If so, skip.
#       currentfiles <- list.files(path = "local/data/BusinessCountsByNUTS2/", pattern = "BUSINESSCOUNT_ENTERPRISE", full.names = T)
#       
#       #Get rid of any forward slashes (just bath/bristol?)
#       geographyname_filtered <- gsub('(?<![0-9])/(?![0-9])', '_', geographies_name[i], perl=TRUE)
#       
#       #File to attempt to download
#       filetodownload <- paste0('local/data/BusinessCountsByNUTS2/BUSINESSCOUNT_ENTERPRISE_NUTS2_',geographyname_filtered,'_',year,'.rds')
#       
#       
#       #Test that works... tick
#       # filetodownload %in% currentfiles
#       
#       if(filetodownload %in% currentfiles){
#         
#       cat('Already got that one. Skipping on... \n')
#         
#       } else {
#       
#         z <- nomis_get_data(id = "NM_142_1", time = as.character(year), geography = geographies_id[i])
#         saveRDS(z,filetodownload)
#         
#         Sys.sleep(10)
#         
#       }
#     
#   }
#   
# }
#   
# #Test... tick
# # download_all_BUSINESSCOUNT_ENTERPRISE(2022)
# # x <- readRDS('local/data/BusinessCountsByNUTS2/BUSINESSCOUNT_ENTERPRISE_NUTS2_Tees Valley and Durham_2022.rds')
# lapply(years, function(x) download_all_BUSINESSCOUNT_ENTERPRISE(x))


#REPEAT GETTING YEARLY IN ONE GO
years = c(2016:2022)
years = c(2016:2021)

#ENTERPRISE LEVEL
download_all_BUSINESSCOUNT_ENTERPRISE <- function(year){
  
  cat('Downloading year ',year,'\n')
  
  x <- proc.time()
  
  #If select is done after download, this isn't going to make any difference... no, it's faster
  z <- nomis_get_data(id = "NM_142_1", time = year, geography = "TYPE450", measures = 20100, 
                      select = c(
                        'DATE',
                        'GEOGRAPHY_NAME',
                        'INDUSTRY_NAME',
                        'INDUSTRY_CODE',
                        'INDUSTRY_TYPE',
                        'GEOGRAPHY_CODE',
                        'EMPLOYMENT_SIZEBAND_NAME',
                        'LEGAL_STATUS_NAME',
                        'OBS_VALUE'
                      )
  )
  
  saveRDS(z,paste0('local/data/BusinessCountsByNUTS2/BUSINESSCOUNT_ENTERPRISE_NUTS2_',year,'.rds'))
  
  print(proc.time()-x)
  
  Sys.sleep(20)
  
}

lapply(years, function(x) download_all_BUSINESSCOUNT_ENTERPRISE(x))


#LOCAL UNIT LEVEL
download_all_BUSINESSCOUNT_LOCALUNITS <- function(year){
  
  cat('Downloading year ',year,'\n')
  
  x <- proc.time()
  
  #If select is done after download, this isn't going to make any difference... no, it's faster
  z <- nomis_get_data(id = "NM_141_1", time = year, geography = "TYPE450", measures = 20100, 
                      select = c(
                        'DATE',
                        'GEOGRAPHY_NAME',
                        'INDUSTRY_NAME',
                        'INDUSTRY_CODE',
                        'INDUSTRY_TYPE',
                        'GEOGRAPHY_CODE',
                        'EMPLOYMENT_SIZEBAND_NAME',
                        'LEGAL_STATUS_NAME',
                        'OBS_VALUE'
                      )
  )
  
  saveRDS(z,paste0('local/data/BusinessCountsByNUTS2/BUSINESSCOUNT_LOCALUNITS_NUTS2_',year,'.rds'))
  
  print(proc.time()-x)
  
  Sys.sleep(20)
  
}

lapply(years, function(x) download_all_BUSINESSCOUNT_LOCALUNITS(x))





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#ITL2 / NUTS2: COMBINE COUNTS DATA INTO SINGLE DF----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Will need reducing a lot to be manageable. What are we after? Look at one to pick out
x <- readRDS('local/data/BusinessCountsByNUTS2/BUSINESSCOUNT_ENTERPRISE_NUTS2_2020.rds')
#x <- readRDS('local/data/BusinessCountsByNUTS2/BUSINESSCOUNT_LOCALUNITS_GB_2016.rds')

unique(x$EMPLOYMENT_SIZEBAND_NAME)
#There are overlapping categories. We may want to keep these at some later time, but let's look at the highest resolution first
#Dropping these:
#Micro (0 to 9)
#Small (10 to 49)
#Medium-sized (50 to 249)
#Large (250+)

#Public / private will be useful. Let's start with total, though.
unique(x$LEGAL_STATUS_NAME)

#5 digit for now
unique(x$INDUSTRY_TYPE)


#9559200 to 262440 rows, 19mb.
x <- x %>% 
  filter(!EMPLOYMENT_SIZEBAND_NAME %in% c(
    'Micro (0 to 9)',
    'Small (10 to 49)',
    'Medium-sized (50 to 249)',
    'Large (250+)',
    'Total'
  ),
  LEGAL_STATUS_NAME == 'Total',
  INDUSTRY_NAME != 'Total',
  INDUSTRY_TYPE == 'SIC 2007 subclass (5 digit)'
  )



#Currently hardcoding filter choices...
LOADBUSINESSCOUNTS_and_reduce <- function(filename){
  
  readRDS(filename) %>% filter(!EMPLOYMENT_SIZEBAND_NAME %in% c(
    'Micro (0 to 9)',
    'Small (10 to 49)',
    'Medium-sized (50 to 249)',
    'Large (250+)',
    'Total'
  ),
  LEGAL_STATUS_NAME == 'Total',
  INDUSTRY_NAME != 'Total',
  INDUSTRY_TYPE == 'SIC 2007 subclass (5 digit)'
  ) %>% 
    select(DATE,GEOGRAPHY_NAME,GEOGRAPHY_CODE,INDUSTRY_NAME,INDUSTRY_CODE,EMPLOYMENT_SIZEBAND_NAME,COUNT=OBS_VALUE)
  
}

#All years... 103mb, very reasonable!
bc <- list.files(path = "local/data/BusinessCountsByNUTS2/", pattern = "ENTERPRISE_NUTS2", full.names = T) %>% 
  map(LOADBUSINESSCOUNTS_and_reduce) %>% 
  bind_rows()

#One single INDUSTY_NAME field has a non-UTF8 char that breaks some things
#Check for annoying chars and fix
#https://stackoverflow.com/a/17292126
bc$INDUSTRY_NAME <- iconv(bc$INDUSTRY_NAME, "UTF-8", "UTF-8",sub='')

#Save!
saveRDS(bc, 'data/sectors/ITL2_BUSINESSCOUNTS_ENTERPRISE_9CATSIZEBAND_LEGALSTATUSALL_INDUSTRYTYPE5DIGIT_16to21.rds')





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#BUSINESS COUNTS: GREAT BRITAIN BULK DOWNLOAD----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#GETTING YEARLY IN ONE GO
years = c(2016:2022)

#ENTERPRISE LEVEL
download_all_BUSINESSCOUNT_ENTERPRISE_GB <- function(year){
  
  cat('Downloading year ',year,'\n')
  
  x <- proc.time()
  
  #If select is done after download, this isn't going to make any difference... no, it's faster
  z <- nomis_get_data(id = "NM_142_1", time = year, geography = "2092957698", measures = 20100, 
                      select = c(
                        'DATE',
                        'GEOGRAPHY_NAME',
                        'INDUSTRY_NAME',
                        'INDUSTRY_CODE',
                        'INDUSTRY_TYPE',
                        'GEOGRAPHY_CODE',
                        'EMPLOYMENT_SIZEBAND_NAME',
                        'LEGAL_STATUS_NAME',
                        'OBS_VALUE'
                      )
  )
  
  saveRDS(z,paste0('local/data/BusinessCountsByNUTS2/BUSINESSCOUNT_ENTERPRISE_GB_',year,'.rds'))
  
  print(proc.time()-x)
  
}

lapply(years, function(x) download_all_BUSINESSCOUNT_ENTERPRISE_GB(x))


#LOCAL UNIT LEVEL
download_all_BUSINESSCOUNT_LOCALUNITS_GB <- function(year){
  
  cat('Downloading year ',year,'\n')
  
  x <- proc.time()
  
  #That's the code for Great Britain in geography
  z <- nomis_get_data(id = "NM_141_1", time = year, geography = "2092957698", measures = 20100, 
                      select = c(
                        'DATE',
                        'GEOGRAPHY_NAME',
                        'INDUSTRY_NAME',
                        'INDUSTRY_CODE',
                        'INDUSTRY_TYPE',
                        'GEOGRAPHY_CODE',
                        'EMPLOYMENT_SIZEBAND_NAME',
                        'LEGAL_STATUS_NAME',
                        'OBS_VALUE'
                      )
  )
  
  saveRDS(z,paste0('local/data/BusinessCountsByNUTS2/BUSINESSCOUNT_LOCALUNITS_GB_',year,'.rds'))
  
  print(proc.time()-x)
  
}

lapply(years, function(x) download_all_BUSINESSCOUNT_LOCALUNITS_GB(x))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#GB: COMBINE COUNTS DATA INTO SINGLE DF----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Same function should work, loaded above in ITL2 version of combine

#All years... 2.7mb
gb <- list.files(path = "local/data/BusinessCountsByNUTS2/", pattern = "ENTERPRISE_GB", full.names = T) %>% 
  map(LOADBUSINESSCOUNTS_and_reduce) %>% 
  bind_rows()

#One single INDUSTY_NAME field has a non-UTF8 char that breaks some things
#Check for annoying chars and fix
#https://stackoverflow.com/a/17292126
gb$INDUSTRY_NAME <- iconv(gb$INDUSTRY_NAME, "UTF-8", "UTF-8",sub='')

#Save!
saveRDS(gb, 'data/sectors/GB_BUSINESSCOUNTS_ENTERPRISE_9CATSIZEBAND_LEGALSTATUSALL_INDUSTRYTYPE5DIGIT_16to21.rds')





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#GB: ENTERPRISE BUSINESS COUNT EXPLORE----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

gb <- readRDS('data/sectors/GB_BUSINESSCOUNTS_ENTERPRISE_9CATSIZEBAND_LEGALSTATUSALL_INDUSTRYTYPE5DIGIT_16to21.rds')

#NOTE, SOME OF THESE ODDITIES DUE TO DOWNLOAD ERROR GETTING JUST 2022 NOT ALL YEARS

#Some oddities here immediately:
#I know there's some nuclear fuel processing goes on. So why is that zero?
counts <- gb %>%
  filter(DATE==2020) %>% 
  group_by(INDUSTRY_NAME) %>% 
  summarise(av = mean(COUNT))

#zero across all years?
counts_allyears <- gb %>%
  group_by(INDUSTRY_NAME) %>% 
  summarise(av = mean(COUNT))

#How many that are zero match across all years and just one?
table(
  (counts %>% filter(av == 0) %>% select(INDUSTRY_NAME) %>% pull) %in% 
    (counts_allyears %>% filter(av == 0) %>% select(INDUSTRY_NAME) %>% pull)
  )

#The same 21 are coming up true?
allyr <- counts_allyears %>% filter(av == 0) %>% select(INDUSTRY_NAME) %>% pull
oneyr <- counts %>% filter(av == 0) %>% select(INDUSTRY_NAME) %>% pull

#Yes, that's the same across years (checked by manually changing year above)
allyr[allyr %in% oneyr]

#Hmm.
gb %>% filter(grepl('cement',INDUSTRY_NAME), DATE==2022) %>% View



#Let's just look at one grouping, to look for change
sizeband <- gb %>% 
  filter(
    # EMPLOYMENT_SIZEBAND_NAME == '0 to 4',
    # EMPLOYMENT_SIZEBAND_NAME == '5 to 9',
    # EMPLOYMENT_SIZEBAND_NAME == '5 to 9',
    EMPLOYMENT_SIZEBAND_NAME == '500 to 999',
    # EMPLOYMENT_SIZEBAND_NAME == '1000+',
    COUNT > 20
    )

#How many SIC sectors per year? Need to check this against other SIC code levels.
sizeband %>% 
  group_by(DATE) %>% 
  summarise(n())

#TOp ones in most recent year
top <- sizeband %>% 
  filter(DATE==2022, COUNT > 3000)#500 to 999
  # filter(DATE==2022, COUNT > 3000)#5 TO 9
  # filter(DATE==2022, COUNT > 15000)#0 TO 4

#One year (to see all)
ggplot(sizeband %>% filter(DATE == 2022),
       aes(y = fct_reorder(INDUSTRY_NAME,COUNT), x = COUNT)
       ) +
  geom_bar(stat='identity') +
  facet_wrap(~DATE)



#All years (filtered down by count)
# ggplot(sizeband %>% filter(INDUSTRY_NAME %in% top$INDUSTRY_NAME),
ggplot(sizeband,
       aes(y = fct_reorder(INDUSTRY_NAME,COUNT), x = COUNT)
       ) +
  geom_bar(stat='identity') +
  facet_wrap(~DATE)









