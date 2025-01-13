#Beatty fothergill update to most recent data that matches
#Cf. https://www.shu.ac.uk/centre-regional-economic-social-research/publications/local-productivity-the-real-differences-across-uk-cities-and-regions
#But we want to do for ITL2s.
library(tidyverse)
library(zoo)
library(sf)
library(tmap)
library(plotly)
library(nomisr)
library(pryr)



#RAW REGIONAL GVA DATA----

#Taking code to update to latest from here:
#https://github.com/DanOlner/ukcompare/blob/095d7efc8a308c60743e2940f8c7959512f09ec7/explore_code/GVA_region_by_sector_explore.R#L6822
#(Currently 6822 in GVA_region_by_sector_explore.R)

#This is jobcounts via BRES. Does it match job counts from the per filled job data?
#Also: it's already got imputed rent removed, to match sectors in BRES (imputed rent of course doesn't have any related jobs)

#All done there and saved, so can reuse for 2022
# itl2.gvaperjob22 <- readRDS('data/itl2_gva_to2022_plusBRES_jobs_to2022.rds')



#GET LATEST REGIONAL GVA DATA----

#Code taken from here, section "UPDATE ITL2 GVA AND JOB DATA TO 2022 / % change plots": 
#https://github.com/DanOlner/ukcompare/blob/095d7efc8a308c60743e2940f8c7959512f09ec7/explore_code/GVA_region_by_sector_explore.R#L6832

#Latest release here:
#https://www.ons.gov.uk/economy/grossvalueaddedgva/datasets/nominalandrealregionalgrossvalueaddedbalancedbyindustry
#April 2024 release for data up to 2022

#Workaround for lack of URL download native in readxl package
#Via https://stackoverflow.com/a/79311678/5023561
url1 <- 'https://www.ons.gov.uk/file?uri=/economy/grossvalueaddedgva/datasets/nominalandrealregionalgrossvalueaddedbalancedbyindustry/current/regionalgrossvalueaddedbalancedbyindustryandallitlregions.xlsx'
p1f <- tempfile(fileext=".xlsx")
download.file(url1, p1f, mode="wb")

#Table 2c is current prices
gva <- readxl::read_excel(path = p1f,range = "Table 2c!A2:AC3938") 

names(gva) <- gsub(x = names(gva), pattern = ' ', replacement = '_')

#Keep SIC sections
#This gets all the letters, nice
SICkeeps <- gva$SIC07_code[substr(gva$SIC07_code,2,2) == ' '] %>% unique


#TWO VERSIONS - ONE THAT KEEPS IMPUTED RENT, ONE THAT REMOVES
#To do the latter for SIC sections, just need to replace the SIC section that includes it with the lower level SIC that doesn't
#This is also then the quickest way to remove imputed rent from the national total and regional totals
#(Reminder - which we can only do using 'current prices' because only those can be re-summed, unlike chained volume measures)

#REPLACE "L (68)" REAL ESTATE ACTIVITIES (WHICH INCLUDES IMPUTED RENT) WITH JUST 68 "Real estate activities, excluding imputed rental"  
SICkeeps_minusImputedRent = SICkeeps
SICkeeps_minusImputedRent[SICkeeps_minusImputedRent == 'L (68)'] <- '68'


#Filter out duplicate value rows and make long by year
#Also convert year to numeric
#NEED TO MANUALLY UPDATE LATEST YEAR
gva.all <- gva %>% 
  filter(SIC07_code %in% SICkeeps) %>% 
  pivot_longer(`1998`:`2022`, names_to = 'year', values_to = 'value') %>% 
  mutate(year = as.numeric(year))

unique(gva.all$Region_name)
unique(gva.all$SIC07_description)


#And non imputed rent version
gva.minusimputedrent <- gva %>% 
  filter(SIC07_code %in% SICkeeps_minusImputedRent) %>% 
  pivot_longer(`1998`:`2022`, names_to = 'year', values_to = 'value') %>% 
  mutate(year = as.numeric(year))

unique(gva.minusimputedrent$Region_name)
unique(gva.minusimputedrent$SIC07_description)





#GET LATEST HOURLY / PER FILLED JOB NUMBERS----

#Current versions as code being written (ONS should link to latest when updated at the top of these)
#"Subregional productivity: labour productivity indices by UK ITL2 and ITL3 subregions" - 
#https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/labourproductivity/datasets/subregionalproductivitylabourproductivitygvaperhourworkedandgvaperfilledjobindicesbyuknuts2andnuts3subregions

#Via here in section 4, where different geographies for the same data listed:
#https://www.ons.gov.uk/economy/economicoutputandproductivity/productivitymeasures/bulletins/regionalandsubregionallabourproductivityuk/2022

url1 <- 'https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/labourproductivity/datasets/subregionalproductivitylabourproductivitygvaperhourworkedandgvaperfilledjobindicesbyuknuts2andnuts3subregions/current/labourproductivityitls.xls'
p1f <- tempfile(fileext=".xls")
download.file(url1, p1f, mode="wb")

perhourworked <- readxl::read_excel(path = p1f,range = "Productivity Hours!A5:V239") 

perfilledjob <- readxl::read_excel(path = p1f,range = "Productivity Jobs!A5:X239") 


#GET LATEST REGIONAL POPULATION NUMBERS----

#... Which can handily be found in the regional GDP data xls
#https://www.ons.gov.uk/economy/grossdomesticproductgdp/datasets/regionalgrossdomesticproductallnutslevelregions

url1 <- 'https://www.ons.gov.uk/file?uri=/economy/grossdomesticproductgdp/datasets/regionalgrossdomesticproductallnutslevelregions/1998to2022/regionalgrossdomesticproductgdpbyallitlregions.xlsx'
p1f <- tempfile(fileext=".xlsx")
download.file(url1, p1f, mode="wb")

residentpop <- readxl::read_excel(path = p1f,range = "Table 6!A2:AB238") 




#NOMISR FOR LATEST 16-64 POP NUMBERS----

#https://cran.r-project.org/web/packages/nomisr/vignettes/introduction.html

#Annual population survey...
#Find correct dataset
x <- nomis_data_info()

#search...
rez <- x %>% filter(grepl(pattern = 'annual pop', x = name.value, ignore.case = T))

#This is just 'annual population survey', no filters
a <- nomis_get_metadata(id = "NM_17_1")

#List of variables.... nearly 4000
#402720769 = T01:22 (Aged 16-64 - All : All People )
vars <- nomis_get_metadata(id = "NM_17_1", concept = "CELL")

#List of geographies
nomis_get_metadata(id = "NM_17_1", concept = "GEOGRAPHY", type = "type") %>% print(n=60)


#Get ITL2 (NUTS2 2016 as was then) and 16 - 64, geography is TYPE438
#Results:
pop16to64 <- nomis_get_data(id = "NM_17_1", geography = "TYPE438", cell = "402720769")

#No values in the data for this geography prior to 2012
unique(pop16to64$DATE_NAME[!is.na(pop16to64$OBS_VALUE)])


#Geography fun - the NUTS2 2016 zones have everything that's now an ITL2 zone...
unique(perhourworked$Region_name[perhourworked$ITL_level=='ITL2']) %in% unique(pop16to64$GEOGRAPHY_NAME)

#(Except for the usual comma differences for two of them....)
unique(perhourworked$Region_name[perhourworked$ITL_level=='ITL2'])[!unique(perhourworked$Region_name[perhourworked$ITL_level=='ITL2']) %in% unique(pop16to64$GEOGRAPHY_NAME)]

#But it DOESN'T HAVE Northern Ireland in there
#Which the APS/LFS does have...

#So let's get that separately
#(And then also get UK total numbers, as will need those too)
#TYPE499 is countries
ni <- nomis_get_data(id = "NM_17_1", geography = "TYPE499", cell = "402720769") %>% 
  filter(GEOGRAPHY_NAME == "Northern Ireland")

#Check column match and join with NUTS2 2016 to make complete 41 zone ITL2 set
table(names(ni) %in% names(pop16to64))#tick

pop16to64 <- pop16to64 %>% bind_rows(ni)

#Tick (except for a few non-matching names due to text diffs... but we can't use codes now because of NI, so fix manually when we need to match)
unique(pop16to64$GEOGRAPHY_NAME)



#Also get UK total population values for 16-64
uk16to64 <- nomis_get_data(id = "NM_17_1", geography = "TYPE499", cell = "402720769") %>% 
  filter(GEOGRAPHY_NAME == "United Kingdom")



#Reduce both of those down to the bits we'll use
pop16to64 <- pop16to64 %>% 
  filter(
    grepl('Jan', DATE_NAME)#keep only one of the quarterly vals (check if should be smoothing for better, but I think that's already done in APS)
  ) %>% 
  mutate(DATE = str_sub(DATE, start = 1, end = 4) %>% as.numeric) %>% 
  filter(
    DATE > 2011#No values before 2012
  ) %>% 
  select(
    DATE,GEOGRAPHY_NAME,GEOGRAPHY_CODE,MEASURES_NAME,OBS_VALUE
  ) %>% 
  pivot_wider(
    names_from = MEASURES_NAME, values_from = OBS_VALUE 
  )


uk16to64 <- uk16to64 %>% 
  filter(
    grepl('Jan', DATE_NAME)#keep only one of the quarterly vals (check if should be smoothing for better, but I think that's already done in APS)
  ) %>% 
  mutate(DATE = str_sub(DATE, start = 1, end = 4) %>% as.numeric) %>% 
  filter(
    DATE > 2011#No values before 2012
  ) %>% 
  select(
    DATE,GEOGRAPHY_NAME,GEOGRAPHY_CODE,MEASURES_NAME,OBS_VALUE
  ) %>% 
  pivot_wider(
    names_from = MEASURES_NAME, values_from = OBS_VALUE 
  )













