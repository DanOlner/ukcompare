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
options(scipen = 99)


#~~~~~~~~~~~~~~~~
#GET DATASETS----
#~~~~~~~~~~~~~~~~

##1. REGIONAL AND UK GVA CURRENT PRICES, WITH/WITHOUT IMPUTED RENT----

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


#Check that gva.all total does equal the total for each ITL2 in the current prices sheet...
gva.totals <- gva %>% 
  filter(SIC07_code == 'Total') %>% 
  pivot_longer(`1998`:`2022`, names_to = 'year', values_to = 'value') %>% 
  mutate(year = as.numeric(year))

gva.totals.summedfromsections <- gva.all %>% 
  group_by(Region_name,year) %>% 
  summarise(value = sum(value)) %>% 
  ungroup()

#Yep, all looks within rounding errors on the individual sectors
chk <- gva.totals %>% 
  left_join(
    gva.totals.summedfromsections %>% rename(valuefromsum = value),
    by = c('Region_name','year')
  ) %>% 
  mutate(diff = value - valuefromsum)

#Check total differences between those... 10,000th of a percent difference, fine
chk %>% summarise(across(value:valuefromsum,sum))

#Can use gva.totals for anything including imputed rent (which I think let's just not do at the moment)



#Also get total GVA current price number for the UK as a whole
#For working out crude average
gva.uk <- readxl::read_excel(path = p1f,range = "Table 1c!A2:AC109")#cell range gets JUST UK 

names(gva.uk) <- gsub(x = names(gva.uk), pattern = ' ', replacement = '_')

#With and without imputed rent again
gva.uk.all <- gva.uk %>% 
  filter(SIC07_code %in% SICkeeps) %>% 
  pivot_longer(`1998`:`2022`, names_to = 'year', values_to = 'value') %>% 
  mutate(year = as.numeric(year))

gva.uk.minusimputedrent <- gva.uk %>% 
  filter(SIC07_code %in% SICkeeps_minusImputedRent) %>% 
  pivot_longer(`1998`:`2022`, names_to = 'year', values_to = 'value') %>% 
  mutate(year = as.numeric(year))



#Last step: GVA totals for all ITL2 zones AND the UK as a whole MINUS imputed rent
gva.itl2.totals.minusimputedrent <- gva.minusimputedrent %>% 
  group_by(Region_name,year) %>% 
  summarise(value = sum(value)) %>% 
  ungroup()

gva.uk.totals.minusimputedrent <- gva.uk.minusimputedrent %>% 
  group_by(year) %>% 
  summarise(value = sum(value)) %>% 
  ungroup()
  

##2. GET LATEST HOURLY / PER FILLED JOB NUMBERS----

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

#HOURS WORKED PER WEEK - needs multiplying up to match yearly GVA values
perhourworked.itl2 <- perhourworked %>% 
  filter(ITL_level == 'ITL2') %>% 
  select(-ITL_level) %>% 
  pivot_longer(contains('Hours'), names_to = 'year', values_to = 'hoursworked') %>% 
  mutate(year = str_sub(year, start = 7, end = 10) %>% as.numeric)

perhourworked.uk <- perhourworked %>% 
  filter(ITL_code == 'UKX') %>% 
  select(-ITL_level) %>% 
  pivot_longer(contains('Hours'), names_to = 'year', values_to = 'hoursworked') %>% 
  mutate(year = str_sub(year, start = 7, end = 10) %>% as.numeric)


perfilledjob.itl2 <- perfilledjob %>% 
  filter(ITL_level == 'ITL2') %>% 
  select(-ITL_level) %>% 
  pivot_longer(contains('Jobs'), names_to = 'year', values_to = 'jobsfilled') %>% 
  mutate(year = str_sub(year, start = 6, end = 9) %>% as.numeric)

perfilledjob.uk <- perfilledjob %>% 
  filter(ITL_code == 'UKX') %>% 
  select(-ITL_level) %>% 
  pivot_longer(contains('Jobs'), names_to = 'year', values_to = 'jobsfilled') %>% 
  mutate(year = str_sub(year, start = 6, end = 9) %>% as.numeric)




##3. GET LATEST REGIONAL POPULATION NUMBERS----

#... Which can handily be found in the regional GDP data xls
#Though these don't have CIs, like we can get from the APS data...
#https://www.ons.gov.uk/economy/grossdomesticproductgdp/datasets/regionalgrossdomesticproductallnutslevelregions

url1 <- 'https://www.ons.gov.uk/file?uri=/economy/grossdomesticproductgdp/datasets/regionalgrossdomesticproductallnutslevelregions/1998to2022/regionalgrossdomesticproductgdpbyallitlregions.xlsx'
p1f <- tempfile(fileext=".xlsx")
download.file(url1, p1f, mode="wb")

#Last col is character, not numeric... 
#Due to ONS introducing some "not trustworthy number" indicators in some regions
#Converting to numeric will convert these to NA, which is fine...
#It does affect some Scots ITL2s, but not the UK total
residentpop <- readxl::read_excel(path = p1f,range = "Table 6!A2:AB238") %>%
  rename(Region_name = `Region name`) %>% 
  mutate(`2022` = as.numeric(`2022`))

residentpop.itl2 <- residentpop %>% 
  filter(ITL == 'ITL2') %>% 
  select(-ITL) %>% 
  pivot_longer(cols = 3:length(names(.)), names_to = 'year', values_to = 'resident_pop') %>% 
  mutate(year = as.numeric(year))

residentpop.uk <- residentpop %>% 
  filter(`ITL code` == 'UKX') %>% 
  select(-ITL) %>% 
  pivot_longer(cols = 3:length(names(.)), names_to = 'year', values_to = 'resident_pop') %>%  
  mutate(year = as.numeric(year))







##4. NOMISR FOR LATEST "16+ IN EMPLOYMENT" POP NUMBERS----

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
varz <- nomis_get_metadata(id = "NM_17_1", concept = "CELL")

#List of geographies
nomis_get_metadata(id = "NM_17_1", concept = "GEOGRAPHY", type = "type") %>% print(n=60)


#Get ITL2 (NUTS2 2016 as was then) and 16 - 64, geography is TYPE438
#Results:
pop16emp <- nomis_get_data(id = "NM_17_1", geography = "TYPE438", cell = "402719489")

#No values in the data for this geography prior to 2012
unique(pop16emp$DATE_NAME[!is.na(pop16emp$OBS_VALUE)])


#Geography fun - the NUTS2 2016 zones have everything that's now an ITL2 zone...
unique(perhourworked$Region_name[perhourworked$ITL_level=='ITL2']) %in% unique(pop16emp$GEOGRAPHY_NAME)

#(Except for the usual comma differences for two of them....)
unique(perhourworked$Region_name[perhourworked$ITL_level=='ITL2'])[!unique(perhourworked$Region_name[perhourworked$ITL_level=='ITL2']) %in% unique(pop16emp$GEOGRAPHY_NAME)]

#But it DOESN'T HAVE Northern Ireland in there
#Which the APS/LFS does have...

#So let's get that separately
#(And then also get UK total numbers, as will need those too)
#TYPE499 is countries
ni <- nomis_get_data(id = "NM_17_1", geography = "TYPE499", cell = "402719489") %>% 
  filter(GEOGRAPHY_NAME == "Northern Ireland")

#Check column match and join with NUTS2 2016 to make complete 41 zone ITL2 set
table(names(ni) %in% names(pop16emp))#tick

pop16emp <- pop16emp %>% bind_rows(ni)

#Tick (except for a few non-matching names due to text diffs... but we can't use codes now because of NI, so fix manually when we need to match)
unique(pop16emp$GEOGRAPHY_NAME)



#Also get UK total population values for 16+ in employment
uk16emp <- nomis_get_data(id = "NM_17_1", geography = "TYPE499", cell = "402719489") %>% 
  filter(GEOGRAPHY_NAME == "United Kingdom")



#Reduce both of those down to the bits we'll use
pop16emp <- pop16emp %>% 
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
  ) %>% 
  rename(Region_name = GEOGRAPHY_NAME)


uk16emp <- uk16emp %>% 
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
  ) %>% 
  rename(Region_name = GEOGRAPHY_NAME)



##5. NOMISR FOR LATEST 16-64 POP NUMBERS----

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
  ) %>% 
  rename(Region_name = GEOGRAPHY_NAME)


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
  ) %>% 
  rename(Region_name = GEOGRAPHY_NAME)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#CHECK ITL2 NAME/CODE MATCHES----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#At least one - 16 to 64 counts - won't have matching codes as NI was added separately
#But what about name matches? Usual list of ones that don't match...?
allnames <- list(
  unique(gva.itl2.totals.minusimputedrent$Region_name),
  unique(perhourworked.itl2$Region_name),
  unique(perfilledjob.itl2$Region_name),
  unique(residentpop.itl2$Region_name),
  unique(pop16to64$Region_name),
  unique(pop16emp$Region_name)
)

#Perfect matches all round, nice
mapply(function(x, y) sum(x %in% y), allnames, allnames)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#MAKE UK LEVEL NUMERATORS/DENOMINATORS----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Add each to the UK GVA df to be used directly

#Use only minus imputed rent for now...
#Not all years will match, those will be NA, that's OK...

#1. GVA in UK (no imputed rent) over total population
#sub with "UK" as we'll combine into one DF with ITL2 after this
gva.uk.totals.mir.ratios <- gva.uk.totals.minusimputedrent %>% 
  left_join(
    residentpop.uk %>% select(year,resident_pop), by = 'year'
  ) %>% 
  mutate(
    UK_gvaperhead_pounds_cp = (value / resident_pop) * 1000000
  )

#2. GVA over 16 to 64 pop
gva.uk.totals.mir.ratios <- gva.uk.totals.mir.ratios %>% 
  left_join(
    uk16to64 %>% select(year = DATE,count16to64 = Value), by = 'year'
  ) %>% 
  mutate(
    UK_gvaper16to64_pounds_cp = (value / count16to64) * 1000000
  )

#3. GVA over 16+ in employment
gva.uk.totals.mir.ratios <- gva.uk.totals.mir.ratios %>% 
  left_join(
    uk16emp %>% select(year = DATE,count16plus_employed = Value), by = 'year'
  ) %>% 
  mutate(
    UK_gvaper16plus_employed_pounds_cp = (value / count16plus_employed) * 1000000
  )

#4. GVA over hours worked
#HOURS WORKED PER WEEK - needs multiplying up to match yearly GVA values
gva.uk.totals.mir.ratios <- gva.uk.totals.mir.ratios %>% 
  left_join(
    perhourworked.uk %>% select(year,hoursworked), by = 'year'
  ) %>% 
  mutate(
    UK_perhourworked_pounds_cp = (value / (hoursworked * 52)) * 1000000
  )

#5. per filled job
gva.uk.totals.mir.ratios <- gva.uk.totals.mir.ratios %>% 
  left_join(
    perfilledjob.uk %>% select(year,jobsfilled), by = 'year'
  ) %>% 
  mutate(
    UK_perfilledjob_pounds_cp = (value / jobsfilled) * 1000000
  )




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#MAKE ITL2 NUMERATORS/DENOMINATORS----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


gva.itl2.totals.mir.ratios <- gva.itl2.totals.minusimputedrent %>% 
  left_join(
    residentpop.itl2 %>% select(year,Region_name,resident_pop), by = c('year','Region_name')
  ) %>% 
  mutate(
    gvaperhead_pounds_cp = (value / resident_pop) * 1000000
  )

#2. GVA over 16 to 64 pop
gva.itl2.totals.mir.ratios <- gva.itl2.totals.mir.ratios %>% 
  left_join(
    pop16to64 %>% select(year = DATE,Region_name,count16to64 = Value), by = c('year','Region_name')
  ) %>% 
  mutate(
    gvaper16to64_pounds_cp = (value / count16to64) * 1000000
  )

#3. GVA over 16 plus employed
gva.itl2.totals.mir.ratios <- gva.itl2.totals.mir.ratios %>% 
  left_join(
    pop16emp %>% select(year = DATE,Region_name,count16plus_employed = Value), by = c('year','Region_name')
  ) %>% 
  mutate(
    gvaper16plus_employed_pounds_cp = (value / count16plus_employed) * 1000000
  )

#4. GVA over hours worked
#HOURS WORKED PER WEEK - needs multiplying up to match yearly GVA values
gva.itl2.totals.mir.ratios <- gva.itl2.totals.mir.ratios %>% 
  left_join(
    perhourworked.itl2 %>% select(year,Region_name,hoursworked), by = c('year','Region_name')
  ) %>% 
  mutate(
    perhourworked_pounds_cp = (value / (hoursworked * 52)) * 1000000
  )

#5. per filled job
gva.itl2.totals.mir.ratios <- gva.itl2.totals.mir.ratios %>% 
  left_join(
    perfilledjob.itl2 %>% select(year,Region_name,jobsfilled), by = c('year','Region_name')
  ) %>% 
  mutate(
    perfilledjob_pounds_cp = (value / jobsfilled) * 1000000
  )



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#COMBINE TO FIND ITL2 % DIFF TO UK AVERAGES----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

allz <- gva.itl2.totals.mir.ratios %>% 
  select(Region_name,year,gvaperhead_pounds_cp,gvaper16to64_pounds_cp,gvaper16plus_employed_pounds_cp,perhourworked_pounds_cp,perfilledjob_pounds_cp) %>% 
  left_join(
    gva.uk.totals.mir.ratios %>% 
      select(year,UK_gvaperhead_pounds_cp,UK_gvaper16to64_pounds_cp,UK_gvaper16plus_employed_pounds_cp,UK_perhourworked_pounds_cp,UK_perfilledjob_pounds_cp),
    by = 'year'
  )

#Save...
write_csv(allz,'data/GVA_measures_ITL2_aspercentofUKaverage1998_2022.csv')

#Reload!
# allz <- read_csv('data/GVA_measures_ITL2_aspercentofUKaverage1998_2022.csv')

#Then find ITL2 zone GVA values as percentage of UK average
allz <- allz %>% 
  mutate(
    gvaperhead_percentofUKav = (gvaperhead_pounds_cp/UK_gvaperhead_pounds_cp) * 100,
    gvaper16to64_percentofUKav = (gvaper16to64_pounds_cp/UK_gvaper16to64_pounds_cp) * 100,
    gvaper16plus_employed_percentofUKav = (gvaper16plus_employed_pounds_cp/UK_gvaper16plus_employed_pounds_cp) * 100,
    gvaperhourworked_percentofUKav = (perhourworked_pounds_cp/UK_perhourworked_pounds_cp) * 100,
    gvaperjobfilled_percentofUKav = (perfilledjob_pounds_cp/UK_perfilledjob_pounds_cp) * 100
  )


#first up for plot with view of data, facet the data type (make long)
allz_long <- allz %>% 
  select(
    Region_name,year,
    `GVA per head (percent of UK av)` = gvaperhead_percentofUKav,
    `GVA per 16 to 64 yr old (percent of UK av)` = gvaper16to64_percentofUKav,
    `GVA per 16+ in employment (percent of UK av)` = gvaper16plus_employed_percentofUKav,
    `GVA per filled job (percent of UK av)` = gvaperjobfilled_percentofUKav,
    `GVA per hour worked (percent of UK av)` = gvaperhourworked_percentofUKav
    ) %>% 
  pivot_longer(
    cols = `GVA per head (percent of UK av)`:`GVA per hour worked (percent of UK av)`,
    names_to = 'measure', values_to = 'percent of UK average'
  ) %>% 
  mutate(
    SY = ifelse(Region_name == 'South Yorkshire', 'SY', 'other')
  )


allz_long.minusNAs <- allz_long[!is.na(allz_long$`percent of UK average`),]


p <- ggplot(
  # allz_long.minusNAs,
  allz_long.minusNAs %>% filter(!Region_name %in% c('Inner London - West', 'Inner London - East')),#filter out London outliers
  aes(x = year, y = `percent of UK average`, colour = SY, alpha = SY, size = SY, group = Region_name)) +
  geom_point() +
  # geom_jitter(width = 0.25) +
  geom_line(size = 0.25) +
  geom_hline(yintercept = 100) +
  scale_colour_manual(values = c('black','red')) +
  scale_alpha_manual(values = c(0.25,1)) +
  scale_size_manual(values = c(0.5,3)) +
  facet_wrap(~measure, scales = 'free') +
  guides(alpha = F, size = F)

p

ggplotly(p, tooltip = c('year','Region_name','percent of UK average'))



#Smoothed version to see trends a little more clearly
allz.smooth <- allz_long.minusNAs %>% 
  group_by(Region_name,measure) %>% 
  mutate(percentUKav_movingav = rollapply(`percent of UK average`,3,mean,align='center',fill=NA)) %>% 
  ungroup()

p <- ggplot(
  allz.smooth[!is.na(allz.smooth$percentUKav_movingav),] %>% filter(!Region_name %in% c('Inner London - West', 'Inner London - East')),#filter out London outliers
  aes(x = year, y = percentUKav_movingav, colour = SY, alpha = SY, size = SY, group = Region_name)) +
  geom_point() +
  # geom_jitter(width = 0.25) +
  geom_line(size = 0.25) +
  geom_hline(yintercept = 100) +
  scale_colour_manual(values = c('black','red')) +
  scale_alpha_manual(values = c(0.25,1)) +
  scale_size_manual(values = c(0.5,3)) +
  facet_wrap(~measure, scales = 'free') +
  guides(alpha = F, size = F)

p

ggplotly(p, tooltip = c('year','Region_name','percent of UK average'))



#Version that shows actual values, including UK average overlaid
#So we can see what "percent of UK av" actually means...

#Just for 2022
actualz <- allz %>% filter(year == 2022) %>% 
  pivot_longer(
    gvaperhead_pounds_cp:gvaperjobfilled_percentofUKav, names_to = 'variable', values_to = 'value_pounds'
  ) %>% 
  filter(!qg('percent',variable)) %>% #remove percent cols, don't need here
  mutate(
    UKaveragevalues = ifelse(qg('UK_',variable),'UK average, pounds','region, pounds'),#flag if UK average value
    variable = gsub('UK_','', variable), #now flagged, make same cat, so will overlay same x category
    SY = ifelse(Region_name == 'South Yorkshire', 'SY', 'Other region'),
  ) %>% 
  filter(value_pounds < 100000)


p <- ggplot(actualz, aes(x = 1, y = value_pounds, colour = UKaveragevalues, size = SY)) +
  # geom_point() +
  geom_jitter(width = 0.0001) +
  facet_wrap(~variable, ncol = 1, scales = 'free_x') +
  coord_flip()

ggplotly(p, tooltip = 'value_pounds', width = 1000, height = 600)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#"ADJUST FOR INDUSTRY MIX"----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#From B+F: "what would have been each area’s GVA per job if each industry in the area had the UK average GVA per job for that industry."

#They avoid BRES due to lack of self-employed, but it has the advantage of better sector granularity.
#And being more up to date.

#For 2022, already have one prepped.
#https://github.com/DanOlner/ukcompare/blob/095d7efc8a308c60743e2940f8c7959512f09ec7/explore_code/GVA_region_by_sector_explore.R#L6822
#(Currently 6822 in GVA_region_by_sector_explore.R)
itl2.gvaperjob22 <- readRDS('data/itl2_gva_to2022_plusBRES_jobs_to2022.rds') %>% 
  mutate(gvaperjob = (gva/jobcount)*1000000)

#For what years do we have missing data?
#All Scots areas, all agri where there isn't enough data in BRES...
# itl2.gvaperjob22 %>% filter(is.na(gva)) %>% View - all present

#Scotland, 2015/16/17, jobcount data missing
itl2.gvaperjob22 %>% filter(is.na(jobcount)) %>% View

#What we're after, paraphrased: “If SY kept its industry mix, but output per worker matched the UK average for each sector, what would its GVA be? If the same applied everywhere, how would SY GVA compare nationally?”

#So, stages to getting this:
#1. Find UK total GVA per FT worker for each section
#2. For each ITL2, multiply number of workers there by #1 to get "if this sector had output per worker of national average..."
#3. Sum up GVA for each region from that, then compare to GVA totals of originals

#UK GVA and jobcount sums for each section
UKlevel <- itl2.gvaperjob22 %>% 
  group_by(SIC07_description, year) %>% 
  summarise(
    gva = sum(gva, na.rm = T), jobcount = sum(jobcount, na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(gvaperjob_ukaverage = (gva/jobcount))#don't multiply up to pounds - just taking back down again below when summed
  # mutate(gvaperjob_ukaverage = (gva/jobcount)*1000000)


#Merge those av UK values back into the data...
itl2.gvaperjob22 <- itl2.gvaperjob22 %>% 
  left_join(
    UKlevel %>% select(SIC07_description,year,gvaperjob_ukaverage),
    by = c('SIC07_description','year')
  )

#Multiply up jobs by UK average for that sector
#Then sum that "if then" GVA per region
#Some NA results here where Scots regions have no agri values
#Will have to leave those out - need all sectors for figures to be correct
GVAifUKaverage <- itl2.gvaperjob22 %>% 
  mutate(gva_if_sectorUKav = jobcount * gvaperjob_ukaverage) %>% 
  group_by(year,Region_name) %>% 
  summarise(gva_ifsectorUKavproductivity = sum(gva_if_sectorUKav))

#Actual regional GVA sums
GVA_regions_actual <- itl2.gvaperjob22 %>% 
  group_by(year,Region_name) %>% 
  summarise(gva_actual = sum(gva))




#Get average UK GVA per job, to compare to
#Which is just this...
#Remarkably unchanged on average! Excepting COVID
UK_avGVAperjob_actual <- itl2.gvaperjob22 %>% 
  group_by(year) %>% 
  summarise(gva = sum(gva), jobcount = sum(jobcount, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(gvaperjob_uk = gva / jobcount)


#Also need "what average would be if GVA in all places was average for that sector"
#It's going to be in a different position
#(Which is possibly less useful than working out regional % differences between if... then and actual)

#Actually, let's do that last thing first
#Removing NA zones to avoid messing up percent diffs
actual_n_ifthen <- GVA_regions_actual %>% 
  left_join(
    GVAifUKaverage, by = c('year','Region_name')
  ) %>% 
  filter(!is.na(gva_ifsectorUKavproductivity)) %>% 
  mutate(
    # diff = gva_ifsectorUKavproductivity - gva_actual,#for eyeballing
    percentdiff = ((gva_ifsectorUKavproductivity - gva_actual)/gva_actual) * 100,
    SY = ifelse(Region_name == 'South Yorkshire', 'SY', 'other')
  )


#Plot that.
p <- ggplot(
  actual_n_ifthen,
  aes(x = year, y = percentdiff, colour = SY, alpha = SY, size = SY, group = Region_name)) +
  geom_point() +
  # geom_jitter(width = 0.25) +
  geom_line(size = 0.25) +
  geom_hline(yintercept = 0) +
  scale_colour_manual(values = c('black','red')) +
  scale_alpha_manual(values = c(0.25,1)) +
  scale_size_manual(values = c(0.5,3)) +
  # facet_wrap(~measure, scales = 'free') +
  guides(alpha = F, size = F) +
  ggtitle("If each ITL2 zone kept its industry mix\nbut output per worker matched the UK average for each SIC section\nwhat would its GVA be?")

p

ggplotly(p, tooltip = c('year','Region_name','percentdiff'))



#And then back to "what's average if we've adjusted all places for industry mix?"
#Which will need weighting by job numbers to do properly
#Need to merge job numbers back in...
# jobnumbers_summedperregion_n_year <- itl2.gvaperjob22 %>% 
#   group_by(year,Region_name) %>% 
#   summarise(jobcount_all = sum(jobcount, na.rm = T)) %>% 
#   ungroup()
# 
# actual_n_ifthen <- actual_n_ifthen %>% 
#   left_join(
#     jobnumbers_summedperregion_n_year, by = c('year','Region_name')
#   )

# UK_avGVAperjob_adjustedforindustrymix_weighted <- actual_n_ifthen %>% 
#   group_by(year) %>%
#   summarise(
#     gva_adjustedforindustrymix_weightedmean = weighted.mean(gva_ifsectorUKavproductivity, jobcount, na.rm=T)
#   ) %>% ungroup()
#   
# 
# 
# #Find % difference to those averages for adj industry mix...
# adjusted_industrymix_percentdiff <- actual_n_ifthen %>% 
#   left_join(
#     UK_avGVAperjob_adjustedforindustrymix_weighted, by = 'year'
#   ) %>% 
#   mutate(
#     percentofukav = (gva_ifsectorUKavproductivity/gva_adjustedforindustrymix_weightedmean) * 100
#   )


#FIND PERCENT DIFFEENCE OF PLACES FROM UK AVERAGE
#Using industry-adjusted values above

#Sum total UK jobs, put that under total “pretend GVA”, repeat for each region, find diff….
#Then sum yearly "if... then... industry mix" GVA and join to that
totalUKjobcount <- itl2.gvaperjob22 %>%
  group_by(year) %>%
  summarise(
    # jobcount_UK = sum(jobcount)
    jobcount_UK = sum(jobcount, na.rm = T)
  ) %>%
  ungroup() %>%
  left_join(
    actual_n_ifthen %>%
      select(year,gva_ifsectorUKavproductivity) %>%
      group_by(year) %>%
      summarise(gva_ifsectorUKavproductivity_TOTAL = sum(gva_ifsectorUKavproductivity)) %>%
      ungroup(),
    by = 'year'
  ) %>%
  mutate(av_UK_GVAPERFILLEDJOB_ifsectorUKproductivity = (gva_ifsectorUKavproductivity_TOTAL / jobcount_UK) * 1000000)


#GVA per job is too high there, but same bias should apply regionally
#So let's see how those numbers turn out...
#Get jobcounts and gva_ifthen for each region / year

#Do regions match here...? Tick (all from same source)
# table(itl2.gvaperjob22$Region_name %in% actual_n_ifthen$Region_name)

ifnthen_jobs <- actual_n_ifthen %>% 
  left_join(
    itl2.gvaperjob22 %>%
      group_by(Region_name,year) %>%
      summarise(
        jobcount_regions = sum(jobcount)
      ) %>%
      ungroup(),
    by = c('Region_name','year')
  ) %>% 
  mutate(
    gva_ifthen_perjobcount = (gva_ifsectorUKavproductivity / jobcount_regions) * 1000000
  ) %>% 
  left_join(
    totalUKjobcount %>% select(year,av_UK_GVAPERFILLEDJOB_ifsectorUKproductivity),
    by = 'year'
  ) %>% 
  mutate(
    gva_perjob_ifthen_percentOfUKav = (gva_ifthen_perjobcount / av_UK_GVAPERFILLEDJOB_ifsectorUKproductivity) * 100
  ) %>% 
  mutate(
    SY = ifelse(Region_name == 'South Yorkshire', 'SY', 'other')
  )

#Concentrating just on the most recent year, how do those numbers look?
#How crazy do those numbers look?
p <- ggplot(
  ifnthen_jobs,
  # allz_long.minusNAs %>% filter(!Region_name %in% c('Inner London - West', 'Inner London - East')),#filter out London outliers
  aes(x = year, y = gva_perjob_ifthen_percentOfUKav, colour = SY, alpha = SY, size = SY, group = Region_name)) +
  geom_point() +
  # geom_jitter(width = 0.25) +
  geom_line(size = 0.25) +
  geom_hline(yintercept = 100) +
  scale_colour_manual(values = c('black','red')) +
  scale_alpha_manual(values = c(0.25,1)) +
  scale_size_manual(values = c(0.5,3)) +
  # facet_wrap(~measure, scales = 'free') +
  guides(alpha = F, size = F)

p

ggplotly(p, tooltip = c('year','Region_name','gva_perjob_ifthen_percentOfUKav'))




#REPEAT THAT EXERCISE ABOVE JUST FOR 2018-22
#As scots agri job count data missing and warping early years
#Check assumptions, keep neat and clear

#This has the values for "region's GVA if SIC section output was UK average" values:
# actual_n_ifthen

#We need two things:
#Each of those regional "if/then"s divided by total job count numbers

#And the national average for the same:
#If industry mix in each place was industry average
#And we sum the total output then divide by job numbers
#What's the UK average?


#Let's get the UK jobcount value again for each year 2018 on first:
totalUKjobcount <- itl2.gvaperjob22 %>%
  filter(year > 2017) %>% 
  group_by(year) %>%
  summarise(
    jobcount_UK = sum(jobcount)
  )

#Total "if/then" industry-adjusted gva for each year
#Ah OK - total GVA constrains to the same value for if/then and actual
#Didn't think there was a reason it needed to...
totalUK_ifthenGVA <- actual_n_ifthen %>% 
  filter(year > 2017) %>% 
  group_by(year) %>%
  summarise(
    GVA_ifsectoravproductivity_UKlevel = sum(gva_ifsectorUKavproductivity),
    GVA_actual_UKlevel = sum(gva_actual)
  )


#Combine those, find GVA per job at UK level for the if/then
#Checking against the above, these are the same numbers, nothing changed
UKtotals_ifthen <- totalUKjobcount %>%
  left_join(totalUK_ifthenGVA, by = 'year') %>% 
  mutate(
    gvaperjob_UK_actual = (GVA_actual_UKlevel / jobcount_UK) * 1000000,
    gvaperjob_UK_ifthen = (GVA_ifsectoravproductivity_UKlevel / jobcount_UK) * 1000000
    )




#Now divide if/then GVA through by job numbers at regional level
#To then compare % gap to those UK averages
#Get regional job counts per year
regional_jobcounts <-itl2.gvaperjob22 %>%
  filter(year > 2017) %>% 
  group_by(year,Region_name) %>%
  summarise(jobcount_regional = sum(jobcount))

#Merge the if... then regional GVA into those job counts
#Find gva per job...
regional_jobcounts <- regional_jobcounts %>% 
  left_join(
    actual_n_ifthen %>% select(year,Region_name,gva_actual,gva_ifsectorUKavproductivity),by = c('year','Region_name')
  ) %>% 
  mutate(
    gvaperjob_regions_actual = (gva_actual / jobcount_regional) * 1000000,
    gvaperjob_regions_ifthen = (gva_ifsectorUKavproductivity / jobcount_regional) * 1000000
    )
  

#Merge in av GVA per job in the UK overall to find ppt difference to that average
#Find percent difference between regions and national figure while here...
regional_jobcounts <- regional_jobcounts %>% 
  left_join(
    UKtotals_ifthen, by = 'year'
  ) %>% 
  mutate(
    percentdiff_toUKav_actual = (gvaperjob_regions_actual / gvaperjob_UK_actual) * 100,
    percentdiff_toUKav_ifthen = (gvaperjob_regions_ifthen / gvaperjob_UK_ifthen) * 100
  ) %>% 
  mutate(
    SY = ifelse(Region_name == 'South Yorkshire', 'SY', 'other')
  )
  

#What's spread of actual vs if then?
compare <- regional_jobcounts %>% 
  pivot_longer(percentdiff_toUKav_actual:percentdiff_toUKav_ifthen, names_to = 'type', values_to = 'percentdiff') %>% 
  mutate(SY = ifelse(Region_name == 'South Yorkshire', 'SY','other')) %>% 
  filter(year == 2022)

#Store in diff var name for combining with flipped adjust for industry mix below
industrymix_matchUKavproductivity <- compare

p <- ggplot(
  compare, 
  aes(x = type, y = percentdiff, colour = SY, alpha = SY, size = SY, group = Region_name)) +
  geom_jitter(width = 0.02) +
  # geom_curve(curvature = 0.3) +
  geom_line(alpha = 0.1, size = 1) +
  scale_colour_manual(values = c('black','red')) +
  scale_alpha_manual(values = c(0.5,1)) +
  scale_size_manual(values = c(2,5)) +
  geom_hline(yintercept = 100) 

p

ggplotly(p, tooltip = c('year','Region_name','percentdiff'))



#Plot single values over time
#Yep, same number as before
p <- ggplot(
  regional_jobcounts,
  aes(x = year, y = percentdiff_toUKav_ifthen, colour = SY, alpha = SY, size = SY, group = Region_name)) +
  geom_point() +
  # geom_jitter(width = 0.25) +
  geom_line(size = 0.25) +
  geom_hline(yintercept = 100) +
  scale_colour_manual(values = c('black','red')) +
  scale_alpha_manual(values = c(0.25,1)) +
  scale_size_manual(values = c(0.5,3)) +
  # facet_wrap(~measure, scales = 'free') +
  guides(alpha = F, size = F)

p

ggplotly(p, tooltip = c('year','Region_name','percentdiff_toUKav'))



#Let’s just do a sanity check on one of them that jumps out as odd: west wales showing as “more productive than average if one assumes its sectors were UK average output”. Really? What sectors?

#Partial code from above to look at, has all the moving parts
#Make gva per job UK av easier to read...
GVAifUKaverage_partial <- itl2.gvaperjob22 %>% 
  mutate(
    gva_if_sectorUKav = jobcount * gvaperjob_ukaverage,
    gvaperjob_ukaverage = gvaperjob_ukaverage * 1000000
  ) 

#Look at west wales for one year
GVAifUKaverage_partial %>% filter(Region_name == 'West Wales', year == 2022) %>% 
  relocate(gva, .before = gva_if_sectorUKav) %>% 
  View




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# **REVERSED** "ADJUST FOR INDUSTRY MIX"----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Reversing what was done previously -
#Keep output per worker the same; adjust the industry mix to be the same as the national average.



#Reload and do from scratch
#Output per worker in each SIC section will STAY THE SAME
#We'll need regional job totals, so do that here
#Keep only years we have full data for
itl2.gvaperjob22 <- readRDS('data/itl2_gva_to2022_plusBRES_jobs_to2022.rds') %>% 
  filter(year > 2017) %>% 
  mutate(gvaperjob = (gva/jobcount)) %>% 
  group_by(year,Region_name) %>% 
  mutate(regional_jobcount_TOTAL = sum(jobcount))
  

#So then -
#Adjust the number of jobs for each of those sectors in each place
#So they match the proportion in those sectors nationally

#For which we need national sums of jobs and then proportions
#To then apply those to ITL2 job totals (maybe side thing of showing what would be different)
#This is sort-of reverse-applying location quotients

#Find proportions per year while here
UKjobsums_persector <- itl2.gvaperjob22 %>% 
  group_by(year,SIC07_description) %>% 
  summarise(jobtotal_perUKsector = sum(jobcount)) %>% 
  group_by(year) %>% 
  mutate(job_proportion_of_total_perUKsector = jobtotal_perUKsector/sum(jobtotal_perUKsector)) 
  
#Check we have 100% per year... tick
# UKjobsums_persector %>% 
#   group_by(year) %>% 
#   summarise(sum(job_percent_of_total_perUKsector))


#Merge in UK percentages for each sector to multiply regional job totals by
#Each SIC section in each place will get same percentage...
#Then multiply regional job count TOTAL by the national percent of that sector
#Which gives us "adjust the industry EMPLOYMENT mix to be the same as the national average"
itl2.gvaperjob22 <- itl2.gvaperjob22 %>% 
  left_join(
    UKjobsums_persector, by = c('year','SIC07_description')
  ) %>% 
  mutate(
    jobcount_if_sectorUKaverage_percent = regional_jobcount_TOTAL * job_proportion_of_total_perUKsector
  )

#Old and new jobcounts per place per year should be the same... tick
# itl2.gvaperjob22 %>% 
#   group_by(year,Region_name) %>% 
#   summarise(oldsum = sum(jobcount), newsum = sum(jobcount_if_sectorUKaverage_percent)) %>% View


#Then we just multiply GVA per job from that sector through...
# itl2.gvaperjob22 <- itl2.gvaperjob22 %>% 
#   mutate(
#     gvapersector_ifsectorUKaverage_percent = jobcount_if_sectorUKaverage_percent * gvaperjob
#   )

#Any inf values there are because there were no jobs
#So gva per job ends up being infinite (mismatch with job data, some of those have tiny amounts of GVA)
#But set those gvaperjobs that are inf to zero
itl2.gvaperjob22$gvaperjob[is.infinite(itl2.gvaperjob22$gvaperjob)] <- 0

itl2.gvaperjob22 <- itl2.gvaperjob22 %>%
  mutate(
    gvapersector_ifsectorUKaverage_percent = jobcount_if_sectorUKaverage_percent * gvaperjob
  )

#These should be approx the same...
# sum(itl2.gvaperjob22$gva)
# sum(itl2.gvaperjob22$gvapersector_ifsectorUKaverage_percent)







#Stage 1 done
#Now to find UK average gva per job for those values
#And regional GVA per job
#And percent differences...

#Taking code from above...
totalUKjobcount <- itl2.gvaperjob22 %>%
  group_by(year) %>%
  summarise(
    jobcount_UK = sum(jobcount)
    # jobcount_UK_moved = sum(jobcount_if_sectorUKaverage_percent)#double checking the same, don't need... tick
  )

#Total "if/then" industry-adjusted gva for each year
totalUK_ifthenGVA <- itl2.gvaperjob22 %>% 
  group_by(year) %>%
  summarise(
    GVA_ifsectors_wereUKmix_but_regional_productivitysame = sum(gvapersector_ifsectorUKaverage_percent),
    GVA_actual_UKlevel = sum(gva)
  )


#Combine those, find GVA per job at UK level for the if/then
#Checking against the above, these are the same numbers, nothing changed
UKtotals_ifthen <- totalUKjobcount %>%
  left_join(totalUK_ifthenGVA, by = 'year') %>% 
  mutate(
    gvaperjob_UK_actual = (GVA_actual_UKlevel / jobcount_UK) * 1000000,
    gvaperjob_UK_ifthen = (GVA_ifsectors_wereUKmix_but_regional_productivitysame / jobcount_UK) * 1000000
  )


#Check sanity
sanitycheck <- itl2.gvaperjob22 %>% filter(year == 2022, Region_name == "South Yorkshire") %>% 
  select(SIC07_description,jobcount,jobcount_if_sectorUKaverage_percent,gva,gvapersector_ifsectorUKaverage_percent) %>% 
  mutate(
    jobcount_percentchange = ((jobcount_if_sectorUKaverage_percent - jobcount)/jobcount) * 100,
    gva_abs_differnce = gvapersector_ifsectorUKaverage_percent - gva,
    gva_percentchange = ((gvapersector_ifsectorUKaverage_percent - gva)/gva) * 100
  ) %>% 
  relocate(jobcount_percentchange, .after = jobcount_if_sectorUKaverage_percent) %>% 
  arrange(-jobcount)

#save...
write_csv(sanitycheck,'data/SY_jobcount_gva_ifsectormix_sameasUKaverage.csv')



#Now divide if/then GVA through by job numbers at regional level
#To then compare % gap to those UK averages
#1.Get regional job counts per year
#2. get sum of GVA if then for each region too
#3. divide through to get "GVA per job in this region if industry mix was UK average but productivity in region is same"
regional_counts <-itl2.gvaperjob22 %>%
  group_by(year,Region_name) %>%
  summarise(
    jobcount_regional = sum(jobcount),
    gva_actual = sum(gva),
    gva_regional_ifindustrymixUKaverage = sum(gvapersector_ifsectorUKaverage_percent)
    ) %>% 
  mutate(
    gvaperjob_regional_actual = (gva_actual / jobcount_regional) * 1000000,
    gvaperjob_regional_ifindustrymixUKaverage = (gva_regional_ifindustrymixUKaverage / jobcount_regional) * 1000000
  )


#Merge in av GVA per job in the UK overall to find ppt difference to that average
#Find percent difference between regions and national figure while here...
regional_counts <- regional_counts %>% 
  left_join(
    UKtotals_ifthen, by = 'year'
  ) %>% 
  mutate(
    percentdiff_toUKav_actual = (gvaperjob_regional_actual / gvaperjob_UK_actual) * 100,
    percentdiff_toUKav_ifthen = (gvaperjob_regional_ifindustrymixUKaverage / gvaperjob_UK_ifthen) * 100
  ) %>% 
  mutate(
    SY = ifelse(Region_name == 'South Yorkshire', 'SY', 'other')
  )


#Check sanity again
# regional_counts %>% filter(year == 2022, Region_name == "South Yorkshire") %>% View



#What's spread of actual vs if then?
compare <- regional_counts %>% 
  pivot_longer(percentdiff_toUKav_actual:percentdiff_toUKav_ifthen, names_to = 'type', values_to = 'percentdiff') %>% 
  mutate(SY = ifelse(Region_name == 'South Yorkshire', 'SY','other')) %>% 
  filter(year == 2022)

p <- ggplot(
  compare, 
  aes(x = type, y = percentdiff, colour = SY, alpha = SY, size = SY, group = Region_name)) +
  geom_jitter(width = 0.02) +
  # geom_curve(curvature = 0.3) +
  geom_line(alpha = 0.1, size = 1) +
  scale_colour_manual(values = c('black','red')) +
  scale_alpha_manual(values = c(0.5,1)) +
  scale_size_manual(values = c(2,5)) +
  geom_hline(yintercept = 100) 

p

ggplotly(p, tooltip = c('year','Region_name','percentdiff'))



#Combine with other flipped version of "adjust for industry mix"

#Some names need changing...
# names(compare)[!names(compare) %in% names(industrymix_matchUKavproductivity)]
# names(industrymix_matchUKavproductivity)[!names(industrymix_matchUKavproductivity) %in% names(compare)]

#Actually, not many given we only need one or two for plotting...
both <- compare %>% 
  ungroup() %>% 
  select(Region_name,type,percentdiff,SY) %>% 
  mutate(plottype = "IF INDUSTRY MIX OF JOBS WAS SAME AS UK AVERAGE MIX (with GVA oer worker unchanged)...\n(Percent difference to UK average)") %>% 
  rbind(
    industrymix_matchUKavproductivity %>%
      ungroup() %>% 
      select(Region_name,type,percentdiff,SY) %>% 
      mutate(plottype = "IF GVA PER WORKER IN EACH SECTOR WAS UK AVERAGE OUTPUT...\n(Percent difference to UK average)")
  )



#Now just need to facet previous plot...
p <- ggplot(
  both, 
  aes(x = type, y = percentdiff, colour = SY, alpha = SY, size = SY, group = Region_name)) +
  geom_jitter(width = 0.02) +
  # geom_curve(curvature = 0.3) +
  geom_line(alpha = 0.1, size = 1) +
  scale_colour_manual(values = c('black','red')) +
  scale_alpha_manual(values = c(0.5,1)) +
  scale_size_manual(values = c(2,5)) +
  geom_hline(yintercept = 100) +
  # guides(#Not working - aim to remove lines, keep points in legend
  #   alpha = guide_legend(override.aes = list(linetype = 0)),
  #   size = guide_legend(override.aes = list(linetype = 0)),
  #   colour = guide_legend(override.aes = list(linetype = 0))
  #   ) +
  facet_wrap(~plottype)

p

ggplotly(p, tooltip = c('year','Region_name','percentdiff'))




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#CHECK ON OTHER APS/LFS VARIABLES----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Looking for confidence intervals for e.g. hours worked per week etc
#To see how it changes possible region productivity position

#This is just 'annual population survey', no filters
a <- nomis_get_metadata(id = "NM_17_1")

#List of variables.... nearly 4000
#402720769 = T01:22 (Aged 16-64 - All : All People )
varz <- nomis_get_metadata(id = "NM_17_1", concept = "CELL")

#Hours worked not in APS vars here

x <- nomis_data_info()




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#QUICK LOOK AT CIS IN APS DATA----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Reminder (from NOMIS downloaded version of data):
#"95% confidence interval (+/-)"

pop16emp <- pop16emp %>% 
  mutate(
    lowCI95 = Value - Confidence,
    highCI95 = Value + Confidence
    )










