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




##4. NOMISR FOR LATEST 16-64 POP NUMBERS----

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
  unique(pop16to64$Region_name)
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

#3. GVA over hours worked
#HOURS WORKED PER WEEK - needs multiplying up to match yearly GVA values
gva.uk.totals.mir.ratios <- gva.uk.totals.mir.ratios %>% 
  left_join(
    perhourworked.uk %>% select(year,hoursworked), by = 'year'
  ) %>% 
  mutate(
    UK_perhourworked_pounds_cp = (value / (hoursworked * 52)) * 1000000
  )

#4. per filled job
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

#3. GVA over hours worked
#HOURS WORKED PER WEEK - needs multiplying up to match yearly GVA values
gva.itl2.totals.mir.ratios <- gva.itl2.totals.mir.ratios %>% 
  left_join(
    perhourworked.itl2 %>% select(year,Region_name,hoursworked), by = c('year','Region_name')
  ) %>% 
  mutate(
    perhourworked_pounds_cp = (value / (hoursworked * 52)) * 1000000
  )

#4. per filled job
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
  select(Region_name,year,gvaperhead_pounds_cp,gvaper16to64_pounds_cp,perhourworked_pounds_cp,perfilledjob_pounds_cp) %>% 
  left_join(
    gva.uk.totals.mir.ratios %>% 
      select(year,UK_gvaperhead_pounds_cp,UK_gvaper16to64_pounds_cp,UK_perhourworked_pounds_cp,UK_perfilledjob_pounds_cp),
    by = 'year'
  )

#Save...
write_csv(allz,'data/GVA_measures_ITL2_aspercentofUKaverage1998_2022.csv')

#Then find ITL2 zone GVA values as percentage of UK average
allz <- allz %>% 
  mutate(
    gvaperhead_percentofUKav = (gvaperhead_pounds_cp/UK_gvaperhead_pounds_cp) * 100,
    gvaper16to64_percentofUKav = (gvaper16to64_pounds_cp/UK_gvaper16to64_pounds_cp) * 100,
    gvaperhourworked_percentofUKav = (perhourworked_pounds_cp/UK_perhourworked_pounds_cp) * 100,
    gvaperjobfilled_percentofUKav = (perfilledjob_pounds_cp/UK_perfilledjob_pounds_cp) * 100
  )


#first up for plot with view of data, facet the data type (make long)
allz_long <- allz %>% 
  select(
    Region_name,year,
    `GVA per head (percent of UK av)` = gvaperhead_percentofUKav,
    `GVA per 16 to 64 yr old (percent of UK av)` = gvaper16to64_percentofUKav,
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


p <- ggplot(
  allz_long[!is.na(allz_long$`percent of UK average`),],
  # allz_long[!is.na(allz_long$`percent of UK average`),] %>% filter(!Region_name %in% c('Inner London - West', 'Inner London - East')),#filter out London outliers
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







