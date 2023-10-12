#BRES explore
library(tidyverse)
library(zoo)
library(sf)
library(tmap)
library(plotly)
library(magick)
library(nomisr)
library(pryr)
library(ggrepel)
options(scipen = 99)

#Check manually set API env variable found when package loaded... tick
#See https://cran.r-project.org/web/packages/nomisr/nomisr.pdf
#getOption("nomisr.API.key")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#BRES open quick look at NOMIS manual download----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#BRES open sample, just 2021, Just SY. LSOA level, 5-digit classification
bres <- read_csv('local/data/BRESopen_SY_LSOA_industrypercent_employees_2021.csv', guess_max = 700)
#From the uneditted CSV:
# "-" These figures are missing. The level of rounding applied varies by estimate. Please see article for further information on how rounding is applied"
#https://www.nomisweb.co.uk/articles/1103.aspx

#Don't know why every odd row is missing values
#Remove
bres <- bres %>% 
  select(!contains("..."))

#Loads as char, guess_max doesn't help
bres <- bres %>% 
  mutate_at(2:ncol(bres),as.numeric)

#What's the percent? Within each zone.
#(Diffs due to rounding applied but clearly 100% per zone)
apply(bres[2:nrow(bres),2:ncol(bres)],2,sum)

#How are industry percents rounded?
#Getting just numbers
shef <- bres %>% select(contains('shef'))

allvals <- unlist(shef)
allvals <- allvals[allvals!=0]
allvals <- allvals[!is.na(allvals)]
allvals <- allvals[order(allvals)]

x <- sample(as.numeric(allvals),1000)
x[order(x)]
max(allvals)

#Just curious...
camels <- bres %>% filter(grepl('camelid',Industry))
apply(camels[,2:ncol(camels)],1,sum)#Doh! Alpaca farm too small to pick up on.




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Testing NOMIS R API package----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#https://cran.r-project.org/web/packages/nomisr/vignettes/introduction.html
#Not quick!
x <- nomis_data_info()
glimpse(x)

#Can reference specific datasets for metadata
#Code on NOMIS, including in URL e.g. for BRES open
#(Though shoudl be in uppercase)
#https://www.nomisweb.co.uk/datasets/newbres6pub


#Nope, not working for BRES top level. I may be missing what it wants exactly
y <- nomis_data_info("NEWBRES6PUB")
glimpse(y)

#Let's try the search option
#name needs to be without spaces
# a <- nomis_search(name = '*businessregister*', keywords = "employment")
a <- nomis_search(name = '*business*')

atts <- a %>% tidyr::unnest(components.attribute) %>% glimpse()
unique(atts$name.value)

#public/private? NM_190_1
a %>% filter(grepl('Business Register and Employment Survey public/private sector  : open access',name.value))

#So, what's the ID for BRES open?
#Noting I can probably only use the API for open data, not safeguarded BRES values...
atts %>% filter(name.value == "Business Register and Employment Survey : open access")


#BRES open ID
y <- nomis_data_info("NM_189_1")
glimpse(y)

#BRES open public private
pp <- nomis_data_info("NM_190_1")
glimpse(pp)




# y %>% tidyr::unnest(annotations.annotation) %>% glimpse()
# y %>% tidyr::unnest(components.attribute) %>% glimpse()
# y %>% tidyr::unnest(components.dimension) %>% glimpse()
# y %>% tidyr::unnest(components.primarymeasure.conceptref) %>% glimpse()
# 
# y %>% tidyr::unnest(annotations.annotation) %>% View
# y %>% tidyr::unnest(components.attribute) %>% View
# y %>% tidyr::unnest(components.dimension) %>% View
# y %>% tidyr::unnest(components.primarymeasure.conceptref) %>% View

#Dimension is the useful one there, we get the data dimensions e.g. for BRES open:
y %>% tidyr::unnest(components.dimension) %>% View
#Not sure yet which of those contains counts vs percentage (freq?)
#[1] "GEOGRAPHY"         "INDUSTRY"          "EMPLOYMENT_STATUS" "MEASURE"           "MEASURES"          "FREQ"    
y %>% tidyr::unnest(components.dimension) %>% select(conceptref) %>% pull()


#"nomis_overview() returns a tibble with a generalised overview of a given dataset"
#Just general metadata
q <- nomis_overview("NM_189_1")
q %>% tidyr::unnest(name) %>% View

#Note, via bug report, this one works for "INDUSTRY" (see below)
q <- nomis_overview("NM_187_1")
q %>% tidyr::unnest(name) %>% View

#This is BRES open "excluded units registered for PAYE only" (which doesn't sound like what we want!)
#Via nomis_search above
q <- nomis_overview("NM_172_1")
q %>% tidyr::unnest(name) %>% View


#"If provided with just a dataset ID, nomis_get_metadata() will return the concepts available for the given dataset."
#Note, this has time in, in addition to the dimensions above. Can use to pick time period for data, right?
a <- nomis_get_metadata(id = "NM_189_1")

#Returns values of the lowest indexed type available (so we're getting the countries here)
nomis_get_metadata(id = "NM_189_1", concept = "GEOGRAPHY")

#passing generic "type" string to type will give all the categories for that dimension
print(nomis_get_metadata(id = "NM_189_1", concept = "geography", type = "type"), n = 40)


#So let's do that for the others we need to pick from too.
#Only the first one is failing to work... API problem? If I can't specify that, that's going to make life hard
nomis_get_metadata(id = "NM_189_1", concept = "INDUSTRY")
nomis_get_metadata(id = "NM_189_1", concept = "EMPLOYMENT_STATUS")
nomis_get_metadata(id = "NM_189_1", concept = "MEASURE")
nomis_get_metadata(id = "NM_189_1", concept = "MEASURES")
nomis_get_metadata(id = "NM_189_1", concept = "TIME")

#Check the 'excluding PAYE' BRES too
#Aaaah these are earlier data 09-15
#They *really* should just explain these points on the website!
nomis_get_metadata(id = "NM_172_1", concept = "GEOGRAPHY")
nomis_get_metadata(id = "NM_172_1", concept = "INDUSTRY")
nomis_get_metadata(id = "NM_172_1", concept = "EMPLOYMENT_STATUS")
nomis_get_metadata(id = "NM_172_1", concept = "MEASURE")
nomis_get_metadata(id = "NM_172_1", concept = "MEASURES")
nomis_get_metadata(id = "NM_172_1", concept = "TIME")

print(nomis_get_metadata(id = "NM_172_1", concept = "geography", type = "type"), n = 40)


nomis_get_metadata("NM_190_1")
#Public private... TYPE438 plz
print(nomis_get_metadata("NM_190_1", concept = "GEOGRAPHY", type = 'type'),n=40)


#"Passing a specific type to the type parameter, in this case “TYPE460” for all post-2010 parliamentary constituencies, returns a tibble with geographic codes for those specific constituencies, which can be used to filter queries"
#So e.g. if I want LSOAs? Hmm, that's a lot of them isn't it?
#OK, so that just gave us a list of all the LSOA names and a possible ID if we want to filter down by that. This can be lookup for that if need be.
d <- nomis_get_metadata(id = "NM_189_1", concept = "geography", type = "TYPE298")

#Presumably we repeat the same pattern for other data types to narrow down?
#Not working for INDUSTRY, oh good.

#OK let's just try this 
#TYPE438 is NUTS2, which should include SY
#Result is 1/2 a gig
z <- nomis_get_data(id = "NM_189_1", time = "latest", geography = "TYPE438")
#Save locally, too big for github
saveRDS(z,'local/data/BRES_NUTS2_2021.rds')

#Do the NUTS2 codes match the ITL2 codes by any chance?
#Of course not
itl2 <- read_csv('data/sectors/Table 2b ITL2 UK chained volume measures in 2019 money value pounds million.csv')
table(itl2$`ITL region code` %in% z$GEOGRAPHY_CODE)

#Names? Mostly.
table(unique(itl2$`ITL region name`) %in% z$GEOGRAPHY_NAME)

#Check which don't match
#Same zones: two are spelling/punctuation, one is that BRES data is GB and ITL GVA data is UK so includes NI
unique(itl2$`ITL region name`)[!unique(itl2$`ITL region name`) %in% z$GEOGRAPHY_NAME]
unique(z$GEOGRAPHY_NAME)[!unique(z$GEOGRAPHY_NAME) %in% unique(itl2$`ITL region name`)]



unique(z$GEOGRAPHY_NAME)
unique(z$DATE)#Oh yes, we specified "latest
unique(z$INDUSTRY_NAME)
unique(z$INDUSTRY_TYPE)#All digit types!

#OK, need to know...
#Dammit, all zeroes! Oh well.
# camelids <- z %>% 
#   filter(INDUSTRY_TYPE =="SIC 2007 subclass (5 digit)", grepl("camelid", INDUSTRY_NAME), DATE == 2021)
# camelids <- z %>% 
#   filter(INDUSTRY_TYPE =="SIC 2007 subclass (5 digit)", grepl("camelid", INDUSTRY_NAME), DATE == 2021, EMPLOYMENT_STATUS_NAME == "Employees", MEASURE_NAME=="Count", MEASURES_NAME=='Value')

#How many in the three digit SIC here? 273
unique(z$INDUSTRY_NAME[z$INDUSTRY_TYPE=="SIC 2007 group (3 digit)"])
#88
unique(z$INDUSTRY_NAME[z$INDUSTRY_TYPE=="SIC 2007 division (2 digit)"])
#If joining with GVA / current price data, some harmonising to be done


#Test getting different years by just getting one geography (Tees Valley / Durham in this case, code in the GEOGRAPHY field in the download)
#Tick.
#chk <- nomis_get_data(id = "NM_189_1", time = "2020", geography = "1837105153")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#CHECKING SOME OTHER GEOGRAPHIES FOR EARLIER DATA----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#For earlier BRES data 09-15 ("excluding units registered for PAYE only")
#Data is missing for NUTS2 level. Including NUTS2 2010, I think - checked on the website, would like to check here also
print(nomis_get_metadata(id = "NM_172_1", concept = "geography", type = "type"), n = 40)

#I try 2013 below, no data
#Pretty sure NUTS 2010 is the same, let's check...
#Tick, all missing
z <- nomis_get_data(id = "NM_172_1", time = "2009", geography = "TYPE455")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~
#BRES OPEN BULK DOWNLOAD----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Throttling turned out not to be an issue, this time at least - these worked.

#2015 to 2021
z <- nomis_get_data(id = "NM_189_1", time = "2020", geography = "TYPE438")
saveRDS(z,'local/data/BRES_NUTS2_2020.rds')

#Let's just try it, see if it complains.
years = c(2015:2021)
# years = c(2015:2021)

download_all_BRESopen <- function(year){
  z <- nomis_get_data(id = "NM_189_1", time = as.character(year), geography = "TYPE438")
  saveRDS(z,paste0('local/data/BRES_NUTS2_',year,'.rds'))
}

lapply(years, function(x) download_all_BRESopen(x))


#REPEAT FOR 2009-15 BRES OPEN. ID = NM_172_1
#Has precise same structure as 15-21 (I think... Slightly different RDS size though, maybe something different)
#Slightly different method, but with minimal impact on numbers i.e. < 1%
#See: https://www.nomisweb.co.uk/articles/1062.aspx and https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/bulletins/businessregisterandemploymentsurveybresprovisionalresults/provisionalresults2016revisedresults2015#impact-of-additional-paye-units
years = c(2009:2014)
# years = c(2010:2014)

#Geography type is different as this one includes two type of NUTS2 to choose from
download_all_BRESopen_earlier <- function(year){
  z <- nomis_get_data(id = "NM_172_1", time = as.character(year), geography = "TYPE450")
  saveRDS(z,paste0('local/data/BRES_NUTS2_',year,'.rds'))
}

lapply(years, function(x) download_all_BRESopen_earlier(x))



#PUBLIC / PRIVATE BREAKDOWN RATHER THAN SIC CODES
#Get one year, check columns to keep
#Very quick, not huge, might as well get all
# chk <- nomis_get_data(id = "NM_190_1", time = "2021", geography = "TYPE438")

years = c(2015:2021)

download_all_BRES_PUBLICPRIVATE <- function(year){
  z <- nomis_get_data(id = "NM_190_1", time = as.character(year), geography = "TYPE438")
  saveRDS(z,paste0('local/data/BRES_PUBLICPRIVATE_NUTS2_',year,'.rds'))
}

lapply(years, function(x) download_all_BRES_PUBLICPRIVATE(x))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#BRES OPEN: ITL2 explore prior to combining----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Keeping 5-digit sectors to start with, see what we can see.

#Start with test of what we want by looking at one year
#Also check that pre-2015 data has similar enough structure to match up
x <- readRDS('local/data/BRES_NUTS2_2021.rds')

#Reminder:
unique(x$GEOGRAPHY_NAME)
unique(x$INDUSTRY_NAME)
unique(x$INDUSTRY_TYPE)#All digit types!

#Quick wranglinz:
#Check on employment count differences for the following:
# Employees: An employee is anyone aged 16 years or over that an organisation directly pays from its payroll(s), in return for carrying out a full-time or part-time job or being on a training scheme. It excludes voluntary workers, self-employed, working owners who are not paid via PAYE. 
# Full-time employees: those working more than 30 hours per week.
# Part-time employees: those working 30 hours or less per week.
# Employment includes employees plus the number of working owners. BRES therefore includes self-employed workers as long as they are registered for VAT or Pay-As-You-Earn (PAYE) schemes. Self employed people not registered for these, along with HM Forces and Government Supported trainees are excluded.
emp <- x %>% 
  filter(INDUSTRY_NAME=='Total',MEASURES_NAME=='Value', MEASURE_NAME=='Count') %>% 
  select(GEOGRAPHY_NAME,EMPLOYMENT_STATUS_NAME,OBS_VALUE)

#Full vs part time
ggplot(
  emp %>% filter(EMPLOYMENT_STATUS_NAME %in% c('Full-time employees','Part-time employees')),
  aes(y = fct_reorder(GEOGRAPHY_NAME,OBS_VALUE, .fun = max), fill = EMPLOYMENT_STATUS_NAME, x = OBS_VALUE)) +
  geom_bar(position = "fill", stat = "identity") +
  geom_vline(xintercept = 0.25)

#employees vs employment
ggplot(
  emp %>% filter(EMPLOYMENT_STATUS_NAME %in% c('Employees','Employment')),
  aes(y = fct_reorder(GEOGRAPHY_NAME,OBS_VALUE, .fun = max), fill = EMPLOYMENT_STATUS_NAME, x = OBS_VALUE)) +
  geom_bar(position = "dodge", stat = "identity")

#Can break down full vs part time and sector at some point
#Part time going to be in certain sectors more than others

#Initially, let's use them all though - "Employment"
#Keep:
#5 digit SIC
#Employment
y <- x %>% 
  filter(INDUSTRY_TYPE=='SIC 2007 subclass (5 digit)', EMPLOYMENT_STATUS_NAME=='Employment',INDUSTRY_NAME!="Total", MEASURES_NAME=='Value') %>% 
  select(DATE,GEOGRAPHY_NAME,GEOGRAPHY_CODE,INDUSTRY_NAME,INDUSTRY_CODE,MEASURE_NAME,OBS_VALUE)

#How many unique industry codes? 729
length(unique(y$INDUSTRY_NAME))

#Keeping only SY to test, what proportions across all 729 industries? Order
sy_percents <- y %>% filter(GEOGRAPHY_NAME=='South Yorkshire', MEASURE_NAME=='Industry percentage') %>% 
  mutate(
    INDUSTRY_NAME = factor(INDUSTRY_NAME),
    INDUSTRY_NAME = fct_reorder(INDUSTRY_NAME, OBS_VALUE)
    # SIZEGROUP = as.integer(cut_number(OBS_VALUE,5))# Not enough values... 
    )

#Need to break that down into chunks to plot
#But what numbers have we got?
unique(sy_percents$OBS_VALUE)
hist(sy_percents$OBS_VALUE)


#Let's just look at the actual counts too
sy_counts <- y %>% filter(GEOGRAPHY_NAME=='South Yorkshire', MEASURE_NAME=='Count') %>% 
  mutate(
    INDUSTRY_NAME = factor(INDUSTRY_NAME),
    INDUSTRY_NAME = fct_reorder(INDUSTRY_NAME, OBS_VALUE)
    )

#There are LOADS more values in the counts than the percents
#So let's use those to do LQs, sum them (will be ballpark for purposes of over-time LQs)
length(unique(sy_percents$OBS_VALUE))
length(unique(sy_counts$OBS_VALUE))


#So, we want this for each year. Count, not percent. We'll work our own percent out from the larger number of values
y <- x %>% 
  filter(INDUSTRY_TYPE=='SIC 2007 subclass (5 digit)', EMPLOYMENT_STATUS_NAME=='Employment',INDUSTRY_NAME!="Total", MEASURES_NAME=='Value', MEASURE_NAME=='Count') %>% 
  select(DATE,GEOGRAPHY_NAME,GEOGRAPHY_CODE,INDUSTRY_NAME,INDUSTRY_CODE,COUNT=OBS_VALUE)


#Out of interest, for the whole of GB...
gb <- y %>% 
  group_by(INDUSTRY_NAME) %>% 
  summarise(count = sum(COUNT))


#Hmm. I'd like to repeat that separately for part time and full time work I think.
ft <- x %>% 
  filter(INDUSTRY_TYPE=='SIC 2007 subclass (5 digit)', EMPLOYMENT_STATUS_NAME=='Full-time employees',INDUSTRY_NAME!="Total", MEASURES_NAME=='Value', MEASURE_NAME=='Count') %>% 
  select(DATE,GEOGRAPHY_NAME,GEOGRAPHY_CODE,INDUSTRY_NAME,INDUSTRY_CODE,COUNT=OBS_VALUE)

pt <- x %>% 
  filter(INDUSTRY_TYPE=='SIC 2007 subclass (5 digit)', EMPLOYMENT_STATUS_NAME=='Part-time employees',INDUSTRY_NAME!="Total", MEASURES_NAME=='Value', MEASURE_NAME=='Count') %>% 
  select(DATE,GEOGRAPHY_NAME,GEOGRAPHY_CODE,INDUSTRY_NAME,INDUSTRY_CODE,COUNT=OBS_VALUE)

gb.ft <- ft %>% 
  group_by(INDUSTRY_NAME) %>% 
  summarise(count = sum(COUNT)) %>% 
  mutate(percent = (count/sum(count)*100))

gb.pt <- pt %>% 
  group_by(INDUSTRY_NAME) %>% 
  summarise(count = sum(COUNT)) %>% 
  mutate(percent = (count/sum(count)*100))

#Temp employment agency activities being 2nd largest in suuuuper unhelpful!
#What sectors are they working in???
#I am presuming it's not the tmep support people themselves, the number's too high

#Note also:
#https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/bulletins/businessregisterandemploymentsurveybresprovisionalresults/provisionalresults2016revisedresults2015
#Issues with agri numbers at regional level - here, the numbers are zero. That's no good.
#Actually, I think agri numbers are in the DEFRA industry field...



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#BRES OPEN: COMBINE ITL2 DATA INTO ONE DF----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#NOTE: FOR NOW, FILTERING DOWN TO 2015-2021 AS 09-14 HAS NO DATA IN THE NUTS2 DOWNLOADS
#There's probably a way to aggregate the data through smaller geographies, will come back to that if needed
# loadallITL2_fulltime_and_reduce <- function(filename){
#   
#   readRDS(filename) %>% filter(INDUSTRY_TYPE=='SIC 2007 subclass (5 digit)', EMPLOYMENT_STATUS_NAME=='Full-time employees',
#                                INDUSTRY_NAME!="Total", MEASURES_NAME=='Value', MEASURE_NAME=='Count') %>% 
#     select(DATE,GEOGRAPHY_NAME,GEOGRAPHY_CODE,INDUSTRY_NAME,INDUSTRY_CODE,COUNT=OBS_VALUE)
#   
# }


loadallITL2_and_Reduce <- function(filename, industry, employment_status){
  
  readRDS(filename) %>% filter(INDUSTRY_TYPE==industry, EMPLOYMENT_STATUS_NAME==employment_status,
                               INDUSTRY_NAME!="Total", MEASURES_NAME=='Value', MEASURE_NAME=='Count') %>% 
    select(DATE,GEOGRAPHY_NAME,GEOGRAPHY_CODE,INDUSTRY_NAME,INDUSTRY_CODE,COUNT=OBS_VALUE)
  
}

#5 digit full timers
itl2 <- list.files(path = "local/data/", pattern = "BRES_NUTS2", full.names = T) %>% 
  # map(loadallITL2_fulltime_and_reduce) %>% 
  map(loadallITL2_and_Reduce, industry = 'SIC 2007 subclass (5 digit)', employment_status = 'Full-time employees') %>%#Note, haven't tested this generic version 
  bind_rows() %>% 
  filter(DATE > 2014)

#One single INDUSTY_NAME field has a non-UTF8 char that breaks some things
#Check for annoying chars and fix
#https://stackoverflow.com/a/17292126
itl2$INDUSTRY_NAME <- iconv(itl2$INDUSTRY_NAME, "UTF-8", "UTF-8",sub='')

#Save for use elsewhere
saveRDS(itl2, 'data/sectors/ITL2_fulltimeemployeecountandpercent5digitSIC_BRESopen15to21.rds')



#5 DIGIT all employees / employment
itl2 <- list.files(path = "local/data/", pattern = "BRES_NUTS2", full.names = T) %>% 
  # map(loadallITL2_fulltime_and_reduce) %>% 
  map(loadallITL2_and_Reduce, industry = 'SIC 2007 subclass (5 digit)', employment_status = 'Employment') %>%#Note, haven't tested this generic version 
  bind_rows() %>% 
  filter(DATE > 2014)

#Just in case...
#https://stackoverflow.com/a/17292126
itl2$INDUSTRY_NAME <- iconv(itl2$INDUSTRY_NAME, "UTF-8", "UTF-8",sub='')

#Save for use elsewhere
saveRDS(itl2, 'data/sectors/ITL2_Employment_countandpercent5digitSIC_BRESopen15to21.rds')



#REPEAT FOR 2 DIGIT full time
itl2 <- list.files(path = "local/data/", pattern = "BRES_NUTS2", full.names = T) %>% 
  # map(loadallITL2_fulltime_and_reduce) %>% 
  map(loadallITL2_and_Reduce, industry = 'SIC 2007 division (2 digit)', employment_status = 'Full-time employees') %>%#Note, haven't tested this generic version 
  bind_rows() %>% 
  filter(DATE > 2014)

#Just in case...
#https://stackoverflow.com/a/17292126
itl2$INDUSTRY_NAME <- iconv(itl2$INDUSTRY_NAME, "UTF-8", "UTF-8",sub='')

#Save for use elsewhere
saveRDS(itl2, 'data/sectors/ITL2_fulltimeemployeecountandpercent2digitSIC_BRESopen15to21.rds')



#REPEAT FOR 2 DIGIT all employees / employment
itl2 <- list.files(path = "local/data/", pattern = "BRES_NUTS2", full.names = T) %>% 
  # map(loadallITL2_fulltime_and_reduce) %>% 
  map(loadallITL2_and_Reduce, industry = 'SIC 2007 division (2 digit)', employment_status = 'Employment') %>%#Note, haven't tested this generic version 
  bind_rows() %>% 
  filter(DATE > 2014)

#Just in case...
#https://stackoverflow.com/a/17292126
itl2$INDUSTRY_NAME <- iconv(itl2$INDUSTRY_NAME, "UTF-8", "UTF-8",sub='')

#Save for use elsewhere
saveRDS(itl2, 'data/sectors/ITL2_Employment_countandpercent2digitSIC_BRESopen15to21.rds')




#Repeat for public/private, different function
loadallITL2_publicprivate_and_Reduce <- function(filename, industry, employment_status){
  
  readRDS(filename) %>% filter(INDUSTRY_TYPE==industry, EMPLOYMENT_STATUS_NAME==employment_status,
                               INDUSTRY_NAME!="Total", MEASURES_NAME=='Value', MEASURE_NAME=='Count') %>% 
    select(DATE,GEOGRAPHY_NAME,GEOGRAPHY_CODE,INDUSTRY_NAME,INDUSTRY_CODE,COUNT=OBS_VALUE)
  
}

itl2 <- list.files(path = "local/data/", pattern = "BRES_PUBLICPRIVATE_NUTS2", full.names = T) %>% 
  map(loadallITL2_publicprivate_and_Reduce, industry = 'SIC 2007 division (2 digit)', employment_status = 'Employment') %>%#Note, haven't tested this generic version 
  bind_rows() %>% 
  filter(DATE > 2014)

#Just in case...
#https://stackoverflow.com/a/17292126
itl2$INDUSTRY_NAME <- iconv(itl2$INDUSTRY_NAME, "UTF-8", "UTF-8",sub='')

#Save for use elsewhere
saveRDS(itl2, 'data/sectors/ITL2_Employment_countandpercent2digitSIC_BRESopen15to21.rds')




#~~~~~~~~~~~~~~~~~~~~~~~~~~~
#ITL2 LOCATION QUOTIENTS PLUS OTHER ADDITIONS TO THE DF----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~

itl2 <- readRDS('data/sectors/ITL2_fulltimeemployeecountandpercent5digitSIC_BRESopen15to21.rds')

#This is currently FULL TIME ONLY, remember. Will need to repeat for PT, "Employment"
#https://www.economicmodeling.com/wp-content/uploads/2007/10/emsi_understandinglq.pdf
#What do we need for yearly location quotients

#Some quick checks. 
table(is.na(itl2$COUNT))

#15 NAs. Where they?
itl2 %>% filter(is.na(COUNT)) %>% View

#Agri values in some bits of North Scotland - setting to zero, won't affect overview
itl2$COUNT[is.na(itl2$COUNT)] <- 0


#CALCULATE LQ
#1. Find regional proportion of sector: sector x as proportion of all sectors in that region
#2. Find proportion of same sector for the UK as a whole (sum sector total, sum entire UK total, find proportion)
#3. LQ is relative proportion - 1 / 2. 
#4. < 1 = region has less concentration than nationally. > 1 = concentration is higher than nationally.

#For regional proportion calc, Add values for calc steps into the same DF
itl2.lq <- itl2 %>%
  group_by(GEOGRAPHY_NAME, DATE) %>% 
  mutate(
    region_totalsize = sum(COUNT, na.rm = T),#a. Current employment per region per year, for regional denominator
    sector_regional_proportion = COUNT / region_totalsize#b. regional sector proportion.
  ) %>% 
  group_by(DATE, INDUSTRY_NAME) %>% 
  mutate(
    uk_sectorsize = sum(COUNT, na.rm = T),#c. Summed current prices for EACH SECTOR, UK-wide
  ) %>% 
  group_by(DATE) %>% 
  mutate(
    uk_totalsize = sum(COUNT, na.rm = T),#d. Summed current prices for WHOLE UK per year, for UK denominator
    sector_uk_proportion = uk_sectorsize / uk_totalsize#e. UK-level sector proportion
  ) %>% 
  mutate(
    LQ = sector_regional_proportion / sector_uk_proportion#f. Location quotient!
  )



#Just check how many GB total sector sizes are all zero employees in the summed ITL2 data
#Compared to GB-level data
chk <- itl2.lq %>% filter(
  uk_sectorsize == 0, DATE == 2021
)

unique(chk$INDUSTRY_NAME)

gb.chk <- readRDS('data/sectors/gb_fulltimeemployeecountandpercent5digitSIC_BRESopen09to21.rds') %>% 
  filter(COUNT == 0, DATE == 2021)

#Yep, all the same ones
table(unique(gb.chk$INDUSTRY_NAME) %in% unique(chk$INDUSTRY_NAME))


#Argument for noting those and removing from the analysis given that LQ of 0 has no meaning.
#Actually let's just do that now and note for later
itl2.lq <- itl2.lq %>% 
  filter(!INDUSTRY_NAME %in% chk$INDUSTRY_NAME)

#Add log column
itl2.lq <- itl2.lq %>% 
  mutate(LQ_log = log(LQ)) %>% 
  mutate(LQ_log = ifelse(is.infinite(LQ_log),NA,LQ_log))#NA removed from plots, but also removable from other calcs


#Add in 2 and 3 digit SIC lookups
#Other ways to cluster 5 digit possibly available based on LQ
#(Lookup made below)
SIClookup <- read_csv('data/SIClookup.csv')

itl2.lq <- itl2.lq %>% 
  left_join(SIClookup %>% select(-SIC_5DIGIT_NAME), by = c('INDUSTRY_CODE' = 'SIC_5DIGIT_CODE'))


#SAVE!
saveRDS(itl2.lq,'data/BRESopen_fulltimeemployees_ITL2_plusSIClookup.rds')


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#TEST LOCATION QUOTIENT CALC----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Make 100% sure it's doing what I want
#First, stare at one year
yr <- itl2.lq %>% filter(DATE==2021)
yr <- itl2.lq %>% filter(DATE==2021, grepl('cutlery',INDUSTRY_NAME))
yr <- itl2.lq %>% filter(DATE==2021, grepl('basic iron',INDUSTRY_NAME))

#Check concentration and specialisation interpretation
#Viewed in one place across sectors = concentration (denominator stays the same in that interpretation)
placetolook <- itl2.lq %>% filter(DATE==2021, grepl('South Yorkshire',GEOGRAPHY_NAME))

p <- ggplot(placetolook %>% filter(INDUSTRY_NAME %in% placetolook$INDUSTRY_NAME[placetolook$LQ > 2]), 
            aes(x = LQ, y = fct_reorder(INDUSTRY_NAME,LQ), fill = INDUSTRY_NAME)) +
  geom_bar(stat = 'identity') +
  guides(fill = F) +
  theme(axis.text.y=element_blank(), #remove x axis labels
        axis.ticks.y=element_blank())

ggplotly(p, tooltip = c("INDUSTRY_NAME" ))



#Then, geographical specialisation.
#Denom here is "how the proportion of industry is distributed across the UK"
#So if we look at one industry, that stays the same and we see proportion across the UK
industrytolook <- itl2.lq %>% filter(DATE==2021, grepl('cutlery',INDUSTRY_NAME))
industrytolook <- itl2.lq %>% filter(DATE==2021, grepl('basic iron',INDUSTRY_NAME))

p <- ggplot(industrytolook %>% filter(INDUSTRY_NAME %in% industrytolook$INDUSTRY_NAME[industrytolook$LQ > 1]), 
            aes(x = LQ, y = fct_reorder(GEOGRAPHY_NAME,LQ), fill = GEOGRAPHY_NAME)) +
  geom_bar(stat = 'identity') +
  guides(fill = F) +
  theme(axis.text.y=element_blank(), #remove x axis labels
        axis.ticks.y=element_blank())

ggplotly(p, tooltip = c("GEOGRAPHY_NAME" ))



#And some other tests while I'm here
#5 digit SIC counts appear to let you get MUCH more accurate job counts than the 2-digit by themselves
#If these numbers are correct and not random estimates - not sure about this
#E.g. the basic metals total is 5000 exactly from 2-digit, for SY in 2021
#From the 5 digit we can get:
itl2.lq %>% 
  filter(GEOGRAPHY_NAME=='South Yorkshire', grepl('basic metal', SIC_2DIGIT_NAME)) %>% 
  summarise(jobtot = sum(COUNT))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~
#BRES LQ: QUICK CHANGE OVER TIME LOOK----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Reload if nec
itl2.lq <- readRDS('data/BRESopen_fulltimeemployees_ITL2_plusSIClookup.rds')


#Test change over time calcs on a single place
sy <- itl2.lq %>% filter(GEOGRAPHY_NAME=='South Yorkshire')

diffchange <- sy %>%
  group_by(INDUSTRY_NAME) %>%
  mutate(logdiff = LQ_log - lag(LQ_log)) %>%
  summarise(difftotal = sum(logdiff,na.rm = T)) %>% 
  ungroup() %>% 
  mutate(group = as.integer(cut_number(difftotal,8)))

#8 is max cust number...??

table(diffchange$difftotal)

#Is the issue all the zeroes?
#Yup.
diffchange <- sy %>%
  group_by(INDUSTRY_NAME) %>%
  mutate(logdiff = LQ_log - lag(LQ_log)) %>%
  summarise(difftotal = sum(logdiff,na.rm = T)) %>% 
  ungroup() %>% 
  filter(difftotal != 0) %>% 
  mutate(group = as.integer(cut_number(difftotal,21)))


#There's a tiny chance sectors with zero did sum to that over time
#But it's more likely those ones were zero for all values and so not much use
#Prob the agri ones, right?
chk <- sy %>%
  group_by(INDUSTRY_NAME) %>%
  mutate(logdiff = LQ_log - lag(LQ_log)) %>%
  summarise(difftotal = sum(logdiff,na.rm = T)) %>% 
  filter(difftotal==0)

chk.sy <- sy %>% 
  filter(
    INDUSTRY_NAME %in% chk$INDUSTRY_NAME
  )

#Not all are zero throughout
sy %>% 
  filter(grepl('Mining of hard coal from deep',INDUSTRY_NAME)) %>% View

#Pick one with some small numbers in and work out how diffsum ends up at zero despite values there
checkdiff <- sy %>% 
  filter(INDUSTRY_NAME=='17120 : Manufacture of paper and paperboard')

#OK - I think it's that diff between time points is going to be returning zeroes
#Because no two adjadcent timepoints have differing values
#There may still be values in there
#But these are all for sectors where largely the work pop is zero anyway
#So I don't think filtering out is an issue

#E.g. these are the ones with the two highest values
sy %>% filter(grepl('Mining of hard coal from deep',INDUSTRY_NAME)) %>% View
sy %>% filter(grepl('Mining of hard coal from open',INDUSTRY_NAME)) %>% View
sy %>% filter(grepl('credit bureaus',INDUSTRY_NAME)) %>% View



#Note, LQs are all relative, so change over time here is relative to GB as a whole
#But let's see how things change over time for a few places
#Cf. change over time for GB as a whole, below

#Break down by largest change over time per sector per place per year
#Can order by place for plotting before comparing
#Leave out DATE from group - we're summing across all dates

#Get a "change over time" set of groups, 21 so there's a middle grouping
diffchange <- itl2.lq %>%
  group_by(INDUSTRY_NAME,GEOGRAPHY_NAME) %>%
  arrange(DATE) %>% 
  mutate(logdiff = LQ_log - lag(LQ_log)) %>%
  summarise(difftotal = sum(logdiff,na.rm = T)) %>% 
  filter(difftotal != 0) %>% #See above
  group_by(GEOGRAPHY_NAME) %>% 
  mutate(group = as.integer(cut_number(difftotal,31))) %>% 
  ungroup()




#TEST WITH SD + OLS APPROACH
#Use SD to get overall volatility.
#Use OLS to get polarity of change.
#Combine... does it do any better at categorising into change groups?

#Create a function that fits lm and returns slope
get_slope <- function(data) {
  model <- lm(LQ_log ~ DATE, data = data, na.action = na.omit)
  coef(model)[2]
}

#Make it a safe function using purrr::possibly
safe_get_slope <- purrr::possibly(get_slope, otherwise = 0)

l <- itl2.lq %>%
  group_by(GEOGRAPHY_NAME, INDUSTRY_NAME) %>%
  nest() %>%
  mutate(slope = map_dbl(data, safe_get_slope)) %>%
  select(-data) %>% 
  mutate(slope = ifelse(is.na(slope),0,slope))


#Find sds also
sds <- itl2.lq %>%
  group_by(GEOGRAPHY_NAME,INDUSTRY_NAME) %>%
  summarise(sd = sd(LQ_log, na.rm=T)) %>%
  ungroup() %>% 
  mutate(sd = ifelse(is.na(sd),0,sd))


#Next - use slope just to set polarity of SD, so can break down in viz into growing/shrinking
#Call diffchange to keep changes below to a minimum
diffchange <- l %>% 
  left_join(
    sds,
    by = c('INDUSTRY_NAME','GEOGRAPHY_NAME')) %>% 
  mutate(sd_w_polarity = ifelse(slope >= 0, sd, sd * -1)) %>% 
  filter(sd_w_polarity != 0) %>% 
  group_by(GEOGRAPHY_NAME) %>% 
  mutate(group = as.integer(cut_number(sd_w_polarity,31))) %>% 
  ungroup()



#Or just using OLS directly...
diffchange <- l %>% 
  filter(slope!=0) %>% 
  group_by(GEOGRAPHY_NAME) %>% 
  mutate(group = as.integer(cut_number(slope,31))) %>% 
  ungroup() %>% 
  rename(difftotal = slope)




#Check single place built in
#For viewing to check
# diffchange <- itl2.lq %>%
#   filter(GEOGRAPHY_NAME==place) %>% 
#   group_by(INDUSTRY_NAME) %>%
#   arrange(DATE) %>% 
#   mutate(logdiff = LQ_log - lag(LQ_log)) %>%
#   mutate(difftotal = sum(logdiff,na.rm = T))


# diffchange <- itl2.lq %>%
#   filter(GEOGRAPHY_NAME==place) %>% 
#   group_by(INDUSTRY_NAME) %>%
#   arrange(DATE) %>% 
#   mutate(logdiff = LQ_log - lag(LQ_log)) %>%
#   summarise(difftotal = sum(logdiff,na.rm = T)) %>% 
#   filter(difftotal != 0) %>% 
#   mutate(group = as.integer(cut_number(difftotal,21))) %>% 
#   ungroup()

#pick a place
place = 'South Yorkshire'
place = 'Greater Manchester'

#climbers
industries = diffchange %>% filter(GEOGRAPHY_NAME == place, group %in% c(31)) %>% select(INDUSTRY_NAME) %>% pull()
#droppers
industries = diffchange %>% filter(GEOGRAPHY_NAME == place, group %in% c(1)) %>% select(INDUSTRY_NAME) %>% pull()
#middle
industries = diffchange %>% filter(GEOGRAPHY_NAME == place, group %in% c(16)) %>% select(INDUSTRY_NAME) %>% pull()

x <- itl2.lq %>% filter(GEOGRAPHY_NAME == place, INDUSTRY_NAME %in% industries) 



y <- x %>% 
  group_by(INDUSTRY_NAME) %>% 
  arrange(DATE) %>% 
  mutate(
    LQ = ifelse(LQ == 0, NA, LQ),#Set to NA so plotly won't show
    movingav = rollapply(LQ,3,mean,align='right',fill=NA)
    # LQ_movingav = rollapply(LQ,3,mean,align='right',fill=NA),0)#Have a count moving average too, so it matches the percent (so count orders are correct vertically)
  )

plot_ly(data = y, x = ~DATE, y = ~movingav, color = ~INDUSTRY_NAME, 
        text = ~paste("Sector:", INDUSTRY_NAME, "\nPEOPLE: ",COUNT),  # Add this line for hover text
        hoverinfo = 'text',
        type = 'scatter', mode = 'lines+markers', line = list(shape = 'linear')) %>%
  layout(title = "Yearly percents by SIC", 
         xaxis = list(title = "Year"), 
         yaxis = list(title = "Value", type='log'),
         # yaxis = list(title = "Value"),
         showlegend = F)

plot_ly(data = x %>% ungroup() %>% filter(LQ!=0), x = ~DATE, y = ~LQ, color = ~INDUSTRY_NAME, 
        text = ~paste("Sector:", INDUSTRY_NAME, "\nPEOPLE: ",COUNT),  # Add this line for hover text
        hoverinfo = 'text',
        type = 'scatter', mode = 'lines+markers', line = list(shape = 'linear')) %>%
  layout(title = "Yearly percents by SIC", 
         # xaxis = list(title = "Year"), 
         yaxis = list(title = "Value", type='log'),
         # yaxis = list(title = "Value"),
         showlegend = F)


#Does ggplot do any better with that last one? Hmm it's just zero values... 
ggplot(x %>% filter(LQ!=0),aes(x = DATE, y = LQ, colour = INDUSTRY_NAME)) +
  geom_point() +
  geom_line() +
  scale_y_log10()


#Just looking at some of those...
x %>% filter(grepl('Farm animal boarding', INDUSTRY_NAME)) %>% View



#~~~~~~~~~~~~~~~~~~~~~~~~~~~
#BRES LQ: LOOKING AT 5-DIGIT SECTOR BY 2-DIGIT SECTOR----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~



#View for 2021. Can just flag one we want to overlay?
#Something odd with water and air transport, remove until work out what
place = 'South Yorkshire'

# x <- itl2.lq %>% filter(DATE == 2015) %>% mutate(flaggedplace = GEOGRAPHY_NAME==place)
# x <- itl2.lq %>% filter(DATE == 2021) %>% mutate(flaggedplace = GEOGRAPHY_NAME==place)

#Attempt to get drawing order working below
x <- itl2.lq %>% filter(DATE == 2021) %>% mutate(flaggedplace = ifelse(GEOGRAPHY_NAME==place, 'A', 'B'))

#Ordering by the flagged place, bit awkward
x$INDUSTRY_NAME <- factor(x$INDUSTRY_NAME)
x$INDUSTRY_NAME <- fct_relevel(
  x$INDUSTRY_NAME, 
  unique(as.character(x$INDUSTRY_NAME))[order(x %>% filter(GEOGRAPHY_NAME==place) %>%ungroup() %>% select(LQ) %>% pull(),decreasing = T)]
)

#Actually, keep that order and use for animation below
# ordertouse <- unique(as.character(x$`SIC07 description`))[order(x %>% filter(`ITL region name`==place) %>%ungroup() %>% select(LQplusone_log) %>% pull(),decreasing = T)]


#Filter down a bit more to try and see
#Going to need abbreviated industry names too

#List of sectors where LQ = certain value for the named place
# sectors.to.view <- x %>% ungroup() %>% filter(flaggedplace == T, LQplusone_log == 0) %>% select(INDUSTRY_NAME) %>% pull() %>% as.character()
sectors.to.view <- x %>% ungroup() %>% filter(GEOGRAPHY_NAME == place, LQ > 2) %>% select(INDUSTRY_NAME) %>% pull() %>% as.character()


#Group 2 digit SICs by average 5-digit LQ
#For each place, in each year
x <- x %>% 
  group_by(SIC_2DIGIT_NAME, DATE, GEOGRAPHY_NAME) %>% 
  mutate(
    av_LQ_2digitSIC = mean(LQ)
  ) %>% 
  group_by(DATE, GEOGRAPHY_NAME) %>% 
mutate(
    RANK_av_LQ_2digitSIC = rank(av_LQ_2digitSIC)
    ) 
# %>% 
#   mutate(flaggedplace = factor(flaggedplace, levels = c(T,F)))

#Quite small number for each place / year
unique(x$RANK_av_LQ_2digitSIC)[order(unique(x$RANK_av_LQ_2digitSIC))]

#Really though?
unique(x %>% filter(GEOGRAPHY_NAME==place,DATE==2021) %>% select(av_LQ_2digitSIC) %>% arrange() %>% pull())


ggplot(
  x,
  # x %>% filter(INDUSTRY_NAME %in% sectors.to.view),
  # x,
  aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace)
  # aes(y = INDUSTRY_NAME, x = LQ_log, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace)
) +
  geom_point() +
  scale_x_continuous(trans = "log10") +
  scale_size_manual(values = c(2,1)) +
  scale_alpha_manual(values = c(1,0.1)) +
  scale_shape_manual(values = c(17,16)) +
  scale_colour_manual(values = c('red','black')) +
  geom_vline(xintercept = 1, colour = 'blue') +
  facet_wrap(~SIC_2DIGIT_NAME,ncol = 8, scales = 'free_y') +
  theme(axis.text.y=element_blank(), #remove x axis labels
        axis.ticks.y=element_blank())



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#ADD IN INDICATION OF 5-DIGIT SIC TREND ONTO LQ PLOTS----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#LOOK AT SOME OF THOSE CLOSER UP
#Do manually to get order right? How annoying it doesn't work automatically
sector2digit_grepl <- 'Manufacture of fabricated metal products, except machinery and equipment'
sector2digit_grepl <- 'basic metals'
sector2digit_grepl <- 'non-metallic'
sector2digit_grepl <- 'Manufacture of machinery and equipment n.e.c.'
sector2digit_grepl <- 'Wholesale trade'
sector2digit_grepl <- 'food products'
sector2digit_grepl <- 'wearing apparel'
sector2digit_grepl <- 'recorded media'
sector2digit_grepl <- 'Manufacture of chemicals'
sector2digit_grepl <- 'rubber and plastic'
sector2digit_grepl <- 'Manufacture of electrical equipment'
sector2digit_grepl <- 'Repair of computers'

#Check is single sector
x %>% filter(grepl(sector2digit_grepl,SIC_2DIGIT_NAME)) %>% select(SIC_2DIGIT_NAME) %>% pull() %>% unique()


#Use diff change to mark change in LQ over the whole period, indicate growth

#Pick a diff change from above, don't use this one. Testing OLS.

# diffchange <- itl2.lq %>%
#   group_by(INDUSTRY_NAME,GEOGRAPHY_NAME) %>%
#   arrange(DATE) %>% 
#   mutate(logdiff = LQ_log - lag(LQ_log)) %>%
#   summarise(difftotal = sum(logdiff,na.rm = T)) %>% 
#   filter(difftotal != 0) %>% #See above
#   group_by(GEOGRAPHY_NAME) %>% 
#   mutate(group = as.integer(cut_number(difftotal,31))) %>% 
#   ungroup()


x <- x %>% 
  left_join(
    diffchange %>% select(-group),
    by = c('INDUSTRY_NAME','GEOGRAPHY_NAME')
  )



#Get min max values over time as well, to add as bars so range of sector is easy to see
minmaxes <- itl2.lq %>% 
  group_by(INDUSTRY_NAME,GEOGRAPHY_NAME) %>% 
  summarise(
    minn = min(LQ),
    maxx = max(LQ)
  )

x <- x %>% 
  left_join(
    minmaxes,
    by = c('INDUSTRY_NAME','GEOGRAPHY_NAME')
  )



#Redo factor, gets lost in join
x$INDUSTRY_NAME <- factor(x$INDUSTRY_NAME)
x$INDUSTRY_NAME <- fct_relevel(
  x$INDUSTRY_NAME, 
  unique(as.character(x$INDUSTRY_NAME))[order(x %>% filter(GEOGRAPHY_NAME==place) %>%ungroup() %>% select(LQ) %>% pull(),decreasing = T)]
)


#reduce sector name lengths
x <- x %>% mutate(INDUSTRY_NAME_REDUCED = substr(INDUSTRY_NAME,1,70))
x$INDUSTRY_NAME_REDUCED <- factor(x$INDUSTRY_NAME_REDUCED)
x$INDUSTRY_NAME_REDUCED <- fct_relevel(
  x$INDUSTRY_NAME_REDUCED, 
  unique(as.character(x$INDUSTRY_NAME_REDUCED))[order(x %>% filter(GEOGRAPHY_NAME==place) %>%ungroup() %>% select(LQ) %>% pull(),decreasing = T)]
)


#Save that 2021 x for later!
saveRDS(x, 'local/x_2021.rds')
x <- readRDS('local/x_2021.rds')

#NOTE: REGIONAL PROPORTION OF JOBS WILL GO UP LEFT TO RIGHT, LARGEST ON RIGHT. DON'T NEED TO PLOT THAT, IT'S LINEAR FOR SPECIFIC SECTORS BAKED INTO LQ MATHS
ggplot() +
  geom_point(
    data = x %>% filter(grepl(sector2digit_grepl,SIC_2DIGIT_NAME), flaggedplace =='B'), 
    # aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace, size = COUNT)) +
    aes(y = INDUSTRY_NAME, x = LQ, size = COUNT),
    alpha = 0.2
  ) +
  geom_point(
    data = x %>% filter(grepl(sector2digit_grepl,SIC_2DIGIT_NAME), flaggedplace =='A'), 
    # aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace, size = COUNT)) +
    aes(y = INDUSTRY_NAME, x = LQ, size = COUNT *1.5),
    shape = 16, colour = 'white'
  ) +
  geom_point(
    data = x %>% filter(grepl(sector2digit_grepl,SIC_2DIGIT_NAME), flaggedplace =='A'), 
    # aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace, size = COUNT)) +
    aes(y = INDUSTRY_NAME, x = LQ, size = COUNT),
    shape = 16, colour = 'red'
  ) +
  scale_size_continuous(range = c(1,18)) +
  scale_x_continuous(trans = "log10") +
  geom_vline(xintercept = 1, colour = 'blue') 
  # guides(size = F)


#Mark av change over time to indicate growth?
ggplot() +
  geom_point(
    data = x %>% filter(grepl(sector2digit_grepl,SIC_2DIGIT_NAME), flaggedplace =='B', difftotal > 0), 
    # aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace, size = COUNT)) +
    aes(y = INDUSTRY_NAME, x = LQ, size = difftotal),
    alpha = 0.2,
    shape = 24,
    fill = 'black'
  ) +
  geom_point(
    data = x %>% filter(grepl(sector2digit_grepl,SIC_2DIGIT_NAME), flaggedplace =='B', difftotal < 0), 
    # aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace, size = COUNT)) +
    aes(y = INDUSTRY_NAME, x = LQ, size = difftotal * -1),
    alpha = 0.2,
    shape = 25,
    fill = 'black'
  ) +
  geom_point(
    data = x %>% filter(grepl(sector2digit_grepl,SIC_2DIGIT_NAME), flaggedplace =='A', difftotal > 0), 
    # aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace, size = COUNT)) +
    aes(y = INDUSTRY_NAME, x = LQ, size = difftotal *1.5),
    shape = 24,
    fill = 'white',
    colour = 'white'
  ) +
  geom_point(
    data = x %>% filter(grepl(sector2digit_grepl,SIC_2DIGIT_NAME), flaggedplace =='A', difftotal < 0), 
    # aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace, size = COUNT)) +
    aes(y = INDUSTRY_NAME, x = LQ, size = difftotal *-1.5),
    shape = 25,
    fill = 'white',
    colour = 'white'
  ) +
  geom_point(
    data = x %>% filter(grepl(sector2digit_grepl,SIC_2DIGIT_NAME), flaggedplace =='A', difftotal > 0), 
    # aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace, size = COUNT)) +
    aes(y = INDUSTRY_NAME, x = LQ, size = difftotal),
    shape = 24,
    fill = 'red'
  ) +
  geom_point(
    data = x %>% filter(grepl(sector2digit_grepl,SIC_2DIGIT_NAME), flaggedplace =='A', difftotal < 0), 
    # aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace, size = COUNT)) +
    aes(y = INDUSTRY_NAME, x = LQ, size = difftotal * -1),
    shape = 25,
    fill = 'red'
  ) +
  # geom_point(
  #   data = x %>% filter(grepl(sector2digit_grepl,SIC_2DIGIT_NAME), flaggedplace =='A'), 
  #   # aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace, size = COUNT)) +
  #   aes(y = INDUSTRY_NAME, x = LQ, size = COUNT),
  #   shape = 16, colour = 'red'
  # ) +
  scale_size_continuous(range = c(1,18)) +
  scale_x_continuous(trans = "log10") +
  geom_vline(xintercept = 1, colour = 'blue') +
  guides(size = F)

  

sector2digit_grepl <- 'Manufacture of fabricated metal products, except machinery and equipment'
sector2digit_grepl <- 'basic metals'
sector2digit_grepl <- 'non-metallic'
sector2digit_grepl <- 'Manufacture of machinery and equipment n.e.c.'
sector2digit_grepl <- 'Wholesale trade'
sector2digit_grepl <- 'food products'
sector2digit_grepl <- 'wearing apparel'
sector2digit_grepl <- 'recorded media'
sector2digit_grepl <- 'Manufacture of chemicals'
sector2digit_grepl <- 'rubber and plastic'
sector2digit_grepl <- 'Manufacture of electrical equipment'
sector2digit_grepl <- 'Repair of computers'
sector2digit_grepl <- 'administration and defence'
sector2digit_grepl <- 'Warehousing and support activities for transportation'


#Check is single sector
x %>% filter(grepl(sector2digit_grepl,SIC_2DIGIT_NAME)) %>% select(SIC_2DIGIT_NAME) %>% pull() %>% unique()


#Try same, indicate with colour
ggplot() +
  geom_point(
    data = x %>% filter(grepl(sector2digit_grepl,SIC_2DIGIT_NAME), flaggedplace =='B', difftotal > 0), 
    # aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace, size = COUNT)) +
    aes(y = INDUSTRY_NAME, x = LQ, size = difftotal),
    alpha = 0.1,
    shape = 16,
    colour = 'green'
  ) +
  geom_point(
    data = x %>% filter(grepl(sector2digit_grepl,SIC_2DIGIT_NAME), flaggedplace =='B', difftotal < 0), 
    # aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace, size = COUNT)) +
    aes(y = INDUSTRY_NAME, x = LQ, size = difftotal * -1),
    alpha = 0.1,
    shape = 16,
    colour = 'red'
  ) +
  geom_point(
    data = x %>% filter(grepl(sector2digit_grepl,SIC_2DIGIT_NAME), flaggedplace =='A', difftotal > 0), 
    # aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace, size = COUNT)) +
    aes(y = INDUSTRY_NAME, x = LQ, size = difftotal *1.75),
    shape = 16,
    colour = 'black'
  ) +
  geom_point(
    data = x %>% filter(grepl(sector2digit_grepl,SIC_2DIGIT_NAME), flaggedplace =='A', difftotal < 0), 
    # aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace, size = COUNT)) +
    aes(y = INDUSTRY_NAME, x = LQ, size = difftotal *-1.75),
    shape = 16,
    colour = 'black'
  ) +
  geom_point(
    data = x %>% filter(grepl(sector2digit_grepl,SIC_2DIGIT_NAME), flaggedplace =='A', difftotal > 0), 
    # aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace, size = COUNT)) +
    aes(y = INDUSTRY_NAME, x = LQ, size = difftotal),
    shape = 16,
    colour = 'green'
  ) +
  geom_point(
    data = x %>% filter(grepl(sector2digit_grepl,SIC_2DIGIT_NAME), flaggedplace =='A', difftotal < 0), 
    # aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace, size = COUNT)) +
    aes(y = INDUSTRY_NAME, x = LQ, size = difftotal * -1),
    shape = 16,
    colour = 'red'
  ) +
  # geom_point(
  #   data = x %>% filter(grepl(sector2digit_grepl,SIC_2DIGIT_NAME), flaggedplace =='A'), 
  #   # aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace, size = COUNT)) +
  #   aes(y = INDUSTRY_NAME, x = LQ, size = COUNT),
  #   shape = 16, colour = 'red'
  # ) +
  scale_size_continuous(range = c(1,24)) +
  scale_x_continuous(trans = "log10") +
  geom_vline(xintercept = 1, colour = 'blue') +
  guides(size = F)



#Can we pick out smaller ones to combine? Is that all going to work with facetting? Seems slim, would have to use cowplot...?
sector2digit_grepl <- 'fabricated metal products, except machinery and equipment|basic metals|non-metallic'

#Check number of sectors is what we're aiming for given grepl
x %>% filter(grepl(sector2digit_grepl,SIC_2DIGIT_NAME)) %>% select(SIC_2DIGIT_NAME) %>% pull() %>% unique()


ggplot() +
  geom_point(
    data = x %>% filter(grepl(sector2digit_grepl,SIC_2DIGIT_NAME), flaggedplace =='B', difftotal > 0), 
    # aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace, size = COUNT)) +
    aes(y = INDUSTRY_NAME, x = LQ, size = difftotal),
    alpha = 0.1,
    shape = 16,
    colour = 'green'
  ) +
  geom_point(
    data = x %>% filter(grepl(sector2digit_grepl,SIC_2DIGIT_NAME), flaggedplace =='B', difftotal < 0), 
    # aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace, size = COUNT)) +
    aes(y = INDUSTRY_NAME, x = LQ, size = difftotal * -1),
    alpha = 0.1,
    shape = 16,
    colour = 'red'
  ) +
  geom_point(
    data = x %>% filter(grepl(sector2digit_grepl,SIC_2DIGIT_NAME), flaggedplace =='A', difftotal > 0), 
    # aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace, size = COUNT)) +
    aes(y = INDUSTRY_NAME, x = LQ, size = difftotal *1.75),
    shape = 16,
    colour = 'black'
  ) +
  geom_point(
    data = x %>% filter(grepl(sector2digit_grepl,SIC_2DIGIT_NAME), flaggedplace =='A', difftotal < 0), 
    # aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace, size = COUNT)) +
    aes(y = INDUSTRY_NAME, x = LQ, size = difftotal *-1.75),
    shape = 16,
    colour = 'black'
  ) +
  geom_point(
    data = x %>% filter(grepl(sector2digit_grepl,SIC_2DIGIT_NAME), flaggedplace =='A', difftotal > 0), 
    # aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace, size = COUNT)) +
    aes(y = INDUSTRY_NAME, x = LQ, size = difftotal),
    shape = 16,
    colour = 'green'
  ) +
  geom_point(
    data = x %>% filter(grepl(sector2digit_grepl,SIC_2DIGIT_NAME), flaggedplace =='A', difftotal < 0), 
    # aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace, size = COUNT)) +
    aes(y = INDUSTRY_NAME, x = LQ, size = difftotal * -1),
    shape = 16,
    colour = 'red'
  ) +
  # geom_point(
  #   data = x %>% filter(grepl(sector2digit_grepl,SIC_2DIGIT_NAME), flaggedplace =='A'), 
  #   # aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace, size = COUNT)) +
  #   aes(y = INDUSTRY_NAME, x = LQ, size = COUNT),
  #   shape = 16, colour = 'red'
  # ) +
  scale_size_continuous(range = c(1,24)) +
  scale_x_continuous(trans = "log10") +
  geom_vline(xintercept = 1, colour = 'blue') +
  guides(size = F) +
  facet_wrap(~SIC_2DIGIT_NAME, ncol = 1, scales = 'free_y') +
  geom_text(data = x %>% filter(grepl(sector2digit_grepl,SIC_2DIGIT_NAME), flaggedplace =='A', difftotal < 0), aes(y = INDUSTRY_NAME, x = LQ, label = COUNT), nudge_x = 0.2, hjust = 0, size = 3)






#TEST ADDING IN ARBITRARY PLACE OVER THE TOP E.G. TO SEE SY + MANC COMPARED
# addplace_to_LQplot <- function(plot_to_addto, place, shapenumber=16,backgroundcolour='black', add_jobnumbers = F, setalpha = 1, addminmax = F){
#   
#   plot_to_addto <- plot_to_addto +
#     geom_point(
#       data = x %>% filter(grepl(sector2digit_grepl,SIC_2DIGIT_NAME), GEOGRAPHY_NAME == place, difftotal > 0), 
#       # aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace, size = COUNT)) +
#       aes(y = INDUSTRY_NAME_REDUCED, x = LQ, size = difftotal *1.75),
#       shape = shapenumber,
#       colour = backgroundcolour,
#       alpha = setalpha
#     ) +
#     geom_point(
#       data = x %>% filter(grepl(sector2digit_grepl,SIC_2DIGIT_NAME), GEOGRAPHY_NAME == place, difftotal < 0), 
#       # aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace, size = COUNT)) +
#       aes(y = INDUSTRY_NAME_REDUCED, x = LQ, size = difftotal *-1.75),
#       shape = shapenumber,
#       colour = backgroundcolour,
#       alpha = setalpha
#     ) +
#     geom_point(
#       data = x %>% filter(grepl(sector2digit_grepl,SIC_2DIGIT_NAME), GEOGRAPHY_NAME == place, difftotal > 0), 
#       # aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace, size = COUNT)) +
#       aes(y = INDUSTRY_NAME_REDUCED, x = LQ, size = difftotal),
#       shape = shapenumber,
#       colour = 'green',
#       alpha = setalpha
#     ) +
#     geom_point(
#       data = x %>% filter(grepl(sector2digit_grepl,SIC_2DIGIT_NAME), GEOGRAPHY_NAME == place, difftotal < 0), 
#       # aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace, size = COUNT)) +
#       aes(y = INDUSTRY_NAME_REDUCED, x = LQ, size = difftotal * -1),
#       shape = shapenumber,
#       colour = 'red',
#       alpha = setalpha
#     ) 
#   
#   
#   if(add_jobnumbers){
#     
#     plot_to_addto <- plot_to_addto +  
#       geom_text(
#         data = x %>% filter(grepl(sector2digit_grepl,SIC_2DIGIT_NAME), GEOGRAPHY_NAME == place), 
#         aes(y = INDUSTRY_NAME_REDUCED, x = max(LQ) + 10, label = paste0(COUNT,', ',round(sector_regional_proportion * 100, 2),'%')),
#         # aes(y = INDUSTRY_NAME, x = max(LQ) + 2, label = COUNT),
#         nudge_x = 0.3, hjust = 1, alpha = 0.7, size = 3
#       )
#     
#     
#   }
#   
#   if(addminmax){
#     
#     plot_to_addto <- plot_to_addto +
#       geom_errorbar(
#         data = x %>% filter(grepl(sector2digit_grepl,SIC_2DIGIT_NAME), GEOGRAPHY_NAME == place),
#         aes(y = INDUSTRY_NAME_REDUCED, xmin = minn, xmax = maxx),
#         width = 0.05
#       )
#     
#   }
#    
#   return(plot_to_addto)
#   
# }



addplace_to_LQplot <- function(df, plot_to_addto, place, shapenumber=16,backgroundcolour='black', add_jobnumbers = F, setalpha = 1, addminmax = F){
  
  plot_to_addto <- plot_to_addto +
    geom_point(
      data = df %>% filter(GEOGRAPHY_NAME == place, difftotal > 0), 
      # aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace, size = COUNT)) +
      aes(y = INDUSTRY_NAME_REDUCED, x = LQ, size = difftotal *1.75),
      shape = shapenumber,
      colour = backgroundcolour,
      alpha = setalpha
    ) +
    geom_point(
      data = df %>% filter(GEOGRAPHY_NAME == place, difftotal < 0), 
      # aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace, size = COUNT)) +
      aes(y = INDUSTRY_NAME_REDUCED, x = LQ, size = difftotal *-1.75),
      shape = shapenumber,
      colour = backgroundcolour,
      alpha = setalpha
    ) +
    geom_point(
      data = df %>% filter(GEOGRAPHY_NAME == place, difftotal > 0), 
      # aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace, size = COUNT)) +
      aes(y = INDUSTRY_NAME_REDUCED, x = LQ, size = difftotal),
      shape = shapenumber,
      colour = 'green',
      alpha = setalpha
    ) +
    geom_point(
      data = df %>% filter(GEOGRAPHY_NAME == place, difftotal < 0), 
      # aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace, size = COUNT)) +
      aes(y = INDUSTRY_NAME_REDUCED, x = LQ, size = difftotal * -1),
      shape = shapenumber,
      colour = 'red',
      alpha = setalpha
    ) 
  
  
  if(add_jobnumbers){
    
    plot_to_addto <- plot_to_addto +  
      geom_text(
        data = df %>% filter(GEOGRAPHY_NAME == place), 
        aes(y = INDUSTRY_NAME_REDUCED, x = max(LQ) + 40, label = paste0(COUNT,', ',round(sector_regional_proportion * 100, 2),'%')),
        # aes(y = INDUSTRY_NAME, x = max(LQ) + 2, label = COUNT),
        nudge_x = 0.3, hjust = 1, alpha = 0.7, size = 3
      )
    
    
  }
  
  if(addminmax){
    
    plot_to_addto <- plot_to_addto +
      geom_errorbar(
        data = df %>% filter(GEOGRAPHY_NAME == place),
        aes(y = INDUSTRY_NAME_REDUCED, xmin = minn, xmax = maxx),
        width = 0.05
      )
    
  }
   
  return(plot_to_addto)
  
}





#FUNCTION FOR SAVING FACETTED SUBPLOTS OF THE ABOVE TO LOOK TOGETHER
savesubplots <- function(sic5filterlist,sic2grepl_remove = '', filename, plot_title = '', facet_ncol = 3, plotheight = 13, plotwidth = 22){
  
  plotdata <- x %>% filter(
    INDUSTRY_NAME %in% sic5filterlist
  )
  
  #2 digit sectors to remove
  if(nchar(sic2grepl_remove)>0){
    
    plotdata <- plotdata %>% filter(
    !grepl(sic2grepl_remove,SIC_2DIGIT_NAME)
    )
    
  }
  
  
  cat('number and proportion of FT workers:\n')
  
  count_n_perc <- plotdata %>% 
    filter(GEOGRAPHY_NAME == 'South Yorkshire') %>% 
    summarise(
      count = sum(COUNT),
      totperc = sum(sector_regional_proportion) * 100
    )
  
  print(
    count_n_perc
  )
  
  plotdata <- plotdata %>% 
    mutate(
      SIC_2DIGIT_NAME = factor(SIC_2DIGIT_NAME),
      SIC_2DIGIT_NAME = fct_reorder(SIC_2DIGIT_NAME,as.numeric(SIC_2DIGIT_CODE))
    )
  
  #Base plot
  p <- ggplot() +
    geom_point(
      data = plotdata %>% filter(difftotal > 0), 
      aes(y = INDUSTRY_NAME_REDUCED, x = LQ, size = difftotal),
      alpha = 0.1,
      shape = 16,
      colour = 'green'
    ) +
    geom_point(
      data = plotdata %>% filter(difftotal < 0), 
      # aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace, size = COUNT)) +
      aes(y = INDUSTRY_NAME_REDUCED, x = LQ, size = difftotal * -1),
      alpha = 0.1,
      shape = 16,
      colour = 'red'
    )  +
    scale_size_continuous(range = c(1,24)) +
    scale_x_continuous(trans = "log10") +
    geom_vline(xintercept = 1, colour = 'blue') +
    guides(size = F) +
    facet_wrap(~SIC_2DIGIT_NAME, ncol = facet_ncol, scales = 'free_y', dir = 'v')#If one sector only, adds name anyway, still useful
  
  
  #Add a place
  p <- addplace_to_LQplot(df = plotdata, plot_to_addto = p, place = 'Greater Manchester', shapenumber = 18,backgroundcolour = '#9ac0db', setalpha = 0.7)
  p <- addplace_to_LQplot(df = plotdata, plot_to_addto = p, place = 'South Yorkshire', shapenumber = 16, add_jobnumbers = T, addminmax = T)
  
  p <- p +
    ggtitle(
      paste0(
        plot_title,
        '. Job count: ', count_n_perc$count,
        ', Percent: ', round(count_n_perc$totperc,2),'%'
        )
      ) +
    theme(plot.title = element_text(face = 'bold'))
    # theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
  
  ggsave(plot = p, filename =paste0('local/localimages/',filename,'.png'), height = plotheight, width = plotwidth)
  
  #Might want to use this subset elsewhere
  return(plotdata)
  
}


#Add in a z score for the OLS slopes for a bit more control on growth/shrinkage
x <- x %>% 
  mutate(slope_zscore = (difftotal - mean(difftotal, na.rm = T))/sd(difftotal, na.rm=T))



#GROUPING THAT CAPTURES MOST OF MANUF PLUS RELATED SECTORS
#THAT ARE PROPORTIONALLY LARGER IN SY
#MANUALLY PULLING OUT SOME 2 DIGIT SECTORS PROB NOT RELEVANT
returnplotdata <- savesubplots(
  sic5filterlist = c(
    x %>% filter(GEOGRAPHY_NAME=="South Yorkshire") %>% #Keep scientific research, though LQ < 1
      ungroup() %>% 
      # filter(grepl('Manuf',INDUSTRY_NAME)) %>% 
      filter(grepl('Scientific research',SIC_2DIGIT_NAME)) %>%
      select(INDUSTRY_NAME) %>% 
      pull,
    x %>% filter(
    GEOGRAPHY_NAME=="South Yorkshire",LQ > 1, minn > 1, sector_regional_proportion * 100 > 0.05,
    !grepl('call centres|Primary education|secondary education|Other education', INDUSTRY_NAME)
  ) %>% 
    ungroup() %>% 
    select(INDUSTRY_NAME) %>% 
    pull),
  sic2grepl_remove = 'Wholesale trade|Air transport|Food and beverage|Legal|Public admin|Residential care|wearing apparel|Retail trade|Warehousing|Rental|materials recovery|Wholesale|Postal|Sports|Computer programming|Human health',
  filename = 'SY_5digit_manufacturing_n_related_LQmorethan1_neverlessthan1',
  plot_title = 'Custom collection of manufacturing and related, LQ > 1 and never less than 1 over date range (except some scientific, as part of manuf cluster). Job proportion > 0.05%'
    )


#SAME AGAIN BUT ONLY LQ GROWING OVER CERTAIN SIZE
returnplotdata <- savesubplots(
  sic5filterlist = x %>% filter(
    GEOGRAPHY_NAME=="South Yorkshire",LQ > 1, minn > 1, difftotal > 0, sector_regional_proportion * 100 > 0.05,
    !grepl('call centres|Primary education|secondary education|Other education', INDUSTRY_NAME)
  ) %>% 
    ungroup() %>% 
    select(INDUSTRY_NAME) %>% 
    pull,
  sic2grepl_remove = 'Wholesale trade|Air transport|Food and beverage|Legal|Public admin|Residential care|wearing apparel|Retail trade|Warehousing|Rental|materials recovery|Wholesale|Postal|Sports|Computer programming|Human health',
  filename = 'SY_5digit_manufacturing_n_related_LQmorethan1_neverlessthan1_slope_morethan_0_b',
  # filename = 'SY_5digit_manufacturing_n_related_LQmorethan1_neverlessthan1_slope_zscore_morethan_0point5',
  plot_title = 'Custom collection of manufacturing and related, LQ > 1 and never less than 1 over date range.\nJob proportion > 0.05%. ONLY GROWING (slope > 0)',
  facet_ncol = 2,
  plotheight = 11,
  plotwidth = 15
    )



#SAME GROUPING BUT FOCUS ON "LQ<1 BUT GROWING"
#Not v interesting, as it turns out
savesubplots(
  sic5filterlist = 
    x %>% filter(
      GEOGRAPHY_NAME=="South Yorkshire",LQ < 1, difftotal > 0, sector_regional_proportion * 100 > 0.05,
      !grepl('call centres|Primary education|secondary education|Other education', INDUSTRY_NAME)
    ) %>% 
      ungroup() %>% 
      select(INDUSTRY_NAME) %>% 
      pull,
  sic2grepl_remove = 'Wholesale trade|Air transport|Food and beverage|Legal|Public admin|Residential care|wearing apparel|Retail trade|Warehousing|Rental|materials recovery|Wholesale|Postal|Sports|Computer programming|Human health',
  filename = 'SY_5digit_manufacturing_n_related_LQ_LESSthan1_slope__morethan0',
  plot_title = 'Custom collection of manufacturing and related, LQ < 1 but growing (slope > 0). Job proportion > 0.05%'
)
#Compare that to just slope > 0
savesubplots(
  sic5filterlist = 
    x %>% filter(
      GEOGRAPHY_NAME=="South Yorkshire",LQ < 1, difftotal > 0, sector_regional_proportion * 100 > 0.05,
      !grepl('call centres|Primary education|secondary education|Other education', INDUSTRY_NAME)
    ) %>% 
      ungroup() %>% 
      select(INDUSTRY_NAME) %>% 
      pull,
  sic2grepl_remove = 'Wholesale trade|Air transport|Food and beverage|Legal|Public admin|Residential care|wearing apparel|Retail trade|Warehousing|Rental|materials recovery|Wholesale|Postal|Sports|Computer programming|Human health',
  filename = 'SY_5digit_manufacturing_n_related_LQ_LESSthan1_slope_morethan0',
  plot_title = 'Custom collection of manufacturing and related, LQ < 1 but growing (slope > 0). Job proportion > 0.05%'
)





#ALL OTHER LQ > 1 / ALWAYS > 1, NOT MANUF FOCUSED
#i.e. the 5 digit opposite of the first one above
returnplotdata2 <- savesubplots(
  sic5filterlist = 
    x %>% filter(
      GEOGRAPHY_NAME=="South Yorkshire",LQ > 1, minn > 1, sector_regional_proportion * 100 > 0.05,
      !INDUSTRY_NAME %in% unique(returnplotdata$INDUSTRY_NAME)
    ) %>% 
      ungroup() %>% 
      select(INDUSTRY_NAME) %>% 
      pull,
  filename = 'SY_5digit_ALLOTHER_NOT_MANUF_LQmorethan1_neverlessthan1',
  plot_title = 'All other sectors not in the custom collection of manufacturing and related, LQ > 1 and never less than 1 over date range. Job proportion > 0.05%'
)


#SAME AGAIN BUT ONLY GROWING slope > 0
returnplotdata2 <- savesubplots(
  sic5filterlist = 
    x %>% filter(
      GEOGRAPHY_NAME=="South Yorkshire",LQ > 1, minn > 1, difftotal > 0, sector_regional_proportion * 100 > 0.05,
      !INDUSTRY_NAME %in% unique(returnplotdata$INDUSTRY_NAME)
    ) %>% 
      ungroup() %>% 
      select(INDUSTRY_NAME) %>% 
      pull,
  filename = 'SY_5digit_ALLOTHER_NOT_MANUF_LQmorethan1_neverlessthan1_slope_morethan0_b',
  plot_title = 'All other sectors LQ > 1 and never less than 1 over date range.\nJob proportion > 0.05%. ONLY GROWING (slope > 0)',
  facet_ncol = 2,
  plotheight = 11,
  plotwidth = 15
)


#SAME AGAIN (nom-manuf) BUT LQ < 1 AND growing ones
savesubplots(
  sic5filterlist = 
    x %>% filter(
      GEOGRAPHY_NAME=="South Yorkshire",LQ < 1, difftotal > 0, sector_regional_proportion * 100 > 0.05,
      !INDUSTRY_NAME %in% unique(returnplotdata$INDUSTRY_NAME)
    ) %>% 
      ungroup() %>% 
      select(INDUSTRY_NAME) %>% 
      pull,
  filename = 'SY_5digit_ALLOTHER_NOT_MANUF_LQLESSTHAN1_slope_MORETHAN_0',
  plot_title = 'All other sectors not in the custom collection of manufacturing and related\nLQ < 1. Job proportion > 0.05%. ONLY GROWING (slope > 0)'
)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#PLOT REGIONAL VS GB PROPORTION ON DIAG PLOT----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

place1 = 'South Yorkshire'

uk_regional_props_minus_targetregion.bres <- itl2.lq %>%
  filter(GEOGRAPHY_NAME != place1) %>% 
  group_by(GEOGRAPHY_NAME, DATE) %>% 
  mutate(
    region_totalsize = sum(COUNT, na.rm = T),#a. Current employment per region per year, for regional denominator
    sector_regional_proportion = COUNT / region_totalsize#b. regional sector proportion.
  ) %>% 
  group_by(DATE, INDUSTRY_NAME) %>% 
  mutate(
    uk_sectorsize = sum(COUNT, na.rm = T),#c. Summed current prices for EACH SECTOR, UK-wide
  ) %>% 
  group_by(DATE) %>% 
  mutate(
    uk_totalsize = sum(COUNT, na.rm = T),#d. Summed current prices for WHOLE UK per year, for UK denominator
    sector_uk_proportion_minustargetITL = uk_sectorsize / uk_totalsize#e. UK-level sector proportion
  )


#Add back in to a copy of the main itl df, so it can be smoothed
itl2.lq.w.sector_uk_proportion_minustargetITL <- itl2.lq %>% 
  left_join(
    uk_regional_props_minus_targetregion.bres %>% 
      select(INDUSTRY_NAME, DATE,sector_uk_proportion_minustargetITL) %>% 
      distinct(INDUSTRY_NAME,DATE,sector_uk_proportion_minustargetITL),
    by = c('INDUSTRY_NAME', 'DATE')
  )

#Check, should be slightly different
cor(itl2.lq.w.sector_uk_proportion_minustargetITL$sector_uk_proportion,itl2.lq.w.sector_uk_proportion_minustargetITL$sector_uk_proportion_minustargetITL, use = 'complete.obs')




twoy <- itl2.lq.w.sector_uk_proportion_minustargetITL %>% 
  ungroup() %>% 
  filter(
    DATE %in% c(min(DATE),max(DATE)),
    GEOGRAPHY_NAME == 'South Yorkshire',
    )

twoy$INDUSTRY_NAME_REDUCED <- gsub(x = twoy$INDUSTRY_NAME, pattern = 'of |and |acture|acturing|activities|equipment|products', replacement = '')

twoy_lags <- twoy %>% 
  arrange(INDUSTRY_NAME,DATE) %>% 
  mutate(
    lag_sector_regional_proportion = sector_regional_proportion - lag(sector_regional_proportion),
    lag_sector_uk_proportion_minustargetITL = sector_uk_proportion_minustargetITL - lag(sector_uk_proportion_minustargetITL)
  ) %>% 
  filter(DATE == max(DATE)) %>% 
  mutate(
    compass = case_when(
      lag_sector_regional_proportion < 0 & lag_sector_uk_proportion_minustargetITL < 0 ~ 'SW',
      lag_sector_regional_proportion < 0 & lag_sector_uk_proportion_minustargetITL > 0 ~ 'NW',
      lag_sector_regional_proportion > 0 & lag_sector_uk_proportion_minustargetITL > 0 ~ 'NE',
      lag_sector_regional_proportion > 0 & lag_sector_uk_proportion_minustargetITL < 0 ~ 'SE'
    )
  )



twoy <- twoy %>%
  left_join(
    twoy_lags %>% 
      select(INDUSTRY_NAME,compass),
    by = 'INDUSTRY_NAME'
  )


filtercompass='NE'#both growing
filtercompass='NW'#SY shrinking, growth elsewhere
filtercompass='SE'#SY growing, shrinkage elsewhere
filtercompass='SW'#both shrinking
filtercompass= c('NE','NW','SE','SW')

#SY growth, both shrink growth elsewhere
filtercompass= c('NE','SE')
#SY shrink, both shrink growth else
filtercompass= c('NW','SW')


#Filter to subset of sectors
twoy.p <- twoy %>% 
  filter(INDUSTRY_NAME %in% returnplotdata$INDUSTRY_NAME)
  # filter(INDUSTRY_NAME %in% returnplotdata2$INDUSTRY_NAME)

sum(
  twoy.p %>% filter(compass %in% filtercompass) %>% select(sector_regional_proportion) %>% pull()
  )*100

#Check none are higher props than on the plot
table(twoy.p$sector_uk_proportion_minustargetITL*100 < 1)
table(twoy.p$sector_regional_proportion*100 < 1)

table(twoy.p$sector_uk_proportion_minustargetITL*100 < 0.1)
table(twoy.p$sector_regional_proportion*100 < 0.1)

max(twoy.p$sector_uk_proportion_minustargetITL*100)
max(twoy.p$sector_regional_proportion*100)

p <- ggplot(
  twoy.p %>% filter(compass %in% filtercompass),
  aes(x = sector_regional_proportion * 100, y = sector_uk_proportion_minustargetITL * 100)) +
  geom_point(size = 5, alpha = 0.75, aes(colour = factor(DATE), group = INDUSTRY_NAME_REDUCED)) +
  geom_line(size = 1, aes(colour = factor(DATE), group = INDUSTRY_NAME_REDUCED)) +
  xlab('South Yorkshire FT job count proportion') +
  ylab('UK FT job count proportion (MINUS South Yorkshire)')  +
  geom_abline(slope = 1, size = 1, colour='blue', alpha = 0.5) +
  coord_cartesian(xlim = c(0.01,1), ylim = c(0.01,1)) + # good for log scale
  # coord_cartesian(xlim = c(0.01,7), ylim = c(0.01,7)) + # good for log scale
  # coord_cartesian(xlim = c(0.01,11), ylim = c(0.01,11)) + # good for log scale
  scale_y_log10() +
  scale_x_log10() +
  guides(colour=guide_legend(title=" "))

p <- p + geom_text_repel(
  data = twoy.p %>% filter(DATE==max(twoy$DATE), compass %in% filtercompass),
  aes(x = sector_regional_proportion * 100, y = sector_uk_proportion_minustargetITL * 100,label = INDUSTRY_NAME_REDUCED, colour = compass),
  alpha=1,
  # colour = 'black',
  nudge_x = .05,
  box.padding = 1,
  nudge_y = 0.05,
  segment.curvature = -0.1,
  segment.ncp = 0.3,
  segment.angle = 20
) +
  # scale_color_brewer(palette = 'Dark2', direction = -1) 
  scale_color_manual(values = c('red','black','#7fc97f','#beaed4','#fdc086','#1f78b4'))


p

ggsave(plot = p, filename = paste0('local/localimages/fivedigit_nonmanuf_collection_nonlog_',filtercompass,'.png'), height = 9, width = 9)











#~~~~~~~~~~~~~~~~~~~~~~~
#OTHER 5 DIGIT STUFF----
#~~~~~~~~~~~~~~~~~~~~~~~

x <- readRDS('local/x_2021.rds')

sectors5other <- x %>% filter(
  GEOGRAPHY_NAME=="South Yorkshire",LQ > 1, minn > 1, difftotal > 0) %>% 
  # GEOGRAPHY_NAME=="South Yorkshire",LQ > 1, minn > 1, difftotal > 0) %>% 
  # GEOGRAPHY_NAME=="South Yorkshire",LQ > 1, minn > 1) %>% 
  ungroup() %>% 
  select(INDUSTRY_NAME) %>% 
  pull


#Bespoke sector selection (which I did somewhere else but can't currently find)
#For now, just pulling out creative + museum from the two digit
creativez <- unique(x$INDUSTRY_NAME[grepl(x = x$SIC_2DIGIT_NAME, pattern = 'creative|museum', ignore.case = T)])

#OK, that'll do for now



length(sectors5other)
length(orig5sectors)

#Keep ones not in the previous list... oh, it's also reduced in the plot thing
sectors5other[!sectors5other %in% orig5sectors]
length(sectors5other[!sectors5other %in% orig5sectors])


#List of the 2 digits I want to take out of that that are less connected also added in
# plotdata <- x %>% filter(
#   INDUSTRY_NAME %in% sectors5other[!sectors5other %in% orig5sectors]
# )

#HAAACK
plotdata <- x %>% filter(
  INDUSTRY_NAME %in% creativez
)

#What the tot workforce in that?
plotdata %>% 
  filter(GEOGRAPHY_NAME == 'South Yorkshire') %>% 
  summarise(
    count = sum(COUNT),
    totperc = sum(sector_regional_proportion) * 100
  )


#Factor order the facets
plotdata <- plotdata %>% 
  mutate(
    SIC_2DIGIT_NAME = factor(SIC_2DIGIT_NAME),
    SIC_2DIGIT_NAME = fct_reorder(SIC_2DIGIT_NAME,as.numeric(SIC_2DIGIT_CODE))
  )


#Base plot
p <- ggplot() +
  geom_point(
    data = plotdata %>% filter(difftotal > 0), 
    aes(y = INDUSTRY_NAME_REDUCED, x = LQ, size = difftotal),
    alpha = 0.1,
    shape = 16,
    colour = 'green'
  ) +
  geom_point(
    data = plotdata %>% filter(difftotal < 0), 
    # aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace, size = COUNT)) +
    aes(y = INDUSTRY_NAME_REDUCED, x = LQ, size = difftotal * -1),
    alpha = 0.1,
    shape = 16,
    colour = 'red'
  )  +
  scale_size_continuous(range = c(1,24)) +
  scale_x_continuous(trans = "log10") +
  geom_vline(xintercept = 1, colour = 'blue') +
  guides(size = F) +
  facet_wrap(~SIC_2DIGIT_NAME, ncol = 3, scales = 'free_y', dir = 'v')#If one sector only, adds name anyway, still useful


#Add a place
p <- addplace_to_LQplot(df = plotdata, plot_to_addto = p, place = 'Greater Manchester', shapenumber = 18,backgroundcolour = '#9ac0db', setalpha = 0.7)
p <- addplace_to_LQplot(df = plotdata, plot_to_addto = p, place = 'South Yorkshire', shapenumber = 16, add_jobnumbers = T, addminmax = T)
p

# ggsave(plot = p, filename = 'local/localimages/sy_5digitLqmorethan1.png', height=15, width=24)
ggsave(plot = p, filename = 'local/localimages/sy_5digitLqmorethan1_slopemorethan0.png', height=15, width=24)





#AND LQ < 1 BUT GROWING AT SIZEABLE RATE

#Difftotal spread?
#Let's add a z score for the slope
x <- x %>% 
  mutate(zscore = (difftotal - mean(difftotal, na.rm = T))/sd(difftotal, na.rm=T))

#check
# mean(x$zscore, na.rm = T)
# sd(x$zscore, na.rm = T)

#Keep bigger changes
sectors5LQ <- x %>% filter(
  GEOGRAPHY_NAME=="South Yorkshire",LQ < 1, zscore > 0.1, sector_regional_proportion * 100 > 0.05) %>% 
  ungroup() %>% 
  select(INDUSTRY_NAME) %>% 
  pull

length(sectors5LQ)

#List of the 2 digits I want to take out of that that are less connected also added in
plotdata <- x %>% filter(
  INDUSTRY_NAME %in% sectors5LQ
)

#What the tot workforce in that?
plotdata %>% 
  filter(GEOGRAPHY_NAME == 'South Yorkshire') %>% 
  summarise(
    count = sum(COUNT),
    totperc = sum(sector_regional_proportion) * 100
  )


#Factor order the facets
plotdata <- plotdata %>% 
  mutate(
    SIC_2DIGIT_NAME = factor(SIC_2DIGIT_NAME),
    SIC_2DIGIT_NAME = fct_reorder(SIC_2DIGIT_NAME,as.numeric(SIC_2DIGIT_CODE))
  )


#Base plot
p <- ggplot() +
  geom_point(
    data = plotdata %>% filter(difftotal > 0), 
    aes(y = INDUSTRY_NAME_REDUCED, x = LQ, size = difftotal),
    alpha = 0.1,
    shape = 16,
    colour = 'green'
  ) +
  geom_point(
    data = plotdata %>% filter(difftotal < 0), 
    # aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace, size = COUNT)) +
    aes(y = INDUSTRY_NAME_REDUCED, x = LQ, size = difftotal * -1),
    alpha = 0.1,
    shape = 16,
    colour = 'red'
  )  +
  scale_size_continuous(range = c(1,24)) +
  scale_x_continuous(trans = "log10") +
  geom_vline(xintercept = 1, colour = 'blue') +
  guides(size = F) +
  facet_wrap(~SIC_2DIGIT_NAME, ncol = 3, scales = 'free_y', dir = 'v')#If one sector only, adds name anyway, still useful


#Add a place
p <- addplace_to_LQplot(df = plotdata, plot_to_addto = p, place = 'Greater Manchester', shapenumber = 18,backgroundcolour = '#9ac0db', setalpha = 0.7)
p <- addplace_to_LQplot(df = plotdata, plot_to_addto = p, place = 'South Yorkshire', shapenumber = 16, add_jobnumbers = T, addminmax = T)
p

ggsave(plot = p, filename = 'local/localimages/sy_5digitLQlessthan1_zscore_0point5above.png', height=15, width=24)











#Let's output all of those to have a look through
SIC2digits <- unique(x$SIC_2DIGIT_NAME)

#Keep ones where LQ for this year / 2 digit SIC is at least 1 for one 5-digit
# SIC2digits <- x %>% filter(LQ > 1, GEOGRAPHY_NAME == 'South Yorkshire') %>% select(SIC_2DIGIT_NAME) %>% pull() %>% unique

for(sector2digit_grepl in SIC2digits){
  
  p <- ggplot() +
    geom_point(
      data = x %>% filter(grepl(sector2digit_grepl,SIC_2DIGIT_NAME), difftotal > 0), 
      # aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace, size = COUNT)) +
      aes(y = INDUSTRY_NAME_REDUCED, x = LQ, size = difftotal),
      alpha = 0.1,
      shape = 16,
      colour = 'green'
    ) +
    geom_point(
      data = x %>% filter(grepl(sector2digit_grepl,SIC_2DIGIT_NAME), difftotal < 0), 
      # aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace, size = COUNT)) +
      aes(y = INDUSTRY_NAME_REDUCED, x = LQ, size = difftotal * -1),
      alpha = 0.1,
      shape = 16,
      colour = 'red'
    )  +
    scale_size_continuous(range = c(1,24)) +
    scale_x_continuous(trans = "log10") +
    geom_vline(xintercept = 1, colour = 'blue') +
    guides(size = F) +
    ylab('') +
    facet_wrap(~SIC_2DIGIT_NAME, ncol = 1, scales = 'free_y')#If one sector only, adds name anyway, still useful
  
  
  #Add a place
  p <- addplace_to_LQplot(df = x %>% filter(grepl(sector2digit_grepl,SIC_2DIGIT_NAME)), plot_to_addto = p, place = 'Greater Manchester', shapenumber = 18,backgroundcolour = '#9ac0db', setalpha = 0.7)
  p <- addplace_to_LQplot(df = x %>% filter(grepl(sector2digit_grepl,SIC_2DIGIT_NAME)), plot_to_addto = p, place = 'South Yorkshire', shapenumber = 16, add_jobnumbers = T, addminmax = T)
  
  # ggsave(plot = p, filename = paste0('local/localimages/2digitSICs_BRES_LQs/',gsub(pattern = ' : ',replacement = '_', x = sector2digit_grepl),'.png'),width = 12, height = 12)
  ggsave(plot = p, filename = paste0('local/localimages/2digitSICs_BRES_LQs/',gsub(pattern = ' : ',replacement = '_', x = sector2digit_grepl),'.png'),width = 9, height = 9)
  
  
}








#Pick out random SY sectors from looking through the above to plot, see underlying pattern
v <- itl2.lq %>% 
  # filter(GEOGRAPHY_NAME=='South Yorkshire', grepl('23130', INDUSTRY_NAME))#manuf hollow glass
  # filter(GEOGRAPHY_NAME=='South Yorkshire', grepl('16230', INDUSTRY_NAME))#manuf other builders’ carpentry / joinery 
  filter(GEOGRAPHY_NAME=='South Yorkshire', grepl('24100', INDUSTRY_NAME))#manuf basic iron steel, 3500 people
  # filter(GEOGRAPHY_NAME=='South Yorkshire', grepl('52103 : Operation of warehousing and storage facilities for land trans', INDUSTRY_NAME))
  # filter(GEOGRAPHY_NAME=='South Yorkshire', grepl('58130 : Publishing of newspapers', INDUSTRY_NAME))
  # filter(GEOGRAPHY_NAME=='South Yorkshire', grepl('43320 : Joinery installation', INDUSTRY_NAME))

ggplot(v, aes(x = DATE, y = LQ)) +
  geom_point() +
  geom_line()
ggplot(v, aes(x = DATE, y = sector_regional_proportion * 100)) +
  geom_point() +
  geom_line()
ggplot(v, aes(x = DATE, y = COUNT)) +
  geom_point() +
  geom_line()


#All places
z <- itl2.lq %>% 
  # filter(grepl('58130 : Publishing of newspapers', INDUSTRY_NAME))
  # filter(grepl('62020 : Computer consultancy activities', INDUSTRY_NAME))
  # filter(grepl('84110 : General public administration activities', INDUSTRY_NAME))
  # filter(grepl('87200 : Residential care activities for learning disabilities, mental', INDUSTRY_NAME))
  filter(grepl('24100 : Manufacture of basic iron and steel', INDUSTRY_NAME))

ggplot(z, aes(x = DATE, y = LQ, colour = GEOGRAPHY_NAME)) +
  geom_point() +
  geom_line()

ggplot(z, aes(x = DATE, y = sector_regional_proportion * 100, colour = GEOGRAPHY_NAME)) +
  geom_point() +
  geom_line() +
  scale_y_log10()

ggplot(z, aes(x = DATE, y = COUNT, colour = GEOGRAPHY_NAME)) +
  geom_point() +
  geom_line() +
  scale_y_log10()

#MOVING AVERAGE
#https://stackoverflow.com/questions/26198551/rolling-mean-moving-average-by-group-id-with-dplyr
z <- z %>% 
  group_by(GEOGRAPHY_NAME) %>% 
  mutate(movingav = rollapply(sector_regional_proportion,3,mean,align='right',fill=NA)) %>%
  # mutate(movingav = rollapply(COUNT,3,mean,align='right',fill=NA)) %>% 
  ungroup()

# plot_ly(data = z %>% ungroup(), x = ~DATE, y = ~sector_regional_proportion, color = ~GEOGRAPHY_NAME, 
plot_ly(data = z %>% ungroup(), x = ~DATE, y = ~movingav, color = ~GEOGRAPHY_NAME, 
        text = ~paste("Place:", GEOGRAPHY_NAME, "\nPEOPLE: ",COUNT),  # Add this line for hover text
        hoverinfo = 'text',
        type = 'scatter', mode = 'lines+markers', line = list(shape = 'linear')) %>%
  layout(title = "Yearly percents by SIC", 
         xaxis = list(title = "Year"), 
         yaxis = list(title = "Value", type='log'),
         # yaxis = list(title = "Value"),
         showlegend = F)


#What's the geography of where increased or decreased in public admin?
#There's a 2019 inflection so would be interesting to see different slopes for different time chunks...
#Come back to.





#What's total employment in 25: manuf fabricated for 5-digits with LQ 1+, in SY?
w <- itl2.lq %>% 
  filter(GEOGRAPHY_NAME=='South Yorkshire', grepl('Manufacture of fabricated metal products, except', SIC_2DIGIT_NAME), LQ > 1, DATE == 2021)

#10,900 people = not far off 3% of working pop, big chunk.
sum(w$COUNT)
(sum(w$COUNT)/376670)*100

#What is it for +1 in 24 and 25?
w <- itl2.lq %>% 
  filter(GEOGRAPHY_NAME=='South Yorkshire', grepl('Manufacture of fabricated metal products, except|Manufacture of basic metals', SIC_2DIGIT_NAME), LQ > 1, DATE == 2021)

#16030, 4.25%
sum(w$COUNT)
(sum(w$COUNT)/376670)*100


#What's the change pattern for those 12?
w <- itl2.lq %>% 
  # filter(GEOGRAPHY_NAME=='South Yorkshire', grepl('Manufacture of fabricated metal products, except|Manufacture of basic metals', SIC_2DIGIT_NAME), LQ > 1, DATE == 2021)
  filter(GEOGRAPHY_NAME=='South Yorkshire', grepl('Manufacture of basic metals', SIC_2DIGIT_NAME), LQ > 1, DATE == 2021)

b <- itl2.lq %>% 
  # filter(GEOGRAPHY_NAME=='South Yorkshire', grepl('Manufacture of fabricated metal products, except', SIC_2DIGIT_NAME), INDUSTRY_NAME %in% w$INDUSTRY_NAME)
  filter(GEOGRAPHY_NAME=='South Yorkshire', grepl('Manufacture of basic metals', SIC_2DIGIT_NAME), INDUSTRY_NAME %in% w$INDUSTRY_NAME)

ggplot(b, aes(x = DATE, y = LQ, colour = INDUSTRY_NAME)) +
  geom_point() +
  geom_line() +
  scale_y_log10() +
  scale_colour_brewer(palette = "Set1")


# plot_ly(data = b %>% ungroup(), x = ~DATE, y = ~LQ, color = ~INDUSTRY_NAME, 
# plot_ly(data = b %>% ungroup(), x = ~DATE, y = ~COUNT, color = ~INDUSTRY_NAME, 
plot_ly(data = b %>% ungroup(), x = ~DATE, y = ~sector_regional_proportion, color = ~INDUSTRY_NAME, 
        text = ~paste("Sector:", INDUSTRY_NAME, "\nPEOPLE: ",COUNT),  # Add this line for hover text
        hoverinfo = 'text',
        type = 'scatter', mode = 'lines+markers', line = list(shape = 'linear')) %>%
  layout(title = "Yearly percents by SIC", 
         xaxis = list(title = "Year"), 
         yaxis = list(title = "Value", type='log'),
         # yaxis = list(title = "Value"),
         showlegend = F)



#MAP??
# oneyear <- itl2.lq %>% filter(grepl('Manufacture of computer, electronic', SIC_2DIGIT_NAME), DATE==2021)
oneyear <- itl2.lq %>% filter(DATE==2021)
itl2.geo <- st_read('data/geographies/International_Territorial_Level_2_January_2021_UK_BFE_V2_2022_-4735199360818908762/ITL2_JAN_2021_UK_BFE_V2.shp') %>% 
  st_simplify(preserveTopology = T, dTolerance = 100)

#Reminder: two spelling diffs, plus UK in the geo vs GB in the non geo
table(itl2.geo$ITL221NM %in% oneyear$GEOGRAPHY_NAME)
itl2.geo$ITL221NM[!itl2.geo$ITL221NM %in% oneyear$GEOGRAPHY_NAME]
unique(oneyear$GEOGRAPHY_NAME)[!unique(oneyear$GEOGRAPHY_NAME) %in% itl2.geo$ITL221NM]

#Manually set names in the geo
itl2.geo$ITL221NM[itl2.geo$ITL221NM=='Northumberland, and Tyne and Wear'] <- 'Northumberland and Tyne and Wear'
itl2.geo$ITL221NM[itl2.geo$ITL221NM=='West Wales and The Valleys'] <- 'West Wales'

#Now a merge will drop NI and we're all good.
chk <- itl2.geo %>% 
  right_join(
    oneyear,
    by = c('ITL221NM'='GEOGRAPHY_NAME')
  )



#Pick a 5-digit sector to plot
maptoplot <- chk %>% filter(INDUSTRY_NAME == '26110 : Manufacture of electronic components')
maptoplot <- chk %>% filter(grepl('26701 : Manufacture of optical precision instruments',INDUSTRY_NAME))
maptoplot <- chk %>% filter(grepl('24100 : Manufacture of basic iron and steel',INDUSTRY_NAME))
maptoplot <- chk %>% filter(grepl('25990 : Manufacture of other fabricated metal products',INDUSTRY_NAME))
maptoplot <- chk %>% filter(grepl('28410 : Manufacture of metal forming machinery',INDUSTRY_NAME))
maptoplot <- chk %>% filter(grepl('28910 : Manufacture of machinery for metallurgy',INDUSTRY_NAME))
maptoplot <- chk %>% filter(grepl('28930 : Manufacture of machinery for food, beverage and tobacco',INDUSTRY_NAME))
maptoplot <- chk %>% filter(grepl('28290 : Manufacture of other general-purpose machinery',INDUSTRY_NAME))
maptoplot <- chk %>% filter(grepl('32500 : Manufacture of medical and dental instruments and supplies',INDUSTRY_NAME))
maptoplot <- chk %>% filter(grepl('43220 : Plumbing, heat and air-conditioning installation',INDUSTRY_NAME))
maptoplot <- chk %>% filter(grepl('49410 : Freight transport by road',INDUSTRY_NAME))
maptoplot <- chk %>% filter(grepl('52103 : Operation of warehousing and storage facilities for land trans',INDUSTRY_NAME))
maptoplot <- chk %>% filter(grepl('69102 : Solicitors',INDUSTRY_NAME))
maptoplot <- chk %>% filter(grepl('69101 : Barristers at law',INDUSTRY_NAME))
maptoplot <- chk %>% filter(grepl('74909 : Other professional, scientific and technical activities',INDUSTRY_NAME))
maptoplot <- chk %>% filter(grepl('81100 : Combined facilities support activities',INDUSTRY_NAME))
maptoplot <- chk %>% filter(grepl('62090 : Other information technology and computer service activities',INDUSTRY_NAME))

tm_shape(maptoplot) +
  # tm_polygons('LQ_log', n = 11, palette="-RdYlGn")
  tm_polygons('LQ_log', n = 11)



#~~~~~~~~~~~~~~~~~~~~~~~~~
#MORE CHANGE OVER TIME----
#~~~~~~~~~~~~~~~~~~~~~~~~~



sectors.to.view <- x %>% ungroup() %>% filter(GEOGRAPHY_NAME == place, LQ > 2) %>% select(INDUSTRY_NAME) %>% pull() %>% as.character()

unique(itl2.lq$GEOGRAPHY_NAME)

#SY over time
sy <- itl2.lq %>% filter(GEOGRAPHY_NAME == 'South Yorkshire') %>% ungroup()
sy <- itl2.lq %>% filter(GEOGRAPHY_NAME == 'Greater Manchester') %>% ungroup()

#MOVING AVERAGE
#https://stackoverflow.com/questions/26198551/rolling-mean-moving-average-by-group-id-with-dplyr
sy <- sy %>% 
  group_by(INDUSTRY_NAME) %>% 
  mutate(movingav = rollapply(LQ,3,mean,align='right',fill=NA)) %>% 
  ungroup()

# plot_ly(data = sy %>% filter(INDUSTRY_NAME %in% sectors.to.view), x = ~DATE, y = ~LQplusone_log, color = ~INDUSTRY_NAME,
plot_ly(data = sy %>% filter(INDUSTRY_NAME %in% sectors.to.view, LQ!=0), x = ~DATE, y = ~movingav, color = ~INDUSTRY_NAME,
        text = ~paste("Sector:", INDUSTRY_NAME),  # Add this line for hover text
        hoverinfo = 'text+y+x',
        type = 'scatter', mode = 'lines+markers', line = list(shape = 'linear')) %>%
  layout(title = "Yearly values by SIC", 
         xaxis = list(title = "Year"),
         yaxis = list(title = "LQ", type='log'),
         # yaxis = list(title = "LQ"),
         showlegend = F)



#Location quotient vs proportion in the region (already calculated) seems obvious...
# plot_ly(data = sy %>% filter(DATE==2015), x = ~LQ, y = ~sector_regional_proportion, color = ~INDUSTRY_NAME,
plot_ly(data = sy %>% filter(DATE==2021), x = ~LQ, y = ~sector_regional_proportion, color = ~SIC_2DIGIT_NAME,
        text = ~paste("5 digit Sector:", INDUSTRY_NAME, "\n2 digit Sector:", SIC_2DIGIT_NAME, "\npercent: ", sector_regional_proportion * 100, "\ncount: ", COUNT),  # Add this line for hover text
        hoverinfo = 'text+y+x',
        type = 'scatter',
        size = 7) %>%
  layout(title = "Yearly values by SIC", 
         xaxis = list(title = "LQ", type = 'log'),
         # yaxis = list(title = "Region proportion"),
         yaxis = list(title = "Region proportion", type = 'log'),
         showlegend = F) %>% 
  add_lines(
    y = range(sy$sector_regional_proportion),
    x = 1,
    line = list(
      color = "grey"
    ),
    inherit = FALSE,
    showlegend = FALSE
  )




#View 2 digit groups on hover
#Adapted from https://stackoverflow.com/a/52709868
xy <- sy %>% filter(DATE==2021)

#create a SharedData object for use in the ggplot below, group by 'groups' 
d <- highlight_key(xy, ~SIC_2DIGIT_NAME)

#create a normal ggplot to fit your needs, but use the SharedData object as data for the chart
p <- ggplot( d, aes(x = LQ, y = sector_regional_proportion, group = SIC_2DIGIT_NAME, colour = INDUSTRY_NAME)) + theme_bw() + geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  geom_vline(xintercept = 1) +
  guides(colour = F)

#now ggplotly the newly created ggplot, and add text for the tooltips as needed
gg <- ggplotly( p, tooltip = c("SIC_2DIGIT_NAME","x","y","INDUSTRY_NAME" ))

#set the highlight-options to your liking, and plot...
highlight( gg, on = "plotly_hover", off = "plotly_deselect", color = "red", opacityDim = 0.2)


#Remove some ineffective 2 place comparison code. See here if want to refer back to:
#https://github.com/DanOlner/ukcompare/tree/203b5e921b13a4d81b769b46981805b9fa57edfb



#Compare regional proportions directly
cp <- itl2.lq %>% filter(GEOGRAPHY_NAME %in% c('South Yorkshire','Greater Manchester'), DATE==2021) %>%
  mutate(sector_regional_percent = sector_regional_proportion * 100) %>% 
  ungroup()

#Put each place on its own axis
cpwide <- cp %>% 
  select(GEOGRAPHY_NAME,INDUSTRY_NAME,sector_regional_proportion) %>% 
  pivot_wider(names_from = GEOGRAPHY_NAME, values_from = sector_regional_proportion)


p <- ggplot(cpwide,aes(x = `Greater Manchester`, y = `South Yorkshire`, colour = INDUSTRY_NAME)) +
  geom_point() + 
  geom_abline(slope = 1) +
  scale_y_log10() +
  scale_x_log10() +
  guides(colour = F)

ggplotly( p, tooltip = c("INDUSTRY_NAME" ))



#~~~~~~~~~~~~
#XY plots----
#~~~~~~~~~~~~

#Plot difference / OLS slope against actual LQ
#Plan: four quadrants mean something different

#Reload for 2021
#x <- saveRDS('local/x_2021.rds')

#Using x created above (need to make that better at some point!) for 2021

placename = 'South Yorkshire'
placename = 'Greater Manchester'
placename = 'Leicestershire, Rutland and Northamptonshire'

p <- ggplot(x %>% filter(GEOGRAPHY_NAME==placename, sector_regional_proportion * 100 > 0.1), aes(x = difftotal, y = LQ, label = INDUSTRY_NAME)) +
  geom_point(aes(size = sector_regional_proportion * 100), alpha = 0.3) +
  scale_y_log10() +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 1) +
  scale_size_continuous(range = c(1,12)) +
  coord_cartesian(xlim = c(-0.2,0.2))

ggplotly(p, tooltip = c("INDUSTRY_NAME"))


ggplot(x %>% filter(GEOGRAPHY_NAME==placename, sector_regional_proportion * 100 > 1), aes(x = difftotal, y = LQ, label = INDUSTRY_NAME)) +
  geom_point(aes(size = sector_regional_proportion * 100)) +
  scale_y_log10() +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 1) +
  # geom_text(
  #   aes(label=INDUSTRY_NAME),
  #   nudge_x=0.01, 
  #   # check_overlap=T,
  #   hjust = 'left'
  # ) +
  geom_text_repel(
    nudge_x = .05,
    box.padding = 0.2,
    nudge_y = 0.05,
    segment.curvature = -0.1,
    segment.ncp = 0.3,
    segment.angle = 20
  ) +
  scale_size_continuous(range = c(3,12))



#LQ vs sector regional proportion (problems with that right? This should correlate...)
ggplot(x %>% filter(GEOGRAPHY_NAME==placename, sector_regional_proportion * 100 > 1), aes(x = sector_regional_proportion, y = LQ, label = INDUSTRY_NAME)) +
  geom_point(aes(size = sector_regional_proportion * 100)) +
  scale_y_log10() +
  geom_hline(yintercept = 1) +
  # geom_text(
  #   aes(label=INDUSTRY_NAME),
  #   nudge_x=0.01, 
  #   # check_overlap=T,
  #   hjust = 'left'
  # ) +
  geom_text_repel(
    nudge_x = .05,
    box.padding = 0.2,
    nudge_y = 0.05,
    segment.curvature = -0.1,
    segment.ncp = 0.3,
    segment.angle = 20
  ) +
  scale_size_continuous(range = c(3,12))




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#MAKE 2,3,5 LEVEL SIC SECTOR LOOKUP----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#Doesn't matter which one, we just need the SIC lookup
sic <- readRDS('local/data/BRES_NUTS2_2021.rds') %>% filter(MEASURES_NAME=='Value', MEASURE_NAME=='Count', !INDUSTRY_TYPE %in% c('industry','SIC 2007 class (4 digit)')) %>% 
  select(INDUSTRY_NAME, INDUSTRY_CODE, INDUSTRY_TYPE) %>% 
  distinct(INDUSTRY_NAME,.keep_all = T)#In theory, *shouldn't be any the same, codes are in all the names

#The different categories are all long but we should be able to match up by code
unique(sic$INDUSTRY_TYPE)
#SIC 2007 class (4 digit) SIC 2007 division (2 digit)    SIC 2007 group (3 digit) SIC 2007 subclass (5 digit) 
#616                          88                         273                         729 
table(sic$INDUSTRY_TYPE)

#We can try group and division
#Drop 4 digit (adding that on load) break the others into their own dfs, add as merged columns to create lookup
#Add in match columns to the two longer codes while making

two.digit <- sic %>% filter(INDUSTRY_TYPE == 'SIC 2007 division (2 digit)') %>% rename(SIC_2DIGIT_NAME = INDUSTRY_NAME, SIC_2DIGIT_CODE = INDUSTRY_CODE) %>% select(-INDUSTRY_TYPE)

#3 digit, ask to match 2 digit, so keep 2 to match on
three.digit <- sic %>% filter(INDUSTRY_TYPE == 'SIC 2007 group (3 digit)') %>% rename(SIC_3DIGIT_NAME = INDUSTRY_NAME, SIC_3DIGIT_CODE = INDUSTRY_CODE) %>% select(-INDUSTRY_TYPE) %>% 
  mutate(matchcode = substr(SIC_3DIGIT_CODE,start=1,stop=2))

#5 digit, ask to match 3 digit, so keep 3 to match on
five.digit <- sic %>% filter(INDUSTRY_TYPE == 'SIC 2007 subclass (5 digit)') %>% rename(SIC_5DIGIT_NAME = INDUSTRY_NAME, SIC_5DIGIT_CODE = INDUSTRY_CODE) %>% select(-INDUSTRY_TYPE) %>% 
  mutate(matchcode = substr(SIC_5DIGIT_CODE,start=1,stop=3))

#Merge. Start with 5 digit and work down
#This took some staring at
SIClookup <- five.digit %>% 
  left_join(three.digit, by = c('matchcode'='SIC_3DIGIT_CODE')) %>% 
  left_join(two.digit, by = c('matchcode.y' = 'SIC_2DIGIT_CODE')) %>% 
  rename(SIC_3DIGIT_CODE = matchcode, SIC_2DIGIT_CODE = `matchcode.y`) %>% 
  select(SIC_2DIGIT_CODE,SIC_2DIGIT_NAME,SIC_3DIGIT_CODE,SIC_3DIGIT_NAME,SIC_5DIGIT_CODE,SIC_5DIGIT_NAME)

write_csv(SIClookup,'data/SIClookup.csv')




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#DOWNLOAD ALL COUNTRY LEVEL DATA----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#To save processing from lower level data. Quicker.

#DOWNLOAD FROM NOMIS
#2015 to 2021
years = c(2015:2021)

download_all_BRESCOUNTRYopen <- function(year){
  z <- nomis_get_data(id = "NM_189_1", time = as.character(year), geography = "TYPE499")
  saveRDS(z,paste0('local/data/BRES_COUNTRIES_',year,'.rds'))
}

lapply(years, function(x) download_all_BRESCOUNTRYopen(x))


#REPEAT FOR 2009-15 BRES OPEN. ID = NM_172_1
#Has precise same structure as 15-21 (I think... Slightly different RDS size though, maybe something different)
#Slightly different method, but with minimal impact on numbers i.e. < 1%
#See: https://www.nomisweb.co.uk/articles/1062.aspx and https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/bulletins/businessregisterandemploymentsurveybresprovisionalresults/provisionalresults2016revisedresults2015#impact-of-additional-paye-units
years = c(2009:2014)

#Geography type is different as this one includes two type of NUTS2 to choose from
download_all_BRESCOUNTRYopen_earlier <- function(year){
  z <- nomis_get_data(id = "NM_172_1", time = as.character(year), geography = "TYPE499")
  saveRDS(z,paste0('local/data/BRES_COUNTRIES_',year,'.rds'))
}

lapply(years, function(x) download_all_BRESCOUNTRYopen_earlier(x))




#~~~~~~~~~~~~~~~~~~~~~~~~~~~
#LINK COUNTRY LEVEL DATA----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Aiming for change over time, percent wise.
#Nabbing code from above, this is the base one year.

#Let's do one before working out how to get them all and process
# z <- readRDS('local/data/BRES_COUNTRIES_2009.rds')

#GB
#Ah - is that DEFRA category all the agri workers?? Would explain that little mystery
#Down to one row per SIC code for GB
# ft <- z %>% 
#   filter(INDUSTRY_TYPE=='SIC 2007 subclass (5 digit)', EMPLOYMENT_STATUS_NAME=='Full-time employees',
#          INDUSTRY_NAME!="Total", MEASURES_NAME=='Value', MEASURE_NAME=='Count',GEOGRAPHY_NAME=='Great Britain') %>% 
#   select(DATE,INDUSTRY_NAME,INDUSTRY_CODE,COUNT=OBS_VALUE)


#NOTE, THIS IS FULL TIME ONLY
loadallgb_and_reduce <- function(filename){
  
  readRDS(filename) %>% filter(INDUSTRY_TYPE=='SIC 2007 subclass (5 digit)', EMPLOYMENT_STATUS_NAME=='Full-time employees',
         INDUSTRY_NAME!="Total", MEASURES_NAME=='Value', MEASURE_NAME=='Count',GEOGRAPHY_NAME=='Great Britain') %>% 
    select(DATE,INDUSTRY_NAME,INDUSTRY_CODE,COUNT=OBS_VALUE)
  
}

#All years
gb <- list.files(path = "local/data/", pattern = "COUNTR", full.names = T) %>% 
  map(loadallgb_and_reduce) %>% 
  bind_rows()

#One single INDUSTY_NAME field has a non-UTF8 char that breaks some things
#Check for annoying chars and fix
#https://stackoverflow.com/a/17292126
gb$INDUSTRY_NAME <- iconv(gb$INDUSTRY_NAME, "UTF-8", "UTF-8",sub='')

#Save for use elsewhere
saveRDS(gb, 'data/sectors/gb_fulltimeemployeecountandpercent5digitSIC_BRESopen09to21.rds')

gb <- readRDS('data/sectors/gb_fulltimeemployeecountandpercent5digitSIC_BRESopen09to21.rds')


#Add each industry percent for each year
#Ungroup cos plotly doesn't like it
gb <- gb %>% 
  group_by(DATE) %>% 
  mutate(percent = (COUNT/sum(COUNT)*100)) %>% 
  ungroup()


#Reduce to a smaller number of the larger sectors
#Though not too small

#Let's start with largest sectors in the earliest year
#First, just for looking at...
largestsectors <- gb %>% 
  filter(DATE==2021) %>% 
  arrange(percent) %>% 
  select(INDUSTRY_NAME,percent) 


largestsectors <- gb %>% 
  filter(DATE==2009) %>% 
  arrange(percent) %>% 
  select(INDUSTRY_NAME,percent) %>% 
  top_n(30)
  

plot_ly(data = gb %>% filter(INDUSTRY_NAME %in% largestsectors$INDUSTRY_NAME), x = ~DATE, y = ~COUNT, color = ~INDUSTRY_NAME, 
        text = ~paste("Sector:", INDUSTRY_NAME),  # Add this line for hover text
        hoverinfo = 'text+y+x',
        type = 'scatter', mode = 'lines+markers', line = list(shape = 'linear')) %>%
  layout(title = "Yearly percents by SIC", 
         xaxis = list(title = "Year"), 
         # yaxis = list(title = "Value", type='log'),
         yaxis = list(title = "Value"),
         showlegend = F)



#Let's look at sectors with biggest % change between 09 and 21
#Separate df to use as filter to pick out
percentchange <- gb %>% 
  filter(DATE %in% c(2009,2021)) %>% 
  group_by(INDUSTRY_NAME) %>% 
  arrange(INDUSTRY_NAME,DATE) %>% 
  mutate(percentchange = ((lead(COUNT)-COUNT)/COUNT)*100)

#NOTE WHAT I DID WRONG THERE:
#That's percent change in the COUNTS
#i.e. in the nominal amounts of those jobs
#Which is useful but not what I'm after
#I want the percent change in the PERCENTAGE so we can examine change in the STRUCTURE OF INDUSTRY between timepoints
#SEPARATE FROM any underlying change in the number of jobs
#make that clear in any explanations! Thus:
percentchange.in.percent.09to21 <- gb %>% 
  filter(DATE %in% c(2009,2021)) %>% 
  group_by(INDUSTRY_NAME) %>% 
  arrange(INDUSTRY_NAME,DATE) %>% 
  mutate(
    percentchange_in_percent = ((lead(percent)-percent)/percent)*100,
    count2021 = lead(COUNT),#Want to keep these for ref and filtering
    percent2021 = lead(percent)#Want to keep these for ref and filtering
    ) %>% 
  filter(DATE == 2009) %>%#keep row with values in
  select(-DATE) %>% 
  filter(!is.infinite(percentchange_in_percent), !is.nan(percentchange_in_percent)) %>% #Those two are no use
  ungroup()

#Quick deciles for sector counts 2009... actually a pretty reasonable spread
quantile(percentchange.in.percent.09to21$COUNT, probs = seq(.1, .9, by = .1))
quantile(percentchange.in.percent.09to21$percent, probs = seq(.1, .9, by = .1))

#I do not understand this sequenced bracketing!!
#Anyway: the quantile point where we hit sectors that make up 1% or more of the workforce
ecdf(percentchange.in.percent.09to21$percent)(1)

#Which is how many sectors? 16 vs 675
table(percentchange.in.percent.09to21$percent >= 1)

#OK, let's just do a decile 'proportion in each' plot plz
percentchange.in.percent.09to21 <- percentchange.in.percent.09to21 %>% 
  mutate(
    decilerange_percent09 = (cut_number(percent,10)),
    decile_percent09 = as.integer(cut_number(percent,10)),
    ventilerange_percent09 = (cut_number(percent,20)),
    ventile_percent09 = as.integer(cut_number(percent,20))
    )

ggplot(
  percentchange.in.percent.09to21 %>% group_by(decile_percent09) %>% summarise(decilesum = sum(percent)),
  aes(y = factor(decile_percent09), x = decilesum)) +
  geom_bar(stat='identity')



#Top ten % is a about 73 sectors then. But let's dig through all the biggest changers and see...

#So. what we're doing next.
#Going to filter out smaller sectors that don't make up much in terms of numbers
#Though noting two issues here:
#1. Smaller sectors may sum to larger clusters
#2. Trying to work that out from the BRES open data is tricky because of the rounding.


#But let's see what's happening in the larger sectors anyway
#Deciles 7 to 10 is sectors with 17000+ people
x <- gb %>% filter(INDUSTRY_NAME %in% (percentchange.in.percent.09to21 %>% filter(decile_percent09 %in% c(7,8,9,10)) %>% select(INDUSTRY_NAME) %>% pull())) %>% #1. Just looking at top decile
  filter(INDUSTRY_NAME %in% (percentchange.in.percent.09to21 %>% filter(percentchange_in_percent < -20) %>% select(INDUSTRY_NAME) %>% pull()))#industries that saw declining numbers below x%

#Issue with 'increasing' with 09 as basis for filtering % is, they may have increased to become important by 2021 
x <- gb %>% filter(INDUSTRY_NAME %in% (percentchange.in.percent.09to21 %>% filter(decile_percent09 %in% c(7,8,9,10)) %>% select(INDUSTRY_NAME) %>% pull())) %>% #1. Just looking at top decile
  filter(INDUSTRY_NAME %in% (percentchange.in.percent.09to21 %>% filter(percentchange_in_percent > 20) %>% select(INDUSTRY_NAME) %>% pull()))


#Let's also just look at top sectors generally
#Hospital - steady until pandemic then stays high
x <- gb %>% filter(INDUSTRY_NAME %in% (percentchange.in.percent.09to21 %>% filter(decile_percent09 %in% c(10)) %>% select(INDUSTRY_NAME) %>% pull()))

#Working through sector size ventiles
x <- gb %>% filter(INDUSTRY_NAME %in% (percentchange.in.percent.09to21 %>% filter(ventile_percent09 %in% c(19)) %>% select(INDUSTRY_NAME) %>% pull()))

plot_ly(data = x, x = ~DATE, y = ~percent, color = ~INDUSTRY_NAME, 
        text = ~paste("Sector:", INDUSTRY_NAME),  # Add this line for hover text
        hoverinfo = 'text+y+x',
        type = 'scatter', mode = 'lines+markers', line = list(shape = 'linear')) %>%
  layout(title = "Yearly percents by SIC", 
         xaxis = list(title = "Year"), 
         yaxis = list(title = "Value", type='log'),
         # yaxis = list(title = "Value"),
         showlegend = F)

#Moving average version to look for trends
#https://stackoverflow.com/questions/26198551/rolling-mean-moving-average-by-group-id-with-dplyr
y <- x %>% 
  group_by(INDUSTRY_NAME) %>% 
  arrange(DATE) %>% 
  mutate(movingav = rollapply(percent,3,mean,align='right',fill=NA))


plot_ly(data = y, x = ~DATE, y = ~movingav, color = ~INDUSTRY_NAME, 
        text = ~paste("Sector:", INDUSTRY_NAME),  # Add this line for hover text
        hoverinfo = 'text+y+x',
        type = 'scatter', mode = 'lines+markers', line = list(shape = 'linear')) %>%
  layout(title = "Yearly percents by SIC", 
         xaxis = list(title = "Year"), 
         yaxis = list(title = "Value", type='log'),
         # yaxis = list(title = "Value"),
         showlegend = F)



#Cluster by change over time.
#Just use plain ol' OLS applied to each sector time series
#https://stackoverflow.com/a/71944195
#Use slope estimate to group to view
l <- gb %>%
  mutate(percentlog = ifelse(percent>0, percent,0.000000001)) %>% #to deal with log 0s
  group_by(INDUSTRY_NAME) %>%
  group_modify(
    # ~ broom::tidy(lm(percentlog ~ DATE, data = .))#That made no difference!
    ~ broom::tidy(lm(percent ~ DATE, data = .))
  ) %>% 
  filter(term!='(Intercept)') %>% 
  arrange(-estimate) %>% #arrange not necessary to cut, but want to view
  ungroup() %>% 
  mutate(group = as.integer(cut_number(estimate,20)))

  
x <- gb %>% filter(INDUSTRY_NAME %in% (l %>% filter(group %in% c(1)) %>% select(INDUSTRY_NAME) %>% pull()))

y <- x %>% 
  group_by(INDUSTRY_NAME) %>% 
  arrange(DATE) %>% 
  mutate(movingav = rollapply(percent,3,mean,align='right',fill=NA))

plot_ly(data = y, x = ~DATE, y = ~movingav, color = ~INDUSTRY_NAME, 
        text = ~paste("Sector:", INDUSTRY_NAME),  # Add this line for hover text
        hoverinfo = 'text+y+x',
        type = 'scatter', mode = 'lines+markers', line = list(shape = 'linear')) %>%
  layout(title = "Yearly percents by SIC", 
         xaxis = list(title = "Year"), 
         yaxis = list(title = "Value", type='log'),
         # yaxis = list(title = "Value"),
         showlegend = F)




#Check other methods for picking out largest vs smallest change
#Use SD. Can combine 'largest change' with polarity of lm to filter down further?
#Log works better here
sds <- gb %>%
  group_by(INDUSTRY_NAME) %>%
  summarise(sd = sd(log(percent))) %>%
  # summarise(sd = sd(percent)) %>% 
  ungroup() %>% 
  mutate(group = as.integer(cut_number(sd,20)))

x <- gb %>% filter(INDUSTRY_NAME %in% (sds %>% filter(group %in% c(1)) %>% select(INDUSTRY_NAME) %>% pull()))

#Big SD and polarity of move

#In the biggest changers over time via SD we have...
#Climbers
x <- gb %>% filter(INDUSTRY_NAME %in% (sds %>% filter(group %in% c(20)) %>% select(INDUSTRY_NAME) %>% pull())) %>% 
  filter(INDUSTRY_NAME %in% (l %>% filter(estimate > 0) %>% select(INDUSTRY_NAME) %>% pull()))

#Droppers
x <- gb %>% filter(INDUSTRY_NAME %in% (sds %>% filter(group %in% c(20)) %>% select(INDUSTRY_NAME) %>% pull())) %>% 
  filter(INDUSTRY_NAME %in% (l %>% filter(estimate <= 0) %>% select(INDUSTRY_NAME) %>% pull()))


#For sectors with lowest SD, just curious...
#Climbers
x <- gb %>% filter(INDUSTRY_NAME %in% (sds %>% filter(group %in% c(1)) %>% select(INDUSTRY_NAME) %>% pull())) %>% 
  filter(INDUSTRY_NAME %in% (l %>% filter(estimate > 0) %>% select(INDUSTRY_NAME) %>% pull()))

#Droppers
x <- gb %>% filter(INDUSTRY_NAME %in% (sds %>% filter(group %in% c(1)) %>% select(INDUSTRY_NAME) %>% pull())) %>% 
  filter(INDUSTRY_NAME %in% (l %>% filter(estimate <= 0) %>% select(INDUSTRY_NAME) %>% pull()))



#Test using sum of diffs to pick out change bands
diffchange <- gb %>%
  group_by(INDUSTRY_NAME) %>%
  arrange(DATE) %>% 
  mutate(logdiff = log(percent)-lag(log(percent))) %>%
  summarise(difftotal = sum(logdiff,na.rm = T)) %>% 
  ungroup() %>% 
  mutate(group = as.integer(cut_number(difftotal,21)))

#Climbers
x <- gb %>% filter(INDUSTRY_NAME %in% (diffchange %>% filter(group %in% c(21)) %>% select(INDUSTRY_NAME) %>% pull())) 
#Droppers
x <- gb %>% filter(INDUSTRY_NAME %in% (diffchange %>% filter(group %in% c(1)) %>% select(INDUSTRY_NAME) %>% pull()))
#In the middle
x <- gb %>% filter(INDUSTRY_NAME %in% (diffchange %>% filter(group %in% c(11)) %>% select(INDUSTRY_NAME) %>% pull()))


#NOTE: THESE ARE PERCENTAGES RELATIVE TO UK WHOLE, SO DROPS CAN BE DUE TO OTHER SECTORS GROWING
#COuld do with also showing raw diff
y <- x %>% 
  group_by(INDUSTRY_NAME) %>% 
  arrange(DATE) %>% 
  mutate(
    movingav = rollapply(percent,3,mean,align='right',fill=NA),
    count_movingav = round(rollapply(COUNT,3,mean,align='right',fill=NA),0)#Have a count moving average too, so it matches the percent (so count orders are correct vertically)
    )

plot_ly(data = y, x = ~DATE, y = ~movingav, color = ~INDUSTRY_NAME, 
        text = ~paste("Sector:", INDUSTRY_NAME, "\nPEOPLE: ",count_movingav),  # Add this line for hover text
        hoverinfo = 'text',
        type = 'scatter', mode = 'lines+markers', line = list(shape = 'linear')) %>%
  layout(title = "Yearly percents by SIC", 
         xaxis = list(title = "Year"), 
         yaxis = list(title = "Value", type='log'),
         # yaxis = list(title = "Value"),
         showlegend = F)


#Non-moving av...
plot_ly(data = x, x = ~DATE, y = ~percent, color = ~INDUSTRY_NAME, 
        text = ~paste("Sector:", INDUSTRY_NAME, "\nPEOPLE: ",COUNT),  # Add this line for hover text
        hoverinfo = 'text',
        type = 'scatter', mode = 'lines+markers', line = list(shape = 'linear')) %>%
  layout(title = "Yearly percents by SIC", 
         xaxis = list(title = "Year"), 
         yaxis = list(title = "Value", type='log'),
         # yaxis = list(title = "Value"),
         showlegend = F)





# chk <- itl2.lq %>% 
#   filter(GEOGRAPHY_NAME=='South Yorkshire', DATE ==2021,
#          grepl('education', SIC_2DIGIT_NAME, ignore.case=T))
# 
# chk %>% 
#   group_by()
