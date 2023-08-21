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

#So, what's the ID for BRES open?
#Noting I can probably only use the API for open data, not safeguarded BRES values...
atts %>% filter(name.value == "Business Register and Employment Survey : open access")


#BRES open ID
y <- nomis_data_info("NM_189_1")
glimpse(y)

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

loadallITL2_fulltime_and_reduce <- function(filename){
  
  readRDS(filename) %>% filter(INDUSTRY_TYPE=='SIC 2007 subclass (5 digit)', EMPLOYMENT_STATUS_NAME=='Full-time employees',
                               INDUSTRY_NAME!="Total", MEASURES_NAME=='Value', MEASURE_NAME=='Count') %>% 
    select(DATE,GEOGRAPHY_NAME,GEOGRAPHY_CODE,INDUSTRY_NAME,INDUSTRY_CODE,COUNT=OBS_VALUE)
  
}

#All years
itl2 <- list.files(path = "local/data/", pattern = "NUTS2", full.names = T) %>% 
  map(loadallITL2_fulltime_and_reduce) %>% 
  bind_rows() %>% 
  filter(DATE > 2014)

#One single INDUSTY_NAME field has a non-UTF8 char that breaks some things
#Check for annoying chars and fix
#https://stackoverflow.com/a/17292126
itl2$INDUSTRY_NAME <- iconv(itl2$INDUSTRY_NAME, "UTF-8", "UTF-8",sub='')

#Save for use elsewhere
saveRDS(itl2, 'data/sectors/ITL2_fulltimeemployeecountandpercent5digitSIC_BRESopen15to21.rds')




#~~~~~~~~~~~~~~~~~~~~~~~~~~~
#ITL2 LOCATION QUOTIENTS----
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


#1. Find regional proportion of sector: sector x as proportion of all sectors in that region
#2. Find proportion of same sector for the UK as a whole (sum sector total, sum entire UK total, find proportion)
#3. LQ is relative proportion - 1 / 2. 
#4. < 1 = region has less concentration than nationally. > 1 = concentration is higher than nationally.

#For regional proportion calc, Add values for calc steps into the same DF
itl2.lq <- itl2 %>%
  group_by(GEOGRAPHY_NAME, DATE) %>% 
  mutate(
    region_totalsize = sum(COUNT),#a. Current employment per region per year, for regional denominator
    sector_regional_proportion = COUNT / region_totalsize#b. regional sector proportion.
  ) %>% 
  group_by(DATE, INDUSTRY_NAME) %>% 
  mutate(
    uk_sectorsize = sum(COUNT),#c. Summed current prices for EACH SECTOR, UK-wide
  ) %>% 
  group_by(DATE) %>% 
  mutate(
    uk_totalsize = sum(COUNT),#d. Summed current prices for WHOLE UK per year, for UK denominator
    uk_regional_proportion = uk_sectorsize / uk_totalsize#e. UK-level sector proportion
  ) %>% 
  mutate(
    LQ = sector_regional_proportion / uk_regional_proportion#f. Location quotient!
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


#Argument for noting those and removing from the analysis given that LQ of 0 has a meaning.
#Actually let's just do that now and note for later
itl2.lq <- itl2.lq %>% 
  filter(!INDUSTRY_NAME %in% chk$INDUSTRY_NAME)




##INITIAL LOOK AT BRES LQ----

itl2.lq <- itl2.lq %>% mutate(LQ_log = log(LQ)) 

#View for 2021. Can just flag one we want to overlay?
#Something odd with water and air transport, remove until work out what
place = 'South Yorkshire'

x <- itl2.lq %>% filter(DATE == 2015) %>% mutate(flaggedplace = GEOGRAPHY_NAME==place)
x <- itl2.lq %>% filter(DATE == 2021) %>% mutate(flaggedplace = GEOGRAPHY_NAME==place)

#Ordering by the flagged place, bit awkward
x$INDUSTRY_NAME <- factor(x$INDUSTRY_NAME)
x$INDUSTRY_NAME <- fct_relevel(
  x$INDUSTRY_NAME, 
  unique(as.character(x$INDUSTRY_NAME))[order(x %>% filter(GEOGRAPHY_NAME==place) %>%ungroup() %>% select(LQplusone_log) %>% pull(),decreasing = T)]
)

#Actually, keep that order and use for animation below
# ordertouse <- unique(as.character(x$`SIC07 description`))[order(x %>% filter(`ITL region name`==place) %>%ungroup() %>% select(LQplusone_log) %>% pull(),decreasing = T)]


#Filter down a bit more to try and see
#Going to need abbreviated industry names too

#List of sectors where LQ = certain value for the named place
# sectors.to.view <- x %>% ungroup() %>% filter(flaggedplace == T, LQplusone_log == 0) %>% select(INDUSTRY_NAME) %>% pull() %>% as.character()
sectors.to.view <- x %>% ungroup() %>% filter(flaggedplace == T, LQ > 2) %>% select(INDUSTRY_NAME) %>% pull() %>% as.character()

ggplot(
  x %>% filter(INDUSTRY_NAME %in% sectors.to.view),
  # x,
  aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace)
  # aes(y = INDUSTRY_NAME, x = LQ_log, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace)
) +
  geom_point() +
  scale_x_continuous(trans = "log10") +
  scale_size_manual(values = c(2,4)) +
  scale_alpha_manual(values = c(0.2,1)) +
  scale_colour_manual(values = c('black','red')) +
  geom_vline(xintercept = 1, colour = 'blue') 
# +
#   theme(axis.text.y=element_blank(), #remove x axis labels
#         axis.ticks.y=element_blank())


unique(itl2.lq$GEOGRAPHY_NAME)


#SY over time
sy <- itl2.lq %>% filter(GEOGRAPHY_NAME == 'South Yorkshire') %>% ungroup()
sy <- itl2.lq %>% filter(GEOGRAPHY_NAME == 'Greater Manchester') %>% ungroup()

#MOVING AVERAGE
#https://stackoverflow.com/questions/26198551/rolling-mean-moving-average-by-group-id-with-dplyr
sy <- sy %>% 
  group_by(INDUSTRY_NAME) %>% 
  mutate(movingav = rollapply(LQ,3,mean,align='right',fill=NA))

# plot_ly(data = sy %>% filter(INDUSTRY_NAME %in% sectors.to.view), x = ~DATE, y = ~LQplusone_log, color = ~INDUSTRY_NAME,
plot_ly(data = sy %>% filter(INDUSTRY_NAME %in% sectors.to.view), x = ~DATE, y = ~movingav, color = ~INDUSTRY_NAME,
        text = ~paste("Sector:", INDUSTRY_NAME),  # Add this line for hover text
        hoverinfo = 'text+y+x',
        type = 'scatter', mode = 'lines+markers', line = list(shape = 'linear')) %>%
  layout(title = "Yearly values by SIC", 
         xaxis = list(title = "Year"),
         # yaxis = list(title = "Value", type='log'),
         yaxis = list(title = "Value"),
         showlegend = F)



#Location quotient vs proportion in the region (already calculated) seems obvious...
# plot_ly(data = sy %>% filter(DATE==2015), x = ~LQ, y = ~sector_regional_proportion, color = ~INDUSTRY_NAME,
plot_ly(data = sy %>% filter(DATE==2015), x = ~LQ, y = ~sector_regional_proportion,
        text = ~paste("Sector:", INDUSTRY_NAME, ", percent: ", sector_regional_proportion * 100, ", count: ", COUNT),  # Add this line for hover text
        hoverinfo = 'text+y+x',
        type = 'scatter') %>%
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


#Test: get 3 or 4 digit SICs, use to colour clusters, see if those are the clusters that matter
#Other ways to cluster 5 digit possibly available based on LQ
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


#Add each industry percent for each year
#Ungroup cos plotly doesn't like it
gb <- gb %>% 
  group_by(DATE) %>% 
  mutate(percent = (COUNT/sum(COUNT)*100)) %>% 
  ungroup()


#Reduce to a smaller number of the larger sectors
#Though not too small

#Let's start with largest sectors in the earliest year
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
x <- gb %>% filter(INDUSTRY_NAME %in% (percentchange.in.percent.09to21 %>% filter(ventile_percent09 %in% c(17)) %>% select(INDUSTRY_NAME) %>% pull()))

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









