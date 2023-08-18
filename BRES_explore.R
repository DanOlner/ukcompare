#BRES explore
library(tidyverse)
library(zoo)
library(sf)
library(tmap)
library(plotly)
library(magick)
library(nomisr)
options(scipen = 99)

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

y %>% tidyr::unnest(annotations.annotation) %>% glimpse()
y %>% tidyr::unnest(components.attribute) %>% glimpse()
y %>% tidyr::unnest(components.dimension) %>% glimpse()
y %>% tidyr::unnest(components.primarymeasure.conceptref) %>% glimpse()

y %>% tidyr::unnest(annotations.annotation) %>% View
y %>% tidyr::unnest(components.attribute) %>% View
y %>% tidyr::unnest(components.dimension) %>% View
y %>% tidyr::unnest(components.primarymeasure.conceptref) %>% View

q <- nomis_overview("NM_189_1")
q %>% tidyr::unnest(name) %>% View



a <- nomis_get_metadata(id = "NM_189_1")

#Returns values of the lowest indexed type available (so we're getting the countries here)
b <- nomis_get_metadata(id = "NM_189_1", concept = "GEOGRAPHY")

#Gives us all the geography types
c <- nomis_get_metadata(id = "NM_189_1", concept = "geography", type = "type")

#"Passing a specific type to the type parameter, in this case “TYPE460” for all post-2010 parliamentary constituencies, returns a tibble with geographic codes for those specific constituencies, which can be used to filter queries"
#So e.g. if I want LSOAs? Hmm, that's a lot of them isn't it?
#OK, so that just gave us a list of all the LSOA names and a possible ID if we want to filter down by that. This can be lookup for that if need be.
d <- nomis_get_metadata(id = "NM_189_1", concept = "geography", type = "TYPE298")

#Presumably we repeat the same pattern for other data types to narrow down?
















