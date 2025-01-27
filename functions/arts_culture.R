#Arts / culture
#Drawing data from here: 
#https://www.artscouncil.org.uk/your-area/culture-and-place-data-explorer
library(tidyverse)
library(zoo)
library(sf)
library(tmap)
library(plotly)
library(nomisr)
library(pryr)
source('functions/misc_functions.R')


#Look at Arts council funding per local authority 23/34 (exported via dashboard)
# ac24 <- read_csv('data/arts_council_funding_23_24_localauthorities.csv')

# PROCESS ARTS COUNCIL DATA----

ac <- read_csv('data/artscouncilfunding_overtime_localauthorities.csv') %>% 
  slice(-c(1:3))

#Get rid of all pound symbols prior to turning into numeric
#https://stackoverflow.com/a/48890726/5023561
ac <- map_df(ac, ~ gsub("£", "", .x))

#Rename columns
#https://stackoverflow.com/a/8613332/5023561
#Get text in brackets
names(ac)[3:ncol(ac)] <- regmatches(names(ac)[3:ncol(ac)], gregexpr("(?<=\\().*?(?=\\))", names(ac)[3:ncol(ac)], perl=T)) %>% unlist
#keep first four chars
names(ac)[3:ncol(ac)] <- str_sub(names(ac)[3:ncol(ac)], start = 1, end = 4)

#Then to numeric for those cols
ac <- ac %>% mutate(across(`2023`:`2018`, ~as.numeric(.)))

#Aaand make long by year, for joining
ac <- ac %>% 
  pivot_longer(
    cols = `2023`:`2018`, names_to = 'DATE', values_to = 'total spend (pounds)'
  ) %>% 
  mutate(DATE = as.numeric(DATE))



# GET AND JOIN TO RESIDENT POPULATION----

#Pulled out of NOMIS at start of uk_housebuildingrates_localauthorities... R script
residentpop2 <- readRDS('local/data/APS_residentpop_upto2023.rds')


#check local authority name match...
table(unique(ac$Area)[4:452] %in% unique(residentpop2$GEOGRAPHY_NAME))

#Check missing ones from resident pop are not in England...
#Tick, Wales and Scotland
unique(residentpop2$GEOGRAPHY_NAME)[!(unique(residentpop2$GEOGRAPHY_NAME)) %in% unique(ac$Area)[4:452]]

#Keep matches
ac.wpop <- ac %>% 
  inner_join(
    residentpop2 %>% select(DATE, GEOGRAPHY_NAME, residentpop_count),
    by = c('Area' = 'GEOGRAPHY_NAME','DATE')
  ) %>% 
  mutate(
    totalspend_perperson_pounds = (`total spend (pounds)`/residentpop_count),
    SY_localauthority = if_else(qg('sheffield|rotherh|barns|doncaster',Area),'SY LA','other'),
    corecity = qg('sheffield|Belfast|Birmingham|Bristol|Cardiff|Glasgow|Leeds|Liverpool|Manchester|upon Tyne|Nottingham',Area)
    )



#Get England totals and add, for working out distance to av
#Compare to GB total completed dwellings per 1000 pop
totals <- ac.wpop %>% 
  group_by(DATE) %>% 
  summarise(
    GB_residents = sum(residentpop_count, na.rm = T),
    GB_totalspend = sum(`total spend (pounds)`, na.rm = T)
  ) %>% 
  mutate(
    GB_totalspend_perperson_pounds = (GB_totalspend / GB_residents),
  )


#Merge into main
ac.wpop <- ac.wpop %>% 
  left_join(
    totals %>% select(DATE,GB_totalspend_perperson_pounds), by = 'DATE'
  ) %>% 
  mutate(
    # percentdiff = ((GB_completions_per1000residents - completions_per1000residents)/GB_completions_per1000residents)*100
    percentofGBav = (totalspend_perperson_pounds/GB_totalspend_perperson_pounds)*100
  )






# QUICK PLOTS----

p <- ggplot() +
  geom_line(
    data = ac.wpop, 
    aes(x = DATE, y = totalspend_perperson_pounds, group = Area),
    alpha = 0.25, size = 0.25
  ) +
  geom_line(
    data = ac.wpop %>% filter(SY_localauthority == 'SY LA'), 
    aes(x = DATE, y = totalspend_perperson_pounds, colour = Area, group = Area),
    size = 2
  )  +
  coord_cartesian(ylim = c(0,100))


ggplotly(p, tooltip = c('Area','totalspend_perperson_pounds'))




#Percent of Eng av
p <- ggplot() +
  geom_line(
    data = ac.wpop %>% filter(SY_localauthority == 'SY LA'), 
    aes(x = DATE, y = percentofGBav, colour = Area, group = Area),
    size = 2
  )


#Core cities % of Eng av
p <- ggplot() +
  geom_line(
    data = ac.wpop %>% filter(corecity), 
    aes(x = DATE, y = percentofGBav, colour = Area, group = Area),
    size = 2
  ) +
  scale_color_brewer(palette = 'Paired', direction = -1) +
  ylab("Percent of England average")


ggplotly(p, tooltip = c('Area','percentofGBav'))






