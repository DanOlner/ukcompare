#Look at UK housebuilding rates at local authority level
library(tidyverse)
library(zoo)
library(sf)
library(tmap)
library(plotly)
library(nomisr)
library(pryr)
source('functions/misc_functions.R')

#READ + PROCESS DATA----

url1 <- 'https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/housing/datasets/housebuildingukpermanentdwellingsstartedandcompletedbylocalauthority/financialyearendingmarch2024/uklocalauthorityhousebuilding.xlsx'
p1f <- tempfile(fileext=".xlsx")
download.file(url1, p1f, mode="wb")

#Table 2c is current prices
#Dates are financial year
completions <- readxl::read_excel(path = p1f,range = "UK_Completions!B6:S367") 

#NAs are text e.g. "[x]" or "[low"], so need to process those as NAs then reformat entire cols as numeric
# completions[completions == "[x]"] <- NA
# completions[completions == "[low]"] <- NA

#All in one method that converts anything non-numeric to NA: https://www.geeksforgeeks.org/convert-multiple-columns-to-numeric-using-dplyr/
#NORTHERN IRELAND, NO DATA UNTIL 2015-16
completions <- completions %>%
  mutate(across(`2009-2010`:`2023-2024`, ~ {
    as.numeric(tryCatch(., error = function(e) NA))
  }))


#Year is "start of financial year" but we just need year that starts
#Then make long
completions <- completions %>%
  rename_with(~ sub("-.*", "", .), starts_with("20")) %>% 
  pivot_longer(`2009`:`2023`, names_to = 'financial_year', values_to = 'dwelling_count') %>% 
  mutate(financial_year = as.numeric(financial_year))



#GET POPULATION DENOMS from Annual Population Survey----

#List of APS variables.... nearly 4000
#404226560 = All residents
varz <- nomis_get_metadata(id = "NM_17_1", concept = "CELL")

#List of geographies
nomis_get_metadata(id = "NM_17_1", concept = "GEOGRAPHY", type = "type") %>% print(n=60)


#Get local authorities - probably have to get both county and district to match all?
#Results:
residentpop1 <- nomis_get_data(id = "NM_17_1", geography = "TYPE423", cell = "404226560")
residentpop2 <- nomis_get_data(id = "NM_17_1", geography = "TYPE424", cell = "404226560")

#Check local authority matches
# table(unique(completions$`Local Authority Code`) %in% residentpop1$GEOGRAPHY_CODE)

#Second LA list in residentpop2 is actually near total match
table(unique(completions$`Local Authority Code`) %in% residentpop2$GEOGRAPHY_CODE)
table(unique(completions$`Local Authority Code`) %in% c(residentpop1$GEOGRAPHY_CODE,residentpop2$GEOGRAPHY_CODE))

#Check on non-matches... all NI
unique(completions$`Local Authority Name`)[!unique(completions$`Local Authority Code`) %in% residentpop2$GEOGRAPHY_CODE]
# unique(residentpop2$GEOGRAPHY_NAME)[!unique(residentpop2$GEOGRAPHY_CODE) %in% unique(completions$`Local Authority Code`)]

#Ah yes, have to get NI geography separately and append
#Let's just do GB comparison here, that should be fine

#Format... (nabbed from Beattyfothergill_update)
residentpop2 <- residentpop2 %>% 
  filter(
    grepl('Jan', DATE_NAME)#keep only one of the quarterly vals (check if should be smoothing for better, but I think that's already done in APS)
  ) %>% 
  mutate(DATE = str_sub(DATE, start = 1, end = 4) %>% as.numeric) %>% 
  select(
    DATE,GEOGRAPHY_NAME,GEOGRAPHY_CODE,MEASURES_NAME,OBS_VALUE
  ) %>% 
  pivot_wider(
    names_from = MEASURES_NAME, values_from = OBS_VALUE 
  ) 

residentpop2 <- residentpop2 %>% 
  rename(residentpop_count = Value, residentpop_CI = Confidence)

#completions - years are 2009 to 2023
unique(residentpop2$DATE)#2004 to 2023



#Merge (losing some LAs from NI in the completion data in the process)
#Check merge first
# chk <- completions %>% 
#   right_join(
#     residentpop2 %>% select(-GEOGRAPHY_NAME) %>% filter(DATE >= 2009),
#     by = c('financial_year' = 'DATE', 'Local Authority Code' = 'GEOGRAPHY_CODE')
#   )
  
#Check that the missing places are NI... tick
# unique(completions$`Local Authority Name`)[!unique(completions$`Local Authority Code`) %in% unique(chk$`Local Authority Code`)]

#Overwrite/keep
completions <- completions %>% 
  right_join(
    residentpop2 %>% select(-GEOGRAPHY_NAME) %>% filter(DATE >= 2009),
    by = c('financial_year' = 'DATE', 'Local Authority Code' = 'GEOGRAPHY_CODE')
  )

#Get completion rates per 1000 people
completions <- completions %>% 
  mutate(
    completions_per1000residents = (dwelling_count / residentpop_count) * 1000,
    SY_localauthority = if_else(qg('sheffield|rotherh|barns|doncaster',`Local Authority Name`),'SY LA','other')
  )


#Compare to GB total completed dwellings per 1000 pop
totals <- completions %>% 
  group_by(financial_year) %>% 
  summarise(
    GB_residents = sum(residentpop_count, na.rm = T),
    GB_dwellingcount = sum(dwelling_count, na.rm = T)
  ) %>% 
  mutate(
    GB_completions_per1000residents = (GB_dwellingcount / GB_residents) * 1000,
  )


#Merge into main
completions <- completions %>% 
  left_join(
    totals %>% select(financial_year,GB_completions_per1000residents), by = 'financial_year'
  ) %>% 
  mutate(
    # percentdiff = ((GB_completions_per1000residents - completions_per1000residents)/GB_completions_per1000residents)*100
    percentofGBav = (completions_per1000residents/GB_completions_per1000residents)*100
  )



#Rank position change could also be useful
completions <- completions %>% 
  group_by(financial_year) %>%
  mutate(
    rank = rank(-completions_per1000residents)
    )


#Save for viewing, put SY places at top
#Last few years to make viewable in github
write_csv(completions %>% 
            arrange(SY_localauthority) %>% 
            filter(financial_year > 2018),
          'data/dwelling_completions_localauthorities.csv'
          )


# EXAMINE SY LOCAL AUTHORITIES' BUILDING RATES----

#Quick plots...
#Messy, probably needs smoothing
#But let's just check SY LAs vs elsewhere first...
ggplot() +
  geom_line(
    data = completions, 
    aes(x = financial_year, y = completions_per1000residents, group = `Local Authority Name`),
    alpha = 0.25, size = 0.25
    ) +
  geom_line(
    data = completions %>% filter(SY_localauthority == 'SY LA'), 
    aes(x = financial_year, y = completions_per1000residents, colour = `Local Authority Name`, group = `Local Authority Name`),
    size = 2
  ) + 
  coord_cartesian(ylim = c(0,15))



#Use percent of GB av...
ggplot() +
  geom_line(
    data = completions %>% filter(SY_localauthority == 'SY LA'), 
    aes(x = financial_year, y = percentofGBav, colour = `Local Authority Name`, group = `Local Authority Name`)
    ) +
  geom_hline(yintercept = 100)


#What does that look like for everywhere?
ggplot() +
  geom_line(
    data = completions, 
    aes(x = financial_year, y = percentofGBav, group = `Local Authority Name`),
    alpha = 0.25, size = 0.25
  ) +
  geom_line(
    data = completions %>% filter(SY_localauthority == 'SY LA'), 
    aes(x = financial_year, y = percentofGBav, colour = `Local Authority Name`, group = `Local Authority Name`),
    size = 2
  ) +
  geom_hline(yintercept = 100) +
  coord_cartesian(ylim = c(0,300))



#Use rank position
ggplot() +
  geom_line(
    data = completions %>% filter(SY_localauthority == 'SY LA'), 
    aes(x = financial_year, y = rank, colour = `Local Authority Name`, group = `Local Authority Name`)
  ) +
  ylab("Rank of dwellings completed per resident (1 is highest rate, 350 lowest)")



#Combine those three for single view
allz <- completions %>% 
  select(-GB_completions_per1000residents) %>% 
  rename(rank_dwellings_per_resident_1ishighest_350lowest = rank) %>% 
  pivot_longer(completions_per1000residents:rank_dwellings_per_resident_1ishighest_350lowest, names_to = 'measure', values_to = 'value'
  )


# ggplot() +
#   geom_line(
#     data = allz %>% filter(SY_localauthority == 'SY LA'), 
#     aes(x = financial_year, y = value, colour = `Local Authority Name`, group = `Local Authority Name`)) +
#   facet_wrap(~measure, scales = 'free_y', ncol = 1)

p <- ggplot(allz %>% filter(SY_localauthority == 'SY LA'), 
       aes(x = financial_year, y = value, colour = `Local Authority Name`, group = `Local Authority Name`)) +
  # geom_point() +
  geom_line() +
  facet_wrap(~measure, scales = 'free_y', ncol = 1)

ggplotly(p, tooltip = 'value')










