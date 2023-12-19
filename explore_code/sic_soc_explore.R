#Annual population survey SOC job data linking to SIC to get skills / sector breakdowns and comparisons
#For UoS laptop
#.libPaths('C:/Users/mg1dol/Documents/templibpaths')
#Regional GVA per sector explore
library(tidyverse)
library(zoo)
library(sf)
library(tmap)
library(plotly)
#library(magick)
library(cowplot)
library(nomisr)
source('functions/misc_functions.R')
options(scipen = 99)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1. GET SOC DATA VIA API----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Finding the right dataset first.
x <- nomis_data_info()
glimpse(x)

#Looking for APS Employment by occupation... hmm, that's SOC2010.
unique(x$name.value)[grepl(x = unique(x$name.value), pattern = 'Employment by occupation', ignore.case = T)]

#Search
a <- nomis_search(name = '*occupation*')
atts <- a %>% tidyr::unnest(components.attribute) %>% glimpse()
unique(atts$name.value)

#Still not there. Hmm.
a <- nomis_search(name = '*survey*')
atts <- a %>% tidyr::unnest(components.attribute) %>% glimpse()
unique(atts$name.value)

#reduce to aps
unique(atts$name.value)[grepl(x = unique(atts$name.value), pattern = 'annual population', ignore.case = T)]

#OK, I think it's likely to be a cell in the top level APS which is...
y <- nomis_data_info("NM_17_1")
glimpse(y)

y %>% tidyr::unnest(components.dimension) %>% View
#"GEOGRAPHY" "CELL"      "MEASURES"  "FREQ" 
#Yup, the table to we want should be in cell. How to get to it?
y %>% tidyr::unnest(components.dimension) %>% select(conceptref) %>% pull()




a <- nomis_get_metadata(id = "NM_17_1")

#Returns values of the lowest indexed type available (so we're getting the countries here)
nomis_get_metadata(id = "NM_17_1", concept = "GEOGRAPHY")

q <- nomis_overview("NM_17_1")
q %>% tidyr::unnest(name) %>% glimpse()


#passing generic "type" string to type will give all the categories for that dimension
#NUTS2 2016, equiv to ITL2 = TYPE438 
#Note, bres_explore 189 on checks NUTS2 against ITL2, needs small update on a couple of names but matches
print(nomis_get_metadata(id = "NM_17_1", concept = "geography", type = "type"), n = 60)
#Test for a single geography. SY code: 1837105162
print(nomis_get_metadata(id = "NM_17_1", concept = "geography", type = "TYPE438"), n = 60)



#And for the rest?
print(nomis_get_metadata(id = "NM_17_1", concept = "CELL"), n = 50)
#ID A = annually
nomis_get_metadata(id = "NM_17_1", concept = "FREQ")
#We want both value and confidence, though wonder how confidence will appear
nomis_get_metadata(id = "NM_17_1", concept = "MEASURES")

#Around 4000 for cell, need to search...
cell <- nomis_get_metadata(id = "NM_17_1", concept = "CELL")
#This is getting closer
cellsub <- cell %>% filter(grepl('T10', label.en, ignore.case = T)) 

#OK, so from cellsub, can see the range of ids we're after there are
#403308801 to 403311104
#Ah no, it's not sequential. Let's nab those codes for shortly downloading...
#Up to row 90
codes <- cellsub$id[1:90]

#NOPE, THAT'S THE OLD SIC CODES!
#SIC 2007 starts at 404357377 (and then presumably has 90 entries for the full lot)
#That's index 271
#So should end index 360?
# codes <- cellsub$id[271:360]

#Except actual SIC07 doesn't seem to have any values/obs.

#If we're after all people
#Numbers too low for that one, let's try...
z <- nomis_get_data(id = "NM_17_1", time = "latest", geography = "TYPE438", cell = "403308801")

z <- nomis_get_data(id = "NM_17_1", time = "latest", geography = "TYPE438", cell = codes) %>%
  select(DATE_NAME,GEOGRAPHY_NAME,GEOGRAPHY_CODE,CELL_NAME,MEASURES_NAME,OBS_VALUE,OBS_STATUS,OBS_STATUS_NAME)
# 
# #Avoid repeat downloads!
# z <- z_download

#Repeat name place checks... 40, correct number
unique(z$GEOGRAPHY_NAME)


#Some column processing. 
#1. remove gumph


#Test
#https://stackoverflow.com/a/12297934
# gsub(x = z$CELL_NAME, pattern = ".*- ","", replacement = '')
# gsub(x = z$CELL_NAME, pattern = " )", replacement = '')
# gsub(x = z$CELL_NAME, pattern = " \\(SOC2020\\)| \\(SOC 2020\\)", replacement = '')

z <- z %>% 
  mutate(
    CELL_NAME = gsub(x = CELL_NAME, pattern = ".*- ","", replacement = ''),
    CELL_NAME = gsub(x = CELL_NAME, pattern = " \\(SOC2020\\)| \\(SOC 2020\\)", replacement = ''),
    CELL_NAME = gsub(x = CELL_NAME, pattern = " )", replacement = ''),
    CELL_NAME = gsub(x = CELL_NAME, pattern = " \\(SIC 2007\\)", replacement = '')
  )

#2. Split SOC and SIC into their own columns
z <- z %>% separate_wider_delim(CELL_NAME, delim = " : ", names = c("SOC2020", "SIC2007"))

#How many unique sectors? 
#Exactly the same as SIC2003 - data is identical I think
unique(z$SIC2007)

saveRDS(z, 'data/NUTS2_2016_latestAPS_SIC2007_SOC2020.rds')


#Check if older version before NOMIS fixed had different data...
#Ah, overwrote with wrong data. Hmmp.
older <- readRDS('data/OLD__NUTS2_2016_latestAPS_SIC2007_SOC2020.rds')



#Might have been downloading the wrong thing. Looking back at the nomis_search...
#Nope, doesn't have the right geographies
print(nomis_get_metadata(id = "NM_218_1", concept = "geography", type = "type"), n = 60)

cell <- nomis_get_metadata(id = "NM_218_1")


z <- read_csv("https://www.nomisweb.co.uk/api/v01/dataset/NM_17_1.data.csv?geography=1837105153...1837105192&date=latest&cell=403308801...403308810,403309057...403309066,403309313...403309322,403309569...403309578,403309825...403309834,403310081...403310090,403310337...403310346,403310593...403310602,403310849...403310858,403311105...403311114,403311361...403311370,403311617...403311626,403311873...403311882,403312129...403312138,403312385...403312394,403312641...403312650,403312897...403312906,403313153...403313162,403313409...403313418,403313665...403313674,403313921...403313930,403314177...403314186,403314433...403314442,403314689...403314698,403314945...403314954,403315201...403315210,403315457...403315466&measures=20100,20701")




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# NOMIS 2, GETTING ECONOMICALLY ACTIVE AS DENOMINATOR FOR OCCUPATION DATA----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#do a search on 'labour force survey', looking for 
#labour force survey - national and regional - headline indicators (seasonally adjusted)
#And getting total economically active / total in employment

# a <- nomis_search(name = '*survey*')
# atts <- a %>% tidyr::unnest(components.attribute) %>% glimpse()
# unique(atts$name.value)
# 
# #NM_59_1
# y <- nomis_data_info("NM_59_1")
# glimpse(y)
# 
# y %>% tidyr::unnest(components.dimension) %>% View
# #"GEOGRAPHY"         "SEX"               "ECONOMIC_ACTIVITY" "VALUE_TYPE"        "MEASURES"          "FREQ" 
# y %>% tidyr::unnest(components.dimension) %>% select(conceptref) %>% pull()
# 
# nomis_get_metadata(id = "NM_59_1", concept = "GEOGRAPHY")
# nomis_get_metadata(id = "NM_59_1", concept = "GEOGRAPHY", type = 'type')
# nomis_get_metadata(id = "NM_59_1", concept = "GEOGRAPHY", type = 'TYPE480')
# nomis_get_metadata(id = "NM_59_1", concept = "SEX")
# nomis_get_metadata(id = "NM_59_1", concept = "ECONOMIC_ACTIVITY")
# nomis_get_metadata(id = "NM_59_1", concept = "VALUE_TYPE")
# nomis_get_metadata(id = "NM_59_1", concept = "MEASURES")
# nomis_get_metadata(id = "NM_59_1", concept = "FREQ")


#Wrong. Probably want 'economic activity by age' from an APS cell, as above, and pick out the right values
#Around 4000 for cell, need to search...

#Ah nope, 'in employment' for actual job counts
#“in employment” and “unemployed” sum to “econ active”.
cell <- nomis_get_metadata(id = "NM_17_1", concept = "CELL")
#Quarterly data, can get jan to dec with e.g. "2005-12" I think...
cell <- nomis_get_metadata(id = "NM_17_1", concept = "time")
#Just want this single table I think
cell %>% filter(grepl('402719489', id, ignore.case = T)) 

in_employment <- nomis_get_data(id = "NM_17_1", time = "latest", geography = "TYPE438", cell = '402719489') %>% 
  select(DATE_NAME,GEOGRAPHY_NAME,GEOGRAPHY_CODE,CELL_NAME,MEASURES_NAME,OBS_VALUE,OBS_STATUS,OBS_STATUS_NAME) %>% 
  rename(ALL_IN_EMPLOYMENT_16PLUS = OBS_VALUE)

#Ah, getting all the data time range easy - just don't ask for a time slot, gives you the lot
#NO VALUES UNTIL JAN 2012. Do others exist in prev version?
# in_employment <- nomis_get_data(id = "NM_17_1", geography = "TYPE438", cell = '402719489') %>% 
#   select(DATE_NAME,GEOGRAPHY_NAME,GEOGRAPHY_CODE,CELL_NAME,MEASURES_NAME,OBS_VALUE,OBS_STATUS,OBS_STATUS_NAME) %>% 
#   rename(ALL_IN_EMPLOYMENT_16PLUS = OBS_VALUE)

#check we have a full set of values... tick
table(is.na(in_employment$ALL_IN_EMPLOYMENT_16PLUS))




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MERGE ECON ACTIVE INTO THE SICSOC NUMBERS----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# chk <- z %>% 
#   left_join(
#     in_employment %>% select(GEOGRAPHY_NAME,MEASURES_NAME,ALL_IN_EMPLOYMENT_16PLUS),
#     by = c('GEOGRAPHY_NAME','MEASURES_NAME')
#   )

#Actually, just want to (a) drop econ active CI value and (b) add just to place
#Cos the CI in the main data can be made into a proportion with the same number, which is what we want
z <- readRDS('data/NUTS2_2016_latestAPS_SIC2007_SOC2020.rds')

z <- z %>% 
  left_join(
    in_employment %>% 
      filter(MEASURES_NAME != 'Confidence') %>% 
      select(GEOGRAPHY_NAME,ALL_IN_EMPLOYMENT_16PLUS),
    by = c('GEOGRAPHY_NAME')
  )

#And make the obs_value proportional to regional econ active
z <- z %>% 
  mutate(OBS_VALUE_REGIONALPERCENT = (OBS_VALUE/ALL_IN_EMPLOYMENT_16PLUS)*100)

saveRDS(z, 'data/sicsoc.rds')

#Check against older incorrect NOMIS data
older <- readRDS('data/OLD__sicsoc.rds')


#Ah ha, values are different. That changes things! Rerunning... 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# WHAT'S THE MISSING / ERROR BAR RATE LIKE? TEST WITH SY----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

unique(z$GEOGRAPHY_NAME)

sy <- z %>% filter(GEOGRAPHY_NAME == 'South Yorkshire')
sy <- z %>% filter(GEOGRAPHY_NAME %in% c('South Yorkshire','Greater Manchester'))
sy <- z %>% filter(grepl(pattern = 'South Yorkshire|Manchester|Merseyside|West York', x = GEOGRAPHY_NAME, ignore.case = T))
sy <- z %>% filter(grepl(pattern = 'South Yorkshire|Manchester|West York', x = GEOGRAPHY_NAME, ignore.case = T))
sy <- z %>% filter(grepl(pattern = 'South Yorkshire|West Mid', x = GEOGRAPHY_NAME, ignore.case = T))
sy <- z %>% filter(grepl(pattern = 'South Yorkshire|West Yorkshire', x = GEOGRAPHY_NAME, ignore.case = T))
sy <- z %>% filter(grepl(pattern = 'South Yorkshire|Merseyside', x = GEOGRAPHY_NAME, ignore.case = T))
sy <- z %>% filter(grepl(pattern = 'South Yorkshire|Leicester', x = GEOGRAPHY_NAME, ignore.case = T))
sy <- z %>% filter(grepl(pattern = 'South Yorkshire|East Yorks', x = GEOGRAPHY_NAME, ignore.case = T))
sy <- z %>% filter(grepl(pattern = 'South Yorkshire|Surrey, East and West Sussex', x = GEOGRAPHY_NAME, ignore.case = T))
sy <- z %>% filter(grepl(pattern = 'South Yorkshire|Inner London - East', x = GEOGRAPHY_NAME, ignore.case = T))


#Eyeballing the OBS_STATUS code, we have:
#F - no value or CI
#A - value present, but the CI for that value can be marked as -->
#G (only get those if value is present / A)

#So if I only want to use values with CIs for this test. can use G?

#Group by SOC and SIC, if any OBS_STATUS are G, set OBS_VALUE to NA (as we can't use it)

#If visualising, we want min max for the error bars
#So let's stick value and conf wide
sy.w <- sy %>% 
  pivot_wider(values_from = OBS_VALUE, names_from = MEASURES_NAME) %>% 
  mutate(
    min_ci = Value - Confidence,
    max_ci = Value + Confidence
  )


#Facet by SIC, plot SOC2020 counts
ggplot(
  sy.w %>% filter(SIC2007!='Total Services'), 
  aes(x = SOC2020, y = Value, fill = GEOGRAPHY_NAME)
  ) +
  geom_bar(stat='identity', position = 'dodge') +
  # facet_wrap(~SIC2007) +
  facet_wrap(~SIC2007, scales = 'free_x') +
  geom_errorbar(aes(ymin = min_ci, ymax = max_ci), position = position_dodge()) +
  coord_flip()






#REPEAT FOR REGIONAL PERCENT OF 'IN EMPLOYMENT'
sy.w <- sy %>% 
  select(-OBS_VALUE) %>% 
  pivot_wider(values_from = OBS_VALUE_REGIONALPERCENT, names_from = MEASURES_NAME) %>% 
  mutate(
    min_ci = Value - Confidence,
    max_ci = Value + Confidence
  )



#Mark pairs where CIs do not overlap
#Rather than messing around with figuring out lags
#make a wider version and do differences there

#https://stackoverflow.com/a/3269471
#If (StartA <= EndB) and (EndA >= StartB) 
sy.ww <- sy.w %>%
  select(GEOGRAPHY_NAME,SIC2007,SOC2020,min_ci,max_ci) %>% 
  pivot_wider(
    names_from = GEOGRAPHY_NAME, values_from = c(min_ci,max_ci),
    values_fn = mean
  ) %>% 
  mutate(CIs_overlap = ifelse(
    (.[,3] <= .[,6] & .[,5] <= .[,4]) |
      (.[,4] <= .[,5] & .[,6] <= .[,3])  , 
    F,T) %>% c(.))#Make generic
  # mutate(CIs_overlap = ifelse(
  #   (`min_ci_Greater Manchester` <= `max_ci_South Yorkshire` & `max_ci_Greater Manchester` <= `min_ci_South Yorkshire`) |
  #     (`min_ci_South Yorkshire` <= `max_ci_Greater Manchester` & `max_ci_South Yorkshire` <= `min_ci_Greater Manchester`)  , 
  #   F,T))


#Merge those back in
#Applies to both places so can merge in on these
sy.w <- sy.w %>% 
  left_join(
    sy.ww,
    by = c('SIC2007','SOC2020')
  )



#Facet by SIC, plot SOC2020 counts
ggplot(
  sy.w %>% filter(SIC2007!='Total Services'), 
  aes(x = SOC2020, y = Value, fill = GEOGRAPHY_NAME)
) +
  geom_bar(stat='identity', position = 'dodge') +
  # facet_wrap(~SIC2007) +
  facet_wrap(~SIC2007, scales = 'free_x') +
  geom_errorbar(aes(ymin = min_ci, ymax = max_ci, colour = CIs_overlap, size = CIs_overlap), position = position_dodge()) +
  coord_flip() +
  xlab("% of total employed") +
  ylab("") +
  theme(legend.title=element_blank(), plot.title = element_text(face = 'bold')) +
  ggtitle("Occupation (SOC2020) vs sector (SIC2007), SY and GM") +
  scale_color_manual(values = c('black','grey')) +
  scale_size_manual(values = c(1,0.5)) +
  guides(colour = F, size = F)

# ggsave('local/localimages/sicsoc_GM_SY.png', width = 14, height = 10)






#Sooo can we get all that data to see where skills differ everywhere for SY significantly?
get_all_places_sicsocs <- function(geography_name,comparator_name){
  
  sy <- z %>% filter(GEOGRAPHY_NAME %in% c(comparator_name,geography_name))
  
  sy.w <- sy %>% 
    select(-OBS_VALUE) %>% 
    pivot_wider(values_from = OBS_VALUE_REGIONALPERCENT, names_from = MEASURES_NAME) %>% 
    mutate(
      min_ci = Value - Confidence,
      max_ci = Value + Confidence
    )
  
  #Mark pairs where CIs do not overlap
  #Rather than messing around with figuring out lags
  #make a wider version and do differences there
  
  #https://stackoverflow.com/a/3269471
  #If (StartA <= EndB) and (EndA >= StartB) 
  sy.ww <- sy.w %>%
    select(GEOGRAPHY_NAME,SIC2007,SOC2020,min_ci,max_ci) %>% 
    pivot_wider(
      names_from = GEOGRAPHY_NAME, values_from = c(min_ci,max_ci),
      values_fn = mean
    ) %>% 
    mutate(CIs_overlap = ifelse(
      (.[,3] <= .[,6] & .[,5] <= .[,4]) |
        (.[,4] <= .[,5] & .[,6] <= .[,3])  , 
      F,T))#Make generic
  # mutate(CIs_overlap = ifelse(
  #   (`min_ci_Greater Manchester` <= `max_ci_South Yorkshire` & `max_ci_Greater Manchester` <= `min_ci_South Yorkshire`) |
  #     (`min_ci_South Yorkshire` <= `max_ci_Greater Manchester` & `max_ci_South Yorkshire` <= `min_ci_Greater Manchester`)  , 
  #   F,T))
  
  
  #Merge those back in
  #Applies to both places so can merge in on these
  
  #Find difference between actual value, then display if sig (which won't show effect size properly, but...)
  sy.w %>% 
    left_join(
      sy.ww,
      by = c('SIC2007','SOC2020')
    ) %>% 
    group_by(SIC2007,SOC2020) %>% 
    mutate(
      valdiff = lag(Value) - Value
    ) %>% 
    filter(GEOGRAPHY_NAME == geography_name) %>% 
    select(GEOGRAPHY_NAME,SOC2020,SIC2007,CIs_overlap,valdiff)
  
}
  
  
# chk <- get_all_places_sicsocs('West Yorkshire')
# table(chk$valdiff)


#OK, let's see...
comparator = 'South Yorkshire'
comparator = 'Greater Manchester'
comparator = 'West Yorkshire'

allz <- purrr::map(
  .f = get_all_places_sicsocs, 
  .x = unique(z$GEOGRAPHY_NAME[z$GEOGRAPHY_NAME!=comparator]),
  comparator_name = comparator
  ) %>% bind_rows


# allz <- purrr::map(.f = get_all_places_sicsocs, .x = unique(z$GEOGRAPHY_NAME[z$GEOGRAPHY_NAME!='South Yorkshire'])) %>% bind_rows

allz <- allz %>% 
  unite(sicsoc, c('SOC2020','SIC2007'), sep = ' || ', remove = F) %>%
  # unite(sicsoc, c('SIC2007','SOC2020'), sep = ' || ', remove = F) %>% 
  mutate(
    valdiff = ifelse(!CIs_overlap, valdiff, NA)
  ) 
  
#heatmap

#Not quite! 
# zerocutoff = mean(allz$valdiff <= 0, na.rm = T)
# ecdf(allz$valdiff)(0)

#This?
# zerocutoff <- allz$valdiff[!is.na(allz$valdiff)]
# zerocutoff <- zerocutoff[order(zerocutoff)]
# max(which(zerocutoff < 0))
# max(which(zerocutoff < 0))/length(zerocutoff)#Goddam, same number!! Somewhat predictably

#Is it based on vector length, does scaling by total length work? Very much no.
#cutoff <- max(which(zerocutoff < 0))/length(allz$valdiff)

#It's not just the inverse is it? Nope
# zerocutoff = 1 - zerocutoff

# zerocutoff = 0.66
# zerocutoff = allz$valdiff[order(allz$valdiff)]


#Actually, it's just the middle position on this scale, not the quantile...?
#turn into scale with zero in
#https://www.statology.org/r-scale-between-0-and-1/
#Tick!
valz <- c(range(allz$valdiff[allz$SIC2007=='Total Services'], na.rm = T), 0)
scale_values <- function(x){(x-min(x))/(max(x)-min(x))}
scaled <- scale_values(valz)
zerocutoff <- scaled[3]


ggplot(allz %>% filter(SIC2007=='Total Services'), aes(x = substr(GEOGRAPHY_NAME,0,20), y = sicsoc, fill= valdiff)) + 
  geom_tile() +
  scale_fill_gradientn(
    colours = c("red", "white", "darkgreen"),
    values = c(0, zerocutoff, 1)#https://stackoverflow.com/a/58725778/5023561
  ) +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=0))



valz <- c(range(allz$valdiff[allz$SIC2007!='Total Services'], na.rm = T), 0)
scale_values <- function(x){(x-min(x))/(max(x)-min(x))}
scaled <- scale_values(valz)
zerocutoff <- scaled[3]



#E63946 - A bright red tone
#F1FAEE - A very light (almost white) pastel green
#A8DADC - A soft, muted teal
#457B9D - A desaturated dark blue
#F4A261 - A sandy brown
#2A9D8F - A medium sea green
#6C757D - A neutral dark gray

#FF5733 - A vibrant orange-red
#CDDC39 - A bright lime green
#00BCD4 - A bright cyan
#9C27B0 - A deep purple
#3F51B5 - A rich indigo
#E91E63 - A strong pink
#009688 - A teal
#FFEB3B - A vivid yellow
#795548 - A brown
#607D8B - A cool blue-gray

repnumber = 10
repnumber = 9

b <- c(
  rep('#FF5733',repnumber),
  rep('#CDDC39',repnumber),
  rep('#00BCD4',repnumber),
  rep('#9C27B0',repnumber),
  rep('#3F51B5',repnumber),
  rep('#E91E63',repnumber),
  rep('#009688',repnumber),
  rep('#FFEB3B',repnumber),
  rep('#607D8B',repnumber)
)


ggplot(allz %>% filter(SIC2007!='G-Q Total Services'), aes(x = substr(GEOGRAPHY_NAME,0,20), y = sicsoc, fill= valdiff)) + 
  geom_tile() +
  scale_fill_gradientn(
    colours = c("red", "white", "darkgreen"),
    values = c(0, zerocutoff, 1)#https://stackoverflow.com/a/58725778/5023561
  ) +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=0)) +
  ggtitle(comparator) +
  theme(
    plot.title = element_text(face = 'bold'),
    axis.text.y = element_text(colour = b)
    ) 
   







#VERSION THAT SHOWS ALL BUT OUTLINES SIG IN BLACK... NOPE, DOESN'T REALLY WORK, TOO HARD TO READ
#And also, a lot of NA data anyway
# allz <- purrr::map(
#   .f = get_all_places_sicsocs, 
#   .x = unique(z$GEOGRAPHY_NAME[z$GEOGRAPHY_NAME!=comparator]),
#   comparator_name = comparator
# ) %>% bind_rows
# 
# 
# allz <- allz %>% 
#   # unite(sicsoc, c('SOC2020','SIC2007'), sep = ' || ', remove = F) %>% 
#   unite(sicsoc, c('SIC2007','SOC2020'), sep = ' || ', remove = F) 
# 
# valz <- c(range(allz$valdiff[allz$SIC2007!='Total Services'], na.rm = T), 0)
# scale_values <- function(x){(x-min(x))/(max(x)-min(x))}
# scaled <- scale_values(valz)
# zerocutoff <- scaled[3]
# 
# b <- c(
#   rep('#FF5733',9),
#   rep('#CDDC39',9),
#   rep('#00BCD4',9),
#   rep('#9C27B0',9),
#   rep('#3F51B5',9),
#   rep('#E91E63',9),
#   rep('#009688',9),
#   rep('#FFEB3B',9),
#   rep('#607D8B',9)
# )
# 
# ggplot(allz %>% filter(SIC2007!='Total Services'), aes(x = substr(GEOGRAPHY_NAME,0,20), y = sicsoc, fill= valdiff, colour = CIs_overlap)) + 
#   geom_tile(width = 0.8, height = 0.8, size = 0.2) +
#   scale_fill_gradientn(
#     colours = c("red", "white", "darkgreen"),
#     values = c(0, zerocutoff, 1)#https://stackoverflow.com/a/58725778/5023561
#   ) +
#   theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=0)) +
#   ggtitle(comparator) +
#   theme(
#     plot.title = element_text(face = 'bold'),
#     axis.text.y = element_text(colour = b)
#   ) +
#   scale_color_manual(values = setNames(c('black','white'),c(F,T)), guide = 'none')









