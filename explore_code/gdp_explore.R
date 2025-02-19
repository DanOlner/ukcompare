#GDP/GVA explore
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

#OVerall GDP/GVA figures, GVA per head/job/hour etc.
#Looking to compare SY to other places

#chained volume GDP at 2019 price levels, should be able to tell us actual growth rates if needed
#A growth standout comparison matrix would be possible here, if we want that

#Sticking purely to ITL2 for now
gdp.itl2 <- read_csv('data/Table 10 Gross Domestic Product chained volume measures in 2019 money value pounds million.csv') %>% 
  rename(ITLcode = `ITL code`, region = `Region name`) %>% 
  filter(ITL == 'ITL2') %>% 
  pivot_longer(cols = `1998`:`2021`, names_to = 'year', values_to = 'gdp') %>% 
  mutate(year = as.numeric(year))


#Though if we're only doing a point estimate and don't need a slope, then current prices would be fine and better?
#At least if finding proportions
gdp.cp.itl2 <- read_csv('data/Table 5 GDP at current market prices pounds million.csv') %>% 
  rename(ITLcode = `ITL code`, region = `Region name`) %>% 
  filter(ITL == 'ITL2') %>% 
  pivot_longer(cols = `1998`:`2021`, names_to = 'year', values_to = 'gdp') %>% 
  mutate(year = as.numeric(year))



#Which is great, but we have to divide same by same, so mostly need to use GVA
#And need to use current price to compare different places
#Best source I seem to have for whole-ITL2 GVA (also balanced) is the sector sheet, already processed. Again:
itl2.topcp <- read_csv('data/sectors/Table 2c ITL2 UK current price estimates pounds million.csv')

names(itl2.topcp) <- gsub(x = names(itl2.topcp), pattern = ' ', replacement = '_')

itl2.topcp <- itl2.topcp %>% 
  filter(SIC07_code == 'Total') %>% #Keep single top level GVA number for each ITL2
  pivot_longer(`1998`:`2021`, names_to = 'year', values_to = 'value') %>% 
  mutate(year = as.numeric(year))


#Then we can also get GVA per filled job and hour worked via
#https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/labourproductivity/datasets/subregionalproductivitylabourproductivitygvaperhourworkedandgvaperfilledjobindicesbyuknuts2andnuts3subregions
#Would also like to see how hours worked per filled job average differs
perfilledjob <- read_csv('data/Table B4 Current Price unsmoothed GVA B per filled job £ ITL2 and ITL3 subregions 2002 to 2021.csv') %>% 
  rename(ITL = `ITL level`, ITLcode = `ITL code`, region = `Region name`) %>% 
  filter(ITL == 'ITL2') %>% 
  pivot_longer(cols = `2002`:`2021`, names_to = 'year', values_to = 'gva') %>% 
  mutate(year = as.numeric(year))

perhourworked <- read_csv('data/Table A4 Current Price unsmoothed GVA B per hour worked £ ITL2 and ITL3 subregions 2004 to 2021.csv') %>% 
rename(ITL = `ITL level`, ITLcode = `ITL code`, region = `Region name`) %>% 
  filter(ITL == 'ITL2') %>% 
  pivot_longer(cols = `2004`:`2021`, names_to = 'year', values_to = 'gva') %>% 
  mutate(year = as.numeric(year))


#Let's start with hours as a more consistent comparator
#dump the lot together...
ggplot(perhourworked, aes(x = year, y = gva, group = region)) +
  geom_line()

#Yoinks
ggplot() +
  geom_line(data = perhourworked, aes(x = year, y = gva, group = region)) +
  geom_line(data = perhourworked %>% filter(region == 'South Yorkshire'), aes(x = year, y = gva, group = region), colour = 'red', linewidth = 2)

#log plus interactive
p <- ggplot() +
  geom_line(data = perhourworked, aes(x = year, y = gva, group = region)) +
  geom_line(data = perhourworked %>% filter(region == 'South Yorkshire'), aes(x = year, y = gva, group = region), colour = 'red', linewidth = 2) +
  scale_y_log10()

ggplotly(p, tooltip = 'region')

#Per filled job?
ggplot() +
  geom_line(data = perfilledjob, aes(x = year, y = gva, group = region)) +
  geom_line(data = perfilledjob %>% filter(region == 'South Yorkshire'), aes(x = year, y = gva, group = region), colour = 'red', linewidth = 2)



#Rank and see which changed position the most?
perhourworked <- perhourworked %>% 
  group_by(year) %>% 
  mutate(rank = rank(gva))

perfilledjob <- perfilledjob %>% 
  group_by(year) %>% 
  mutate(rank = rank(gva))

ggplot() +
  geom_line(data = perhourworked, aes(x = year, y = rank, group = region, colour = region)) +
  geom_line(data = perhourworked %>% filter(region == 'South Yorkshire'), aes(x = year, y = rank, group = region), colour = 'red', linewidth = 2)

ggplot() +
  geom_line(data = perfilledjob, aes(x = year, y = rank, group = region, colour = region)) +
  geom_line(data = perfilledjob %>% filter(region == 'South Yorkshire'), aes(x = year, y = rank, group = region), colour = 'red', linewidth = 2)




#Repeat for ITL3
perfilledjob.itl3 <- read_csv('data/Table B4 Current Price unsmoothed GVA B per filled job £ ITL2 and ITL3 subregions 2002 to 2021.csv') %>% 
  rename(ITL = `ITL level`, ITLcode = `ITL code`, region = `Region name`) %>% 
  filter(ITL == 'ITL3') %>% 
  pivot_longer(cols = `2002`:`2021`, names_to = 'year', values_to = 'gva') %>% 
  mutate(year = as.numeric(year))

perhourworked.itl3 <- read_csv('data/Table A4 Current Price unsmoothed GVA B per hour worked £ ITL2 and ITL3 subregions 2004 to 2021.csv') %>% 
  rename(ITL = `ITL level`, ITLcode = `ITL code`, region = `Region name`) %>% 
  filter(ITL == 'ITL3') %>% 
  pivot_longer(cols = `2004`:`2021`, names_to = 'year', values_to = 'gva') %>% 
  mutate(year = as.numeric(year))

ggplot() +
  geom_line(data = perhourworked.itl3, aes(x = year, y = gva, group = region)) +
  geom_line(data = perhourworked.itl3 %>% filter(region == 'Sheffield'), aes(x = year, y = gva, group = region), colour = 'red', linewidth = 2) +
  geom_line(data = perhourworked.itl3 %>% filter(grepl(x = region, pattern = 'Barnsley', ignore.case = T)), aes(x = year, y = gva, group = region), colour = 'blue', linewidth = 2)

#log plus interactive
p <- ggplot() +
  geom_line(data = perhourworked.itl3, aes(x = year, y = gva, group = region)) +
  geom_line(data = perhourworked.itl3 %>% filter(region == 'Sheffield'), aes(x = year, y = gva, group = region), colour = 'red', linewidth = 2) +
  geom_line(data = perhourworked.itl3 %>% filter(grepl(x = region, pattern = 'Barnsley', ignore.case = T)), aes(x = year, y = gva, group = region), colour = 'blue', linewidth = 2) +
  scale_y_log10()

ggplotly(p, tooltip = 'region')



#GETTING BASIC IF THENS ON GROWTH
#If SY was as large as England av, and England av minus London, how much bigger would it be? (And Northern av?)
#Which just involves comparing per job / hour proportions and their change

#I want some smoothing though. For doing size comparisons, might be worth trying a few things
#But let's start with 3 year smoothing and check differences
perhourworked <- perhourworked %>% 
  arrange(year) %>% 
  group_by(region) %>%
  mutate(
    movingav = rollapply(gva,3,mean,align='right',fill=NA),
    rank_movingav = rollapply(rank,3,mean,align='right',fill=NA),
    rank_movingav_7yr = rollapply(rank,7,mean,align='right',fill=NA)
    )

perfilledjob <- perfilledjob %>% 
  arrange(year) %>% 
  group_by(region) %>%
  mutate(
    movingav = rollapply(gva,3,mean,align='right',fill=NA),
    rank_movingav = rollapply(rank,3,mean,align='right',fill=NA),
    rank_movingav_7yr = rollapply(rank,7,mean,align='right',fill=NA)
    )


#Picking out England and North etc...
#Via https://github.com/DanOlner/regionalGVAbyindustry

#Northern England
north <- perhourworked$region[grepl('Greater Manc|Merseyside|West Y|Cumbria|Cheshire|Lancashire|East Y|North Y|Tees|Northumb|South Y', perhourworked$region, ignore.case = T)] %>% unique

#South England
south <- perhourworked$region[!grepl('Greater Manc|Merseyside|West Y|Cumbria|Cheshire|Lancashire|East Y|North Y|Tees|Northumb|South Y|Scot|Highl|Wales|Ireland', perhourworked$region, ignore.case = T)] %>% unique

#South minus London
south.minus.london <- south[!grepl('london',south,ignore.case = T)]

#England!
england <- c(north,south)

#England minus London
england.minus.london <- england[!grepl('london',england,ignore.case = T)]

#UK minus London
uk.minus.london <- perhourworked$region[!grepl('london',england,ignore.case = T)] %>% unique


#Check is all correct with map
itl2.geo <- st_read('data/geographies/International_Territorial_Level_2_January_2021_UK_BFE_V2_2022_-4735199360818908762/ITL2_JAN_2021_UK_BFE_V2.shp') %>% 
  st_simplify(preserveTopology = T, dTolerance = 100)

#Tick
table(england %in% itl2.geo$ITL221NM)

#Ticks all round
plot(st_geometry(itl2.geo %>% filter(ITL221NM %in% england)), col = 'grey')
plot(st_geometry(itl2.geo %>% filter(ITL221NM %in% north)), col = 'blue', add = T)
plot(st_geometry(itl2.geo %>% filter(ITL221NM %in% south)), col = 'green', add = T)
plot(st_geometry(itl2.geo %>% filter(ITL221NM %in% england.minus.london)), col = 'red', add = T)

plot(st_geometry(itl2.geo %>% filter(ITL221NM %in% uk.minus.london)), col = 'grey')

unique(perhourworked$year)
unique(perhourworked$year[!is.na(perhourworked$movingav)])


#Check whether pre covid gaps are very different or not too...
#Let's do this with a couple of flag columns so we can plot boxplots etc too

#Per hour worked
perhourworked <- perhourworked %>% 
  mutate(ns_england_restofUK = case_when(
    region %in% north ~ 'North England',
    region %in% south ~ 'South Eng (inc. London)',
    .default = 'rest of UK'
  ))

table(perhourworked$ns_england_restofUK, useNA = 'always')

perhourworked <- perhourworked %>% 
  mutate(ns_england_restofUK_londonseparate = case_when(
    region %in% north ~ 'North England',
    region %in% south.minus.london ~ 'South Eng (exc. London)',
    grepl('london',region,ignore.case = T) ~ 'London',
    .default = 'rest of UK'
  ))

table(perhourworked$ns_england_restofUK_londonseparate, useNA = 'always')


#Repeat for per filled job
perfilledjob <- perfilledjob %>% 
  mutate(ns_england_restofUK = case_when(
    region %in% north ~ 'North England',
    region %in% south ~ 'South Eng (inc. London)',
    .default = 'rest of UK'
  ))

table(perfilledjob$ns_england_restofUK, useNA = 'always')

perfilledjob <- perfilledjob %>% 
  mutate(ns_england_restofUK_londonseparate = case_when(
    region %in% north ~ 'North England',
    region %in% south.minus.london ~ 'South Eng (exc. London)',
    grepl('london',region,ignore.case = T) ~ 'London',
    .default = 'rest of UK'
  ))

table(perfilledjob$ns_england_restofUK_londonseparate, useNA = 'always')



#plot smoothed values, compare pre and post covid, add marker for SY
perhourworked <- perhourworked %>% 
  mutate(is_sy = region == 'South Yorkshire')

perfilledjob <- perfilledjob %>% 
  mutate(is_sy = region == 'South Yorkshire')


ggplot(perhourworked %>% filter(year %in% c(2018,2021)), aes(x = ns_england_restofUK_londonseparate, y = movingav, colour = is_sy, size = is_sy)) +
  geom_point(alpha = 0.75) +
  scale_size_manual(values = c(5,10)) +
  facet_wrap(~year)

ggplot(perfilledjob %>% filter(year %in% c(2018,2021)), aes(x = ns_england_restofUK_londonseparate, y = movingav, colour = is_sy, size = is_sy)) +
  geom_point(alpha = 0.75) +
  scale_size_manual(values = c(5,10)) +
  facet_wrap(~year)


p <- ggplot(perhourworked %>% filter(year %in% c(2018,2021)), aes(x = ns_england_restofUK_londonseparate, y = movingav, colour = is_sy, size = is_sy, group = region)) +
  geom_point(alpha = 0.75) +
  scale_size_manual(values = c(3,6)) +
  facet_wrap(~year)

ggplotly(p, tooltip = 'region')


#Just checking some other years
#That's very interesting. Seems to suggest gap to London actually closed.
#And SY caught up with rest of north.
ggplot(perhourworked %>% filter(year %in% c(2006,2021)), aes(x = ns_england_restofUK_londonseparate, y = movingav, colour = is_sy, size = is_sy)) +
  geom_point(alpha = 0.75) +
  scale_size_manual(values = c(5,10)) +
  facet_wrap(~year, scales = 'free_y')

p <- ggplot(perhourworked %>% filter(year %in% c(2006,2021)), aes(x = ns_england_restofUK_londonseparate, y = movingav, colour = is_sy, size = is_sy, group = region)) +
  geom_point(alpha = 0.75) +
  scale_size_manual(values = c(3,6)) +
  facet_wrap(~year, scales = 'free_y')
  
ggplotly(p, tooltip = 'region')


#Want full scale from zero plz, better for showing how close productivity is across everywhere
ggplot(perhourworked %>% filter(year %in% c(2006,2021)), aes(x = ns_england_restofUK_londonseparate, y = movingav, colour = is_sy, size = is_sy)) +
  geom_point(alpha = 0.75) +
  scale_size_manual(values = c(5,10)) +
  coord_cartesian(ylim=c(0,60)) +
  facet_wrap(~year, scales = 'free_y')




#Attempt at showing year change in positions
ggplot(perhourworked %>% filter(year %in% c(2006,2021)), aes(x = ns_england_restofUK_londonseparate, y = rank_movingav, colour = factor(year))) +
  geom_point(alpha = 0.75, position = position_dodge(width = 0.5)) +
  geom_line()


#Want to get lines between dodge. Option here: don't use dodge at all.
#https://stackoverflow.com/a/70122473/5023561
#OK, that's pretty informative
p <- ggplot(perhourworked %>% filter(year %in% c(2006,2021)), aes(x = factor(year), y = rank_movingav, group = region, color = factor(year))) +
  geom_line(aes(group = region), color = "grey44") +
  geom_point(aes(shape = is_sy, size = is_sy)) +
  facet_wrap(vars(ns_england_restofUK_londonseparate), nrow = 1)

ggplotly(p, tooltip = 'region')

#7 year rank moving av... 2010 first year with data
unique(perhourworked$year[!is.na(perhourworked$rank_movingav_7yr)])

p <- ggplot(perhourworked %>% filter(year %in% c(2010,2018)), aes(x = factor(year), y = rank_movingav_7yr, group = region, color = factor(year))) +
  geom_line(aes(group = region), color = "grey44") +
  geom_point(aes(shape = is_sy, size = is_sy)) +
  facet_wrap(vars(ns_england_restofUK_londonseparate), nrow = 1)

ggplotly(p, tooltip = 'region')


#What about more recently, just for three year overlap? And also pre and post covid?
p <- ggplot(perhourworked %>% filter(year %in% c(2018,2021)), aes(x = factor(year), y = rank_movingav, group = region, color = factor(year))) +
  geom_line(aes(group = region), color = "grey44") +
  geom_point(aes(shape = is_sy, size = is_sy)) +
  facet_wrap(vars(ns_england_restofUK_londonseparate), nrow = 1)

ggplotly(p, tooltip = 'region')


#And just pre covid
p <- ggplot(perhourworked %>% filter(year %in% c(2015,2018)), aes(x = factor(year), y = rank_movingav, group = region, color = factor(year))) +
  geom_line(aes(group = region), color = "grey44") +
  geom_point(aes(shape = is_sy, size = is_sy)) +
  facet_wrap(vars(ns_england_restofUK_londonseparate), nrow = 1)

ggplotly(p, tooltip = 'region')


#First coalition years to pre covid
p <- ggplot(perhourworked %>% filter(year %in% c(2012,2018)), aes(x = factor(year), y = rank_movingav, group = region, color = factor(year))) +
  geom_line(aes(group = region), color = "grey44") +
  geom_point(aes(shape = is_sy, size = is_sy)) +
  facet_wrap(vars(ns_england_restofUK_londonseparate), nrow = 1)

ggplotly(p, tooltip = 'region')



#Try also for smoothed per hour GVA... oh yes, totally uninteresting!
# p <- ggplot(perhourworked %>% filter(year %in% c(2006,2021)), aes(x = factor(year), y = movingav, group = region, color = factor(year))) +
#   geom_line(aes(group = region), color = "grey44") +
#   geom_point(aes(shape = is_sy, size = is_sy)) +
#   facet_wrap(vars(ns_england_restofUK_londonseparate), nrow = 1)
# 
# ggplotly(p, tooltip = 'region')




#BOXPLOT
ggplot(perhourworked %>% filter(year %in% c(2018,2021)), aes(x = ns_england_restofUK_londonseparate, y = movingav)) +
  geom_boxplot() +
  scale_size_manual(values = c(5,10)) +
  facet_wrap(~year)



#BASIC PERCENT DIFFERENCES IN PRODUCTIVITY, AVERAGES FOR ENGLAND, NORTH ETC
#Note, sy and uk_av are both single value columns just added for comparison to the regional averages
averages.perhourworked <- perhourworked %>% 
  filter(year == 2021) %>% 
  group_by(ns_england_restofUK_londonseparate) %>% 
  summarise(
    sy = perhourworked %>% filter(year == 2021, region == 'South Yorkshire') %>% select(movingav) %>% pull,
    mean_gva_av3years = mean(movingav, na.rm=T),
    uk_av_minuslondon = perhourworked %>% filter(!grepl('london',region,ignore.case = T), year == 2021) %>% select(movingav) %>% pull %>% mean(na.rm=T)
    ) %>% ungroup()


#Apart from London, all the other regional avs are not vastly different?




#WEIGHTED AVERAGE FOR UK MINUS LONDON
#Using number of hours worked per ITL2

#Get hours worked per week total for ITL2
totalhoursperweek.itl2 <- read_csv('data/Productivity Hours Worked per Week ITL2 and ITL3 subregions constrained to ITL1 2004 2021.csv') %>%
  rename(ITL = `ITL level`, ITLcode = `ITL code`, region = `Region name`) %>% 
  filter(ITL == 'ITL2') %>% 
  pivot_longer(cols = `2004`:`2021`, names_to = 'year', values_to = 'hours_per_week') %>% 
  mutate(year = as.numeric(year))


#Add in all the region labels
totalhoursperweek.itl2 <- totalhoursperweek.itl2 %>% 
  mutate(ns_england_restofUK = case_when(
    region %in% north ~ 'North England',
    region %in% south ~ 'South Eng (inc. London)',
    .default = 'rest of UK'
  ))

table(perhourworked$ns_england_restofUK, useNA = 'always')

totalhoursperweek.itl2 <- totalhoursperweek.itl2 %>% 
  mutate(ns_england_restofUK_londonseparate = case_when(
    region %in% north ~ 'North England',
    region %in% south.minus.london ~ 'South Eng (exc. London)',
    grepl('london',region,ignore.case = T) ~ 'London',
    .default = 'rest of UK'
  ))

table(perhourworked$ns_england_restofUK_londonseparate, useNA = 'always')

totalhoursperweek.itl2 <- totalhoursperweek.itl2 %>% 
  mutate(UK_minus_london = case_when(
    grepl('london',region,ignore.case = T) ~ 'London',
    .default = 'UK minus London'
  ))

table(totalhoursperweek.itl2$UK_minus_london, useNA = 'always')


#Weighted averages for the various groupings
#Check for 2021, do averages weighted by number of hours work per week that year in that place


#Both go up, though London by more
#Due presumably to cities having more hours per week / being more productive
weightedaverages.perhourworked <- perhourworked %>% filter(year == 2021) %>% 
  left_join(
    totalhoursperweek.itl2 %>% filter(year == 2021) %>% select(region,hours_per_week,UK_minus_london),
    by = 'region'
  ) %>% 
  group_by(UK_minus_london) %>% #group by "UK minus london" vs "london by itself"
  summarise(
    sy = perhourworked %>% filter(year == 2021, region == 'South Yorkshire') %>% select(movingav) %>% pull,
    mean_gva_av3years_weighted = weighted.mean(movingav, hours_per_week, na.rm=T)#get weighted average by each ITL2 grouping
  ) %>% 
  mutate(
    prop_diff = (mean_gva_av3years_weighted - sy)/sy
  )

#So SY would need to be ~17.7% more productive to match non London UK average

#out of interest, comparison to rest of North?
weightedaverages.perhourworked.north <- perhourworked %>% filter(year == 2021) %>% 
  left_join(
    totalhoursperweek.itl2 %>% filter(year == 2021) %>% select(region,hours_per_week,UK_minus_london),
    by = 'region'
  ) %>% 
  group_by(ns_england_restofUK_londonseparate) %>% #group by "UK minus london" vs "london by itself"
  summarise(
    sy = perhourworked %>% filter(year == 2021, region == 'South Yorkshire') %>% select(movingav) %>% pull,
    mean_gva_av3years_weighted = weighted.mean(movingav, hours_per_week, na.rm=T)#get weighted average by each ITL2 grouping
  ) %>% 
  mutate(
    prop_diff = (mean_gva_av3years_weighted - sy)/sy
  )




#Sanity check the weighted average manually, check with london
#TICK
chk <- perhourworked %>% filter(year == 2021) %>% 
  left_join(
    totalhoursperweek.itl2 %>% filter(year == 2021) %>% select(region,hours_per_week,UK_minus_london),
    by = 'region'
  ) %>% filter(grepl('london',region,ignore.case = T)) %>% 
  ungroup() %>% 
  mutate(hours_per_week_normalised = hours_per_week / sum(hours_per_week)) %>% 
  mutate(
    manualweightedav_weights = movingav * hours_per_week_normalised,
    manualweightedav = sum(manualweightedav_weights)
    )



#So we can then take the chained volume / actual economy size numbers and change those
#This is what the SEP did, adjusting by x amount, projecting forward, working out how to get from one path to the other
sy_gdp_2021 <- gdp.itl2 %>% filter(year == 2021, region == 'South Yorkshire') %>% select(gdp) %>% pull

#Current prices version
sy_gdp_cp_2021 <- gdp.cp.itl2 %>% filter(year == 2021, region == 'South Yorkshire') %>% select(gdp) %>% pull



prop_difftoeng_av_minuslondon <- weightedaverages.perhourworked %>% filter(UK_minus_london == 'UK minus London') %>% select(prop_diff) %>% pull

#5.6 billion extra
sy_gdp_2021 * prop_difftoeng_av_minuslondon

#current prices (GDP)
sy_gdp_cp_2021 * prop_difftoeng_av_minuslondon

#Get population numbers for per person figures
personcounts <- read_csv('data/Table 17 Total resident population numbers persons.csv') %>% 
  rename(ITLcode = `ITL code`, region = `Region name`) %>% 
  filter(ITL == 'ITL2') %>% 
  pivot_longer(cols = `1998`:`2021`, names_to = 'year', values_to = 'personcount') %>% 
  mutate(year = as.numeric(year))

sy_people2021 <- personcounts %>% 
  filter(year == 2021, region == 'South Yorkshire') %>% 
  select(personcount) %>% pull


#Extra amount of GDP per person
((sy_gdp_2021 * prop_difftoeng_av_minuslondon)/sy_people2021)*1000000

#Current price version
((sy_gdp_cp_2021 * prop_difftoeng_av_minuslondon)/sy_people2021)*1000000



#Using those numbers - what's 3% of that? 
#For how much private investment could be crowded in by public investment if using numbers from 
#Jan 2024 report: “Boosting growth and productivity in the United Kingdom through investments in the sustainable economy” (LSE, Grantham Foundation, CEP, Productivity Institute) 
sy_gdp_2021 * 0.03 *1000000

#current prices
sy_gdp_cp_2021 * 0.03 *1000000





#CRESR PRODUCTIVTY----

#Table pulled from this report:
#The productivity of industries and places, Christina Beatty Steve Fothergill 2020
#https://www.shu.ac.uk/centre-regional-economic-social-research/publications/the-productivity-of-industries-and-places

#Drop UK, can assume that's the av and add line
pip <- read_csv('data/productivity-industries-places.csv') %>% filter(Place!='United Kingdom')

#Get factor order from column order before converting to long
measure_factororder = names(pip)[2:9]

pip <- pip %>% 
  pivot_longer(cols = 2:9, names_to = "measure", values_to = "value") %>% 
  mutate(measure = factor(measure, ordered = T, levels = measure_factororder)) %>% 
  mutate(SCR = ifelse(Place == 'Sheffield City Region', T,F))

p <- ggplot(pip, aes(x = measure, y = value, group = Place, colour = SCR, alpha = SCR)) +
  geom_jitter(width = 0.1, size = 3) +
  geom_hline(yintercept = 100, colour = 'red', size = 3, alpha = 0.25) +
  scale_colour_manual(values = c('black','red')) +
  scale_alpha_manual(values = c(0.5,1)) +
  coord_flip() +
  xlab('') + ylab('index value (UK average is 100)')
  
p <- ggplotly(p, tooltip = 'Place')

#Makes horrible separate folder for libraries, but works (manual export via RStudio puts in single html script)
htmlwidgets::saveWidget(as_widget(p), "docs/plotly/pip_chart.html")













