#Regional GVA per sector explore
library(tidyverse)
library(zoo)
library(sf)
library(tmap)
library(plotly)
library(magick)
library(cowplot)
source('functions/misc_functions.R')
options(scipen = 99)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#LOAD AND INITIAL PROCESS----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#https://www.ons.gov.uk/economy/grossvalueaddedgva/datasets/nominalandrealregionalgrossvalueaddedbalancedbyindustry
#"Regional gross value added (balanced) by industry: all ITL regions"
#release date Apr 23

#Saved as CSV in Excel.
#Note: remove header in text editor, not Excel - Excel will format CSV with some SIC codes as dates, with no option to turn off.
#Thanks Excel.
itl1 <- read_csv('data/sectors/Table 1b ITL1 UK chained volume measures in 2019 money value pounds million.csv')
itl2 <- read_csv('data/sectors/Table 2b ITL2 UK chained volume measures in 2019 money value pounds million.csv')
itl3 <- read_csv('data/sectors/Table 3b ITL3 UK chained volume measures in 2019 money value pounds million.csv')

#itl3 geography
itl3.geo <- st_read('data/geographies/International_Territorial_Level_3_January_2021_UK_BUC_V3_2022_6920195468392554877/ITL3_JAN_2021_UK_BUC_V3.shp')
plot(st_geometry(itl3.geo))

#itl2 geography
itl2.geo <- st_read('data/geographies/International_Territorial_Level_2_January_2021_UK_BFE_V2_2022_-4735199360818908762/ITL2_JAN_2021_UK_BFE_V2.shp')
plot(st_geometry(itl2.geo))

#Check ID matches... tick
table(itl3.geo$ITL321CD %in% itl3$`ITL region code`)
table(itl2.geo$ITL221CD %in% itl2$`ITL region code`)

#If just after the highest resolution SIC codes
#Remove rows that are the higher level SIC categories like sectors
#Data also includes sums for "All industries" and "services sector"

#May want to go back to higher-level categories, but let's do this first

#So, removing sum rows to get to just most granular SIC categories...
#NOTE: summed rows will not equal those already-summed categories, as they're adjusted separately...
#Note 2 from the excel sheet: "Components will not sum to totals since chain-linking produces non-additive volume estimates."

#Ones to remove from "SIC07 code" (looking/checking in orig excel sheet)
#These are all categories with duplicate values in other rows (which takes some spotting, the colour coding is not very helpful!)
#Note: "real estate activities" is a single category, but there are two subcats that break it down into
#With and without imputed rental. So just need those two to avoid double counting.



#!!!!!!!!!!!
#WARNING: CURRENTLY ONLY CORRECT LIST TO REMOVE FOR ITL2
#!!!!!!!!!!!

#Each geog level has a different number of SIC categories in it 
SICremoves = c(
  'Total',
  'A-E',
  'A (1-3)',
  'C (10-33)',
  'CA (10-12)',
  'CB (13-15)',
  'CC (16-18)',
  'CG (22-23)',
  'CH (24-25)',
  'CL (29-30)',
  'CM (31-33)',
  'F (41-43)',
  'G-T',
  'G (45-47)',
  'H (49-53)',
  'I (55-56)',
  'J (58-63)',
  'K (64-66)',
  'L (68)',#real estate activities - leaves in "Real estate activities, excluding imputed rental" & "Owner-occupiers' imputed rental" as separate categories
  'M (69-75)',
  'N (77-82)',
  'Q (86-88)',
  'R (90-93)',
  'S (94-96)'
)

#~~~~~~~~~~~~~~~~~~~~~~~~
#ITL1: UK AS A WHOLE----
#~~~~~~~~~~~~~~~~~~~~~~~~

#Check what happened at UK level first, for comparison
#(No direct pop comparisons necessary at this stage, though productivity per person still missing here)
#Look at just UK here
itl1.hluk <- itl1 %>% 
  filter(!`SIC07 code` %in% SICremoves, `ITL region name` == "United Kingdom") %>% 
  pivot_longer(`1998`:`2021`, names_to = 'year', values_to = 'value')


#Break down into sector size tranches and facet
#Use sizes in most recent year
sectorsizes <- itl1.hluk %>% 
  group_by(`SIC07 code`) %>% 
  summarise(av = mean(value)) %>% 
  mutate(group = as.integer(cut_number(av, n = 5)))

#Or size of sector for most recent value, which might make more sense than average for viewing
sectorsizes <- itl1.hluk %>% 
  filter(year == 2021) %>% 
  mutate(group = as.integer(cut_number(value, n = 5)))

#While we're here... what are proportions?
sectorsizes$percent <- (sectorsizes$value / sum(sectorsizes$value))*100

#Add group labels to data
itl1.hluk <- itl1.hluk %>% 
  left_join(
    sectorsizes %>% select(`SIC07 code`, group),
    by = "SIC07 code"
    )


ggplot(itl1.hluk, aes(x = year, y = value, colour = `SIC07 code`, group = `SIC07 code`)) +
  geom_line() +
  facet_wrap(~group, scales = 'free_y')


#PLOTLY VERSION
#For hovering to easily see the sector names
plot_ly(data = itl1.hluk %>% filter(group==5), x = ~year, y = ~value, color = ~`SIC07 code`, 
        text = ~paste("Sector:", `SIC07 description`),  # Add this line for hover text
        hoverinfo = 'text+y+x',
        type = 'scatter', mode = 'lines+markers', line = list(shape = 'linear')) %>%
  layout(title = "Yearly values by SIC", 
         xaxis = list(title = "Year"), 
         yaxis = list(title = "Value", type='log'),
         # yaxis = list(title = "Value"),
         showlegend = TRUE)



#MOVING AVERAGE
#https://stackoverflow.com/questions/26198551/rolling-mean-moving-average-by-group-id-with-dplyr
itl1.hluk <- itl1.hluk %>% 
  group_by(`SIC07 code`) %>% 
  mutate(movingav = rollapply(value,5,mean,align='right',fill=NA))

plot_ly(data = itl1.hluk %>% filter(group==3), x = ~year, y = ~movingav, color = ~`SIC07 code`, 
        text = ~paste("Sector:", `SIC07 description`),  # Add this line for hover text
        hoverinfo = 'text+y+x',
        type = 'scatter', mode = 'lines+markers', line = list(shape = 'linear')) %>%
  layout(title = "Yearly values by SIC", 
         xaxis = list(title = "Year"), 
         # yaxis = list(title = "Value", type='log'),
         yaxis = list(title = "Value"),
         showlegend = TRUE)





#~~~~~~~~
#ITL2----
#~~~~~~~~

#filter those out
itl2.h <- itl2 %>% 
  filter(!`SIC07 code` %in% SICremoves)

#Some of the numbers at these geographies are very small integers, we have no decimal changes – any growth between years is going to look jumpy if just looking at percentages / ppt

#Nevertheless, quick plot
#Make years long
itl2.hl <- itl2.h %>% 
  pivot_longer(`1998`:`2021`, names_to = 'year', values_to = 'value')

#MOVING AVERAGE
#https://stackoverflow.com/questions/26198551/rolling-mean-moving-average-by-group-id-with-dplyr
itl2.hl <- itl2.hl %>% 
  group_by(`ITL region name`,`SIC07 code`) %>% 
  mutate(movingav = rollapply(value,5,mean,align='right',fill=NA))


#test plot on smaller group of places
ggplot(
  itl2.hl %>% filter(`ITL region name` %in% unique(itl2.hl$`ITL region name`)[1:5]),
  aes(x = year, y = value, colour = `SIC07 code`, group = `SIC07 code`)
) +
  geom_line() +
  facet_wrap(~`ITL region name`) +
  # scale_y_log10() +
  guides(colour = "none")




#Look at larger sectors
sectors.byavsize <- itl2.hl %>% 
  group_by(`ITL region name`, `SIC07 code`) %>% 
  summarise(av = mean(value), sectorname = max(`SIC07 description`)) %>% 
  arrange(-av)

p <- ggplot(
  itl2.hl %>% filter(
    `ITL region name` %in% c(unique(sectors.byavsize$`ITL region name`)[1:9],'South Yorkshire','Greater Manchester')
    ),
  aes(x = year, y = value, colour = `SIC07 description`, group = `SIC07 code`)
) +
  geom_line() +
  facet_wrap(~`ITL region name`, ncol = 2) +
  # scale_y_log10() +
  guides(colour = "none")


ggplotly( p, tooltip = c('SIC07 description'))


#PLOTLY VERSION
#For hovering to easily see the sector names
plot_ly(data = itl2.hl %>% filter(`ITL region name`=="South Yorkshire"), x = ~year, y = ~value, color = ~`SIC07 code`,
# plot_ly(data = itl2.hl %>% filter(`ITL region name`=="South Yorkshire", `SIC07 description` == unique(itl2.hl$`SIC07 description`)[grepl('basic metal',unique(itl2.hl$`SIC07 description`))]), 
        x = ~year, y = ~value, color = ~`SIC07 code`,
# plot_ly(data = itl2.hl %>% filter(`ITL region name`=="Greater Manchester"), x = ~year, y = ~value, color = ~`SIC07 code`,
        text = ~paste("Sector:", `SIC07 description`),  # Add this line for hover text
        hoverinfo = 'text+y+x',
        type = 'scatter', mode = 'lines+markers', line = list(shape = 'linear')) %>%
  layout(title = "Yearly values by SIC", 
         xaxis = list(title = "Year"), 
         yaxis = list(title = "Value"),
         # yaxis = list(title = "Value", type='log'),
         showlegend = TRUE)


#MOVING AV
plot_ly(data = itl2.hl %>% filter(`ITL region name`=="South Yorkshire"), x = ~year, y = ~movingav, color = ~`SIC07 code`,
# plot_ly(data = itl2.hl %>% filter(`ITL region name`=="Greater Manchester"), x = ~year, y = ~movingav, color = ~`SIC07 code`,
        text = ~paste("Sector:", `SIC07 description`),  # Add this line for hover text
        hoverinfo = 'text+y+x',
        type = 'scatter', mode = 'lines+markers', line = list(shape = 'linear')) %>%
  layout(title = "Yearly values by SIC", 
         xaxis = list(title = "Year"), 
         yaxis = list(title = "Value"),
         # yaxis = list(title = "Value", type='log'),
         showlegend = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#LOCATION QUOTIENTS FOR GVA CURRENT PRICES, ITL2----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#ITL2 SIC removes (different for GB-level data from the same excel doc)
SICremoves = c(
  'Total',
  'A-E',
  'A (1-3)',
  'C (10-33)',
  'CA (10-12)',
  'CB (13-15)',
  'CC (16-18)',
  'CG (22-23)',
  'CH (24-25)',
  'CL (29-30)',
  'CM (31-33)',
  'F (41-43)',
  'G-T',
  'G (45-47)',
  'H (49-53)',
  'I (55-56)',
  'J (58-63)',
  'K (64-66)',
  'L (68)',#real estate activities - leaves in "Real estate activities, excluding imputed rental" & "Owner-occupiers' imputed rental" as separate categories
  'M (69-75)',
  'N (77-82)',
  'Q (86-88)',
  'R (90-93)',
  'S (94-96)'
)

#Using 'current prices' - LQs are purely proportional at single time points
#So across-time comparisons only matter for the proportions, not the nominal values
#Given that - the GVA current price values actually sum correctly across industries and within regions (unlike volume-chained)
itl2.cp <- read_csv('data/sectors/Table 2c ITL2 UK current price estimates pounds million.csv')

#Filter out duplicate value rows and make long by year
#Also convert year to numeric
itl2.cp <- itl2.cp %>% filter(!`SIC07 code` %in% SICremoves) %>% 
  pivot_longer(`1998`:`2021`, names_to = 'year', values_to = 'value') %>% 
  mutate(year = as.numeric(year))



#Check on just water and air transport, where the neg values are
# itl2.cp %>% filter(`SIC07 description` == "Water and air transport") %>% View

#RANDOM NEGATIVE VALUES IN THERE
#Looking, I think just typos given previous data
#(And also makes no logical sense, so...)
# itl2.cp %>% filter(value < 0)

#NA any negative values in GVA, can't be trusted
#Only 2021 values for water transport
itl2.cp <- itl2.cp %>% 
  mutate(value = ifelse(value < 0, NA, value))


#check sums just for one year
#that is, make sure kept single categories add up to the correct total
# chk <- itl2.cp %>% 
#   filter(year == 2021) %>% 
#   group_by(`ITL region name`) %>% 
#   summarise(mytotal = sum(value))
# 
# #Check against totals in orig
# chk2 <- read_csv('data/sectors/Table 2c ITL2 UK current price estimates pounds million.csv') %>% 
#   filter(`SIC07 code` == 'Total') %>% 
#   pivot_longer(`1998`:`2021`, names_to = 'year', values_to = 'originaltotal') %>% 
#   filter(year == 2021) %>% 
#   arrange(`ITL region name`)
# 
# #Rounding error diffs I think, all fine
# #Use totals here for proportions so they're accurate / sum to 1
# chk <- chk %>% 
#   left_join(chk2, by = "ITL region name") %>% 
#   mutate(diff = originaltotal-mytotal)


#https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/datasets/locationquotientdataandindustrialspecialisationforlocalauthorities
#Notes there and more info in the publication from it
#Explaining concentration and specialisation LQ being same number

#https://www.economicmodeling.com/wp-content/uploads/2007/10/emsi_understandinglq.pdf
#What do we need for yearly location quotients

#1. Find regional proportion of sector: sector x as proportion of all sectors in that region
#2. Find proportion of same sector for the UK as a whole (sum sector total, sum entire UK total, find proportion)
#3. LQ is relative proportion - 1 / 2. 
#4. < 1 = region has less concentration than nationally. > 1 = concentration is higher than nationally.

#For regional proportion calc, Add values for calc steps into the same DF
itl2.cp <- itl2.cp %>%
  group_by(`ITL region name`, year) %>% 
  mutate(
    region_totalsize = sum(value, na.rm = T),#a. Current price per region per year, for regional denominator
    sector_regional_proportion = value / region_totalsize#b. regional sector proportion (noting that a single row in this group is a single sector)
    ) %>% 
  group_by(year, `SIC07 code`) %>% 
  mutate(
    uk_sectorsize = sum(value, na.rm = T),#c. Summed current prices for EACH SECTOR, UK-wide
    ) %>% 
  group_by(year) %>% 
  mutate(
    uk_totalsize = sum(value, na.rm = T),#d. Summed current prices for WHOLE UK per year, for UK denominator
    sector_uk_proportion = uk_sectorsize / uk_totalsize#e. UK-level sector proportion
    ) %>% 
  mutate(
    LQ = sector_regional_proportion / sector_uk_proportion#f. Location quotient!
  ) %>% 
  mutate(LQ_log = log(LQ)) 

  
#Those regional values by themselves are going to be interesting:
#How has a region's OWN economy structure changed over time?

#Quick question:
#How do LQs spread across all regions? Some presumably wider spread than others, right?
#Pick a year or two
#OK, some silly values in there for places with tiny proportions
#Narrow down
#Too small numbers for decent density plot
# ggplot(itl2.cp %>% filter(year==2021), aes(x = LQ, colour = `SIC07 code`)) +
#   geom_density() +
#   coord_cartesian(xlim = c(-25,25))

#Check one LQ spread...
y <- itl2.cp %>% filter(year==2021, `SIC07 code` == 75)
plot(density(y$LQ))
hist(y$LQ,breaks = 10)

#TEST DUPLICATE
#WITHOUT PLUS ONE, DON'T NEED THAT, SHOULDN'T BE NEG VALUES
x <- itl2.cp %>% 
  filter(year==2021) %>% 
  mutate(LQ_log = log(LQ)) %>% 
  group_by(`SIC07 description`) %>% 
  summarise(
    mean = mean(LQ), min = min(LQ), max = max(LQ),
    mean_log = mean(LQ_log), min_log = min(LQ_log), max_log = max(LQ_log)
    ) 

#We do have a reasonable spread there. Would quite like to see.
ggplot(x %>% pivot_longer(min:max, names_to = 'minmax', values_to = 'value'), 
       aes(y = `SIC07 description`, x = value, colour = minmax)) +
  geom_point() + 
  coord_cartesian(xlim = c(-25,25))



#SAME

#Log version... this is looking more useful to me
#Reorder based on minimum values: close to zeroes, no sectoral presence there
minmaxes <- x %>% pivot_longer(min_log:max_log, names_to = 'minmax', values_to = 'value') 
minmaxes$`SIC07 description` <- factor(minmaxes$`SIC07 description`)
minmaxes$`SIC07 description` <- fct_reorder(minmaxes$`SIC07 description`, -rep(x$min_log, each = 2))

ggplot(minmaxes, 
       aes(y = `SIC07 description`, x = value, colour = minmax)) +
  geom_point() + 
  coord_cartesian(xlim = c(0,3)) +
  # geom_vline(xintercept = 0)#this value? I've added +1 to LQ before logging, so where 1 was the <> point, it becomes 2. Hence log 2 for the cutoff between "less than national / more than national".
  geom_vline(xintercept = log(2))#this value? I've added +1 to LQ before logging, so where 1 was the <> point, it becomes 2. Hence log 2 for the cutoff between "less than national / more than national".

#Though it would also be possible just to convert the 0-1 range to a more useful value by inverting
#So the ratios are properly readable / symmetric

#Actually, that's a very useful plot, UK structure wise:
#Note the line of near-zeroes for manufacturing
#Some places have very little of any sector
#Can get at what those patterns are as we proceed

#Can just add SY to that, in fact.
#Get the LQ for each sector in SY for 2021...
#And make log+1 value
sylq <- itl2.cp %>% 
  filter(`ITL region name` == 'South Yorkshire', year == 2021) %>% 
  mutate(LQplusone_log = log(LQ + 1)) 
  

#Keep only relevant columns from minmax and combine
minmaxes_plus_sy <- 
  rbind(
    minmaxes %>% select(`SIC07 description`,minmax,value),
    sylq %>% ungroup() %>% mutate(minmax = "SY") %>% select(`SIC07 description`,minmax, value = LQplusone_log)
  ) %>% 
  arrange(as.character(`SIC07 description`))#arranging purely for later factor ordering

#Order by SY
minmaxes_plus_sy$`SIC07 description` <- fct_reorder(minmaxes_plus_sy$`SIC07 description`, 
                                                    -rep(minmaxes_plus_sy$value[minmaxes_plus_sy$minmax=="SY"], each = 3))


ggplot(minmaxes_plus_sy, 
       aes(y = `SIC07 description`, x = value, colour = minmax, shape = minmax, size = minmax)) +
  geom_point() + 
  coord_cartesian(xlim = c(0,3)) +
  geom_vline(xintercept = log(2)) +#this value? I've added +1 to LQ before logging, so where 1 was the <> point, it becomes 2. Hence log 2 for the cutoff between "less than national / more than national".
  scale_shape_manual(values = c(16,16,17)) +
  scale_size_manual(values = c(2,2,3))




#Rather than minmax, let's do a full spread and place SY in that context
#I should make numbers for other places too for comparison

#View for 2021. Can just flag one we want to overlay?
#Something odd with water and air transport, remove until work out what
place = 'South Yorkshire'

x <- itl2.cp %>% filter(year == 2021, `SIC07 description` != 'Water and air transport') %>% mutate(flaggedplace = `ITL region name`==place)

#Ordering by the flagged place, bit awkward
x$`SIC07 description` <- factor(x$`SIC07 description`)
x$`SIC07 description` <- fct_relevel(
  x$`SIC07 description`, 
  unique(as.character(x$`SIC07 description`))[order(x %>% filter(`ITL region name`==place) %>%ungroup() %>% select(LQ_log) %>% pull(),decreasing = T)]
  )

#Actually, keep that order and use for animation below
ordertouse <- unique(as.character(x$`SIC07 description`))[order(x %>% filter(`ITL region name`==place) %>%ungroup() %>% select(LQ_log) %>% pull(),decreasing = T)]


ggplot(
  x, 
  aes(y = `SIC07 description`, x = LQ_log, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace)
  ) +
  geom_point() +
  scale_size_manual(values = c(2,4)) +
  scale_alpha_manual(values = c(0.2,1)) +
  scale_colour_manual(values = c('black','red')) +
  geom_vline(xintercept = 0, colour = 'blue') 
  # scale_shape_manual(values = c(16,24))

#test save size for animation... yep, good
# ggsave(filename = 'local/localimages/animations/test.png', height = 10, width = 10)


#ANIMATE THAT OVER ALL YEARS
#Bouncing off https://ryanpeek.org/2016-10-19-animated-gif_maps_in_r/
LQovertime_ggplot <- function(filteryear,placename){
  
  x <- itl2.cp %>% filter(year == filteryear, `SIC07 description` != 'Water and air transport') %>% mutate(flaggedplace = `ITL region name`==placename)
  
  #Ordering by the flagged place, bit awkward
  x$`SIC07 description` <- factor(x$`SIC07 description`)
  x$`SIC07 description` <- fct_relevel(
    x$`SIC07 description`, 
    ordertouse
  )

  ggplot(
    x, 
    aes(y = `SIC07 description`, x = LQplusone_log, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace)
  ) +
    geom_point() +
    coord_cartesian(xlim = c(0,3.5)) +
    scale_size_manual(values = c(2,4)) +
    scale_alpha_manual(values = c(0.2,1)) +
    scale_colour_manual(values = c('black','red')) +
    geom_vline(xintercept = log(2), colour = 'blue') +
    annotate("text", x = 2.5, y = 15, label = filteryear, size = 15) +
    annotate("text", x = 2.5, y = 38, label = filteryear, size = 15) +
    annotate("text", x = 2.5, y = 61, label = filteryear, size = 15)
  
  ggsave(filename = paste0('local/localimages/animations/',filteryear,'.png'), height = 10, width = 10)
  
}

#Change plotting order for SICs before running function
x <- itl2.cp %>% filter(year == 2009, `SIC07 description` != 'Water and air transport') %>% mutate(flaggedplace = `ITL region name`==place)
ordertouse <- unique(as.character(x$`SIC07 description`))[order(x %>% filter(`ITL region name`==place) %>%ungroup() %>% select(LQplusone_log) %>% pull(),decreasing = T)]

#Create images in folder
lapply(1998:2021, function(x) LQovertime_ggplot(x, 'South Yorkshire'))

list.files(path = "local/localimages/animations/", pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=2) %>% # animates, can opt for number of loops
  image_write("local/localimages/animations/LQ_overtime_w_SY_2009baseyear.gif")





#Plot just one place statically


#Random thing: grouped df won't plot lines: https://stackoverflow.com/questions/50889627/plotly-does-not-show-lines
sy <- itl2.cp %>% filter(`ITL region name` == 'South Yorkshire') %>% ungroup() %>% 
  filter(`SIC07 description`!='Water and air transport')#negative values

sy <- itl2.cp %>% filter(`ITL region name` == 'Greater Manchester') %>% ungroup() %>% 
  filter(`SIC07 description`!='Water and air transport')#negative values

sy <- itl2.cp %>% filter(`ITL region name` == 'Leicestershire, Rutland and Northamptonshire') %>% ungroup() %>% 
  filter(`SIC07 description`!='Water and air transport')#negative values

#MOVING AVERAGE
#https://stackoverflow.com/questions/26198551/rolling-mean-moving-average-by-group-id-with-dplyr
sy <- sy %>% 
  group_by(`SIC07 code`) %>% 
  mutate(movingav = rollapply(LQ,5,mean,align='right',fill=NA))

plot_ly(data = sy, x = ~year, y = ~LQ, color = ~`SIC07 code`,
# plot_ly(data = sy, x = ~year, y = ~movingav, color = ~`SIC07 code`,
        text = ~paste("Sector:", `SIC07 description`),  # Add this line for hover text
        hoverinfo = 'text+y+x',
        type = 'scatter', mode = 'lines+markers', line = list(shape = 'linear')) %>%
  layout(title = "Yearly values by SIC", 
         xaxis = list(title = "Year"), 
         yaxis = list(title = "Value", type='log'),
         # yaxis = list(title = "Value"),
         showlegend = TRUE)

#Sector regional proportion change by itself?
plot_ly(data = sy, x = ~year, y = ~sector_regional_proportion, color = ~`SIC07 code`,
# plot_ly(data = sy, x = ~year, y = ~movingav, color = ~`SIC07 code`,
        text = ~paste("Sector:", `SIC07 description`),  # Add this line for hover text
        hoverinfo = 'text+y+x',
        type = 'scatter', mode = 'lines+markers', line = list(shape = 'linear')) %>%
  layout(title = "Yearly values by SIC", 
         xaxis = list(title = "Year"), 
         # yaxis = list(title = "Value", type='log'),
         yaxis = list(title = "Value"),
         showlegend = TRUE)
 



#That last one shows SY basic metals having a slow decline then massive jump
#How did it change proprortionally across places?
onesectorallplaces <- itl2.cp %>% filter(`SIC07 description` == unique(itl2.hl$`SIC07 description`)[grepl('basic metal',unique(itl2.hl$`SIC07 description`))]) %>% ungroup() 
onesectorallplaces <- itl2.cp %>% filter(`SIC07 description` == unique(itl2.hl$`SIC07 description`)[grepl('Owner-occupiers',unique(itl2.hl$`SIC07 description`))]) %>% ungroup() 

#MOVING AVERAGE
#https://stackoverflow.com/questions/26198551/rolling-mean-moving-average-by-group-id-with-dplyr
onesectorallplaces <- onesectorallplaces %>% 
  group_by(`ITL region code`) %>% 
  mutate(
    movingav = rollapply(LQ,5,mean,align='right',fill=NA),
    movingav_regionalprop = rollapply(sector_regional_proportion,5,mean,align='right',fill=NA)
    ) %>% 
  ungroup()

plot_ly(data = onesectorallplaces, x = ~year, y = ~movingav, color = ~`ITL region code`,
# plot_ly(data = onesectorallplaces, x = ~year, y = ~LQ, color = ~`ITL region code`,
        text = ~paste(`ITL region name`),  # Add this line for hover text
        hoverinfo = 'text+y+x',
        type = 'scatter', mode = 'lines+markers', line = list(shape = 'linear')) %>%
  layout(title = "Yearly values by SIC", 
         xaxis = list(title = "Year"), 
         yaxis = list(title = "Value", type='log'),
         # yaxis = list(title = "Value"),
         showlegend = F)


#Actual proportion of that sector in the different places over time?
plot_ly(data = onesectorallplaces, x = ~year, y = ~movingav_regionalprop, color = ~`ITL region code`,
        # plot_ly(data = onesectorallplaces, x = ~year, y = ~LQ, color = ~`ITL region code`,
        text = ~paste(`ITL region name`),  # Add this line for hover text
        hoverinfo = 'text+y+x',
        type = 'scatter', mode = 'lines+markers', line = list(shape = 'linear')) %>%
  layout(title = "Yearly values by SIC", 
         xaxis = list(title = "Year"), 
         # yaxis = list(title = "Value", type='log'),
         yaxis = list(title = "Value"),
         showlegend = F)




#Location quotient vs proportion in the region (already calculated) seems obvious, though note issues with LQ maths
# plot_ly(data = sy %>% filter(DATE==2015), x = ~LQ, y = ~sector_regional_proportion, color = ~INDUSTRY_NAME,
plot_ly(data = sy %>% filter(year==2021), x = ~LQ, y = ~sector_regional_proportion,
        text = ~paste("Sector:", `SIC07 description`, "\npercent: ", sector_regional_proportion * 100, "\nvalue: ", value),  # Add this line for hover text
        hoverinfo = 'text+y+x',
        type = 'scatter',
        size = 7) %>%
  layout(title = "Yearly values by SIC", 
         # xaxis = list(title = "LQ"),
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





#~~~~~~~~~~~~~~~~~~~~~~~~~
#LQ/ITL2 CHANGE CHARTS----
#~~~~~~~~~~~~~~~~~~~~~~~~~

#Originally made for BRES data, repeat code that adds change over time (via OLS) as colour

#Create a function that fits lm and returns slope
get_slope <- function(data) {
  model <- lm(LQ_log ~ year, data = data, na.action = na.omit)
  coef(model)[2]
}

#Make it a safe function using purrr::possibly
#(Issue with some data being all NAs)
safe_get_slope <- purrr::possibly(get_slope, otherwise = 0)

l <- itl2.cp %>%
  group_by(`ITL region name`, `SIC07 description`) %>%
  nest() %>%
  mutate(slope = map_dbl(data, safe_get_slope)) %>%
  select(-data) %>% 
  mutate(slope = ifelse(is.na(slope),0,slope)) %>% 
  arrange(slope)


#Break into groups of differing slope
diffchange <- l %>% 
  filter(slope!=0) %>% 
  group_by(`ITL region name`) %>% 
  mutate(group = as.integer(cut_number(slope,7))) %>% 
  ungroup() %>% 
  rename(difftotal = slope)


#Look at some for sanity check
place = 'South Yorkshire'
place = 'Greater Manchester'

#climbers
industries = diffchange %>% filter(`ITL region name` == place, group %in% c(7)) %>% select(`SIC07 description`) %>% pull()
#droppers
industries = diffchange %>% filter(`ITL region name` == place, group %in% c(4)) %>% select(`SIC07 description`) %>% pull()
#middle
industries = diffchange %>% filter(`ITL region name` == place, group %in% c(1)) %>% select(`SIC07 description`) %>% pull()

#Select those industries / place
x <- itl2.cp %>% filter(`ITL region name` == place, `SIC07 description` %in% industries) 


#Check one set of slopes
# sy <- itl2.cp %>% filter(`ITL region name` == place) %>% 
#   inner_join(l, by = c("ITL region name","SIC07 description"))
# 
# #unique slope values in order
# slopeorder <- unique(sy$slope)[order(unique(sy$slope))]
# 
# x <- sy %>% filter(slope %in% slopeorder[1:10])

y <- x %>% 
  group_by(`SIC07 description`) %>% 
  arrange(year) %>% 
  mutate(
    LQ = ifelse(LQ == 0, NA, LQ),#Set to NA so plotly won't show
    movingav = rollapply(LQ,3,mean,align='right',fill=NA)
    # LQ_movingav = rollapply(LQ,3,mean,align='right',fill=NA),0)#Have a count moving average too, so it matches the percent (so count orders are correct vertically)
  )

plot_ly(data = y, x = ~year, y = ~movingav, color = ~`SIC07 description`,
# plot_ly(data = y, x = ~year, y = ~LQ, color = ~`SIC07 description`,
        # text = ~paste("Sector:", `SIC07 description`, "\nGVA: ",value, "\nslope: ",slope),  # Add this line for hover text
        text = ~paste("Sector:", `SIC07 description`, "\nGVA: ",value),  # Add this line for hover text
        hoverinfo = 'text',
        type = 'scatter', mode = 'lines+markers', line = list(shape = 'linear')) %>%
  layout(title = "Yearly percents by SIC", 
         xaxis = list(title = "Year"), 
         yaxis = list(title = "Value", type='log'),
         # yaxis = list(title = "Value"),
         showlegend = F)





#LQ PLOTS
x <- itl2.cp %>% filter(year == 2021)

#Add slopes into data to get LQ plots
x <- x %>% 
  left_join(
    diffchange %>% select(-group),
    by = c("ITL region name","SIC07 description")
  )

#Get min max values over time as well, to add as bars so range of sector is easy to see
minmaxes <- itl2.cp %>% 
  group_by(`SIC07 description`,`ITL region name`) %>% 
  summarise(
    minn = min(LQ),
    maxx = max(LQ)
  )

x <- x %>% 
  left_join(
    minmaxes,
    by = c("ITL region name","SIC07 description")
  )


#Factor
x$`SIC07 description` <- factor(x$`SIC07 description`)
x$`SIC07 description` <- fct_relevel(
  x$`SIC07 description`, 
  unique(as.character(x$`SIC07 description`))[order(x %>% filter(`ITL region name`==place) %>%ungroup() %>% select(LQ) %>% pull(),decreasing = T)]
)








#LQ PLOT
#Function for overlaying specific places / ITL2 zones
addplace_to_LQplot <- function(plot_to_addto, place, shapenumber=16,backgroundcolour='black', add_gva = F, setalpha = 1, addminmax = F){
  
  plot_to_addto <- plot_to_addto +
    geom_point(
      data = x %>% filter(`ITL region name` == place, difftotal > 0), 
      # aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace, size = COUNT)) +
      aes(y = `SIC07 description`, x = LQ, size = difftotal *1.75),
      shape = shapenumber,
      colour = backgroundcolour,
      alpha = setalpha
    ) +
    geom_point(
      data = x %>% filter(`ITL region name` == place, difftotal < 0), 
      # aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace, size = COUNT)) +
      aes(y = `SIC07 description`, x = LQ, size = difftotal *-1.75),
      shape = shapenumber,
      colour = backgroundcolour,
      alpha = setalpha
    ) +
    geom_point(
      data = x %>% filter(`ITL region name` == place, difftotal > 0), 
      # aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace, size = COUNT)) +
      aes(y = `SIC07 description`, x = LQ, size = difftotal),
      shape = shapenumber,
      colour = 'green',
      alpha = setalpha
    ) +
    geom_point(
      data = x %>% filter(`ITL region name` == place, difftotal < 0), 
      # aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace, size = COUNT)) +
      aes(y = `SIC07 description`, x = LQ, size = difftotal * -1),
      shape = shapenumber,
      colour = 'red',
      alpha = setalpha
    ) 
  
  
  if(add_gva){
    
    plot_to_addto <- plot_to_addto +  
      geom_text(
        data = x %>% filter(`ITL region name` == place), 
        aes(y = `SIC07 description`, x = 20, label = paste0('£',value,'M, ',round(sector_regional_proportion * 100, 2),'%')),
        # aes(y = INDUSTRY_NAME, x = max(LQ) + 2, label = COUNT),
        nudge_x = 0.3, hjust = 1, alpha = 0.7, size = 3
      )
    
    if(addminmax){
      
      plot_to_addto <- plot_to_addto +
        geom_errorbar(
          data = x %>% filter(`ITL region name` == place),
          aes(y = `SIC07 description`, xmin = minn, xmax = maxx)
          )
      
    }
    
    
  }
  
  return(plot_to_addto)
  
}




place1 = 'South Yorkshire'
place2 = 'Greater Manchester'

place1 = 'Greater Manchester'
place2 = 'South Yorkshire'

place1 = 'West Yorkshire'
place2 = 'South Yorkshire'

place1 = 'Leicestershire, Rutland and Northamptonshire'
place2 = 'South Yorkshire'

place1 = 'Dorset and Somerset'
place2 = 'South Yorkshire'

place1 = 'West Midlands'
place2 = 'South Yorkshire'


#Factor
x$`SIC07 description` <- factor(x$`SIC07 description`)
x$`SIC07 description` <- fct_relevel(
  x$`SIC07 description`, 
  unique(as.character(x$`SIC07 description`))[order(x %>% filter(`ITL region name`==place1) %>%ungroup() %>% select(LQ) %>% pull(),decreasing = T)]
)


#Base plot
p <- ggplot() +
  geom_point(
    data = x %>% filter(difftotal > 0), 
    # aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace, size = COUNT)) +
    aes(y = `SIC07 description`, x = LQ, size = difftotal),
    alpha = 0.1,
    shape = 16,
    colour = 'green'
  ) +
  geom_point(
    data = x %>% filter(difftotal < 0), 
    # aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace, size = COUNT)) +
    aes(y = `SIC07 description`, x = LQ, size = difftotal * -1),
    alpha = 0.1,
    shape = 16,
    colour = 'red'
  )  +
  scale_size_continuous(range = c(1,17)) +
  scale_x_continuous(trans = "log10", limits = c(0.001, 51)) +
  geom_vline(xintercept = 1, colour = 'blue') +
  guides(size = F) 



#Add a place
p <- addplace_to_LQplot(plot_to_addto = p, place = place2, shapenumber = 18,backgroundcolour = '#9ac0db', setalpha = 0.7)
p <- addplace_to_LQplot(plot_to_addto = p, place = place1, shapenumber = 16, add_gva = T, addminmax = T)
p


ggsave(plot = p, filename = paste0('local/localimages/gva_',place1,'_v_',place2,'_plot.png'), width = 12, height = 12)





#UDPATE: THIS IS PROBABLY BETTER DONE WITH THE TIME-BARS IN THE PLOT ABOVE
#It would be good to see how radically the structure's changed over time
#No animation, let's just save for each year - using one year as reference for the factor ordering

#Assuming we've looked at 2021 first above (which we have)
ordertouse <- unique(as.character(x$`SIC07 description`))[order(x %>% filter(`ITL region name`==place) %>%ungroup() %>% select(LQ) %>% pull(),decreasing = T)]


for(yeartosave in 1998:2021){


  x <- itl2.cp %>% filter(year == yeartosave)
  
  #Add slopes into data to get LQ plots
  x <- x %>% 
    left_join(
      diffchange %>% select(-group),
      by = c(c("ITL region name","SIC07 description"))
    )
  
  #Factor
  x$`SIC07 description` <- factor(x$`SIC07 description`)
  x$`SIC07 description` <- fct_relevel(
    x$`SIC07 description`, 
    ordertouse
  )
  
  #Base plot
  p <- ggplot() +
    geom_point(
      data = x %>% filter(difftotal > 0), 
      # aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace, size = COUNT)) +
      aes(y = `SIC07 description`, x = LQ, size = difftotal),
      alpha = 0.1,
      shape = 16,
      colour = 'green'
    ) +
    geom_point(
      data = x %>% filter(difftotal < 0), 
      # aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace, size = COUNT)) +
      aes(y = `SIC07 description`, x = LQ, size = difftotal * -1),
      alpha = 0.1,
      shape = 16,
      colour = 'red'
    )  +
    scale_size_continuous(range = c(1,17)) +
    scale_x_continuous(trans = "log10", limits = c(0.01,15)) +
    geom_vline(xintercept = 1, colour = 'blue') +
    guides(size = F) +
    ggtitle(yeartosave) +
    # coord_cartesian(xlim = c(0,12)) +
    annotate("text", x = 0.02, y = 15, label = yeartosave, size = 15) +
    annotate("text", x = 0.02, y = 38, label = yeartosave, size = 15) +
    annotate("text", x = 0.02, y = 61, label = yeartosave, size = 15)
  
  
  #Add a place
  p <- addplace_to_LQplot(plot_to_addto = p, place = 'Greater Manchester', shapenumber = 18,backgroundcolour = '#9ac0db', setalpha = 0.7)
  p <- addplace_to_LQplot(plot_to_addto = p, place = 'South Yorkshire', shapenumber = 16, add_gva = T)
  
  ggsave(filename = paste0('local/localimages/GVA_LQplots/',yeartosave,'.png'), height = 10, width = 10)  

}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#GETTING BRES EMPLOYMENT NUMBERS FROM 5 DIGIT SUMS----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#The point: 5 digit SICs in BRES have more granular employee count numbers
#That can be summed to get finer 2 digit SIC sums

#Structure of dataset will be the same for doing both, so can function that up can't I?

add_jobcount_from5digit_summedTo2digit <- function(df){
  
  #GET SIC lookup
  SIClookup <- read_csv('data/SIClookup.csv')
  
  df <- df %>% 
    left_join(SIClookup %>% select(-SIC_5DIGIT_NAME), by = c('INDUSTRY_CODE' = 'SIC_5DIGIT_CODE'))
  
  #Get per region / per year / per 2 digit sector job count sums from 5 digit
  df %>% 
    group_by(DATE,GEOGRAPHY_NAME,SIC_2DIGIT_CODE) %>% 
    summarise(COUNT = sum(COUNT))
  
}

bres_FT_from5digit <- add_jobcount_from5digit_summedTo2digit(readRDS('data/sectors/ITL2_fulltimeemployeecountandpercent5digitSIC_BRESopen15to21.rds'))

bres_EMPLOYMENT_from5digit <- add_jobcount_from5digit_summedTo2digit(readRDS('data/sectors/ITL2_Employment_countandpercent5digitSIC_BRESopen15to21.rds'))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Linking BRES EMPLOYMENT TO GVA----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Towards gva per job

#Starting with a look at what SIC codes we've got in both.
#Reminder: the number of categories for the GVA data is different depending on the geography scale, dropping for each smaller geography
#Another reminder: the cruder SIC cats for BRES will have more accurate employee count numbers because rounding will be on the larger numbers

#Looking again at a single BRES year to get the SICs
br <- readRDS('local/data/BRES_NUTS2_2021.rds')

# division (2 digit)    88
# group (3 digit)      273
# class (4 digit)      616
# subclass (5 digit)   729
br %>% 
  group_by(INDUSTRY_TYPE) %>% 
  distinct(INDUSTRY_NAME) %>% 
  summarise(count = n())

#For the ITL2 level GVA data... 72
#So these are division, but a bit less. Some are presumably going to need employee counts aggregating
length(unique(itl2.cp$`SIC07 description`))

#from BRES
sic2 <- br %>% filter(INDUSTRY_TYPE=='SIC 2007 division (2 digit)') %>% 
  distinct(INDUSTRY_NAME, .keep_all = T) %>% 
  select(BRES_INDUSTRY_NAME=INDUSTRY_NAME,BRES_INDUSTRY_CODE=INDUSTRY_CODE)


#From GVA data
gv <- itl2.cp %>% 
  ungroup() %>% 
  distinct(`SIC07 description`, .keep_all = T) %>% 
  select(GVA_INDUSTRY_NAME = `SIC07 description`, GVA_INDUSTRY_CODE = `SIC07 code`)


#Some automatching to be done. Some notes:
#The GVA data has 68 and 68IMP. But the BRES data at 2-digit level only has 68, so it's both combined.
#I want to be able to separate those, so might pull from...
sic3v <- unique(
  br %>% filter(INDUSTRY_TYPE=='SIC 2007 group (3 digit)')
  %>% select(INDUSTRY_NAME) %>% pull
)

sic4v <- unique(
  br %>% filter(INDUSTRY_TYPE=='SIC 2007 class (4 digit)')
  %>% select(INDUSTRY_NAME) %>% pull
)

sic5v <- unique(
  br %>% filter(INDUSTRY_TYPE=='SIC 2007 subclass (5 digit)')
  %>% select(INDUSTRY_NAME) %>% pull
)

#Where's imputed? It's in there somewhere isn't it? Or did I imagine that?
#Might not be as there's no associated jobs?
#Newp, doesn't have it. I did in fact imagine it being in the BRES industry list, it's not.
sic3v[grepl('imput',sic3v,ignore.case = T)]
sic4v[grepl('imput',sic4v,ignore.case = T)]
sic5v[grepl('imput',sic5v,ignore.case = T)]

#Which is easier - can just exclude imputed from GVA/BRES link, no jobs anyway

#Pull out numbers in brackets, don't need the rest
#https://stackoverflow.com/a/8613332
repl <- regmatches(gv$GVA_INDUSTRY_CODE, gregexpr("(?<=\\().*?(?=\\))", gv$GVA_INDUSTRY_CODE, perl=T))
repl[lengths(repl) == 0] <- NA
repl <- unlist(repl)

gv$GVA_INDUSTRY_CODE <- ifelse(is.na(repl), gv$GVA_INDUSTRY_CODE, repl)
#And can drop imputed rent, we can't match against that
gv <- gv %>% filter(GVA_INDUSTRY_CODE!='68IMP')






#CORRECT NON MATCHING ITL2 NAMES
#Are there also any issues with non-matching ITL2 names? 
#NI will be missing anyway, GVA data is UK, BRES is GB, but...
table(unique(itl2.cp$`ITL region name`) %in% unique(br$GEOGRAPHY_NAME))
unique(itl2.cp$`ITL region name`)[!unique(itl2.cp$`ITL region name`) %in% unique(br$GEOGRAPHY_NAME)]
unique(br$GEOGRAPHY_NAME)[!unique(br$GEOGRAPHY_NAME) %in% unique(itl2.cp$`ITL region name`)]

#Yup. Have already checked geography matches elsewhere, just need a name tweak
itl2.cp$`ITL region name`[itl2.cp$`ITL region name` == 'Northumberland, and Tyne and Wear'] <- 'Northumberland and Tyne and Wear'
itl2.cp$`ITL region name`[itl2.cp$`ITL region name` == 'West Wales and The Valleys'] <- 'West Wales'





#The BRES data at 2 digit SIC level
bres <- readRDS('data/sectors/ITL2_fulltimeemployeecountandpercent2digitSIC_BRESopen15to21.rds') %>% 
  mutate(INDUSTRY_CODE_NUMERIC = as.numeric(INDUSTRY_CODE))

#Add in the 5-digit-summed job counts
#Check industry code match... tick
table(bres$INDUSTRY_CODE %in% bres_FT_from5digit$SIC_2DIGIT_CODE)

bres <- bres %>% 
  left_join(
    bres_FT_from5digit %>% rename(COUNT_5DIGIT = COUNT),
    by = c('DATE','GEOGRAPHY_NAME','INDUSTRY_CODE'='SIC_2DIGIT_CODE')
  )


#PLAN:
#if we mark the bres categories with group names
#Such that ones that need collating are in the same group
#Can then just do easily with dplyr

#Create a copy of GV where the codes are expanded to 1 each per row
#But the names will be duplicates
#Can then merge on the codes, and then group by these names to create the groups
#Is the theory
df <- gv

# Split the rows with hyphenated codes
split_rows <- function(row) {
  start_code <- as.numeric(strsplit(row$GVA_INDUSTRY_CODE, "-")[[1]][1])
  end_code <- as.numeric(strsplit(row$GVA_INDUSTRY_CODE, "-")[[1]][2])
  
  codes <- as.character(start_code:end_code)
  data.frame(
    GVA_INDUSTRY_NAME = rep(row$GVA_INDUSTRY_NAME, length(codes)),
    GVA_INDUSTRY_CODE = codes,
    stringsAsFactors = FALSE
  )
}

# Identify the rows to be split
rows_to_split <- grepl("-", df$GVA_INDUSTRY_CODE)

# Use lapply to preserve dataframe structure and then rbind to combine the rows
expanded_rows <- do.call(rbind, lapply(1:nrow(df), function(i) {
  if (rows_to_split[i]) {
    split_rows(df[i, , drop = FALSE])
  }
}))

# Remove the hyphenated rows from the original dataframe
df <- df[!rows_to_split, ]

# Combine the two dataframes
df <- rbind(df, expanded_rows)

df <- df %>% 
  mutate(GVA_INDUSTRY_CODE_NUMERIC = as.numeric(GVA_INDUSTRY_CODE))


#JOIN TO BRES EMPLOYMENT COUNTS
#Reminder of employee cats in the BRES data
## Employees: An employee is anyone aged 16 years or over that an organisation directly pays from its payroll(s), in return for carrying out a full-time or part-time job or being on a training scheme. It excludes voluntary workers, self-employed, working owners who are not paid via PAYE. 
# Full-time employees: those working more than 30 hours per week.
# Part-time employees: those working 30 hours or less per week.
# Employment includes employees plus the number of working owners. BRES therefore includes self-employed workers as long as they are registered for VAT or Pay-As-You-Earn (PAYE) schemes. Self employed people not registered for these, along with HM Forces and Government Supported trainees are excluded.
#And note, no gig economy jobs: 
#https://www.ons.gov.uk/aboutus/transparencyandgovernance/freedomofinformationfoi/workersinthegigeconomy


#87 in the expanded GVA, 88 in BRES
#Let's see where's different
table(df$GVA_INDUSTRY_CODE_NUMERIC %in% bres$INDUSTRY_CODE_NUMERIC)
table(unique(bres$INDUSTRY_CODE_NUMERIC) %in% df$GVA_INDUSTRY_CODE_NUMERIC)

#"99 : Activities of extraterritorial organisations and bodies"
unique(bres$INDUSTRY_CODE_NUMERIC)[!unique(bres$INDUSTRY_CODE_NUMERIC) %in% df$GVA_INDUSTRY_CODE_NUMERIC]

#Which I suspect are all zeroes anyway... tick
bres %>% filter(INDUSTRY_NAME == '99 : Activities of extraterritorial organisations and bodies') %>% 
  summarise(sum(COUNT))


#So can drop those via inner join, the rest match
#Tick, looks good
both <- bres %>% 
  inner_join(
    df,
    by = c('INDUSTRY_CODE_NUMERIC' = 'GVA_INDUSTRY_CODE_NUMERIC')
    )


#Can now group by GVA industry name and sum the counts
bres_w_gvacodes <- both %>% 
  group_by(GVA_INDUSTRY_NAME, GEOGRAPHY_NAME, DATE) %>%
  summarise(
    COUNT = sum(COUNT, na.rm=T),
    COUNT_5DIGIT = sum(COUNT_5DIGIT, na.rm=T),
    INDUSTRY_CODE = max(GVA_INDUSTRY_CODE)
    )

#All that needs now is the original number code merged back in, so we have those labelled
bres_w_gvacodes <- bres_w_gvacodes %>% 
  left_join(
    gv,
    by = 'GVA_INDUSTRY_NAME'
    ) %>% 
  select(-INDUSTRY_CODE) %>% 
  rename(INDUSTRY_CODE = GVA_INDUSTRY_CODE, INDUSTRY_NAME = GVA_INDUSTRY_NAME)


#Think we might have some zero job counts on at least one other sector?
#Activities of households.
#Remove that one too, note no jobs data there
bres_w_gvacodes %>% 
  group_by(INDUSTRY_NAME) %>% 
  summarise(COUNT= sum(COUNT)) %>% 
  arrange(-COUNT) %>% 
  print(n=80)

bres_w_gvacodes <- bres_w_gvacodes %>% 
  filter(INDUSTRY_NAME!='Activities of households')

#Keep matching sectors, join
gva_w_jobcounts <- bres_w_gvacodes %>% 
  left_join(
    itl2.cp,
    by = c('DATE'='year','INDUSTRY_NAME'='SIC07 description','GEOGRAPHY_NAME'='ITL region name')
  ) %>% 
  rename(
    JOBCOUNT_FT = COUNT, 
    JOBCOUNT_FT_5DIGIT = COUNT_5DIGIT, 
    GVA = value
    )


#GVA per FT job
gva_w_jobcounts <- gva_w_jobcounts %>% 
  mutate(
    GVA_perFTjob = (GVA * 1000000) / JOBCOUNT_FT,
    GVA_perFTjob_5digit = (GVA * 1000000) / JOBCOUNT_FT_5DIGIT
    )



#REPEAT FOR "EMPLOYMENT". Reminder:
#"  "Employment" includes employees plus the number of working owners. BRES therefore includes self-employed workers as long as they are registered for VAT or Pay-As-You-Earn (PAYE) schemes. Self employed people not registered for these, along with HM Forces and Government Supported trainees are excluded."
bres_all <- readRDS('data/sectors/ITL2_Employment_countandpercent2digitSIC_BRESopen15to21.rds') %>% 
  mutate(INDUSTRY_CODE_NUMERIC = as.numeric(INDUSTRY_CODE))

#Add in the 5-digit-summed job counts
#Check industry code match... tick
table(bres_all$INDUSTRY_CODE %in% bres_EMPLOYMENT_from5digit$SIC_2DIGIT_CODE)

bres_all <- bres_all %>% 
  left_join(
    bres_EMPLOYMENT_from5digit %>% rename(COUNT_5DIGIT = COUNT),
    by = c('DATE','GEOGRAPHY_NAME','INDUSTRY_CODE'='SIC_2DIGIT_CODE')
  )


both <- bres_all %>% 
  inner_join(
    df,
    by = c('INDUSTRY_CODE_NUMERIC' = 'GVA_INDUSTRY_CODE_NUMERIC')
  )


#Can now group by GVA industry name and sum the counts
bres_all_w_gvacodes <- both %>% 
  group_by(GVA_INDUSTRY_NAME, GEOGRAPHY_NAME, DATE) %>%
  summarise(
    COUNT = sum(COUNT, na.rm=T),
    COUNT_5DIGIT = sum(COUNT_5DIGIT, na.rm=T),
    INDUSTRY_CODE = max(GVA_INDUSTRY_CODE)
  )

#All that needs now is the original number code merged back in, so we have those labelled
bres_all_w_gvacodes <- bres_all_w_gvacodes %>% 
  left_join(
    gv,
    by = 'GVA_INDUSTRY_NAME'
  ) %>% 
  select(-INDUSTRY_CODE) %>% 
  rename(INDUSTRY_CODE = GVA_INDUSTRY_CODE, INDUSTRY_NAME = GVA_INDUSTRY_NAME)


#Think we might have some zero job counts on at least one other sector?
#Activities of households.
#Remove that one too, note no jobs data there
bres_all_w_gvacodes %>% 
  group_by(INDUSTRY_NAME) %>% 
  summarise(
    COUNT= sum(COUNT, na.rm=T),
    COUNT_5DIGIT = sum(COUNT_5DIGIT, na.rm=T),
    ) %>% 
  arrange(-COUNT) %>% 
  print(n=80)

bres_all_w_gvacodes <- bres_all_w_gvacodes %>% 
  filter(INDUSTRY_NAME!='Activities of households')

#Keep matching sectors, join
gva_w_jobcounts_EMP <- bres_all_w_gvacodes %>% 
  left_join(
    itl2.cp,
    by = c('DATE'='year','INDUSTRY_NAME'='SIC07 description','GEOGRAPHY_NAME'='ITL region name')
  ) %>% 
  rename(
    JOBCOUNT_EMPLOYMENT = COUNT, 
    JOBCOUNT_EMPLOYMENT_5DIGIT = COUNT_5DIGIT, 
    GVA = value)


#GVA per FT job
gva_w_jobcounts_EMP <- gva_w_jobcounts_EMP %>% 
  mutate(
    GVA_per_EMPLOYMENT = (GVA * 1000000) / JOBCOUNT_EMPLOYMENT,
    GVA_per_EMPLOYMENT_5digit = (GVA * 1000000) / JOBCOUNT_EMPLOYMENT_5DIGIT
    )


#Add the second lot of job numbers to the same df
gva_w_jobcounts <- gva_w_jobcounts %>% 
  left_join(
    gva_w_jobcounts_EMP %>% select(INDUSTRY_NAME, GEOGRAPHY_NAME,DATE,JOBCOUNT_EMPLOYMENT,JOBCOUNT_EMPLOYMENT_5DIGIT,GVA_per_EMPLOYMENT,GVA_per_EMPLOYMENT_5digit),
    by = c('INDUSTRY_NAME','GEOGRAPHY_NAME','DATE')
  ) %>% 
  relocate(GVA, .before = GVA_perFTjob) %>% 
  relocate(JOBCOUNT_FT, .before = GVA_perFTjob) %>% 
  relocate(JOBCOUNT_FT_5DIGIT, .before = GVA_perFTjob)


#SAVE
saveRDS(gva_w_jobcounts, 'data/UK_GVA_with_BRES_jobcounts_and_jobcounts_summedfrom5digitSIC.rds')


#Just some sanity checks
#Absolutely nothing far off the line here
ggplot(gva_w_jobcounts, aes(x = JOBCOUNT_FT, y = JOBCOUNT_FT_5DIGIT)) +
  geom_point(alpha=0.05) +
  facet_wrap(~DATE)


#But I'm seeing some unrealistic volality across years?
#Which may purely be a BRES sampling issue, but...?
#Let's check on per year differences
#Probably worth picking out ones with largest SD per sector/place for the log diffs
#Just to see what we're looking at here
diffz <- gva_w_jobcounts %>% 
  arrange(DATE) %>% 
  group_by(INDUSTRY_NAME,GEOGRAPHY_NAME) %>% 
  mutate(
    logchange = log(JOBCOUNT_FT_5DIGIT) - log(lag(JOBCOUNT_FT_5DIGIT)),
    change = JOBCOUNT_FT_5DIGIT - lag(JOBCOUNT_FT_5DIGIT),
    sd = sd(logchange, na.rm=T)
    )

#NOne 5 digit version  
diffz <- gva_w_jobcounts %>% 
  arrange(DATE) %>% 
  group_by(INDUSTRY_NAME,GEOGRAPHY_NAME) %>% 
  mutate(
    logchange = log(JOBCOUNT_FT) - log(lag(JOBCOUNT_FT)),
    change = JOBCOUNT_FT - lag(JOBCOUNT_FT),
    sd = sd(logchange, na.rm=T)
    )
  

#Check!
# diffz %>% filter(GEOGRAPHY_NAME == 'South Yorkshire') %>% View

# 
# ggplot(diffz, aes(x = DATE, y = change)) +
#   geom_point(alpha=0.3) +
#   # scale_y_continuous(trans = "log") +
#   facet_wrap(~INDUSTRY_NAME, scales = 'free_y')


#How back for some of the top SDs? 
topsds <- unique(diffz$sd)[order(unique(diffz$sd),decreasing = T)]

# ggplot(diffz %>% ungroup() %>% filter(sd %in% topsds[(2784-9):2784]), 
ggplot(diffz %>% ungroup() %>% filter(sd %in% topsds[runif(50,min=1,max=2784)]), 
# ggplot(diffz %>% ungroup() %>% filter(sd %in% topsds[1:50]), 
       aes(x = DATE, y = JOBCOUNT_FT, colour = INDUSTRY_NAME)
       ) +
  geom_line() +
  facet_wrap(~GEOGRAPHY_NAME, scales='free_y') +
  guides(colour=F)






#~~~~~~~~~~~~~~~~~~~~~~~~~~
#LOOKING AT GVA PER JOB----
#~~~~~~~~~~~~~~~~~~~~~~~~~~

#Question 1: how far apart are the GVA per job ratios for 'full time employees' vs 'employment'?
#See above for the difference...

#Find ratio, then let's see the spread.
#Note to self: this ratio is the same thing as the actual job number ratio
# gva_w_jobcounts <- gva_w_jobcounts %>% 
  # mutate(ratio = GVA_perFTjob / GVA_per_EMPLOYMENT)

#So let's just look at the job number ratio. I have checked this before, it's very close for "all employees" (that includes PT)
#But this...?
gva_w_jobcounts <- gva_w_jobcounts %>% 
  mutate(
    ratio = JOBCOUNT_FT / JOBCOUNT_EMPLOYMENT,
    ratio5 = JOBCOUNT_FT_5DIGIT / JOBCOUNT_EMPLOYMENT_5DIGIT
    )

#Check ratio differences for all places
ggplot(gva_w_jobcounts, aes(x = GEOGRAPHY_NAME, y = ratio)) +
# ggplot(gva_w_jobcounts, aes(x = GEOGRAPHY_NAME, y = ratio5)) +
  # geom_boxplot() +
  geom_violin(fill = 'white') +
  # geom_jitter(alpha = 0.01) +
  coord_flip()


#Sector breakdown also informative
#E.g. basic metals and heavier sectors - number of part time very small, so difference is very small
# ggplot(gva_w_jobcounts, aes(x = fct_reorder(INDUSTRY_NAME,ratio), y = ratio)) +
ggplot(gva_w_jobcounts, aes(x = fct_reorder(INDUSTRY_NAME,ratio5), y = ratio5)) +
  # geom_boxplot() +
  geom_violin(fill = 'white') +
  # geom_jitter(alpha = 0.01) +
  coord_flip()



#Next: add in total GVA per region / total GVA per employee per region










#Quick looks


gva_m <- gva_w_jobcounts %>%
  group_by(INDUSTRY_NAME,GEOGRAPHY_NAME) %>% 
  arrange(DATE) %>% 
  mutate(
    movingav_FT = rollapply(GVA_perFTjob_5digit,3,mean,align='right',fill=NA),
    movingav_EMP = rollapply(GVA_per_EMPLOYMENT_5digit,3,mean,align='right',fill=NA)
    ) %>% 
  ungroup()

#NOTE: THIS IS CURRENT PRICES - BEAR IN MIND FOR CHANGE OVER TIME
#The unsmoothed data is pretty chaotic...
plot_ly(data = gva_m %>% filter(grepl('basic metals',INDUSTRY_NAME), JOBCOUNT_FT >= 1000), x = ~DATE, y = ~movingav_FT, color = ~GEOGRAPHY_NAME,
# plot_ly(data = gva_m %>% filter(grepl('basic metals',INDUSTRY_NAME), JOBCOUNT_FT >= 1000), x = ~DATE, y = ~movingav_EMP, color = ~GEOGRAPHY_NAME,
# plot_ly(data = gva_m %>% filter(grepl('basic metals',INDUSTRY_NAME), JOBCOUNT_FT >= 1000), x = ~DATE, y = ~GVA_perFTjob_5digit, color = ~GEOGRAPHY_NAME,
# plot_ly(data = gva_m %>% filter(grepl('basic metals',INDUSTRY_NAME), JOBCOUNT_FT >= 1000), x = ~DATE, y = ~GVA_per_EMPLOYMENT_5digit, color = ~GEOGRAPHY_NAME,
        text = ~paste("Place:", GEOGRAPHY_NAME,'\nFull times: ', JOBCOUNT_FT_5DIGIT,'\nGVA: ',GVA,'\nGVA per employment: ',GVA_perFTjob_5digit),  # Add this line for hover text
        hoverinfo = 'text',
        type = 'scatter', mode = 'lines+markers', line = list(shape = 'linear')) %>%
  layout(title = "Yearly values by SIC", 
         xaxis = list(title = "Year"), 
         yaxis = list(title = "Value", type='log'),
         # yaxis = list(title = "Value"),
         showlegend = TRUE)


#Why some missing data?
#Hit zero on one year. But note also: this is going to be firms' head offices, hence the very high GVA.
#Which is also a reminder: local units probably the better data to be using.
chk <- gva_w_jobcounts %>% 
  filter(GEOGRAPHY_NAME ==  'Inner London - West', grepl('basic metals',INDUSTRY_NAME))





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#GVA per worker comparisons----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#From here, just going to use the 5 digits as they're more accurate.
gva_jobs5 <- gva_w_jobcounts %>% 
  select(-JOBCOUNT_FT,-GVA_perFTjob,-JOBCOUNT_EMPLOYMENT,-GVA_per_EMPLOYMENT,-ratio) %>% 
  rename(JOBCOUNT_FT=JOBCOUNT_FT_5DIGIT,GVA_perFTjob=GVA_perFTjob_5digit,JOBCOUNT_EMPLOYMENT=JOBCOUNT_EMPLOYMENT_5DIGIT,GVA_per_EMPLOYMENT=GVA_per_EMPLOYMENT_5digit,ratio=ratio5)

#MEAN GVA PER REGION, with ratio of that to each region's individual sectors
gva_jobs5 <- gva_jobs5 %>%
  group_by(DATE,GEOGRAPHY_NAME) %>% 
  mutate(
    mean_GVA_perFT = (sum(GVA,na.rm=T)/sum(JOBCOUNT_FT,na.rm=T))*1000000,
    mean_GVA_perEMP = (sum(GVA,na.rm=T)/sum(JOBCOUNT_EMPLOYMENT,na.rm=T))*1000000,
    ratio_GVA_perFT = GVA_perFTjob / mean_GVA_perFT,
    ratio_GVA_perEMP = GVA_per_EMPLOYMENT / mean_GVA_perEMP
    )


#Add in GVA per job as a proportion of GB total GVA (for the present sectors, missing imputed rent)
#For an easy comparison over time option
#That value will be "% or proportion this one job adds to the total".
#Which is going to be tiny, so may want to scale up
gva_jobs5 <- gva_jobs5 %>% 
  group_by(DATE) %>% 
  mutate(
    total_GB_GVA_peryear = sum(GVA, na.rm=T),
    GVA_aspercentofGBtotal_perFTjob = ((GVA_perFTjob / 1000000) / total_GB_GVA_peryear) * 100,#had been scaled up to pounds
    GVA_aspercentofGBtotal_perEMPLOYMENT = ((GVA_per_EMPLOYMENT / 1000000) / total_GB_GVA_peryear) * 100#had been scaled up to pounds
    )




#Check!
ggplot(
  gva_jobs5 %>% filter(DATE==2021),
  aes(x = GEOGRAPHY_NAME, y = ratio_GVA_perFT)
) +
  geom_boxplot() +
  # geom_violin(fill = 'white') +
  scale_y_log10() +
  coord_flip() +
  geom_hline(yintercept = 1)


#Just look at one or two of those directly...
#For SY, couple of things to note
#Warehousing, tiny GVA, huge numbers
#Many manuf sectors at or below 1
gva_jobs5 %>% 
  filter(GEOGRAPHY_NAME=='South Yorkshire',DATE==2021) %>% 
  select(INDUSTRY_NAME,GVA,JOBCOUNT_FT,GVA_perFTjob,mean_GVA_perFT,ratio_GVA_perFT,
         JOBCOUNT_EMPLOYMENT,GVA_per_EMPLOYMENT,mean_GVA_perEMP,ratio_GVA_perEMP) %>% 
  arrange(-ratio_GVA_perFT) %>% 
  View

gva_jobs5 %>% 
  filter(GEOGRAPHY_NAME=='Greater Manchester',DATE==2021) %>% 
  select(INDUSTRY_NAME,GVA,JOBCOUNT_FT,GVA_perFTjob,mean_GVA_perFT,ratio_GVA_perFT,
         JOBCOUNT_EMPLOYMENT,GVA_per_EMPLOYMENT,mean_GVA_perEMP,ratio_GVA_perEMP) %>% 
  arrange(-ratio_GVA_perFT) %>% 
  View




gva_m <- gva_jobs5 %>%
  group_by(INDUSTRY_NAME,GEOGRAPHY_NAME) %>% 
  arrange(DATE) %>% 
  mutate(
    movingav_FT = rollapply(ratio_GVA_perFT,3,mean,align='right',fill=NA),
    movingav_EMP = rollapply(ratio_GVA_perEMP,3,mean,align='right',fill=NA)
  ) %>% 
  ungroup()



#Let's see how ratios INTERNALLY RELATIVE TO REGION have changed
# plot_ly(data = gva_m %>% filter(GEOGRAPHY_NAME=='South Yorkshire', JOBCOUNT_FT >= 500), x = ~DATE, y = ~movingav_FT, color = ~INDUSTRY_NAME,
plot_ly(data = gva_m %>% filter(GEOGRAPHY_NAME=='South Yorkshire', JOBCOUNT_FT >= 500, !grepl('pension funding',INDUSTRY_NAME)), x = ~DATE, y = ~ratio_GVA_perFT, color = ~INDUSTRY_NAME,#remove insurance
# plot_ly(data = gva_m %>% filter(GEOGRAPHY_NAME=='South Yorkshire', JOBCOUNT_FT >= 500, !grepl('pension funding',INDUSTRY_NAME)), x = ~DATE, y = ~movingav_FT, color = ~INDUSTRY_NAME,#remove insurance
        text = ~paste("Sector:", INDUSTRY_NAME,'\nFull times: ', JOBCOUNT_FT,'\nGVA: ',GVA,'\nGVA per employment: ',GVA_perFTjob, '\nRatio to av: ', ratio_GVA_perFT),  # Add this line for hover text
        hoverinfo = 'text',
        type = 'scatter', mode = 'lines+markers', line = list(shape = 'linear')) %>%
  layout(title = "Yearly values by SIC", 
         xaxis = list(title = "Year"), 
         yaxis = list(title = "Value", type='log'),
         # yaxis = list(title = "Value"),
         showlegend = F)




#Test plots for GVA per worker for a single year
#All sectors, all places, adding in SY.
#Without doing the growth thing for now, let's just see the spread
#Good ol' "mark one place" strategy.
place = 'South Yorkshire'

gvax <- gva_jobs5 %>% filter(DATE == 2021) %>% mutate(flaggedplace = ifelse(GEOGRAPHY_NAME==place, 'A', 'B'))

#Ordering by the flagged place, bit awkward
gvax$INDUSTRY_NAME <- factor(gvax$INDUSTRY_NAME)
gvax$INDUSTRY_NAME <- fct_relevel(
  gvax$INDUSTRY_NAME, 
  unique(as.character(gvax$INDUSTRY_NAME))[order(gvax %>% filter(GEOGRAPHY_NAME==place) %>%ungroup() %>% select(GVA_perFTjob) %>% pull(),decreasing = T)]
)

ggplot(
  gvax %>% filter(JOBCOUNT_FT >= 500),
  aes(y = INDUSTRY_NAME, x = GVA_perFTjob, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace)
  ) +
  geom_point() +
  scale_x_continuous(limits = c(0,400000)) +
  scale_size_manual(values = c(4,2)) +
  scale_alpha_manual(values = c(1,0.2)) +
  scale_shape_manual(values = c(17,16)) +
  scale_colour_manual(values = c('red','black'))
  # geom_vline(xintercept = 1, colour = 'blue') 
  # facet_wrap(~SIC_2DIGIT_NAME,ncol = 8, scales = 'free_y') +
  # theme(axis.text.y=element_blank(), #remove x axis labels
        # axis.ticks.y=element_blank())



#Using proportions to national GVA total should look identical, right?
#Right. But can now repeat for all years and look for change / volatility
ggplot(
  gvax %>% filter(JOBCOUNT_FT >= 500),
  aes(y = INDUSTRY_NAME, x = GVA_aspercentofGBtotal_perFTjob, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace)
) +
  geom_point() +
  scale_x_continuous(limits = c(0,0.000024)) +
  scale_size_manual(values = c(4,2)) +
  scale_alpha_manual(values = c(1,0.2)) +
  scale_shape_manual(values = c(17,16)) +
  scale_colour_manual(values = c('red','black'))


#OK, so now we're happy that the proportions work
#Want to see what the volatility is like with these numbers over time before proceeding with any change analysis

#Keep one order... this is currently 2021, from above
ordertouse <- unique(as.character(gvax$INDUSTRY_NAME))[order(gvax %>% filter(GEOGRAPHY_NAME==place) %>%ungroup() %>% select(GVA_perFTjob) %>% pull(),decreasing = T)]

for(filteryear in 2015:2021){
  
  gvaxloop <- gva_jobs5 %>% filter(DATE == filteryear) %>% mutate(flaggedplace = ifelse(GEOGRAPHY_NAME==place, 'A', 'B'))
  
  #Ordering by the flagged place, bit awkward
  gvaxloop$INDUSTRY_NAME <- factor(gvaxloop$INDUSTRY_NAME)
  gvaxloop$INDUSTRY_NAME <- fct_relevel(
    gvaxloop$INDUSTRY_NAME, 
    ordertouse
  )
  
  ggplot(
    gvaxloop %>% filter(JOBCOUNT_FT >= 500),
        aes(y = INDUSTRY_NAME, x = GVA_aspercentofGBtotal_perFTjob, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace)
  ) +
    geom_point() +
    scale_x_continuous(limits = c(0,0.000024)) +
    scale_size_manual(values = c(4,2)) +
    scale_alpha_manual(values = c(1,0.2)) +
    scale_shape_manual(values = c(17,16)) +
    scale_colour_manual(values = c('red','black')) +
    ggtitle(filteryear)
  
  
  ggsave(filename = paste0('local/localimages/GVA_perworker_proportions/',filteryear,'.png'),width = 12, height = 12)
  
}




#Quick dirty version for adding arbitrary places
addplace <- function(p, df, place, countcutoff = 0, colourp, shapep){
  
  p <- p + geom_point(
    data = df %>% filter(GEOGRAPHY_NAME == place, JOBCOUNT_FT >= countcutoff),
    aes(y = INDUSTRY_NAME, x = GVA_aspercentofGBtotal_perFTjob), 
    shape = shapep, size = 4, colour = colourp
  )
  
}

#Base plot
countcutoff = 500

p <- ggplot() +
  geom_point(
    data = gvax %>% filter(JOBCOUNT_FT >= countcutoff),
    aes(y = INDUSTRY_NAME, x = GVA_aspercentofGBtotal_perFTjob),
    alpha = 0.2, size = 2, colour = 'black'
    ) +
  scale_x_continuous(limits = c(0,0.000024)) +
  ggtitle(2021)


p <- addplace(p, gvax, place = 'South Yorkshire', countcutoff = countcutoff, colourp = 'red', shapep = 16)
# p <- addplace(p, gvax, place = 'Greater Manchester', countcutoff = countcutoff, colourp = 'blue', shapep = 17)
p <- addplace(p, gvax, place = 'Inner London - East', countcutoff = countcutoff, colourp = 'blue', shapep = 17)
p



#Repeat to look at all years
for(filteryear in 2015:2021){
  
  gvaxloop <- gva_jobs5 %>% filter(DATE == filteryear) 
  
  #Ordering by the flagged place, bit awkward
  gvaxloop$INDUSTRY_NAME <- factor(gvaxloop$INDUSTRY_NAME)
  gvaxloop$INDUSTRY_NAME <- fct_relevel(
    gvaxloop$INDUSTRY_NAME, 
    ordertouse
  )
  
  #Base plot
  countcutoff = 500
  
  p <- ggplot() +
    geom_point(
      data = gvaxloop %>% filter(JOBCOUNT_FT >= countcutoff),
      aes(y = INDUSTRY_NAME, x = GVA_aspercentofGBtotal_perFTjob),
      alpha = 0.2, size = 2, colour = 'black'
    ) +
    scale_x_continuous(limits = c(0,0.000024)) +
    ggtitle(filteryear)
  
  
  p <- addplace(p, gvaxloop, place = 'South Yorkshire', countcutoff = countcutoff, colourp = 'red', shapep = 16)
  # p <- addplace(p, gvaxloop, place = 'Greater Manchester', countcutoff = countcutoff, colourp = 'blue', shapep = 17)
  # p <- addplace(p, gvaxloop, place = 'Inner London - East', countcutoff = countcutoff, colourp = 'blue', shapep = 17)
  p <- addplace(p, gvaxloop, place = 'West Yorkshire', countcutoff = countcutoff, colourp = 'blue', shapep = 17)
  
  ggsave(plot = p, filename = paste0('local/localimages/GVA_perworker_proportions/',filteryear,'.png'),width = 12, height = 12)
  
}



#Get slopes for GVA per FT job
FT_slopes <- compute_slope_or_zero(data = gva_jobs5, GEOGRAPHY_NAME, INDUSTRY_NAME, y = "GVA_aspercentofGBtotal_perFTjob", x = "DATE")

#... and EMPLOYMENT
EMP_slopes <- compute_slope_or_zero(data = gva_jobs5, GEOGRAPHY_NAME, INDUSTRY_NAME, y = "GVA_aspercentofGBtotal_perEMPLOYMENT", x = "DATE")



#Add both of those slope types to the single year data
gvax <- gvax %>% 
  left_join(
    EMP_slopes %>% rename(slope_EMP = slope),
    by = c('INDUSTRY_NAME','GEOGRAPHY_NAME')
  ) %>% 
  left_join(
    FT_slopes %>% rename(slope_FT = slope),
    by = c('INDUSTRY_NAME','GEOGRAPHY_NAME')
  )

#Out of interest... huh, high but way off perfect
cor(gvax$slope_EMP,gvax$slope_FT)

#Add groupings to that. Recall, it's not that many sectors (70) so not too many groups required
gvax <- gvax %>% 
  group_by(GEOGRAPHY_NAME) %>% 
  mutate(
    FTslope_group = as.integer(cut_number(slope_FT,5)),
    EMPslope_group = as.integer(cut_number(slope_EMP,5))
    ) %>% 
  ungroup()


#Get over time data from that...
place = 'South Yorkshire'
place = 'Greater Manchester'

industries = gvax %>% filter(GEOGRAPHY_NAME == place, FTslope_group %in% c(5)) %>% select(INDUSTRY_NAME) %>% pull()
industries = gvax %>% filter(GEOGRAPHY_NAME == place, FTslope_group %in% c(3)) %>% select(INDUSTRY_NAME) %>% pull()
industries = gvax %>% filter(GEOGRAPHY_NAME == place, FTslope_group %in% c(1)) %>% select(INDUSTRY_NAME) %>% pull()

x <- gva_jobs5 %>% filter(GEOGRAPHY_NAME == place, INDUSTRY_NAME %in% industries) %>% ungroup()

y <- x %>% 
  group_by(INDUSTRY_NAME) %>% 
  arrange(DATE) %>% 
  mutate(
    movingav = rollapply(GVA_aspercentofGBtotal_perFTjob,3,mean,align='right',fill=NA)
  )



#Get min max values over time as well, to add as bars so range of sector is easy to see
minmaxes <- gva_jobs5 %>% 
  group_by(GEOGRAPHY_NAME,INDUSTRY_NAME) %>% 
  summarise(
    minn_ft = min(GVA_aspercentofGBtotal_perFTjob),
    maxx_ft = max(GVA_aspercentofGBtotal_perFTjob)
  )

x <- x %>% 
  left_join(
    minmaxes,
    by = c("GEOGRAPHY_NAME","INDUSTRY_NAME")
  )






# plot_ly(data = y %>% filter(JOBCOUNT_FT >=500), x = ~DATE, y = ~movingav, color = ~INDUSTRY_NAME,
# plot_ly(data = y, x = ~DATE, y = ~movingav, color = ~INDUSTRY_NAME,
plot_ly(data = y %>% filter(JOBCOUNT_FT >=500,INDUSTRY_NAME!='Insurance and pension funding'), x = ~DATE, y = ~GVA_aspercentofGBtotal_perFTjob, color = ~INDUSTRY_NAME,
# plot_ly(data = y %>% filter(JOBCOUNT_FT >=500,INDUSTRY_NAME!='Insurance and pension funding'), x = ~DATE, y = ~movingav, color = ~INDUSTRY_NAME,
# plot_ly(data = x, x = ~DATE, y = ~GVA_aspercentofGBtotal_perFTjob, color = ~INDUSTRY_NAME, 
        text = ~paste("Sector:", INDUSTRY_NAME, "\nGVA % per job: ",GVA_aspercentofGBtotal_perFTjob,'\nFT JOBs: ',JOBCOUNT_FT),  # Add this line for hover text
        hoverinfo = 'text',
        type = 'scatter', mode = 'lines+markers', line = list(shape = 'linear')) %>%
  layout(title = "Yearly percents by SIC", 
         xaxis = list(title = "Year"), 
         yaxis = list(title = "Value", type='log'),
         # yaxis = list(title = "Value"),
         showlegend = F)



#Hmm, think there might be an issue with the volality of the job numbers...
plot_ly(data = x, x = ~DATE, y = ~JOBCOUNT_FT, color = ~INDUSTRY_NAME,
        # plot_ly(data = x, x = ~DATE, y = ~GVA_aspercentofGBtotal_perFTjob, color = ~INDUSTRY_NAME, 
        text = ~paste("Sector:", INDUSTRY_NAME, "\nGVA % per job: ",GVA_aspercentofGBtotal_perFTjob,'\nFT JOBs: ',JOBCOUNT_FT),  # Add this line for hover text
        hoverinfo = 'text',
        type = 'scatter', mode = 'lines+markers', line = list(shape = 'linear')) %>%
  layout(title = "Yearly percents by SIC", 
         xaxis = list(title = "Year"), 
         yaxis = list(title = "Value", type='log'),
         # yaxis = list(title = "Value"),
         showlegend = F)





#Look at all
x <- gva_jobs5 %>% filter(DATE==2021)

# y <- x %>% 
#   group_by(INDUSTRY_NAME) %>% 
#   arrange(DATE) %>% 
#   mutate(
#     movingav = rollapply(GVA_aspercentofGBtotal_perFTjob,3,mean,align='right',fill=NA)
#   )



#Get min max values over time as well, to add as bars so range of sector is easy to see
minmaxes <- gva_jobs5 %>% 
  group_by(GEOGRAPHY_NAME,INDUSTRY_NAME) %>% 
  summarise(
    minn_ft = min(GVA_aspercentofGBtotal_perFTjob),
    maxx_ft = max(GVA_aspercentofGBtotal_perFTjob)
  )

x <- x %>% 
  left_join(
    minmaxes,
    by = c("GEOGRAPHY_NAME","INDUSTRY_NAME")
  )


#Look at one sector, all places, compare full range
ggplot(x,
       aes(y = GEOGRAPHY_NAME)) +
  geom_errorbar(aes(y = GEOGRAPHY_NAME, xmin = minn_ft, xmax = maxx_ft)) +
  facet_wrap(~INDUSTRY_NAME)

place='South Yorkshire'

#Ooo, think we need to order those and look at all
for(i in unique(gva_jobs5$INDUSTRY_NAME)){
  
  y <- x %>% filter(INDUSTRY_NAME == i, JOBCOUNT_FT >= 500) %>% 
    rowwise() %>% 
    mutate(meanminmax = mean(c(minn_ft,maxx_ft))) %>% 
    ungroup() %>% 
    mutate(
      flaggedplace = ifelse(GEOGRAPHY_NAME==place, 'A', 'B'),
      GEOGRAPHY_NAME = factor(GEOGRAPHY_NAME),
      GEOGRAPHY_NAME = fct_reorder(GEOGRAPHY_NAME, meanminmax)
      ) 
    
  
  
  p <- ggplot(y,
         aes(y = GEOGRAPHY_NAME, colour = flaggedplace)) +
    geom_errorbar(aes(y = GEOGRAPHY_NAME, xmin = minn_ft, xmax = maxx_ft)) +
    facet_wrap(~INDUSTRY_NAME) +
    guides(colour=F) 
  
  if(length(y %>% filter(GEOGRAPHY_NAME == place) %>% select(minn_ft) %>% pull) !=0)
  {
    p <- p +
      geom_vline(xintercept = y %>% filter(GEOGRAPHY_NAME == place) %>% select(minn_ft) %>% pull, alpha = 0.3)
  }
  
  if(length(y %>% filter(GEOGRAPHY_NAME == place) %>% select(maxx_ft) %>% pull) !=0)
  {
    p <- p +
      geom_vline(xintercept = y %>% filter(GEOGRAPHY_NAME == place) %>% select(maxx_ft) %>% pull, alpha = 0.3)
  }
  
  ggsave(plot = p, filename = paste0('local/localimages/GVA_perc_minmax/',i,'.png'), width = 8, height = 8)
  
}




#Want to pick a few of those sectors out for direct side by side comparison.
#So, turn round / facet.
#Changing order of places will still happen but actual job GVA range will be visible
#Which sectors to pick? Stick to some of the key manuf ones first, let's see about those
#Which are?
sectors <- unique(x$INDUSTRY_NAME)[grepl('basic metals|fabricated metal|machinery and equipment', unique(x$INDUSTRY_NAME))]


y <- x %>% filter(INDUSTRY_NAME %in% sectors, JOBCOUNT_FT >= 500) %>% 
  # y <- x %>% filter(INDUSTRY_NAME %in% sectors, JOBCOUNT_FT >= 500) %>% 
  rowwise() %>% 
  mutate(meanminmax = mean(c(minn_ft,maxx_ft))) %>% 
  ungroup() %>% 
  # group_by(INDUSTRY_NAME) %>% 
  mutate(
    flaggedplace = ifelse(GEOGRAPHY_NAME==place, 'A', 'B'),
    GEOG_NAME_SHORTENED = substr(GEOGRAPHY_NAME, start = 1, stop = 10),
    GEOG_NAME_SHORTENED = factor(GEOG_NAME_SHORTENED),
    GEOG_NAME_SHORTENED = fct_reorder(GEOG_NAME_SHORTENED, meanminmax)
  ) 

p <- ggplot(y,
            aes(y = GEOG_NAME_SHORTENED, colour = flaggedplace)) +
  geom_errorbar(aes(y = GEOG_NAME_SHORTENED, xmin = minn_ft, xmax = maxx_ft)) +
  facet_wrap(~INDUSTRY_NAME, nrow = 1, scales = 'free_x') +
  guides(colour='none') +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


if(length(y %>% filter(GEOG_NAME_SHORTENED == place) %>% select(minn_ft) %>% pull) !=0){
  p <- p +
    geom_vline(xintercept = y %>% filter(GEOG_NAME_SHORTENED == place) %>% select(minn_ft) %>% pull, alpha = 0.3)
}

if(length(y %>% filter(GEOG_NAME_SHORTENED == place) %>% select(maxx_ft) %>% pull) !=0){
  p <- p +
    geom_vline(xintercept = y %>% filter(GEOG_NAME_SHORTENED == place) %>% select(maxx_ft) %>% pull, alpha = 0.3)
}

p

# ggsave(plot = p, filename = paste0('local/localimages/GVA_perc_minmax/',sectors,'.png'), width = 15, height = 8)

#That's being horrible. Let's cowplot that instead
#Get three separate ones
sectors <- unique(x$INDUSTRY_NAME)[grepl('basic metals|fabricated metal|machinery and equipment', unique(x$INDUSTRY_NAME))]
sectors <- unique(x$INDUSTRY_NAME)[grepl('research and development|food products|non-metallic', unique(x$INDUSTRY_NAME))]


sectors <- unique(x$INDUSTRY_NAME)


#Get list of plots
spanplot <- function(sector){
  
  y <- x %>% filter(INDUSTRY_NAME %in% sector, JOBCOUNT_FT >= 250) %>% 
    # y <- x %>% filter(INDUSTRY_NAME %in% sectors, JOBCOUNT_FT >= 500) %>% 
    rowwise() %>% 
    mutate(meanminmax = mean(c(minn_ft,maxx_ft))) %>% 
    ungroup() %>% 
    # group_by(INDUSTRY_NAME) %>% 
    mutate(
      flaggedplace = ifelse(GEOGRAPHY_NAME==place, 'A', 'B'),
      GEOG_NAME_SHORTENED = substr(GEOGRAPHY_NAME, start = 1, stop = 10),
      GEOG_NAME_SHORTENED = factor(GEOG_NAME_SHORTENED),
      GEOG_NAME_SHORTENED = fct_reorder(GEOG_NAME_SHORTENED, meanminmax)
    ) 
  
  p <- ggplot(y,
              aes(y = GEOG_NAME_SHORTENED, colour = flaggedplace)) +
    geom_errorbar(aes(y = GEOG_NAME_SHORTENED, xmin = minn_ft, xmax = maxx_ft)) +
    facet_wrap(~INDUSTRY_NAME, nrow = 1, scales = 'free_x') +
    guides(colour='none') +
    scale_x_continuous(limits = c(0,0.0000125)) +
    coord_flip() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    ylab('')
  
  
  if(length(y %>% filter(GEOGRAPHY_NAME == place) %>% select(minn_ft) %>% pull) !=0){
    p <- p +
      geom_vline(xintercept = y %>% filter(GEOGRAPHY_NAME == place) %>% select(minn_ft) %>% pull, alpha = 0.3)
  }
  
  if(length(y %>% filter(GEOGRAPHY_NAME == place) %>% select(maxx_ft) %>% pull) !=0){
    p <- p +
      geom_vline(xintercept = y %>% filter(GEOGRAPHY_NAME == place) %>% select(maxx_ft) %>% pull, alpha = 0.3)
  }
  
  p
  
}

plotz <- lapply(sectors, function(x) spanplot(x))

cp <- plot_grid(plotlist = plotz, nrow = 5)

save_plot(plot = cp, filename = paste0('local/localimages/GVA_perc_minmax/',paste(sectors, collapse = '_'),'.png'), base_height = 6, base_width = 18)

#For large plot
save_plot(plot = cp, filename = paste0('local/localimages/GVA_perc_minmax/',paste(sectors, collapse = '_'),'.png'), base_height = 25, base_width = 50, limitsize=F)
















