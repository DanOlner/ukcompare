#Regional GVA per sector explore
library(tidyverse)
library(zoo)
library(sf)
library(tmap)
library(plotly)
library(magick)
library(cowplot)
library(ggrepel)
source('functions/misc_functions.R')
options(scipen = 99)

#for work laptop library path setting
# .libPaths('C:/Users/mg1dol/Documents/templibpaths')

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
# sectorsizes <- itl1.hluk %>% 
#   group_by(`SIC07 code`) %>% 
#   summarise(av = mean(value)) %>% 
#   mutate(group = as.integer(cut_number(av, n = 5)))

#Or size of sector for most recent value, which might make more sense than average for viewing
sectorsizes <- itl1.hluk %>% 
  filter(year == 2019) 
# %>% mutate(group = as.integer(cut_number(value, n = 5)))

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



#SIDEQUEST, MAINLY CONTINUED BELOW IN ITS OWN SECTION
#REPEAT GETTING THE ABOVE PROPORTIONS BUT WITH IMPUTED RENT REMOVED
itl2.cp.no.ir <- itl2.cp %>%
  filter(`SIC07 description`!="Owner-occupiers' imputed rental") %>% 
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

xmm <- itl2.cp %>% filter(year == 2021, `SIC07 description` != 'Water and air transport') %>% mutate(flaggedplace = `ITL region name`==place)

#Ordering by the flagged place, bit awkward
xmm$`SIC07 description` <- factor(xmm$`SIC07 description`)
xmm$`SIC07 description` <- fct_relevel(
  xmm$`SIC07 description`, 
  unique(as.character(xmm$`SIC07 description`))[order(xmm %>% filter(`ITL region name`==place) %>%ungroup() %>% select(LQ_log) %>% pull(),decreasing = T)]
  )

#Actually, keep that order and use for animation below
ordertouse <- unique(as.character(xmm$`SIC07 description`))[order(xmm %>% filter(`ITL region name`==place) %>%ungroup() %>% select(LQ_log) %>% pull(),decreasing = T)]


ggplot(
  xmm, 
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
  
  xmm <- itl2.cp %>% filter(year == filteryear, `SIC07 description` != 'Water and air transport') %>% mutate(flaggedplace = `ITL region name`==placename)
  
  #Ordering by the flagged place, bit awkward
  xmm$`SIC07 description` <- factor(xmm$`SIC07 description`)
  xmm$`SIC07 description` <- fct_relevel(
    xmm$`SIC07 description`, 
    ordertouse
  )

  ggplot(
    xmm, 
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
xmm <- itl2.cp %>% filter(year == 2009, `SIC07 description` != 'Water and air transport') %>% mutate(flaggedplace = `ITL region name`==place)
ordertouse <- unique(as.character(xmm$`SIC07 description`))[order(xmm %>% filter(`ITL region name`==place) %>%ungroup() %>% select(LQplusone_log) %>% pull(),decreasing = T)]

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



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#SIDEQUEST: GVA WITHOUT IMPUTED RENT----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#What I'd like: GVA proportions with and without imputed rent
#Both need calculating separately, probably 

#Here's one I made earlier - this has imputed rent removed
glimpse(itl2.cp.no.ir)


#Can we see the two of them side by side? I.e. just for one year
#what is each ITL2 proportion of UK total GVA?
#Also just need a single unique region total size and UK total size, to get proportion
no.ir2021 <- itl2.cp.no.ir %>% filter(year==2021) %>% 
  distinct(`ITL region name`,region_totalsize,uk_totalsize) %>% 
  mutate(percent_of_uk_gva = (region_totalsize/uk_totalsize)*100)

#Repeat for with imputed rent
with.ir2021 <- itl2.cp %>% filter(year==2021) %>% 
  distinct(`ITL region name`,region_totalsize,uk_totalsize) %>% 
  mutate(percent_of_uk_gva = (region_totalsize/uk_totalsize)*100)

#Check there weren't any duplicate totalsize values... tick
length(unique(ir2021$`ITL region name`)) == length(unique(itl2.cp$`ITL region name`))

#Check orig total size uk was correct... tick
no.ir2021 %>% 
# with.ir2021 %>% 
  group_by(year) %>% 
  summarise(sum(percent_of_uk_gva))



#What's the proportion diff with / without IR?
#12.66% of the economy. Uh huh.
(no.ir2021$uk_totalsize[1]/with.ir2021$uk_totalsize[1])*100
100-((no.ir2021$uk_totalsize[1]/with.ir2021$uk_totalsize[1])*100)



#Direct comparison of sizes will be tricky. What will be easier: 
#For each region, what proportion of its GVA is imputed rent?
#Hmm, we already had that figure in the original calcs didn't we, actually?

#So we can just look at the proportion that imputed rent is in different ITL2s?
ggplot(
  itl2.cp %>% filter(`SIC07 description` == "Owner-occupiers' imputed rental"),
  aes(x = year, y = sector_regional_proportion*100, colour = `ITL region name`)
  ) +
  geom_line() +
  geom_point()



plot_ly(
  data = itl2.cp %>% ungroup() %>% filter(`SIC07 description` == "Owner-occupiers' imputed rental"), 
        x = ~year, y = ~sector_regional_proportion*100, color = ~`ITL region name`,
        text = ~paste("ITL:", `ITL region name`),  # Add this line for hover text
        hoverinfo = 'text+y+x',
        type = 'scatter', mode = 'lines+markers', line = list(shape = 'linear')) %>%
  layout(title = "Yearly values by SIC", 
         xaxis = list(title = "Year"), 
         # yaxis = list(title = "Value", type='log'),
         yaxis = list(title = "Value"),
         showlegend = TRUE)

#That's fascinating plot for several reasons, but...


#Better question:
#What proportion of the whole UK economy does each ITL make up
#With and without imputed rent?
#That can give us a sense of the difference without getting on to denominators like FT jobs or per capita etc
#Just raw difference

#Calculated above. Put next to each other to see diff
both <- no.ir2021 %>% 
  select(-region_totalsize,-uk_totalsize) %>% 
  rename(percent_of_uk_gva_NO_IR = percent_of_uk_gva) %>% 
  left_join(
    with.ir2021 %>% rename(percent_of_uk_gva_WITH_IR = percent_of_uk_gva) %>% select(-region_totalsize,-uk_totalsize),
    by = c('year','ITL region name')
  ) %>% 
  relocate(percent_of_uk_gva_WITH_IR, .before = percent_of_uk_gva_NO_IR) %>% 
  mutate(percentdiff = (percent_of_uk_gva_NO_IR/percent_of_uk_gva_WITH_IR)*100)


ggplot(
  both %>% mutate(`ITL region name` = factor(`ITL region name`)), 
  aes(x = fct_reorder(`ITL region name`,percentdiff), y = percentdiff)
  ) +
  geom_bar(stat='identity') +
  coord_flip(ylim = c(90,107)) +
  # coord_cartesian(xlim = c(80,110)) +
  geom_hline(yintercept = 100)


#Let's just stare at Inner London West.
#I know underlying reason: finance there massive, so imputed rent relatively less strong.
#But...



#That totally needs a map making!
itl2.geo <- st_read('data/geographies/International_Territorial_Level_2_January_2021_UK_BFE_V2_2022_-4735199360818908762/ITL2_JAN_2021_UK_BFE_V2.shp') %>% 
  st_simplify(preserveTopology = T, dTolerance = 100)

chk <- itl2.geo %>% 
  right_join(
    both,
    by = c('ITL221NM'='ITL region name')
  )


#Just interpreted that
#If a place is 90%, its GVA is a 10% smaller proportion of the whole UK economy (for that year) than if imputed rent included.
tm_shape(chk) +
  tm_polygons('percentdiff', n = 11)
  # tm_polygons('percentdiff', n = 11, palette="-RdYlGn")



#Next Q. How have those percentages changed over time?


#~~~~~~~~~~~~~~~~~~~~~~~~~
#LQ/ITL2 CHANGE CHARTS----
#~~~~~~~~~~~~~~~~~~~~~~~~~

#Originally made for BRES data, repeat code that adds change over time (via OLS) as colour

#Create a function that fits lm and returns slope
get_slope2 <- function(data) {
  model <- lm(LQ_log ~ year, data = data, na.action = na.omit)
  coef(model)[2]
}

#Make it a safe function using purrr::possibly
#(Issue with some data being all NAs)
safe_get_slope2 <- purrr::possibly(get_slope2, otherwise = 0)

ll <- itl2.cp %>%
  group_by(`ITL region name`, `SIC07 description`) %>%
  nest() %>%
  mutate(slope = map_dbl(data, safe_get_slope2)) %>%
  select(-data) %>% 
  mutate(slope = ifelse(is.na(slope),0,slope)) %>% 
  arrange(slope)


#Break into groups of differing slope
diffchange2 <- ll %>% 
  filter(slope!=0) %>% 
  group_by(`ITL region name`) %>% 
  mutate(group = as.integer(cut_number(slope,7))) %>% 
  ungroup() %>% 
  rename(difftotal = slope)


#Look at some for sanity check
place = 'South Yorkshire'
# place = 'Greater Manchester'

#climbers
industries = diffchange2 %>% filter(`ITL region name` == place, group %in% c(7)) %>% select(`SIC07 description`) %>% pull()
#droppers
industries = diffchange2 %>% filter(`ITL region name` == place, group %in% c(4)) %>% select(`SIC07 description`) %>% pull()
#middle
industries = diffchange2 %>% filter(`ITL region name` == place, group %in% c(1)) %>% select(`SIC07 description`) %>% pull()

#Select those industries / place
xmn <- itl2.cp %>% filter(`ITL region name` == place, `SIC07 description` %in% industries) 


#Check one set of slopes
# sy <- itl2.cp %>% filter(`ITL region name` == place) %>% 
#   inner_join(l, by = c("ITL region name","SIC07 description"))
# 
# #unique slope values in order
# slopeorder <- unique(sy$slope)[order(unique(sy$slope))]
# 
# x <- sy %>% filter(slope %in% slopeorder[1:10])

y <- xmn %>% 
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
xmm <- itl2.cp %>% filter(year == 2021)

#Add slopes into data to get LQ plots
xmm <- xmm %>% 
  left_join(
    diffchange2 %>% select(-group),
    by = c("ITL region name","SIC07 description")
  )

#Get min max values over time as well, to add as bars so range of sector is easy to see
minmaxes2 <- itl2.cp %>% 
  group_by(`SIC07 description`,`ITL region name`) %>% 
  summarise(
    minn = min(LQ),
    maxx = max(LQ)
  )

xmm <- xmm %>% 
  left_join(
    minmaxes2,
    by = c("ITL region name","SIC07 description")
  )


#Factor
xmm$`SIC07 description` <- factor(xmm$`SIC07 description`)
xmm$`SIC07 description` <- fct_relevel(
  xmm$`SIC07 description`, 
  unique(as.character(xmm$`SIC07 description`))[order(xmm %>% filter(`ITL region name`==place) %>%ungroup() %>% select(LQ) %>% pull(),decreasing = T)]
)








#LQ PLOT
#Function for overlaying specific places / ITL2 zones
addplace_to_LQplot2 <- function(df, plot_to_addto, place, shapenumber=16,backgroundcolour='black', add_gva = F, setalpha = 1, addminmax = F){
  
  plot_to_addto <- plot_to_addto +
    geom_point(
      data = df %>% filter(`ITL region name` == place, difftotal > 0), 
      # aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace, size = COUNT)) +
      aes(y = `SIC07 description`, x = LQ, size = difftotal *1.75),
      shape = shapenumber,
      colour = backgroundcolour,
      alpha = setalpha
    ) +
    geom_point(
      data = df %>% filter(`ITL region name` == place, difftotal < 0), 
      # aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace, size = COUNT)) +
      aes(y = `SIC07 description`, x = LQ, size = difftotal *-1.75),
      shape = shapenumber,
      colour = backgroundcolour,
      alpha = setalpha
    ) +
    geom_point(
      data = df %>% filter(`ITL region name` == place, difftotal > 0), 
      # aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace, size = COUNT)) +
      aes(y = `SIC07 description`, x = LQ, size = difftotal),
      shape = shapenumber,
      colour = 'green',
      alpha = setalpha
    ) +
    geom_point(
      data = df %>% filter(`ITL region name` == place, difftotal < 0), 
      # aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace, size = COUNT)) +
      aes(y = `SIC07 description`, x = LQ, size = difftotal * -1),
      shape = shapenumber,
      colour = 'red',
      alpha = setalpha
    ) 
  
  
  if(add_gva){
    
    plot_to_addto <- plot_to_addto +  
      geom_text(
        data = df %>% filter(`ITL region name` == place), 
        aes(y = `SIC07 description`, x = 20, label = paste0('£',value,'M, ',round(sector_regional_proportion * 100, 2),'%')),
        # aes(y = INDUSTRY_NAME, x = max(LQ) + 2, label = COUNT),
        nudge_x = 0.3, hjust = 1, alpha = 0.7, size = 3
      )
    
    if(addminmax){
      
      plot_to_addto <- plot_to_addto +
        geom_errorbar(
          data = df %>% filter(`ITL region name` == place),
          aes(y = `SIC07 description`, xmin = minn, xmax = maxx),
          width = 0.05
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
xmm$`SIC07 description` <- factor(xmm$`SIC07 description`)
xmm$`SIC07 description` <- fct_relevel(
  xmm$`SIC07 description`, 
  unique(as.character(xmm$`SIC07 description`))[order(xmm %>% filter(`ITL region name`==place1) %>%ungroup() %>% select(LQ) %>% pull(),decreasing = T)]
)


#Base plot
p <- ggplot() +
  geom_point(
    data = xmm %>% filter(difftotal > 0), 
    # aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace, size = COUNT)) +
    aes(y = `SIC07 description`, x = LQ, size = difftotal),
    alpha = 0.1,
    shape = 16,
    colour = 'green'
  ) +
  geom_point(
    data = xmm %>% filter(difftotal < 0), 
    # aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace, size = COUNT)) +
    aes(y = `SIC07 description`, x = LQ, size = difftotal * -1),
    alpha = 0.1,
    shape = 16,
    colour = 'red'
  )  +
  scale_size_continuous(range = c(1,17)) +
  scale_x_continuous(trans = "log10", limits = c(0.01, 51)) +
  geom_vline(xintercept = 1, colour = 'blue') +
  guides(size = F) 



#Add a place
p <- addplace_to_LQplot2(df = xmm, plot_to_addto = p, place = place2, shapenumber = 18,backgroundcolour = '#9ac0db', setalpha = 0.7)
p <- addplace_to_LQplot2(df = xmm, plot_to_addto = p, place = place1, shapenumber = 16, add_gva = T, addminmax = T)
p


ggsave(plot = p, filename = paste0('local/localimages/gva_',place1,'_v_',place2,'_plot.png'), width = 12, height = 12)





#Reduced version 1: growth sectors, always LQ>1
sic5filter <- xmm %>% filter(
  `ITL region name`=="South Yorkshire",LQ > 1, difftotal > 0,
  # `ITL region name`=="South Yorkshire",LQ > 1, minn > 1,#no slope, can be going up or down in that time
  # `ITL region name`=="South Yorkshire",LQ > 1, minn > 1, difftotal > 0,#never LQ < 1
  !grepl('Water and air', `SIC07 description`)
) %>% 
  ungroup() %>% 
  select(`SIC07 description`) %>% 
  pull


reduced <- xmm %>% filter(
  `SIC07 description` %in% sic5filter
)

#total GVA and percent, for the above
value_n_perc <- reduced %>% 
  filter(`ITL region name` == 'South Yorkshire') %>% 
  summarise(
    value = sum(value),
    totperc = sum(sector_regional_proportion) * 100
  )



#Base plot
p <- ggplot() +
  geom_point(
    data = reduced %>% filter(difftotal > 0), 
    # aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace, size = COUNT)) +
    aes(y = `SIC07 description`, x = LQ, size = difftotal),
    alpha = 0.1,
    shape = 16,
    colour = 'green'
  ) +
  geom_point(
    data = reduced %>% filter(difftotal < 0), 
    # aes(y = INDUSTRY_NAME, x = LQ, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace, size = COUNT)) +
    aes(y = `SIC07 description`, x = LQ, size = difftotal * -1),
    alpha = 0.1,
    shape = 16,
    colour = 'red'
  )  +
  scale_size_continuous(range = c(1,17)) +
  scale_x_continuous(trans = "log10", limits = c(0.001, 51)) +
  geom_vline(xintercept = 1, colour = 'blue') +
  guides(size = F) +
  ggtitle(
    paste0(
      'SY GVA LQs > 1. ', round(value_n_perc$totperc,2),'% of SY economy.'
    )
  ) +
  theme(plot.title = element_text(face = 'bold')) +
  ylab("")



#Add a place
p <- addplace_to_LQplot2(df = reduced, plot_to_addto = p, place = place2, shapenumber = 18,backgroundcolour = '#9ac0db', setalpha = 0.7)
p <- addplace_to_LQplot2(df = reduced, plot_to_addto = p, place = place1, shapenumber = 16, add_gva = T, addminmax = T)
p

ggsave(plot = p, filename = 'local/localimages/SY_LQ_morethan1.png', height = 8, width = 8)



#Lemme see the full path of those selections plz thank you
y <- itl2.cp %>% filter(`ITL region name` == place1, `SIC07 description` %in% unique(reduced$`SIC07 description`)) %>% 
  group_by(`SIC07 description`) %>% 
  arrange(year) %>% 
  mutate(
    LQ = ifelse(LQ == 0, NA, LQ),#Set to NA so plotly won't show
    movingav = rollapply(LQ,3,mean,align='right',fill=NA)
    # LQ_movingav = rollapply(LQ,3,mean,align='right',fill=NA),0)#Have a count moving average too, so it matches the percent (so count orders are correct vertically)
  )

y <- y %>% 
  mutate(`SIC07 description` = factor(`SIC07 description`),
         `SIC07 description` = fct_reorder(`SIC07 description`, LQ)
         # `SIC07 description` = fct_reorder(`SIC07 description`, movingav, .na_rm = T)
         )

#Break into av LQ groups for facetting
cutz <- y %>% 
  group_by(`SIC07 description`) %>% 
  summarise(avLQ = mean(LQ, na.rm = T)) %>% 
  mutate(LQ_group = as.numeric(cut_number(avLQ, 4)))


y <- y %>% 
  left_join(cutz, by = 'SIC07 description')


# plot_ly(data = y, x = ~year, y = ~movingav, color = ~`SIC07 description`,
#         # plot_ly(data = y, x = ~year, y = ~LQ, color = ~`SIC07 description`,
#         # text = ~paste("Sector:", `SIC07 description`, "\nGVA: ",value, "\nslope: ",slope),  # Add this line for hover text
#         text = ~paste("Sector:", `SIC07 description`, "\nGVA: ",value),  # Add this line for hover text
#         hoverinfo = 'text',
#         type = 'scatter', mode = 'lines+markers', line = list(shape = 'linear')) %>%
#   layout(title = "Yearly percents by SIC", 
#          xaxis = list(title = "Year"), 
#          yaxis = list(title = "Value", type='log'),
#          # yaxis = list(title = "Value"),
#          showlegend = F)

#ggplot version
ggplot(y, aes(x = year, y = movingav, colour = fct_reorder(`SIC07 description`, -movingav, na.rm=T))) +
  geom_point() +
  geom_line() +
  # scale_color_distiller(type = 'qual', direction = -1) +
  # scale_color_brewer(palette = 'Dark2', direction = -1) +
  # scale_color_brewer(palette = 'Paired', direction = -1) +
  scale_y_log10() +
  theme(legend.title=element_blank()) +
  ylab('LQ, 3 year moving average, log') +
  geom_hline(yintercept = 1)

#Faceted by groups (based on av LQ over time) version
ggplot(y, aes(x = year, y = movingav, colour = fct_reorder(`SIC07 description`, -movingav, na.rm=T))) +
  geom_point() +
  geom_line() +
  # scale_color_distiller(type = 'qual', direction = -1) +
  # scale_color_brewer(palette = 'Dark2', direction = -1) +
  # scale_color_brewer(palette = 'Paired', direction = -1) +
  scale_y_log10() +
  theme(legend.title=element_blank()) +
  ylab('LQ, 3 year moving average, log') +
  facet_wrap(~LQ_group, scales = 'free_y') +
  geom_hline(yintercept = 1)
  


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#XY PLOT OF SY LQS VS REGIONAL PROPORTION----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#To check on actual GVA sizes vs relative position in UK
#For a single year (might be interesting to animate this, but can look at pair of years for now)

#Hmm, arguably could just put UK regional proportion rather than LQ. 2 dimensional version gives clear picture
# ggplot(y, aes(x = sector_regional_proportion, y = sector_uk_proportion, label = `SIC07 description`)) +
ggplot(
  # itl2.cp %>% filter(`ITL region name` == place1, year == 2021),
  itl2.cp %>% filter(`ITL region name` == place1, year == 2021, `SIC07 description` %in% unique(reduced$`SIC07 description`)),
       aes(x = sector_regional_proportion * 100, y = sector_uk_proportion * 100, label = `SIC07 description`)) +
  geom_point(size = 4, colour = 'red') +
  # geom_vline(xintercept = 0) +
  # geom_hline(yintercept = 1) +
  geom_text_repel(
    nudge_x = .05,
    box.padding = 0.2,
    nudge_y = 0.05,
    segment.curvature = -0.1,
    segment.ncp = 0.3,
    segment.angle = 20
  ) +
  geom_abline(slope = 1, size = 1, colour='blue', alpha = 0.5) +
  coord_cartesian(xlim = c(0.1,11), ylim = c(0.1,11)) + # good for log scale
  scale_y_log10() +
  scale_x_log10()
  



#Making a version that has "UK regional proportion minus the target region"
#Otherwise they're autocorrelating
#Make that and then just merge back in?
uk_regional_props_minus_targetregion <- itl2.cp %>%
  filter(`ITL region name` != place1) %>% 
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
    sector_uk_proportion_minustargetITL = uk_sectorsize / uk_totalsize#e. UK-level sector proportion
  ) 


#Add back in to a copy of the main itl df, so it can be smoothed
itl2.cp.w.sector_uk_proportion_minustargetITL <- itl2.cp %>% 
  left_join(
    uk_regional_props_minus_targetregion %>% 
      select(`SIC07 description`, year,sector_uk_proportion_minustargetITL) %>% 
      distinct(`SIC07 description`,year,sector_uk_proportion_minustargetITL),
    by = c('SIC07 description', 'year')
  )

#Correlation between these should be close but not one... Yup.
cor(itl2.cp.w.sector_uk_proportion_minustargetITL$sector_uk_proportion,itl2.cp.w.sector_uk_proportion_minustargetITL$sector_uk_proportion_minustargetITL, use = 'complete.obs')
plot(itl2.cp.w.sector_uk_proportion_minustargetITL$sector_uk_proportion,itl2.cp.w.sector_uk_proportion_minustargetITL$sector_uk_proportion_minustargetITL)


#Let's try a smoothed / animated version of that.
#Add smoothing first
smoothband = 1

anim_lq <- itl2.cp.w.sector_uk_proportion_minustargetITL %>% filter(`ITL region name` == place1, `SIC07 description` %in% unique(reduced$`SIC07 description`)) %>% 
  group_by(`SIC07 description`) %>% 
  arrange(year) %>% 
  mutate(
    regionalprop_movingav = rollapply(sector_regional_proportion,smoothband,mean,align='right',fill=NA),
    ukprop_movingav = rollapply(sector_uk_proportion,smoothband,mean,align='right',fill=NA),
    ukprop_minustargetITL_movingav = rollapply(sector_uk_proportion_minustargetITL,smoothband,mean,align='right',fill=NA)
  ) %>% #remove NA years smoothing created
  filter(!is.na(regionalprop_movingav))
  # filter(!is.na(regionalprop_movingav), !is.na(ukprop_minustargetITL_movingav))



for(i in min(anim_lq$year):2021){
  
  p <- ggplot(
    # itl2.cp %>% filter(`ITL region name` == place1, year == 2021),
    anim_lq %>% filter(year == i),
    aes(x = regionalprop_movingav * 100, y = ukprop_minustargetITL_movingav * 100, label = `SIC07 description`)) +
    geom_point(size = 4, colour = 'red') +
    xlab('South Yorkshire GVA proportion') +
    ylab('UK GVA proportion') +
    # geom_vline(xintercept = 0) +
    # geom_hline(yintercept = 1) +
    geom_text_repel(
      nudge_x = .05,
      box.padding = 0.2,
      nudge_y = 0.05,
      segment.curvature = -0.1,
      segment.ncp = 0.3,
      segment.angle = 20
    ) +
    geom_abline(slope = 1, size = 1, colour='blue', alpha = 0.5) +
    coord_cartesian(xlim = c(0.1,11), ylim = c(0.1,11)) + # good for log scale
    scale_y_log10() +
    scale_x_log10() +
    annotate("text", x = 0.2, y = 9, label = i, size = 25) 
    # annotate("text", x = 7, y = 0.15, label = i, size = 25)
  
  ggsave(plot = p, filename = paste0('local/localimages/animations/GVA_LQ_2D/',i,'.png'))
  
}

list.files(path = "local/localimages/animations/GVA_LQ_2D/", pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=2) %>% # animates, can opt for number of loops
  image_write("local/localimages/animations/GVA_LQ_2D/SYregional_v_UKproportions_GVAcurrentprices.gif")




#Plot comparing two years, linking years with lines
twoy <- anim_lq %>% filter(year %in% c(min(year),max(year)))

twoy$INDUSTRY_NAME_REDUCED <- gsub(x = twoy$`SIC07 description`, pattern = 'of |and |acture|acturing|activities|equipment|products', replacement = '')


#Mark four corners of the compass!
# Lines going ‘South West’ = GVA proportion shrinking in both SY and other UK.
# ‘SE’: growing in SY, shrinking elsewhere
# ‘NE’: growing everywhere (slope = relative rate)
# ‘NW’: growing in both SY and other UK

#Lag first, then polarity
#Make separately to get polarities then merge back in for labelling
twoy_lags <- twoy %>% 
  arrange(`SIC07 description`,year) %>% 
  mutate(
    lag_sector_regional_proportion = sector_regional_proportion - lag(sector_regional_proportion),
    lag_sector_uk_proportion_minustargetITL = sector_uk_proportion_minustargetITL - lag(sector_uk_proportion_minustargetITL)
  ) %>% 
  filter(year == max(year)) %>% 
  mutate(
    compass = case_when(
      lag_sector_regional_proportion < 0 & lag_sector_uk_proportion_minustargetITL < 0 ~ 'SW',
      lag_sector_regional_proportion < 0 & lag_sector_uk_proportion_minustargetITL > 0 ~ 'NW',
      lag_sector_regional_proportion > 0 & lag_sector_uk_proportion_minustargetITL > 0 ~ 'NE',
      lag_sector_regional_proportion > 0 & lag_sector_uk_proportion_minustargetITL < 0 ~ 'SE'
    )
  )

#No NW, curious
#Joining in the labels below

p <- ggplot(
  twoy,
  aes(x = sector_regional_proportion * 100, y = sector_uk_proportion_minustargetITL * 100)) +
  geom_point(size = 5, alpha = 0.75, aes(colour = factor(year), group = INDUSTRY_NAME_REDUCED)) +
  geom_line(size = 1, aes(colour = factor(year), group = INDUSTRY_NAME_REDUCED)) +
  xlab('South Yorkshire GVA proportion') +
  ylab('UK GVA proportion (MINUS South Yorkshire)')  +
  geom_abline(slope = 1, size = 1, colour='blue', alpha = 0.5) +
  coord_cartesian(xlim = c(0.1,11), ylim = c(0.1,11)) + # good for log scale
  # scale_y_log10() +
  # scale_x_log10() +
  guides(colour=guide_legend(title=" "))

p <- p + geom_text_repel(
    data = twoy %>% filter(year==max(twoy$year)) %>% 
      left_join(
        twoy_lags %>% 
          select(`SIC07 description`,compass),
        by = 'SIC07 description'
        ),
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
  scale_color_manual(values = c('red','black','#7fc97f','#beaed4','#fdc086','#ffff99'))
  

p

ggsave(plot = p, filename = 'local/localimages/SY_LQ2D_nonlog.png', height = 9, width = 9)

 


#REPEAT COMPASS FOR ALL SY SECTORS
#Have a look at which grown, just by 98-21 change
#how compares to UK growth
smoothband = 1

anim_lq <- itl2.cp.w.sector_uk_proportion_minustargetITL %>% filter(`ITL region name` == place1) %>% 
  group_by(`SIC07 description`) %>% 
  arrange(year) %>% 
  mutate(
    regionalprop_movingav = rollapply(sector_regional_proportion,smoothband,mean,align='right',fill=NA),
    ukprop_movingav = rollapply(sector_uk_proportion,smoothband,mean,align='right',fill=NA),
    ukprop_minustargetITL_movingav = rollapply(sector_uk_proportion_minustargetITL,smoothband,mean,align='right',fill=NA)
  ) %>% #remove NA years smoothing created
  filter(!is.na(regionalprop_movingav))

# twoy <- anim_lq %>% filter(year %in% c(min(year),max(year)))
#Different startyears
startyear = 1998
# startyear = 2010#middle
endyear = 2021

startyear = 1998
endyear = 2009

startyear = 2015
endyear = 2021

startyear = 2008
endyear = 2021

startyear = 1998
endyear = 2007

startyear = 2016
endyear = 2021


twoy <- anim_lq %>% filter(year %in% c(startyear, endyear))

twoy$INDUSTRY_NAME_REDUCED <- gsub(x = twoy$`SIC07 description`, pattern = 'of |and |acture|acturing|activities|equipment|products', replacement = '')

twoy_lags <- twoy %>% 
  arrange(`SIC07 description`,year) %>% 
  mutate(
    lag_sector_regional_proportion = sector_regional_proportion - lag(sector_regional_proportion),
    lag_sector_uk_proportion_minustargetITL = sector_uk_proportion_minustargetITL - lag(sector_uk_proportion_minustargetITL)
  ) %>% 
  filter(year == endyear) %>% #using final year to mark when going in particular compass direction
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
      select(`SIC07 description`,compass),
    by = 'SIC07 description'
  )

#Water and air transport snuck in there somehow and added 2020 in
twoy <- twoy %>% 
  filter(!grepl('Water and air', `SIC07 description`))
  

filtercompass='NE'
filtercompass='NW'
filtercompass='SE'
filtercompass='SW'

#SY growth
filtercompass=c('SE','NE')
#SY shrinkage
filtercompass=c('SW','NW')



# p <- ggplot(
#   twoy %>% filter(compass == filtercompass),
#   aes(x = sector_regional_proportion * 100, y = sector_uk_proportion_minustargetITL * 100)) +
#   geom_point(size = 5, alpha = 0.75, aes(colour = factor(year), group = INDUSTRY_NAME_REDUCED)) +
#   # geom_segment(size = 1, aes(colour = factor(year), group = INDUSTRY_NAME_REDUCED)) +
#   geom_line(size = 1, aes(colour = factor(year), group = INDUSTRY_NAME_REDUCED)) +
#   xlab('South Yorkshire GVA proportion') +
#   ylab('UK GVA proportion (MINUS South Yorkshire)')  +
#   geom_abline(slope = 1, size = 1, colour='blue', alpha = 0.5) +
#   coord_cartesian(xlim = c(0.1,11), ylim = c(0.1,11)) + # good for log scale
#   scale_y_log10() +
#   scale_x_log10() +
#   guides(colour=guide_legend(title=" "))
# 
# 
# #Try using geom_segment to get arrow. Needs the start and end in the same row to be able to plot
# #Need start and end of line available on the same row
# twoy.wide <- twoy %>% filter(compass == filtercompass) %>% 
#   mutate(year = ifelse(year == min(year), 'startyear', 'endyear')) %>% 
#   select(year,sector_uk_proportion_minustargetITL,sector_regional_proportion) %>% 
#   pivot_wider(names_from = year, values_from = c(sector_uk_proportion_minustargetITL,sector_regional_proportion))
# 
# p <- p +
#   geom_segment(data = twoy.wide, aes(x = sector_regional_proportion_startyear * 100, y = sector_uk_proportion_minustargetITL_startyear  * 100, 
#                                      xend = sector_regional_proportion_endyear * 100, yend = sector_uk_proportion_minustargetITL_endyear * 100),
#                arrow = arrow(length = unit(0.5, "cm"))
#                )
# 
# 
# p <- p + geom_text_repel(
#   data = twoy %>% filter(year==max(twoy$year), compass == filtercompass),
#   aes(x = sector_regional_proportion * 100, y = sector_uk_proportion_minustargetITL * 100,label = INDUSTRY_NAME_REDUCED, colour = compass),
#   alpha=1,
#   # colour = 'black',
#   nudge_x = .05,
#   box.padding = 1,
#   nudge_y = 0.05,
#   segment.curvature = -0.1,
#   segment.ncp = 0.3,
#   segment.angle = 20
# ) +
#   # scale_color_brewer(palette = 'Dark2', direction = -1) 
#   scale_color_manual(values = c('red','black','#7fc97f','#beaed4','#fdc086','#1f78b4'))
# 
# 
# p


#Swapping order so arrow below all else, so line doesn't overwrite

#Try using geom_segment to get arrow. Needs the start and end in the same row to be able to plot
#Need start and end of line available on the same row
twoy.wide <- twoy %>% filter(compass %in% filtercompass) %>% 
  mutate(year = ifelse(year == min(year), 'startyear', 'endyear')) %>% 
  select(year,sector_uk_proportion_minustargetITL,sector_regional_proportion) %>% 
  pivot_wider(names_from = year, values_from = c(sector_uk_proportion_minustargetITL,sector_regional_proportion))

p <- ggplot() +
  geom_segment(data = twoy.wide, aes(x = sector_regional_proportion_startyear * 100, y = sector_uk_proportion_minustargetITL_startyear  * 100, 
                                     xend = sector_regional_proportion_endyear * 100, yend = sector_uk_proportion_minustargetITL_endyear * 100),
               arrow = arrow(length = unit(0.5, "cm")),
               size = 1
  )

p <- p + 
  geom_point(data = twoy %>% filter(compass%in%filtercompass), size = 5, alpha = 0.75, 
             aes(x = sector_regional_proportion * 100, y = sector_uk_proportion_minustargetITL * 100,colour = factor(year), group = INDUSTRY_NAME_REDUCED)) +
  # geom_segment(size = 1, aes(colour = factor(year), group = INDUSTRY_NAME_REDUCED)) +
  geom_line(data = twoy %>% filter(compass == filtercompass), size = 1, aes(x = sector_regional_proportion * 100, y = sector_uk_proportion_minustargetITL * 100,colour = factor(year), group = INDUSTRY_NAME_REDUCED)) +
  xlab('South Yorkshire GVA proportion') +
  ylab('UK GVA proportion (MINUS South Yorkshire)')  +
  geom_abline(slope = 1, size = 1, colour='blue', alpha = 0.5) +
  coord_cartesian(xlim = c(0.1,11), ylim = c(0.1,11)) + # good for log scale
  scale_y_log10() +
  scale_x_log10() +
  guides(colour=guide_legend(title=" "))




p <- p + geom_text_repel(
  data = twoy %>% filter(year==max(twoy$year), compass%in%filtercompass),
  aes(x = sector_regional_proportion * 100, y = sector_uk_proportion_minustargetITL * 100,label = INDUSTRY_NAME_REDUCED, colour = compass),
  alpha=1,
  # colour = 'black',
  nudge_x = .05,
  box.padding = 1,
  nudge_y = 0.05,
  segment.curvature = -0.1,
  segment.ncp = 0.3,
  segment.angle = 20,
  max.overlaps = 20
) +
  # scale_color_brewer(palette = 'Dark2', direction = -1) 
  scale_color_manual(values = c('red','black','#7fc97f','#beaed4','#fdc086','#1f78b4'))


p

filtercompasscombined <- paste0(filtercompass, collapse = '_')

ggsave(plot = p, filename = paste0('local/localimages/GVA_2Dplots/SY_LQ2D_',startyear,'_to_',endyear,'_',filtercompasscombined,'.png'), height = 9, width = 9)


#get full sector name list
twoy %>% filter(compass%in%filtercompass) %>% select(`SIC07 description`) %>% pull %>% unique

#Or can I look by hover at ones not labelled?
ggplotly(p, tooltip = c('INDUSTRY_NAME_REDUCED'))


#want to pick out total sectors that are growing in SY 2016-2021 and already a strength here
#So, any that are LQ > 1 and growing
chk <- twoy %>% filter(
  compass%in%filtercompass,
  sector_regional_proportion > sector_uk_proportion_minustargetITL,#LQ > 1 
  year == max(year)
  )

#How much of yearly GVA for final year do those make up?
sum(chk$value)
unique(chk$region_totalsize)
(sum(chk$value)/unique(chk$region_totalsize))*100

#What's the proportion if we exclude health?
chk2 <- twoy %>% filter(
  compass%in%filtercompass,
  sector_regional_proportion > sector_uk_proportion_minustargetITL,#LQ > 1 
  year == max(year),
  `SIC07 description`!='Human health activities'
  )

#How much of yearly GVA for final year do those make up?
sum(chk2$value)
unique(chk2$region_totalsize)
(sum(chk2$value)/unique(chk2$region_totalsize))*100

#SAVE SY GROWTH SECTORS 2016-2021 THAT ARE ALSO LQ>1
saveRDS(chk,'SY_2016_21_growth_plusLQmorethan1.rds')




#REPEAT FOR ALL, NOT JUST THOSE CONCENTRATED MORE IN SY
chk <- twoy %>% filter(
  compass%in%filtercompass,
  # sector_regional_proportion > sector_uk_proportion_minustargetITL,#LQ > 1 
  year == max(year)
)

#How much of yearly GVA for final year do those make up?
sum(chk$value)
unique(chk$region_totalsize)
(sum(chk$value)/unique(chk$region_totalsize))*100

#What's the proportion if we exclude health?
chk2 <- twoy %>% filter(
  compass%in%filtercompass,
  # sector_regional_proportion > sector_uk_proportion_minustargetITL,#LQ > 1 
  year == max(year),
  `SIC07 description`!='Human health activities'
)

#How much of yearly GVA for final year do those make up?
sum(chk2$value)
unique(chk2$region_totalsize)
(sum(chk2$value)/unique(chk2$region_totalsize))*100




#ANIMATED VERSION OF 2D LQ PLOT (BROKEN DOWN BY SOME CATEGORY...?)----

#Let's try doing a moving average window of x years
#Just going to test with ALL sectors regardless of initial direction (probably a mess)

LQ_foranim <- function(startyear,window,subfoldername,sectorstoshow){
  
  twoy <- anim_lq %>% filter(year %in% c(startyear,startyear+window))
  
  twoy$INDUSTRY_NAME_REDUCED <- gsub(x = twoy$`SIC07 description`, pattern = 'of |and |acture|acturing|activities|equipment|products', replacement = '')
  
  twoy_lags <- twoy %>%
    arrange(`SIC07 description`,year) %>%
    mutate(
      lag_sector_regional_proportion = sector_regional_proportion - lag(sector_regional_proportion),
      lag_sector_uk_proportion_minustargetITL = sector_uk_proportion_minustargetITL - lag(sector_uk_proportion_minustargetITL)
    ) %>%
    filter(year == max(year)) %>%
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
        select(`SIC07 description`,compass),
      by = 'SIC07 description'
    )
  # 
  #Water and air transport snuck in there somehow and added 2020 in
  twoy <- twoy %>% 
    filter(!grepl('Water and air', `SIC07 description`))
  
  # filtercompass='NE'
  # filtercompass='NW'
  # filtercompass='SE'
  # filtercompass='SW'
  
  #Select sectors manually
  twoy <- twoy %>% filter(
    `SIC07 description` %in% sectorstoshow
  )
  
  #Try using geom_segment to get arrow. Needs the start and end in the same row to be able to plot
  #Need start and end of line available on the same row
  twoy.wide <- twoy %>% filter(compass%in%filtercompass) %>% 
    mutate(year = ifelse(year == min(year), 'startyear', 'endyear')) %>% 
    select(year,sector_uk_proportion_minustargetITL,sector_regional_proportion) %>% 
    pivot_wider(names_from = year, values_from = c(sector_uk_proportion_minustargetITL,sector_regional_proportion))
  
  p <- ggplot() +
    geom_segment(data = twoy.wide, aes(x = sector_regional_proportion_startyear * 100, y = sector_uk_proportion_minustargetITL_startyear  * 100, 
                                       xend = sector_regional_proportion_endyear * 100, yend = sector_uk_proportion_minustargetITL_endyear * 100),
                 arrow = arrow(length = unit(0.25, "cm")),
                 size = 1,
                 lineend = 'round', linejoin = 'round'
    )
  
  p <- p + 
    geom_point(data = twoy %>% filter(compass %in% filtercompass), size = 5, alpha = 0.75, 
               aes(x = sector_regional_proportion * 100, y = sector_uk_proportion_minustargetITL * 100,colour = factor(year), group = INDUSTRY_NAME_REDUCED)) +
    geom_line(data = twoy %>% filter(compass%in%filtercompass), size = 1, aes(x = sector_regional_proportion * 100, y = sector_uk_proportion_minustargetITL * 100,group = INDUSTRY_NAME_REDUCED), colour = 'red') +
    # geom_line(data = twoy %>% filter(compass%in%filtercompass), size = 1, aes(x = sector_regional_proportion * 100, y = sector_uk_proportion_minustargetITL * 100,colour = factor(year), group = INDUSTRY_NAME_REDUCED)) +
    xlab('South Yorkshire GVA proportion') +
    ylab('UK GVA proportion (MINUS South Yorkshire)')  +
    geom_abline(slope = 1, size = 1, colour='blue', alpha = 0.5) +
    coord_cartesian(xlim = c(0.1,11), ylim = c(0.1,11)) + # good for log scale
    scale_y_log10() +
    scale_x_log10() +
    guides(colour=guide_legend(title=" "))
  
  p <- p + geom_text_repel(
    data = twoy %>% filter(year==max(twoy$year), compass%in%filtercompass),
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
  

  
  ggsave(plot = p, filename = paste0('local/localimages/GVA_2DLQPLOTSOVERTIME/',subfoldername,'/',startyear,'_to_',startyear+window,'.png'), height = 9, width = 9)
  
}


filtercompass= c('NE','NW','SE','SW')
lapply(1998:2016, function(x) LQ_foranim(startyear = x, window = 5, subfoldername = 'allsectors_fiveyearwindow'))


filtercompass= c('NE','NW','SE','SW')
lapply(1998:2016, function(x) 
  LQ_foranim(startyear = x, window = 5, subfoldername = 'manuf_fiveyearwindow',
             sectorstoshow = unique(itl2.cp$`SIC07 description`[grepl('manuf',itl2.cp$`SIC07 description`,ignore.case = T)]))
  )

filtercompass= c('NE','NW','SE','SW')
lapply(1998:2016, function(x) 
  LQ_foranim(startyear = x, window = 5, subfoldername = 'nonmanuf_fiveyearwindow',
             sectorstoshow = unique(itl2.cp$`SIC07 description`[!grepl('manuf',itl2.cp$`SIC07 description`,ignore.case = T)]))
  )















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
gva_w_jobcounts <- readRDS('data/UK_GVA_with_BRES_jobcounts_and_jobcounts_summedfrom5digitSIC.rds')



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


#saving gva_job5 for later
saveRDS(gva_jobs5,'data/gva_per_worker_from5digitSIC_BRESsums.rds')


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

xg <- gva_jobs5 %>% filter(GEOGRAPHY_NAME == place, INDUSTRY_NAME %in% industries) %>% ungroup()

y <- xg %>% 
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

xg <- xg %>% 
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
plot_ly(data = xg, x = ~DATE, y = ~JOBCOUNT_FT, color = ~INDUSTRY_NAME,
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
xg <- gva_jobs5 %>% filter(DATE==2021)

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

xg <- xg %>% 
  left_join(
    minmaxes,
    by = c("GEOGRAPHY_NAME","INDUSTRY_NAME")
  )


#Look at one sector, all places, compare full range
ggplot(xg,
       aes(y = GEOGRAPHY_NAME)) +
  geom_errorbar(aes(y = GEOGRAPHY_NAME, xmin = minn_ft, xmax = maxx_ft)) +
  facet_wrap(~INDUSTRY_NAME)

place='South Yorkshire'

#Ooo, think we need to order those and look at all
for(i in unique(gva_jobs5$INDUSTRY_NAME)){
  
  y <- xg %>% filter(INDUSTRY_NAME == i, JOBCOUNT_FT >= 500) %>% 
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
    guides(colour=F) +
    ylab("")
  
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
sectors <- unique(xg$INDUSTRY_NAME)[grepl('basic metals|fabricated metal|machinery and equipment', unique(xg$INDUSTRY_NAME))]


y <- xg %>% filter(INDUSTRY_NAME %in% sectors, JOBCOUNT_FT >= 500) %>% 
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
sectors <- unique(xg$INDUSTRY_NAME)[grepl('basic metals|fabricated metal|machinery and equipment', unique(xg$INDUSTRY_NAME))]
sectors <- unique(xg$INDUSTRY_NAME)[grepl('basic metals|fabricated metal|Manufacture of machinery and equipment|Manufacture of food products', unique(xg$INDUSTRY_NAME))]
sectors <- unique(xg$INDUSTRY_NAME)[grepl('research and development|food products|non-metallic', unique(xg$INDUSTRY_NAME))]
sectors <- unique(xg$INDUSTRY_NAME)[grepl('construction of buildings|education', unique(xg$INDUSTRY_NAME), ignore.case = T)]
sectors <- unique(xg$INDUSTRY_NAME)[grepl('education|human health|public admin|wholesale trade of motor|warehousing', unique(xg$INDUSTRY_NAME), ignore.case = T)]
sectors <- unique(xg$INDUSTRY_NAME)[grepl('education|human health|public admin|legal and accounting', unique(xg$INDUSTRY_NAME), ignore.case = T)]
sectors <- unique(xg$INDUSTRY_NAME)[grepl('wholesale trade|Residential care|sports|postal', unique(xg$INDUSTRY_NAME), ignore.case = T)]


#Get sectors for 2016-21 SY growth plus LQ > 1
chk <- readRDS('SY_2016_21_growth_plusLQmorethan1.rds')

sectors <- unique(chk$`SIC07 description`)


#Get list of plots
spanplot <- function(sector, includeplacesnames = T){
  
  y <- xg %>% filter(INDUSTRY_NAME %in% sector, JOBCOUNT_FT >= 250) %>% 
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
    ylab('')
  
  if(!includeplacesnames){
    p <- p +
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
  } else {
    p <- p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
  }
  
  
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

plotz <- lapply(sectors, function(x) spanplot(x, includeplacesnames = F))

cp <- plot_grid(plotlist = plotz, nrow = 4)

save_plot(plot = cp, filename = paste0('local/localimages/GVA_perc_minmax/',paste(sectors, collapse = '_'),'.png'), base_height = 4, base_width = 15)
# save_plot(plot = cp, filename = paste0('local/localimages/GVA_perc_minmax/',paste(sectors, collapse = '_'),'.png'), base_height = 6, base_width = 18)

#For large plot
save_plot(plot = cp, filename = paste0('local/localimages/GVA_perc_minmax/',paste(sectors, collapse = '_'),'.png'), base_height = 25, base_width = 50, limitsize=F)

#ot that large!
save_plot(plot = cp, filename = paste0('local/localimages/GVA_perc_minmax/',paste(sectors, collapse = '_'),'.png'), base_height = 15, base_width = 20, limitsize=F)







#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#ITL3 CURRENT PRICE DATA PROCESSING----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Remove any rows that are totals of other rows
#Only want to keep unique SIC values that then sum to the national total
#Different regional scales have slightly different SIC categories, so need a different remove list for each
ITL3_SICremoves = c(
  'Total',#leave this in the CSV by commenting out, to check the categories left over total correctly in lines 42-63 below (then can remove by uncommenting)
  'A-E',
  'C (10-33)',
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
#Given that - the GVA current price values actually sum correctly across industries and within regions (unlike chained volume)
itl3.cp <- read_csv('data/sectors/Table 3c ITL3 UK current price estimates pounds million.csv')

#Filter out duplicate value rows and make long by year
#Also convert year to numeric
itl3.cp <- itl3.cp %>% filter(!`SIC07 code` %in% ITL3_SICremoves) %>% 
  pivot_longer(`1998`:`2021`, names_to = 'year', values_to = 'value') %>% 
  mutate(year = as.numeric(year))

#Check totals match... 
# chk1 <- itl3.cp %>% 
#   filter(`SIC07 code`!='Total') %>% 
#   group_by(year, `ITL region name`) %>% 
#   summarise(sum = sum(value))
# 
# chk2 <- itl3.cp %>% 
#   filter(`SIC07 code`=='Total')
# 
# both <- chk1 %>% 
#   left_join(
#     chk2,
#     by = c('year','ITL region name')
#   )

#tick, within rounding error
# max(abs(both$sum-both$value))
# table(abs(both$sum-both$value)==0)


#Check on just water and air transport, where the neg values are
# itl3.cp %>% filter(`SIC07 description` == "Water and air transport") %>% View

#RANDOM NEGATIVE VALUES IN THERE
#Looking, I think just typos given previous data
#(And also makes no logical sense, so...)
# itl3.cp %>% filter(value < 0)

#Any neg values in ITL3 like with 2? Yup. Water again.
# table(itl3.cp$value < 0)
# itl3.cp %>% filter(value < 0) %>% View

#NA any negative values in GVA
#Only 2021 values for water transport, for four places, again, same as itl2
itl3.cp <- itl3.cp %>% 
  mutate(value = ifelse(value < 0, NA, value))

#Turn any columns with spaces into snake case
names(itl3.cp) <- gsub(x = names(itl3.cp), pattern = ' ', replacement = '_')

write_csv(itl3.cp, 'data/ITL3currentprices_long.csv')





#~~~~~~~~~~~~~~~~~
#ITL3 LQ PLOTS----
#~~~~~~~~~~~~~~~~~

itl3.cp <- itl3.cp %>% 
  split(.$year) %>% 
  map(add_location_quotient_and_proportions, 
      regionvar = ITL_region_name,
      lq_var = SIC07_description,
      valuevar = value) %>% 
  bind_rows()


#Looking at proportions for Sheffield vs 3places
itl3.cp %>% filter(
  grepl(pattern = 'sheffield', x = ITL_region_name, ignore.case = T),
  year == 2021
) %>% 
  mutate(regional_percent = sector_regional_proportion *100) %>% 
  select(SIC07_description,regional_percent, LQ) %>% 
  arrange(-LQ) %>% 
  slice(1:20)

itl3.cp %>% filter(
  grepl(pattern = 'rotherham', x = ITL_region_name, ignore.case = T),
  year == 2021
) %>% 
  mutate(regional_percent = sector_regional_proportion *100) %>% 
  select(SIC07_description,regional_percent, LQ) %>% 
  arrange(-LQ) %>% 
  slice(1:20)

#Just check LQ UK spread is broadly similar...
#Actually, no it isn't. That's curious.
LQspread <- itl3.cp %>% 
  filter(year == 2021) %>% 
  group_by(SIC07_description) %>% 
  summarise(LQ_spread = diff(range(LQ))) %>% 
  arrange(-LQ_spread)

#Show top 
LQspread[1:20,]

#Maps will be different
#Load ITL2 map data using the sf library
itl3.geo <- st_read('data/geographies/International_Territorial_Level_3_January_2021_UK_BUC_V3_2022_6920195468392554877/ITL3_JAN_2021_UK_BUC_V3.shp', quiet = T) %>% st_simplify(preserveTopology = T, dTolerance = 100)

#Check match
itl3.geo$ITL321NM[!itl3.geo$ITL321NM %in% itl3.cp$ITL_region_name]
unique(itl3.cp$ITL_region_name[!itl3.cp$ITL_region_name %in% itl3.geo$ITL321NM])

#Note: no Northern Ireland because of BRES link, which is GB only
itl3.geo$ITL321NM[itl3.geo$ITL321NM == 'Durham'] <- 'Durham CC'
itl3.geo$ITL321NM[itl3.geo$ITL321NM == 'Shropshire'] <- 'Shropshire CC'
itl3.geo$ITL321NM[itl3.geo$ITL321NM == 'Buckinghamshire'] <- 'Buckinghamshire CC'
itl3.geo$ITL321NM[itl3.geo$ITL321NM == 'Inverness and Nairn, Moray, and Badenoch and Strathspey'] <- 'Inverness and Nairn, Moray, Badenoch and Strathspey'


sel = 10

#Join map data to a subset of the GVA data
sector_LQ_map <- itl3.geo %>% 
  right_join(
    itl3.cp %>% filter(
      year==2021,
      SIC07_description == LQspread$SIC07_description[sel]#picking out the fourth highest geographical spread sector
    ),
    by = c('ITL321NM'='ITL_region_name')
  )

#Plot map
tm_shape(sector_LQ_map) +
  tm_polygons('LQ_log', n = 9) +
  tm_layout(title = paste0('LQ spread of\n',LQspread$SIC07_description[sel],'\nAcross ITL3 regions'), legend.outside = T)



#OK, let's see that in LQ plot form
#Use
#LQ_slopes %>% filter(slope==0)
#To see which didn't get slopes (only 8 rows in the current data)
LQ_slopes <- get_slope_and_se_safely(
  data = itl3.cp, 
  ITL_region_name, SIC07_description,#slopes will be found within whatever grouping vars are added here
  y = LQ_log, x = year)

#Filter down to a single year
yeartoplot <- itl3.cp %>% filter(year == 2021)

#Add slopes into data to get LQ plots
yeartoplot <- yeartoplot %>% 
  left_join(
    LQ_slopes,
    by = c('ITL_region_name','SIC07_description')
  )

#Get min/max values for LQ over time as well, for each sector and place, to add as bars so range of sector is easy to see
minmaxes <- itl3.cp %>% 
  group_by(SIC07_description,ITL_region_name) %>% 
  summarise(
    min_LQ_all_time = min(LQ),
    max_LQ_all_time = max(LQ)
  )

#Join min and max
yeartoplot <- yeartoplot %>% 
  left_join(
    minmaxes,
    by = c('ITL_region_name','SIC07_description')
  )

place = 'Sheffield'

#Get a vector with sectors ordered by the place's LQs, descending order
#Use this next to factor-order the SIC sectors
sectorLQorder <- itl3.cp %>% filter(
  ITL_region_name == place,
  year == 2021
) %>% 
  arrange(-LQ) %>% 
  select(SIC07_description) %>% 
  pull()

#Turn the sector column into a factor and order by LCR's LQs
yeartoplot$SIC07_description <- factor(yeartoplot$SIC07_description, levels = sectorLQorder, ordered = T)

# Reduce to SY LQ 1+
lq.selection <- yeartoplot %>% filter(
  ITL_region_name == place,
  # slope > 1,#LQ grew relatively over time
  LQ > 1
)
# 
#Keep only sectors that were LQ > 1 from the main plotting df
yeartoplot <- yeartoplot %>% filter(
  SIC07_description %in% lq.selection$SIC07_description
)

#All the names
unique(itl3.cp$ITL_region_name)[order(unique(itl3.cp$ITL_region_name))]


p <- LQ_baseplot(df = yeartoplot, alpha = 0, sector_name = SIC07_description, 
                 LQ_column = LQ, change_over_time = slope)

# p <- addplacename_to_LQplot(df = yeartoplot, plot_to_addto = p, 
#                             placename = 'Manchester', shapenumber = 23,
#                             region_name = ITL_region_name,#The next four, the function needs them all 
#                             sector_name = SIC07_description, change_over_time = slope, LQ_column = LQ)
# 
# p <- addplacename_to_LQplot(df = yeartoplot, plot_to_addto = p, 
#                             placename = 'Leeds', shapenumber = 22,
#                             region_name = ITL_region_name,
#                             sector_name = SIC07_description, change_over_time = slope, LQ_column = LQ)

p <- addplacename_to_LQplot(df = yeartoplot, plot_to_addto = p, 
                            placename = 'Barnsley, Doncaster and Rotherham', shapenumber = 23,
                            region_name = ITL_region_name,
                            sector_name = SIC07_description, change_over_time = slope, LQ_column = LQ)

p <- addplacename_to_LQplot(df = yeartoplot, plot_to_addto = p, 
                            placename = place, shapenumber = 16,
                            min_LQ_all_time = min_LQ_all_time,max_LQ_all_time = max_LQ_all_time,#Include minmax
                            value_column = value, sector_regional_proportion = sector_regional_proportion,#include numbers
                            region_name = ITL_region_name,
                            sector_name = SIC07_description, change_over_time = slope, LQ_column = LQ)
p <- p + 
  annotate(
    "text",
    label = "Sheffield: Circles\nB/D/R: diamonds",
    x = 0.05, y = 10,
    
  )

p







#Place 3 places in priority...
#Filter down to a single year
yeartoplot <- itl3.cp %>% filter(year == 2021)

#Add slopes into data to get LQ plots
yeartoplot <- yeartoplot %>% 
  left_join(
    LQ_slopes,
    by = c('ITL_region_name','SIC07_description')
  )

#Get min/max values for LQ over time as well, for each sector and place, to add as bars so range of sector is easy to see
minmaxes <- itl3.cp %>% 
  group_by(SIC07_description,ITL_region_name) %>% 
  summarise(
    min_LQ_all_time = min(LQ),
    max_LQ_all_time = max(LQ)
  )

#Join min and max
yeartoplot <- yeartoplot %>% 
  left_join(
    minmaxes,
    by = c('ITL_region_name','SIC07_description')
  )

place = yeartoplot$ITL_region_name[grepl(pattern = 'Rother',x = yeartoplot$ITL_region_name, ignore.case = T)] %>% unique

#Get a vector with sectors ordered by the place's LQs, descending order
#Use this next to factor-order the SIC sectors
sectorLQorder <- itl3.cp %>% filter(
  ITL_region_name == place,
  year == 2021
) %>% 
  arrange(-LQ) %>% 
  select(SIC07_description) %>% 
  pull()

#Turn the sector column into a factor and order by LCR's LQs
yeartoplot$SIC07_description <- factor(yeartoplot$SIC07_description, levels = sectorLQorder, ordered = T)

# Reduce to SY LQ 1+
lq.selection <- yeartoplot %>% filter(
  ITL_region_name == place,
  # slope > 1,#LQ grew relatively over time
  LQ > 1
)
# 
#Keep only sectors that were LQ > 1 from the main plotting df
yeartoplot <- yeartoplot %>% filter(
  SIC07_description %in% lq.selection$SIC07_description
)



p <- LQ_baseplot(df = yeartoplot, alpha = 0, sector_name = SIC07_description, 
                 LQ_column = LQ, change_over_time = slope)

# p <- addplacename_to_LQplot(df = yeartoplot, plot_to_addto = p, 
#                             placename = 'Manchester', shapenumber = 23,
#                             region_name = ITL_region_name,#The next four, the function needs them all 
#                             sector_name = SIC07_description, change_over_time = slope, LQ_column = LQ)
# 
# p <- addplacename_to_LQplot(df = yeartoplot, plot_to_addto = p, 
#                             placename = 'Leeds', shapenumber = 22,
#                             region_name = ITL_region_name,
#                             sector_name = SIC07_description, change_over_time = slope, LQ_column = LQ)

p <- addplacename_to_LQplot(df = yeartoplot, plot_to_addto = p, 
                            placename = 'Sheffield', shapenumber = 23,
                            region_name = ITL_region_name,
                            sector_name = SIC07_description, change_over_time = slope, LQ_column = LQ)

p <- addplacename_to_LQplot(df = yeartoplot, plot_to_addto = p, 
                            placename = place, shapenumber = 16,
                            min_LQ_all_time = min_LQ_all_time,max_LQ_all_time = max_LQ_all_time,#Include minmax
                            value_column = value, sector_regional_proportion = sector_regional_proportion,#include numbers
                            region_name = ITL_region_name,
                            sector_name = SIC07_description, change_over_time = slope, LQ_column = LQ)
p <- p + 
  annotate(
    "text",
    label = "B/D/R: Circles, Sheffield: diamonds",
    x = 0.05, y = 2,
    
  )

p





#TIMEPLOT
#Pick a sector to plot separately for all places
#Use grepl as a shortcut to search for sector names
sector <- itl3.cp$SIC07_description[grepl('fabricated metal', itl3.cp$SIC07_description ,ignore.case = T)] %>% unique
sector <- itl3.cp$SIC07_description[grepl('other manuf', itl3.cp$SIC07_description ,ignore.case = T)] %>% unique
sector <- itl3.cp$SIC07_description[grepl('education', itl3.cp$SIC07_description ,ignore.case = T)] %>% unique
sector <- itl3.cp$SIC07_description[grepl('telecom', itl3.cp$SIC07_description ,ignore.case = T)] %>% unique
sector <- itl3.cp$SIC07_description[grepl('pension funding', itl3.cp$SIC07_description ,ignore.case = T)] %>% unique
sector <- itl3.cp$SIC07_description[grepl('petrol', itl3.cp$SIC07_description ,ignore.case = T)] %>% unique
sector <- itl3.cp$SIC07_description[grepl('motor trades', itl3.cp$SIC07_description ,ignore.case = T)] %>% unique

timeplot <- itl3.cp %>% 
  filter(SIC07_description == sector) 

#Use zoo's rollapply function to get a moving average
timeplot <- timeplot %>% 
  group_by(ITL_region_name) %>% 
  arrange(year) %>% 
  mutate(
    LQ_movingav = rollapply(LQ,3,mean,align='right',fill=NA),
    percent_movingav = rollapply(sector_regional_proportion * 100,3,mean,align='right',fill=NA)
  )

#Or pick top size values
#Largest % in 2021
largest_percents <- timeplot %>% 
  filter(year == 2021) %>% 
  arrange(-percent_movingav)

#Keep only the top ten places and order them
timeplot <- timeplot %>% 
  mutate(ITL_region_name = factor(ITL_region_name, ordered = T, levels = largest_percents$ITL_region_name)) %>% 
  filter(ITL_region_name %in% c(largest_percents$ITL_region_name[1:10],'Sheffield','Barnsley, Doncaster and Rotherham'))


places = c('Sheffield','Barnsley, Doncaster and Rotherham')

#Mark the ITL of interest so it can be clearer in the plot
timeplot <- timeplot %>%
  mutate(
    ITL2ofinterest = ifelse(ITL_region_name %in% places, 'ITL of interest','other'),
  )

ggplot(timeplot %>% 
         rename(`ITL region` = ITL_region_name) %>% 
         filter(!is.na(percent_movingav)),#remove NAs from dates so the x axis doesn't show them
       aes(x = year, y = percent_movingav, colour = `ITL region`, size = ITL2ofinterest, linetype = ITL2ofinterest, group = `ITL region`)) +
  geom_point() +
  geom_line() +
  scale_size_manual(values = c(4,1)) +
  scale_color_brewer(palette = 'Paired', direction = 1) +
  ylab('Regional GVA percent') +
  scale_y_log10() +
  guides(size = "none", linetype = "none") +
  ggtitle(
    paste0(sector,'\n', paste0(places, collapse = ', '), ' highlighted in thicker lines')
  ) +
  theme(plot.title = element_text(face = 'bold'))


# timeplot <- timeplot %>% 
#   mutate(percent_sector_regional_proportion = sector_regional_proportion * 100)



#Hmm, again, think I'd like to see all those plz!
sectors <- unique(itl3.cp$SIC07_description)

for(i in sectors){
  
  timeplot <- itl3.cp %>% 
    filter(SIC07_description == i) 
  
  #Use zoo's rollapply function to get a moving average
  timeplot <- timeplot %>% 
    group_by(ITL_region_name) %>% 
    arrange(year) %>% 
    mutate(
      LQ_movingav = rollapply(LQ,3,mean,align='right',fill=NA),
      percent_movingav = rollapply(sector_regional_proportion * 100,3,mean,align='right',fill=NA)
    )
  
  #Or pick top size values
  #Largest % in 2021
  largest_percents <- timeplot %>% 
    filter(year == 2021) %>% 
    arrange(-percent_movingav)
  
  #Keep only the top ten places and order them
  timeplot <- timeplot %>% 
    mutate(ITL_region_name = factor(ITL_region_name, ordered = T, levels = largest_percents$ITL_region_name)) %>% 
    filter(ITL_region_name %in% c(largest_percents$ITL_region_name[1:10],'Sheffield','Barnsley, Doncaster and Rotherham'))
  
  
  places = c('Sheffield','Barnsley, Doncaster and Rotherham')
  
  #Mark the ITL of interest so it can be clearer in the plot
  timeplot <- timeplot %>%
    mutate(
      ITL2ofinterest = ifelse(ITL_region_name %in% places, 'ITL of interest','other'),
    )
  
  p <- ggplot(timeplot %>% 
           rename(`ITL region` = ITL_region_name) %>% 
           filter(!is.na(percent_movingav)),#remove NAs from dates so the x axis doesn't show them
         aes(x = year, y = percent_movingav, colour = `ITL region`, size = ITL2ofinterest, linetype = ITL2ofinterest, group = `ITL region`)) +
    geom_point() +
    geom_line() +
    scale_size_manual(values = c(4,1)) +
    scale_color_brewer(palette = 'Paired', direction = 1) +
    ylab('Regional GVA percent') +
    # scale_y_log10() +
    guides(size = "none", linetype = "none") +
    ggtitle(
      paste0(i,'\n', paste0(places, collapse = ', '), ' highlighted in thicker lines')
    ) +
    theme(plot.title = element_text(face = 'bold'))
  
  ggsave(plot = p, filename = paste0('local/localimages/Shef_3places_GVAsectors/',i,'.png'), height = 8, width = 13)
  
  
}












#TOP SECTORS BY LQ GROWTH TREND
#Look just at place of interest
#And arrange by the 'growth' slope.
place = 'Sheffield'
place = 'Barnsley, Doncaster and Rotherham'

#Look just at place of interest
#And arrange by the 'growth' slope.
place_slopes <- yeartoplot %>% 
  filter(ITL_region_name == place) %>% 
  arrange(-slope)

#Use that to filter the main df and order sectors by which slope is largest
timeplot.sectors <- itl3.cp %>% 
  filter(ITL_region_name == place) %>% 
  mutate(SIC07_description = factor(SIC07_description, ordered = T, levels = place_slopes$SIC07_description))

#Moving averages
timeplot.sectors <- timeplot.sectors %>% 
  group_by(SIC07_description) %>% 
  arrange(year) %>% 
  mutate(
    LQ_movingav = rollapply(LQ,3,mean,align='right',fill=NA),
    percent_movingav = rollapply(sector_regional_proportion * 100,3,mean,align='right',fill=NA)
  )

#Filter down to top ten LQ growth sectors
timeplot.sectors <- timeplot.sectors %>% 
  filter(
    SIC07_description %in% place_slopes$SIC07_description[1:10]
  )

#Plot GVA percent of the largest LQ growth sectors
ggplot(timeplot.sectors %>% 
         rename(Sector = SIC07_description) %>% 
         filter(!is.na(percent_movingav)),#remove NAs from dates so the x axis doesn't show them
       aes(x = year, y = percent_movingav, colour = Sector, group = Sector)) +
       # aes(x = year, y = sector_regional_proportion*100, colour = Sector, group = Sector)) +
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = 'Paired', direction = -1) +
  ylab('GVA percent') +
  guides(size = "none", linetype = "none") +
  ggtitle(
    paste0('Top ten sectors by LQ growth trend\n', place)
  ) +
  theme(plot.title = element_text(face = 'bold'))





#LQ 2D PLOTS
#quick Sheffield / other 2D LQ plot comparison?
#Northern England
north <- itl2.cp$ITL_region_name[grepl('Greater Manc|Merseyside|West Y|Cumbria|Cheshire|Lancashire|East Y|North Y|Tees|Northumb|South Y', itl2.cp$ITL_region_name, ignore.case = T)] %>% unique

#South England
south <- itl2.cp$ITL_region_name[!grepl('Greater Manc|Merseyside|West Y|Cumbria|Cheshire|Lancashire|East Y|North Y|Tees|Northumb|South Y|Scot|Highl|Wales|Ireland', itl2.cp$ITL_region_name, ignore.case = T)] %>% unique


xplace = 'Sheffield'
yplace = 'Barnsley, Doncaster and Rotherham'

p <- twod_proportionplot(
  df = itl3.cp,
  # df = itl3.cp %>% filter(ITL_region_name %in% c('Sheffield','Barnsley, Doncaster and Rotherham')),#just checking, doing this shouldn't make a difference
  regionvar = ITL_region_name,
  category_var = SIC07_description, 
  valuevar = value, 
  timevar = year, 
  start_time = 2017,
  end_time = 2021,
  # start_time = 1998,
  # end_time = 2007,
  # compasspoints_to_display = c('SE','SW'),
  # compasspoints_to_display = c('NE'),
  # compasspoints_to_display = c('SE'),
  compasspoints_to_display = c('NW'),
  # compasspoints_to_display = c('SW'),
  x_regionnames = xplace,
  y_regionnames = yplace
)

#add these after
p <- p + 
  xlab(paste0(xplace,' GVA proportions')) +
  ylab(paste0(yplace,' GVA proportions')) +
  scale_y_log10() +
  scale_x_log10() +
  coord_fixed(xlim = c(0.1,11), ylim = c(0.1,11))# good for log scale

p





#PULL OUT SHEFFIELD AND ROTHERHAM SIG GVA GROWTH TRENDS
itl3.cp <- itl3.cp %>% 
  mutate(
    log_sector_regional_proportion = log(sector_regional_proportion),
    INDUSTRY_NAME_REDUCED = gsub(x = SIC07_description, pattern = 'of |and |acture|acturing| activities| equipment| products', replacement = '')
    )

#Slopes only for more recent data
gva_slopes <- get_slope_and_se_safely(data = itl3.cp %>% filter(year %in% 2015:2021), 
                                      ITL_region_name, SIC07_description, y = "log_sector_regional_proportion", x = "year") %>%
  mutate(
    min95 = slope - (se * 1.96),
    max95 = slope + (se * 1.96),
    min90 = slope - (se * 1.645),
    max90 = slope + (se * 1.645),
    crosseszero95 = min95 * max95 < 0,#mark if crosses zero
    crosseszero90 = min90 * max90 < 0,#mark if crosses zero
    slopepolarity = ifelse(slope > 0, 'increasing', 'decreasing')
  )



itl3.gvax <- itl3.cp %>% filter(year == 2021) %>% 
  left_join(
    gva_slopes,
    by = c('SIC07_description','ITL_region_name')
  )

place = 'Sheffield'
place = 'Barnsley, Doncaster and Rotherham'


itl3.gva.plot <- itl3.gvax %>% 
  filter(
    ITL_region_name == place,
    !is.na(slope)
  ) 


#https://stackoverflow.com/a/38862452
a <- ifelse(itl3.gva.plot$crosseszero95[order(itl3.gva.plot$slope)], "Grey", "Black")

#95% CIs
ggplot(itl3.gva.plot, 
       aes(x = slope, y = fct_reorder(INDUSTRY_NAME_REDUCED,slope), colour = crosseszero95, size = crosseszero95)) +
  geom_point(size = 2) +
  geom_errorbar(aes(xmin = min95, xmax = max95)) +
  geom_vline(xintercept = 0, colour = 'red', alpha = 0.5) +
  scale_size_manual(values = c(1,0.5)) +
  scale_colour_manual(values = c('firebrick4','gray30')) +
  theme(axis.text.y = element_text(colour = a))

b <- ifelse(itl3.gva.plot$crosseszero90[order(itl3.gva.plot$slope)], "Grey", "Black")

#90% CIs
#Facetting doesn't work for the axis text colouring
ggplot(itl3.gva.plot, 
       aes(x = slope *100, y = fct_reorder(INDUSTRY_NAME_REDUCED,slope), colour = crosseszero90, size = crosseszero90)) +
  geom_point(size = 2) +
  geom_errorbar(aes(xmin = min90*100, xmax = max90*100)) +
  geom_vline(xintercept = 0, colour = 'red', alpha = 0.5) +
  scale_size_manual(values = c(1,0.5)) +
  scale_colour_manual(values = c('firebrick4','gray30')) +
  theme(axis.text.y = element_text(colour = b)) 






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#GVA PER WORKER SLOPES, CHECKING FOR SIGNIFICANCE----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#One I made earlier
gva_jobs5 <- readRDS('local/data/misc/gva_per_worker_from5digitSIC_BRESsums.rds')

#Sanity check via plotting some of these
#Pick one we know is sig (in theory)
chk <- gva_jobs5 %>% 
  filter(
    # GEOGRAPHY_NAME=='South Yorkshire',
    GEOGRAPHY_NAME=='Inner London - West',
    # INDUSTRY_NAME=='Manufacture of electrical equipment'
    # grepl(pattern = 'fabricated metal', x = INDUSTRY_NAME, ignore.case = T)
    # grepl(pattern = 'services to buildings', x = INDUSTRY_NAME, ignore.case = T)
    grepl(pattern = 'scientific research', x = INDUSTRY_NAME, ignore.case = T)
    
    )


#Ah, I have not logged these.
#Shouldn't change sigs but means can't interpret as % change per year
#Log it up
gva_jobs5 <- gva_jobs5 %>% 
  mutate(
    log_GVA_aspercentofGBtotal_perFTjob = log(GVA_aspercentofGBtotal_perFTjob),
    log_GVA_aspercentofGBtotal_perEMPLOYMENT = log(GVA_aspercentofGBtotal_perEMPLOYMENT)
  )

ggplot(chk, aes(x = DATE, y = log_GVA_aspercentofGBtotal_perFTjob * 100)) +
  geom_point(size = 2) +
  geom_smooth(method = 'lm')

#FT employees
# FT_slopes <- get_slope_and_se_safely(data = gva_jobs5, GEOGRAPHY_NAME, INDUSTRY_NAME, y = "GVA_aspercentofGBtotal_perFTjob", x = "DATE") %>%
FT_slopes <- get_slope_and_se_safely(data = gva_jobs5, GEOGRAPHY_NAME, INDUSTRY_NAME, y = "log_GVA_aspercentofGBtotal_perFTjob", x = "DATE") %>%
  mutate(
    min95 = slope - (se * 1.96),
    max95 = slope + (se * 1.96),
    min90 = slope - (se * 1.645),
    max90 = slope + (se * 1.645),
    crosseszero95 = min95 * max95 < 0,#mark if crosses zero
    crosseszero90 = min90 * max90 < 0,#mark if crosses zero
    slopepolarity = ifelse(slope > 0, 'increasing', 'decreasing')
  )

#... and EMPLOYMENT (all jobs)
EMP_slopes <- get_slope_and_se_safely(data = gva_jobs5, GEOGRAPHY_NAME, INDUSTRY_NAME, y = "log_GVA_aspercentofGBtotal_perEMPLOYMENT", x = "DATE") %>% mutate(
  min95 = slope - (se * 1.96),
  max95 = slope + (se * 1.96),
  min90 = slope - (se * 1.645),
  max90 = slope + (se * 1.645),
  crosseszero95 = min95 * max95 < 0,#mark if crosses zero
  crosseszero90 = min90 * max90 < 0,#mark if crosses zero
  slopepolarity = ifelse(slope > 0, 'increasing', 'decreasing')
)
  


#Quick check - pretty sure log and non log will have the same categories crossing zero, they should. But just confirming.
#Note, the order in the plot does change but that's not important.
# FT_slopes_log <- get_slope_and_se_safely(data = gva_jobs5, GEOGRAPHY_NAME, INDUSTRY_NAME, y = "log_GVA_aspercentofGBtotal_perFTjob", x = "DATE") %>%
#   mutate(
#     min95 = slope - (se * 1.96),
#     max95 = slope + (se * 1.96),
#     min90 = slope - (se * 1.645),
#     max90 = slope + (se * 1.645),
#     crosseszero95 = min95 * max95 < 0,#mark if crosses zero
#     crosseszero90 = min90 * max90 < 0#mark if crosses zero
#   )
# 
# 
# FT_slopes_nonlog <- get_slope_and_se_safely(data = gva_jobs5, GEOGRAPHY_NAME, INDUSTRY_NAME, y = "GVA_aspercentofGBtotal_perFTjob", x = "DATE") %>%
#   mutate(
#     min95 = slope - (se * 1.96),
#     max95 = slope + (se * 1.96),
#     min90 = slope - (se * 1.645),
#     max90 = slope + (se * 1.645),
#     crosseszero95 = min95 * max95 < 0,#mark if crosses zero
#     crosseszero90 = min90 * max90 < 0#mark if crosses zero
#   )
# 
# #Mostly. Think we get a few edge cases. Assuming the falses are close
# table(FT_slopes_log$crosseszero95 == FT_slopes_nonlog$crosseszero95)
# table(FT_slopes_log$crosseszero90 == FT_slopes_nonlog$crosseszero90)
# 
# #Aye, all edge cases
# chk <- FT_slopes_log %>% ungroup() %>% filter(crosseszero95 != FT_slopes_nonlog$crosseszero95)
# ggplot(chk, 
#        aes(x = slope, y = fct_reorder(INDUSTRY_NAME,slope), colour = crosseszero95, size = crosseszero95)) +
#   geom_point(size = 2) +
#   geom_errorbar(aes(xmin = min95, xmax = max95)) +
#   geom_vline(xintercept = 0, colour = 'red', alpha = 0.5) +
#   scale_size_manual(values = c(1,0.5)) +
#   scale_colour_manual(values = c('firebrick4','gray30')) +
#   theme(axis.text.y = element_text(colour = a))

# gvax <- gva_jobs5 %>% filter(DATE == 2021) %>% mutate(flaggedplace = ifelse(GEOGRAPHY_NAME==place, 'A', 'B'))

#Let's just look at FT for now...
gvax <- gva_jobs5 %>% filter(DATE == 2021) %>% 
  # left_join(
  #   EMP_slopes %>% rename(slope_EMP = slope, ),
  #   by = c('INDUSTRY_NAME','GEOGRAPHY_NAME')
  # ) %>% 
  left_join(
    # FT_slopes,
    EMP_slopes,
    by = c('INDUSTRY_NAME','GEOGRAPHY_NAME')
  ) %>% mutate(
    INDUSTRY_NAME_REDUCED = gsub(x = INDUSTRY_NAME, pattern = 'of |and |acture|acturing| activities| equipment| products', replacement = '')
  )


gvax$GEOGRAPHY_NAME[grepl(pattern = 'London', x = gvax$GEOGRAPHY_NAME, ignore.case = T)] %>% unique

place = 'South Yorkshire'
place = 'Greater Manchester'
place = 'West Yorkshire'
place = 'Inner London - West'

#OK, so. Quick plot of South Yorkshire sectors with sig slopes, both neg and pos
# gva_sy_sig <- gvax %>% 
#   filter(GEOGRAPHY_NAME == place, crosseszero == F) %>% 
#   mutate(positive_slope = slope > 0)
# 
# 
# #OK, well... those aren't the cheeriest numbers, but...
# ggplot(gva_sy_sig, aes(x = slope, y = fct_reorder(INDUSTRY_NAME,slope))) +
#   geom_point() +
#   geom_errorbar(aes(xmin = min95, xmax = max95)) +
#   geom_vline(xintercept = 0, colour = 'red', alpha = 0.5)


#OK, might be worth looking at all but greying out nonsig
gva_sy <- gvax %>% 
  filter(
    GEOGRAPHY_NAME == place,
    !is.na(slope)
    ) %>% 
  mutate(
    INDUSTRY_NAME_REDUCED = gsub(x = INDUSTRY_NAME, pattern = 'of |and |acture|acturing| activities| equipment| products', replacement = '')
  )

# gva_sy <- gva_sy
# gva_sy <- gva_sy %>% filter(INDUSTRY_NAME != 'Insurance and pension funding')#South Yorkshire, remove annoying
# gva_sy <- gva_sy %>% filter(!grepl(pattern = 'forestry|scientific|wearing apparel', x = INDUSTRY_NAME, ignore.case = T))#Greater Manchester, remove annoying
# gva_sy <- gva_sy %>% filter(!grepl(pattern = 'wearing apparel', x = INDUSTRY_NAME, ignore.case = T))#West Yorkshire, remove annoying

#https://stackoverflow.com/a/38862452
a <- ifelse(gva_sy$crosseszero95[order(gva_sy$slope)], "Grey", "Black")

#95% CIs
ggplot(gva_sy, 
       aes(x = slope, y = fct_reorder(INDUSTRY_NAME_REDUCED,slope), colour = crosseszero95, size = crosseszero95)) +
  geom_point(size = 2) +
  geom_errorbar(aes(xmin = min95, xmax = max95)) +
  geom_vline(xintercept = 0, colour = 'red', alpha = 0.5) +
  scale_size_manual(values = c(1,0.5)) +
  scale_colour_manual(values = c('firebrick4','gray30')) +
  theme(axis.text.y = element_text(colour = a))

b <- ifelse(gva_sy$crosseszero90[order(gva_sy$slope)], "Grey", "Black")

#90% CIs
#Facetting doesn't work for the axis text colouring
ggplot(gva_sy, 
       aes(x = slope *100, y = fct_reorder(INDUSTRY_NAME_REDUCED,slope), colour = crosseszero90, size = crosseszero90)) +
  geom_point(size = 2) +
  geom_errorbar(aes(xmin = min90*100, xmax = max90*100)) +
  geom_vline(xintercept = 0, colour = 'red', alpha = 0.5) +
  scale_size_manual(values = c(1,0.5)) +
  scale_colour_manual(values = c('firebrick4','gray30')) +
  theme(axis.text.y = element_text(colour = b)) 
  # facet_wrap(~slopepolarity, scales = 'free_y')
  



#Can we identify and count which sectors have many significant entries?
#SY could then be doing OK even if non-shrinking, relatively
#We want 'crosses zero' and the polarity combined, so we can count those
# gvax <- gvax %>% 
#   unite(crosses_zero90_n_polarity, c("crosseszero90", "slopepolarity"), remove = F) %>% 
#   unite(crosses_zero95_n_polarity, c("crosseszero95", "slopepolarity"), remove = F)


#Proportions of each
gvax_props <- gvax %>% 
  group_by(INDUSTRY_NAME) %>% 
  summarise(
    prop.sig90.dec = mean(slopepolarity=='decreasing' & !crosseszero90),
    prop.sig90.inc = mean(slopepolarity=='increasing' & !crosseszero90)
  )



#Mappy mappy
itl2.geo <- st_read('data/geographies/International_Territorial_Level_2_January_2021_UK_BFE_V2_2022_-4735199360818908762/ITL2_JAN_2021_UK_BFE_V2.shp', quiet = T) %>% 
  st_simplify(preserveTopology = T, dTolerance = 100)

#Oh good.
#table(itl2.geo$ITL221NM %in% gvax$GEOGRAPHY_NAME)

#Fix!
itl2.geo$ITL221NM[!itl2.geo$ITL221NM %in% gvax$GEOGRAPHY_NAME]
unique(gvax$GEOGRAPHY_NAME[!gvax$GEOGRAPHY_NAME %in% itl2.geo$ITL221NM])

#Note: no Northern Ireland because of BRES link, which is GB only
itl2.geo$ITL221NM[itl2.geo$ITL221NM == 'Northumberland, and Tyne and Wear'] <- 'Northumberland and Tyne and Wear'
itl2.geo$ITL221NM[itl2.geo$ITL221NM == 'West Wales and The Valleys'] <- 'West Wales'

#Join map data to a subset of the GVA data
map <- itl2.geo %>% 
  right_join(
    gvax %>% filter(
      # grepl(pattern = 'finance and insurance', x = INDUSTRY_NAME, ignore.case = T)
      # grepl(pattern = 'fabricated metal', x = INDUSTRY_NAME, ignore.case = T)
      grepl(pattern = 'basic metal', x = INDUSTRY_NAME, ignore.case = T)
      # grepl(pattern = 'electrical equipment', x = INDUSTRY_NAME, ignore.case = T)
      # grepl(pattern = 'scientific research', x = INDUSTRY_NAME, ignore.case = T)
      # grepl(pattern = 'printing and reprod', x = INDUSTRY_NAME, ignore.case = T)
      # grepl(pattern = 'wholesale trade', x = INDUSTRY_NAME, ignore.case = T)
    ),
    by = c('ITL221NM'='GEOGRAPHY_NAME')
  )


tm_shape(map) +
  tm_polygons('slope', n = 9) +
  tm_layout(title = '', legend.outside = T) +
tm_shape(
  map %>% filter(!crosseszero90)
  ) +
  tm_borders(col='blue', lwd = 3) +
  tm_view(bbox = c(left=-180, bottom=-60, right=180, top=85))





#Let's have a look at the lot
for(i in unique(gvax$INDUSTRY_NAME_REDUCED)){
  
  map <- itl2.geo %>% 
    right_join(
      gvax %>% filter(INDUSTRY_NAME_REDUCED == i),
      by = c('ITL221NM'='GEOGRAPHY_NAME')
    ) %>% 
    mutate(slope100 = slope * 100)

  m <- tm_shape(map) +
    tm_polygons('slope100', n = 9, title="") +
    tm_layout(title = i, legend.bg.color = 'white', legend.bg.alpha = 0.5) +
    # tm_layout(title = i, legend.outside = T) +
    tm_shape(
      map %>% filter(!crosseszero90)
    ) +
    tm_borders(col='blue', lwd = 3)
  
  # tmap_save(tm = m, filename = paste0('local/localimages/gva_perFTworker_trendmaps/',i,'.png'), width = 1100, dpi = 300, outer.margins = 0, asp=0)
  # tmap_save(tm = m, filename = paste0('local/localimages/gva_perFTworker_trendmaps/',i,'.png'), width = 1100, dpi = 300, asp = 0.45)
  tmap_save(tm = m, filename = paste0('local/localimages/gva_perFTworker_trendmaps/',i,'_emp.png'), width = 1500, dpi = 300)
  
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#USE GVA PER WORKER DATA TO LOOK AT JOB COUNTS AT THIS SECTOR LEVEL----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#To keep this clear, we can drop most of this and rerun proportions / LQ for jobs at this scale
#Useful for matching what we already have
#Note, imputed rental dropped because it doesn't contain any jobs...
gva_jobs5 <- readRDS('local/data/misc/gva_per_worker_from5digitSIC_BRESsums.rds')

#There was kind of an argument for doing that generally, but we'll leave that for now
#Have checked, doesn't make radical difference...
jobs <- gva_jobs5 %>% 
  select(-c(region_totalsize:LQ_log),-c(ratio:total_GB_GVA_peryear))

  
#Get proportion values based on job counts
#Actuallyu don't need to do this, do we? It gets calculated in the proportion plot
# jobs <- jobs %>% 
#   split(.$DATE) %>% 
#   map(add_location_quotient_and_proportions, 
#       regionvar = GEOGRAPHY_NAME,
#       lq_var = INDUSTRY_NAME,
#       valuevar = JOBCOUNT_FT) %>% 
#   bind_rows()




#Check what that looks like for the same periods and comparisons for GVA as the existing story
# debugonce(twod_proportionplot)
p <- twod_proportionplot(
  df = jobs,
  regionvar = GEOGRAPHY_NAME,
  category_var = INDUSTRY_NAME, 
  valuevar = JOBCOUNT_FT, 
  timevar = DATE, 
  # start_time = 2008,
  # end_time = 2021,
  start_time = 2015,
  end_time = 2021,
  compasspoints_to_display = c('NE','SE'),
  # compasspoints_to_display = c('NW','SW'),
  # compasspoints_to_display = c('NE','NW'),
  x_regionnames = 'South Yorkshire',
  y_regionnames = jobs$GEOGRAPHY_NAME[jobs$GEOGRAPHY_NAME!='South Yorkshire']
)

#add these after
p <- p + 
  xlab('South Yorkshire GVA proportions') +
  ylab('Rest of UK (minus SY) GVA proportions') +
  scale_y_log10() +
  scale_x_log10() +
  coord_fixed(xlim = c(0.1,11), ylim = c(0.1,11))# good for log scale

p





#SOUTH YORKSHIRE COMPARED TO REST OF NORTH, JOBS WISE
north_minus_sy <- jobs$GEOGRAPHY_NAME[grepl('Greater Manc|Merseyside|West Y|Cumbria|Cheshire|Lancashire|East Y|North Y|Tees|Northumb', jobs$GEOGRAPHY_NAME, ignore.case = T)] %>% unique

p <- twod_proportionplot(
  df = jobs,
  regionvar = GEOGRAPHY_NAME,
  category_var = INDUSTRY_NAME, 
  valuevar = JOBCOUNT_FT, 
  timevar = DATE, 
  # start_time = 2008,
  # end_time = 2021,
  start_time = 2015,
  end_time = 2021,
  # compasspoints_to_display = c('NE','SE'),
  # compasspoints_to_display = c('SE'),
  compasspoints_to_display = c('NE'),
  # compasspoints_to_display = c('NW','SW'),
  # compasspoints_to_display = c('NE','NW'),
  x_regionnames = 'South Yorkshire',
  y_regionnames = north_minus_sy
)

#add these after
p <- p + 
  xlab('South Yorkshire GVA proportions') +
  ylab('Rest of UK (minus SY) GVA proportions') +
  scale_y_log10() +
  scale_x_log10() +
  coord_fixed(xlim = c(0.1,11), ylim = c(0.1,11))# good for log scale

p



#Sectors...
#First up, add regional proportions. Want to compare those to job counts on y axis

jobs <- jobs %>% 
  split(.$DATE) %>% 
  map(
    add_location_quotient_and_proportions,
    regionvar = GEOGRAPHY_NAME, lq_var = INDUSTRY_NAME, valuevar = GVA    
      ) %>% bind_rows()
  

#Test that worked OK
jobs %>% filter(
  GEOGRAPHY_NAME == 'South Yorkshire',
  DATE == 2021
) %>% 
  mutate(regional_percent = sector_regional_proportion *100) %>% 
  select(INDUSTRY_NAME,regional_percent, LQ) %>% 
  arrange(-LQ) %>% 
  slice(1:20)



jobs <- jobs %>% 
group_by(INDUSTRY_NAME,GEOGRAPHY_NAME) %>% 
  arrange(DATE) %>% 
  mutate(
    gva_percent_movingav = zoo::rollapply(sector_regional_proportion *100,3,mean,align='center',fill=NA),
    jobs_movingav = zoo::rollapply(JOBCOUNT_FT,3,mean,align='center',fill=NA),
    gvaperjob_movingav = zoo::rollapply(GVA_perFTjob,3,mean,align='center',fill=NA)
  ) %>% ungroup()


jobs$DATE[!is.na(jobs$gvaperjob_movingav)] %>% unique

# debugonce(twod_generictimeplot)
twod_generictimeplot(
  df = jobs %>% filter(INDUSTRY_NAME=='Telecommunications') %>% mutate(`gva/job` = gvaperjob_movingav/1000),
  category_var = GEOGRAPHY_NAME,
  x_var = gva_percent_movingav,
  y_var = jobs_movingav,
  timevar = DATE,
  label_var = `gva/job`,
  start_time = 2016,
  end_time = 2020
)



twod_generictimeplot(
  df = jobs %>% filter(grepl(x = INDUSTRY_NAME, pattern = 'computer program', ignore.case=T)) %>% mutate(`gva/job` = gvaperjob_movingav/1000),
  category_var = GEOGRAPHY_NAME,
  x_var = gva_percent_movingav,
  y_var = jobs_movingav,
  timevar = DATE,
  label_var = `gva/job`,
  start_time = 2016,
  end_time = 2020
)

# p + 
#   scale_y_log10() +
#   scale_x_log10() 


#We can make LQ maps for those as well...
#Load ITL2 map data using the sf library
itl2.geo <- st_read('data/geographies/International_Territorial_Level_2_January_2021_UK_BFE_V2_2022_-4735199360818908762/ITL2_JAN_2021_UK_BFE_V2.shp', quiet = T) %>% st_simplify(preserveTopology = T, dTolerance = 100)

#unique(itl2.geo$ITL221NM)[order(unique(itl2.geo$ITL221NM))]

itl2.geo$ITL221NM[grepl("Northum",x = itl2.geo$ITL221NM, ignore.case = T)] <- "Northumberland and Tyne and Wear"
itl2.geo$ITL221NM[grepl("West Wales",x = itl2.geo$ITL221NM, ignore.case = T)] <- "West Wales"

sector = jobs$INDUSTRY_NAME[grepl(x = jobs$INDUSTRY_NAME, pattern = 'computer program', ignore.case=T)] %>% unique
sector = jobs$INDUSTRY_NAME[grepl(x = jobs$INDUSTRY_NAME, pattern = 'telecom', ignore.case=T)] %>% unique

#Join map data to a subset of the GVA data
sector_LQ_map <- itl2.geo %>% 
  right_join(
    jobs %>% filter(
      DATE==2021,
      INDUSTRY_NAME == sector
    ),
    by = c('ITL221NM'='GEOGRAPHY_NAME')
  )


#Plot map
tm_shape(sector_LQ_map) +
  tm_polygons('LQ_log', n = 9) +
  tm_layout(title = paste0('LQ spread of\n',sector,'\nAcross ITL2 regions'), legend.outside = T)
  



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CHAINED VOLUME GVA SLOPES FOR HIGHER LEVEL SECTORS (SECTIONS)----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Ideally with some forecasting just based on those slopes
#First, pull out sectors
itl2.cv <- read_csv('data/sectors/Table 2b ITL2 UK chained volume measures in 2019 money value pounds million.csv')

#no spaces plz!
names(itl2.cv) <- gsub(x = names(itl2.cv), pattern = ' ', replacement = '_')

#does it work just pulling out sectors with a single space in the second character slot?
#That looks like it should be single letter sections
#TICK!
cvSICkeeps <- itl2.cv$SIC07_code[substr(itl2.cv$SIC07_code,2,2) == ' '] %>% unique

itl2.cvs <- itl2.cv %>% 
  filter(SIC07_code %in% cvSICkeeps) %>% 
  pivot_longer(`1998`:`2021`, names_to = 'year', values_to = 'value') %>% 
  mutate(year = as.numeric(year))


#Save that for elsewhere
saveRDS(itl2.cvs, 'data/UKchainedvolume_itl2_SIC_sections.rds')

#Trend lines, log values
#Good for comparing and ranking % changes, but...
slopes.log <- get_slope_and_se_safely(data = itl2.cvs, ITL_region_name,SIC07_description, y = log(value), x = year)

#... also want these, so can project out forecasts from latest values
slopes <- get_slope_and_se_safely(data = itl2.cvs, ITL_region_name,SIC07_description, y = value, x = year)




#Pick a sector to plot separately for all places
#Use grepl as a shortcut to search for sector names

place = 'South Yorkshire'

sector <- itl2.cvs$SIC07_description[grepl('manuf', itl2.cvs$SIC07_description ,ignore.case = T)] %>% unique

timeplot <- itl2.cvs %>% 
  filter(SIC07_description == sector) 

#Use zoo's rollapply function to get a moving average
timeplot <- timeplot %>% 
  group_by(ITL_region_name) %>% 
  arrange(year) %>% 
  mutate(
    movingav = rollapply(value,3,mean,align='right',fill=NA)
    )

#Or pick top size values
#Largest % in 2021
# place_selection <- timeplot %>% 
#   filter(year == 2021) %>% 
#   arrange(-movingav)


#Use largest positive log slopes to filter
place_selection <- slopes.log %>% filter(SIC07_description == sector) %>%
  arrange(-slope)
  # arrange(slope)#to get the reverse

#Keep only the top ten places and order them
timeplot <- timeplot %>% 
  mutate(ITL_region_name = factor(ITL_region_name, ordered = T, levels = place_selection$ITL_region_name)) %>%
  filter(ITL_region_name %in% c(place_selection$ITL_region_name[1:10],place))#Make sure chosen place added

#Mark the ITL of interest so it can be clearer in the plot
timeplot <- timeplot %>%
  mutate(
    ITL2ofinterest = ifelse(ITL_region_name == place, 'ITL of interest','other'),
  )

ggplot(timeplot %>% 
         rename(`ITL region` = ITL_region_name) %>% 
         filter(!is.na(movingav)),#remove NAs from dates so the x axis doesn't show them
       aes(x = year, y = movingav, colour = `ITL region`, size = ITL2ofinterest, linetype = ITL2ofinterest, group = `ITL region`)) +
  geom_point() +
  geom_line() +
  scale_y_log10() +
  scale_size_manual(values = c(2.5,1)) +
  scale_color_brewer(palette = 'Paired', direction = 1) +
  ylab('Regional GVA chained volume measure') +
  guides(size = "none", linetype = "none") +
  ggtitle(
    paste0(sector,'\n', place, ' highlighted')
  ) +
  theme(plot.title = element_text(face = 'bold'))







#Output all sectors and see...
#NOTE, MAY BE EDITED TO OUTPUT RAW CHANGE NOT 3 YEAR SMOOTHED
for(growing in c(T,F)){

  for(sector in unique(itl2.cvs$SIC07_description)){
  
    timeplot <- itl2.cvs %>% 
      filter(SIC07_description == sector) 
    
    #Use zoo's rollapply function to get a moving average
    timeplot <- timeplot %>% 
      group_by(ITL_region_name) %>% 
      arrange(year) %>% 
      mutate(
        movingav = rollapply(value,3,mean,align='right',fill=NA)
      )
    
    #Or pick top size values
    #Largest % in 2021
    # place_selection <- timeplot %>% 
    #   filter(year == 2021) %>% 
    #   arrange(-movingav)
    
    
    #Use largest positive log slopes to filter
    if(growing){
      place_selection <- slopes.log %>% filter(SIC07_description == sector) %>%
        arrange(-slope)
    } else {
      place_selection <- slopes.log %>% filter(SIC07_description == sector) %>%
        arrange(slope)
      }
    
    #Keep only the top ten places and order them
    timeplot <- timeplot %>% 
      mutate(ITL_region_name = factor(ITL_region_name, ordered = T, levels = place_selection$ITL_region_name)) %>%
      filter(ITL_region_name %in% c(place_selection$ITL_region_name[1:10],place))#Make sure chosen place added
    
    #Mark the ITL of interest so it can be clearer in the plot
    timeplot <- timeplot %>%
      mutate(
        ITL2ofinterest = ifelse(ITL_region_name == place, 'ITL of interest','other'),
      )
    
    p <- ggplot(timeplot %>% 
             rename(`ITL region` = ITL_region_name) %>% 
             filter(!is.na(movingav)),#remove NAs from dates so the x axis doesn't show them
           aes(x = year, y = value, colour = `ITL region`, size = ITL2ofinterest, linetype = ITL2ofinterest, group = `ITL region`)) +
           # aes(x = year, y = movingav, colour = `ITL region`, size = ITL2ofinterest, linetype = ITL2ofinterest, group = `ITL region`)) +
      geom_point() +
      geom_line() +
      scale_y_log10() +
      scale_size_manual(values = c(2.5,1)) +
      scale_color_brewer(palette = 'Paired', direction = 1) +
      ylab('Regional GVA chained volume measure') +
      guides(size = "none", linetype = "none") +
      ggtitle(
        paste0(ifelse(growing,'HIGHEST GROWTH','LOWEST GROWTH'),'\n',sector,'\n', place, ' highlighted')
      ) +
      theme(plot.title = element_text(face = 'bold'))
  
    ggsave(plot = p, 
           filename = paste0('local/localimages/chainedvolumeITL2_sectors_unsmoothed/',sector,'_',ifelse(growing,'HIGHESTGROWTH','LOWESTGROWTH'),'_chainedvolumeITL2_sectors.png'), 
           # filename = paste0('local/localimages/chainedvolumeITL2_sectors/',sector,'_',ifelse(growing,'HIGHESTGROWTH','LOWESTGROWTH'),'_chainedvolumeITL2_sectors.png'), 
           # filename = paste0('local/localimages/chainedvolumeITL2_sectors/',ifelse(growing,'HIGHESTGROWTH','LOWESTGROWTH'),'_',sector,'_chainedvolumeITL2_sectors.png'), 
           width = 11, height = 7)
  
  
  }

}



#I'd like a map as well e.g. for Information and communication. All high growth, but SY clearly in the high growth group.
#What's the geography of that?
itl2.geo <- st_read('data/geographies/International_Territorial_Level_2_January_2021_UK_BFE_V2_2022_-4735199360818908762/ITL2_JAN_2021_UK_BFE_V2.shp', quiet = T) %>% st_simplify(preserveTopology = T, dTolerance = 100)

#Names match without alteration in this case, jolly good
table(unique(slopes.log$ITL_region_name) %in% itl2.geo$ITL221NM)



# slopes.log <- get_slope_and_se_safely(data = itl2.cvs %>% filter(year %in% 1998:2021), ITL_region_name,SIC07_description, y = log(value), x = year) %>% 
# slopes.log <- get_slope_and_se_safely(data = itl2.cvs %>% filter(year %in% 2013:2019), ITL_region_name,SIC07_description, y = log(value), x = year) %>% 
slopes.log <- get_slope_and_se_safely(data = itl2.cvs %>% filter(year %in% 2015:2021), ITL_region_name,SIC07_description, y = log(value), x = year) %>%
#Avoid covid
# slopes.log <- get_slope_and_se_safely(data = itl2.cvs %>% filter(year %in% 2013:2019), ITL_region_name,SIC07_description, y = log(value), x = year) %>% 
  mutate(
    min99 = slope - (se * getZScore(99)),
    max99 = slope + (se * getZScore(99)),
    min95 = slope - (se * getZScore(95)),
    max95 = slope + (se * getZScore(95)),
    min90 = slope - (se * getZScore(90)),
    max90 = slope + (se * getZScore(90)),
    crosseszero99 = min99 * max99 < 0,#mark if crosses zero
    crosseszero95 = min95 * max95 < 0,#mark if crosses zero
    crosseszero90 = min90 * max90 < 0,#mark if crosses zero
    slopepolarity = ifelse(slope > 0, 'increasing', 'decreasing')
  )



#Pick sector
sector = slopes.log$SIC07_description[grepl(pattern = 'manuf', x = slopes.log$SIC07_description, ignore.case = T)] %>% unique
sector = slopes.log$SIC07_description[grepl(pattern = 'information', x = slopes.log$SIC07_description, ignore.case = T)] %>% unique
sector = slopes.log$SIC07_description[grepl(pattern = 'financ', x = slopes.log$SIC07_description, ignore.case = T)] %>% unique

# View(slopes.log %>% filter(SIC07_description == sector))

map <- itl2.geo %>% 
  right_join(
    slopes.log %>% filter(SIC07_description == sector),
    by = c('ITL221NM'='ITL_region_name')
  ) %>% mutate(slope100 = slope * 100)

tm_shape(map) +
  tm_polygons('slope100', n = 7, title="") +
  tm_layout(title = sector, legend.bg.color = 'white', legend.bg.alpha = 0.5) +
  tm_shape(
    map %>% filter(!crosseszero95)
  ) +
  tm_borders(col='blue', lwd = 3)




#Find out what slopes are statistically separarable
#For e.g South Yorkshire: a full matrix grid of “which slopes are statistically separable”, pairing every sector against every other, to pick out any obvious patterns of growth difference (for maybe different time points). One plot per place.
#Can then do the same for individual sectors: pair each place off against every other for that sector, giving another matrix. Then that’s one plot per sector, if we want to do that for all.
#With variable factors being CI level 95% 90% etc…

#Using the log slopes, so scale of sector itself doesn't matter

#Will probably be good to function this up but let's do manually first

#log slope CIs
slopes.log <- slopes.log %>% 
  mutate(
    min.ci = slope - (se * 2.58),
    max.ci = slope + (se * 2.58)#99%
    # min.ci = slope - (se * 1.96),
    # max.ci = slope + (se * 1.96)#95%
  )

#Thing to copy from sicsoc. This takes in two places and compares. We want to repeat for all pairs of sectors in this case
# sy.ww <- sy.w %>%
#   select(GEOGRAPHY_NAME,SIC2007,SOC2020,min_ci,max_ci) %>% 
#   pivot_wider(
#     names_from = GEOGRAPHY_NAME, values_from = c(min_ci,max_ci),
#     values_fn = mean
#   ) %>% 
#   mutate(CIs_overlap = ifelse(
#     (.[,3] <= .[,6] & .[,5] <= .[,4]) |
#       (.[,4] <= .[,5] & .[,6] <= .[,3])  , 
#     F,T))

place = 'South Yorkshire'
place = 'Greater Manchester'

#Just for single place...
slopes.log.1place <- slopes.log %>% 
  filter(ITL_region_name == place)

#All combinations of sectors
combos <- expand.grid(SIC07_description1 = slopes.log.1place$SIC07_description, SIC07_description2 = slopes.log.1place$SIC07_description)
#Remove dups
#Actually, no, don't - we want to be able to show the slope value, if not overlapping, for every sector in the grid
# combos <- combos[!duplicated(t(apply(combos, 1, sort))), ]


#Merge in two repeated sets of the values and CIs to check for CI overlap for each pair
combos <- combos %>% 
  left_join(
    slopes.log.1place %>% ungroup() %>% select(
      SIC07_description,
      slopeone = slope,
      min.cione = min.ci,
      max.cione = max.ci
    ),
    by = c('SIC07_description1' = 'SIC07_description')
  )

#Merge in two repeated sets of the values and CIs to check for CI overlap for each pair
combos <- combos %>% 
  left_join(
    slopes.log.1place %>% ungroup() %>% select(
      SIC07_description,
      slopetwo = slope,
      min.citwo = min.ci,
      max.citwo = max.ci
    ),
    by = c('SIC07_description2' = 'SIC07_description')
  )


#Apply CI overlap test to all pairs
#https://stackoverflow.com/a/3269471
#If (StartA <= EndB) and (EndA >= StartB) 
#This is the order in the comparative df
#3. "min_ci_Greater Manchester"
#4. "min_ci_South Yorkshire"
#5. "max_ci_Greater Manchester"
#6. "max_ci_South Yorkshire"   
#Equivs are 4,7,5,8
combos <- combos %>% 
  mutate(CIs_overlap = ifelse(
        (.[,4] <= .[,8] & .[,5] <= .[,7]) |
          (.[,7] <= .[,5] & .[,8] <= .[,4])  ,
        F,T)) %>% 
  mutate(
    slopediff = slopetwo - slopeone,#Add in slope differences
    slopediff.sig = ifelse(CIs_overlap, F,T)#add in version with NAs for not sig
    )




zerocutoff = 0.5


#Using colour for grid outline for sig values doesn't quite work, it draws messily
#Add as extra layer over the top instead
ggplot(combos, aes(x = substr(SIC07_description1,0,30), y = substr(SIC07_description2,0,30), fill= slopediff)) + 
  geom_tile() +
  scale_fill_gradientn(
    colours = c("red", "white", "darkgreen"),
    values = c(0, zerocutoff, 1)#https://stackoverflow.com/a/58725778/5023561
  ) +
  theme(
    axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=0),
    axis.title.x=element_blank(),
    axis.title.y=element_blank()
    ) +
  # geom_tile(data = combos %>% filter(slopediff.sig), colour = 'white', size = 2) 
  geom_tile(data = combos %>% filter(slopediff.sig), colour = 'black', size = 1)
  


#Sample plot comparing two slopes to show sig diff
#manuf vs mining quarrying
sampleplot <- itl2.cvs %>%
  filter(
    ITL_region_name == 'South Yorkshire',
    # SIC07_description %in% c('Manufacturing','Mining and quarrying')
    SIC07_description %in% c('Manufacturing','Construction')
  )

sampleplot <- itl2.cvs %>%
  filter(
    ITL_region_name == 'South Yorkshire'
    # SIC07_description %in% c('Manufacturing','Mining and quarrying')
    # SIC07_description %in% c('Manufacturing','Construction')
  )

#default level is 0.95
ggplot(sampleplot, aes(x=year,y=value,colour=SIC07_description )) +
  geom_line(size=2) +
  # scale_y_log10() +
  geom_smooth(method='lm') 
  # geom_smooth(method='lm', linetype=0) 

# #Sample of two that are not separable
# sampleplot <- itl2.cvs %>%
#   filter(
#     ITL_region_name == 'South Yorkshire',
#     SIC07_description %in% c('Human health and social work activities','Education')
#   )
# 
# #default level is 0.95
# ggplot(sampleplot, aes(x=year,y=value,colour=SIC07_description )) +
#   geom_line(size=2) +
#   # scale_y_log10() +
#   geom_smooth(method='lm') 


#Pick a different data range
slopes.log <- get_slope_and_se_safely(data = itl2.cvs, ITL_region_name,SIC07_description, y = log(value), x = year)
slopes.log <- get_slope_and_se_safely(data = itl2.cvs %>% filter(year %in% 2015:2021), ITL_region_name,SIC07_description, y = log(value), x = year)
#Avoid covid
slopes.log <- get_slope_and_se_safely(data = itl2.cvs %>% filter(year %in% 2013:2019), ITL_region_name,SIC07_description, y = log(value), x = year)


#Functioning up
# debugonce(slopeDiffGrid)
slopeDiffGrid(slope_df = slopes.log, confidence_interval = 95, column_to_grid = SIC07_description, column_to_filter = ITL_region_name, filterval = 'South Yorkshire')

slopeDiffGrid(slope_df = slopes.log, confidence_interval = 99, column_to_grid = SIC07_description, column_to_filter = ITL_region_name, filterval = 'Merseyside')

slopeDiffGrid(slope_df = slopes.log, confidence_interval = 95, column_to_grid = ITL_region_name, column_to_filter = SIC07_description, filterval = 'Manufacturing')

slopeDiffGrid(slope_df = slopes.log, confidence_interval = 99, column_to_grid = ITL_region_name, column_to_filter = SIC07_description, filterval = 'Mining and quarrying')

slopeDiffGrid(slope_df = slopes.log, confidence_interval = 99, column_to_grid = ITL_region_name, column_to_filter = SIC07_description, filterval = 'Information and communication')

slopeDiffGrid(slope_df = slopes.log, confidence_interval = 99, column_to_grid = ITL_region_name, column_to_filter = SIC07_description, filterval = 'Other service activities')

slopeDiffGrid(slope_df = slopes.log, confidence_interval = 99, column_to_grid = ITL_region_name, column_to_filter = SIC07_description, filterval = 'Accommodation and food service activities')


#Two places against each other
#debugonce(slopeDiffGrid)
slopeDiffGrid(slope_df = slopes.log, confidence_interval = 95, column_to_grid = SIC07_description, column_to_filter = ITL_region_name, filterval = 'South Yorkshire', filterval2 = 'Greater Manchester')
slopeDiffGrid(slope_df = slopes.log, confidence_interval = 95, column_to_grid = SIC07_description, column_to_filter = ITL_region_name, filterval2 = 'South Yorkshire', filterval = 'Greater Manchester')
slopeDiffGrid(slope_df = slopes.log, confidence_interval = 95, column_to_grid = SIC07_description, column_to_filter = ITL_region_name, filterval2 = 'South Yorkshire', filterval = 'West Yorkshire')




#random check: 2015 to 2021 ICT, is the slope 18.7% per year for SY?
chk <- itl2.cvs %>% 
  filter(
    SIC07_description == 'Information and communication',
    ITL_region_name == 'South Yorkshire',
    year %in% 2015:2021
  )

#Yup, is about right
((chk$value - lag(chk$value))/lag(chk$value))*100
mean(((chk$value - lag(chk$value))/lag(chk$value))*100, na.rm=T)

ggplot(chk, aes(x = year, y = value)) +
  geom_point() +
  geom_smooth(method = lm)

#OK, let's output all the sectors and see see
daterange = c(2013:2019)
daterange = c(2015:2021)
slopes.log <- get_slope_and_se_safely(data = itl2.cvs %>% filter(year %in% daterange), ITL_region_name,SIC07_description, y = log(value), x = year)

for(sector in unique(slopes.log$SIC07_description)){
  
  p <- slopeDiffGrid(slope_df = slopes.log, confidence_interval = 95, column_to_grid = ITL_region_name, column_to_filter = SIC07_description, filterval = sector)
  
  ggsave(plot = p, filename = paste0('local/localimages/sector_slope_grids/',sector,'_',min(daterange),'_',max(daterange),'.png'), width = 13, height = 13)
  
}


#Repeat for all places
# for(place in unique(slopes.log$ITL_region_name)){
#   
#   p <- slopeDiffGrid(slope_df = slopes.log, confidence_interval = 95, column_to_grid = SIC07_description, column_to_filter = ITL_region_name, filterval = place)
#   
#   ggsave(plot = p, filename = paste0('local/localimages/place_slope_grids/',place,'.png'), width = 13, height = 13)
#   
# }


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#GVA PER WORKER, CHAINED VOLUME HIGHER LEVEL SECTORS----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

itl2.jobs <- readRDS('data/itl2_BRES_jobs_SIC_sections.rds')

itl2.cvs <- readRDS('data/UKchainedvolume_itl2_SIC_sections.rds')


#Couple of places names to fix before a merge
unique(itl2.jobs$GEOGRAPHY_NAME)[!unique(itl2.jobs$GEOGRAPHY_NAME) %in% unique(itl2.cvs$ITL_region_name)]

itl2.cvs.tweakednames <- itl2.cvs

itl2.cvs.tweakednames$ITL_region_name[grepl("Northum",x = itl2.cvs.tweakednames$ITL_region_name, ignore.case = T)] <- "Northumberland and Tyne and Wear"
itl2.cvs.tweakednames$ITL_region_name[grepl("West Wales and The Valleys",x = itl2.cvs.tweakednames$ITL_region_name, ignore.case = T)] <- "West Wales"

#Right join to match the fewer available years in the BRES data
itl2.gvaperjob <- itl2.cvs.tweakednames %>% 
  rename(gva = value) %>% 
  right_join(
    itl2.jobs %>% select(-SIC_SECTION_NAME) %>% rename(jobcount = COUNT),
    by = c('year' = 'DATE','ITL_region_name' = 'GEOGRAPHY_NAME','SIC07_code' = 'SIC_SECTION_CODE')
  ) %>% 
  mutate(gvaperjob = (gva/jobcount) * 1000000)

#We have a sector non-match, which one?
unique(itl2.jobs$SIC_SECTION_NAME)[!unique(itl2.jobs$SIC_SECTION_NAME) %in% unique(itl2.gvaperjob$SIC07_description)]
unique(itl2.cvs$SIC07_description)[!unique(itl2.cvs$SIC07_description) %in% unique(itl2.gvaperjob$SIC07_description)]#It's activities of households - those aren't jobs, so won't appear. OK then.
unique(itl2.gvaperjob$SIC07_description)[!unique(itl2.gvaperjob$SIC07_description) %in% unique(itl2.jobs$SIC_SECTION_NAME)]






perjobslopes.log <- get_slope_and_se_safely(data = itl2.gvaperjob, ITL_region_name,SIC07_description, y = log(gvaperjob), x = year)


#Output all sectors and see...
#NOTE, MAY BE EDITED TO OUTPUT RAW CHANGE NOT 3 YEAR SMOOTHED
place = 'South Yorkshire'

for(growing in c(T,F)){
  
  for(sector in unique(itl2.gvaperjob$SIC07_description)){
    
    timeplot <- itl2.gvaperjob %>% 
      filter(SIC07_description == sector) 
    
    #Use zoo's rollapply function to get a moving average
    timeplot <- timeplot %>% 
      group_by(ITL_region_name) %>% 
      arrange(year) %>% 
      mutate(
        movingav = rollapply(gvaperjob,3,mean,align='right',fill=NA)
      )
    
    
    #Use largest positive log slopes to filter
    if(growing){
      place_selection <- slopes.log %>% filter(SIC07_description == sector) %>%
        arrange(-slope)
    } else {
      place_selection <- slopes.log %>% filter(SIC07_description == sector) %>%
        arrange(slope)
    }
    
    #Keep only the top ten places and order them
    timeplot <- timeplot %>% 
      mutate(ITL_region_name = factor(ITL_region_name, ordered = T, levels = place_selection$ITL_region_name)) %>%
      filter(ITL_region_name %in% c(place_selection$ITL_region_name[1:10],place))#Make sure chosen place added
    
    #Mark the ITL of interest so it can be clearer in the plot
    timeplot <- timeplot %>%
      mutate(
        ITL2ofinterest = ifelse(ITL_region_name == place, 'ITL of interest','other'),
      )
    
    p <- ggplot(timeplot %>% 
                  rename(`ITL region` = ITL_region_name),
                aes(x = year, y = gvaperjob, colour = `ITL region`, size = ITL2ofinterest, linetype = ITL2ofinterest, group = `ITL region`)) +
      # aes(x = year, y = movingav, colour = `ITL region`, size = ITL2ofinterest, linetype = ITL2ofinterest, group = `ITL region`)) +
      geom_point() +
      geom_line() +
      scale_y_log10() +
      scale_size_manual(values = c(2.5,1)) +
      scale_color_brewer(palette = 'Paired', direction = 1) +
      ylab('GVA per job, pounds') +
      guides(size = "none", linetype = "none") +
      ggtitle(
        paste0(ifelse(growing,'HIGHEST GROWTH','LOWEST GROWTH'),'\n',sector,'\n', place, ' highlighted')
      ) +
      theme(plot.title = element_text(face = 'bold'))
    
    ggsave(plot = p, 
           filename = paste0('local/localimages/chainedvolumeITL2_gvaperjob/',sector,'_',ifelse(growing,'HIGHESTGROWTH','LOWESTGROWTH'),'_chainedvolumeITL2_sectors.png'), 
           # filename = paste0('local/localimages/chainedvolumeITL2_sectors/',sector,'_',ifelse(growing,'HIGHESTGROWTH','LOWESTGROWTH'),'_chainedvolumeITL2_sectors.png'), 
           # filename = paste0('local/localimages/chainedvolumeITL2_sectors/',ifelse(growing,'HIGHESTGROWTH','LOWESTGROWTH'),'_',sector,'_chainedvolumeITL2_sectors.png'), 
           width = 11, height = 7)
    
    
  }
  
}






#Slopes and stuff
perjobslopes.log <- get_slope_and_se_safely(data = itl2.gvaperjob, ITL_region_name,SIC07_description, y = log(gvaperjob), x = year)
perjobslopes.log <- get_slope_and_se_safely(data = itl2.gvaperjob %>% filter(year %in% 2017:2021), ITL_region_name,SIC07_description, y = log(gvaperjob), x = year)
perjobslopes.log <- get_slope_and_se_safely(data = itl2.gvaperjob %>% filter(year %in% 2015:2019), ITL_region_name,SIC07_description, y = log(gvaperjob), x = year)
perjobslopes.log <- get_slope_and_se_safely(data = itl2.gvaperjob %>% filter(year %in% 2015:2021), ITL_region_name,SIC07_description, y = log(gvaperjob), x = year)

#Functioning up
# debugonce(slopeDiffGrid)
slopeDiffGrid(slope_df = perjobslopes.log, confidence_interval = 95, column_to_grid = SIC07_description, column_to_filter = ITL_region_name, filterval = 'South Yorkshire')
slopeDiffGrid(slope_df = perjobslopes.log, confidence_interval = 95, column_to_grid = ITL_region_name, column_to_filter = SIC07_description, filterval = 'Manufacturing')
slopeDiffGrid(slope_df = perjobslopes.log, confidence_interval = 95, column_to_grid = ITL_region_name, column_to_filter = SIC07_description, filterval = 'Information and communication')


# daterange = c(2013:2019)
daterange = c(2015:2021)
daterange = c(2017:2021)
perjobslopes.log <- get_slope_and_se_safely(data = itl2.gvaperjob %>% filter(year %in% daterange), ITL_region_name, SIC07_description, y = log(gvaperjob), x = year)


for(sector in unique(perjobslopes.log$SIC07_description)){
  
  p <- slopeDiffGrid(slope_df = perjobslopes.log, confidence_interval = 95, column_to_grid = ITL_region_name, column_to_filter = SIC07_description, filterval = sector)
  
  ggsave(plot = p, filename = paste0('local/localimages/GVAPERJOB_sectorgrids_1721/',sector,'_',min(daterange),'_',max(daterange),'.png'), width = 13, height = 13)
  
}


#Maaaaap
sector = perjobslopes.log$SIC07_description[grepl(pattern = 'elec', x = perjobslopes.log$SIC07_description, ignore.case = T)] %>% unique

# View(slopes.log %>% filter(SIC07_description == sector))

daterange = c(2015:2021)
perjobslopes.log <- get_slope_and_se_safely(data = itl2.gvaperjob %>% filter(year %in% daterange), ITL_region_name, SIC07_description, y = log(gvaperjob), x = year) %>%
  mutate(
    min99 = slope - (se * getZScore(99)),
    max99 = slope + (se * getZScore(99)),
    min95 = slope - (se * getZScore(95)),
    max95 = slope + (se * getZScore(95)),
    min90 = slope - (se * getZScore(90)),
    max90 = slope + (se * getZScore(90)),
    crosseszero99 = min99 * max99 < 0,#mark if crosses zero
    crosseszero95 = min95 * max95 < 0,#mark if crosses zero
    crosseszero90 = min90 * max90 < 0,#mark if crosses zero
    slopepolarity = ifelse(slope > 0, 'increasing', 'decreasing')
  )


map <- itl2.geo %>% 
  right_join(
    perjobslopes.log %>% filter(SIC07_description == sector),
    by = c('ITL221NM'='ITL_region_name')
  ) %>% mutate(slope100 = slope * 100)

tm_shape(map) +
  tm_polygons('slope100', n = 7, title="") +
  tm_layout(title = sector, legend.bg.color = 'white', legend.bg.alpha = 0.5) +
  tm_shape(
    map %>% filter(!crosseszero95)
  ) +
  tm_borders(col='blue', lwd = 3)



#TABLE OF JOB COUNT / GVA / GVA PER WORKER CHANGE FROM START TO END OF DATA
#Noting this doesn't include and error rates, but helps give a sense of the different scales of sectors

#Actually, let's just do 2021
job_gva_scale <- itl2.gvaperjob %>% 
  filter(year == 2021, ITL_region_name=='South Yorkshire',SIC07_description!='Real estate activities') %>% 
  arrange(-gva)


job_gva_scale_long <- itl2.gvaperjob %>% 
  filter(year == 2021, ITL_region_name=='South Yorkshire',SIC07_description!='Real estate activities') %>% 
  select(SIC07_description,gva,jobcount,gvaperjob) %>% 
  pivot_longer(cols = gva:gvaperjob, names_to = 'type', values_to = 'value')

job_gva_scale$SIC07_description <- factor(job_gva_scale$SIC07_description, levels = job_gva_scale[order(-job_gva_scale$gva), "SIC07_description"] %>% pull)

ggplot(job_gva_scale, aes(x = SIC07_description, y = y, fill = value)) +
  geom_tile() +
  geom_text(aes(label = value), color = "black") +
  scale_fill_gradient(low = "blue", high = "red") +
  # theme_minimal() +
  labs(fill = "Value")





#TESTING 2D GENERIC TIMEPLOT FOR GVA VS JOB COUNT
# debugonce(twod_generictimeplot)
p <- twod_generictimeplot(
  # df = itl2.gvaperjob %>% filter(ITL_region_name == 'Greater Manchester', SIC07_description!='Real estate activities') %>% mutate(`gva/job` = gvaperjob/1000), 
  # df = itl2.gvaperjob %>% filter(ITL_region_name == 'Northumberland and Tyne and Wear', SIC07_description!='Real estate activities') %>% mutate(`gva/job` = gvaperjob/1000), 
  df = itl2.gvaperjob %>% filter(ITL_region_name == 'South Yorkshire', SIC07_description!='Real estate activities') %>% mutate(`gva/job` = gvaperjob/1000),
  category_var = SIC07_description,
  x_var = gva,
  y_var = jobcount,
  timevar = year,
  label_var = `gva/job`,
  start_time = 2015,
  end_time = 2021
)

p + theme(aspect.ratio=1)
p + theme(aspect.ratio=1) + scale_y_log10() + scale_x_log10() + coord_cartesian(xlim = c(1000,10000), ylim = c(20000,60000))



p <- twod_generictimeplot(
  # df = itl2.gvaperjob %>% filter(SIC07_description=='Information and communication') %>% mutate(`gva/job` = gvaperjob/1000), 
  df = itl2.gvaperjob %>% filter(SIC07_description=='Manufacturing') %>% mutate(`gva/job` = gvaperjob/1000),
  # df = itl2.gvaperjob %>% filter(grepl('Health',SIC07_description,ignore.case=T)) %>% mutate(`gva/job` = gvaperjob/1000),
  # df = itl2.gvaperjob %>% filter(ITL_region_name == 'South Yorkshire', SIC07_description!='Real estate activities') %>% mutate(`gva/job` = gvaperjob/1000), 
  category_var = ITL_region_name,
  x_var = gva,
  y_var = jobcount,
  timevar = year,
  label_var = `gva/job`,
  start_time = 2015,
  end_time = 2021
)

p + theme(aspect.ratio=1)

p + theme(aspect.ratio=1) + scale_y_log10() + scale_x_log10() 


#OK, wanna see all of everything please, they're fascinating

#PLACES
for(loggit in c(T,F)){
  
  for(place in unique(itl2.gvaperjob$ITL_region_name)){
    
    cat(place,'\n')
    
    p <- twod_generictimeplot(
      df = itl2.gvaperjob %>% filter(ITL_region_name == place, SIC07_description!='Real estate activities') %>% mutate(`gva/job` = gvaperjob/1000), 
      category_var = SIC07_description,
      x_var = gva,
      y_var = jobcount,
      timevar = year,
      label_var = `gva/job`,
      start_time = 2015,
      end_time = 2021
    )
    
    if(loggit) p <- p + theme(aspect.ratio=1) + scale_y_log10() + scale_x_log10() else p <- p + theme(aspect.ratio=1)
    
    ggsave(plot = p, filename = paste0('local/localimages/2D_GVAPERWORKER_PLACES/',gsub("[^A-Za-z]", "", place),'_',ifelse(loggit, 'LOG',''),'.png'), width = 10, height = 10)
    
  }
  
}


#SECTORS
for(loggit in c(T,F)){
  
  for(sector in unique(itl2.gvaperjob$SIC07_description)){
    
    cat(sector,'\n')
    
    p <- twod_generictimeplot(
      df = itl2.gvaperjob %>% filter(SIC07_description==sector) %>% mutate(`gva/job` = gvaperjob/1000),
      category_var = ITL_region_name,
      x_var = gva,
      y_var = jobcount,
      timevar = year,
      label_var = `gva/job`,
      start_time = 2015,
      end_time = 2021
    )
    
    if(loggit) p <- p + theme(aspect.ratio=1) + scale_y_log10() + scale_x_log10() else p <- p + theme(aspect.ratio=1)
    
    ggsave(plot = p, filename = paste0('local/localimages/2D_GVA_PERWORKER_SECTORS//',gsub("[^A-Za-z]", "", sector),'_',ifelse(loggit, 'LOG',''),'.png'), width = 10, height = 10)
    
  }
  
}



#TEST VERSION OF 2D PLOT THAT NORMALISES ALL VECTORS TO ZERO AND SCALES BY % CHANGE BETWEEN TIMEPOINTS
#Getting a list this time, including produced data, so we can expand the range of the plot for the labels
debugonce(twod_generictimeplot_normalisetozero)
p <- twod_generictimeplot_normalisetozero(
  # df = itl2.gvaperjob %>% filter(SIC07_description=='Information and communication') %>% mutate(`gva/job` = gvaperjob/1000),
  df = itl2.gvaperjob %>% filter(SIC07_description=='Manufacturing') %>% mutate(`gva/job` = gvaperjob/1000),
  # df = itl2.gvaperjob %>% filter(grepl('Health',SIC07_description,ignore.case=T)) %>% mutate(`gva/job` = gvaperjob/1000),
  # df = itl2.gvaperjob %>% filter(ITL_region_name == 'South Yorkshire', SIC07_description!='Real estate activities') %>% mutate(`gva/job` = gvaperjob/1000), 
  category_var = ITL_region_name,
  x_var = gva,
  y_var = jobcount,
  timevar = year,
  label_var = `gva/job`,
  category_var_value_to_highlight = 'South Yorkshire',
  start_time = 2015,
  end_time = 2021
)


xrange_adjust = diff(range(p[[2]]$x_pct_change)) * 0.1
yrange_adjust = diff(range(p[[2]]$y_pct_change)) * 0.1

p[[1]] + coord_fixed(
  xlim = c(
    min(p[[2]]$x_pct_change) - xrange_adjust,
    ifelse(max(p[[2]]$x_pct_change) > 0,max(p[[2]]$x_pct_change) + xrange_adjust,0)#hack for health, need to make generic
  ),
  ylim = c(
    min(p[[2]]$y_pct_change) - yrange_adjust,max(p[[2]]$y_pct_change) + yrange_adjust 
  )
) 


#All plz!
for(sector in unique(itl2.gvaperjob$SIC07_description)){
  
  p <- twod_generictimeplot_normalisetozero(
    df = itl2.gvaperjob %>% filter(SIC07_description==sector) %>% mutate(`gva/job` = gvaperjob/1000),
    category_var = ITL_region_name,
    x_var = gva,
    y_var = jobcount,
    timevar = year,
    label_var = `gva/job`,
    category_var_value_to_highlight = 'South Yorkshire',
    start_time = 2015,
    # end_time = 2021
    end_time = 2019
  )
  
  # p <- p[[1]] + coord_fixed()
  
  xrange_adjust = diff(range(p[[2]]$x_pct_change)) * 0.1
  yrange_adjust = diff(range(p[[2]]$y_pct_change)) * 0.1

  p <- p[[1]] + coord_fixed(
    xlim = c(
      min(p[[2]]$x_pct_change) - xrange_adjust,
      ifelse(max(p[[2]]$x_pct_change) > 0,max(p[[2]]$x_pct_change) + xrange_adjust,0)#hack for health, need to make generic
    ),
    ylim = c(
      min(p[[2]]$y_pct_change) - yrange_adjust,max(p[[2]]$y_pct_change) + yrange_adjust 
    )
  ) 
  
  # ggsave(plot = p, filename = paste0('local/localimages/2D_COMPASSPLOTS_SECTORS/',gsub("[^A-Za-z]", "", sector),'.png'), width = 12, height = 12)
  ggsave(plot = p, filename = paste0('local/localimages/2D_COMPASSPLOTS_SECTORS_to2019/',gsub("[^A-Za-z]", "", sector),'.png'), width = 12, height = 12)
  # ggsave(plot = p, filename = paste0('local/localimages/2D_COMPASSPLOTS_SECTORS_2019to2021/',gsub("[^A-Za-z]", "", sector),'.png'), width = 12, height = 12)
  
}




#PLACES
for(place in unique(itl2.gvaperjob$ITL_region_name)){
  
  p <- twod_generictimeplot_normalisetozero(
    df = itl2.gvaperjob %>% filter(ITL_region_name==place) %>% mutate(`gva/job` = gvaperjob/1000),
    category_var = SIC07_description,
    x_var = gva,
    y_var = jobcount,
    timevar = year,
    label_var = `gva/job`,
    start_time = 2015,
    end_time = 2021
  )
  
  # p <- p[[1]] + coord_fixed()
  
  xrange_adjust = diff(range(p[[2]]$x_pct_change)) * 0.1
  yrange_adjust = diff(range(p[[2]]$y_pct_change)) * 0.1

  p <- p[[1]] + coord_fixed(
    xlim = c(
      min(p[[2]]$x_pct_change) - xrange_adjust,
      ifelse(max(p[[2]]$x_pct_change) > 0,max(p[[2]]$x_pct_change) + xrange_adjust,0)#hack for health, need to make generic
    ),
    ylim = c(
      min(p[[2]]$y_pct_change) - yrange_adjust,max(p[[2]]$y_pct_change) + yrange_adjust 
    )
  ) 
  
  ggsave(plot = p, filename = paste0('local/localimages/2D_COMPASSPLOTS_PLACES/',gsub("[^A-Za-z]", "", place),'.png'), width = 12, height = 12)
  
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#MAKE 2D PLOT WITH MULTIPLE TIME POINTS: GVA PER WORKER TEST----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Make a version that can show multiple defined time points
#Most greyed out
# debugonce(twod_generictimeplot_multipletimepoints)
p <- twod_generictimeplot_multipletimepoints(
  # df = itl2.gvaperjob %>% filter(ITL_region_name == 'Greater Manchester', SIC07_description!='Real estate activities') %>% mutate(`gva/job` = gvaperjob/1000),
  df = itl2.gvaperjob %>% filter(ITL_region_name == 'South Yorkshire', SIC07_description!='Real estate activities') %>% mutate(`gva/job` = gvaperjob/1000),
  category_var = SIC07_description,
  x_var = gva,
  y_var = jobcount,
  timevar = year,
  label_var = `gva/job`,
  times = c(2017,2019,2021)
  # times = c(2017:2021)
  # times = c(2015,2019)
  # times = c(2015,2017,2019)
  # times = c(2015:2019)
)

p + theme(aspect.ratio=1)
p + theme(aspect.ratio=1) + scale_y_log10() + scale_x_log10()


#Smoothed version
smoothband = 3
itl2.gvaperjob.smoothed <- itl2.gvaperjob %>% 
  group_by(ITL_region_name,SIC07_description) %>% 
  arrange(year) %>% 
mutate(
  jobcount_movingav = rollapply(jobcount,smoothband,mean,align='center',fill=NA),
  gva_movingav = rollapply(gva,smoothband,mean,align='center',fill=NA),
  `gva/job_movingav` = rollapply(gvaperjob/1000,smoothband,mean,align='center',fill=NA)
)

#Check available years
itl2.gvaperjob.smoothed$year[!is.na(itl2.gvaperjob$jobcount_movingav)] %>% unique
itl2.gvaperjob.smoothed$year[!is.na(itl2.gvaperjob$gva_movingav)] %>% unique




p <- twod_generictimeplot_multipletimepoints(
  df = itl2.gvaperjob.smoothed %>% filter(ITL_region_name == 'South Yorkshire', SIC07_description!='Real estate activities'),
  # df = itl2.gvaperjob.smoothed %>% filter(ITL_region_name == 'Greater Manchester', SIC07_description!='Real estate activities'),
  # df = itl2.gvaperjob.smoothed %>% filter(!is.na(gva_movingav),ITL_region_name == 'South Yorkshire', SIC07_description!='Real estate activities'),
  category_var = SIC07_description,
  x_var = gva_movingav,
  y_var = jobcount_movingav,
  timevar = year,
  label_var = `gva/job_movingav`,
  times = c(2016:2020)
)

p + theme(aspect.ratio=1)



#There's a non-match on some of the places in this data - usual suspects, wales and Northumberland.
#That's breaking some of the smoothing
#Until then, let's just stick to SY for this one.
sy <- itl2.gvaperjob.smoothed %>% filter(ITL_region_name == 'South Yorkshire')

#For the smoothed itl2.gvaperjob data...
#What was the raw vs percent change in each sector's GVA and jobs, first to last?
#To compare to showing manuf and ICT changing similarly

#Get first and last years
yz <- sy %>% 
  filter(
    year %in% range(unique(sy$year[!is.na(sy$jobcount_movingav)]))
    )

#Better!
yz$year[!is.na(yz$gva_movingav)] %>% unique


diffz <- yz %>% 
  arrange(year) %>% 
  group_by(SIC07_description) %>% 
  mutate(
    gva_rawdiff = gva_movingav - lag(gva_movingav),
    gva_percentdiff = ((gva_movingav - lag(gva_movingav))/lag(gva_movingav))*100,
    jobs_rawdiff = jobcount_movingav - lag(jobcount_movingav),
    jobs_percentdiff = ((jobcount_movingav - lag(jobcount_movingav))/lag(jobcount_movingav))*100,
    gvaperjob_rawdiff = `gva/job_movingav` - lag(`gva/job_movingav`),
    gvaperjob_percentdiff = ((`gva/job_movingav` - lag(`gva/job_movingav`))/lag(`gva/job_movingav`))*100
  ) %>% 
  filter(!is.na(gva_rawdiff))










#Repeating same for percentaging centrepoint plot
#NOPE, WAAAAY TOO MESSY
#Also trying with smoothed...
debugonce(twod_generictimeplot_normalisetozero__multipletimepoints)
p <- twod_generictimeplot_normalisetozero__multipletimepoints(
  # df = itl2.gvaperjob %>% filter(SIC07_description=='Information and communication') %>% mutate(`gva/job` = gvaperjob/1000),
  df = itl2.gvaperjob %>% filter(SIC07_description=='Manufacturing') %>% mutate(`gva/job` = gvaperjob/1000),
  # df = itl2.gvaperjob %>% filter(grepl('Health',SIC07_description,ignore.case=T)) %>% mutate(`gva/job` = gvaperjob/1000),
  # df = itl2.gvaperjob %>% filter(ITL_region_name == 'South Yorkshire', SIC07_description!='Real estate activities') %>% mutate(`gva/job` = gvaperjob/1000), 
  category_var = ITL_region_name,
  x_var = gva,
  y_var = jobcount,
  timevar = year,
  label_var = `gva/job`,
  category_var_value_to_highlight = 'South Yorkshire',
  times = c(2015,2017,2019)
)


xrange_adjust = diff(range(p[[2]]$x_pct_change)) * 0.1
yrange_adjust = diff(range(p[[2]]$y_pct_change)) * 0.1

#min and max may be anywhere in vector
# xrange_adjust = diff(  c(min(p[[2]]$x_pct_change,max(p[[2]]$x_pct_change)  )) * 0.1
# xrange_adjust = diff(  c(min(p[[2]]$y_pct_change,max(p[[2]]$y_pct_change)  )) * 0.1

p[[1]] + coord_fixed(
  xlim = c(
    min(p[[2]]$x_pct_change) - xrange_adjust,
    ifelse(max(p[[2]]$x_pct_change) > 0,max(p[[2]]$x_pct_change) + xrange_adjust,0)#hack for health, need to make generic
  ),
  ylim = c(
    min(p[[2]]$y_pct_change) - yrange_adjust,max(p[[2]]$y_pct_change) + yrange_adjust 
  )
) 

p[[1]] + coord_fixed(xlim = c(-50,50),ylim = c(-50,50))




#Also trying with smoothed... ALSO YUCK!
debugonce(twod_generictimeplot_normalisetozero__multipletimepoints)
p <- twod_generictimeplot_normalisetozero__multipletimepoints(
  # df = itl2.gvaperjob %>% filter(SIC07_description=='Information and communication') %>% mutate(`gva/job` = gvaperjob/1000),
  df = itl2.gvaperjob.smoothed %>% filter(SIC07_description=='Manufacturing') %>% ungroup(),
  # df = itl2.gvaperjob %>% filter(grepl('Health',SIC07_description,ignore.case=T)) %>% mutate(`gva/job` = gvaperjob/1000),
  # df = itl2.gvaperjob %>% filter(ITL_region_name == 'South Yorkshire', SIC07_description!='Real estate activities') %>% mutate(`gva/job` = gvaperjob/1000), 
  category_var = ITL_region_name,
  x_var = gva_movingav,
  y_var = jobcount_movingav,
  timevar = year,
  label_var = `gva/job_movingav`,
  category_var_value_to_highlight = 'South Yorkshire',
  times = c(2017:2019)
)

p[[1]] + coord_fixed(xlim = c(-20,20),ylim = c(-20,20))





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#CHAINED VOLUME SLOPE ANALYSIS FOR ITL2 + 2 DIGIT SICS----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

itl2.cv2digit <- read_csv('data/sectors/Table 2b ITL2 UK chained volume measures in 2019 money value pounds million.csv')

#no spaces plz!
names(itl2.cv2digit) <- gsub(x = names(itl2.cv2digit), pattern = ' ', replacement = '_')

#Keep SICs to check removal was correct
sicchk <- itl2.cv2digit %>% 
  select(SIC07_code,SIC07_description) %>% 
  distinct

#Previously selected SIC sectors to remove
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

itl2.cv2digit <- itl2.cv2digit %>% 
  filter(!SIC07_code %in% SICremoves) %>% 
  pivot_longer(`1998`:`2021`, names_to = 'year', values_to = 'value') %>% 
  mutate(year = as.numeric(year))

#Check we got the right ones.. tick
sicchk <- sicchk %>% 
  left_join(
    itl2.cv2digit %>% select(SIC07codefiltered = SIC07_code,SIC07desc_filtered = SIC07_description) %>% distinct,
    by = c('SIC07_code' = 'SIC07codefiltered')
  )

#Save that for elsewhere
saveRDS(itl2.cv2digit, 'data/UKchainedvolume_itl2_SIC_2digit.rds')



#Any neg vals? Yup. Obv, shouldn't be
table(itl2.cv2digit$value < 0)

#Three land transport values in 2020. Is this some odd COVID adjustment?
View(itl2.cv2digit %>% filter(value < 0))

slopes.log <- get_slope_and_se_safely(data = itl2.cv2digit %>% filter(year %in% 2013:2019), ITL_region_name,SIC07_description, y = log(value), x = year)
slopes.log <- get_slope_and_se_safely(data = itl2.cv2digit %>% filter(year %in% 2015:2021), ITL_region_name,SIC07_description, y = log(value), x = year)

# debugonce(slopeDiffGrid)
slopeDiffGrid(slope_df = slopes.log, confidence_interval = 95, column_to_grid = SIC07_description, column_to_filter = ITL_region_name, filterval = 'South Yorkshire')

sector = unique(itl2.cv2digit$SIC07_description)[grepl('pension',unique(itl2.cv2digit$SIC07_description),ignore.case = T)]
sector = unique(itl2.cv2digit$SIC07_description)[grepl('fabric',unique(itl2.cv2digit$SIC07_description),ignore.case = T)]

slopeDiffGrid(slope_df = slopes.log, confidence_interval = 99, column_to_grid = ITL_region_name, column_to_filter = SIC07_description, filterval = sector)


#random check: 2015 to 2021 ICT, is the slope 18.7% per year for SY?
chk <- itl2.cvs %>% 
  filter(
    SIC07_description == 'Information and communication',
    ITL_region_name == 'South Yorkshire',
    year %in% 2015:2021
  )

#Yup, is about right
((chk$value - lag(chk$value))/lag(chk$value))*100
mean(((chk$value - lag(chk$value))/lag(chk$value))*100, na.rm=T)

ggplot(chk, aes(x = year, y = value)) +
  geom_point() +
  geom_smooth(method = lm)

#OK, let's output all the sectors and see see
daterange = c(2013:2019)
daterange = c(2015:2021)
slopes.log <- get_slope_and_se_safely(data = itl2.cv2digit %>% filter(year %in% daterange), ITL_region_name,SIC07_description, y = log(value), x = year)

for(sector in unique(slopes.log$SIC07_description)){
  
  p <- slopeDiffGrid(slope_df = slopes.log, confidence_interval = 95, column_to_grid = ITL_region_name, column_to_filter = SIC07_description, filterval = sector)
  
  ggsave(plot = p, filename = paste0('local/localimages/sector_2digit_slope_grids/',sector,'_',min(daterange),'_',max(daterange),'.png'), width = 13, height = 13)
  
}




#MAPPP
itl2.geo <- st_read('data/geographies/International_Territorial_Level_2_January_2021_UK_BFE_V2_2022_-4735199360818908762/ITL2_JAN_2021_UK_BFE_V2.shp', quiet = T) %>% st_simplify(preserveTopology = T, dTolerance = 100)

#Names match without alteration in this case, jolly good
table(unique(slopes.log$ITL_region_name) %in% itl2.geo$ITL221NM)



# slopes.log <- get_slope_and_se_safely(data = itl2.cvs %>% filter(year %in% 1998:2021), ITL_region_name,SIC07_description, y = log(value), x = year) %>% 
slopes.log <- get_slope_and_se_safely(data = itl2.cv2digit %>% filter(year %in% 2013:2019), ITL_region_name,SIC07_description, y = log(value), x = year) %>%
# slopes.log <- get_slope_and_se_safely(data = itl2.cvs %>% filter(year %in% 2015:2021), ITL_region_name,SIC07_description, y = log(value), x = year) %>%
  #Avoid covid
  # slopes.log <- get_slope_and_se_safely(data = itl2.cvs %>% filter(year %in% 2013:2019), ITL_region_name,SIC07_description, y = log(value), x = year) %>% 
  mutate(
    min99 = slope - (se * getZScore(99)),
    max99 = slope + (se * getZScore(99)),
    min95 = slope - (se * getZScore(95)),
    max95 = slope + (se * getZScore(95)),
    min90 = slope - (se * getZScore(90)),
    max90 = slope + (se * getZScore(90)),
    crosseszero99 = min99 * max99 < 0,#mark if crosses zero
    crosseszero95 = min95 * max95 < 0,#mark if crosses zero
    crosseszero90 = min90 * max90 < 0,#mark if crosses zero
    slopepolarity = ifelse(slope > 0, 'increasing', 'decreasing')
  )



#Pick sector
# sector = slopes.log$SIC07_description[grepl(pattern = 'manuf', x = slopes.log$SIC07_description, ignore.case = T)] %>% unique
sector = slopes.log$SIC07_description[grepl(pattern = 'scientific res', x = slopes.log$SIC07_description, ignore.case = T)] %>% unique
# sector = slopes.log$SIC07_description[grepl(pattern = 'financ', x = slopes.log$SIC07_description, ignore.case = T)] %>% unique

# View(slopes.log %>% filter(SIC07_description == sector))

map <- itl2.geo %>% 
  right_join(
    slopes.log %>% filter(SIC07_description == sector),
    by = c('ITL221NM'='ITL_region_name')
  ) %>% mutate(slope100 = slope * 100)

tm_shape(map) +
  tm_polygons('slope100', n = 7, title="") +
  tm_layout(title = sector, legend.bg.color = 'white', legend.bg.alpha = 0.5) +
  tm_shape(
    map %>% filter(!crosseszero95)
  ) +
  tm_borders(col='blue', lwd = 3)





#DIFF TIME PLOTS FOR GROWTH
itl2.cv2digit <- itl2.cv2digit %>%
  group_by(ITL_region_name,SIC07_description) %>% 
  arrange(year) %>% 
  mutate(
    lagvalue_log = log(value) - log(lag(value)),
    lagvalue_log_movingav = zoo::rollapply(lagvalue_log,7,mean,align='center',fill=NA)
  )


#Plz check maths!
# View(itl2.cv2digit %>% filter(ITL_region_name=='Sheffield', SIC07_description=='Manufacturing', !is.na(lagvalue_log)))
for(sector in unique(itl2.cv2digit$SIC07_description)){
  
  timeplot <- itl2.cv2digit %>% 
    filter(SIC07_description == sector) 
  
  #Or pick top size values
  #Largest % in 2021
  largest_percents <- timeplot %>% 
    filter(year == 2021) %>% 
    arrange(-value)
  
  #Let's get Sheffield and BDR in a set order so colours don't change
  # places = c('South Yorkshire')
  places = c('South Yorkshire','Greater Manchester')
  levels1 <- largest_percents$ITL_region_name[!largest_percents$ITL_region_name %in% places]
  levels <- c(levels1,places)
  
  #Keep only the top ten places and order them
  timeplot <- timeplot %>% 
    mutate(ITL_region_name = factor(ITL_region_name, ordered = T, levels = levels)) %>% 
    filter(ITL_region_name %in% c(largest_percents$ITL_region_name[1:10],places))
  
  
  
  #Mark the ITL of interest so it can be clearer in the plot
  timeplot <- timeplot %>%
    mutate(
      ITL2ofinterest = ifelse(ITL_region_name %in% places, 'ITL of interest','other'),
    )
  
  #turning log diff into percentage change in line using lagvalue
  p <- ggplot(timeplot %>% rename(`ITL region` = ITL_region_name) %>% filter(year %in% c(1998:2021), !is.na(lagvalue_log_movingav)),
              # ggplot(timeplot %>% rename(`ITL region` = ITL_region_name) %>% filter(year %in% c(2010:2021)),
              # aes(x = year, y = value, colour = `ITL region`, size = ITL2ofinterest, linetype = ITL2ofinterest, group = `ITL region`)) +
              # aes(x = year, y = (exp(lagvalue_log) -1) * 100, colour = `ITL region`, size = ITL2ofinterest, linetype = ITL2ofinterest, group = `ITL region`)) +
              aes(x = year, y = (exp(lagvalue_log_movingav) -1) * 100, colour = `ITL region`, size = ITL2ofinterest, linetype = ITL2ofinterest, group = `ITL region`)) +
    geom_point() +
    geom_line() +
    scale_size_manual(values = c(4,1)) +
    scale_color_brewer(palette = 'Paired', direction = 1) +
    ylab('GVA (chained volume) percent change, 7 year moving av') +
    # scale_y_log10() +
    guides(size = "none", linetype = "none") +
    ggtitle(
      paste0(sector,'\n', paste0(places, collapse = ', '), ' highlighted in thicker lines')
    ) +
    theme(plot.title = element_text(face = 'bold')) +
    geom_hline(yintercept = 0)
  
  ggsave(paste0('local/localimages/ITL2_sector_timeplots_diff/',sector,'.png'), p, width = 12, height = 8)
  
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#ITL2 + 2 DIGIT SICS LINKED TO JOBS----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

itl2.cv2digit <- readRDS('data/UKchainedvolume_itl2_SIC_2digit.rds')

#Can we just adapt from previous link to BRES at 2-digit SIC for current prices?
#In theory, the only column that's going to change is the value
#Once all LQ etc calcs are dropped
gva_w_jobcounts <- readRDS('data/UK_GVA_with_BRES_jobcounts_and_jobcounts_summedfrom5digitSIC.rds') %>% 
  select(-c(`SIC07 code`:LQ_log),-GVA_perFTjob,-GVA_perFTjob_5digit,-GVA_per_EMPLOYMENT,-GVA_per_EMPLOYMENT_5digit,-GVA) %>% 
  rename(ITL_region_name = GEOGRAPHY_NAME, SIC07_description = INDUSTRY_NAME, SIC07_code = INDUSTRY_CODE, year = DATE)

#Check link with chained volume... 
unique(itl2.cv2digit$SIC07_code)[order(unique(itl2.cv2digit$SIC07_code))]
unique(gva_w_jobcounts$SIC07_code)[order(unique(gva_w_jobcounts$SIC07_code))]

#I think keeping only text in brackets should make a perfect match
#Pull out numbers in brackets, don't need the rest
#https://stackoverflow.com/a/8613332
repl <- regmatches(itl2.cv2digit$SIC07_code, gregexpr("(?<=\\().*?(?=\\))", itl2.cv2digit$SIC07_code, perl=T))
repl[lengths(repl) == 0] <- NA
repl <- unlist(repl)

itl2.cv2digit$SIC07_code <- ifelse(is.na(repl), itl2.cv2digit$SIC07_code, repl)

#A couple still not matching... those are both sectors with zero job counts, can be dropped
unique(itl2.cv2digit$SIC07_code)[!unique(itl2.cv2digit$SIC07_code) %in% unique(gva_w_jobcounts$SIC07_code)]

itl2.cv2digit.jobs <- itl2.cv2digit %>% 
  select(-SIC07_description) %>% 
  filter(!SIC07_code %in% c("68IMP","97-98"))

#TICK
table(unique(itl2.cv2digit.jobs$SIC07_code) %in% unique(gva_w_jobcounts$SIC07_code))


#MERGE THE CHAINED VOLUME VALUES INTO THE JOB COUNTS
#Right join to keep only 2015-2021 years that there's job data for
itl2.cv2digit.jobs <- itl2.cv2digit.jobs %>% 
  right_join(
    gva_w_jobcounts,
    by = c('SIC07_code','ITL_region_name','year')
  )


#Get gva per job value. Keep same name as others to make code more reusable, but note this is FULL TIME jobs
itl2.cv2digit.jobs <- itl2.cv2digit.jobs %>% 
  mutate(gvaperjob = (value * 1000000) / JOBCOUNT_FT_5DIGIT)


#aaaand save there
saveRDS(itl2.cv2digit.jobs, 'data/ITL2_chainedvolumeGVA_2digitSICs_andjobs.rds')


sector = itl2.cv2digit.jobs$SIC07_description[grepl(pattern = 'telecom', x = itl2.cv2digit.jobs$SIC07_description, ignore.case = T)] %>% unique

#Keep only sectors over a certain size to display
#As some of the smaller ones are very volatile over time
#Base on size in 2019
place = 'South Yorkshire'
place = 'Greater Manchester'

sectororder <- itl2.cv2digit.jobs %>% 
  filter(ITL_region_name == place, year == 2019) %>% 
  arrange(-JOBCOUNT_FT_5DIGIT) %>% 
  mutate(JOBCOUNT_FT_5DIGIT_PERCENT = (JOBCOUNT_FT_5DIGIT/sum(JOBCOUNT_FT_5DIGIT))*100)
  
#Actually, let's just look. For SY, tricky sectors including insurance and pension funding
#But it's big enough that I don't think I want it removed.
#What are the numbers doing?
#Numbers are tiny, but then 1500 workers. A firm or two must have moved here. Not a huge % of jobs
View(itl2.cv2digit.jobs %>% filter(ITL_region_name == place, SIC07_description == 'Insurance and pension funding'))
View(itl2.cv2digit.jobs %>% filter(ITL_region_name == place, SIC07_description == 'Forestry and fishing'))

sectorstodrop = c('Insurance and pension funding','Forestry and fishing')
sectorstodrop = NULL

#Or if keeping some e.g. here look just at those subsectors in the ICT section
chk <- itl2.cv2digit.jobs$SIC07_description[grepl(pattern = 'telecom|publishing|motion|program|information', x = itl2.cv2digit.jobs$SIC07_description, ignore.case = T)] %>% unique
sectorstodrop <- itl2.cv2digit.jobs$SIC07_description[!grepl(pattern = 'telecom|publishing|motion|program|information', x = itl2.cv2digit.jobs$SIC07_description, ignore.case = T)] %>% unique

#manufacturing
chk <- itl2.cv2digit.jobs$SIC07_description[grepl(pattern = 'manufact', x = itl2.cv2digit.jobs$SIC07_description, ignore.case = T)] %>% unique
sectorstodrop <- itl2.cv2digit.jobs$SIC07_description[!grepl(pattern = 'manufact', x = itl2.cv2digit.jobs$SIC07_description, ignore.case = T)] %>% unique

#Let's use smoothed values too. Another option is predicted values at each end from slope, but...
itl2.cv2digit.jobs <- itl2.cv2digit.jobs %>%
  group_by(ITL_region_name,SIC07_description) %>% 
  arrange(year) %>% 
  mutate(
    gva_movingav = zoo::rollapply(value,3,mean,align='center',fill=NA),
    jobs_movingav = zoo::rollapply(JOBCOUNT_FT_5DIGIT,3,mean,align='center',fill=NA),
    gvaperjob_movingav = zoo::rollapply(gvaperjob,3,mean,align='center',fill=NA)
  )

#Remaining centred years with smoothed values
unique(itl2.cv2digit.jobs$year[!is.na(itl2.cv2digit.jobs$gva_movingav)])   

p <- twod_generictimeplot_normalisetozero(
  # df = itl2.gvaperjob %>% filter(SIC07_description=='Information and communication') %>% mutate(`gva/job` = gvaperjob/1000),
  # df = itl2.gvaperjob %>% filter(SIC07_description=='Manufacturing') %>% mutate(`gva/job` = gvaperjob/1000),
  # df = itl2.gvaperjob %>% filter(grepl('Health',SIC07_description,ignore.case=T)) %>% mutate(`gva/job` = gvaperjob/1000),
  df = itl2.cv2digit.jobs %>% filter(ITL_region_name == place, !SIC07_description %in% sectorstodrop) %>% mutate(`gva/job` = gvaperjob_movingav/1000),
  # df = itl2.cv2digit.jobs %>% filter(ITL_region_name == 'West Yorkshire', SIC07_description!='Real estate activities') %>% mutate(`gva/job` = gvaperjob/1000),
  category_var = SIC07_description,
  x_var = gva_movingav,
  y_var = jobs_movingav,
  timevar = year,
  label_var = `gva/job`,
  # category_var_value_to_highlight = sector,
  start_time = 2016,
  end_time = 2020
)

xrange_adjust = diff(range(p[[2]]$x_pct_change)) * 0.1
yrange_adjust = diff(range(p[[2]]$y_pct_change)) * 0.1

p[[1]] + coord_fixed(
  xlim = c(
    min(p[[2]]$x_pct_change) - xrange_adjust,
    ifelse(max(p[[2]]$x_pct_change) > 0,max(p[[2]]$x_pct_change) + xrange_adjust,0)#hack for health, need to make generic
  ),
  ylim = c(
    min(p[[2]]$y_pct_change) - yrange_adjust,max(p[[2]]$y_pct_change) + yrange_adjust
    # -20,80
  )
) 




#Looking at particular sector
p <- twod_generictimeplot_normalisetozero(
  df = itl2.cv2digit.jobs %>% filter(SIC07_description=='Telecommunications') %>% mutate(`gva/job` = gvaperjob_movingav/1000),
  category_var = ITL_region_name,
  x_var = gva_movingav,
  y_var = jobs_movingav,
  timevar = year,
  label_var = `gva/job`,
  category_var_value_to_highlight = place,
  start_time = 2016,
  end_time = 2020
)

xrange_adjust = diff(range(p[[2]]$x_pct_change)) * 0.1
yrange_adjust = diff(range(p[[2]]$y_pct_change)) * 0.1

p[[1]] + coord_fixed(
  xlim = c(
    min(p[[2]]$x_pct_change) - xrange_adjust,
    ifelse(max(p[[2]]$x_pct_change) > 0,max(p[[2]]$x_pct_change) + xrange_adjust,0)#hack for health, need to make generic
  ),
  ylim = c(
    min(p[[2]]$y_pct_change) - yrange_adjust,max(p[[2]]$y_pct_change) + yrange_adjust 
  )
) 

#Haaaack
p[[1]] + coord_fixed(
  xlim = c(-50,250),
  ylim = c(-60,60)
)




#Again, but not normalised so we can see where SY is
p <- twod_generictimeplot(
  df = itl2.cv2digit.jobs %>% filter(SIC07_description=='Telecommunications') %>% mutate(`gva/job` = gvaperjob_movingav/1000),
  category_var = ITL_region_name,
  x_var = gva_movingav,
  y_var = jobs_movingav,
  timevar = year,
  label_var = `gva/job`,
  start_time = 2016,
  end_time = 2020
)

p + 
  scale_y_log10() +
  scale_x_log10() 




#All plz!
for(sector in unique(itl2.gvaperjob$SIC07_description)){
  
  p <- twod_generictimeplot_normalisetozero(
    df = itl2.gvaperjob %>% filter(SIC07_description==sector) %>% mutate(`gva/job` = gvaperjob/1000),
    category_var = ITL_region_name,
    x_var = gva,
    y_var = jobcount,
    timevar = year,
    label_var = `gva/job`,
    category_var_value_to_highlight = 'South Yorkshire',
    start_time = 2015,
    # end_time = 2021
    end_time = 2019
  )
  
  # p <- p[[1]] + coord_fixed()
  
  xrange_adjust = diff(range(p[[2]]$x_pct_change)) * 0.1
  yrange_adjust = diff(range(p[[2]]$y_pct_change)) * 0.1
  
  p <- p[[1]] + coord_fixed(
    xlim = c(
      min(p[[2]]$x_pct_change) - xrange_adjust,
      ifelse(max(p[[2]]$x_pct_change) > 0,max(p[[2]]$x_pct_change) + xrange_adjust,0)#hack for health, need to make generic
    ),
    ylim = c(
      min(p[[2]]$y_pct_change) - yrange_adjust,max(p[[2]]$y_pct_change) + yrange_adjust 
    )
  ) 
  
  # ggsave(plot = p, filename = paste0('local/localimages/2D_COMPASSPLOTS_SECTORS/',gsub("[^A-Za-z]", "", sector),'.png'), width = 12, height = 12)
  ggsave(plot = p, filename = paste0('local/localimages/2D_COMPASSPLOTS_SECTORS_to2019/',gsub("[^A-Za-z]", "", sector),'.png'), width = 12, height = 12)
  # ggsave(plot = p, filename = paste0('local/localimages/2D_COMPASSPLOTS_SECTORS_2019to2021/',gsub("[^A-Za-z]", "", sector),'.png'), width = 12, height = 12)
  
}





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#CHAINED VOLUME SLOPE ANALYSIS FOR ITL3 AND 20 SIC SECTIONS----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#It may not have the same 20 sections, we'll need to look. Let's see.
itl3.cv <- read_csv('data/sectors/Table 3b ITL3 UK chained volume measures in 2019 money value pounds million.csv')

names(itl3.cv) <- gsub(x = names(itl3.cv), pattern = ' ', replacement = '_')

cvSICkeeps <- itl3.cv$SIC07_code[substr(itl3.cv$SIC07_code,2,2) == ' '] %>% unique

#Check if it's a different list from ITL2 chained volume...
chk <- itl2.cv$SIC07_code[substr(itl2.cv$SIC07_code,2,2) == ' '] %>% unique

#Yes, it's different. A few to add in manually where sections codes combined
#Which ones in ITL3 codes are NOT in ITL2, to narrow down?
#We don't want all of these, but some we will
unique(itl3.cv$SIC07_code)[!unique(itl3.cv$SIC07_code) %in% itl2.cv$SIC07_code[substr(itl2.cv$SIC07_code,2,2) == ' '] %>% unique]

#Just these two, I believe
#"AB (1-9)"
#"DE (35-39)"
cvSICkeeps <- c(cvSICkeeps,"AB (1-9)","DE (35-39)")

itl3.cvs <- itl3.cv %>% 
  filter(SIC07_code %in% cvSICkeeps) %>% 
  pivot_longer(`1998`:`2021`, names_to = 'year', values_to = 'value') %>% 
  mutate(year = as.numeric(year))

#Not lost much at all in the combining relative to this level at ITL2
View(itl3.cvs %>% select(SIC07_code,SIC07_description) %>% distinct)

#Save that for elsewhere
saveRDS(itl3.cvs, 'data/UKchainedvolume_itl3_SIC_sections.rds')




slopes.log <- get_slope_and_se_safely(data = itl3.cvs %>% filter(year %in% 2015:2021), ITL_region_name,SIC07_description, y = log(value), x = year)
#Avoid covid
slopes.log <- get_slope_and_se_safely(data = itl3.cvs %>% filter(year %in% 2010:2019), ITL_region_name,SIC07_description, y = log(value), x = year)


#Two places against each other
#debugonce(slopeDiffGrid)
place1 = itl3.cvs %>% select(ITL_region_name) %>% distinct %>% filter(grepl('Sheffield',ITL_region_name,ignore.case=T)) %>% pull %>% unique
place2 = itl3.cvs %>% select(ITL_region_name) %>% distinct %>% filter(grepl('Barnsley',ITL_region_name,ignore.case=T)) %>% pull %>% unique

slopeDiffGrid(slope_df = slopes.log, confidence_interval = 95, column_to_grid = SIC07_description, column_to_filter = ITL_region_name, filterval = place1)
slopeDiffGrid(slope_df = slopes.log, confidence_interval = 95, column_to_grid = SIC07_description, column_to_filter = ITL_region_name, filterval = place2)


#Repeat, filtering out agri (see below - sudden jump in recent years looks odd, and its a tiny sector anyway)
slopeDiffGrid(slope_df = slopes.log %>% filter(SIC07_description!='Agriculture, forestry and fishing; mining and quarrying'), confidence_interval = 95, column_to_grid = SIC07_description, column_to_filter = ITL_region_name, filterval = place1)
slopeDiffGrid(slope_df = slopes.log %>% filter(SIC07_description!='Agriculture, forestry and fishing; mining and quarrying'), confidence_interval = 95, column_to_grid = SIC07_description, column_to_filter = ITL_region_name, filterval = place2)


#manuf looking stronger in BDR. Compare all places for manuf
slopeDiffGrid(slope_df = slopes.log, confidence_interval = 95, column_to_grid = ITL_region_name, column_to_filter = SIC07_description, filterval = 'Manufacturing')

#That's waaay too many places. Option to return data added, let's see about measuring how many pos/neg slopes
griddata <- slopeDiffGrid(slope_df = slopes.log, confidence_interval = 95, column_to_grid = ITL_region_name, column_to_filter = SIC07_description, filterval = 'Manufacturing', returndata = T)

#So, if we group by gridcol2 (which is the y axis)
#And then count slopes that are false for CIs_overlap and then the pos or neg slopes
#Can then group the prop poses and see where certain places lie 
#
count_sigs <- griddata %>% 
  group_by(gridcol2) %>% 
  summarise(
    prop_pos = mean(!CIs_overlap & slopediff > 0),
    prop_neg = mean(!CIs_overlap & slopediff < 0)
  )

#https://stats.stackexchange.com/a/204354
#Get percentile
ecdf_fun <- function(x,perc) ecdf(x)(perc)
ecdf_fun(count_sigs$prop_pos,count_sigs$prop_pos[count_sigs$gridcol2 == 'Barnsley, Doncaster and Rotherham'])
ecdf_fun(count_sigs$prop_pos,count_sigs$prop_pos[count_sigs$gridcol2 == 'Sheffield'])
ecdf_fun(count_sigs$prop_neg,count_sigs$prop_neg[count_sigs$gridcol2 == 'Barnsley, Doncaster and Rotherham'])
ecdf_fun(count_sigs$prop_neg,count_sigs$prop_neg[count_sigs$gridcol2 == 'Sheffield'])


#Which we could then repeat for all sectors, to see where both places sit, right?
propz <- list()

for(sector in unique(slopes.log$SIC07_description)){
  
  griddata <- slopeDiffGrid(slope_df = slopes.log, confidence_interval = 95, column_to_grid = ITL_region_name, column_to_filter = SIC07_description, filterval = sector, returndata = T)
  
  count_sigs <- griddata %>% 
    group_by(gridcol2) %>% 
    summarise(
      prop_pos = mean(!CIs_overlap & slopediff > 0, na.rm = T),
      prop_neg = mean(!CIs_overlap & slopediff < 0, na.rm = T)
    )
  
  #https://stats.stackexchange.com/a/204354
  #Get percentile
  bpos <- ecdf_fun(count_sigs$prop_pos,count_sigs$prop_pos[count_sigs$gridcol2 == 'Barnsley, Doncaster and Rotherham'])
  spos <- ecdf_fun(count_sigs$prop_pos,count_sigs$prop_pos[count_sigs$gridcol2 == 'Sheffield'])
  bneg <- ecdf_fun(count_sigs$prop_neg,count_sigs$prop_neg[count_sigs$gridcol2 == 'Barnsley, Doncaster and Rotherham'])
  sneg <- ecdf_fun(count_sigs$prop_neg,count_sigs$prop_neg[count_sigs$gridcol2 == 'Sheffield'])
  
  val_bpos <- count_sigs$prop_pos[count_sigs$gridcol2 == 'Barnsley, Doncaster and Rotherham']
  val_spos <- count_sigs$prop_pos[count_sigs$gridcol2 == 'Sheffield']
  val_bneg <- count_sigs$prop_neg[count_sigs$gridcol2 == 'Barnsley, Doncaster and Rotherham']
  val_sneg <- count_sigs$prop_neg[count_sigs$gridcol2 == 'Sheffield']
  
  minpos <- min(count_sigs$prop_pos,count_sigs$prop_pos)
  maxpos <- max(count_sigs$prop_pos,count_sigs$prop_pos)
  minneg <- min(count_sigs$prop_neg,count_sigs$prop_pos)
  maxneg <- max(count_sigs$prop_neg,count_sigs$prop_pos)
  
  propz[[length(propz)+1]] <- list(
    sector = sector, bpos = bpos, spos = spos, bneg = bneg, sneg = sneg, 
    val_bpos=val_bpos,
    val_spos=val_spos,
    val_bneg=val_bneg,
    val_sneg=val_sneg,
    minpos=minpos,
    maxpos=maxpos,
    minneg=minneg,
    maxneg=maxneg
    )
  
}

percentiles_df <- bind_rows(propz)







#Compare one to the other on different axes
slopeDiffGrid(slope_df = slopes.log, confidence_interval = 95, column_to_grid = SIC07_description, column_to_filter = ITL_region_name, filterval = place2, filterval2 = place1)

slopeDiffGrid(slope_df = slopes.log, confidence_interval = 95, column_to_grid = SIC07_description, column_to_filter = ITL_region_name, filterval = place1, filterval2 = place2)







#Look at order by sector size in 2021
chk <- itl3.cvs %>% 
  filter(ITL_region_name %in% c(place1,place2), year == 2021) %>% 
  arrange(ITL_region_name,value)

#Just looking at change in agri forestry etc
chk <- itl3.cvs %>% 
  filter(ITL_region_name %in% c(place1,place2), SIC07_description == 'Agriculture, forestry and fishing; mining and quarrying') %>% 
  arrange(ITL_region_name,year)





#Time plots for all sectors, highlighting Sheffield + BDR
#Education doesn't seem to separate in the same way it did with the proportions, what's the crack?
sector <- itl3.cvs$SIC07_description[grepl('information', itl3.cvs$SIC07_description ,ignore.case = T)] %>% unique
sector <- itl3.cvs$SIC07_description[grepl('educ', itl3.cvs$SIC07_description ,ignore.case = T)] %>% unique




#Also, try a differenced version - change in growth between timepoints. What does that look like?
#Places will then start from same zero point, good way to compare scales
itl3.cvs <- itl3.cvs %>%
  group_by(ITL_region_name,SIC07_description) %>% 
  arrange(year) %>% 
  mutate(
    lagvalue_log = log(value) - log(lag(value)),
    lagvalue_log_movingav = zoo::rollapply(lagvalue_log,7,mean,align='center',fill=NA)
    )


#Plz check maths!
# View(itl3.cvs %>% filter(ITL_region_name=='Sheffield', SIC07_description=='Manufacturing', !is.na(lagvalue_log)))
for(sector in unique(itl3.cvs$SIC07_description)){

  timeplot <- itl3.cvs %>% 
    filter(SIC07_description == sector) 
  
  #Or pick top size values
  #Largest % in 2021
  largest_percents <- timeplot %>% 
    filter(year == 2021) %>% 
    arrange(-value)
  
  #Let's get Sheffield and BDR in a set order so colours don't change
  places = c('Sheffield','Barnsley, Doncaster and Rotherham')
  levels1 <- largest_percents$ITL_region_name[!largest_percents$ITL_region_name %in% places]
  levels <- c(levels1,places)
  
  #Keep only the top ten places and order them
  timeplot <- timeplot %>% 
    mutate(ITL_region_name = factor(ITL_region_name, ordered = T, levels = levels)) %>% 
    filter(ITL_region_name %in% c(largest_percents$ITL_region_name[1:10],'Sheffield','Barnsley, Doncaster and Rotherham'))
  
  
  
  #Mark the ITL of interest so it can be clearer in the plot
  timeplot <- timeplot %>%
    mutate(
      ITL2ofinterest = ifelse(ITL_region_name %in% places, 'ITL of interest','other'),
    )
  
  #turning log diff into percentage change in line using lagvalue
  p <- ggplot(timeplot %>% rename(`ITL region` = ITL_region_name) %>% filter(year %in% c(1998:2021), !is.na(lagvalue_log_movingav)),
  # ggplot(timeplot %>% rename(`ITL region` = ITL_region_name) %>% filter(year %in% c(2010:2021)),
           aes(x = year, y = value, colour = `ITL region`, size = ITL2ofinterest, linetype = ITL2ofinterest, group = `ITL region`)) +
           # aes(x = year, y = (exp(lagvalue_log) -1) * 100, colour = `ITL region`, size = ITL2ofinterest, linetype = ITL2ofinterest, group = `ITL region`)) +
           # aes(x = year, y = (exp(lagvalue_log_movingav) -1) * 100, colour = `ITL region`, size = ITL2ofinterest, linetype = ITL2ofinterest, group = `ITL region`)) +
    geom_point() +
    geom_line() +
    scale_size_manual(values = c(4,1)) +
    scale_color_brewer(palette = 'Paired', direction = 1) +
    ylab('GVA (chained volume, millions)') +
    scale_y_log10() +
    guides(size = "none", linetype = "none") +
    ggtitle(
      paste0(sector,'\n', paste0(places, collapse = ', '), ' highlighted in thicker lines')
    ) +
    theme(plot.title = element_text(face = 'bold')) +
    geom_hline(yintercept = 0)
  
  ggsave(paste0('local/localimages/ITL3_sector_timeplots/',sector,'.png'), p, width = 12, height = 8)

}





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#CHAINED VOLUME SLOPE ANALYSIS FOR ITL3 AT 2 DIGIT SIC----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Again, probably a different list of available 2 digit SICs compared to ITL2
itl3.cv2digit <- read_csv('data/sectors/Table 3b ITL3 UK chained volume measures in 2019 money value pounds million.csv')

#no spaces plz!
names(itl3.cv2digit) <- gsub(x = names(itl3.cv2digit), pattern = ' ', replacement = '_')

#Keep SICs to check removal was correct
sicchk <- itl3.cv2digit %>% 
  select(SIC07_code,SIC07_description) %>% 
  distinct

#Previously selected SIC sectors to remove
ITL3_SICremoves = c(
  'Total',#leave this in the CSV by commenting out, to check the categories left over total correctly in lines 42-63 below (then can remove by uncommenting)
  'A-E',
  'C (10-33)',
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


itl3.cv2digit <- itl3.cv2digit %>% 
  filter(!SIC07_code %in% ITL3_SICremoves) %>% 
  pivot_longer(`1998`:`2021`, names_to = 'year', values_to = 'value') %>% 
  mutate(year = as.numeric(year))

#Check we got the right ones.. tick
sicchk <- sicchk %>% 
  left_join(
    itl3.cv2digit %>% select(SIC07codefiltered = SIC07_code,SIC07desc_filtered = SIC07_description) %>% distinct,
    by = c('SIC07_code' = 'SIC07codefiltered')
  )

#Save that for elsewhere
saveRDS(itl3.cv2digit, 'data/UKchainedvolume_itl3_SIC_2digit.rds')



#SECTOR DIFF TIMEPLOTS
itl3.cv2digit <- itl3.cv2digit %>%
  group_by(ITL_region_name,SIC07_description) %>% 
  arrange(year) %>% 
  mutate(
    lagvalue_log = log(value) - log(lag(value)),
    lagvalue_log_movingav = zoo::rollapply(lagvalue_log,7,mean,align='center',fill=NA)
  )


for(sector in unique(itl3.cv2digit$SIC07_description)){
  
  timeplot <- itl3.cv2digit %>% 
    filter(SIC07_description == sector) 
  
  #Or pick top size values
  #Largest % in 2021
  largest_percents <- timeplot %>% 
    filter(year == 2021) %>% 
    arrange(-value)
  
  #Let's get Sheffield and BDR in a set order so colours don't change
  places = c('Sheffield','Barnsley, Doncaster and Rotherham')
  levels1 <- largest_percents$ITL_region_name[!largest_percents$ITL_region_name %in% places]
  levels <- c(levels1,places)
  
  #Keep only the top ten places and order them
  timeplot <- timeplot %>% 
    mutate(ITL_region_name = factor(ITL_region_name, ordered = T, levels = levels)) %>% 
    filter(ITL_region_name %in% c(largest_percents$ITL_region_name[1:10],'Sheffield','Barnsley, Doncaster and Rotherham'))
  
  
  
  #Mark the ITL of interest so it can be clearer in the plot
  timeplot <- timeplot %>%
    mutate(
      ITL2ofinterest = ifelse(ITL_region_name %in% places, 'ITL of interest','other'),
    )
  
  #turning log diff into percentage change in line using lagvalue
  p <- ggplot(timeplot %>% rename(`ITL region` = ITL_region_name) %>% filter(year %in% c(1998:2021), !is.na(lagvalue_log_movingav)),
              # ggplot(timeplot %>% rename(`ITL region` = ITL_region_name) %>% filter(year %in% c(2010:2021)),
              # aes(x = year, y = value, colour = `ITL region`, size = ITL2ofinterest, linetype = ITL2ofinterest, group = `ITL region`)) +
              # aes(x = year, y = (exp(lagvalue_log) -1) * 100, colour = `ITL region`, size = ITL2ofinterest, linetype = ITL2ofinterest, group = `ITL region`)) +
              aes(x = year, y = (exp(lagvalue_log_movingav) -1) * 100, colour = `ITL region`, size = ITL2ofinterest, linetype = ITL2ofinterest, group = `ITL region`)) +
    geom_point() +
    geom_line() +
    scale_size_manual(values = c(4,1)) +
    scale_color_brewer(palette = 'Paired', direction = 1) +
    ylab('Percent change in GVA year on year, 7 year moving av (chained volume, millions)') +
    # scale_y_log10() +
    guides(size = "none", linetype = "none") +
    ggtitle(
      paste0(sector,'\n', paste0(places, collapse = ', '), ' highlighted in thicker lines')
    ) +
    theme(plot.title = element_text(face = 'bold')) +
    geom_hline(yintercept = 0)
  
  ggsave(paste0('local/localimages/ITL3_2digitsector_timeplots_diff/',sector,'.png'), p, width = 12, height = 8)
  
}





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#BASIC PROPORTIONS FOR ITL3 AND 20 SIC SECTIONS----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Which I don't seem to have. Can compare props to LQs easily enough, let's just check plz!
itl3.cp <- read_csv('data/sectors/Table 3c ITL3 UK current price estimates pounds million.csv')

names(itl3.cp) <- gsub(x = names(itl3.cp), pattern = ' ', replacement = '_')

cvSICkeeps <- itl3.cp$SIC07_code[substr(itl3.cp$SIC07_code,2,2) == ' '] %>% unique

#Check if it's a different list from ITL2 chained volume...
chk <- itl2.cv$SIC07_code[substr(itl2.cv$SIC07_code,2,2) == ' '] %>% unique

#Yes, it's different. A few to add in manually where sections codes combined
#Which ones in ITL3 codes are NOT in ITL2, to narrow down?
#We don't want all of these, but some we will
unique(itl3.cp$SIC07_code)[!unique(itl3.cp$SIC07_code) %in% itl2.cv$SIC07_code[substr(itl2.cv$SIC07_code,2,2) == ' '] %>% unique]

#Just these two, I believe
#"AB (1-9)"
#"DE (35-39)"
cvSICkeeps <- c(cvSICkeeps,"AB (1-9)","DE (35-39)")

itl3.cps <- itl3.cp %>% 
  filter(SIC07_code %in% cvSICkeeps) %>% 
  pivot_longer(`1998`:`2021`, names_to = 'year', values_to = 'value') %>% 
  mutate(year = as.numeric(year))

#Not lost much at all in the combining relative to this level at ITL2
View(itl3.cps %>% select(SIC07_code,SIC07_description) %>% distinct)

#Save that for elsewhere
saveRDS(itl3.cps, 'data/UKcurrentprices_itl3_SIC_sections.rds')



#Props and LQs
itl3.cps <- itl3.cps %>% 
  split(.$year) %>% 
  map(add_location_quotient_and_proportions, 
      regionvar = ITL_region_name,
      lq_var = SIC07_description,
      valuevar = value) %>% 
  bind_rows()


itl3.cps %>% filter(
  ITL_region_name == 'Sheffield',
  # grepl(x = ITL_region_name, pattern = 'Rotherham'),
  year == 2021
) %>% 
  mutate(regional_percent = sector_regional_proportion *100) %>% 
  select(SIC07_description,regional_percent, LQ) %>% 
  arrange(-regional_percent) %>% 
  slice(1:length(unique(itl3.cps$SIC07_description)))

itl3.cps %>% filter(
  # ITL_region_name == 'Sheffield',
  grepl(x = ITL_region_name, pattern = 'Rotherham'),
  year == 2021
) %>% 
  mutate(regional_percent = sector_regional_proportion *100) %>% 
  select(SIC07_description,regional_percent, LQ) %>% 
  arrange(-regional_percent) %>% 
  slice(1:length(unique(itl3.cps$SIC07_description)))

itl3.cps.proporder <- itl3.cps %>% filter(
  ITL_region_name == 'Sheffield',
  # grepl(x = ITL_region_name, pattern = 'Rotherham'),
  year == 2021
) %>% 
  mutate(regional_percent = sector_regional_proportion *100) %>% 
  select(SIC07_description,regional_percent, LQ) %>% 
  arrange(-regional_percent) %>% 
  slice(1:length(unique(itl3.cps$SIC07_description)))




#Pick out particular sector and view all LQs and props for different places
itl3.cps %>% filter(
  # ITL_region_name == 'Sheffield',
  grepl(x = SIC07_description, pattern = 'information', ignore.case = T),
  year == 2021
) %>% 
  mutate(regional_percent = sector_regional_proportion *100) %>% 
  select(ITL_region_name,regional_percent, LQ) %>% 
  arrange(-LQ) %>% 
  slice(1:length(unique(itl3.cps$ITL_region_name))) %>% print(n=50)

itl3.cps %>% filter(
  # ITL_region_name == 'Sheffield',
  grepl(x = SIC07_description, pattern = 'manuf', ignore.case = T),
  year == 2021
) %>% 
  mutate(regional_percent = sector_regional_proportion *100) %>% 
  select(ITL_region_name,regional_percent, LQ) %>% 
  arrange(-LQ) %>% 
  slice(1:length(unique(itl3.cps$ITL_region_name))) %>% print(n=50)


#Maps of those also would be nice. Which I have already done somewhere...








#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#BASIC PROPORTIONS FOR ITL2 AND 20 SIC SECTIONS----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

itl2.cp <- read_csv('data/sectors/Table 2c ITL2 UK current price estimates pounds million.csv')

names(itl2.cp) <- gsub(x = names(itl2.cp), pattern = ' ', replacement = '_')

#This gets all the letters, nice
cvSICkeeps <- itl2.cp$SIC07_code[substr(itl2.cp$SIC07_code,2,2) == ' '] %>% unique

#Filter out duplicate value rows and make long by year
#Also convert year to numeric
itl2.cps <- itl2.cp %>% 
  filter(SIC07_code %in% cvSICkeeps) %>% 
  pivot_longer(`1998`:`2021`, names_to = 'year', values_to = 'value') %>% 
  mutate(year = as.numeric(year))

#Save that for elsewhere
saveRDS(itl2.cps, 'data/UKcurrentprices_itl2_SIC_sections.rds')

#Props and LQs
itl2.cps <- itl2.cps %>% 
  split(.$year) %>% 
  map(add_location_quotient_and_proportions,
      regionvar = ITL_region_name,
      lq_var = SIC07_description,
      valuevar = value) %>% 
  bind_rows()


itl2.cps %>% filter(
  grepl(x = ITL_region_name, pattern = 'South Y'),
  year == 2021
) %>% 
  mutate(regional_percent = sector_regional_proportion *100) %>% 
  select(SIC07_description,regional_percent, LQ) %>% 
  arrange(-regional_percent) %>% 
  slice(1:length(unique(itl2.cps$SIC07_description)))

itl2.cps %>% filter(
  grepl(x = ITL_region_name, pattern = 'West Y'),
  year == 2021
) %>% 
  mutate(regional_percent = sector_regional_proportion *100) %>% 
  select(SIC07_description,regional_percent, LQ) %>% 
  arrange(-regional_percent) %>% 
  slice(1:length(unique(itl2.cps$SIC07_description)))

itl2.cps %>% filter(
  grepl(x = ITL_region_name, pattern = 'Mersey'),
  year == 2021
) %>% 
  mutate(regional_percent = sector_regional_proportion *100) %>% 
  select(SIC07_description,regional_percent, LQ) %>% 
  arrange(-regional_percent) %>% 
  slice(1:length(unique(itl2.cps$SIC07_description)))


itl2.cps %>% filter(
  grepl(x = ITL_region_name, pattern = 'Manc'),
  year == 2021
) %>% 
  mutate(regional_percent = sector_regional_proportion *100) %>% 
  select(SIC07_description,regional_percent, LQ) %>% 
  arrange(-regional_percent) %>% 
  slice(1:length(unique(itl2.cps$SIC07_description)))







itl2.cps.proporder <- itl2.cps %>% filter(
  # grepl(x = ITL_region_name, pattern = 'Manc'),
  grepl(x = ITL_region_name, pattern = 'South Y'),
  # year == 2021
  year == 2015
) %>% 
  mutate(regional_percent = sector_regional_proportion *100) %>% 
  select(SIC07_description,regional_percent, LQ) %>% 
  arrange(-regional_percent) %>% 
  slice(1:length(unique(itl2.cps$SIC07_description)))


itl2.cps %>% filter(
  # grepl(x = ITL_region_name, pattern = 'Manc'),
  grepl(x = ITL_region_name, pattern = 'South Y'),
  year == 2021
  # year == 2019
) %>% 
  mutate(regional_percent = sector_regional_proportion *100) %>% 
  select(SIC07_description,regional_percent, LQ) %>% 
  # arrange(-LQ) %>% 
  arrange(-regional_percent) %>%
  slice(1:length(unique(itl2.cps$SIC07_description)))



#Checking size changes
#Current prices
itl2.cps %>% filter(
  grepl(x = ITL_region_name, pattern = 'South Y'),
  # grepl(x = SIC07_description, pattern = 'Information')
  grepl(x = SIC07_description, pattern = 'health', ignore.case=T)) %>% 
  mutate(regional_percent = sector_regional_proportion *100) %>% 
  select(year,SIC07_description,value,regional_percent, LQ) %>% 
  arrange(year) %>% 
  print(n = 25)


#Chained volume 20 sectors
itl2.gvaperjob %>% 
  filter(
    grepl(x = ITL_region_name, pattern = 'South Y'),
    grepl(x = SIC07_description, pattern = 'Information')
    # grepl(x = SIC07_description, pattern = 'Manuf')
  ) %>% 
  print(n = 25)


itl2.gvaperjob %>% filter(
  grepl(x = ITL_region_name, pattern = 'South Y'),
  # grepl(x = SIC07_description, pattern = 'Information')
  grepl(x = SIC07_description, pattern = 'health', ignore.case=T)) %>% 
  select(year,SIC07_description,gva) %>% 
  arrange(year) %>% 
  print(n = 25)



#Check these numbers from PfG, think they're wrong
#"There are over 1,800 businesses in the health sector in South Yorkshire, which has nearly 90,000 employees, making it greater as a proportion of workers than that in Greater Cambridge, West Yorkshire, London, and Oxfordshire. In 2019, it represented £3.34 billion (12.3%) of GVA – considerably higher than the national share (8.6%)."

#Chained volume for money amount (is in 2019 prices), current prices for %.
#CP for national share. Which I can pull out separately right?

#Get national level current prices
itl1.cp <- read_csv('data/sectors/Table 1c ITL1 UK current price estimates pounds million.csv')

names(itl1.cp) <- gsub(x = names(itl1.cp), pattern = ' ', replacement = '_')

#Use different df to get the twenty sections easily (not the same match with this one, other spaces)
cvSICkeeps <- itl2.cv$SIC07_code[substr(itl2.cv$SIC07_code,2,2) == ' '] %>% unique

#Filter out duplicate value rows and make long by year
#Also convert year to numeric
itl1.cp <- itl1.cp %>% 
  filter(SIC07_code %in% cvSICkeeps, ITL_region_name == 'United Kingdom') %>% 
  pivot_longer(`1998`:`2021`, names_to = 'year', values_to = 'value') %>% 
  mutate(year = as.numeric(year))

#Get by year percentages of whole
itl1.cp <- itl1.cp %>% 
  group_by(year) %>% 
  mutate(sector_percent = (value / sum(value)) * 100)

#Sanity check. Tick.
itl1.cp %>% 
  group_by(year) %>% 
  summarise(sum(sector_percent))

#So, health in 2019 percent
itl1.cp %>% filter(year == 2019, grepl('health',SIC07_description,ignore.case = T)) %>% select(sector_percent) %>% pull


#Then, confirm health % in current prices in South Yorkshire
itl2.cps %>% filter(
  # grepl(x = ITL_region_name, pattern = 'Manc'),
  grepl(x = ITL_region_name, pattern = 'South Y'),
  # year == 2021
  year == 2019
) %>% 
  mutate(regional_percent = sector_regional_proportion *100) %>% 
  select(SIC07_description,regional_percent, LQ) %>% 
  arrange(-regional_percent) %>% #stop here to get full list
  filter(grepl('health',SIC07_description,ignore.case = T)) %>% select(regional_percent) %>% pull



#Then, health money amount in chained volume measure
itl2.cvs %>% 
  filter(year == 2019, grepl('health',SIC07_description,ignore.case = T), grepl('south y',ITL_region_name,ignore.case = T)) %>% 
  select(value) %>% pull
  



#Make a little current size of economy plot
#Let's try to get jobs and GVA proportion for rest of UK vs SY in there
#For some key sectors

#Getting LQs for 20 sections
place = 'South Yorkshire'

p <- twod_proportionplot(
  df = itl2.cps,
  x_regionnames = place, 
  y_regionnames = unique(itl2.cps$ITL_region_name[itl2.cps$ITL_region_name != place]),
  regionvar = ITL_region_name,
  category_var = SIC07_description, 
  valuevar = value, 
  timevar = year, 
  start_time = 2015, 
  end_time = 2021 
  # compasspoints_to_display = c('SE','NE')
)

#add some extras
p <- p + 
  xlab(paste0(place, ' GVA proportion')) +
  ylab(paste0('UK GVA proportion (MINUS ',place,')')) 
  # coord_fixed(xlim = c(0.1,12), ylim = c(0.1,12)) +  # good for log scale
  # scale_y_log10() +
  # scale_x_log10()

p


#Get data from that to make a simpler plot and add in employment numbers
d <- return_compasspoints(
  df = itl2.cps,
  x_regionnames = place, 
  y_regionnames = unique(itl2.cps$ITL_region_name[itl2.cps$ITL_region_name != place]),
  regionvar = ITL_region_name,
  category_var = SIC07_description, 
  valuevar = value, 
  timevar = year, 
  start_time = 2015, 
  end_time = 2021 
  # compasspoints_to_display = c('SE','NE')
)

d <- d %>% 
  left_join(
    itl2.gvaperjob %>% filter(ITL_region_name == place, year == 2021) %>% select(SIC07_description, jobcount),#This has job counts in
    by = 'SIC07_description'
  ) %>% filter(SIC07_description!='Activities of households') %>% ungroup() %>% 
  mutate(
    x_sector_percent = x_sector_total_proportion * 100,
    y_sector_percent = y_sector_total_proportion * 100,
    INDUSTRY_NAME_REDUCED = gsub(x = SIC07_description, pattern = 'of |and |activities|equipment|products', replacement = '')
  )


#Plot subset
ggplot(d %>% filter(x_sector_percent > 4, SIC07_description!='Real estate activities'), aes(x = x_sector_percent, y = y_sector_percent, size = jobcount)) +
  geom_point() +
  geom_abline(slope = 1, size = 1, colour='blue', alpha = 0.5) +
  coord_fixed(xlim = c(4,12.6), ylim = c(4,12.6)) +
  geom_text_repel(
    aes(label = paste0(INDUSTRY_NAME_REDUCED, "\n(",jobcount," jobs)")),
    # aes(label = paste0(SIC07_description, "\n(",round(x_sector_total_proportion * 100,2),"%,",round(y_sector_total_proportion * 100,2),"%)")),
    alpha=1,
    nudge_x = .05,
    box.padding = 1,
    nudge_y = 0.05,
    segment.curvature = -0.1,
    segment.ncp = 0.3,
    segment.angle = 20,
    max.overlaps = 20,
    size = 4
    ) +
  scale_size(range = c(2,10)) +
  guides(size = F) +
  xlab(paste0(place, ' GVA percentage')) +
  ylab(paste0('UK GVA percentage (MINUS ',place,')')) +
  theme_bw()
  













