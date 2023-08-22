#Regional GVA per sector explore
library(tidyverse)
library(zoo)
library(sf)
library(tmap)
library(plotly)
library(magick)
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

ggplot(
  itl2.hl %>% filter(
    `ITL region name` %in% unique(itl2.hl$`ITL region name`)[1:5]
    ),
  aes(x = year, y = value, colour = `SIC07 code`, group = `SIC07 code`)
) +
  geom_line() +
  facet_wrap(~`ITL region name`) +
  # scale_y_log10() +
  guides(colour = "none")



#PLOTLY VERSION
#For hovering to easily see the sector names
plot_ly(data = itl2.hl %>% filter(`ITL region name`=="South Yorkshire"), x = ~year, y = ~value, color = ~`SIC07 code`,
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

#Using 'current prices' - LQs are purely proportional at single time points
#So across-time comparisons only matter for the proportions, not the nominal values
#Given that - the GVA current price values actually sum correctly across industries and within regions (unlike volume-chained)
itl2.cp <- read_csv('data/sectors/Table 2c ITL2 UK current price estimates pounds million.csv')

#Filter out duplicate value rows and make long by year
itl2.cp <- itl2.cp %>% filter(!`SIC07 code` %in% SICremoves) %>% 
  pivot_longer(`1998`:`2021`, names_to = 'year', values_to = 'value')

#RANDOM NEGATIVE VALUES IN THERE
#Looking, I think just typos given previous data
#(And also makes no logical sense, so...)
itl2.cp %>% filter(value < 0)

#Check on just water and air transport, where the neg values are
itl2.cp %>% filter(`SIC07 description` == "Water and air transport") %>% View

#Hmm. Will come back to that.

#check sums just for one year
#that is, make sure kept single categories add up to the correct total
chk <- itl2.cp %>% 
  filter(year == 2021) %>% 
  group_by(`ITL region name`) %>% 
  summarise(mytotal = sum(value))

#Check against totals in orig
chk2 <- read_csv('data/sectors/Table 2c ITL2 UK current price estimates pounds million.csv') %>% 
  filter(`SIC07 code` == 'Total') %>% 
  pivot_longer(`1998`:`2021`, names_to = 'year', values_to = 'originaltotal') %>% 
  filter(year == 2021) %>% 
  arrange(`ITL region name`)

#Rounding error diffs I think, all fine
#Use totals here for proportions so they're accurate / sum to 1
chk <- chk %>% 
  left_join(chk2, by = "ITL region name") %>% 
  mutate(diff = originaltotal-mytotal)


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
    region_totalsize = sum(value),#a. Current price per region per year, for regional denominator
    sector_regional_proportion = value / region_totalsize#b. regional sector proportion (noting that a single row in this group is a single sector)
    ) %>% 
  group_by(year, `SIC07 code`) %>% 
  mutate(
    uk_sectorsize = sum(value),#c. Summed current prices for EACH SECTOR, UK-wide
    ) %>% 
  group_by(year) %>% 
  mutate(
    uk_totalsize = sum(value),#d. Summed current prices for WHOLE UK per year, for UK denominator
    uk_regional_proportion = uk_sectorsize / uk_totalsize#e. UK-level sector proportion
    ) %>% 
  mutate(
    LQ = sector_regional_proportion / uk_regional_proportion#f. Location quotient!
  )
  
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



#ORIGNAL:

#Just check range and means. Means should be 1ish...
#Testing +1 and log
#+1 to get rid of <0 values
#Why? Because the values are otherwise asymmetrical either side of 1
x <- itl2.cp %>% 
  filter(year==2021) %>% 
  mutate(LQplusone_log = log(LQ + 1)) %>% 
  group_by(`SIC07 description`) %>% 
  summarise(
    mean = mean(LQ), min = min(LQ), max = max(LQ),
    mean_log = mean(LQplusone_log), min_log = min(LQplusone_log), max_log = max(LQplusone_log)
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
itl2.cp <- itl2.cp %>% 
  mutate(LQplusone_log = log(LQ + 1)) 




#View for 2021. Can just flag one we want to overlay?
#Something odd with water and air transport, remove until work out what

place = 'South Yorkshire'

x <- itl2.cp %>% filter(year == 2021, `SIC07 description` != 'Water and air transport') %>% mutate(flaggedplace = `ITL region name`==place)

#Ordering by the flagged place, bit awkward
x$`SIC07 description` <- factor(x$`SIC07 description`)
x$`SIC07 description` <- fct_relevel(
  x$`SIC07 description`, 
  unique(as.character(x$`SIC07 description`))[order(x %>% filter(`ITL region name`==place) %>%ungroup() %>% select(LQplusone_log) %>% pull(),decreasing = T)]
  )

#Actually, keep that order and use for animation below
ordertouse <- unique(as.character(x$`SIC07 description`))[order(x %>% filter(`ITL region name`==place) %>%ungroup() %>% select(LQplusone_log) %>% pull(),decreasing = T)]


ggplot(
  x, 
  aes(y = `SIC07 description`, x = LQplusone_log, shape = flaggedplace, alpha = flaggedplace, size = flaggedplace, colour = flaggedplace)
  ) +
  geom_point() +
  scale_size_manual(values = c(2,4)) +
  scale_alpha_manual(values = c(0.2,1)) +
  scale_colour_manual(values = c('black','red')) +
  geom_vline(xintercept = log(2), colour = 'blue') 
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
         # yaxis = list(title = "Value", type='log'),
         yaxis = list(title = "Value"),
         showlegend = TRUE)



#Location quotient vs proportion in the region (already calculated) seems obvious...
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



#Version comparing more than one place. Can we link related points?
x <- itl2.cp %>% filter(`ITL region name` %in% c('South Yorkshire','Greater Manchester')) %>% ungroup() %>% 
  filter(`SIC07 description`!='Water and air transport') 
x <- itl2.cp %>% filter(`ITL region name` %in% c('Leicestershire, Rutland and Northamptonshire','Greater Manchester')) %>% ungroup() %>% 
  filter(`SIC07 description`!='Water and air transport') 
# %>% #negative values
#   group_by(`SIC07 description`)

plot_ly(data = x %>% filter(year==2021), x = ~LQ, y = ~sector_regional_proportion, color = ~`SIC07 description`, split =~`SIC07 description`,
        # symbol =~`ITL region name`,
        text = ~paste("Place: ",`ITL region name`,"\nSector:", `SIC07 description`, "\npercent: ", sector_regional_proportion * 100, "\nvalue: ", value),  # Add this line for hover text
        hoverinfo = 'text+y+x',
        type = 'scatter',
        mode = 'lines+markers', line = list(shape = 'linear')) %>%
  layout(title = "Yearly values by SIC", 
         xaxis = list(title = "LQ"),
         # xaxis = list(title = "LQ", type = 'log'),
         yaxis = list(title = "Region proportion"),
         # yaxis = list(title = "Region proportion", type = 'log'),
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








p <- plot_ly(data = x, 
             x = ~LQ, y = ~sector_regional_proportion, color = ~`ITL region name`,
             type = 'scatter', 
             mode = 'markers',
             marker = list(size = 10)) %>%
  layout(showlegend = T)

# Add lines connecting corresponding points between Group A and Group B
for(i in 1:(nrow(x) / 2)) {
  # Subset data for the two groups of the current row
  subset_df <- df[df$id == i, ]
  
  p <- p %>%
    add_trace(
      x = subset_df$variable1,
      y = subset_df$variable2,
      type = 'scatter',
      mode = 'lines',
      line = list(color = 'grey', width = 2),
      showlegend = F
    )
}





# Sample data
df <- data.frame(
  id = 1:4,  # to uniquely identify rows/observations
  group = c(rep("A", 2), rep("B", 2)),
  variable1 = c(5, 7, 8, 6),
  variable2 = c(6, 8, 7, 9)
)

p <- plot_ly(data = df, 
             x = ~variable1, 
             y = ~variable2, 
             color = ~factor(group),
             type = 'scatter', 
             mode = 'markers',
             marker = list(size = 10)) %>%
  layout(showlegend = T)

# Add lines connecting corresponding points between Group A and Group B
for(i in 1:(nrow(df) / 2)) {
  # Subset data for the two groups of the current row
  subset_df <- df[df$id == i, ]
  
  p <- p %>%
    add_trace(
      x = subset_df$variable1,
      y = subset_df$variable2,
      type = 'scatter',
      mode = 'lines',
      line = list(color = 'grey', width = 2),
      showlegend = F
    )
}

p













