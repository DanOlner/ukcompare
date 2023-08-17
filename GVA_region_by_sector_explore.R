#Regional GVA per sector explore
library(tidyverse)
library(sf)
library(tmap)
library(plotly)

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
#Note: "real estate activities" is a single category, but there are two subcats that break it down into
#With and without imputed rental. So just need those two to avoid double counting.
SICremoves = c(
  'Total',
  'A-E',
  'A (1-3)',
  'B (5-9)',
  'C (10-33)',
  'CA (10-12)',
  'CB (13-15)',
  'CC (16-18)',
  'CD-CE (19-20)',
  'CF (21)',
  'CG (22-23)',
  'CH (24-25)',
  'CL (29-30)',
  'CM (31-33)',
  'E (36-39)',
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
  'S (94-96)',
  'T (97-98)'
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
         showlegend = TRUE)













