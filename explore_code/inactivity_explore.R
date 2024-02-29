#inactivity explore
library(tidyverse)
library(zoo)
library(sf)
library(tmap)
library(plotly)
library(magick)
library(nomisr)
library(pryr)
library(ggrepel)
library(ggmosaic)
options(scipen = 99)

#Census download----

##Get inactivity table----

#https://www.nomisweb.co.uk/datasets/c2021ts066
a <- nomis_search(name = '*TS066*')
atts <- a %>% tidyr::unnest(components.attribute) %>% glimpse()
unique(atts$name.value)

#Will have to be local authorities again
print(nomis_get_metadata(id = "NM_2083_1", concept = "geography", type = "type"), n = 60)
print(nomis_get_metadata(id = "NM_2083_1", concept = "geography", type = "TYPE423"), n = 2500)

#Get IDs for four places again
a <- nomis_get_metadata(id = "NM_2083_1", concept = "geography", type = "TYPE423")
ids <- a$id[grepl('sheff|rother|barnsley|doncaster', a$label.en, ignore.case = T)]

econ_active <- nomis_get_data(id = "NM_2083_1", time = "latest", geography = ids)

unique(econ_active$C2021_EASTAT_20_NAME)


#Get all local authorities, to find which has the min econ active in certain categories
ALL.LAS.econ_active <- nomis_get_data(id = "NM_2083_1", time = "latest", geography = "TYPE423")



#Get England and Wales version of the same
EW.econ_active <- nomis_get_data(id = "NM_2083_1", time = "latest", geography = '2092957703')



##Get inactivity  / health / care crosstab----

#https://www.nomisweb.co.uk/datasets/c2021rm022
a <- nomis_search(name = '*RM022*')
atts <- a %>% tidyr::unnest(components.attribute) %>% glimpse()
unique(atts$name.value)

#Will have to be local authorities again
print(nomis_get_metadata(id = "NM_2122_1", concept = "geography", type = "type"), n = 60)
print(nomis_get_metadata(id = "NM_2122_1", concept = "geography", type = "TYPE423"), n = 2500)

#Get IDs for four places again
a <- nomis_get_metadata(id = "NM_2122_1", concept = "geography", type = "TYPE423")
ids <- a$id[grepl('sheff|rother|barnsley|doncaster', a$label.en, ignore.case = T)]

activity_crosstab <- nomis_get_data(id = "NM_2122_1", time = "latest", geography = ids)



##Get inactivity / age crosstab----

#https://www.nomisweb.co.uk/datasets/c2021rm024
a <- nomis_search(name = '*RM024*')
atts <- a %>% tidyr::unnest(components.attribute) %>% glimpse()
unique(atts$name.value)

#Will have to be local authorities again
print(nomis_get_metadata(id = "NM_2124_1", concept = "geography", type = "type"), n = 60)
print(nomis_get_metadata(id = "NM_2124_1", concept = "geography", type = "TYPE423"), n = 2500)

#Get IDs for four places again
a <- nomis_get_metadata(id = "NM_2124_1", concept = "geography", type = "TYPE423")
ids <- a$id[grepl('sheff|rother|barnsley|doncaster', a$label.en, ignore.case = T)]

activity_age_crosstab <- nomis_get_data(id = "NM_2124_1", time = "latest", geography = ids)



#Repeat to get England total numbers... can compare to England and Wales
print(nomis_get_metadata(id = "NM_2124_1", concept = "geography", type = "TYPE499"), n = 2500)

EW.activity_age_crosstab <- nomis_get_data(id = "NM_2124_1", time = "latest", geography = '2092957703')



#Get all local authorities, to find which has the min econ active in certain categories
ALL.LAS.econ_active_age_crosstab <- nomis_get_data(id = "NM_2124_1", time = "latest", geography = "TYPE423")




# INACTIVITY TABLE----

unique(econ_active$C2021_EASTAT_20_NAME)

#Quick look at pattern across the four places
inactivity <- econ_active %>% 
  filter(
    # C2021_EASTAT_20_NAME!='Total: All usual residents aged 16 years and over', 
    MEASURES_NAME == 'Value',
    grepl('inactive', C2021_EASTAT_20_NAME, ignore.case = T),
    grepl('retired|student|home|disabled|other', C2021_EASTAT_20_NAME, ignore.case = T)
    )


ggplot(inactivity, aes(x = GEOGRAPHY_NAME, y = OBS_VALUE, fill = C2021_EASTAT_20_NAME)) +
  geom_bar(position = 'fill', stat = 'identity') +
  coord_flip() +
  scale_fill_brewer(palette = 'Paired')

#Non fill version to see actual scale in each category
ggplot(inactivity, aes(x = GEOGRAPHY_NAME, y = OBS_VALUE, fill = C2021_EASTAT_20_NAME)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  scale_fill_brewer(palette = 'Paired')



#Look at percents of whole, already in the data
# View(econ_active %>% filter(MEASURES_NAME == 'Percent'))


inactivity_percents <- econ_active %>% 
  filter(
    # C2021_EASTAT_20_NAME!='Total: All usual residents aged 16 years and over', 
    MEASURES_NAME == 'Percent',
    grepl('inactive', C2021_EASTAT_20_NAME, ignore.case = T),
    grepl('retired|student|home|disabled|other', C2021_EASTAT_20_NAME, ignore.case = T)
  )


#Add in england/wales percents
inactivity_percents <- inactivity_percents %>% 
  rbind(
    EW.econ_active %>% filter(
      # C2021_EASTAT_20_NAME!='Total: All usual residents aged 16 years and over', 
      MEASURES_NAME == 'Percent',
      grepl('inactive', C2021_EASTAT_20_NAME, ignore.case = T),
      grepl('retired|student|home|disabled|other', C2021_EASTAT_20_NAME, ignore.case = T)
    )
  ) %>% 
  mutate(GEOGRAPHY_NAME = factor(GEOGRAPHY_NAME, ordered = T, levels = c('Sheffield','Rotherham','Doncaster','Barnsley','England and Wales')))


ggplot(inactivity_percents, aes(x = fct_rev(GEOGRAPHY_NAME), y = OBS_VALUE, fill = C2021_EASTAT_20_NAME)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  scale_fill_brewer(palette = 'Paired')


#Same minus retired
ggplot(inactivity_percents %>% filter(C2021_EASTAT_20_NAME!='Economically inactive: Retired'), aes(x = GEOGRAPHY_NAME, y = OBS_VALUE, fill = C2021_EASTAT_20_NAME)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  scale_fill_brewer(palette = 'Paired')


#Dodge version (will work given %s)
#Minus retired
ggplot(inactivity_percents %>% filter(C2021_EASTAT_20_NAME!='Economically inactive: Retired'), aes(fill = GEOGRAPHY_NAME, y = OBS_VALUE, x = C2021_EASTAT_20_NAME)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  coord_flip() +
  scale_fill_brewer(palette = 'Paired')

#With retired
ggplot(inactivity_percents, aes(fill = GEOGRAPHY_NAME, y = OBS_VALUE, x = C2021_EASTAT_20_NAME)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  coord_flip() +
  scale_fill_brewer(palette = 'Paired')


#WANT THOSE NUMBERS FOR SY. So will need to use totals and then recalculate the percentages





#Check key econ active mins for all local authorities. What's it look like?
unique(ALL.LAS.econ_active$C2021_EASTAT_20_NAME)

LLTIs <- ALL.LAS.econ_active %>% 
  filter(
    MEASURES_NAME=='Percent',
    C2021_EASTAT_20_NAME=='Economically inactive: Long-term sick or disabled'
  )

ggplot(LLTIs, aes(x = OBS_VALUE)) +
  geom_histogram(binwidth = 0.5) +
  geom_vline(
    data  = LLTIs %>% filter(grepl('sheff|rother|barnsley|doncaster', GEOGRAPHY_NAME, ignore.case = T)),
    aes(xintercept = OBS_VALUE, colour = GEOGRAPHY_NAME)
    )

INACTIVE.OTHER <- ALL.LAS.econ_active %>% 
  filter(
    MEASURES_NAME=='Percent',
    C2021_EASTAT_20_NAME=='Economically inactive: Other'
  )

ggplot(INACTIVE.OTHER, aes(x = OBS_VALUE)) +
  geom_histogram(binwidth = 0.5) +
  geom_vline(
    data  = INACTIVE.OTHER %>% filter(grepl('sheff|rother|barnsley|doncaster', GEOGRAPHY_NAME, ignore.case = T)),
    aes(xintercept = OBS_VALUE, colour = GEOGRAPHY_NAME)
  )


LLTIs_n_other <- ALL.LAS.econ_active %>% 
  filter(
    MEASURES_NAME=='Percent',
    C2021_EASTAT_20_NAME %in% c('Economically inactive: Long-term sick or disabled','Economically inactive: Other','Economically inactive: Retired','Economically inactive: Student','Economically inactive: Looking after home or family')
  )

ggplot(LLTIs_n_other, aes(x = OBS_VALUE)) +
  geom_density(fill = 'grey', alpha = 0.5) +
  geom_vline(
    data  = LLTIs_n_other %>% filter(grepl('sheff|rother|barnsley|doncaster', GEOGRAPHY_NAME, ignore.case = T)),
    aes(xintercept = OBS_VALUE, colour = GEOGRAPHY_NAME)
  ) +
  stat_summary(aes(xintercept = ..x.., y = 0), fun = mean, geom = "vline", orientation = "y", size = 2, alpha = 0.25) +#https://stackoverflow.com/a/7318539
  facet_wrap(~C2021_EASTAT_20_NAME, scales = 'free')



#Work out bespoke percentages for counts not including full time students
unique(ALL.LAS.econ_active$C2021_EASTAT_20_NAME)

ALL.LAS.econ_active.nostudents <- ALL.LAS.econ_active %>% 
  select(GEOGRAPHY_NAME,C2021_EASTAT_20_NAME,MEASURES_NAME,OBS_VALUE) %>% 
  # filter(MEASURES_NAME == 'Value') %>% 
  pivot_wider(names_from = MEASURES_NAME, values_from = OBS_VALUE) %>% #keep orig percentages for comparison of change
  rename(orig_percent = Percent) %>% 
  filter(
    C2021_EASTAT_20_NAME %in% c(
      'Economically active (excluding full-time students):In employment',
      'Economically active (excluding full-time students): Unemployed',
      'Economically inactive: Retired',
      'Economically inactive: Looking after home or family',
      'Economically inactive: Long-term sick or disabled',
      'Economically inactive: Other'
      )
  ) %>% 
  group_by(GEOGRAPHY_NAME) %>% 
  mutate(TOTAL_MINUS_STUDENTS = sum(Value)) %>% 
  ungroup() %>% 
  mutate(PERCENT_MINUS_STUDENTS = (Value/TOTAL_MINUS_STUDENTS)*100)
  
#Check percent comparisons... is also places with highest student proportions (Notts at top?)
#Seems too high for Notts?
ALL.LAS.econ_active.nostudents %>% 
  group_by(GEOGRAPHY_NAME) %>% 
  summarise(
    orig = sum(orig_percent),
    new = sum(PERCENT_MINUS_STUDENTS)
  ) %>% 
  arrange(orig) %>% 
  print(n = 50)


#Check... yup, apparently student numbers correct. Google said 1 in 8 for Notts, not in Census. COVID effect?
ALL.LAS.econ_active %>% 
  filter(
    MEASURES_NAME == 'Percent',
    C2021_EASTAT_20_NAME %in% c('Economically active and a full-time student','Economically inactive: Student')
    ) %>% 
  group_by(GEOGRAPHY_NAME) %>% 
  summarise(percenttotal = sum(OBS_VALUE)) %>% 
  arrange(-percenttotal) %>% 
  print(n=50)


#Repeat same plot as above now, excluding full time students
ggplot(ALL.LAS.econ_active.nostudents, aes(x = PERCENT_MINUS_STUDENTS)) +
  geom_density(fill = 'grey', alpha = 0.5) +
  geom_vline(
    data  = ALL.LAS.econ_active.nostudents %>% filter(grepl('sheff|rother|barnsley|doncaster', GEOGRAPHY_NAME, ignore.case = T)),
    aes(xintercept = PERCENT_MINUS_STUDENTS, colour = GEOGRAPHY_NAME)
  ) +
  stat_summary(aes(xintercept = ..x.., y = 0), fun = mean, geom = "vline", orientation = "y", size = 2, alpha = 0.25) +#https://stackoverflow.com/a/7318539
  facet_wrap(~C2021_EASTAT_20_NAME, scales = 'free')



#REPEAT TO JUST CHECK PROPORTION OF EMPLOYED VS (ECON ACTIVE) UNEMPLOYED
#Because includin inactive makes it look like SY has lower unemployment
#That's I think just an artifact of higher econ inactive percentages
#Checked with age groups below, true there
ALL.LAS.econ_active.nostudents.chk <- ALL.LAS.econ_active %>% 
  select(GEOGRAPHY_NAME,C2021_EASTAT_20_NAME,MEASURES_NAME,OBS_VALUE) %>% 
  # filter(MEASURES_NAME == 'Value') %>% 
  pivot_wider(names_from = MEASURES_NAME, values_from = OBS_VALUE) %>% #keep orig percentages for comparison of change
  rename(orig_percent = Percent) %>% 
  filter(
    C2021_EASTAT_20_NAME %in% c(
      'Economically active (excluding full-time students):In employment',
      'Economically active (excluding full-time students): Unemployed'
    )
  ) %>% 
  group_by(GEOGRAPHY_NAME) %>% 
  mutate(TOTAL_MINUS_STUDENTS = sum(Value)) %>% 
  ungroup() %>% 
  mutate(PERCENT_MINUS_STUDENTS = (Value/TOTAL_MINUS_STUDENTS)*100)

#Pretty middling actually, and again Barnsley looking pretty good on this
ggplot(ALL.LAS.econ_active.nostudents.chk, aes(x = PERCENT_MINUS_STUDENTS)) +
  geom_density(fill = 'grey', alpha = 0.5) +
  geom_vline(
    data  = ALL.LAS.econ_active.nostudents.chk %>% filter(grepl('sheff|rother|barnsley|doncaster', GEOGRAPHY_NAME, ignore.case = T)),
    aes(xintercept = PERCENT_MINUS_STUDENTS, colour = GEOGRAPHY_NAME)
  ) +
  stat_summary(aes(xintercept = ..x.., y = 0), fun = mean, geom = "vline", orientation = "y", size = 2, alpha = 0.25) +#https://stackoverflow.com/a/7318539
  facet_wrap(~C2021_EASTAT_20_NAME, scales = 'free')




#NOW SUM TOTALS FOR SOUTH YORKSHIRE AND CALCULATE %S AGAIN
#Can just calculate and then append to the dfs above
sy <- ALL.LAS.econ_active %>% 
  select(GEOGRAPHY_NAME,C2021_EASTAT_20_NAME,MEASURES_NAME,OBS_VALUE) %>% 
  pivot_wider(names_from = MEASURES_NAME, values_from = OBS_VALUE) %>% #keep orig percentages for comparison of change
  rename(orig_percent = Percent) %>% 
  filter(
    C2021_EASTAT_20_NAME %in% c(
      'Economically active (excluding full-time students):In employment',
      'Economically active (excluding full-time students): Unemployed',
      'Economically inactive: Retired',
      'Economically inactive: Looking after home or family',
      'Economically inactive: Long-term sick or disabled',
      'Economically inactive: Other'
    ),
    grepl('sheff|rother|barnsley|doncaster', GEOGRAPHY_NAME, ignore.case = T)
  ) %>% 
  group_by(C2021_EASTAT_20_NAME) %>% #sum values across all four places
  summarise(Value = sum(Value)) %>% 
  mutate(GEOGRAPHY_NAME = 'South Yorkshire') %>% 
  ungroup() %>% 
  mutate(PERCENT_MINUS_STUDENTS = (Value/sum(Value))*100)


#Exclude the four places from the original so means aren't skewed, and combine
SY.plus.ALL.LAS.econ_active.nostudents <- 
  ALL.LAS.econ_active.nostudents %>% 
  select(-orig_percent,-TOTAL_MINUS_STUDENTS) %>% 
  filter(!grepl('sheff|rother|barnsley|doncaster', GEOGRAPHY_NAME, ignore.case = T)) %>% 
  rbind(sy)


ggplot(SY.plus.ALL.LAS.econ_active.nostudents, aes(x = PERCENT_MINUS_STUDENTS)) +
  geom_density(fill = 'grey', alpha = 0.5) +
  geom_vline(
    data  = SY.plus.ALL.LAS.econ_active.nostudents %>% filter(GEOGRAPHY_NAME=='South Yorkshire'),
    aes(xintercept = PERCENT_MINUS_STUDENTS, colour = GEOGRAPHY_NAME)
  ) +
  stat_summary(aes(xintercept = ..x.., y = 0), fun = mean, geom = "vline", orientation = "y", size = 2, alpha = 0.25) +#https://stackoverflow.com/a/7318539
  facet_wrap(~C2021_EASTAT_20_NAME, scales = 'free')







# INACTIVITY + CARE CROSSTAB----

unique(activity_crosstab$C2021_EASTAT_10_NAME)
unique(activity_crosstab$C2021_HEALTH_3_NAME)
unique(activity_crosstab$C2021_CARER_5_NAME)

#For inactive, what's the care amount breakdowns?
#Keeping only 'health good / bad' total, we only want inactivity + care cats crosstab
activity_crosstab.care <- activity_crosstab %>% 
  filter(
    grepl('inactive', C2021_EASTAT_10_NAME, ignore.case = T),
    grepl('retired|student|home|disabled|other', C2021_EASTAT_10_NAME, ignore.case = T),
    C2021_HEALTH_3_NAME=='Total',
    C2021_CARER_5_NAME!='Total',
    
  )



ggplot(activity_crosstab.care, aes(x = C2021_CARER_5_NAME, y = OBS_VALUE, fill = C2021_EASTAT_10_NAME)) +
  geom_bar(position = 'fill', stat = 'identity') +
  facet_wrap(~GEOGRAPHY_NAME, nrow = 1) +
  coord_flip() +
  scale_fill_brewer(palette = 'Paired')


#Non fill version to see actual scale in each category
#Mostly 'provides no unpaid care' but those might add up, so...
ggplot(activity_crosstab.care, aes(x = C2021_CARER_5_NAME, y = OBS_VALUE, fill = C2021_EASTAT_10_NAME)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~GEOGRAPHY_NAME, nrow = 1, scales = 'free_x') +
  coord_flip() +
  scale_fill_brewer(palette = 'Paired')






# INACTIVITY + AGE CROSSTAB----

#Annoying, don't have age / retired. How many 'retired' are due to ill health or something else?
unique(activity_age_crosstab$C2021_EASTAT_7_NAME)
unique(activity_age_crosstab$C2021_AGE_7_NAME)


activity_age_crosstab.nt <- activity_age_crosstab %>% 
  filter(
    C_SEX_NAME == 'All persons',
    C2021_EASTAT_7_NAME!='Total',
    C2021_AGE_7_NAME!='Total'
  )



ggplot(activity_age_crosstab.nt, aes(x = C2021_AGE_7_NAME, y = OBS_VALUE, fill = C2021_EASTAT_7_NAME)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~GEOGRAPHY_NAME, nrow = 1, scales = 'free_x') +
  coord_flip() +
  scale_fill_brewer(palette = 'Paired')

ggplot(activity_age_crosstab.nt, aes(fill = C2021_AGE_7_NAME, y = OBS_VALUE, x = C2021_EASTAT_7_NAME)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~GEOGRAPHY_NAME, nrow = 1, scales = 'free_x') +
  coord_flip() +
  scale_fill_brewer(palette = 'Paired')



#Can we just look at the age profile plz?
age <- activity_age_crosstab %>% 
  filter(
    C_SEX_NAME == 'All persons',
    C2021_EASTAT_7_NAME=='Total',
    C2021_AGE_7_NAME!='Total'
  )


#Add in row for England and Wales for comparison
ew.ageonly <- EW.activity_age_crosstab %>% 
  filter(
    C_SEX_NAME == 'All persons',
    C2021_EASTAT_7_NAME=='Total',
    C2021_AGE_7_NAME!='Total'
  )

age <- rbind(age,ew.ageonly)

age <- age %>% 
  mutate(GEOGRAPHY_NAME = factor(GEOGRAPHY_NAME, ordered = T, levels = c('Sheffield','Rotherham','Doncaster','Barnsley','England and Wales')))

ggplot(age, aes(fill = C2021_AGE_7_NAME, y = OBS_VALUE, x = fct_rev(GEOGRAPHY_NAME))) +
  geom_bar(stat = 'identity', position = 'fill') +
  coord_flip() +
  scale_fill_brewer(palette = 'Paired')

#Might be better dodged? Oh but needs to be %s if so given Eng / Wales
ggplot(age, aes(fill = C2021_AGE_7_NAME, y = OBS_VALUE, x = fct_rev(GEOGRAPHY_NAME))) +
  geom_bar(stat = 'identity', position = 'dodge', width = 1) +
  coord_flip() +
  scale_fill_brewer(palette = 'Paired')


#What if we remove students? Will need to then redo totals for those separately?
#Come back to that



#For all places, check where the 4 places sit econ inactivity wise for each age group
#Do they differ?

#Need to calculate percents here, so pull out total into its own column
#WE Want to work out econ active percentages WITHIN EACH AGE BRACKET AS THE DENOM so will need those subtotals

#Non totals, other bins
ALL.LAS.econ_active_age_comp <- ALL.LAS.econ_active_age_crosstab %>% 
  filter(
    C_SEX_NAME=='All persons',
    !C2021_AGE_7_NAME %in% c('Total','Aged 15 years and under'),#15 years and under not in this data, those are zeroes, not inc in %
    C2021_EASTAT_7_NAME!='Total'      
  ) %>% 
  select(GEOGRAPHY_NAME,C2021_AGE_7_NAME,C2021_EASTAT_7_NAME,OBS_VALUE)

#Get correct age group totals and get percentage
ALL.LAS.econ_active_age_comp <- ALL.LAS.econ_active_age_comp %>% 
  group_by(GEOGRAPHY_NAME,C2021_AGE_7_NAME) %>% 
  mutate(
    WITHINAGEGROUP_TOTALS = sum(OBS_VALUE)
    ) %>% 
  ungroup() %>% 
  mutate(WITHINAGEGROUP_PERCENT = (OBS_VALUE/WITHINAGEGROUP_TOTALS) * 100)

#Check that worked... tick
ALL.LAS.econ_active_age_comp %>% 
  group_by(GEOGRAPHY_NAME,C2021_AGE_7_NAME) %>% 
  summarise(sum(WITHINAGEGROUP_PERCENT))


#Plot four places against everywhere else
ggplot(ALL.LAS.econ_active_age_comp, aes(x = WITHINAGEGROUP_PERCENT)) +
  geom_histogram() +
  geom_vline(
    data  = ALL.LAS.econ_active_age_comp %>% filter(grepl('sheff|rother|barnsley|doncaster', GEOGRAPHY_NAME, ignore.case = T)),
    aes(xintercept = WITHINAGEGROUP_PERCENT, colour = GEOGRAPHY_NAME)
  ) +
  facet_wrap(~C2021_AGE_7_NAME+C2021_EASTAT_7_NAME, scales = 'free')

#Pick out some examples to see if that looks sane at all
chk <- ALL.LAS.econ_active_age_comp %>% 
  filter(
    grepl('sheff|rother|barnsley|doncaster|Barnet|Bedford', GEOGRAPHY_NAME, ignore.case = T),
    C2021_AGE_7_NAME == 'Aged 16 to 24 years',
    C2021_EASTAT_7_NAME == 'Economically active (excluding full-time students): Unemployed'
  )






#ANOTHER VERSION WHERE WE REMOVE ALL FULL TIME STUDENTS AND RECALC ALL PERCENTAGES

unique(ALL.LAS.econ_active_age_crosstab$C2021_EASTAT_7_NAME)

ALL.LAS.econ_active_age_comp_nostudents <- ALL.LAS.econ_active_age_crosstab %>% 
  filter(
    C_SEX_NAME=='All persons',
    !C2021_AGE_7_NAME %in% c('Total','Aged 15 years and under'),#15 years and under not in this data, those are zeroes, not inc in %
    !C2021_EASTAT_7_NAME %in% c('Total','Economically active and a full-time student: Unemployed','Economically active and a full-time student: In employment','Economically inactive and a full-time student')      
  ) %>% 
  select(GEOGRAPHY_NAME,C2021_AGE_7_NAME,C2021_EASTAT_7_NAME,OBS_VALUE)

#Get correct age group totals and get percentage
ALL.LAS.econ_active_age_comp_nostudents <- ALL.LAS.econ_active_age_comp_nostudents %>% 
  group_by(GEOGRAPHY_NAME,C2021_AGE_7_NAME) %>% 
  mutate(
    WITHINAGEGROUP_TOTALS = sum(OBS_VALUE)
  ) %>% 
  ungroup() %>% 
  mutate(WITHINAGEGROUP_PERCENT = (OBS_VALUE/WITHINAGEGROUP_TOTALS) * 100)

#Check that worked... tick
ALL.LAS.econ_active_age_comp_nostudents %>% 
  group_by(GEOGRAPHY_NAME,C2021_AGE_7_NAME) %>% 
  summarise(sum(WITHINAGEGROUP_PERCENT))


#Plot four places against everywhere else
ggplot(ALL.LAS.econ_active_age_comp_nostudents, aes(x = WITHINAGEGROUP_PERCENT)) +
  geom_histogram() +
  geom_vline(
    data  = ALL.LAS.econ_active_age_comp_nostudents %>% filter(grepl('sheff|rother|barnsley|doncaster', GEOGRAPHY_NAME, ignore.case = T)),
    aes(xintercept = WITHINAGEGROUP_PERCENT, colour = GEOGRAPHY_NAME)
  ) +
  facet_wrap(~C2021_EASTAT_7_NAME+C2021_AGE_7_NAME, scales = 'free', nrow=3)

#Repeat with density + mean
ggplot(ALL.LAS.econ_active_age_comp_nostudents, aes(x = WITHINAGEGROUP_PERCENT)) +
  geom_density(fill = 'grey', alpha = 0.5) +
  geom_vline(
    data  = ALL.LAS.econ_active_age_comp_nostudents %>% filter(grepl('sheff|rother|barnsley|doncaster', GEOGRAPHY_NAME, ignore.case = T)),
    aes(xintercept = WITHINAGEGROUP_PERCENT, colour = GEOGRAPHY_NAME)
  ) +
  stat_summary(aes(xintercept = ..x.., y = 0), fun = mean, geom = "vline", orientation = "y", size = 2, alpha = 0.25) +#https://stackoverflow.com/a/73185398/5023561
  facet_wrap(~C2021_EASTAT_7_NAME+C2021_AGE_7_NAME, scales = 'free', nrow=3)




#Note: the lower ‘unemployed’ percentages are probably due to econ inactivity outweighing them. Repeat percentage calcs WITHOUT inactive in there, suspect different story.
#Yep - actually, minus econ inactive, SY otherwise employed vs unemployed does better than most places
unique(ALL.LAS.econ_active_age_crosstab$C2021_EASTAT_7_NAME)

ALL.LAS.chk <- ALL.LAS.econ_active_age_crosstab %>% 
  filter(
    C_SEX_NAME=='All persons',
    !C2021_AGE_7_NAME %in% c('Total','Aged 15 years and under'),#15 years and under not in this data, those are zeroes, not inc in %
    !C2021_EASTAT_7_NAME %in% c('Total','Economically active and a full-time student: Unemployed','Economically active and a full-time student: In employment','Economically inactive and a full-time student','Economically inactive (excluding full-time students)')      
  ) %>% 
  select(GEOGRAPHY_NAME,C2021_AGE_7_NAME,C2021_EASTAT_7_NAME,OBS_VALUE)

#Get correct age group totals and get percentage
ALL.LAS.chk <- ALL.LAS.chk %>% 
  group_by(GEOGRAPHY_NAME,C2021_AGE_7_NAME) %>% 
  mutate(
    WITHINAGEGROUP_TOTALS = sum(OBS_VALUE)
  ) %>% 
  ungroup() %>% 
  mutate(WITHINAGEGROUP_PERCENT = (OBS_VALUE/WITHINAGEGROUP_TOTALS) * 100)

#Check that worked... tick
ALL.LAS.chk %>% 
  group_by(GEOGRAPHY_NAME,C2021_AGE_7_NAME) %>% 
  summarise(sum(WITHINAGEGROUP_PERCENT))

ggplot(ALL.LAS.chk, aes(x = WITHINAGEGROUP_PERCENT)) +
  geom_density(fill = 'grey', alpha = 0.5) +
  geom_vline(
    data  = ALL.LAS.chk %>% filter(grepl('sheff|rother|barnsley|doncaster', GEOGRAPHY_NAME, ignore.case = T)),
    aes(xintercept = WITHINAGEGROUP_PERCENT, colour = GEOGRAPHY_NAME)
  ) +
  stat_summary(aes(xintercept = ..x.., y = 0), fun = mean, geom = "vline", orientation = "y", size = 2, alpha = 0.25) +#https://stackoverflow.com/a/73185398/5023561
  facet_wrap(~C2021_EASTAT_7_NAME+C2021_AGE_7_NAME, scales = 'free', nrow=2)





#might also want to consider the following:
#Across each econ_active category, what percentage of age group makes up that category for all the LAs? (So, 90 degrees to what I’ve done above)
#That way, should be able to see demographic differences between places for those
#Tho less useful

#Sticking to no full time students to get %s
# ALL.LAS.across_econ_active_comp_nostudents <- ALL.LAS.econ_active_age_crosstab %>% 
#   filter(
#     C_SEX_NAME=='All persons',
#     !C2021_AGE_7_NAME %in% c('Total','Aged 15 years and under'),#15 years and under not in this data, those are zeroes, not inc in %
#     !C2021_EASTAT_7_NAME %in% c('Total','Economically active and a full-time student: Unemployed','Economically active and a full-time student: In employment','Economically inactive and a full-time student')      
#   ) %>% 
#   select(GEOGRAPHY_NAME,C2021_AGE_7_NAME,C2021_EASTAT_7_NAME,OBS_VALUE)
# 
# #Get correct age group totals and get percentage
# ALL.LAS.across_econ_active_comp_nostudents <- ALL.LAS.across_econ_active_comp_nostudents %>% 
#   group_by(GEOGRAPHY_NAME,C2021_EASTAT_7_NAME) %>% 
#   mutate(
#     WITHIN_EA_TOTALS = sum(OBS_VALUE)
#   ) %>% 
#   ungroup() %>% 
#   mutate(WITHIN_EA_PERCENT = (OBS_VALUE/WITHIN_EA_TOTALS) * 100)
# 
# #Check that worked... tick
# ALL.LAS.across_econ_active_comp_nostudents %>% 
#   group_by(GEOGRAPHY_NAME,C2021_EASTAT_7_NAME) %>% 
#   summarise(sum(WITHIN_EA_PERCENT))
# 
# 
# #Plot four places against everywhere else
# ggplot(ALL.LAS.across_econ_active_comp_nostudents, aes(x = WITHIN_EA_PERCENT)) +
#   geom_histogram() +
#   geom_vline(
#     data  = ALL.LAS.across_econ_active_comp_nostudents %>% filter(grepl('sheff|rother|barnsley|doncaster', GEOGRAPHY_NAME, ignore.case = T)),
#     aes(xintercept = WITHIN_EA_PERCENT, colour = GEOGRAPHY_NAME)
#   ) +
#   facet_wrap(~C2021_EASTAT_7_NAME+C2021_AGE_7_NAME, scales = 'free', nrow=3)
#   
















