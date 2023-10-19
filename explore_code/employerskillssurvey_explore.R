#Employer skills survey 2022 explore
#Downloaded from
#https://explore-education-statistics.service.gov.uk/find-statistics/employer-skills-survey/2022
library(tidyverse)

emp <- read_csv('local/data/employerskillssurvey2022/employer-skills-survey_2022/data/mayoral_authority_core_indicators_22.csv')

#SY in as Sheffield City Region
table(emp$english_devolved_area_name)

#filter out empty rows... can't find actual codes for the letters used
empsub <- emp %>% 
  filter(weight_base!='u')

#For SCR, how many sectors?
scr <- empsub %>% filter(english_devolved_area_name=='Sheffield City Region')

table(scr$sector)

#data guidance I think says we can filter like this...
scr.tots <- scr %>% filter(
  estab_size == 'Total',
  single_or_multi_site == 'Total',
  estab_type == 'Total')

#Same number of sectors in both
length(unique(scr$sector)) == length(unique(scr.tots$sector))

