#Employer skills survey 2022 explore
#Downloaded from
#https://explore-education-statistics.service.gov.uk/find-statistics/employer-skills-survey/2022
library(tidyverse)

#It says it's mayoral authority, it lies!!
emp <- read_csv('local/data/employerskillssurvey2022/employer-skills-survey_2022/data/mayoral_authority_core_indicators_22.csv')

#filter out empty rows... can't find actual codes for the letters used
empsub <- emp %>% 
  filter(weight_base!='u')
