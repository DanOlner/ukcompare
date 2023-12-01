#Employer skills survey 2022 explore
#Downloaded from
#https://explore-education-statistics.service.gov.uk/find-statistics/employer-skills-survey/2022
library(tidyverse)

emp <- read_csv('local/data/employer-skills-survey_2022/data/mayoral_authority_core_indicators_22.csv')

#Check against numbers at the national level 
#e.g. that 'weight base' is the theoretical total of firm count in SCR not nationally
empnat <- read_csv('local/data/employer-skills-survey_2022/data/core_indicators_11_22.csv')

unique(empnat$region_name)

#check if region_name NAs are the national numbers
empnat.chk <- empnat %>% filter(is.na(region_name)) %>% 
  filter(
    estab_size == 'Total',
    single_or_multi_site == 'Total',
    estab_type == 'Total')

unique(empnat.chk$country_name)

#SY in as Sheffield City Region
table(emp$english_devolved_area_name)

#filter out empty rows... can't find actual codes for the letters used
empsub <- emp %>% 
  filter(weight_base!='u')

#For SCR, how many sectors?
scr <- empsub %>% filter(english_devolved_area_name=='Sheffield City Region')

unique(emp$sector)
unique(scr$sector)
length(unique(emp$sector))
length(unique(scr$sector))

#Fairly large thing missing! But we do have ICT. Which might be useful.
unique(emp$sector)[!unique(emp$sector) %in% unique(scr$sector)]


#data guidance I think says we can filter like this...
scr.tots <- scr %>% filter(
  estab_size == 'Total',
  single_or_multi_site == 'Total',
  estab_type == 'Total')

#Same number of sectors in both
length(unique(scr$sector)) == length(unique(scr.tots$sector))


#Checked above, weight base is for SCR

scr.tots$htfv_den

#Looking at the sample sizes, seeing if can reverse engineer what they've done
#E.g.
scr.ict <- scr.tots %>% filter(sector=='Information & Communications')
scr.ict$sample_size
scr.ict$employees
scr.ict$sample_size_w_vac
scr.ict$weight_base
scr.ict$has_vac
scr.ict$has_htfv#hard to fill vacancies

#Does the weight base divided by the in-theory-values produce the number of actual ests
#From the sample that said that? 
#Nope and nope!
as.numeric(scr.ict$weight_base)/as.numeric(scr.ict$has_htfv)
as.numeric(scr.ict$has_trained_base)/as.numeric(scr.ict$has_htfv)

#Does the tech doc say anything about this?
#https://assets.publishing.service.gov.uk/media/5f860d2ee90e07415f72b3ad/6099_Employer_Skills_Survey_Technical_Report_IFF_DfE.pdf

#"Survey data were weighted and grossed up to the total population of establishments and total population of employees, according to the 2019 IDBR ‒ the latest available business population statistics published by ONS at the time that weighting was carried out.
#"Data dictionary files were created listing each variable with notes and guidance on the correct weight to use."
#Not for me, for them?

#Oh look, a section called
#"4. Using the survey for analysis"
#ah, it is available in "ONS Virtual Microdata Laboratory and with the UK Data Service"

#"Appendix I: Sampling error and statistical confidence"
#It has 95% CIs here... but not for mayoral authority level. Harrrrr umph.
#And not sure I'd feel confident guessing them. Can get in touch maybe?












