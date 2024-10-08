#earnings explore
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


#NOMIS download----

a <- nomis_search(name = '*earnings*')
atts <- a %>% tidyr::unnest(components.attribute) %>% glimpse()
unique(atts$name.value)

#annual survey of hours and earnings  - resident analysis
y <- nomis_data_info("NM_30_1")
glimpse(y)

#dimensions
#"GEOGRAPHY" "SEX"       "ITEM"      "PAY"       "MEASURES"  "FREQ" 
y %>% tidyr::unnest(components.dimension) %>% select(conceptref) %>% pull()

a <- nomis_get_metadata(id = "NM_30_1")

#Will have to combine local authorities for SY
print(nomis_get_metadata(id = "NM_30_1", concept = "geography", type = "type"), n = 60)
print(nomis_get_metadata(id = "NM_30_1", concept = "geography", type = "TYPE423"), n = 2500)

#Get codes for 4 places
a <- nomis_get_metadata(id = "NM_30_1", concept = "geography", type = "TYPE423")
ids <- a$id[grepl('sheff|rother|barnsley|doncaster', a$label.en, ignore.case = T)]

#Other dims
#Note that according to a manual download and reading the excel sheet:
#Confidence is "Standard error as a percentage of the figure"
print(nomis_get_metadata(id = "NM_30_1", concept = "MEASURES"), n = 60)
print(nomis_get_metadata(id = "NM_30_1", concept = "PAY"), n = 60)
print(nomis_get_metadata(id = "NM_30_1", concept = "SEX"), n = 60)
print(nomis_get_metadata(id = "NM_30_1", concept = "ITEM"), n = 60)

# z <- nomis_get_data(id = "NM_30_1", time = "latest", geography = "TYPE423")
z <- nomis_get_data(id = "NM_30_1", time = "latest", geography = ids)

#tick
unique(z$GEOGRAPHY_NAME)


#Census download----

#Get Highest level of qualification by economic activity
#To separate out who's working in each qual level in SY


#https://www.nomisweb.co.uk/query/construct/components/stdListComponent.asp?menuopt=12&subcomp=100
a <- nomis_search(name = '*RM048*')
atts <- a %>% tidyr::unnest(components.attribute) %>% glimpse()
unique(atts$name.value)

#Will have to be local authorities again
print(nomis_get_metadata(id = "NM_2148_1", concept = "geography", type = "type"), n = 60)
print(nomis_get_metadata(id = "NM_2148_1", concept = "geography", type = "TYPE423"), n = 2500)

#Get IDs for four places again
a <- nomis_get_metadata(id = "NM_2148_1", concept = "geography", type = "TYPE423")
ids <- a$id[grepl('sheff|rother|barnsley|doncaster', a$label.en, ignore.case = T)]

qual.econ <- nomis_get_data(id = "NM_2148_1", time = "latest", geography = ids)

unique(qual.econ$C2021_EASTAT_7_NAME)
unique(qual.econ$C2021_HIQUAL_8_NAME)

#table(qual.econ$C2021_EASTAT_7_NAME,qual.econ$C2021_HIQUAL_8_NAME)


#Quick look at pattern across the four places
qe_4 <- qual.econ %>% filter(C2021_EASTAT_7_NAME!='Total')

ggplot(qe_4, aes(x = C2021_HIQUAL_8_NAME, y = OBS_VALUE, fill = C2021_EASTAT_7_NAME)) +
  geom_bar(position = 'fill', stat = 'identity') +
  facet_wrap(~GEOGRAPHY_NAME, nrow = 1) +
  coord_flip() +
  scale_fill_brewer(palette = 'Paired')

#Non fill version to see actual scale in each category
ggplot(qe_4 %>% filter(C2021_HIQUAL_8_NAME!='Total'), aes(x = C2021_HIQUAL_8_NAME, y = OBS_VALUE, fill = C2021_EASTAT_7_NAME)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~GEOGRAPHY_NAME, nrow = 1, scales = 'free_x') +
  coord_flip() +
  scale_fill_brewer(palette = 'Paired')


#https://github.com/haleyjeppson/ggmosaic
#https://cran.r-project.org/web/packages/ggmosaic/vignettes/ggmosaic.html
#https://cran.r-project.org/web/packages/ggmosaic/vignettes/ggmosaic.html
#Way to use counts;
#https://stackoverflow.com/questions/50227916/adding-counts-to-ggmosaic-can-this-be-done-simpler


#Census process----

#We want just econ active + employed
#To make a range of people working, at different qual levels

#For now, excluding ambiguous categories - for our overall goal of wage difference estimation
#It'll be a relatively small part of the uncertainty.
#But note for future, might change that.

#So - just in employment:
#Let's exclude those categories and then order correctly
factorlevels = unique(qe.w$C2021_HIQUAL_8_NAME)[c(5,1,2,3,4)]

qe.w <- qe_4 %>%
  filter(grepl('in employment', C2021_EASTAT_7_NAME, ignore.case = T),!grepl('appre|other|total', C2021_HIQUAL_8_NAME, ignore.case = T)) %>% 
  group_by(GEOGRAPHY_NAME,C2021_HIQUAL_8_NAME) %>% 
  summarise(total_inemployment = sum(OBS_VALUE)) %>% 
  mutate(qual_level = factor(C2021_HIQUAL_8_NAME, ordered = T, levels = factorlevels))

#Break that down - how many actually working in each category?
ggplot(qe.w, aes(x = GEOGRAPHY_NAME, y = total_inemployment, fill = qual_level)) +
  geom_bar(position = 'fill', stat = 'identity') +
  scale_fill_brewer(palette = 'Paired') +
  ggtitle('All in employment, by qualification level')






#Get earnings stats for SY----

#Via 4 places stats. Let's check those. Percentiles may be empty.
unique(z$PAY_NAME)

#Was going to use annual pay, but Barnsley data missing or huge error bars
fourplaces <- z %>%
  select(
    GEOGRAPHY_NAME,ITEM_NAME,SEX_NAME,PAY_NAME,MEASURES_NAME,OBS_VALUE
  ) %>% 
  filter(
    SEX_NAME == 'Total',
    # PAY_NAME %in% c('Hourly pay - gross','Hours worked - total'),
    PAY_NAME %in% c('Weekly pay - gross','Hours worked - total'),
    # PAY_NAME == 'Annual pay - gross',
    MEASURES_NAME %in% c('Value','Confidence')
  )

  
#Note that according to a manual download and reading the excel sheet:
#Confidence is "Standard error as a percentage of the figure"
#Let's turn that into error bars
fourplaces <- fourplaces %>% 
  pivot_wider(names_from = MEASURES_NAME, values_from = OBS_VALUE) %>% 
  mutate(
    se = Value * (Confidence/100),
    conf_min95 = Value - (se * 1.96),
    conf_max95 = Value + (se * 1.96)
    # conf_min95 = Value - (Value * (Confidence/100) * 1.96),
    # conf_max95 = Value + (Value * (Confidence/100) * 1.96)
      )

#save that for elsewhere
write_csv(fourplaces,'data/SY_earnings_percentiles_2023.csv')

percentiles <- fourplaces %>% 
  filter(
    grepl('percentile',ITEM_NAME),
    # grepl('Annual',PAY_NAME)
    grepl('Hourly|Weekly|Annual',PAY_NAME)
    )

ggplot(percentiles, aes(x = ITEM_NAME, y = Value)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf_min95, ymax = conf_max95)) +
  facet_wrap(~GEOGRAPHY_NAME)

ggplot(percentiles, aes(x = ITEM_NAME, y = Value, colour = GEOGRAPHY_NAME)) +
  geom_point(position = position_dodge(width = 1)) +
  geom_errorbar(aes(ymin = conf_min95, ymax = conf_max95), width = 0.1, position = position_dodge(width = 1)) +
  facet_wrap(~ITEM_NAME, scales = 'free_x', nrow = 1)

#Free y scale
ggplot(percentiles, aes(x = ITEM_NAME, y = Value, colour = GEOGRAPHY_NAME)) +
  geom_point(position = position_dodge(width = 1)) +
  geom_errorbar(aes(ymin = conf_min95, ymax = conf_max95), width = 0.1, position = position_dodge(width = 1)) +
  facet_wrap(~ITEM_NAME, scales = 'free', nrow = 1)




# CREATE EARNINGS / QUAL WAGE GUESSTIMATE FROM REPEATED SAMPLNG----

# Sample from earnings and qualification percentiles to build up estimate of wages for the four places at different qual levels

#TEST:
#0 to 1 value to sample both earnings and quals quantile by
sample_point = runif(1)

#For this place
place = 'Sheffield'

#Do one place at a time, recombine after
debugonce(sample_by_percentile)
earnings.sample <- sample_by_percentile(percentiles %>% filter(GEOGRAPHY_NAME==place), ITEM_NAME, Value, sample_point)

#Qual level much easier
#order by factor levels first
qe.w <- qe.w %>% arrange(qe.w$qual_level)

#so this should now be in right order for prob weighting to work... tick
qe.w$qual_level[qe.w$GEOGRAPHY_NAME == place]

#Get sample
#Nope!
#qual.sample <- sample(levels(qe.w$qual_level), size = 1, prob = qe.w$total_inemployment[qe.w$GEOGRAPHY_NAME == place])

#Calculate cumulative weights
cumulative_weights <- cumsum(qe.w$total_inemployment[qe.w$GEOGRAPHY_NAME == place]) / sum(qe.w$total_inemployment[qe.w$GEOGRAPHY_NAME == place])

#Find the interval into which the random number falls
#Repeated sampling should match place proportions. Check shortly.
qual.sample <- levels(qe.w$qual_level)[which(cumulative_weights >= sample_point)[1]]



results = list()

#RUN FOR EACH PLACE, REPEATED SAMPLES, STORE THEN GET MEAN AND SPREAD
for(place in unique(percentiles$GEOGRAPHY_NAME)){
  
  #Number of samples
  for(i in 1:4000){
    
    sample_point = runif(1)
    
    #Do one place at a time, recombine after
    earnings.sample <- sample_by_percentile(percentiles %>% filter(GEOGRAPHY_NAME==place), ITEM_NAME, Value, sample_point)
    
    #Qual level much easier
    #order by factor levels first
    qe.w <- qe.w %>% arrange(qe.w$qual_level)
    
    #Get sample
    #Repeated sampling should match place proportions. Check shortly.
    #assumes cumul weights worked out above
    qual.sample <- levels(qe.w$qual_level)[which(cumulative_weights >= sample_point)[1]]
    
    results[[length(results)+1]] <- list(place = place, qual.sample = qual.sample, earnings.sample = earnings.sample)
    
  }
  
}

results <- bind_rows(results)

#Test of means... looks OK.
results %>% 
  group_by(place,qual.sample) %>% 
  summarise(meanval = mean(earnings.sample, na.rm = T))

#Now to get dists. Use SD then assume sample range
resultsummary <- results %>%
  group_by(place,qual.sample) %>% 
  summarise(
    meanval = mean(earnings.sample, na.rm = T),
    sd = sd(earnings.sample, na.rm = T)
    ) %>% 
  mutate(
    conf_min95 = meanval - (sd * 1.96),
    conf_max95 = meanval + (sd * 1.96),
    qual_level = factor(qual.sample, ordered = T, levels = factorlevels)
  )


#Merge back in the raw counts in those qual categories
resultsummary <- resultsummary %>% 
  left_join(
    qe.w %>% select(GEOGRAPHY_NAME,qual_level,total_inemployment),
    by = c('place' = 'GEOGRAPHY_NAME','qual_level')
  )


#SANITY CHECK ON GETTING DIFFS 
#Looking in resultsummary, we have mean and confidence mins and maxes in the first rows
#Those are all sane-looking and the right polarity - plotted here:
ggplot(resultsummary, aes(x = place, y = meanval, colour = qual_level)) +
  geom_point(position = position_dodge(width = 1), size = 3) +
  geom_errorbar(aes(ymin = conf_min95, ymax = conf_max95), width = 0.1, position = position_dodge(width = 1), linewidth = 1) +
  scale_colour_brewer(palette = 'Paired') +
  facet_wrap(~place, scales = 'free_x', nrow = 1) +
  ylab('weekly gross earnings')


#But note, the confidence min for level 1 is LOWER THAN the confidence min for 'no quals'
#And the calcs below ended up with a min diff for L1 to L2 HIGHER THAN the max diff, which is likely going to be due to that too
#So, what exactly are we after when we find differences in earnings between qualification levels?

#Let's do some worked examples to make sure it's right
#Picking out just barnsley
b <- resultsummary %>% filter(place=='Barnsley')

#No quals mean and 95% confidence min max
noqual_mean = b$meanval[b$qual.sample == 'No qualifications']
noqual_min = b$conf_min95[b$qual.sample == 'No qualifications']
noqual_max = b$conf_max95[b$qual.sample == 'No qualifications']

#Level 1 mean and 95% confidence min max
l1_mean = b$meanval[b$qual.sample == 'Level 1 and entry level qualifications']
l1_min = b$conf_min95[b$qual.sample == 'Level 1 and entry level qualifications']
l1_max = b$conf_max95[b$qual.sample == 'Level 1 and entry level qualifications']

#Level 2 mean and 95% confidence min max
l2_mean = b$meanval[b$qual.sample == 'Level 2 qualifications']
l2_min  = b$conf_min95[b$qual.sample == 'Level 2 qualifications']
l2_max  = b$conf_max95[b$qual.sample == 'Level 2 qualifications']

noqual_mean
noqual_min 
noqual_max 

l1_mean
l1_min 
l1_max 

l2_mean
l2_min  
l2_max  


#So, questions we want to answer from those?
#1. What's the mean difference in earnings between those qualification levels? i.e. the extra earnings at the level above?
#Which is just 
l1_mean - noqual_mean
l2_mean - l1_mean

#Do we have an issue though with trying to directly diff the confidence intervals?
l1_min - noqual_min
l1_max - noqual_max

#Note, the min is higher than the max
l2_min - l1_min
l2_max - l1_max

#Converting that into English:
#The difference between the minimum likely level 2 wage and the likely min of the qual level below it is £116.87 a week per working person
l2_min - l1_min
#The difference between the maximum likely level 2 wage and the likely max of the qual level below it is £108.87 a week per working person
l2_max - l1_max

#mean is this, sitting between them still
l2_mean - l1_mean

#So what we're getting there is:
#At 95% level, a most likely difference for earnings DIFFERENCES at mean, min 95% and max 95% points. Min doesn't need to be lower than max.

#So that does give us the spread of possible differences
#But  may not accurately capture the confidence interval of the difference between means
#The formula for working that out requires the sample size, which we don't have from the earnings data

#So... 
#Get % diffs between levels
resultsummary <- resultsummary %>% 
  arrange(place,qual_level) %>% 
  group_by(place) %>% 
  mutate(
    diff_mean = meanval - lag(meanval),
    diff_confmin = conf_min95 - lag(conf_min95),
    diff_confmax = conf_max95 - lag(conf_max95),
    percentdiff_mean = ((meanval - lag(meanval))/meanval)*100,
    percentdiff_confmin = ((conf_min95 - lag(conf_min95))/conf_min95)*100,
    percentdiff_confmax = ((conf_max95 - lag(conf_max95))/conf_max95)*100,
    ifonepercent_mean = lag(total_inemployment) * 0.01 * diff_mean,
    ifonepercent_confmin = lag(total_inemployment) * 0.01 * diff_confmin,
    ifonepercent_confmax = lag(total_inemployment) * 0.01 * diff_confmax
    )


#Can sum those 'if 1%' for each place for total money amounts...
resultsummary %>% 
  group_by(qual_level) %>% 
  summarise(
    sum(ifonepercent_mean),
    sum(ifonepercent_confmin),
    sum(ifonepercent_confmax)
    )

#Let's make those yearly huh?
yearly.ifonepercents <- resultsummary %>% 
  group_by(qual_level) %>% 
  summarise(
    mean = sum(ifonepercent_mean) * 52,
    min = sum(ifonepercent_confmin) * 52,
    max = sum(ifonepercent_confmax) * 52
  )




#Repeat for no quals to L2 difference
resultsummary.noqualstoL2 <- resultsummary %>% 
  select(place,qual.sample,meanval,sd,conf_min95,conf_max95,qual_level,total_inemployment) %>% 
  filter(qual.sample %in% c('No qualifications','Level 2 qualifications')) %>% 
  arrange(place,qual_level) %>% 
  group_by(place) %>% 
  mutate(
    diff_mean = meanval - lag(meanval),
    diff_confmin = conf_min95 - lag(conf_min95),
    diff_confmax = conf_max95 - lag(conf_max95),
    percentdiff_mean = ((meanval - lag(meanval))/meanval)*100,
    percentdiff_confmin = ((conf_min95 - lag(conf_min95))/conf_min95)*100,
    percentdiff_confmax = ((conf_max95 - lag(conf_max95))/conf_max95)*100,
    ifonepercent_mean = lag(total_inemployment) * 0.01 * diff_mean,
    ifonepercent_confmin = lag(total_inemployment) * 0.01 * diff_confmin,
    ifonepercent_confmax = lag(total_inemployment) * 0.01 * diff_confmax
  )


yearly.ifonepercents.noqualtoL2 <- resultsummary.noqualstoL2 %>% 
  group_by(qual_level) %>% 
  summarise(
    mean = sum(ifonepercent_mean) * 52,
    min = sum(ifonepercent_confmin) * 52,
    max = sum(ifonepercent_confmax) * 52,
    total_inemployment = sum(total_inemployment)
  )




#Check
#Looking plausible
ggplot(resultsummary, aes(x = place, y = meanval, colour = qual_level)) +
  geom_point(position = position_dodge(width = 1), size = 3) +
  geom_errorbar(aes(ymin = conf_min95, ymax = conf_max95), width = 0.1, position = position_dodge(width = 1), linewidth = 1) +
  scale_colour_brewer(palette = 'Paired') +
  facet_wrap(~place, scales = 'free_x', nrow = 1) +
  ylab('weekly gross earnings')


#Save for viewing
write_csv(resultsummary %>% select(-qual_level) %>% rename(qual_level = `qual.sample`,mean_gross_weekly_earnings = meanval),'data/earnings_v_qualifications_sy4places.csv')
write_csv(yearly.ifonepercents,'data/earnings_v_qualifications_ifonepercentoflowerqual_movedtohigher.csv')




#SOME EXTRA QUAL SUMS----

#Reminder
unique(qual.econ$C2021_EASTAT_7_NAME)
unique(qual.econ$C2021_HIQUAL_8_NAME)

#Just totals for each qual level currently
qe_chk <- qual.econ %>% filter(C2021_EASTAT_7_NAME=='Total') %>% 
  select(GEOGRAPHY_NAME,econ_activity = C2021_EASTAT_7_NAME, qual = C2021_HIQUAL_8_NAME, count = OBS_VALUE)

ggplot(qe_chk %>% filter(qual!='Total'), aes(x = econ_activity, y = count, fill = qual)) +
  geom_bar(position = 'fill', stat = 'identity') +
  facet_wrap(~GEOGRAPHY_NAME, nrow = 1) +
  # coord_flip() +
  scale_fill_brewer(palette = 'Paired')

#Save as CSV
write_csv(qe_chk, 'data/census2021_SY_places_qualcounts.csv')

#So we just want counts for:
# Number of those with up to level 2 quals
# Number of those with no quals
# Number of those with level 3 quals
# Number of those with level 4+ quals


#Add 'Up to level 2'
up_to_level2 <- qe_chk %>% 
  filter(qual %in% c("No qualifications","Level 1 and entry level qualifications","Level 2 qualifications")) %>% 
  group_by(GEOGRAPHY_NAME) %>% 
  summarise(up_to_level2 = sum(count))


#Sum for all of SY
sy_allquals <- qe_chk %>% 
  group_by(qual) %>% 
  summarise(sy_count = sum(count))

sy_up_to_level2 <- qe_chk %>% 
  filter(qual %in% c("No qualifications","Level 1 and entry level qualifications","Level 2 qualifications")) %>% 
  summarise(sy_count = sum(count))



#Weighted mean earnings values for SY at different qual levels
resultsummary %>% 
  group_by(qual_level) %>% 
  summarise(weighted.mean = weighted.mean(meanval,w = total_inemployment))






