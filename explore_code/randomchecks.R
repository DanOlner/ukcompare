#Random checks

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#rank of diffs different from rank of nominals?----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

x <- runif(100)
y <- runif(100)

xdiff <- diff(x)
ydiff <- diff(y)

order(x)
order(xdiff)

#Are they in any way similar?
xdiff <- c(1,xdiff)

#Yes.
plot(x,xdiff)

#Is the order similar?
orderx <- order(x)
orderdiffx <- order(xdiff)

#But their *order* is in no way related.
plot(orderx,orderdiffx)
cor(orderx,orderdiffx)

#So very different spearmans
cor(orderx,orderdiffx,method = 'spearman')
cor(x,xdiff,method = 'spearman')

#The next check is whether Spearmans is any good at clustering differenced time series
#Test:
#Diff time series
#But then also log to make magnitude change comparable for different scale time series
#Err log first, then diff! Can't log - values obv

#Some data to experiment on:
gb <- readRDS('data/sectors/gb_fulltimeemployeecountandpercent5digitSIC_BRESopen09to21.rds')

# gb <- gb %>% group_by(DATE) %>% 
#   mutate(
#     count = sum(COUNT),
#     percent = (count/sum(count)*100),
#     percent = ifelse(is.nan(percent), 0, percent)
#     )

#Drop sectors who never get employees above 1000
drops <- gb %>% 
  group_by(INDUSTRY_NAME) %>% 
  summarise(maxcount = max(COUNT))

gb.test <- gb %>% 
  filter( INDUSTRY_NAME %in% (drops %>% filter(maxcount >= 1000) %>% select(INDUSTRY_NAME) %>% pull()) )

#Check for annoying column
#gb.test$INDUSTRY_NAME[grepl('build',gb.test$INDUSTRY_NAME)]

#From some scribbles, 2nd diff is probably what one would want.
#It picks up on similar changes in vector direction, I think


#Find diffs
#Log not necessary if diffing %s surely?
gb.test <- gb.test %>% 
  group_by(INDUSTRY_NAME) %>% 
  arrange(DATE) %>% 
  mutate(
    # log_percent = log(percent),
    diff = percent-lag(percent),
    second_diff = diff-lag(diff)
    # diff_log = log_percent-lag(log_percent)
    ) %>% 
  filter(DATE != 2009) %>% #drop the NA year
  # filter(INDUSTRY_CODE!=22230) %>%  #also remove annoying UTF-8 issue sector just for now
  ungroup()
  
# chk <- gb.test %>% filter(INDUSTRY_NAME %in% gb.test$INDUSTRY_NAME[grepl('build',gb.test$INDUSTRY_NAME)])


#Check if this works to replace the naughty non-UTF8 chars... tick!
#https://stackoverflow.com/a/17292126
gb.test$INDUSTRY_NAME <- iconv(gb.test$INDUSTRY_NAME, "UTF-8", "UTF-8",sub='')


#Check diff scales before / after log
# plot_ly(data = gb.test %>% filter(INDUSTRY_NAME %in% gb.test$INDUSTRY_NAME[grepl('build',gb.test$INDUSTRY_NAME)]), x = ~DATE, y = ~diff_log, color = ~INDUSTRY_NAME, 
plot_ly(data = gb.test, x = ~DATE, y = ~diff_log, color = ~INDUSTRY_NAME, 
        text = ~paste("Sector:", INDUSTRY_NAME),  # Add this line for hover text
        hoverinfo = 'text+y+x',
        type = 'scatter', mode = 'lines+markers', line = list(shape = 'linear')) %>%
  layout(title = "Yearly percents by SIC", 
         xaxis = list(title = "Year"), 
         # yaxis = list(title = "Value", type='log'),
         yaxis = list(title = "Value"),
         showlegend = F)

#OK, that looks like diffs in the same scale


#Log pattern 0-1 / 1-x symmetric either side of 1, right?
#Tick, but not linear
x <- 1:100
y <- c(1/(101-x),x)
plot(y)
plot(log(y))
plot(log10(y))


#Presumably relatively linear closer to 0?
#Ish.
x <- 1:20
y <- c(1/(21-x),x)
plot(y)




#~~~~~~~~~~~~~
#LQ CHECKS----
#~~~~~~~~~~~~~

#Concentration and specialisation, same number in different form?
#Via https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/datasets/locationquotientdataandindustrialspecialisationforlocalauthorities
#1) E(ir) = number of employee jobs in industry i in region r
#2) E(i) = total number of employee jobs in industry i for whole of GB (for example)
#3) E(r) = total number of employee jobs in region r
#4) E = total number of employee jobs in GB

#Concentration:
# (1 / 2) over (3 / 4)
# Specialisation:
#(1 / 3) over (2 / 4)

eir = 4
ei = 55
er = 20
e = 125

#Yup, same number
(eir/ei)/(er/e)
(eir/er)/(ei/e)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#BUSINESS SIZE BAND CHECKS----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#For the larger counts, band size error bars are going to be negligible and the mean exactly the centre
#But let's work it out where we've got places with small numbers? For some, we just won't know - but then the band range is just the min/max, we know that



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#EIGENCHECKS----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Install and load the 'Matrix' package for sparse matrix operations (if not already installed)
library(Matrix)


# Create a hypothetical friendship adjacency matrix
# 1 indicates a friendship, and 0 indicates no friendship

# Let's assume:
# Person 1 is friends with everyone (the popular individual).
# Person 2, 3, 4 are close friends among themselves (a tight-knit group).
# Person 5, 6 are friends (another smaller group).

friendship_matrix <- matrix(c(0, 1, 1, 1, 1, 1,
                              1, 0, 1, 1, 0, 0,
                              1, 1, 0, 1, 0, 0,
                              1, 1, 1, 0, 0, 0,
                              1, 0, 0, 0, 0, 1,
                              1, 0, 0, 0, 1, 0), ncol=6)

# Print the friendship matrix
print(friendship_matrix)

# Compute the eigenvectors and eigenvalues of the matrix
eig <- eigen(friendship_matrix)

# The first eigenvector
print(eig$vectors[,1])

# The second eigenvector
print(eig$vectors[,2])



#COEF CHECKS----

#Which coef value is the SE? If I want to be able to return that?
x = runif(100)
y = runif(100)

m <- lm(data = data.frame(x,y), formula = y ~ x)

#Slope/coef
coef(m)[2]

#Oh, we need more info to pull out the SE
coef(m)

ms <- summary(m)
ms[[4]]
ms[[4]]['x','Std. Error']#this!
summary(m)[[4]]['x','Std. Error']#this!
#is this the same number? Tick
summary(m)$coefficients[2, 2]


#Try in amended function
itl2.cp <- read_csv('data/sectors/ITL2currentprices_long.csv')

itl2.cp <- itl2.cp %>% 
  split(.$year) %>% 
  map(add_location_quotient_and_proportions, 
      regionvar = ITL_region_name,
      lq_var = SIC07_description,
      valuevar = value) %>% 
  bind_rows()

debugonce(get_slope_and_se_or_zero)
get_slope_and_se_or_zero(itl2.cp, ITL_region_name, SIC07_description,#slopes will be found within whatever grouping vars are added here
                         y = LQ_log, x = year)


#Aye, working, with a bit of AI help.
#So let's test / see what we get here
#Add in some 95% CIs
LQ_slopes <- get_slope_and_se_safely(
  data = itl2.cp, 
  ITL_region_name, SIC07_description,#slopes will be found within whatever grouping vars are added here
  y = log(sector_regional_proportion), x = year) %>% 
  mutate(
    min95 = slope - (se * 1.96),
    max95 = slope + (se * 1.96),
    crosseszero = min95 * max95 < 0#mark if crosses zero
  )

#LQs
LQ_slopes <- get_slope_and_se_safely(
  data = itl2.cp, 
  ITL_region_name, SIC07_description,#slopes will be found within whatever grouping vars are added here
  y = LQ_log, x = year) %>% 
  mutate(
    min95 = slope - (se * 1.96),
    max95 = slope + (se * 1.96),
    crosseszero = min95 * max95 < 0#mark if crosses zero
  )

#similar prop for LQ vs sector_regional_proportion
table(LQ_slopes$crosseszero) %>% prop.table


#Filter down to a single year
yeartoplot <- itl2.cp %>% filter(year == 2021)

#Add slopes into data to get LQ plots
yeartoplot <- yeartoplot %>% 
  left_join(
    LQ_slopes,
    by = c('ITL_region_name','SIC07_description')
  )




#TEST FUNCTION THAT WRAPS GREPL TO DO THE THING I DO ALL THE TIME (e.g. "South Y" to filter all "South Yorkshire"s)----

#Random thing to test on
itl2.gvaperjob22 <- readRDS('data/itl2_gva_to2022_plusBRES_jobs_to2022.rds')

#ignore.case = T set in function
itl2.gvaperjob22 %>% filter(gq('south y',Region_name), gq('agri',SIC07_description))



