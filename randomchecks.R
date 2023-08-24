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












