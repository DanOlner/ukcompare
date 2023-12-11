#Misc functions
library(tidyverse)
library(ggrepel)

#Compute series of slopes within groups safely, returning 0 if can't calculate
compute_slope_or_zero <- function(data, ..., y, x) {
  
  groups <- quos(...) 
  y <- enquo(y)
  x <- enquo(x)

  #Function to compute slope
  get_slope <- function(data) {
    # model <- lm(data = data, formula = as.formula(paste0(!!y, " ~ ", !!x)))
    model <- lm(data = data, formula = as.formula(paste0(quo_name(y), " ~ ", quo_name(x))))
    coef(model)[2]
  }

  #Make it a safe function using purrr::possibly
  safe_get_slope <- possibly(get_slope, otherwise = 0)

  #Group and summarize
  data %>%
    group_by(!!!groups) %>%
    nest() %>%
    mutate(slope = map_dbl(data, safe_get_slope)) %>%
    select(-data)
  
}



#Version that returns slope and SE (for 2D LM only...)
get_slope_and_se_safely <- function(data, ..., y, x) {
  
  groups <- quos(...)  
  y <- enquo(y)
  x <- enquo(x) 
  
  #Function to compute slope
  get_slope_and_se <- function(data) {
    # model <- lm(data = data, formula = as.formula(paste0(!!y, " ~ ", !!x)))
    model <- lm(data = data, formula = as.formula(paste0(quo_name(y), " ~ ", quo_name(x))))
    
    slope <- coef(model)[2]
    se <- summary(model)$coefficients[2, 2]
    return(list(slope = slope, se = se))
    
    
    # return(c(coef(model)[2],summary(model)[[4]]['x','Std. Error']))
  }
  
  #Make it a safe function using purrr::possibly
  safe_get_slope <- possibly(get_slope_and_se, otherwise = list(slope = NA, se = NA))
  
  #Group and summarize
  data %>%
    group_by(!!!groups) %>%
    nest() %>%
    mutate(result = map(data, safe_get_slope)) %>% 
    mutate(slope = map_dbl(result, "slope"),
           se = map_dbl(result, "se")) %>%
    select(-data, -result)
  
}




#Create location quotients (and the regional and larger scale proportions needed to calculate it) and return attached to original dataframe
add_location_quotient_and_proportions <- function(df, regionvar, lq_var, valuevar){
  
  regionvar <- enquo(regionvar)
  lq_var <- enquo(lq_var)
  valuevar <- enquo(valuevar)
  
  df <- df %>%
    group_by(!!regionvar) %>% 
    mutate(
      region_totalsize = sum(!!valuevar, na.rm = T),#a. Current price per region per year, for regional denominator
      sector_regional_proportion = !!valuevar / region_totalsize#b. regional sector proportion (noting that a single row in this group is a single sector)
    ) %>% 
    group_by(!!lq_var) %>% 
    mutate(
      total_sectorsize = sum(!!valuevar, na.rm = T),#c. Summed current prices for EACH SECTOR, UK-wide
    ) %>% 
    ungroup() %>% 
    mutate(
      totalsize = sum(!!valuevar, na.rm = T),#d. Summed current prices for WHOLE UK per year, for UK denominator
      sector_total_proportion = total_sectorsize / totalsize#e. UK-level sector proportion
    ) %>% 
    mutate(
      LQ = sector_regional_proportion / sector_total_proportion#f. Location quotient!
    ) %>% 
    mutate(LQ_log = log(LQ)) 
  
  return(df)
  
}


#Get regional and full geography proportions
#But the full geography minus the region value so they're
#Both entirely separate calculations
#So total can be plotted on one axis and region on the other
#Without the total including part of the region amount
# get_sector_total_proportions <- function(df, lq_var, valuevar){
# 
#   lq_var <- enquo(lq_var) 
#   valuevar <- enquo(valuevar)
# 
#   df <- df %>%
#     group_by(!!lq_var) %>%
#     mutate(
#       total_sectorsize = sum(!!valuevar, na.rm = T),#c. Summed current prices for EACH SECTOR, UK-wide
#     ) %>%
#     ungroup() %>%
#     mutate(
#       totalsize = sum(!!valuevar, na.rm = T),#d. Summed current prices for WHOLE UK per year, for UK denominator
#       sector_total_proportion = total_sectorsize / totalsize#e. UK-level sector proportion, minus the excluded place
#     )
# 
#   return(df)
# 
# }
# 











#Make base plot for LQ plot, with option of setting alpha to zero if we don't want to see all other places
LQ_baseplot <- function(df, alpha = 0.1, sector_name, LQ_column, change_over_time){
  
  sector_name <- enquo(sector_name)
  LQ_column <- enquo(LQ_column)
  change_over_time <- enquo(change_over_time) 
  
  p <- ggplot() +
  geom_point(
    data = df %>% filter(!!change_over_time > 0), 
    aes(y = !!sector_name, x = !!LQ_column, size = !!change_over_time),
    alpha = alpha,
    shape = 16,
    colour = 'green'
  ) +
  geom_point(
    data = df %>% filter(!!change_over_time < 0), 
    aes(y = !!sector_name, x = !!LQ_column, size = !!change_over_time * -1),
    alpha = alpha,
    shape = 16,
    colour = 'red'
  )  +
  scale_size_continuous(range = c(1,17)) +
  scale_x_continuous(trans = 'log10') +
  geom_vline(xintercept = 1, colour = 'blue') +
  guides(size = F) +
  ylab("")
  
  return(p)

}
  

#For LQ change plots, overlay another place on the base plot
#It expects the following:
#dataframe containing a region and sector column, where the sector column is an ordered factor, ordered before it gets here
#a column with the LQ value
#a column with values of change over time showing growth or shrinkage
#Optional columns to include:
#a column with the raw value the LQ is based on, and a column containing the sector regional proportion from the LQ calculation (both appear in text if included)
#a column with min and max values to overlay as bars to indicate full range of the data
addplacename_to_LQplot <- function(df, plot_to_addto, placename, shapenumber=16, backgroundcolour='black', add_gva = F, setalpha = 1,
                                   region_name, sector_name,change_over_time, value_column, LQ_column, sector_regional_proportion,
                                   min_LQ_all_time,max_LQ_all_time){
  
  region_name <- enquo(region_name) 
  sector_name <- enquo(sector_name)
  change_over_time <- enquo(change_over_time)
  LQ_column <- enquo(LQ_column)
  
  plot_to_addto <- plot_to_addto +
    geom_point(
      data = df %>% filter(!!region_name == placename, !!change_over_time > 0), 
      aes(y = !!sector_name, x = !!LQ_column, size = !!change_over_time *1.75),
      shape = shapenumber,
      colour = backgroundcolour,
      alpha = setalpha
    ) +
    geom_point(
      data = df %>% filter(!!region_name == placename, !!change_over_time < 0), 
      aes(y = !!sector_name, x = !!LQ_column, size = !!change_over_time *-1.75),
      shape = shapenumber,
      colour = backgroundcolour,
      alpha = setalpha
    ) +
    geom_point(
      data = df %>% filter(!!region_name == placename, !!change_over_time > 0), 
      aes(y = !!sector_name, x = !!LQ_column, size = !!change_over_time),
      shape = shapenumber,
      colour = 'green',
      alpha = setalpha
    ) + 
    geom_point(
      data = df %>% filter(!!region_name == placename, !!change_over_time < 0), 
      aes(y = !!sector_name, x = !!LQ_column, size = !!change_over_time * -1),
      shape = shapenumber,
      colour = 'red',
      alpha = setalpha
    ) 
  
  #Test for one of these missing, don't display if so
  if(!(missing(value_column)|missing(sector_regional_proportion))){
    value_column <- enquo(value_column)
    sector_regional_proportion <- enquo(sector_regional_proportion)
    
    plot_to_addto <- plot_to_addto +  
      geom_text(
        data = df %>% filter(!!region_name == placename), 
        aes(y = !!sector_name, x = 20, label = paste0('£',!!value_column,'M, ',round(!!sector_regional_proportion * 100, 2),'%')),
        nudge_x = 0.3, hjust = 1, alpha = 0.7, size = 3
      )
    
    #Test for one of these missing, don't display if so
    if(!(missing(min_LQ_all_time)|missing(max_LQ_all_time)) ){
      
      min_LQ_all_time <- enquo(min_LQ_all_time)
      max_LQ_all_time <- enquo(max_LQ_all_time)
      
      plot_to_addto <- plot_to_addto +
        geom_errorbar(
          data = df %>% filter(!!region_name == placename),
          aes(y = !!sector_name, xmin = !!min_LQ_all_time, xmax = !!max_LQ_all_time),
          width = 0.1
        )
      
    }
    
    
  }
  
  return(plot_to_addto)
  
}










#2D PROPORTION PLOT COMPARING TWO TIME POINTS
# X axis name or names (of subregions)
# Y axis name or names (of subregions)
# Column to be comparing (so e.g. we can do smoothing on it beforehand if we want)
# time variable
# Start time
# End time
# Compass position to display
twod_proportionplot <- function(df, regionvar, category_var, valuevar, timevar, start_time, end_time, x_regionnames, y_regionnames, compasspoints_to_display = c('NE','NW','SE','SW'), sectors_to_display = NULL){
  
  regionvar <- enquo(regionvar)
  category_var <- enquo(category_var)
  valuevar <- enquo(valuevar)
  timevar <- enquo(timevar)
  
  x_region_results <- df %>% 
    filter(!!regionvar %in% x_regionnames) %>% 
    group_split(!!timevar) %>% 
    map(add_location_quotient_and_proportions, 
        regionvar = !!regionvar,
        lq_var = !!category_var,
        valuevar = !!valuevar) %>% 
    bind_rows() %>% 
    group_by(!!category_var,!!timevar) %>% 
    summarise(x_sector_total_proportion = min(sector_total_proportion))#just get unique values
  
  y_region_results <- df %>% 
    filter(!!regionvar %in% y_regionnames) %>% 
    group_split(!!timevar) %>% 
    map(add_location_quotient_and_proportions, 
        regionvar = !!regionvar,
        lq_var = !!category_var,
        valuevar = !!valuevar) %>% 
    bind_rows() %>% 
    group_by(!!category_var,!!timevar) %>% 
    summarise(y_sector_total_proportion = min(sector_total_proportion))#just get unique values
  
  #Join both
  both <- x_region_results %>% 
    left_join(
      y_region_results,
      by = c(quo_name(category_var),quo_name(timevar))
    ) 
  
  #If including subset of sectors to display
  if(!missing(sectors_to_display)){
    
    twoy <- both %>% filter(
      !!timevar %in% c(start_time, end_time),
      !!category_var %in% sectors_to_display
      ) 
    
  } else {
    
    twoy <- both %>% filter(!!timevar %in% c(start_time, end_time))  
    
  }

  twoy_lags <- twoy %>%
    arrange(!!category_var,!!timevar) %>%
    mutate(
      lag_x_sector_total_proportion = x_sector_total_proportion - lag(x_sector_total_proportion),
      lag_y_sector_total_proportion = y_sector_total_proportion - lag(y_sector_total_proportion)
    ) %>%
    filter(!!timevar == end_time) %>% #using final year to mark when going in particular compass direction
    mutate(
      compass = case_when(
        lag_x_sector_total_proportion < 0 & lag_y_sector_total_proportion < 0 ~ 'SW',
        lag_x_sector_total_proportion < 0 & lag_y_sector_total_proportion > 0 ~ 'NW',
        lag_x_sector_total_proportion > 0 & lag_y_sector_total_proportion > 0 ~ 'NE',
        lag_x_sector_total_proportion > 0 & lag_y_sector_total_proportion < 0 ~ 'SE'
      ) 
    )

  twoy <- twoy %>%  
    left_join( 
      twoy_lags %>%
        select(!!category_var,compass),
      by = quo_name(category_var)
    )

  
  twoy.wide <- twoy %>% filter(compass %in% compasspoints_to_display) %>%
    mutate(!!timevar := ifelse(!!timevar == min(!!timevar), 'start', 'end')) %>%
    select(!!category_var,!!timevar,x_sector_total_proportion,y_sector_total_proportion) %>%
    pivot_wider(names_from = !!timevar, values_from = c(x_sector_total_proportion,y_sector_total_proportion))

  p <- ggplot() +
    geom_segment(data = twoy.wide, aes(x = x_sector_total_proportion_start * 100, y = y_sector_total_proportion_start  * 100,
                                       xend = x_sector_total_proportion_end * 100, yend = y_sector_total_proportion_end * 100),
                 arrow = arrow(length = unit(0.5, "cm")),
                 size = 1
    )

  p <- p +
    geom_point(data = twoy %>% filter(compass%in%compasspoints_to_display), size = 5, alpha = 0.75,
               aes(x = x_sector_total_proportion * 100, y = y_sector_total_proportion * 100,colour = factor(!!timevar), group = !!category_var)) +
    geom_line(data = twoy %>% filter(compass %in% compasspoints_to_display), size = 1, aes(x = x_sector_total_proportion * 100, y = y_sector_total_proportion * 100, group = !!category_var), colour = 'red') +
    geom_abline(slope = 1, size = 1, colour='blue', alpha = 0.5) +
    # coord_cartesian(xlim = c(0.1,11), ylim = c(0.1,11)) + # good for log scale
    # scale_y_log10() +
    # scale_x_log10() +
    guides(colour=guide_legend(title=" "))
  
  p <- p + geom_text_repel(
    data = twoy %>% filter(!!timevar==max(!!timevar), compass%in%compasspoints_to_display),
    aes(x = x_sector_total_proportion * 100, y = y_sector_total_proportion * 100,
        label = paste0(!!category_var, "\n(",round(x_sector_total_proportion * 100,2),"%,",round(y_sector_total_proportion * 100,2),"%)"), 
        # label = paste0(!!category_var, "\n(x:",round(x_sector_total_proportion * 100,2),"%,y:",round(y_sector_total_proportion * 100,2),"%)"), 
        colour = compass),
    alpha=1,
    nudge_x = .05,
    box.padding = 1,
    nudge_y = 0.05,
    segment.curvature = -0.1,
    segment.ncp = 0.3,
    segment.angle = 20,
    max.overlaps = 20
  ) +
    scale_color_manual(values = setNames(c("red", "black",'#7fc97f','#beaed4','#fdc086','#1f78b4'),
                                         c(start_time, end_time,'NE','SE','NW','SW')))

  p

  
}





#Return compass points for use elsewhere in filtering
return_compasspoints <- function(df, regionvar, category_var, valuevar, timevar, start_time, end_time, x_regionnames, y_regionnames, compasspoints_to_display = c('NE','NW','SE','SW'), sectors_to_display = NULL){
  
  regionvar <- enquo(regionvar)
  category_var <- enquo(category_var)
  valuevar <- enquo(valuevar)
  timevar <- enquo(timevar)
  
  x_region_results <- df %>% 
    filter(!!regionvar %in% x_regionnames) %>% 
    group_split(!!timevar) %>% 
    map(add_location_quotient_and_proportions, 
        regionvar = !!regionvar,
        lq_var = !!category_var,
        valuevar = !!valuevar) %>% 
    bind_rows() %>% 
    group_by(!!category_var,!!timevar) %>% 
    summarise(x_sector_total_proportion = min(sector_total_proportion))#just get unique values
  
  y_region_results <- df %>% 
    filter(!!regionvar %in% y_regionnames) %>% 
    group_split(!!timevar) %>% 
    map(add_location_quotient_and_proportions, 
        regionvar = !!regionvar,
        lq_var = !!category_var,
        valuevar = !!valuevar) %>% 
    bind_rows() %>% 
    group_by(!!category_var,!!timevar) %>% 
    summarise(y_sector_total_proportion = min(sector_total_proportion))#just get unique values
  
  #Join both
  both <- x_region_results %>% 
    left_join(
      y_region_results,
      by = c(quo_name(category_var),quo_name(timevar))
    ) 
  
  #If including subset of sectors to display
  if(!missing(sectors_to_display)){
    
    twoy <- both %>% filter(
      !!timevar %in% c(start_time, end_time),
      !!category_var %in% sectors_to_display
      ) 
    
  } else {
    
    twoy <- both %>% filter(!!timevar %in% c(start_time, end_time)) 
    
  }

  twoy %>%
    arrange(!!category_var,!!timevar) %>%
    mutate(
      lag_x_sector_total_proportion = x_sector_total_proportion - lag(x_sector_total_proportion),
      lag_y_sector_total_proportion = y_sector_total_proportion - lag(y_sector_total_proportion)
    ) %>%
    filter(!!timevar == end_time) %>% #using final year to mark when going in particular compass direction
    mutate(
      compass = case_when(
        lag_x_sector_total_proportion < 0 & lag_y_sector_total_proportion < 0 ~ 'SW',
        lag_x_sector_total_proportion < 0 & lag_y_sector_total_proportion > 0 ~ 'NW',
        lag_x_sector_total_proportion > 0 & lag_y_sector_total_proportion > 0 ~ 'NE',
        lag_x_sector_total_proportion > 0 & lag_y_sector_total_proportion < 0 ~ 'SE'
      ) 
    )

}




#Do generic 2D change over time plot
#With arbitrary x and y axis values that are passed directly in
# Start time, end time.
# X_var, will be values from that column e.g. GVA
# Y_var, values e.g. job count
# Category_Var = either e.g. places or sectors
#Label var, from the two time points, to display
twod_generictimeplot <- function(df, category_var, x_var, y_var, timevar, label_var, start_time, end_time, compasspoints_to_display = c('NE','NW','SE','SW')){
  
  category_var <- enquo(category_var)  
  x_var <- enquo(x_var)
  y_var <- enquo(y_var)
  timevar <- enquo(timevar)
  label_var <- enquo(label_var)
  
  #Reduce to the two start and end timepoints we want to display
  twoy <- df %>%
    filter(
    !!timevar %in% c(start_time, end_time)
    ) %>% 
    arrange(!!timevar)
  
  twoy_lags <- twoy %>%
    arrange(!!category_var,!!timevar) %>%
    mutate(
      lag_x_var = !!x_var - lag(!!x_var),
      lag_y_var = !!y_var - lag(!!y_var)
    ) %>%
    filter(!!timevar == end_time) %>% #using final year to mark when going in particular compass direction
    mutate(
      compass = case_when(
        lag_x_var < 0 & lag_y_var < 0 ~ 'SW', 
        lag_x_var < 0 & lag_y_var > 0 ~ 'NW',
        lag_x_var > 0 & lag_y_var > 0 ~ 'NE', 
        lag_x_var > 0 & lag_y_var < 0 ~ 'SE'
      ) 
    )
  
  twoy <- twoy %>%  
    left_join( 
      twoy_lags %>%
        select(!!category_var,compass),
      by = quo_name(category_var)
    )
  
  
  twoy.wide <- twoy %>% filter(compass %in% compasspoints_to_display) %>%
    mutate(!!timevar := ifelse(!!timevar == min(!!timevar), 'start', 'end')) %>%
    select(!!category_var,!!timevar,!!x_var,!!y_var,!!label_var) %>%
    pivot_wider(names_from = !!timevar, values_from = c(!!x_var,!!y_var,!!label_var))
  
  #Rename wide two year for change vector back to generic names
  names(twoy.wide) <- c(names(twoy.wide)[1],'x_start','x_end','y_start','y_end','label_start','label_end')
  
  p <- ggplot() +
    geom_segment(data = twoy.wide, aes(x = x_start, y = y_start ,
                                       xend = x_end, yend = y_end),
                 arrow = arrow(length = unit(0.5, "cm")),
                 size = 1
    )
  
  p <- p +
    geom_point(data = twoy %>% filter(compass%in%compasspoints_to_display), size = 5, alpha = 0.75,
               aes(x = !!x_var, y = !!y_var,colour = factor(!!timevar), group = !!category_var)) +
    geom_line(data = twoy %>% filter(compass %in% compasspoints_to_display), size = 1, aes(x = !!x_var, y = !!y_var, group = !!category_var), colour = 'red') +
    # geom_abline(slope = 1, size = 1, colour='blue', alpha = 0.5) +
    # coord_cartesian(xlim = c(0.1,11), ylim = c(0.1,11)) + # good for log scale
    # scale_y_log10() +
    # scale_x_log10() +
    guides(colour=guide_legend(title=" ")) +
    xlab(quo_name(x_var)) +
    ylab(quo_name(y_var)) 
  
  #Reduce to latest year and merge in values for labels
  label_df <- twoy %>% filter(!!timevar==max(!!timevar), compass %in% compasspoints_to_display) %>%
    left_join(
      twoy.wide %>% select(!!category_var,label_start,label_end)
    )
  
  p <- p + geom_text_repel(
    data = label_df,
    # data = twoy %>% filter(!!timevar==max(!!timevar), compass %in% compasspoints_to_display),
    aes(x = !!x_var, y = !!y_var,
        label = paste0(!!category_var, "\n(",quo_name(label_var),": ",round(label_start,2),ifelse(label_start < label_end," >> "," << "),round(label_end,2),")"),
        # label = paste0(!!category_var, "\n(",quo_name(x_var),": ",round(!!x_var,2),", ",quo_name(y_var),": ",round(!!y_var,2),")"),
        # label = paste0(!!category_var, "\n(",quo_name(x_var),": ",round(!!x_var,2),", ",quo_name(y_var),": ",round(!!y_var,2),")"),
        # label = paste0(!!category_var, "\n(x:",round(x_var,2),"%,y:",round(y_var,2),"%)"),
        colour = compass),
    alpha=1,
    nudge_x = .05,
    box.padding = 1,
    nudge_y = 0.05,
    segment.curvature = -0.1,
    segment.ncp = 0.3,
    segment.angle = 20,
    max.overlaps = 20
  ) +
    scale_color_manual(values = setNames(c("red", "black",'#7fc97f','#beaed4','#fdc086','#1f78b4'),
                                         c(start_time, end_time,'NE','SE','NW','SW')))
  
  p
  
  
}


percent_change <- function(x,y) ((y - x) / x) * 100


#Normalise all vectors so origin is zero
#And scale all by % change between time points
# Start time, end time.
# X_var, will be values from that column e.g. GVA
# Y_var, values e.g. job count
# Category_Var = either e.g. places or sectors
#Label var, from the two time points, to display
twod_generictimeplot_normalisetozero <- function(df, category_var, x_var, y_var, timevar, label_var, start_time, end_time, compasspoints_to_display = c('NE','NW','SE','SW'), category_var_value_to_highlight="NULL"){
  
  category_var <- enquo(category_var)   
  x_var <- enquo(x_var)
  y_var <- enquo(y_var)
  timevar <- enquo(timevar)
  label_var <- enquo(label_var)
  
  #Reduce to the two start and end timepoints we want to display
  twoy <- df %>%
    filter(
    !!timevar %in% c(start_time, end_time)
    ) %>% 
    arrange(!!timevar)
  
  twoy_lags <- twoy %>%
    arrange(!!category_var,!!timevar) %>%
    mutate(
      lag_x_var = !!x_var - lag(!!x_var),
      lag_y_var = !!y_var - lag(!!y_var)
    ) %>%
    filter(!!timevar == end_time) %>% #using final year to mark when going in particular compass direction
    mutate(
      compass = case_when(
        lag_x_var < 0 & lag_y_var < 0 ~ 'SW', 
        lag_x_var < 0 & lag_y_var > 0 ~ 'NW',
        lag_x_var > 0 & lag_y_var > 0 ~ 'NE', 
        lag_x_var > 0 & lag_y_var < 0 ~ 'SE'
      ) 
    )
  
  twoy <- twoy %>%  
    left_join( 
      twoy_lags %>%
        select(!!category_var,compass),
      by = quo_name(category_var) 
    ) 
  
  
  twoy.wide <- twoy %>% filter(compass %in% compasspoints_to_display) %>%
    mutate(!!timevar := ifelse(!!timevar == min(!!timevar), 'start', 'end')) %>%
    select(!!category_var,!!timevar,!!x_var,!!y_var,!!label_var) %>%
    pivot_wider(names_from = !!timevar, values_from = c(!!x_var,!!y_var,!!label_var)) 
  
  #Rename wide two year for change vector back to generic names
  names(twoy.wide) <- c(names(twoy.wide)[1],'x_start','x_end','y_start','y_end','label_start','label_end')
  
  #Make percent change values for the vector x and y values
  twoy.wide <- twoy.wide %>% 
    mutate(
      x_pct_change = percent_change(x_start, x_end),
      y_pct_change = percent_change(y_start, y_end),
      category_var_val_to_highlight = ifelse(!!category_var == category_var_value_to_highlight, T,F)
    )
  
  #Vectors all centred on zero, percent change for all shown
  #Annotate with a triangle indicating the half of the plot where GVA per worker will have dropped between time points
  p <- ggplot() +
    annotate(geom = "polygon", x = c(-1000, 1000, -1000), y = c(1000, 1000, -1000), fill = "white", alpha = 0.5)
    
    
  p <- p +
    geom_segment(data = twoy.wide, aes(x = 0, y = 0 ,xend = x_pct_change, yend = y_pct_change), 
                 # colour = category_var_val_to_highlight),
                 arrow = arrow(length = unit(0.5, "cm")),
                 size = 1, alpha = 0.5
    ) +
    # scale_color_manual(values=c("red","blue")) +
    guides(colour=guide_legend(title=" ")) +
    xlab( paste0(quo_name(x_var),' percent change ',start_time,' to ',end_time) ) +
    ylab( paste0(quo_name(y_var),' percent change ',start_time,' to ',end_time) ) +
    geom_vline(xintercept = 0, alpha = 0.1, size =2, colour = 'red') +
    geom_hline(yintercept = 0, alpha = 0.1, size =2, colour = 'red') 
    # geom_abline(intercept = 0, slope = 1, alpha = 0.1, size =2, colour = 'black') 
    # geom_abline(intercept = 0, slope = -1, alpha = 0.1, size =2, colour = 'green') 
    
  #Colour aes clashes with one below for labels, which I want to keep
  #Do hacky overlay instead
  p <- p + geom_segment(data = twoy.wide %>% filter(category_var_val_to_highlight), 
                        aes(x = 0, y = 0 ,
                                              xend = x_pct_change, yend = y_pct_change), 
                        arrow = arrow(length = unit(0.5, "cm")),
                        size = 2, colour = '#3333ff'
  )
  
  
  # #Reduce to latest year and merge in values for labels
  label_df <- twoy %>% filter(!!timevar==max(!!timevar), compass %in% compasspoints_to_display) %>%
    left_join(
      twoy.wide %>% select(!!category_var,label_start,label_end,x_pct_change,y_pct_change,category_var_val_to_highlight)
    ) %>% 
    mutate(category_var_val_to_highlight = ifelse(category_var_val_to_highlight, 'bold','plain'))

  p <- p + geom_text_repel(
    data = label_df,
    # data = twoy %>% filter(!!timevar==max(!!timevar), compass %in% compasspoints_to_display),
    aes(x = x_pct_change, y = y_pct_change,fontface = category_var_val_to_highlight,
        label = paste0(!!category_var, "\n(",quo_name(label_var),": ",round(label_start,2),ifelse(label_start < label_end," >> "," << "),round(label_end,2),")"),
        # label = paste0(!!category_var, "\n(",quo_name(x_var),": ",round(!!x_var,2),", ",quo_name(y_var),": ",round(!!y_var,2),")"),
        # label = paste0(!!category_var, "\n(",quo_name(x_var),": ",round(!!x_var,2),", ",quo_name(y_var),": ",round(!!y_var,2),")"),
        # label = paste0(!!category_var, "\n(x:",round(x_var,2),"%,y:",round(y_var,2),"%)"),
        colour = compass),
    alpha=1,
    nudge_x = .05,
    box.padding = 1,
    nudge_y = 0.05,
    segment.curvature = -0.1,
    segment.ncp = 0.3,
    segment.angle = 20,
    max.overlaps = 20
  ) +
    scale_color_manual(values = setNames(c("red", "black",'#7fc97f','#beaed4','#fdc086','#1f78b4'),
                                         c(start_time, end_time,'NE','SE','NW','SW')))
  
  return(list(plot = p, twoyeardata = twoy.wide))
  
  
}








twod_generictimeplot_multipletimepoints <- function(df, category_var, x_var, y_var, timevar, label_var, times, compasspoints_to_display = c('NE','NW','SE','SW')){
  
  category_var <- enquo(category_var)   
  x_var <- enquo(x_var)
  y_var <- enquo(y_var)
  timevar <- enquo(timevar)
  label_var <- enquo(label_var)
  
  #Though plotting multiple
  #Still use start and end point to get overall compass direction
  twoy <- df %>%
    filter(
      !!timevar %in% c(min(times), max(times))
    ) %>% 
    arrange(!!timevar)
  
  twoy_lags <- twoy %>%
    arrange(!!category_var,!!timevar) %>%
    mutate(
      lag_x_var = !!x_var - lag(!!x_var),
      lag_y_var = !!y_var - lag(!!y_var)
    ) %>%
    filter(!!timevar == max(times)) %>% #using final year to mark when going in particular compass direction
    mutate(
      compass = case_when(
        lag_x_var < 0 & lag_y_var < 0 ~ 'SW', 
        lag_x_var < 0 & lag_y_var > 0 ~ 'NW',
        lag_x_var > 0 & lag_y_var > 0 ~ 'NE', 
        lag_x_var > 0 & lag_y_var < 0 ~ 'SE'
      ) 
    )
  
  twoy <- twoy %>%  
    left_join( 
      twoy_lags %>%
        select(!!category_var,compass),
      by = quo_name(category_var)
    )
  
  
  #Loop over all timepoint  pairs to plot as vectors
  current_years <- times[-length(times)]
  next_years <- times[-1]
  
  # Use mapply to create the pairs
  year_pairs <- mapply(c, current_years, next_years, SIMPLIFY = FALSE)
  
  # The list of year pairs
  # year_pairs

  p <- ggplot()
  
  #Check if last entry
  last = year_pairs[[length(year_pairs)]]
  
  for(i in year_pairs){
    
    cat('year pair: ', i, '\n')
  
    #Get those two years
    segment <- df %>%
      filter(
        !!timevar %in% i
      ) %>% 
      arrange(!!timevar)
  
    segment <- segment %>%  
      left_join(
        twoy_lags %>%
          select(!!category_var,compass),
        by = quo_name(category_var)
      )
    
    twoy.wide <- segment %>% filter(compass %in% compasspoints_to_display) %>%
      mutate(!!timevar := ifelse(!!timevar == min(!!timevar), 'start', 'end')) %>%
      select(!!category_var,!!timevar,!!x_var,!!y_var,!!label_var) %>%
      pivot_wider(names_from = !!timevar, values_from = c(!!x_var,!!y_var,!!label_var))
    
    
    #Rename wide two year for change vector back to generic names
    names(twoy.wide) <- c(names(twoy.wide)[1],'x_start','x_end','y_start','y_end','label_start','label_end')
    
    #Change arrow for last
    #Easiest with list just to test if both years correct
    if(mean(i == last)==1){
    
    p <- p + geom_segment(data = twoy.wide, aes(x = x_start, y = y_start , xend = x_end, yend = y_end),
                   arrow = arrow(length = unit(0.5, "cm")),
                   size = 1)
                   
    } else {
      
    p <- p + geom_segment(data = twoy.wide, aes(x = x_start, y = y_start , xend = x_end, yend = y_end),
                   # arrow = arrow(length = unit(0.5, "cm")),
                   size = 1)
      
    }
                   
  
  }#end for
  
  p <- p +
    geom_point(data = twoy %>% filter(compass%in%compasspoints_to_display), size = 5, alpha = 0.75,
               aes(x = !!x_var, y = !!y_var,colour = factor(!!timevar), group = !!category_var)) +
    # geom_line(data = twoy %>% filter(compass %in% compasspoints_to_display), size = 1, aes(x = !!x_var, y = !!y_var, group = !!category_var), colour = 'red') +
    # geom_abline(slope = 1, size = 1, colour='blue', alpha = 0.5) +
    # coord_cartesian(xlim = c(0.1,11), ylim = c(0.1,11)) + # good for log scale
    # scale_y_log10() +
    # scale_x_log10() +
    guides(colour=guide_legend(title=" ")) +
    xlab(quo_name(x_var)) +
    ylab(quo_name(y_var))
  # 
  # #Reduce to latest year and merge in values for labels
  label_df <- twoy %>% filter(!!timevar==max(!!timevar), compass %in% compasspoints_to_display) %>%
    left_join(
      twoy.wide %>% select(!!category_var,label_start,label_end)
    )
  
  p <- p + geom_text_repel(
    data = label_df,
    # data = twoy %>% filter(!!timevar==max(!!timevar), compass %in% compasspoints_to_display),
    aes(x = !!x_var, y = !!y_var,
        label = paste0(!!category_var, "\n(",quo_name(label_var),": ",round(label_start,2),ifelse(label_start < label_end," >> "," << "),round(label_end,2),")"),
        # label = paste0(!!category_var, "\n(",quo_name(x_var),": ",round(!!x_var,2),", ",quo_name(y_var),": ",round(!!y_var,2),")"),
        # label = paste0(!!category_var, "\n(",quo_name(x_var),": ",round(!!x_var,2),", ",quo_name(y_var),": ",round(!!y_var,2),")"),
        # label = paste0(!!category_var, "\n(x:",round(x_var,2),"%,y:",round(y_var,2),"%)"),
        colour = compass),
    alpha=1,
    nudge_x = .05,
    box.padding = 1,
    nudge_y = 0.05,
    segment.curvature = -0.1,
    segment.ncp = 0.3,
    segment.angle = 20,
    max.overlaps = 20
  ) +
    scale_color_manual(values = setNames(c("red", "black",'#7fc97f','#beaed4','#fdc086','#1f78b4'),
                                         c(min(times), max(times),'NE','SE','NW','SW')))
  
  p
  
  
}







twod_generictimeplot_normalisetozero__multipletimepoints <- function(df, category_var, x_var, y_var, timevar, label_var, times, compasspoints_to_display = c('NE','NW','SE','SW'), category_var_value_to_highlight="NULL"){
  
  category_var <- enquo(category_var)    
  x_var <- enquo(x_var)
  y_var <- enquo(y_var)
  timevar <- enquo(timevar)
  label_var <- enquo(label_var)
  
  #Though plotting multiple
  #Still use start and end point to get overall compass direction
  twoy <- df %>%
    filter(
      !!timevar %in% c(min(times), max(times))
    ) %>% 
    arrange(!!timevar)
  
  twoy_lags <- twoy %>%
    arrange(!!category_var,!!timevar) %>%
    mutate(
      lag_x_var = !!x_var - lag(!!x_var),
      lag_y_var = !!y_var - lag(!!y_var)
    ) %>%
    filter(!!timevar == max(times)) %>% #using final year to mark when going in particular compass direction
    mutate(
      compass = case_when(
        lag_x_var < 0 & lag_y_var < 0 ~ 'SW', 
        lag_x_var < 0 & lag_y_var > 0 ~ 'NW',
        lag_x_var > 0 & lag_y_var > 0 ~ 'NE', 
        lag_x_var > 0 & lag_y_var < 0 ~ 'SE'
      ) 
    )
  
  twoy <- twoy %>%  
    left_join( 
      twoy_lags %>%
        select(!!category_var,compass),
      by = quo_name(category_var)
    )
  
  #Loop over all timepoint  pairs to plot as vectors
  current_years <- times[-length(times)]
  next_years <- times[-1]
  
  # Use mapply to create the pairs
  year_pairs <- mapply(c, current_years, next_years, SIMPLIFY = FALSE)
  
  # The list of year pairs
  # year_pairs
  
  #Vectors all centred on zero, percent change for all shown
  #Annotate with a triangle indicating the half of the plot where GVA per worker will have dropped between time points
  p <- ggplot() +
    annotate(geom = "polygon", x = c(-1000, 1000, -1000), y = c(1000, 1000, -1000), fill = "white", alpha = 0.5)
  
  #Check if last entry
  last = year_pairs[[length(year_pairs)]]
  
  #store previous x and y, start at zero
  lastx <- 0
  lasty <- 0
  
  for(i in year_pairs){
    
    cat('year pair: ', i, '\n')
    
    #Get those two years
    segment <- df %>%
      filter(
        !!timevar %in% i
      ) %>% 
      arrange(!!timevar)
    
    segment <- segment %>%  
      left_join(
        twoy_lags %>%
          select(!!category_var,compass),
        by = quo_name(category_var)
      )
    
    twoy.wide <- segment %>% filter(compass %in% compasspoints_to_display) %>%
      mutate(!!timevar := ifelse(!!timevar == min(!!timevar), 'start', 'end')) %>%
      select(!!category_var,!!timevar,!!x_var,!!y_var,!!label_var) %>%
      pivot_wider(names_from = !!timevar, values_from = c(!!x_var,!!y_var,!!label_var))
    
    
    #Rename wide two year for change vector back to generic names
    names(twoy.wide) <- c(names(twoy.wide)[1],'x_start','x_end','y_start','y_end','label_start','label_end')
    
    
    #Make percent change values for the vector x and y values
    twoy.wide <- twoy.wide %>% 
      mutate(
        x_pct_change = percent_change(lastx + x_start, lastx + x_end),
        y_pct_change = percent_change(lasty + y_start, lasty + y_end),
        category_var_val_to_highlight = ifelse(!!category_var == category_var_value_to_highlight, T,F),
        lastx = lastx,#single zero should repeat
        lasty = lasty#single zero should repeat
      )
    
    
    #Change arrow for last
    #Easiest with list just to test if both years correct
    if(mean(i == last)==1){
      
      p <- p + geom_segment(data = twoy.wide, aes(x = lastx, y = lasty ,
                                                  xend = x_pct_change, yend = y_pct_change),
                            arrow = arrow(length = unit(0.5, "cm")),
                            size = 1)
      
      #REPEAT LOOP FOR SELECTED FEATURE
      #Highlight one place
      #Colour aes clashes with one below for labels, which I want to keep
      #Do hacky overlay instead
      p <- p + geom_segment(data = twoy.wide %>% filter(category_var_val_to_highlight), 
                            aes(x = lastx, y = lasty ,
                                xend = x_pct_change, yend = y_pct_change), 
                            arrow = arrow(length = unit(0.5, "cm")),
                            size = 2, colour = '#3333ff'
      )
      
      
    } else {
      
      p <- p + geom_segment(data = twoy.wide, aes(x = lastx, y = lasty ,
                                                  xend = x_pct_change, yend = y_pct_change),
                            # arrow = arrow(length = unit(0.5, "cm")),
                            size = 1)
      
      #REPEAT LOOP FOR SELECTED FEATURE
      #Highlight one place
      #Colour aes clashes with one below for labels, which I want to keep
      #Do hacky overlay instead
      p <- p + geom_segment(data = twoy.wide %>% filter(category_var_val_to_highlight), 
                            aes(x = lastx, y = lasty ,
                                xend = x_pct_change, yend = y_pct_change), 
                            # arrow = arrow(length = unit(0.5, "cm")),
                            size = 2, colour = '#3333ff'
      )
      
    }
    
    
    lastx <- twoy.wide$x_pct_change
    lasty <- twoy.wide$y_pct_change
    
    print(lastx)
    print(lasty)
    
  }#end for
  
  
  
  p <- p +
    # geom_segment(data = twoy.wide, aes(x = 0, y = 0 ,xend = x_pct_change, yend = y_pct_change), 
    #              # colour = category_var_val_to_highlight),
    #              arrow = arrow(length = unit(0.5, "cm")),
    #              size = 1, alpha = 0.5
    # ) +
    guides(colour=guide_legend(title=" ")) +
    xlab( paste0(quo_name(x_var),' percent change ',min(times),' to ',max(times)) ) +
    ylab( paste0(quo_name(y_var),' percent change ',min(times),' to ',max(times)) ) +
    geom_vline(xintercept = 0, alpha = 0.1, size =2, colour = 'red') +
    geom_hline(yintercept = 0, alpha = 0.1, size =2, colour = 'red') 
  
  
  
  # #Reduce to latest year and merge in values for labels
  label_df <- twoy %>% filter(!!timevar==max(!!timevar), compass %in% compasspoints_to_display) %>%
    left_join(
      twoy.wide %>% select(!!category_var,label_start,label_end,x_pct_change,y_pct_change,category_var_val_to_highlight)
    ) %>% 
    mutate(category_var_val_to_highlight = ifelse(category_var_val_to_highlight, 'bold','plain'))
  
  p <- p + geom_text_repel(
    data = label_df,
    # data = twoy %>% filter(!!timevar==max(!!timevar), compass %in% compasspoints_to_display),
    aes(x = x_pct_change, y = y_pct_change,fontface = category_var_val_to_highlight,
        label = paste0(!!category_var, "\n(",quo_name(label_var),": ",round(label_start,2),ifelse(label_start < label_end," >> "," << "),round(label_end,2),")"),
        # label = paste0(!!category_var, "\n(",quo_name(x_var),": ",round(!!x_var,2),", ",quo_name(y_var),": ",round(!!y_var,2),")"),
        # label = paste0(!!category_var, "\n(",quo_name(x_var),": ",round(!!x_var,2),", ",quo_name(y_var),": ",round(!!y_var,2),")"),
        # label = paste0(!!category_var, "\n(x:",round(x_var,2),"%,y:",round(y_var,2),"%)"),
        colour = compass),
    alpha=1,
    nudge_x = .05,
    box.padding = 1,
    nudge_y = 0.05,
    segment.curvature = -0.1,
    segment.ncp = 0.3,
    segment.angle = 20,
    max.overlaps = 20
  ) +
    scale_color_manual(values = setNames(c("red", "black",'#7fc97f','#beaed4','#fdc086','#1f78b4'),
                                         c(min(times), max(times),'NE','SE','NW','SW')))
  
  return(list(plot = p, twoyeardata = twoy.wide))
  
  
}
















#Pass in a desired confidence interval in percent
#Returns the correct +/- z score for applying to a standard error
getZScore <- function(confidence_percent) {
  # Convert the percentage to a proportion
  confidence_proportion <- confidence_percent / 100
  
  # Calculate the two-tailed probability
  alpha <- 1 - confidence_proportion
  
  # Calculate the one-tailed probability
  alpha_half <- alpha / 2
  
  # Get the Z-score for the one-tailed probability
  z_score <- qnorm(1 - alpha_half)
  
  return(z_score)
}



#Produce a ggplot visualisation of differences between OLS slopes (for time series here)
#Between pairs of slopes e.g. all sectors in SY (or another particular place) compared to each other
#Or all places paired for a particular sector
#filterval2 for comparing a different pair of slopes for two grid-columns e.g. two different places
#Will need naming if using two separate ones
slopeDiffGrid <- function(slope_df, confidence_interval, column_to_grid, column_to_filter, filterval, filterval2 = NULL){
  
  column_to_grid <- enquo(column_to_grid) 
  column_to_filter <- enquo(column_to_filter) 
  
  #Filter down to single thing (sector, place etc)
  #If looking at two filtervals on different axes...
  #Do first before overwriting slope_df
  if(!is.null(filterval2)) slope_df2 <- slope_df %>% filter(!!column_to_filter == filterval2)
  
  slope_df <- slope_df %>% 
    filter(!!column_to_filter == filterval)
  
  #Apply CIs
  ci <- getZScore(confidence_interval)
  
  # cat('CI = ',confidence_interval,', z score = ', ci, '\n')
  
  
  if(!is.null(filterval2)){
    
    slope_df2 <- slope_df2 %>% 
      mutate(
        min.ci = slope - (se * ci),
        max.ci = slope + (se * ci),
        crosses.zero = min.ci * max.ci < 0
      )
    
  }
  
  slope_df <- slope_df %>% 
    mutate(
      min.ci = slope - (se * ci),
      max.ci = slope + (se * ci),
      crosses.zero = min.ci * max.ci < 0
    )
  
  
  #Apply CI overlap test to all pairs
  #https://stackoverflow.com/a/3269471
  #If (StartA <= EndB) and (EndA >= StartB) 
  
  #Get all pair combos
  combos <- expand.grid(gridcol1 = slope_df[quo_name(column_to_grid)] %>% pull, gridcol2 = slope_df[quo_name(column_to_grid)] %>% pull)
  
  #Merge in two repeated sets of the values and CIs to check for CI overlap for each pair
  combos <- combos %>%
    left_join(
      slope_df %>% ungroup() %>% select(
        !!column_to_grid,
        slopeone = slope,
        min.cione = min.ci,
        max.cione = max.ci,
        crosses.zero.one = crosses.zero
      ),
      by = c('gridcol1' = quo_name(column_to_grid))
    )

  #Merge in two repeated sets of the values and CIs to check for CI overlap for each pair
  #If using a different place/sector etc for one of the matrix axes, do that here
  if(!is.null(filterval2)){
    
    combos <- combos %>%
      left_join(
        slope_df2 %>% ungroup() %>% select(
          !!column_to_grid,
          slopetwo = slope,
          min.citwo = min.ci,
          max.citwo = max.ci,
          crosses.zero.two = crosses.zero
        ),
        by = c('gridcol2' = quo_name(column_to_grid))
      )
    
  } else {
  
  combos <- combos %>%
    left_join(
      slope_df %>% ungroup() %>% select(
        !!column_to_grid,
        slopetwo = slope,
        min.citwo = min.ci,
        max.citwo = max.ci,
        crosses.zero.two = crosses.zero
      ),
      by = c('gridcol2' = quo_name(column_to_grid))
    )
  
  }

  # 
  # #Apply CI overlap test to all pairs
  # #https://stackoverflow.com/a/3269471
  # #If (StartA <= EndB) and (EndA >= StartB)
  combos <- combos %>% 
    mutate(CIs_overlap = ifelse(
      (.[,'min.cione'] <= .[,'max.citwo'] & .[,'max.cione'] <= .[,'min.citwo']) |
        (.[,'min.citwo'] <= .[,'max.cione'] & .[,'max.citwo'] <= .[,'min.cione'])  , 
      F,T)
      # CIs_overlap = factor(CIs_overlap, ordered = T, levels = c(F,T))
      ) %>% 
    mutate(
      slopediff = slopetwo - slopeone#Add in slope differences
    )
   
  #Zero point of scale
  #Though shouldn't be necessary here as symmetric differences across the matrix diagonal mean the scale is always an exact mirror
  valz <- c(range(combos$slopediff, na.rm = T), 0)
  scale_values <- function(x){(x-min(x))/(max(x)-min(x))}
  scaled <- scale_values(valz)
  zerocutoff <- scaled[3]
  
  
  #Use x and y axis names to mark the polarity of those individual slopes
  #And whether they cross zero
  combos <- combos %>% 
    mutate(
      slopecolour_x = case_when(
        !crosses.zero.one & slopeone > 0 ~ "#28da28",
        crosses.zero.one & slopeone > 0 ~ "#96c493",
        !crosses.zero.one & slopeone < 0 ~ "red",
        crosses.zero.one & slopeone < 0 ~ "#ffcccc"
      ),
      slopecolour_y = case_when(
        !crosses.zero.two & slopetwo > 0 ~ "#28da28",
        crosses.zero.two & slopetwo > 0 ~ "#96c493",
        !crosses.zero.two & slopetwo < 0 ~ "red",
        crosses.zero.two & slopetwo < 0 ~ "#ffcccc"
      )
    )
  
  
  #Get unique values from that for the column_to_grid column
  #And order so matches x y axis order
  #(Probably a neater way of doing this)
  slopecolours_x <- combos %>% 
    distinct(gridcol1, .keep_all = T) %>% 
    arrange(gridcol1) %>% 
    select(slopecolour_x) %>% 
    pull
  
  slopecolours_y <- combos %>% 
    distinct(gridcol2, .keep_all = T) %>% 
    arrange(gridcol2) %>% 
    select(slopecolour_y) %>% 
    pull
  
  
  #Add in yearly % change of slope and CIs from log on the left axis
  combos <- combos %>% 
    mutate(
      slopeone_percent = round((exp(slopeone) -1) * 100,1),
      min.cione_percent = round((exp(min.cione) -1) * 100,1),
      max.cione_percent = round((exp(max.cione) -1) * 100,1),
      slopetwo_percent = round((exp(slopetwo) -1) * 100,1),
      min.citwo_percent = round((exp(min.citwo) -1) * 100,1),
      max.citwo_percent = round((exp(max.citwo) -1) * 100,1),
    )
  
  #Using colour for grid outline for sig values doesn't quite work, it draws messily
  #Add as extra layer over the top instead
  p <- ggplot(combos, aes(
    # x = substr(gridcol1,0,30),
    x = paste0(substr(gridcol1,0,24),' (',slopeone_percent,'% CI: ',min.cione_percent,'%,',max.cione_percent,'%)'),
    y = paste0(substr(gridcol2,0,24),' (',slopetwo_percent,'% CI: ',min.citwo_percent,'%,',max.citwo_percent,'%)'), 
    fill= slopediff, colour = CIs_overlap)
    ) + 
    geom_tile(width = 0.8, height = 0.8, size = 1) +
    scale_fill_gradientn(
      colours = c("red", "white", "darkgreen"),
      values = c(0, zerocutoff, 1)#https://stackoverflow.com/a/58725778/5023561
    ) +
    theme(
      axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=0, colour = slopecolours_x),
      axis.text.y = element_text(colour = slopecolours_y)
      # axis.title.x=element_blank(),
      # axis.title.y=element_blank()
    ) +
    scale_color_manual(values = setNames(c('black','white'),c(F,T)), guide = 'none')
  
  #Add in axis labels if we're comparing two different things, else remove entirely
  if(!is.null(filterval2)) p <- p + xlab(filterval) + ylab(filterval2) else p <- p + theme(axis.title.x=element_blank(),axis.title.y=element_blank())
  
  p
    
}






#Sticking several things together for timeplots for ease of reuse
timeplot <- function(){
  
  
  
}












