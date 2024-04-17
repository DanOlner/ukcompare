#GVA value decomposition explore
#Diewert, W. Erwin, and Kevin J. Fox. “Decomposing Value Added Growth into Explanatory Factors.” SSRN Scholarly Paper. Rochester, NY, January 20, 2017. https://doi.org/10.2139/ssrn.2913920.
#https://papers.ssrn.com/abstract=2913920
#https://github.com/shipei-zeng/dfvad](https://github.com/shipei-zeng/dfvad
#https://search.r-project.org/CRAN/refmans/dfvad/html/00Index.html]

#Was this the code used in that paper? The creator gets no mention (but they're from UNSW, or were...)
#Dates don't match, don't think so?

#Testing code from github
library(tidyverse)
library(reshape2)#old!
library(dfvad)

#Code copied from repo is working:

# Get the decomposition result
df <- value_decom(c("h2","x2"), c("w2","u2"), "y2", "p2", "year", mining)[[2]]
# Extract columns and rename
df_cmpt <- data.frame(df[,"period"], log(df[,c("T", "E", "C")]))
colnames(df_cmpt) <- c("year", "lnT", "lnE", "lnC")
df_tfp <- data.frame(df[,"period"], log(df[,"TFP"]))
colnames(df_tfp) <- c("year", "lnTFP")
# Set the colour scheme
palette_a <- c("goldenrod1", "seashell4", "red")
# Convert data into a tidy form
df_cmpt_tidy <- melt(df_cmpt, id.vars="year")
# Plot the components
plot_out <- ggplot(df_cmpt_tidy) + geom_bar(aes(x=year, y=value, fill=variable), stat="identity") +
  geom_line(data=df_tfp, aes(x=year,y=lnTFP,color='black'), lwd=0.5) +
  ylab('Log Index') + xlab('Year') + 
  scale_fill_manual("", values=palette_a) + 
  scale_colour_manual("", values=c('black'='black'), labels = c('lnTFP')) + 
  scale_x_continuous(breaks = seq(min(df$period), max(df$period), by = 3)) + 
  theme_classic()
print(plot_out)


#What can we glean from the data?
#Ah:
#"Sample Data for Weighted Average Aggregation": https://search.r-project.org/CRAN/refmans/dfvad/html/sector.html
View(dfvad::sector)
#"Sample Data for Value Added Decomposition": https://search.r-project.org/CRAN/refmans/dfvad/html/mining.html
View(dfvad::mining)
