library(dplyr)
library(ggplot2)

df <- read.csv('viz_data.csv')
head(df)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# non-medalists or medalists 
non_medal = subset(df, df$Medal == 'None')
medal = subset(df, df$Medal != 'None')

non_weight <- ggplot(non_medal, aes(x=non_medal$Sex,y=non_medal$Weight)) + 
  geom_boxplot(outlier.colour="black")+
  xlab('Sex')+
  ylab('Weight')+
  ggtitle('Weights of Female and Male Olympians Non Medalists')

medal_weight <- ggplot(medal, aes(x=medal$Sex,y=medal$Weight)) + 
  geom_boxplot(outlier.colour="black")+
  xlab('Sex')+
  ylab('Weight')+
  ggtitle('Weights of Female and Male Olympians Medalists')

medal_height <- ggplot(medal, aes(x=medal$Sex,y=medal$Height)) + 
  geom_boxplot(outlier.colour="black")+
  xlab('Sex')+
  ylab('Weight')+
  ggtitle('Heights of Female and Male Olympians Medalists')

non_height <- ggplot(non_medal, aes(x=non_medal$Sex,y=non_medal$Height)) + 
  geom_boxplot(outlier.colour="black")+
  xlab('Sex')+
  ylab('Weight')+
  ggtitle('Heights of Female and Male Olympians Non Medalists')

multiplot(non_height, medal_height, non_weight, medal_weight, cols = 2)

###################################################################
# USA Soviet Union Germany Great Britian France Italy Sweden Australia

time_series <- subset(df, df$Team == 'United States' | df$Team == 'Soviet Union' |
                        df$Team == 'Germany' | df$Team == 'Great Britain' |
                        df$Team == 'France' |df$Team == 'Italy' | df$Team == 'Sweden' |
                        df$Team == 'Australia' | df$Team == 'Canada' | df$Team == 'Hungary')

time_series_none <- subset(time_series, time_series$Medal != 'None')

medals_count <- time_series_none %>% 
  group_by(time_series_none$Team, time_series_none$Year, time_series_none$NOC) %>%
  count()

colnames(medals_count) <- c('country','year','NOC','count')

###################################################################
library(reshape)
gdp <- read.csv('API_NY.GDP.MKTP.CD_DS2_en_csv_v2_10576830.csv')
gdp_melt <- melt(gdp, id = c('Country.Code','ï..Country.Name'))
colnames(gdp_melt) <- c('country_code','name','year','value')
gdp_melt$year <- as.integer(gdp_melt$year)
gdp_melt$year <- as.character(gdp_melt$year)
gdp_melt$year <- gsub("X", "", gdp_melt$year)

medals_count_gdp <- left_join(medals_count, gdp_melt , by=c("NOC" = "country_code", 'year'='year'))

no_nulls <- na.omit(medals_count_gdp)
cor(no_nulls$count, no_nulls$value)