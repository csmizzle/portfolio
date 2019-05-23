# HW 2 - Chris Smith
# load data & workspace
#install.packages("ggplot2")
#install.packages("tidyverse")
#install.packages("plotly")
library(tidyverse)
library(ggplot2)
library(plotly)

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

# load data
filename <- file.choose()
df <- read.csv(filename, header = T)

# dists for total sale for water colors and drawing paper
g1 <- ggplot(df, aes(x=df$paper,y=df$total.sale, fill = df$paper)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=16, outlier.size=2) +
  xlab('Paper Type') + 
  ylab('Total Sales') + 
  ggtitle('Sales By Paper Type') +
  labs(fill='paper')

# growth over years for different 
g <- data.frame(tapply(df$year, list(df$year,df$store), sum))
g <- add_rownames(g, 'year')

g2 <- ggplot(g, aes(g$year, group=1)) + 
  geom_line(aes(y = g$Davenport, colour = "Davenport"))+
  geom_line(aes(y = g$Dublin, colour = "Dublin"))+
  geom_line(aes(y = g$Portland, colour = "Portland"))+
  geom_line(aes(y = g$Syracuse, colour = "Syracuse"))+
  xlab('Year ')+
  ylab('Total Sales')+
  ggtitle('Sales Growth Per Store')

# bar plot 
g3 <- ggplot(df, aes(x=df$store,y=df$total.sale, fill = df$paper)) + 
  geom_bar(stat = "identity") + 
  xlab('Store') + 
  ylab('Total Sales') +
  ggtitle('Sales By Store') + 
  labs(fill='Paper Type')

# 4 
watercolor <- data.frame(subset(df, df$paper =='watercolor'))
paper <- data.frame(subset(df, df$paper =='paper'))

g4 <- ggplot(watercolor, aes(x=watercolor$store,y=watercolor$total.sale, fill = watercolor$paper.type)) +
  geom_bar(stat = "identity") +
  xlab('Store Region') +
  ylab('Total Sales') +
  ggtitle('Total Sales by Paper Type - Watercolor') +
  labs(fill='Type of Paper')

# davenport - 
dav <- subset(df,df$store == 'Davenport')

g5 <- ggplot(dav, aes(x=dav$rep,y=dav$total.sale, fill = dav$paper)) + 
  geom_bar(stat = "identity") + 
  xlab('Rep') + 
  ylab('Total Sales') +
  ggtitle('Sales By Rep in Davenport') + 
  labs(fill='Paper Type')

# 6 
data <- data.frame(tapply(df$year, list(df$year,df$paper), sum))
data <- add_rownames(data, "year")
data$ratio_drawing <- with(data, data$drawing/(data$drawing+data$watercolor))
data$ratio_watercolor <- with(data, data$watercolor/(data$drawing+data$watercolor))


g6<-ggplot(data, aes(data$year, group=1)) +
  geom_line(aes(y = data$ratio_watercolor, colour = "watercolor"))+
  geom_line(aes(y = data$ratio_drawing, colour = "drawing"))+
  xlab('Year ')+
  ylab('Total Sales')+
  ggtitle('Sales Growth Per Store')

multiplot(g1, g2, g3, g4, g5, g6, cols = 2)
