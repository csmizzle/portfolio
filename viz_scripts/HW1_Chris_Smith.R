# Homework - 1 / Chris Smith 

# load data & workspace
#install.packages("ggplot2")
#install.packages("tidyverse")
#install.packages("plotly")
library(ggplot2)
library(tidyverse)
library(plotly)

filename <- file.choose()

df <- read.csv(fileanme)

# understand data 
str(df)

# stacked barchart 
fill_colors <- c()
for ( i in 1:length(df$store) ) {
  if (df$store[i] == 'Syracuse') {
    fill_colors <- c(fill_colors, "#921122")
  } else {
    fill_colors <- c(fill_colors, "#cccccc")
  }
}

df$color <- fill_colors

df2 <- data.frame(tapply(df$color, list(df$rep, df$color), FUN = length))
df2[is.na(df2)] <- 0
df2$color <- ifelse(df2$X.921122 ==0,"#921122","#cccccc")
df2$sales <-  with(df2, df2$X.921122 + df2$X.cccccc)
df2 <- add_rownames(df2, "rep")
df2$sales <-as.numeric(df2$sales)
d1 <- ggplot(df2, aes(x=df2$rep, y=df2$sales))+geom_bar(stat = "identity", fill = df2$color)
d1 <- d1 + labs(x = "Sales Rep", y=("Number of Sales"), title ='Number of Sales by Sales Rep')
d1

# part 2

g <- ggplot(df, aes(x=df$store,y=df$total.sale, fill = df$paper)) + 
  geom_bar(stat = "identity") + 
  xlab('Store') + 
  ylab('Total Sales') +
  ggtitle('Sales By Store') + 
  labs(fill='Paper Type')

g

# boxplot of sales across stores
g2 <- ggplot(df, aes(x=df$store,y=df$total.sale, fill = df$store)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=16, outlier.size=2) +
  xlab('Stores') + 
  ylab('Total Sales') + 
  ggtitle('Sales By Store') +
  labs(fill='Stores')

g2

# dist of sales by paper
g3 <- ggplot(df, aes(x=df$paper,y=df$total.sale, fill=df$paper)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=16, outlier.size=2) + 
  xlab('Sale Price') + 
  ylab('Count of Sales Price') + 
  ggtitle('Distribution of Sales By Paper Type') +
  labs(fill='Paper Types')

g3

# time series
time_series <- data.frame(table(df$year))

ggplot(time_series, aes(x=time_series$Var1,y=time_series$Freq, group =1))+
  geom_point()+
  geom_line()+
  xlab('Year')+
  ylab('Customer Count')