suppressWarnings(library(stringr))
suppressWarnings(library(httr))
suppressWarnings(library(plyr))
suppressWarnings(library(tm))
suppressWarnings(library(topicmodels))
suppressWarnings(library(syuzhet))
suppressWarnings(library(ggplot2))
suppressWarnings(library(scales))
suppressWarnings(library(reshape2))
suppressWarnings(library(dplyr))
suppressWarnings(library(tidyr))

# read in data
setwd("C:/Users/chris/Desktop/pitchfork")
df <- read.csv("reviews_final.csv")

# remove columns with no artists
df <- df[-which(df$artist == ""), ]

# replace special characters
df_clean = lapply(df, function(x) {
  x = gsub("â€™", "",x)
  x = gsub("â€œ", '"',x)
  x = gsub("â€", '"',x) 
  x = gsub(",", '',x) 
  x = gsub("^[[:space:]]*","",x)
  x = gsub("[[:space:]]*$","",x)
  x = gsub("--", '',x)
  x = gsub("-", '',x)
  x = gsub('"', '',x)
  x = gsub("'", "", x)
})
df_clean = data.frame(df_clean)
df2 = data.frame(df_clean)

# proper data types
str(df2)
df2$author <- as.character(df2$author)
df2$date_published <- as.character.Date(df$date_published)
df2$label <- as.character(df$label)
df2$artist <- as.character(df$artist)
df2$album <- as.character(df$album)
df2$genre <- as.character(df$genre)

# rating requires alittle tweaking
options(digits = 3)
df2$rating <- as.character(df2$rating)
df2$rating <- as.numeric(df2$rating)
df2 <- df2[which(df2$rating <= 10), ]

# check for NAs
sum(is.na(df2))
df2 <- na.omit(df2)

# EDA
library(sqldf)

# genres
genre <- data.frame(table(df2$genre))
sum(genre$Freq)
genre <- genre[which(genre$Freq> 300), ]
genre

# Pie Chart with Percentages
library(plotrix)
slices <- c(1599, 428, 607, 307, 358, 334, 834, 1260, 3744) 
lbls <- c("Electronic", "Electronic Rock", "Experimental", "Experimental Rock", "Folk/Country",
          "Metal", "Pop/R&B", "Rap", "Rock")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie3D(slices,labels=lbls,explode=0.1,
      main="Pie Chart of Genres with > 300 Reviews")

barplot(table(df$genre), main = 'Genre Count', xlab = 'Genre', ylab = "Count")

query <- "select genre, avg(rating) as avg_rating from df2 
          where genre = 'Electronic'
          or genre = 'Electronic,Rock'
          or genre = 'Experimental'
          or genre = 'Experimental Rock'
          or genre = 'Metal'
          or genre = 'Folk/Country'
          or genre = 'Metal'
          or genre = 'Pop/R&B'
          or genre = 'Rap'
          or genre = 'Rock'
          group by genre"

genre_rating <- sqldf(query)
genre_rating

# perfect scores

query <- "select genre, rating from df2 
          where genre = 'Electronic'
          or genre = 'Electronic,Rock'
          or genre = 'Experimental'
          or genre = 'Experimental Rock'
          or genre = 'Metal'
          or genre = 'Folk/Country'
          or genre = 'Metal'
          or genre = 'Pop/R&B'
          or genre = 'Rap'
          or genre = 'Rock'"
genre_rating <- sqldf(query)
genre_rating
perfect_scores <- genre_rating[genre_rating$rating == 10.0,]
barplot(table(perfect_scores$genre), main = 'Perfect Scores', xlab = 'Genres', ylab = 'Count')

# authors
authors <- data.frame(table(df$author))
colnames(authors) <- c('author', 'count')
authors <- authors[order(-authors$count),]
authors <- authors[1:20, ]

authors$author <- as.character(authors$author)
authors$author <- factor(authors$author, levels=unique(authors$author))
authors <- authors[order(-authors$count),]
p1 <- ggplot(authors, aes(x=author, y=count, fill = count))+
  geom_bar(stat='identity') +
  xlab('Authors') +
  ylab('# of Reviews') +
  coord_flip()
p1

# lets look at top 5 contributors 

query <- "select avg(rating) as average_rating, author from df2
          where author = 'Ian Cohen'
          or author = 'Stuart Berman'
          or author = 'Stephen M. Deusner'
          or author = 'Jayson Greene'
          or author = 'Philip Sherburne'
          group by author"

top_authors_rating <- sqldf(query)
p2 <- ggplot(data=top_authors_rating, aes(x=top_authors_rating$author, y=average_rating)) + 
  geom_bar(stat='identity')
p2

# trim df to genres I'm interested in
query <- "select * from df2 
          where genre = 'Electronic'
          or genre = 'Electronic,Rock'
          or genre = 'Experimental'
          or genre = 'Experimental Rock'
          or genre = 'Metal'
          or genre = 'Folk/Country'
          or genre = 'Metal'
          or genre = 'Pop/R&B'
          or genre = 'Rap'
          or genre = 'Rock'"
trimmed_df <- sqldf(query)
trimmed_df$mashed_date <- gsub('\\s+', '', trimmed_df$date_published)
#write.csv(trimmed_df, "cleaned_trimmed_review.csv")

#distribution of scores across genres 
p3 <- ggplot(trimmed_df, aes(rating)) +
      geom_histogram(bins = 20) +
      facet_wrap(~genre, ncol = 2, scales = "free_x")+
      ggtitle("Distribution of scores by Genre")
p3

p4 <- ggplot(trimmed_df, aes(rating, fill = genre)) +
      geom_histogram(bins=30)+
      ggtitle("Distribution of Scores by Genre")
library(plotly)
p4 <- ggplotly(p4)
p4
# Ian Cohen
query <- "select * from trimmed_df where author = 'Ian Cohen'"
cohen <- sqldf(query)
barplot(table(cohen$genre), col = heat.colors(12), main = 'Ian Cohen',xlab = "Genre",ylab = "# of Reviews")
summary(cohen$rating)
#write.csv(cohen, "cohen.csv")

# Stuart Berman
query <- "select * from trimmed_df where author = 'Stuart Berman'"
berman <- sqldf(query)
barplot(table(berman$genre), col = heat.colors(12), main = 'Stuart Berman')
summary(berman$rating)
#write.csv(berman, "berman.csv")

# Stephen M. Deusner
query <- "select * from trimmed_df where author = 'Stephen M. Deusner'"
deusner <- sqldf(query)
barplot(table(deusner$genre), col = heat.colors(12), main = 'Stephen M. Deusner')
summary(deusner$rating)
#write.csv(deusner, 'deusner.csv')

# Jayson Greene
query <- "select * from trimmed_df where author = 'Jayson Greene'"
greene <- sqldf(query)
barplot(table(greene$genre), col = heat.colors(12), main = 'Jayson Greene')
summary(greene$rating)
#write.csv(greene, "greene.csv")

# Philip Sherburne
query <- "select * from trimmed_df where author = 'Philip Sherburne'"
sherburne <- sqldf(query)
barplot(table(sherburne$genre), col = heat.colors(12), main = "Philip Sherburne")
summary(sherburne$rating)
#write.csv(sherburne, 'sherburne.csv')

# Rap
query <- "select * from trimmed_df where genre = 'Rap'"
rap <- sqldf(query)
rap$mashed_date <- gsub('\\s+', '', rap$date_published)
#write.csv(rap, "rap_all.csv")

# Average rating by year
library(stringr)
library(sqldf)
library(stringi)
rap$year = str_sub(rap$mashed_date, start=-4)
query <- "select avg(rating) as avg_rating, year from rap
          group by year"
avg_rap <- sqldf(query)
avg_rap = avg_rap[1:11, ]

# I Was wrong about the avgerage Hip Hop ratings
p5 <- ggplot()+
      geom_line(data=avg_rap, aes(x=year, y=avg_rating, size = 3),color='orange', group = 1)+
      geom_point(data=avg_rap, aes(x=year, y=avg_rating, size = 3),color='red')+
      ggtitle("Average Hip Hop Reviews 2008 - 2018")+
      xlab("Year")+
      ylab("Average Rating")+
      theme(text = element_text(size=15))
p5