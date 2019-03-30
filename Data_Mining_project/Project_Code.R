# Foreign Exchange Analysis
# The format is known as currency units per U.S. Dollar. Explained by example, each rate in the Euro column says how much U.S. 
# Dollar you had to pay at a certain date to buy 1 Euro. Hence, the rates in the column U.S. Dollar are always 1.
# Setting up Workspace
library(ggplot2)
library(dplyr)
library(RWeka)
library(zoo)
library(plotly)
library(lubridate)
library(reshape2)
library(caret)

# reading in data
setwd("C:/Users/chris/Desktop/Werk/Data Mining/Project/Currency")
data = read.csv("Currency_Exchange_Rates.csv")

# Exploring the dataset
summary(data)
str(data)

# cleaning
names(data) <- gsub("\\.", "", names(data))
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")

#LOCF can be used to optimize model and nullify NAs, will cut down on amount of data as well
sum(is.na(data))
data = na.locf(data)

#create new data set without dollar or date for model
df = data[, -1] # remove date
df = df[, -51] # by removing the dollar, the text above needs to be fully understood moivng forward.
head(df)

#melt data for visualiztion
meltdf <- melt(data, id=c("Date"))

#new visualization
melt_plot = ggplot(meltdf) +
  geom_line(aes(x = meltdf$Date,
            y = meltdf$value,
            colour = meltdf$variable)
  )
int_plot = ggplotly(melt_plot)
int_plot

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(df, SplitRatio = 7/10)
train = subset(df, split == TRUE)
test  = subset(df, split == FALSE)

#Decision Tree dfs
library(RWeka)
train_dt <- train
test_dt <- test
#Numeric to Nominal 
NN <-make_Weka_filter("weka/filters/unsupervised/attribute/NumericToNominal")
train_dt <- NN(data=train_dt, control = Weka_control(R="1-50"), na.action = NULL)
test_dt <- NN(data=test_dt, control = Weka_control(R="1-50"), na.action = NULL)
#DT Model
options(java.parameters = "-Xmx10000m")
m=J48(train_dt$UKPoundSterling~., data=train_dt, control = Weka_control(U=FALSE, M=2,
                                                        C=0.5))
e <- evaluate_Weka_classifier(m,numFolds = 10, seed = 1, class = TRUE)
pred=predict(m, newdata = testset, type=c("class"))

#Build Model using several diferent techniques 
library(randomForest)
set.seed(1234)
regressor = randomForest(x = train[-50],
                         y = train$UKPoundSterling,
                         ntree = 500,
                         nodesize = 3,
                         importance = TRUE)

plot(regressor)
#Prediction
results = test
set.seed(10)
y_pred_rf = predict(regressor, newdata = test)
results$rf_pred_2 = y_pred_rf
results$error_2 = results$UKPoundSterling - results$rf_pred_2
mean(results$error_2)

#caret 
set.seed(10)
rf = train(UKPoundSterling ~ ., train,
           method = 'cforest',
           trControl = trainControl(
             method = 'cv', number=10,
             verboseIter = TRUE
           ))

# Predicting a new result with Random Forest Regression
y_pred = predict(rf, newdata = test)
results = test
results$rf_pred = y_pred

#Looking at Error
results$error = results$UKPoundSterling - results$rf_pred
mean(results$error)

#Visualizng the results
library(ggplot2)
set.seed(10)
plot1=ggplot() +
  geom_point(aes(x = data$Date, y = data$UKPoundSterling),
             colour = 'red') +
  geom_line(aes(x = data$Date, y = predict(regressor, newdata = data)),
            colour = 'blue') +
  geom_line(aes(x = data$Date, y = predict(rf, newdata = data)),
            colour = 'green') +
  ggtitle('Random Forest Regressions on UK Sterling Foreign Exchnage Rate') +
  xlab('Date') +
  ylab('Currency Units per US Dollar')

int_plot_2 = ggplotly(plot1)
int_plot_2