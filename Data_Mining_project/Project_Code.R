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

# Fitting Simple Linear Regression to the Training set
regressor_lm = lm(formula = UKPoundSterling ~ .,
               data = train)

# Predicting the Test set results
y_pred = predict(regressor_lm, newdata = test)
summary(regressor_lm)

# select variables based on p-values from full model
myVars = c('AlgerianDinar','AustralianDollar','BolivarFuerte',
           'BotswanaPula','BrazilianReal','CanadianDollar',
           'ChileanPeso','ChineseYuan','CzechKoruna',
           'DanishKrone','Euro','HungarianForint','IndonesianRupiah',
           'IranianRial','JapaneseYen','KazakhstaniTenge',
           'KuwaitiDinar','MalaysianRinggit','MauritianRupee',
           'MexicanPeso','NorwegianKrone','NuevoSol',
           'PakistaniRupee','PesoUruguayo','SouthAfricanRand',
           'SriLankaRupee','ThaiBaht','TrinidadAndTobagoDollar',
           'TunisianDinar','UKPoundSterling')

# MSE for LM
#mse 
library(MLmetrics)
MSE(y_pred=y_pred,y_true = test$UKPoundSterling)

# trim for furhter models
train_trim = train[myVars]
test_trim = test[myVars]

# Fitting Simple Linear Regression to the trimmed Training set
regressor_lm_2 = lm(formula = UKPoundSterling ~ .,
                  data = train_trim)

# Predicting the Test set results
y_pred = predict(regressor_lm_2, newdata = test_trim)
summary(regressor_lm)

#mse for lm 2
library(MLmetrics)
MSE(y_pred=y_pred,y_true = test_trim$UKPoundSterling)

#Build Model using several diferent techniques 
library(randomForest)
set.seed(1234)
regressor_rf = randomForest(x = train_trim[-30],
                         y = train_trim$UKPoundSterling,
                         ntree = 500,
                         nodesize = 3,
                         importance = TRUE)

plot(regressor)

# Predicting a new result with Random Forest Regression
y_pred = predict(regressor_rf, newdata = test_trim)
results = test
results$rf_pred = y_pred

#mse 
library(MLmetrics)
MSE(y_pred=y_pred,y_true = test_trim$UKPoundSterling)

#Prediction
results = test
set.seed(10)
y_pred_rf = predict(regressor, newdata = test)
results$rf_pred_2 = y_pred_rf
results$error_2 = results$UKPoundSterling - results$rf_pred_2
mean(results$error_2)

#caret 
set.seed(10)
rf = train(UKPoundSterling ~ ., train_trim,
           method = 'cforest',
           trControl = trainControl(
             method = 'cv', number=10,
             verboseIter = TRUE
           ))

# Predicting a new result with Random Forest Regression
y_pred = predict(rf, newdata = test_trim)
results = test
results$rf_pred = y_pred

#Looking at Error
results$error = results$UKPoundSterling - results$rf_pred
mean(results$error)

# Predicting a new result with Random Forest Regression
y_pred = predict(rf, test_trim)
errors <- y_pred-test$UKPoundSterling
test_set$error <- errors
abs(mean(y_pred-test$UKPoundSterling))

#mse 
library(MLmetrics)
MSE(y_pred=y_pred,y_true = test_trim$UKPoundSterling)

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
  ggtitle('Random Forest & Linear Regressions on UK Sterling Foreign Exchnage Rate') +
  xlab('Date') +
  ylab('Currency Units per US Dollar')

int_plot_2 = ggplotly(plot1)
int_plot_2