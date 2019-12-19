rm(list=ls(all=T))

# setting the working directory 
setwd("C:/Users/Yashwanth/Desktop/Edwisor project")
# check the current working directory 
getwd()

# load required libraries 
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees','fastDummies', 'psych')

#install.packages(x)
lapply(x, require, character.only = TRUE)

library("dplyr")
library("plyr")
library("ggplot2")
library("data.table")
library("GGally")

#ls() lists all objects in the current workspace and 
#the aforementioned command removes all objects from the R memory.
#rm() is basically 'remove{base}'. Here we remove installed objects which is from variable x 
rm(x)

#load data into r 
df = read.csv("day.csv", header = T, na.strings = c(" ","","NA"))
# Summarizing  data 
# dim helps to see no of observations and variables in the dataset.
# dataset contains 731 obs. of 16 variables 
dim(df)

# getting first 5 rows of the datasets 
head(df, 5)

# target variables is "cnt" and all other variables are independent variables. 

# getting the column names of the dataset.
colnames(df)

# str of the data 
str(df)

# to get the quick summary of the dataset 
# we can observe the categorical variables are having mean, median, max, min etc which 
# is not appropriate. So we need to convert those categorical variable into factors. 
summary(df)

######Exploratory Data Analysis##################
# season, year, month, holiday, weekday. working day, weathersit, dteday all are 
# categorical variables. So, all those variables are converted into factors(i.e.,
# creating into levels. for example yr has 2 levels(2011 and 2012) and month has 12
# levels.

df$season = as.factor(df$season)
df$yr = as.factor(df$yr)
df$mnth = as.factor(df$mnth)
df$holiday = as.factor(df$holiday)
df$weekday = as.factor(df$weekday)
df$workingday = as.factor(df$workingday)
df$weathersit = as.factor(df$weathersit)
d1=unique(df$dteday)
df1=data.frame(d1)
class(df1)
df$dteday=as.Date(df1$d1,format="%Y-%m-%d")
df1$d1=as.Date(df1$d1,format="%Y-%m-%d")
df$dteday=format(as.Date(df1$d1,format="%Y-%m-%d"), "%d")
df$dteday=as.factor(df$dteday)

str(df)

# library psych to get the quick summary

describe(df)



############## univariate analysis##################
# plot and histgram helps us to find the distribution of data. 
# cnt is normally distributed 
plot(density(df$cnt))
hist(df$cnt, main = " cnt histogram" , xlab = 'cnt', ylab = "freq")

plot(density(df$temp))
hist(df$temp, main = " temp histogram" , xlab = 'temp', ylab = "freq")

plot(density(df$hum))
hist(df$hum, main = " hum histogram" , xlab = 'hum', ylab = "freq")

plot(density(df$windspeed))
hist(df$windspeed, main = " windspeed histogram" , xlab = 'windspeed', ylab = "freq")


# Visualize categorical Variable 'mnth' with target variable 'cnt'

ggplot(df, aes(x= mnth, y=cnt),fill="grey") + 
  stat_summary(fun.y="mean", geom="bar")

ggplot(df)+
  geom_histogram(aes(x=cnt,y=..density..),
                 fill= "grey")+
  geom_density(aes(x=cnt,y=..density..))

# Visualize categorical Variable 'holiday' 


ggplot(df) +
  geom_bar(aes(x=holiday),fill="grey")

# it is showing that almost all the  cycle rentals are happening  on holidays

# Visualize categorical Variable 'weekday' 

ggplot(df) +
  geom_bar(aes(x=weekday),fill="grey") 

# it is showing  counts are same on all weekdays

# Visualize categorical Variable 'weathersit' 

ggplot(df) +
  geom_bar(aes(x=weathersit), col = 5) 

# count  is more when  whether is " Clear, Few clouds, Partly cloudy, Partly cloudy"

# Visualize categorical Variable 'season'

ggplot(df) +
  geom_bar(aes(x=season), col = 5)

################### bivariate#############
#check the relationship between 'temp' and 'atemp' variable

ggplot(df, aes(x= temp,y=atemp)) +
  geom_point()+
  geom_smooth()
#This  graph is saying that very strong relationship  between 'temp' and 'atemp'

#check the relationship between 'temp' and 'hum' variable
ggplot(df, aes(x= temp,y=hum)) +
  geom_point()+
  geom_smooth()

# here  it is showing  Humidity is increses  till temparature is 0.7 and it is decreasing  gradually

#check the relationship between 'temp' and 'windspeed' variable
ggplot(df, aes(x= temp,y=windspeed)) +
  geom_point()+
  geom_smooth()

# it is showing that very less nagative   correlation between  temp and windspeed

#check the relationship between all numeric variable using pair plot

ggpairs(df[,c('atemp','temp','hum','windspeed','cnt')])

# that above plot stating that less  nagative relationship between
# 'cnt'-'hum'  and cnt-windspeed

# and there is strong positive relationship between 
# temp- cnt and  atemp-cnt


############### Missing value analysis ##############

sum(is.na(df$cnt))
missing_val = data.frame(apply(df, 2, function(x){sum(is.na(x))}))

missing_val$columns = row.names(missing_val)

row.names(missing_val) = NULL

names(missing_val)[1] = "missing_percentage"

missing_val$missing_percentage = (missing_val$missing_percentage/nrow(df))*100.

missing_val = missing_val[order(-missing_val$missing_percentage),]

missing_val = missing_val[,c(2,1)]

#So , no missing  values are presnt in the data set

################ outlier analysis ##################

boxplot(df$windspeed, main = "box plot @ windspeed", ylab = "windspeed", col = 5)
# we found some outlier present in windspeed. Those values are left as
# itis because of environmental properties. 


# checking outliers in casual variable. 
boxplot(df$casual, main = " outlier check @ casual", ylab = " casual", col = 5)
# or 
ggplot(data = df, aes(x = "", y = casual)) + 
  geom_boxplot() 
# there are few outliers present in the variable 

#Checking outliers in registered varibale 
boxplot(df$registered, main = "outlier @ registered", ylab = "registered", col = 5)
#or 
ggplot(data = df, aes(x = "", y =registered)) + 
  geom_boxplot()

## it is  showing that there is no outliers in  cnt variable

#Checking outliers in cnt varibale 

boxplot(df$cnt, main = "outlier @ cnt", ylab = "cnt", col = 5)

ggplot(data = df, aes(x = "", y = cnt)) + 
  geom_boxplot() 


boxplot(df$hum, main = "outlier @ hum", ylab = "hum", col = 5)

ggplot(data = df, aes(x = "", y = hum)) + 
  geom_boxplot() 

# it is  showing that there is no outliers in  cnt variable


# #################  Treat Outliers ##############

# analyse relationship between causal and cnt variables before  outlier treatment
ggplot(df, aes(x= casual,y=cnt)) +
  geom_point()+
  geom_smooth()

# #Remove outliers using boxplot method


val = df$casual[df$casual %in% boxplot.stats(df$casual)$out]

df = df[which(!df$casual %in% val),]

dim(df)

################# Feature selection ####################

numeric_index = sapply(df, is.numeric)

## Correlation Plot 
corrgram(df[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

# correlation matrix  stating  'temp' and 'atemp' having strong relationship.
# and also matrix shows that there is very less relation between hum and cnt.

## Dimension Reduction####
df = subset(df,select = -c(atemp, hum))


## instant variable is just a record of number of happenings, which will not contribute
# to analysis. 

df = subset(df, select = -c(instant))

dim(df)
colnames(df)

# dataset now has 687 observations with 13 variable. casual and registered are count
# of bike rents which is added to cnt (target varibale for analysis). so removing them. 


# dataset now has 687 obs. and 13 variables. 
dim(df)

###
###################### model development #####################

library(rpart)
library(MASS)

rmExcept("df")

# random sampling 
train_index = sample(1:nrow(df), 0.8 * nrow(df))
# train data (549 obs and 13 variables)
train = df[train_index,]
# test data (138 obs and 13 variables)
test = df[-train_index,]

Fit = rpart(cnt~., data = train, method = "anova")
print(Fit)

predictions_DT = predict(Fit, test[,-13])

print(Fit)

par(cex = 0.5)
plot(Fit)
text(Fit)

############# Evaluate  Decision tree ###################


#MAPE
#calculate MAPE
MAPE = function(y, yhat){
  mean(abs((y - yhat)/y))*100
}

MAPE(test[,13], predictions_DT)

#MAPE = 12.20

#Evaluate  Model using RMSE

RMSE = function(y_test,y_predict) {
  
  difference = y_test - y_predict
  root_mean_square = sqrt(mean(difference^2))
  return(root_mean_square)
  
}


RMSE(test[,13], predictions_DT)

#RMSE = 513.006

#############Random Forest Model##########################

RF_model = randomForest(cnt ~ ., train, importance = TRUE, ntree = 200)
importance(RF_model, type = 1)

RF_model

plot(RF_model)

predictions_RF = predict(RF_model, test[,-13])




MAPE(test[,13], predictions_RF)

#MAPE = 8.50

RMSE(test[,13], predictions_RF)

#RMSE = 310.53 


################Linear Regression################
library(usdm)
#converting multilevel categorical variable into binary dummy variable
cnames= c("dteday","season","mnth","weekday","weathersit")
data_lr=df[,cnames]
cnt=data.frame(df$cnt)
names(cnt)[1]="cnt"
data_lr = fastDummies::dummy_cols(data_lr)
dim(data_lr)
data_lr= subset(data_lr,select = -c(dteday,season,mnth,weekday,weathersit))
d3 = cbind(data_lr,df)
d3= subset(d3,select = -c(dteday,season,mnth,weekday,weathersit,cnt))
data_lr=cbind(d3,cnt)

#dividind data into test and train
train_index = sample(1:nrow(data_lr), 0.8 * nrow(data_lr))
train_lr = data_lr[train_index,]
test_lr = data_lr[-train_index,]

#Linear regression model making
lm_model = lm(cnt ~., data = train_lr)
predictions_LR = predict(lm_model,test_lr[,-65])


summary(lm_model)

MAPE(test_lr[,65], predictions_LR)

#Accuracy:  99.9 + accuracy

RMSE(test_lr[,65], predictions_LR)

#RMSE = 2.191067e-12

