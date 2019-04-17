##### Source code on Machine Learning Project On Cab fare Prediction Using R ##### 

##### Removing all the previous objects From R environment ##### 
rm(list=ls())

### Setting the working diretory and importing the required Dataset ###
setwd("D:/python & R/Projects/Cab Fare Prediction")
train_set=read.csv("train_cab.csv")
test_set =read.csv("test.csv")

######## Joining the train_set and test_set to form a new dataset ########
new_var= test_set$passenger_count
test_set$fare_amount = with(test_set,new_var)
test_set$fare_amount = NA
rm(new_var)
data =rbind(train_set,test_set)
######### Under standing the data ########
rm(train_set,test_set)

str(data)
summary(data)

####converting the fare_amount to to numerical ####
data$fare_amount = as.character(data$fare_amount)
data$fare_amount = as.numeric( data$fare_amount)

####removing the Pickup_datetime variable to avoid curse of dimensionality ####
data$pickup_datetime = NULL


##### Checking the data once again #####
str(data)
summary(data)


##### missing value analysis #####
missing_val = data.frame(apply(data,2,function(x){sum(is.na(x))}))


#### correcting the missing_val data frame ####
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_percentage"

#### Calculating the percentage of missing value and arranging in descending order ####
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(data)) * 100
missing_val = missing_val[order(-missing_val$Missing_percentage),]

##### Missing value imputation #####
#data[1,1]
#data[1,1]=NA
#actual value = 4.5
##mean = 15.01
## median = 8.5


### Median Method for missing value Imputation ###
data$fare_amount[is.na(data$fare_amount)] =median(data$fare_amount, na.rm= T)
data$pickup_longitude[is.na(data$pickup_longitude)] =median(data$pickup_longitude, na.rm= T)
data$pickup_latitude[is.na(data$pickup_latitude)] =median(data$ pickup_latitude, na.rm= T)
data$dropoff_longitude[is.na(data$dropoff_longitude)] =median(data$dropoff_longitude, na.rm= T)
data$dropoff_latitude[is.na(data$dropoff_latitude)] =median(data$dropoff_latitude, na.rm= T)
data$passenger_count[is.na(data$passenger_count)] =median(data$passenger_count, na.rm= T)


########Creating the new data frame after Missing value imputation Process######## 
imputed_value = data.frame(apply(data,2,function(x){sum(is.na(x))}))


########## outlier Analysis ##########
numeric_index = sapply(data,is.numeric) #selecting only numeric
numeric_data = data[,numeric_index]

cnames = colnames(numeric_data)
###### Removing outlier ######
#loop to remove from all variables
for(i in cnames){
  print(i)
  val = data[,i][data[,i] %in% boxplot.stats(data[,i])$out]
  print(length(val))
  data = data[which(!data[,i] %in% val),]
}


####replacing outliers with Na and imputing through Median####
for(i in cnames) {
  val = data[,i][data[,i] %in% boxplot.stats(data[,i])$out]
  #print(length(val))
  data[,i][data[,i] %in% val] = NA
}

sum(is.na(data))

### Median Method for missing value Imputation ###
data$fare_amount[is.na(data$fare_amount)] =median(data$fare_amount, na.rm= T)
data$pickup_longitude[is.na(data$pickup_longitude)] =median(data$pickup_longitude, na.rm= T)
data$pickup_latitude[is.na(data$pickup_latitude)] =median(data$ pickup_latitude, na.rm= T)
data$dropoff_longitude[is.na(data$dropoff_longitude)] =median(data$dropoff_longitude, na.rm= T)
data$dropoff_latitude[is.na(data$dropoff_latitude)] =median(data$dropoff_latitude, na.rm= T)
data$passenger_count[is.na(data$passenger_count)] =median(data$passenger_count, na.rm= T)



######feature Selection######

# checking VIF factor for numeric columns
library(usdm)
vif(data[,cnames])

###################### Building model ######################
#dividing the dataset into training set and test set

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(data$fare_amount, SplitRatio = 0.8)
training_set = subset(data, split == TRUE)
test_set = subset(data, split == FALSE)



####### Linear regression #######
# Fitting Linear Regression to the Training set
regressor = lm(formula = fare_amount ~ .,
               data = training_set)
# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)
#### Checking the summary of the Regressor ####
summary(regressor)





###### Decision Tree Regression ######
# Fitting Decision Tree Regression to the dataset
# install.packages('rpart')
library(rpart)
regressor = rpart(formula = fare_amount ~ .,
                  data = training_set,
                  control = rpart.control(minsplit = 1))

# Predicting a new result with Decision Tree Regression
y_pred = predict(regressor, test_set)






###### Random Forest Regression ######
# install.packages('randomForest')
library(randomForest)
set.seed(1234)
regressor = randomForest(x = training_set,
                         y = training_set$fare_amount,
                         ntree = 500)

# Predicting a new result with Random Forest Regression
y_pred = predict(regressor, test_set)
regressor
#### Checking the summary of the Regressor ####
regressor

plot(regressor)

