MusicRecord<-read.csv(file.choose()) #load data
data <-read.csv(file.choose()) #load data

library(dplyr)
library(ggplot2)
library(stats)
library(MASS)

##### helper function
standardize <- function(column){
  sigma_sq = var(column)
  mu = mean(column)
  return( (column-mu)/sigma_sq)
}

##### data analysis

# check for missing values
data[is.na(data)]

# we look at the distribution of varibale pitch,
# majority of the data is close to a single value,
# therefore pitch does not provide new info for model --> remove
ggplot(data, aes(x=tempo)) + geom_histogram(bins = 100)


# check for outliers

replace_outliers <- function(column){
  # This function replaces outliers with the upper and lower 
    # outlier boundaries
  second_quant = quantile(column, probs=0.25)
  third_quant = quantile(column, probs=0.75)
  iqr =  third_quant - second_quant
  
  upper = third_quant + 1.5*iqr
  lower = second_quant - 1.5*iqr
  
  n = length(column)
  
  for (i in 1:n){
  val = column[i]
  if (val > upper){
    column[i] = upper
  } else if ( val < lower ){
    column[i] = lower
  }
  }
  return (column)
}
##### data processing

# standardize data = center data and get equal variance
data$loudness <- standardize(data$loudness)
data$tempo <- standardize(data$tempo)
data$energy <- standardize(data$energy)

# replace the outliers for standardized variables
data$loudness <- replace_outliers(data$loudness)
data$tempo <- replace_outliers(data$tempo)
data$energy <- replace_outliers(data$energy)


SongsTrain = data %>% filter(year <= 2009)
SongsTest = data %>% filter(year == 2010)

#non-predictors
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")



# To remove these variables from your training and testing sets:
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]





# build a logistic regression model 
log_mod = glm(formula = Top10 ~ timesignature + timesignature_confidence +
                loudness + tempo_confidence + key + key_confidence + energy +
                pitch + timbre_0_min + timbre_0_max + timbre_1_min + timbre_2_min +
                timbre_3_max + timbre_4_min + timbre_4_max + timbre_5_min +
                timbre_6_min + timbre_6_max + timbre_7_min + timbre_7_max +
                timbre_8_min + timbre_10_min + timbre_10_max + timbre_11_min +
                timbre_11_max, family = binomial, data = SongsTrain)

log_mod = glm(formula = Top10 ~ . , family = binomial, data = SongsTrain) %>% stepAIC(trace=FALSE)



summary(log_mod)

# You can make predictions on the test set by using the command:
testPredict = predict(log_mod, newdata=SongsTest, type="response")

# Then, you can create a confusion matrix with a threshold of 0.15 by using the table command:
confusion.matrix<-table(SongsTest$Top10, testPredict >= 0.30)

# Getting true and false positive and negatives 
TP <- confusion.matrix[2,2]
TN <- confusion.matrix[1,1]
FP <- confusion.matrix[1,2]
FN <- confusion.matrix[2,1]

# Classifier Performance Metrics
accuracy = (TP + TN) / (TP + FP + TN + FN)
precision = TP / (TP + FP)
recall = TP / (TP + FN)
f1 = 2 * ( precision * recall) / (precision + recall)

results_2 = c(accuracy, precision, recall, f1)

print(results_2)

thresholds = seq.int(0,100,5)