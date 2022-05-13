MusicRecord<-read.csv(file.choose()) #load data
data <-read.csv(file.choose()) #load data


# How many songs does the dataset include for which the artist name is "Michael Jackson"?
table(MusicRecord$artistname == "Michael Jackson")
#install.packages("dplyr")
# Alternatively, use the pipe %>% function in "dplyr" package
library(dplyr)
library(ggplot2)


SongsTrain = MusicRecord %>% filter(year <= 2009)
SongsTest = MusicRecord %>% filter(year == 2010)

#non-predictors
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")

standardize <- function(column){
  sigma_sq = var(column)
  mu = mean(column)
  return( (column-mu)/sigma_sq)
}



# To remove these variables from your training and testing sets:
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]

### data analysis
# we look at the distribution of varibale pitch,
# majority of the data is close to a single value,
# therefore pitch does not provide new info for model --> remove
ggplot(data, aes(x=pitch)) + geom_histogram(bins = 100)



# build a logistic regression model 
log_mod = glm(formula = Top10 ~ timesignature + timesignature_confidence + 
                loudness + tempo_confidence + key + key_confidence + energy + 
                pitch + timbre_0_min + timbre_0_max + timbre_1_min + timbre_2_min + 
                timbre_3_max + timbre_4_min + timbre_4_max + timbre_5_min + 
                timbre_6_min + timbre_6_max + timbre_7_min + timbre_7_max + 
                timbre_8_min + timbre_10_min + timbre_10_max + timbre_11_min + 
                timbre_11_max, family = binomial, data = SongsTrain)

summary(log_mod)

# You can make predictions on the test set by using the command:
testPredict = predict(log_mod, newdata=SongsTest, type="response")

# Then, you can create a confusion matrix with a threshold of 0.15 by using the table command:
confusion.matrix<-table(SongsTest$Top10, testPredict >= 0.50)

# The accuracy of the model is? 
Count.correct<-confusion.matrix[1,1]+confusion.matrix[2,2]
Count.wrong<-confusion.matrix[1,2]+confusion.matrix[2,1]

acc<-Count.correct/(Count.correct+Count.wrong)


