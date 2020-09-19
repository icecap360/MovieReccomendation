library(tidyverse)
library(tidyr)
library(stringr)
library(caret)
loadData <- function() {
  #I have previously saved edx and validation into rds format
  return (list(readRDS('edx.rds'), readRDS('validation.rds')))
}
data <- loadData()
edx <- data[[1]]
validation <- data[[2]]
rm(data)


preprocessYear <- function(dat) {
  #this function extracts the year from the timestamps
  return (dat %>% mutate(year = format(as.Date.POSIXct(timestamp, origin = "1970-01-01"),'%Y') ))
}
edx <- preprocessYear(edx)
validation <- preprocessYear(validation)

#Now extract the various genres by making a genre column for each genre
genress <- c('Mystery','Western','Action','Animation','Fantasy',
             'IMAX','War','Comedy','Romance','Thriller','SciFi',
             'Musical','Western','Crime','Adventure','Documentary',
             'Drama', 'Horror', 'Children', 'None', 'FilmNoir')
preprocess_ExtractGenre<- function(d) {
  d <- d %>% mutate(Mystery = str_detect(genres, 'Mystery') , 
                    Western = str_detect(genres, 'Western') ,
                    Action = str_detect(genres, 'Action') , 
                    Animation = str_detect(genres, 'Animation') , 
                    Fantasy = str_detect(genres, 'Fantasy') ,
                    IMAX = str_detect(genres, 'IMAX') , 
                    War = str_detect(genres, 'War') , 
                    Comedy = str_detect(genres, 'Comedy') ,
                    Romance = str_detect(genres, 'Romance') ,
                    Thriller = str_detect(genres, 'Thriller') ,
                    SciFi = str_detect(genres, 'Sci-Fi') ,
                    Musical = str_detect(genres, 'Musical') ,
                    Western = str_detect(genres, 'Western') ,
                    Crime = str_detect(genres, 'Crime') ,
                    Adventure = str_detect(genres, 'Adventure') ,
                    Documentary = str_detect(genres, 'Documentary') ,
                    Drama = str_detect(genres, 'Drama') ,
                    Horror = str_detect(genres, 'Horror') ,
                    Children = str_detect(genres, 'Children'),
                    FilmNoir = str_detect(genres , "FilmNoir"),
                    None = str_detect(genres , "(no genres listed)")
  )
  return (d)
}
dedx <- preprocess_ExtractGenre(edx)
validation <- preprocess_ExtractGenre(validation)
rm(edx)
#Save this new preprocessed data
saveRDS(dedx, 'dedx.rds')
saveRDS(validation, 'validation.rds')

#Load preprocessed data
loadData <- function() {
  dedx <- readRDS('dedx.rds')
  test <- readRDS('validation.rds')
  valIndex <- createDataPartition(dedx$rating,times=1, p=0.2, list = F)
  train_set <- dedx[-valIndex , ]
  validation <- dedx[valIndex , ]
  return (list(train_set,validation, test )  )
}
data <- loadData()
train_set <- data[[1]]
validation <- data[[2]]
test <- data[[3]]
rm(data)


#Baseline model, mean rating
#I also initialize the residual and prediction columns
meanRating <- mean(train_set$rating)
train_set$predictions <- meanRating
train_set$residual <-  train_set$rating - train_set$predictions
valPredictions <- rep(meanRating , dim(validation)[1])
testPredictions <- rep(0 , dim(test)[1]) 
modelTable <- data.frame(name = 'Baseline' , 
                         rmse = RMSE(valPredictions , validation$rating))

#HELPER FUNCTIONS
addEffect <- function(groupName , oldValPrediction, oldTestPrediction , 
                      train_set, validation, test, regularizer=1.0) {
  #First, calculate the effect
  effectGroup <- train_set %>% group_by_(c(groupName)) %>% 
    summarize(eff = mean(residual ) * regularizer)
  #Later in the notebook I will be using a genre feature. Because I want the 
  #genre effect to only effect genre of that type, I must set the nonmatching 
  #genre effect to zero. 
  if (groupName %in% genress) {
    #remove the effect of not in genre
    effectGroup[effectGroup[,groupName]==FALSE, 'eff']=0
  }
  #Second, predict on all three data sets, and update the residuals
  train_set$predictions <- left_join(train_set,effectGroup,
                                     by=groupName)$eff + 
    train_set$predictions
  train_set <- train_set %>% mutate(residual = rating - predictions)
  modelPredictionsVal <- predictt(validation,groupName,effectGroup,
                                  oldValPrediction)
  modelPredictionsTest <- predictt(test,groupName,effectGroup,oldTestPrediction)
  
  return (list(valRMSE =RMSE(modelPredictionsVal, validation$rating) , 
               train = train_set, 
               valPredictions = modelPredictionsVal,
               testPredictions = modelPredictionsTest))
}
predictt <- function(data, groupName, effectGroup, oldPredictions) {
  effectPredicitons <- left_join(data , effectGroup, by=groupName) %>%
    .$eff
  effectPredicitons[is.na(effectPredicitons)] <- 0 #set the effect for data points
  #missing features to zero
  return (effectPredicitons + oldPredictions)
}

#Add a new nratings column which contains the number of ratings a movie got in total
#This will be used in one of the models
train_set <- train_set %>% group_by(movieId) %>% mutate(nratings = n())
validation <- validation %>% group_by(movieId) %>% mutate(nratings = n())
test <- test %>% group_by(movieId) %>% mutate(nratings = n())

#Combine the validation and training set into a new 'fulltrain' dataset.
#Also initialize the prediction and residual columns on the new 'fulltrain' dataset.
testPredictions <- rep(meanRating , dim(test)[1])
train_set <- train_set %>% select(-predictions, -residual)
fulltrain <- rbind(train_set, validation)
fulltrain$predictions <- meanRating
fulltrain$residual <-  fulltrain$rating - fulltrain$predictions
rm(train_set)

#Now add each effect in the same order and using the same regulariztion parameters
#as the training set model. However I have to reconstruct the 
#model on the combined train and validation sets. 
result <- addEffect('movieId' , valPredictions, testPredictions , 
                    fulltrain, validation, test)
fulltrain <- result$train
testPredictions <- result$testPredictions
result <- addEffect('userId' , valPredictions, testPredictions , 
                    fulltrain, validation, test)
fulltrain <- result$train
testPredictions <- result$testPredictions
result <- addEffect('nratings' , valPredictions, testPredictions , 
                    fulltrain, validation, test, 0.3)
fulltrain <- result$train
testPredictions <- result$testPredictions
result <- addEffect('year' , valPredictions, testPredictions , 
                    fulltrain, validation, test, 0.3)
fulltrain <- result$train
testPredictions <- result$testPredictions
result <- addEffect('None' , valPredictions, testPredictions , 
                    fulltrain, validation, test, 0.5)
fulltrain <- result$train
testPredictions <- result$testPredictions
cat('RMSE = ', RMSE(testPredictions  , test$rating)) 
test <- test %>% group_by(movieId) %>% mutate(nratings = n())