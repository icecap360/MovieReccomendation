library(tidyverse)
library(tidyr)
library(stringr)
library(caret)
loadData <- function() {
  dedx <- readRDS('dedx.rds')
  test <- readRDS('validation.rds')
  valIndex <- createDataPartition(dedx$rating,times=1, p=0.2, list = F)
  train_set <- dedx[-valIndex , ]
  validation <- dedx[valIndex , ]
  return (list(train_set,validation, test )  )
}
genress <- c('Mystery','Western','Action','Animation','Fantasy',
             'IMAX','War','Comedy','Romance','Thriller','SciFi',
             'Musical','Western','Crime','Adventure','Documentary',
             'Drama', 'Horror', 'Children', 'None', 'FilmNoir')
data <- loadData()
train_set <- data[[1]]
validation <- data[[2]]
test <- data[[3]]
rm(data)
#rm(test)

#Model 1, mean rating
#I was originally going to not include this, however because there
#are movies/users in the validation/test set that are not in the train
#set, the mean serves as a good baseline
meanRating <- mean(train_set$rating)
train_set$predictions <- meanRating
train_set$residual <-  train_set$rating - train_set$predictions

valPredictions <- rep(meanRating , dim(validation)[1])
testPredictions <- rep(0 , dim(test)[1]) #I will not use this until
#the end but because I created a single function for all 3 datasets I had to create a
#placeholder
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
findBestRegularizer <-function(groupName , oldValPrediction, oldTestPrediction , 
                               train_set, validation, test) {
  bestReg <- 1.0
  leastRMSE <- 1.0
  for (regularizer in c(1.0,0.75,0.5,0.3)) {
    result <- addEffect(groupName , oldValPrediction, oldTestPrediction , 
                        train_set, validation, test, regularizer)
    if (leastRMSE > result$RMSE) {
      leastRMSE <- result$RMSE
      bestReg <- regularizer
    }
  }
  return (bestReg)
} 
#Model 2, Movie Effect + Model 1
print(names(train_set))
result <- addEffect('movieId' , valPredictions, testPredictions , 
                    train_set, validation, test)
train_set <- result$train
valPredictions <- result$valPredictions
modelTable <- rbind(modelTable , data.frame(name='Movie Effect', rmse=result$RMSE))

#Model 3, User Effect + Model 2
result <- addEffect('userId' , valPredictions, testPredictions , 
                    train_set, validation, test)
train_set <- result$train
valPredictions <- result$valPredictions
modelTable <- rbind(modelTable , data.frame(name='User Effect', rmse=result$RMSE))

#Model 4, nratings + Model 3
#new nratings column is created
train_set <- train_set %>% group_by(movieId) %>% mutate(nratings = n())
validation <- validation %>% group_by(movieId) %>% mutate(nratings = n())
test <- test %>% group_by(movieId) %>% mutate(nratings = n())
reg <- findBestRegularizer('nratings' , valPredictions, testPredictions , 
                           train_set, validation, test)
result <- addEffect('nratings' , valPredictions, testPredictions , 
                    train_set, validation, test, 0.3)
train_set <- result$train
valPredictions <- result$valPredictions
testPredictions <- result$testPredictions
modelTable <- rbind(modelTable , data.frame(name='User Effect', rmse=result$RMSE))

#Model 5, year + Model 4
result <- addEffect('year' , valPredictions, testPredictions , 
                    train_set, validation, test, 0.3)
train_set <- result$train
valPredictions <- result$valPredictions



#Find the genress that the model is incorrectly predicting
for (i in genress) {cat (train_set %>% group_by_(c(i)) %>% summarize(mr=mean(residual)) %>% .$mr , i ,'\n') }
#The None genre has a high residuals

#Model 6, None(no genres) + Model 5
result <- addEffect('None' , valPredictions, testPredictions , 
                    train_set, validation, test, 0.5)
train_set <- result$train
valPredictions <- result$valPredictions




#Combine the validation and training set into a new 'fulltrain' dataset.
#Also create a prediction column, right now with just the average movie rating.
testPredictions <- rep(meanRating , dim(test)[1])
train_set <- train_set %>% select(-predictions, -residual)
fulltrain <- rbind(train_set, validation)
fulltrain$predictions <- meanRating
fulltrain$residual <-  fulltrain$rating - fulltrain$predictions
rm(train_set)


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