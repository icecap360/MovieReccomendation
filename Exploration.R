library(tidyverse)
library(tidyr)
library(stringr)
loadData <- function() {
  #I have previously saved edx and validation into rds format
  return (list(readRDS('edx.rds'), readRDS('validation.rds')))
  }
data <- loadData()
edx <- data[[1]]
validation <- data[[2]]
rm(data)


#EXPLORATION
#-What are the groups of users? high vs low raters, picky vs not picky

#How users rate movies on average follows a normal distribution 
#looking at the summary table, users on average deviate from there average rating 
#by 1 stars, which is not a lot. The vast majority of users on average rate between 3.4-3.8 stars
avgRatingHist <- edx %>% group_by(userId) %>% summarize(avgRating=mean(rating), 
            varRating=var(rating)) %>% ggplot(aes(avgRating)) +geom_histogram()+
  ggtitle('Users rate movies on a normal distribution')
summaryUserRating <- edx %>% group_by(userId) %>% summarize(avgRating=mean(rating), varRating=var(rating)) %>% 
  summarize(meanRating=mean(avgRating), varofRating=var(varRating), 
        meanOfVarRating=mean(varRating), varOfVarofRating=var(varRating) )
#How user ratings vary is right skewed, that is there are plenty of picky raters
varRatingHist <- edx %>% group_by(userId) %>% summarize(avgRating=mean(rating), 
                                                        varRating=var(rating)) %>%ggplot(aes(varRating))+  
  geom_histogram()+geom_vline(aes(xintercept=mean(varRating)))+ggtitle('Variance of user ratings histogram, vertical line is the mean')

#-What is the distributions of ratings
#there are a lot more full stars then half stars, and more higher rated
#movies then lower rated movies
ratingsDensity <- edx %>% ggplot(aes(rating)) + geom_bar()+ggtitle('There are more full then half star ratings')


#YEARLY TRENDS
#the ratings show trends year over year, particularly 1997,1999,1995,2008,2000 have high ratings on average
#However it is not that strong (except year 1995) as the mean stays within 3.4-3.6 stars
yearlyRating <- edx %>% mutate(date = as.Date.POSIXct(timestamp, origin = "1970-01-01")) %>% 
  mutate(year = format(date, '%Y')) %>% 
  group_by(year) %>% summarize(meanRt = mean(rating))
avgRatingOverTime <- ggplot(yearlyRating , aes(year, meanRt))+geom_point()

#numratings per year has levelled out over time
#1995 contains almost no ratings!
yearlyWatchers <- edx %>% mutate(date = as.Date.POSIXct(timestamp, origin = "1970-01-01")) %>% 
  mutate(year = format(date, '%Y')) %>% 
  group_by(year) %>% summarize(nratings = n ())
numRatingOverTime <- ggplot(yearlyWatchers , aes(year, nratings))+geom_point()
preprocessYear <- function(dat) {
  return (dat %>% mutate(year = format(as.Date.POSIXct(timestamp, origin = "1970-01-01"),'%Y') ))
}
edx <- preprocessYear(edx)
validation <- preprocessYear(validation)

#-Does when movie was came out effect rating
#No, when a movie comes out shows no distribution with ratings
movieReleaseInfluence<- edx %>% group_by(movieId) %>% summarize(cameout = 
        min(as.Date.POSIXct(timestamp)), 
        rat = mean(rating)) %>% ggplot(aes(cameout , rat))+geom_point()


#-Do ratings increase with time
#looking at the density the data is highly centered around the mean
#so the ratings show no correlation with when these were released
timeRating <- edx %>% group_by(movieId) %>% summarize(corr = 
              cor(timestamp,rating) )
timeHist <- timeRating %>% ggplot(aes(corr)) + geom_density()


#-What is relationship between movie ratings and number of ratings
#Yes, how many ratings a movie gets has a correlation with the number
#mean rating, especially with movies over 1000 ratings. The overall correlation
#is slightly positive, about 0.21
numRating<- edx %>% group_by(movieId) %>% summarize(numRating = n(), 
        mrating=mean(rating)) %>% ggplot(aes(mrating, 
          numRating )) + geom_point() + geom_smooth()
summ <- edx %>% group_by(movieId) %>% summarize(nratings=n() , mr=mean(rating))
correlationNumRatingAndRatinngs <- cor(summ$nratings, summ$mr)


#-Distribution of ratings among genres
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
#IMAX, War, Documentary, show extreme influence (0.3 stars)
#Crime, and mystery show slight positive influence (0.2 stars)
#while comedy action and scifi show a slight negative influence (-0.1 stars).
#about 0.2 stars. The others show no difference. However there may be some 
#confounding as genres that are more watched (more ratings) also tend to 
#have higher ratings
genress <- c('Mystery','Western','Action','Animation','Fantasy',
            'IMAX','War','Comedy','Romance','Thriller','SciFi',
            'Musical','Western','Crime','Adventure','Documentary',
            'Drama', 'Horror', 'Children', 'None', 'FilmNoir')
dedx <- preprocess_ExtractGenre(edx)
validation <- preprocess_ExtractGenre(validation)
rm(edx)
saveRDS(dedx, 'dedx.rds')
saveRDS(validation, 'validation.rds')

genreInfluenceSum <- sapply(genress , function(nm) {
  summ <- dedx %>% group_by_(.dots=c(nm)) %>% summarize(mr=mean(rating), 
                                                        nratings=n())
  trueIndices <- summ[,nm] == TRUE
  return (list(summ$mr[trueIndices]- summ$mr[!trueIndices] , 
               summ$nratings[trueIndices]    ))})
rownames(genreInfluenceSum) = c('gain', 'nratings')
genreInfluenceSum <- as.data.frame(t(genreInfluenceSum))
genreInfluenceSum$gain <- as.numeric(genreInfluenceSum$gain)
genreInfluenceSum$nratings <- as.numeric(genreInfluenceSum$nratings)
genreInfluenceSum$genre = rownames(genreInfluenceSum)

print(genreInfluenceSum)
genreGain <- genreInfluenceSum %>% ggplot(
  aes(x=nratings , y=gain, label=genre))+geom_label()



#-Distribution of genres among movies and users, heatmaps
