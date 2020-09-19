# MovieRecommendation--Capstone

Creating a movie recommendation system using the MovieLens dataset, 10M version (contains 10 million ratings). This project was a part of my capstone for HarvardX Data Science certificate.   

The data set was about 150MB (too big to make an packaged-model on my PC) so a model based approach was taken. The final report is available at Report.pdf in the root directory. 

## Technologies used:  

* RMarkdown to create the pdf report 

* R 

* RStudio integrated development environment. 

* Various packages within R  

## Modelling 

After extensive exploration of the effect of various movies, users,  and genres, the following model was selected  

Linear regression could not be used to estimate the coefficients because the data set was too large for the computations on a personal computer. The coefficients were estimated by the mean of the residuals within a group, this approach is commonly known as a model based approach. 

**The main feature of this project is the complete report (“Report.pdf” in the root directory), which presents findings, how the model is constructed, model evaluation and limitations/future work.**  

## Directories: 

Data: Contains raw data and feature extraction. 

Exploration: Exploring and visualizing the effect of the predictors on movie ratings 

ModelExploration: Trying out  models and model evaluation. 

Final_Model_Construction: Building the final model. 

Report.rmd: RMarkdown file to make Report.pdf  
