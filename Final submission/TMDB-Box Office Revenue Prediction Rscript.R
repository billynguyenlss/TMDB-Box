#   SETTING UP ENVIRONMENT

# Chunk 1
# set up environment, standardize figure output
options(tinytex.verbose = TRUE)
knitr::opts_chunk$set(echo = TRUE, fig.height = 3.5, fig.width = 5,
                      fig.align = "center")
options(knitr.kable.NA = "")

# Chunk 2 - Set up environment
if(!require(matrixStats)) install.packages("matrixStats", 
                                           repos = "http://cran.us.r-project.org", 
                                           dependencies = TRUE)
if(!require(Amelia)) install.packages("Amelia", 
                                      repos = "http://cran.us.r-project.org", 
                                      dependencies = TRUE)
if(!require(gridExtra)) install.packages("gridExtra", 
                                         repos = "http://cran.us.r-project.org", 
                                         dependencies = TRUE)
if(!require(tidyverse)) install.packages("tidyverse", 
                                         repos = "http://cran.us.r-project.org", 
                                         dependencies = TRUE)
if(!require(caret)) install.packages("caret", 
                                     repos = "http://cran.us.r-project.org", 
                                     dependencies = TRUE)
if(!require(kableExtra)) install.packages("kableExtra", 
                                          repos = "http://cran.us.r-project.org", 
                                          dependencies = TRUE)
if(!require(TMDb)) install.packages("TMDb", 
                                    repos = "http://cran.us.r-project.org", 
                                    dependencies = TRUE)
if(!require(GGally)) install.packages("GGally", 
                                      repos = "http://cran.us.r-project.org", 
                                      dependencies = TRUE)
if(!require(randomForest)) install.packages("randomForest", 
                                            repos = "http://cran.us.r-project.org", 
                                            dependencies = TRUE)
if(!require(arm)) install.packages("arm", 
                                   repos = "http://cran.us.r-project.org", 
                                   dependencies = TRUE)
if(!require(mboost)) install.packages("mboost", 
                                      repos = "http://cran.us.r-project.org", 
                                      dependencies = TRUE)

# Chunk 3 - Import libraries
library(tidyverse)
library(caret)
library(matrixStats)
library(grDevices)
library(gridExtra)
library(Amelia)
library(DataExplorer)
library(knitr)
library(kableExtra)
library(lubridate)
library(corrplot)
library(TMDb)
library(GGally)
library(randomForest)
library(arm)
library(mboost)

# INTRODUCTION
## Competition Evaluation

# Chunk 4 
# write function RMSLE to evaluate modeling performance
RMSLE <- function(predicted_revenue, true_revenue){
  sqrt(mean((logb(true_revenue + 1)- logb(predicted_revenue + 1))^2))
}

# Chunk 5
# write function RMSL to evaluate modeling performance
RMSE <- function(predicted_revenue, true_revenue){
  sqrt(mean((true_revenue- predicted_revenue)^2))
}

# Chunk 6
# import leaderboard data
url_leaderboard <- url("https://raw.githubusercontent.com/billynguyenlss/TMDB-Box/master/data/tmdb-publicleaderboard.csv")
tmdb_leaderboard <- read.csv(url_leaderboard, na.strings=c("", '#N/A', '[]', '0'))
tmdb_leaderboard$SubmissionDate <- ymd_hms(tmdb_leaderboard$SubmissionDate)

# Chunk 7
final_board <- tmdb_leaderboard %>% group_by(TeamName) %>%
  summarize(submitted_times = n(),
            best_score = min(Score)) %>% arrange(best_score)

# Chunk 8
top_10 <- final_board[140,3]

# Chunk 9
final_board %>% 
  ggplot(aes(best_score)) +
  geom_histogram(fill = "steel blue", binwidth = 0.1) +
  scale_x_continuous(minor_breaks = seq(0, 30, 1)) +
  geom_vline(xintercept = 1.7068, color = "red") +
  geom_text(aes(x = 5, y = 240, label = "top 10% (RMSLE < 1.7068)"), color = "red") +
  labs(title = "TMDB Box office competition final leaderboard",
       caption = "Data source from Kaggle's TMDB Box office competition public leaderboard")

## DATA OVERVIEW
### **Kaggle competition's dataset**

# Chunk 10 - download raw data from my github repo
url_train <- 
  url("https://raw.githubusercontent.com/billynguyenlss/TMDB-Box/master/data/train.csv")
train_set <- read.csv(url_train, na.strings=c("", '#N/A', '[]', '0'))

url_test <- 
  url("https://raw.githubusercontent.com/billynguyenlss/TMDB-Box/master/data/test.csv")
test_set <- read.csv(url_test, na.strings=c("", '#N/A', '[]', '0'))

url_sample_submission <- 
  url("https://raw.githubusercontent.com/billynguyenlss/TMDB-Box/master/data/sample_submission.csv")
sample_submission <- read.csv(url_sample_submission, na.strings=c("", '#N/A', '[]', '0'))

# Chunk 13 - combine train_set and test_set to reduce time to pre-processing data
train_set <- train_set %>% mutate_if(is.factor, as.character)
test_set <- test_set %>% mutate_if(is.factor, as.character)
df <- bind_rows(train_set, test_set)

#### **The Movies Database API**

# Chunk 16 - download additional data from my github repo
url_additional <- url("https://raw.githubusercontent.com/billynguyenlss/TMDB-Box/master/data/personal%20additional%20data/full_additional_features_2.csv")
additional_data <- read.csv(url_additional, na.strings=c("", '#N/A', '[]', '0'))

#### **Wikipedia**

# Chunk 18 -  download data from my github repo
url_wikipedia_budget <- 
  url("https://raw.githubusercontent.com/billynguyenlss/TMDB-Box/master/wikipedia_us_budget.csv")

wikipedia_budget <- read.csv(url_wikipedia_budget, na.strings=c("", '#N/A', '[]', '0'))

# to assure the final budget as numeric type
wikipedia_budget$final_budget <- as.numeric(wikipedia_budget$final_budget)

# ANALYSIS
# Revenue

# Chunk 20 - convert to date-and-time format
df$release_date <- mdy(df$release_date)
summary(df$release_date)

# Chunk 21 - left joining the addition data into df
df <- df %>% left_join(additional_data, by = "imdb_id")
df$new_release_date <- as.character(df$new_release_date)

df$release_date <- df$new_release_date
df$release_date <- mdy(df$release_date)

# Chunk 23
df <- df %>%
  mutate(new_revenue = ifelse(is.na(new_revenue), revenue, new_revenue))

df$revenue[1:3000] <- df$new_revenue[1:3000]

# extract the true_rating value from additional data
# replace any NA value by median value
test_y <- test_set %>% left_join(additional_data, by = "imdb_id") %>%
  mutate(new_revenue = ifelse(is.na(new_revenue), median(new_revenue, na.rm = T), new_revenue)) %>%
  pull(new_revenue)

### **Budget**
# Chunk 28
# joining addition wikipedia budget
df <- df %>% mutate(imdb_id = as.factor(imdb_id)) %>%
  left_join(wikipedia_budget[,c(1,2,5)],by = "imdb_id")


#replace low budget value
df <- df %>% 
  mutate(new_budget = ifelse((new_budget <= 1000 & revenue > 10000), final_budget, new_budget))

# replace NA budget value by wikipedia budget
df <- df %>% mutate(new_budget = ifelse(is.na(new_budget),
                                        final_budget, new_budget))

# Chunk 29
# create summary budget table by release year
df$release_year <- year(df$release_date)

budget_by_year <- df %>%
  group_by(release_year) %>%
  summarize(avg_budget = mean(new_budget, na.rm = T),
            median_budget = median(new_budget, na.rm = T))

# replace NA budget by median_budget value
df <- df %>% left_join(budget_by_year, by = "release_year") %>%
  mutate(new_budget = ifelse(is.na(new_budget), median_budget,
                             new_budget))
df$new_budget[is.na(df$new_budget)] <- df$median_budget

# Chunk 30
#replace low budget value
df <- df %>% 
  mutate(budget = ifelse((budget <= 1000 & revenue > 10000), final_budget, budget))

# replace NA budget value by wikipedia budget
df <- df %>% mutate(budget = ifelse(is.na(budget),
                                    final_budget, budget))

# Chunk 31
# replace NA budget by median_budget value
df$budget[is.na(df$budget)] <- median(df$budget, na.rm = T)

# re-check NA value in budget
mean(is.na(df$new_budget))

### **Runtime**
# Chunk 32 - replace NA by median value
df$runtime[is.na(df$runtime)] <- median(df$runtime, na.rm = T)

# Chunk 34 - calculate average revenue
average_revenue <- mean(df$new_revenue, na.rm = TRUE)

### **Profit ratio & Return on Investment (ROI)**
# Chunk 35 -  calculate average_profit_ratio
avg_profit_ratio <- sum(df$revenue[1:3000], na.rm = T)/sum(df$new_budget[1:3000])

# Chunk 39 - add outlier vector of extremely high ROI
outlier_rows <- c(1231,1680)

### **Correlation between revenue and budget, popularity, runtime**
# Chunk 43 - add popularity outier rows to outlier_rows vector
outlier_rows <- c(outlier_rows, which(df$new_popularity > 100))

### **Change across time**
# Chunk 45 - calculate the release month
df$release_month <- month(df$release_date)
df$weekday <- weekdays(df$release_date)

### **belongs_to_collection**
# Chunk 47
# extract collection
df$collection <- str_extract(df$belongs_to_collection, 
                             pattern = "(?<=name\\'\\:\\s{1}\\').+(?=\\'\\,\\s{1}\\'poster)")
df$collection[is.na(df$collection)] <- "no collection"

### **genres**
# Chunk 50 - calculate number of genres per movie
df$number_genres <- str_count(df$genres, pattern = "id")
df$number_genres[is.na(df$number_genres)] <- 0

# Chunk 52 - replace NA value by "no genre"
df$genres[is.na(df$genres)] <- "no genre"

# Chunk 53
# create a vector with all genre levels
genres_matching_point <- "Comedy|Horror|Action|Drama|Documentary|Science Fiction|
              Crime|Fantasy|Thriller|Animation|Adventure|Mystery|War|Romance|Music|
              Family|Western|History|TV Movie|Foreign"

# extract the main genre from genres

df$main_genre <- str_extract(df$genres, genres_matching_point)
df$main_genre[is.na(df$main_genre)] <- "no genre"

### **production_companies**
# Chunk 56
# calculate number of company of a movie
df$number_of_company <- str_count(df$production_companies, pattern = "\\'name\\'")

# replace NA number by median
df$number_of_company[is.na(df$number_of_company)] <- median(df$number_of_company, na.rm = TRUE)

# Chunk 58
# remove all unnecessary character and keep only production company's name
df$companies <- gsub("(\\[?\\{\\'name\\'\\:\\s\\')|(\\'\\,\\s{1}\\'id\\'\\:\\s{1}\\d+\\}\\]?)",
                     "",df$production_companies)

# replace NA value in feature companies by "no production companies info"
df$companies[is.na(df$companies)] <- "no production companies info"

# create a list of all production companies
production_companies <- strsplit(df$companies, ", ") 
production_companies <- unlist(production_companies, use.names=FALSE)

# Chunk 59
# extract first company
df$first_company <- gsub("\\,\\s{1}.*","",df$companies)

# Chunk 60
# calculate the total revenue from train data
total_revenue <- sum(train_set$revenue)

# create a summary table by first company
first_company_summary <- df[] %>% group_by(first_company) %>%
  summarize(movies_per_company = n(),
            avg_budget_per_company = mean(budget, na.rm = TRUE),
            avg_revenue_per_company = mean(revenue, na.rm = TRUE),
            ROI_per_company = round((mean(revenue, na.rm = TRUE) - 
                                       mean(budget, na.rm = TRUE))/
                                      mean(budget, na.rm = TRUE),3))

### **production_countries**
# Chunk 63
# extract the first production country
df$country <- gsub("(\\[?\\{\\'iso\\_3166\\_1\\'\\:\\s{1}\\')|(\\'\\,\\s{1}\\'name.*\\}\\]?)",
                   "",df$production_countries)
df$country[is.na(df$country)] <- "no country info"

# create summary table by first production country
country_summary <- df %>% mutate(country = factor(country)) %>%
  group_by(country) %>%
  summarize(count = n(),avg_revenue = mean(revenue, na.rm = T)) %>%
  arrange(desc(count))

# Chunk 64 - Create vector of top production countries
top_countries <- c("US","GB","CA","DE","AU","JP","CN","no country info")

## **Features engineering**
### **Time-effect: Normallized popularity**

# CHunk 65
# create the summarized table for popularity
df$popularity <- df$new_popularity

popularity_sum <- df %>%
  group_by(release_year) %>%
  summarize(avg_popularity = mean(new_popularity, na.rm = T))
# create new normallized popularity
df <- df %>%
  left_join(popularity_sum, by = "release_year") %>%
  mutate(normallized_popularity = new_popularity/avg_popularity)

# Chunk 66
# create 2nd summarize table for normallized_popularity
norm_popularity_sum <- df %>%
  group_by(release_year) %>%
  summarize(max_norm_pop = max(normallized_popularity, na.rm = T))
# add 2nd normallized popularity
df <- df %>%
  left_join(norm_popularity_sum, by = "release_year") %>%
  mutate(second_norm_popularity = normallized_popularity/max_norm_pop)

### **Budget and popularity interaction**
# Chunk 67
df <- df %>%
  # add interaction features between variables
  mutate(budget_pop = budget*new_popularity,
         budget_norm_pop = budget*normallized_popularity)

### **Expected revenue based on average profit ratio**
# Chunk 68
# calculate expected revenue based on average profit
df <- df %>%
  mutate(expected_revenue = new_budget*avg_profit_ratio)

### **Genres**
# Chunk 69
# create a vector contain all genre name
genres <- levels(factor(df$main_genre))

# Chunk 70
# create features for each genre
for (i in 1:19){
  df[,genres[i]] <- ifelse(str_detect(df$genres,genres[i]),
                           1,0)
}

# calculate the column index for genres
from_genre <- grep("Action", colnames(df))
to_genre <- grep("Mystery", colnames(df))

### **Top production companies**
# CHunk 71
# create top production companies vector
top_production_companies <- first_company_summary %>% 
  arrange(desc(movies_per_company)) %>% head(10) %>%
  pull(first_company)

# create Dummy features for each production company
for (i in 1:length(top_production_companies)){
  df[,top_production_companies[i]] <- 
    ifelse(str_detect(df$production_companies,top_production_companies[i]),
           1,0)
}

df$`no production companies info` <-
  ifelse(str_detect(df$companies, "no production companies info"),1,0)

from_company <- grep(head(top_production_companies,1), colnames(df))

to_company <- grep(tail(top_production_companies,1), colnames(df))

df$other_production_company <-
  ifelse(rowMeans((df[,from_company:to_company])) >0,0,1)

for (i in from_company:to_company){
  df[is.na(df[,i]),i] <- 0
}

### **Top production countries**
# create Dummy features for each production country
for (i in 1:length(top_countries)){
  df[,top_countries[i]] <- 
    ifelse(str_detect(df$production_countries,top_countries[i]),
           1,0)
}

df$`no country info` <-
  ifelse(str_detect(df$first_company, "no country info"),1,0)

# determine the column index of production country features
from_country <- grep(head(top_countries,1), colnames(df))

to_country <- grep(tail(top_countries,1), colnames(df))

# create feature other_production_countries
df$other_production_countries <-
  ifelse(rowMeans((df[,from_country:to_country])) >0,0,1)

# replace any NA value by zero
for (i in from_country:to_country){
  df[is.na(df[,i]),i] <- 0
}

### **Features selection**
# Chunk 73
# selecting features
dat <- df %>% 
  mutate(revenue_pop = expected_revenue*new_popularity,
         revenue_norm_pop = expected_revenue*normallized_popularity) %>%
  dplyr::select(revenue,
                expected_revenue, revenue_pop, revenue_norm_pop,
                new_budget,
                budget_pop,
                budget_norm_pop,
                new_popularity, normallized_popularity,
                collection_status,
                number_of_company, 
                number_genres,
                from_genre:to_genre,(to_genre + 2):(to_genre + 7),
                from_company:to_company, to_company + 1,
                from_country:to_country, to_country + 1,
                release_year, release_month, weekday)

# replace NA
for (i in 2:(ncol(dat)-1)){
  dat[is.na(dat[,i]),i] <- median(dat[,i], na.rm = T)
}

# Chunk 74
# create train data
dat_train <- dat[1:3000,]

# replace outlier in train data
dat_train <- dat_train[-outlier_rows,]
dat_train <- filter(dat_train, new_budget > 1000 & revenue > 1000)

# create test data
dat_test <- dat[3001:7398,]

# Chunk 75
# create data for cross validation in modeling training
index <- createDataPartition(dat_train$revenue, times = 1, p = 0.8, list = FALSE)
train_set <- dat_train[index,]
test_set <- dat_train[-index,]

## **Modeling**
### **Design of experiment**

### **Experiment 1: Naive model**
# Chunk 76
predicted_revenue <- rep(mean(train_set$revenue, na.rm = T), times = nrow(test_set))

results_exp1 <- data.frame(Exp_No = 1,
                           Experiment = "Naive model",
                           normal_calculation = RMSLE(predicted_revenue, test_set$revenue))

### **Experiment 2: Best ML methods**
# Chunk 77
# modeling with different methods
# NOTED: this process will take some minute
fit_rf_exp2 <- train(revenue ~ ., 
                     data = train_set,
                     method = "rf",
                     importance = TRUE,
                     verbose = TRUE,
                     trControl = trainControl(method = "cv",
                                              number = 5,
                                              p = 0.8))

fit_bayesglm_exp2 <- train(revenue~.,
                           data = train_set,
                           method = "bayesglm",
                           trControl = trainControl(method = "cv",
                                                    number = 5,
                                                    p = 0.8))
fit_glmboost_exp2 <- train(revenue~.,
                           data = train_set,
                           method = "glmboost",
                           trControl = trainControl(method = "cv",
                                                    number = 5,
                                                    p = 0.8))
fit_lm_exp2 <- train(revenue~.,
                     data = train_set,
                     method = "lm",
                     trControl = trainControl(method = "cv",
                                              number = 5,
                                              p = 0.8))

# Chunk 78
# calculate predicted revenue
yhat_rf <- predict(fit_rf_exp2, newdata = test_set)
yhat_bayesglm <- predict(fit_bayesglm_exp2, newdata = test_set)
yhat_glmboost <- predict(fit_glmboost_exp2, newdata = test_set)
yhat_lm <- predict(fit_lm_exp2, newdata = test_set)

# Chunk 79
# ensembles
ensembles <- data.frame(rf = yhat_rf,
                        bayesglm = yhat_bayesglm,
                        glmboost = yhat_glmboost,
                        lm = yhat_lm)

# Chunk 80
# to replace negative predicted revenue by median
ensembles$bayesglm[ensembles$bayesglm < 0] <- median(ensembles$bayesglm)
ensembles$glmboost[ensembles$glmboost < 0] <- median(ensembles$glmboost)
ensembles$lm[ensembles$lm < 0] <- median(ensembles$lm)

# Chunk 81
results_exp2<- data.frame(Exp_No = 2,
                          Experiment = "Evaluate machine learning methods",
                          rf = RMSLE(ensembles$rf, test_set$revenue),
                          bayesglm = RMSLE(ensembles$bayesglm, test_set$revenue),
                          glmboost = RMSLE(ensembles$glmboost, test_set$revenue),
                          lm = RMSLE(ensembles$lm, test_set$revenue))


### **Experiment 3: Features selection**

#### **Feature Selection using Univariate Filters**
# Chunk 82
# use sbf function from Caret to select features
# NOTED: this process will take few minutes
filterCtrl <- sbfControl(functions = rfSBF, method = "repeatedcv", repeats = 5)
set.seed(10)
rfWithFilter <- sbf(train_set[,-1], train_set$revenue, sbfControl = filterCtrl)

# Chunk 85 - calculate predicted revenue
yhat_sbf <- predict(rfWithFilter, test_set)
results_exp3 <-data.frame(Exp_No = 3,
                          Experiment = "Feature selection",
                          rf = RMSLE(yhat_sbf, test_set$revenue))


#### **Confounding between variables**

# Chunk 87 - confounding models experiments
# training 3 models: all features, budget_group features, revenue_group features
fit_all_features <- train(revenue ~ ., 
                          data = train_set,
                          method = "rf",
                          importance = TRUE,
                          verbose = TRUE,
                          trControl = trainControl(method = "cv",
                                                   number = 5,
                                                   p = 0.8))

fit_revenue_exp2 <- train(revenue ~ ., 
                          data = train_set[,-c(5,6,7)],
                          method = "rf",
                          importance = TRUE,
                          verbose = TRUE,
                          trControl = trainControl(method = "cv",
                                                   number = 5,
                                                   p = 0.8))

fit_budget_exp2 <- train(revenue ~ ., 
                         data = train_set[,-c(2,3,4)],
                         method = "rf",
                         importance = TRUE,
                         verbose = TRUE,
                         trControl = trainControl(method = "cv",
                                                  number = 5,
                                                  p = 0.8))

# calculate the yhat of 3 models
yhat_all_features <- predict(fit_all_features, test_set)
yhat_revenue <- predict(fit_revenue_exp2, test_set)
yhat_budget <- predict(fit_budget_exp2, test_set)

### **Experiment 4: Logarithm transformation**
# Chunk 89
# logarit transforming 
for (i in 1:8){
  train_set[,i] <- logb(train_set[,i]+1)
  test_set[,i] <- logb(test_set[,i]+1)
}

# Chunk 91
# experiment 4: training model
filterCtrl <- sbfControl(functions = rfSBF, method = "repeatedcv", repeats = 5)
set.seed(10)
rf_with_filter_logb <- sbf(train_set[,-1], train_set$revenue, sbfControl = filterCtrl)

# Chunk 92
# experiment 4: predict revenue
yhat_exp4 <- predict(rf_with_filter_logb, test_set)
results_exp4 <- data.frame(Exp_No = 4,
                           Experiment = "Logarit transformation",
                           rf = RMSE(yhat_exp4, test_set$revenue))

# **Results & discussion**

## **Final models for validation**
# CHunk 95
# logarit transforming 
for (i in 1:8){
  dat_train[,i] <- logb(dat_train[,i]+1)
  dat_test[,i] <- logb(dat_test[,i]+1)
}

### **Model 1: Feature Selection using Univariate Filters**
# CHunk 96 - training model
filterCtrl <- sbfControl(functions = rfSBF, method = "repeatedcv", repeats = 5)
set.seed(10)
final_model_1 <- sbf(dat_train[,-c(1,3,4,5,6,7)], dat_train$revenue, sbfControl = filterCtrl)

# CHunk 97
# calculate predicted revenue by model 1
yhat_final_1 <- predict(final_model_1, dat_test)
final_model1_RMSLE <- RMSE(yhat_final_1, logb(test_y+1))

### **Model 2: Revenue-group features**
# Chunk 98
# train model 2 with revenue-group features
final_model_2 <- train(revenue ~ ., 
                       data = train_set[,-c(5,6,7)],
                       method = "rf",
                       importance = TRUE,
                       verbose = TRUE,
                       trControl = trainControl(method = "cv",
                                                number = 5,
                                                p = 0.8))

# Chunk 99 - calculate predicted revenue by model 2
yhat_final_2 <- predict(final_model_2, dat_test)
final_model2_RMSLE <- RMSE(yhat_final_2, logb(test_y+1))

### **Model 3: Budget-group features**
# Chunk 100 - trainin model 3
# train model 3 with budget-group features
final_model_3 <- train(revenue ~ ., 
                       data = train_set[,-c(2:4)],
                       method = "rf",
                       importance = TRUE,
                       verbose = TRUE,
                       trControl = trainControl(method = "cv",
                                                number = 5,
                                                p = 0.8))

# Chunk 101 - calculate predicted revenue by model 3
yhat_final_3 <- predict(final_model_3, dat_test)
final_model3_RMSLE <- RMSE(yhat_final_3, logb(test_y+1))

## **Results**
validation_results <- data.frame(Univariate_Filters = final_model1_RMSLE,
                                 Revenue_group_Features = final_model2_RMSLE,
                                 Budget_group_Features = final_model3_RMSLE) 

validation_results 
