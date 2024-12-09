##########################################################
# 1. Data loading - code provided by PH129.x
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

#############################################################
#
# Movielens Project Code
#
#############################################################

# Install additional packages
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
library(knitr)
library(kableExtra)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(scales)

#############################################################
# 1. Data preparation
#############################################################
# Split edx into train and test sets
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train_set <- edx[-test_index,]
temp <- edx[test_index,]

# Make sure userId and movieId in test set are also in train set
test_set <- temp %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Add rows removed from test set back into train set
removed <- anti_join(temp, test_set)
train_set <- rbind(train_set, removed)

rm(test_index, temp, removed)

# Create an overview table with row and column count
dataset_overview <- data.frame(
  Dataset = c("edx", "final_holdout_test", "train_set", "test_set"),
  Rows = c(nrow(edx), nrow(final_holdout_test), nrow(train_set), nrow(test_set)),
  Columns = c(ncol(edx), ncol(final_holdout_test), ncol(train_set), ncol(test_set))
)
tab1 <- kable(dataset_overview, booktabs = TRUE, escape = FALSE) %>%
  kable_styling(position = "center", latex_options = c("striped"))
tab1

# Combine col rating from both sets to compare
combined_data <- bind_rows(
  mutate(select(test_set, rating), data = "Test Set"),
  mutate(select(train_set, rating), data = "Training Set")
)

# Rating density distribution in test vs. train sets
graph_1 <- ggplot(combined_data, aes(x = rating, y = after_stat(density), color = data)) +
  geom_line(stat = "density") +
  ggtitle("Rating Density Distribution (Test vs. Training)") +
  xlab("Rating") +
  ylab("Density") +
  scale_x_continuous(breaks = seq(0.5, 5, 0.5)) +
  theme_economist_white(gray_bg = FALSE) + 
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("#01a2d9", "#014d64"))
graph_1

# Define Root Mean Squared Error (RMSE) formula
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#############################################################
# 2. Exploratory data analysis
#############################################################
# Check for missing values to tidy data if necessary
missing_values <- data.frame(
  Dataset = c("edx", "final_holdout_test"),
  Missing = c(anyNA(edx), anyNA(final_holdout_test))
)
tab2 <- kable(missing_values, booktabs = TRUE, escape = FALSE) %>%
  kable_styling(position = "center", latex_options = c("striped"))

# Show edx table structure - escape bar char
edx_head <- head(edx) %>%
  mutate_all(stringr::str_replace_all, pattern=fixed("|"), replacement=":")

# Summary of the edx dataset
tab4 <- kable(summary(edx), booktabs = TRUE, escape = FALSE, caption = "edx Summary") %>%
  kable_styling(position = "center", latex_options = c("striped"))

# Counts of users (69878), movies (10667) and Genres (797)
edx_summary <- edx %>%
  summarize(Users = n_distinct(userId),
            Movies = n_distinct(movieId),
            Genres = n_distinct(genres))

tab5 <- kable(edx_summary, booktabs = TRUE, escape = FALSE, caption = "edx Summary") %>%
  kable_styling(position = "center", latex_options = c("striped"))

# Rating scores (10), low 0.5 and high is 5 in 0.5 steps
sort(unique(edx$rating))

# Mean of ratings
mean(edx$rating)

# Rating histogram
graph_2 <- edx %>%
  ggplot(aes(rating)) +
  geom_histogram(binwidth = 0.5, color = "#014d64", fill="lightblue") +
  xlab("Rating value") + ylab("Number of ratings") +
  ggtitle("Ratings Histogram") +
  scale_x_continuous(breaks = seq(0.5, 5, 0.5)) +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  theme_economist_white(gray_bg = FALSE) + 
  scale_colour_economist()
# The ratings mean is 3.512465, is the most used rating, integer ratings are more common

# Full star vs half star ratings
values <- edx %>%
  summarize(full_pct = sum(rating %% 1 == 0)/length(rating),
            half_pct = 1 - full_pct)
df <- data.frame(ratings = as.numeric(round(values*100, 1)), cat = c("Full stars", "Half stars"))

# Create the pie chart
graph_3 <- ggplot(df, aes(x = "", y = ratings, fill = cat)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  ggtitle("Rating ratio Full / Half star") +
  scale_fill_manual(values = c("#014d64", "lightblue")) +
  geom_text(aes(label = paste0(ratings, "%")), position = position_stack(vjust=0.5), color="white") +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_economist_white(gray_bg = FALSE) + scale_colour_economist() +
  theme(legend.position = "right") +
  theme(axis.text.x = element_blank(),axis.text.y = element_blank(), axis.ticks = element_blank())

# Number of ratings per user
graph_4 <- edx %>%
  count(userId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "#014d64", fill = "lightblue") +
  labs(title = "Number of ratings per user",
       x = "Users",
       y = "Number of Ratings") +
  scale_x_log10() +
  theme_economist_white(gray_bg = FALSE) + scale_colour_economist()

# Number of ratings per user
graph_5 <- edx %>% 
  count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "#014d64", fill = "lightblue") +
  scale_x_log10() + 
  labs(title = "Number of ratings per movie",
       x = "Movies",
       y = "Number of Ratings") +
  scale_x_log10() +
  theme_economist_white(gray_bg = FALSE) + scale_colour_economist()

#############################################################
# 3. Modeling
#############################################################

# Baseline Prediction based on Mean Rating
mu <- mean(train_set$rating)
rmse_baseline <- RMSE(test_set$rating, mu)

# Save results in a data frame
if(exists("rmse_results")) {
  rm(rmse_results)
}
rmse_results <- data.frame(method = "Naive Mean", RMSE = rmse_baseline)
kable(rmse_results, booktabs = TRUE, escape = FALSE) %>%
  kable_styling(position = "center", latex_options = c("striped"))

# Movie effect
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

predicted_ratings <- mu + test_set %>%
  left_join(movie_avgs, by="movieId") %>%
  pull(b_i)

rmse_movie_effect <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results, data.frame(method = "Movie Effect", RMSE = rmse_movie_effect))
kable(rmse_results, booktabs = TRUE, escape = FALSE) %>%
  kable_styling(position = "center", latex_options = c("striped"))

# User effect
user_avgs <- train_set %>%
  left_join(movie_avgs, by="movieId") %>%
  group_by(userId) %>%
  summarise(b_u = mean(rating - mu - b_i))

predicted_ratings <- test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

rmse_user_effect <- RMSE(test_set$rating, predicted_ratings)
rmse_results <- bind_rows(rmse_results, data.frame(method="Movie + User Effects", RMSE = rmse_user_effect))
kable(rmse_results, booktabs = TRUE, escape = FALSE) %>%
  kable_styling(position = "center", latex_options = c("striped"))

# Genres effect
genre_avgs <- train_set %>%
  left_join(movie_avgs, by="movieId") %>%
  left_join(user_avgs, by="userId") %>%
  group_by(genres) %>%
  summarise(b_g = mean(rating - mu - b_i - b_u))

predicted_ratings <- test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  pull(pred)

rmse_genre_effect <- RMSE(test_set$rating, predicted_ratings)
rmse_results <- bind_rows(rmse_results, data.frame(method="Movie + User + Genre Effects", RMSE = rmse_genre_effect))
kable(rmse_results, booktabs = TRUE, escape = FALSE) %>%
  kable_styling(position = "center", latex_options = c("striped"))

# Regularization
lambdas <- seq(1, 7, 0.25)
rmses <- sapply(lambdas, function(lambda) {
  mu <- mean(train_set$rating)
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarise(b_i = sum(rating - mu)/(n() + lambda))
  b_u <- train_set %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarise(b_u = sum(rating - mu - b_i)/(n() + lambda))
  b_g <- train_set %>%
    left_join(movie_avgs, by="movieId") %>%
    left_join(user_avgs, by="userId") %>%
    group_by(genres) %>%
    summarise(b_g = sum(rating - mu - b_i - b_u)/(n() + lambda))
  predicted_ratings <- test_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    mutate(pred = mu + b_i + b_u + b_g) %>%
    pull(pred)
  return(RMSE(test_set$rating, predicted_ratings))
})

# Find optimal lambda and RMSE
opt_lambda <- lambdas[which.min(rmses)]
min_rmse <- min(rmses)

# Plot RMSE against Lambdas
data <- data.frame(lambdas = lambdas, rmses = rmses)
graph_6 <-ggplot(data, aes(x = lambdas, y = rmses)) +
  geom_point(color = "#014d64", size = 3) +
  geom_line(color = "#014d64", linewidth = 1) +
  geom_vline(xintercept = opt_lambda, linetype = "dashed", color = "lightblue", linewidth = 1) +
  geom_hline(yintercept = min_rmse, linetype = "dashed", color = "lightblue", linewidth = 1) + 
  labs(x = "Lambda", y = "RMSE", title = "Lambda vs RMSE") + 
  scale_x_continuous(breaks = c(seq(min(data$lambdas), max(data$lambdas), by = 1), opt_lambda)) +
  scale_y_continuous(breaks = c(seq(min(data$rmses), max(data$rmses), by = 1), min_rmse)) +
  theme_economist_white(gray_bg = FALSE) + 
  scale_colour_economist()
lambda <- opt_lambda

rmse_results <- bind_rows(rmse_results, data.frame(method="Regularization", RMSE = min_rmse))
kable(rmse_results, booktabs = TRUE, escape = FALSE) %>%
  kable_styling(position = "center", latex_options = c("striped"))

#########################################################
# Calculate with lambda which minimizes RMSE
#########################################################
b_i <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n() + lambda)) 

b_u <- train_set %>%
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n() + lambda)) 

b_g <- train_set %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - b_i - b_u - mu)/(n() + lambda))

#########################################################
# Final result with final_holdout_test
#########################################################
predicted_ratings_final <- final_holdout_test %>%
  left_join(b_i, by='movieId') %>%
  left_join(b_u, by='userId') %>%
  left_join(b_g, by='genres') %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  pull(pred)

RMSE_final <- RMSE(final_holdout_test$rating, predicted_ratings_final)

rmse_results <- bind_rows(rmse_results, data.frame(method="Final Holdout Test", RMSE = RMSE_final))
kable(rmse_results, booktabs = TRUE, escape = FALSE) %>%
  kable_styling(position = "center", latex_options = c("striped"))