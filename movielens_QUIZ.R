##########################################################
# Create edx and final_holdout_test sets 
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

# Q1
str(edx)
# How many rows?
rows <- dim(edx)[1]
# How many columns?
columns <- dim(edx)[2]

# Q2
# How many zeros were given as ratings in the edx dataset?
zeros <- sum(edx$rating == 0)
# How many threes were given as ratings in the edx dataset?
threes <- sum(edx$rating == 3)

# Q3 - How many movies are in the edx dataset?
n_distinct(edx$movieId)

# Q4 - How many users are in the edx dataset?
n_distinct(edx$userId)

# Q5 - How many movies per genre?
genres_list <- c('Drama', 'Comedy', 'Thriller', 'Romance')
movies_genre <- sapply(genres_list, function(g){
  edx %>% filter(str_detect(genres, g)) %>% tally()
})

# Q6 - Movie with highest rating
m_ratings <- edx %>% group_by(movieId) %>% 
  summarize(m_ratings = n(), movie_title = first(title)) %>%
  arrange(desc(m_ratings)) %>%
  top_n(10, m_ratings)

# Q7 - Five most given ratings in order from most to least
ratings <- edx %>% group_by(rating) %>% 
  summarize(number = n())
ratings %>% top_n(5) %>% arrange(desc(number))

# Q8 - Half-star vs full-star ratings
ratings %>%
  mutate(half_star = rating %% 1 == 0.5) %>%
  group_by(half_star) %>%
  summarize(number = sum(number))