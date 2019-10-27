library(dslabs)
library(tidyverse)
library(lubridate)
data("movielens")
summary(movielens)
movielens %>% head()
class(movielens)

# Q1 What year has the highest median number of ratings
# Count the number of ratings for each movie
rating_amount <- movielens %>% 
  group_by(movieId, year) %>% 
  summarise(count = n())
rating_amount %>% head()
# Plot it against the year movie came out. Use square root transformation on counts
plot(rating_amount$year, rating_amount$count)
year_ratings_median <- rating_amount %>%
  group_by(year) %>%
  summarize(medyear = median(count))
year_ratings_median
max(year_ratings_median$medyear)
year <- year_ratings_median %>%
  filter(medyear == 8)
year
# Mallivastaus
movielens %>% group_by(movieId) %>%
  summarize(n = n(), year = as.character(first(year))) %>%
  qplot(year, n, data = ., geom = "boxplot") +
  coord_trans(y = "sqrt") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Q2
# Average rating for the movie the Shawshang Redemption
selected <- movielens %>%
  filter(year >= 1993 & year <= 2018) %>%
  filter(str_detect(title, 'Shawshank')) %>%
  summarize(average = mean(rating))
selected 
# Average number of ratings per year for the movie Forrest Gumå
selected2 <- movielens %>%
  filter(year >= 1993 & year <= 2018) %>%
  group_by(movieId) %>%
  filter(str_detect(title, 'Gump')) %>%
  summarize(r_per_year = n()/(2018-1994))
selected2

rating_Gu <- selected %>%
  filter(str_detect(title, 'Gump')) %>%
  summarize(n = n())
rating_Gu
# Mallivastaus
movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  top_n(25, rate) %>%
  arrange(desc(rate)) 

# Q3
table <- movielens %>% 
  select(movieId, title, year, rating) %>% 
  filter (year >=1993 & year <= 2018)

movies <- table %>% 
  group_by(movieId) %>% 
  summarize(nb_rating=length(rating), avg_rating=mean(rating))

movies %>% 
  ggplot(aes(x=nb_rating, y=(avg_rating))) + 
  geom_point()
# Mallivastaus
movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  ggplot(aes(rate, rating)) +
  geom_point() +
  geom_smooth()

# Q5
movielens <- mutate(movielens, date = as_datetime(timestamp))
movielens %>% head()

# Q6
movielens_week <- movielens %>%
  mutate(week = week(date)) %>%
  group_by(week) %>%
  summarize(avg_rating = mean(rating))
movielens_week
plot(movielens_week$week, movielens_week$avg_rating)
# Mallivastaus
movielens %>% mutate(date = round_date(date, unit = "week")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth()

# Q8
movielens_genres <- movielens %>%
  mutate(genres = as.factor(genres)) %>%
  group_by(genres) %>%
  filter(n()>1000) %>%
  summarize(avg_rating = mean(rating), sd_rating = sd(rating)) %>%
  arrange(avg_rating)
movielens_genres
# Malliratkaisu
movielens %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 1000) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
Submit