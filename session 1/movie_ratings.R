library(tidyverse)
library(lubridate)
library(here)
library(mosaic)
library(infer)
library(ggridges)
library(viridis)

# while it's fine to know about working directories, I suggest 
# you learn to use the package 'here' that makes organising files easy
# https://malco.io/2018/11/05/why-should-i-use-the-here-package/
movies <- read_csv(here::here('data', 'movies.csv'))

# Get a glimpse of the data- which vairables do we have, etc
glimpse(movies)

# Count how many movies 
movies %>% 
  count(genre,sort=TRUE)


#**************************************
# Plot boxplots of ratings, faceted by genre
movies %>%
  ggplot()+
  aes(x = rating, colour=genre) +
  geom_boxplot() +
  facet_wrap(~genre, scales = "free")+
  theme_bw()+
  theme(legend.position = "none")+
  labs(title = "Distribution of IMDB ratings by film genre",
       x = "Film Rating",
       y = " ")+
  NULL

# Plot histogram of ratings, faceted by genre
movies %>%
  ggplot()+
  aes(x = rating, fill=genre) +
  geom_histogram() +
  facet_wrap(~genre, scales = "free")+
  theme_bw()+
  theme(legend.position = "none")+
  labs(title = "Distribution of IMDB ratings by film genre",
       x = "Film Rating",
       y = " ")+
  NULL



# Plot ECDF of rating, faceted by genre
movies %>%
  ggplot()+
  aes(x = rating, colour=genre) +
  stat_ecdf(geom = "step", pad = FALSE) +
  scale_y_continuous(labels = scales::percent)+
  facet_wrap(~genre, scales = "free")+
  theme_bw()+
  theme(legend.position = "none")+
  labs(title = "Distribution of IMDB ratings by film genre",
       x = "Film Rating",
       y = " ")+
  NULL

#**************************************
# Plot distributions of ratings, faceted by genre
movies %>%
  ggplot()+
  aes(x = rating, colour=genre) +
  geom_density() +
  facet_wrap(~genre)+
  theme_bw()+
  theme(legend.position = "none")+
  labs(title = "Distribution of IMDB ratings by film genre",
       x = "Film Rating",
       y = " ")+
  NULL



#**************************************
# Confidence Interval (CI) using the formula mean +- t_critical * SE

genre_formula_ci <- movies %>%
  group_by(genre) %>% 
  summarise(mean_rating = mean(rating),
            median_rating = median(rating),
            sd_rating = sd(rating),
            count = n(),
            # get t-critical value with (n-1) degrees of freedom
            t_critical = qt(0.975, count-1),
            se_rating = sd_rating/sqrt(count),
            margin_of_error = t_critical * se_rating,
            rating_low = mean_rating - margin_of_error,
            rating_high = mean_rating + margin_of_error
  ) %>% 
  arrange(desc(mean_rating))


genre_formula_ci

# we will draw a violin plot and then use position="jitter" or geom_jitter() 
# to see how spread out the actual points are

movies %>%
  ggplot(aes(x = rating, y = reorder(genre, rating), colour=genre)) +
  geom_violin()+
  geom_point(position = "jitter", size = 0.6, alpha = 0.45) + 
  
  # we can also superimpose  the means as a big orange dot,
  # but first we must create a tibble that has two columns: genre and rating
  stat_summary(#geom = "pointrange", 
               fun.data = "mean_se", 
               colour = "orange", 
               linewidth = 1.2, 
               size = 1.2,
               fun.args = list(mult = 2)) +
  
  # add labels, on x= y axes and title
  labs(x= "Mean IMDB Rating", 
       y = NULL,
       title="Which film genres have the highest mean IMDB ratings?") + 

  theme_bw()+
  theme(legend.position = 'none')+
  scale_x_continuous(breaks=seq(0,10,1))+
  NULL


#visualise CIs for all genres. 
genre_formula_ci %>% 
ggplot() + 
  aes(x=reorder(genre, mean_rating), y=mean_rating, colour=genre) +
  geom_point() +
  geom_errorbar(width=.5, aes(ymin=rating_low, ymax=rating_high)) + 
  labs(x=" ",
       y= "Mean IMDB Rating", 
       title="Which film genres have the highest mean IMDB ratings?") + 
  theme_bw()+
  coord_flip()+
  theme(legend.position = "none")+
  NULL
