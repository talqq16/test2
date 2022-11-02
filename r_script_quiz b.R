library(dplyr)
library(tidyverse)

tibble(num = 1:10, sl = letters[1:10], cl = LETTERS[1:10])

matrix(cbind(1:10, letters[1:10], LETTERS[1:10]), ncol = 3)


setwd("C:/Users/User/Desktop")

Marvel_DC = read.csv("Marvel_DC_imdb.csv")

marvel = Marvel_DC %>%
  filter(Category == "Marvel")

DC = Marvel_DC %>%
  filter(Category == "DC")

rating_marvel = marvel %>%
  filter(Rating %in% c("TV-PG", "TV-14")) %>%
  select(Rating, everything())

movie_score_marvel = rating_marvel %>%
  group_by(Movie) %>%
  summarise(count = n(), mean_score = mean(IMDB_Score))


rating_dc = DC %>%
  filter(Rating %in% c("TV-PG", "TV-14")) %>%
  select(Rating, everything())

top

movie_score_dc = rating_dc %>%
  group_by(Movie) %>%
  summarise(count = n(), mean_score = mean(IMDB_Score))

top_10_marvel = movie_score_marvel %>%
  top_n(10) %>%
  arrange(desc(mean_score)) %>%


top_10_DC = movie_score_dc %>%
  top_n(10) %>%
  arrange(desc(mean_score)) 
  
  
dc_vs_marvel = bind_rows(top_10_DC, top_10_marvel) %>%
  arrange(desc(mean_score)) 

library(ggplot2)

ggplot(data = dc_vs_marvel) +
  geom_point(mapping = aes(x= count, y= mean_score), size = 5)

DD = rbind(dc_vs_marvel, Marvel_DC)