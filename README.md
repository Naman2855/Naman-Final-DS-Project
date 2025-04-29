---
title: "Are Movie Sequels Actually Worse?"
author: "Naman Chandak"
date: "`r Sys.Date()`"
output: html_document
---

```{r setup, include=FALSE}
library(tidyverse)
library(lubridate)
library(janitor)
library(stringr)
```

# Introduction

The purpose of this project is to explore whether movie sequels tend to receive lower ratings compared to original movies. Using data from the TMDb movie dataset, we will analyze trends in ratings, revenue, and genres between originals and sequels.

# Data Importing

```{r load-data}
movies <- read_csv("tmdb_5000_movies.csv")
```

# Data Cleaning

```{r cleaning}
movies <- movies %>%
  clean_names() %>%
  mutate(
    release_date = ymd(release_date),
    sequel = if_else(str_detect(title, "\\b(2|3|4|5|II|III|IV|V|VI|VII)\\b"), "Sequel", "Original")
  )

# Check how many sequels and originals
movies %>% count(sequel)
```

# Exploratory Data Analysis

## Ratings: Originals vs Sequels

```{r ratings-comparison}
movies %>%
  filter(!is.na(vote_average)) %>%
  ggplot(aes(x = sequel, y = vote_average, fill = sequel)) +
  geom_boxplot() +
  labs(title = "Movie Ratings: Originals vs Sequels",
       x = "Movie Type",
       y = "TMDb Average Rating") +
  theme_minimal()
```

## Revenue: Originals vs Sequels

```{r revenue-comparison}
movies %>%
  filter(!is.na(revenue), revenue > 0) %>%
  ggplot(aes(x = sequel, y = revenue, fill = sequel)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(title = "Movie Revenue: Originals vs Sequels",
       x = "Movie Type",
       y = "Revenue (Log Scale)") +
  theme_minimal()
```

# Genre Exploration (Optional)

```{r genre-exploration}
# Note: genres is messy (JSON-like) - for now simple text extraction
movies %>%
  filter(sequel == "Sequel") %>%
  select(genres) %>%
  head(5)
```

# 1. Density Plot: Ratings Distribution

```{r density-plot}
movies %>%
  ggplot(aes(x = vote_average, fill = sequel)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribution of Ratings: Originals vs Sequels",
       x = "TMDb Rating",
       y = "Density") +
  theme_minimal()
```

# 2. Trend Plot: Rating Over Time

```{r trend-over-time}
movies %>%
  filter(!is.na(release_date)) %>%
  mutate(year = year(release_date)) %>%
  group_by(year, sequel) %>%
  summarise(avg_rating = mean(vote_average, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = avg_rating, color = sequel)) +
  geom_line() +
  labs(title = "Average Rating Over Time: Originals vs Sequels",
       x = "Year",
       y = "Average Rating") +
  theme_minimal()
```

# 3. Scatter Plot: Budget vs Rating

```{r budget-vs-rating}
movies %>%
  filter(budget > 0) %>%
  ggplot(aes(x = budget, y = vote_average, color = sequel)) +
  geom_point(alpha = 0.5) +
  scale_x_log10() +
  labs(title = "Budget vs Rating (Log Scale)",
       x = "Budget (log scale)",
       y = "TMDb Rating") +
  theme_minimal()
```

# 4. Bar Plot: Count of Originals vs Sequels

```{r barplot-counts}
movies %>%
  ggplot(aes(x = sequel, fill = sequel)) +
  geom_bar() +
  labs(title = "Number of Original Movies vs Sequels",
       x = "Movie Type",
       y = "Count") +
  theme_minimal()
```
