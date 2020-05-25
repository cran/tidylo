## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  warning = FALSE, message = FALSE,
  collapse = TRUE,
  comment = "#>")

suppressPackageStartupMessages(library(ggplot2))
theme_set(theme_light())

## ----bigram_counts------------------------------------------------------------
library(dplyr)
library(janeaustenr)
library(tidytext)
library(stringr)

tidy_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token="ngrams", n = 2, to_lower = FALSE) %>%
  filter(!str_detect(bigram, "[A-Z]"))

bigram_counts <- tidy_bigrams %>%
  count(book, bigram, sort = TRUE)

bigram_counts

## ----bigram_log_odds, dependson="bigram_counts"-------------------------------
library(tidylo)

bigram_log_odds <- bigram_counts %>%
  bind_log_odds(book, bigram, n) 

bigram_log_odds %>%
  arrange(-log_odds_weighted)

## ----bigram_plot, dependson="bigram_log_odds", fig.width=10, fig.height=7-----
library(ggplot2)

bigram_log_odds %>%
  group_by(book) %>%
  top_n(10) %>%
  ungroup %>%
  mutate(bigram = reorder(bigram, log_odds_weighted)) %>%
  ggplot(aes(bigram, log_odds_weighted, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, scales = "free") +
  coord_flip() +
  labs(x = NULL)

## ----gear_counts--------------------------------------------------------------
gear_counts <- mtcars %>%
  count(vs, gear)

gear_counts

## ----dependson="gear_counts"--------------------------------------------------
regularized <- gear_counts %>%
  bind_log_odds(vs, gear, n)

regularized

## ----dependson="gear_counts"--------------------------------------------------
unregularized <- gear_counts %>%
  bind_log_odds(vs, gear, n, uninformative = TRUE, unweighted = TRUE)

unregularized

