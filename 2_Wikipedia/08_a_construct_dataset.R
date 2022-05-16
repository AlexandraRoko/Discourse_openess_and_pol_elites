# ------------------------------------------------------------------------
# Script name: 08_a_construct_dataset.R
# Purpose of script: dataset construction
# Dependencies: script 1-7. 
# Author: Simon Munzert, altered by Alexandra Rottenkolber
# ------------------------------------------------------------------------

## load packages and functions -------------------------------
library(sets)
source("_packages.r")
source("_functions.r")

#setwd("XXX")

## import Wikidata database ----------
load("./01_data/Wikipedia/output/MP_data.RData")
MP_data$pageRank <- NULL
MP_data$positions <- NULL

## import page statistics -----#
load("./01_data/Wikipedia/output/pageStatistics_df_complete.RData")

page_info_statistics_df_complete$page_url <- str_replace_all(page_info_statistics_df_complete$page, " ", "_") %>% sapply(., URLencode)
page_prose_statistics_df_complete$page_url <- str_replace_all(page_prose_statistics_df_complete$page, " ", "_") %>% sapply(., URLencode)
page_info_statistics_df_complete <- dplyr::select(page_info_statistics_df_complete, page_url, pageviews, revisions, editors, created_at)
page_info_statistics_df_complete$date_retrieved <- as.Date("2021-02-26")
page_info_statistics_df_complete$article_age <- ymd(page_info_statistics_df_complete$date_retrieved) - ymd(page_info_statistics_df_complete$created_at)
page_info_statistics_df_complete$pageviews <- as.numeric(page_info_statistics_df_complete$pageviews)
page_info_statistics_df_complete$revisions <- as.numeric(page_info_statistics_df_complete$revisions)
page_info_statistics_df_complete$editors <- as.numeric(page_info_statistics_df_complete$editors)

page_info_statistics_df_complete$revisions_avg <- as.numeric(page_info_statistics_df_complete$revisions) / as.numeric(page_info_statistics_df_complete$article_age)
page_prose_statistics_df_complete <- dplyr::select(page_prose_statistics_df_complete, page_url, characters, words, unique_references)
page_prose_statistics_df_complete$characters <- as.numeric(page_prose_statistics_df_complete$characters)
page_prose_statistics_df_complete$words <- as.numeric(page_prose_statistics_df_complete$words)
page_prose_statistics_df_complete$unique_references <- as.numeric(page_prose_statistics_df_complete$unique_references)

MP_data$wikiurl_basename <- basename(MP_data$wikiurl)


## import pageviews data -----
load("./01_data/Wikipedia/output/pageviews_average_2017.RData")
pageviews_df$page_url <- str_replace(pageviews_df$wikiurl_basename, ".csv$", "")
pageviews_df$wikiurl_basename <- NULL

## import pages data -----
load("./01_data/Wikipedia/output/wikipages_df.RData")
wikipages_df$page_url <- wikipages_df$article_name
wikipages_df$article_name <- NULL

## import wikipedia graph data -----
load("./01_data/Wikipedia/output/wikiGraph.RData")

## compute pageRank ----------
pagerank <- graph_wiki %>% page.rank(directed = TRUE) %>%  use_series("vector") 
pagerank_all <- graph_wiki_all %>% page.rank(directed = TRUE) %>%  use_series("vector") 
pagerank_df <- data.frame(page_url = article_name, pagerank, pagerank_all, stringsAsFactors = FALSE)

## make one data.frame -------
wikimeasures_df <- MP_data %>% merge(page_info_statistics_df_complete, by.x = "wikiurl_basename", by.y = "page_url", all.x = TRUE) %>%
  merge(page_prose_statistics_df_complete, by.x = "wikiurl_basename", by.y = "page_url", all.x = TRUE) %>%
  merge(pageviews_df, by.x = "wikiurl_basename", by.y = "page_url", all.x = TRUE) %>% 
  merge(wikipages_df, by.x = "wikiurl_basename", by.y = "page_url", all.x = TRUE) %>% 
  merge(pagerank_df, by.x = "wikiurl_basename", by.y = "page_url", all.x = TRUE)

## export data
save(wikimeasures_df, file = "./data/output/wikimeasures_df.RData")

page_info_statistics_df_complete <- page_info_statistics_df_complete %>% rename(wikiurl_basename = page_url)

data_1 <- plyr::join(MP_data, page_info_statistics_df_complete, by = "wikiurl_basename", type = "left") 
data_2 <- merge(data_1, page_prose_statistics_df_complete, by.x = "wikiurl_basename", by.y = "page_url", all.x = TRUE)
data_3 <- merge(data_2, pageviews_df, by.x = "wikiurl_basename", by.y = "page_url", all.x = TRUE)
data_4 <- merge(data_3, wikipages_df, by.x = "wikiurl_basename", by.y = "page_url", all.x = TRUE)
data_5 <- merge(data_4, pagerank_df, by.x = "wikiurl_basename", by.y = "page_url", all.x = TRUE)

## generate rankings ----
# wikimeasures_df$pagerank_rank <- rank(-wikimeasures_df$pagerank, ties.method = "min")
# wikimeasures_df$closeness_in_5_rank <- rank(-wikimeasures_df$closeness_in_5, ties.method = "min")
# wikimeasures_df$pageviews_rank <- rank(-wikimeasures_df$pageviews, ties.method = "min")
# wikimeasures_df$article_size_rank <- rank(-wikimeasures_df$article_size, ties.method = "min")
# wikimeasures_df$edits_rank <- rank(-wikimeasures_df$num_edits, ties.method = "min")
# wikimeasures_df$editors_rank <- rank(-wikimeasures_df$num_editors, ties.method = "min")
# wikimeasures_df$extlinks_rank <- rank(-wikimeasures_df$num_extlinks, ties.method = "min")




