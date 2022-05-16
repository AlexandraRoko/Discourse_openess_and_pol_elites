# ------------------------------------------------------------------------
# Script name: 08_b_construct_dataset_with_time_dimension.R
# Purpose of script: dataset construction (including time dimension)
# Dependencies: script 1-7.
# Author: Alexandra Rottenkolber based on a script by Simon Munzert
# ------------------------------------------------------------------------

## load packages and functions -------------------------------
library(descr)
library(stringr)

source("_packages.r")
source("_functions.r")

##### one frame
## import Wikidata database ---------- Get socioeconomic data
load("./01_data/Wikipedia/output/MP_data.RData")
MP_data$pageRank <- NULL
MP_data$positions <- NULL
MP_data$wikiurl_basename <- basename(MP_data$wikiurl)

IDs_df <- subset(MP_data, select = c("wikidataid","wikiurl_basename"))   
IDs_df$wikiurl_basename_clean <- str_replace(IDs_df$wikiurl_basename, "%E1%BA%9E", "%C3%9F") #URLdecode(IDs_df$wikiurl_basename)
IDs_df$wikiurl_basename <- NULL

YEAR = c(2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021)

# construct template for final data frame
final_df <- data.frame(wikidataid=character(),
                       YEAR=character(),
                       wikiurl_basename_clean=character(),
                       pageviews_avg=character(),
                       start_date_average=character(),
                       end_date_average=character(),
                       article_size=character(),
                       article_refs=character(),
                       article_extlinks=character(),
                       articles_intlinks=character(),
                       pagerank=character(),
                       pagerank_all=character())


##### in yearly frames
for (k in seq_along(YEAR)){

  year = as.character(YEAR[k])
  
  IDs_df$YEAR <- year
  
  ## import pageviews data -----
  load(paste("./01_data/Wikipedia/output/historical_pageviews/pageviews_average_", year, ".RData", sep = "")) 
  nrow(pageviews_df)
  names(pageviews_df)
  pageviews_df$page_url <- str_replace(pageviews_df$wikiurl_basename, ".csv$", "")
  pageviews_df$YEAR <- year
  pageviews_df$page_url_clean <- str_replace(pageviews_df$page_url, "%E1%BA%9E", "%C3%9F")
  pageviews_df$wikiurl_basename <- NULL
  pageviews_df$page_url <- NULL
  
  ## import pages data -----
  load(paste("./01_data/Wikipedia/output/politicians_DE_wikiarticles_history/wikipages_df", year, ".RData", sep = ""))
  nrow(wikipages_df)
  names(wikipages_df)
  wikipages_df$page_url <- wikipages_df$article_name
  wikipages_df$YEAR <- year
  wikipages_df$page_url_clean <- str_replace(wikipages_df$page_url, "%E1%BA%9E", "%C3%9F")
  wikipages_df$article_name <- NULL
  wikipages_df$page_url <- NULL
  
  ## import wikipedia graph data -----
  load(paste("./01_data/Wikipedia/output/historical_wikigraphs/wikiGraph", year, ".RData", sep = ""))
  
  ### compute pageRank ----------
  pagerank <- graph_wiki %>% page.rank(directed = TRUE) %>%  use_series("vector") 
  pagerank_all <- graph_wiki_all %>% page.rank(directed = TRUE) %>%  use_series("vector") 
  pagerank_df <- data.frame(page_url = article_name, pagerank, pagerank_all, stringsAsFactors = FALSE)
  pagerank_df$page_url_clean <- str_replace(pagerank_df$page_url, "%E1%BA%9E", "%C3%9F")
  pagerank_df$YEAR <- year
  pagerank_df$page_url <- NULL
  
  colnames(pagerank_df)
  colnames(wikipages_df)
  colnames(pageviews_df)

  temp_df <- IDs_df %>% merge(pageviews_df, by.x = c("wikiurl_basename_clean", "YEAR"), by.y = c("page_url_clean", "YEAR"), all.x = TRUE) %>%
    merge(wikipages_df, by.x = c("wikiurl_basename_clean", "YEAR"), by.y = c("page_url_clean", "YEAR"), all.x = TRUE) %>%
    merge(pagerank_df, by.x = c("wikiurl_basename_clean", "YEAR"), by.y = c("page_url_clean", "YEAR"), all.x = TRUE)
  
  final_df <- rbind(final_df, temp_df)

}

## socio-demographics factors and revisions /editors
revisions_editors_df <- read.csv(file = './01_data/Wikipedia/output/history_norevisions_nousers_counted_per_year.csv') # there is also monthly data
revisions_editors_df <- subset(revisions_editors_df, select = c("year","title","no_of_revisions","unique_users","cum_sum_revisions","cum_sum_users","wikidataid"))   
revisions_editors_df$title <- NULL
nolangeds_df <- read.csv(file = './01_data/Wikipedia/output/history_nolanguages_counted_all_years.csv')
nolangeds_df <- subset(nolangeds_df, select = c("wikidataid","year","no_languages_created","cum_sum_no_languages"))   

MP_data$wikiurl_basename_clean <- str_replace(MP_data$wikiurl_basename, "%E1%BA%9E", "%C3%9F") # convert two different ß to one
MP_data_select <- MP_data[, c("wikidataid","wikititle","name",
                              "sex","ethnicity","religion","birth","death","wikiurl","pageRankGlobalALL")]

final_data_df <- final_df %>% merge(revisions_editors_df, by.x = c("wikidataid", "YEAR"), by.y = c("wikidataid", "year"), all.x = TRUE) %>%
  merge(nolangeds_df, by.x = c("wikidataid", "YEAR"), by.y = c("wikidataid", "year"), all.x = TRUE) %>%
  merge(MP_data_select, by.x = c("wikidataid"), by.y = c("wikidataid"), all.x = TRUE)
final_data_df <- distinct(final_data_df)

######################################################################################################################################################################

## export data
save(final_data_df, file = "./01_data/Wikipedia/output/wikimeasures_df_with_time_dim.RData")



