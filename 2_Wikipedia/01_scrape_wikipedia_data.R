## load packages and functions -------------------------------
library(WikipediR)
library(tidyverse)
library(legislatoR)
library(SPARQL) # SPARQL querying package
library(ggplot2)
library(readr)
library(stringr)

source("/Users/alexandrarottenkolber/Documents/02_Hertie_School/Master thesis/Master_Thesis_Hertie/prominence-code_Simon_AR/_packages.r")
source("/Users/alexandrarottenkolber/Documents/02_Hertie_School/Master thesis/Master_Thesis_Hertie/prominence-code_Simon_AR/_functions.r")

getwd()
setwd("/Users/alexandrarottenkolber/Documents/02_Hertie_School/Master thesis/Master_Thesis_Hertie/data_analysis")

# Politicians part of the parliament between 1990 and 2021 based on the Comparative Legislator Database
relevant_MPs <- data.frame(semi_join(x = get_core(legislature = "deu"),
                                     y = filter(get_political(legislature = "deu"), (session >=12 & session <=19)), 
                                     by = "pageid"))

# write.csv2(relevant_MPs, "./data/relevant_MPs.csv", row.names = FALSE)

## Import politicians entities --------------------------
relevant_MPs <- read_csv2("./data/relevant_MPs.csv")
relevant_MPs$id_raw <- str_extract(relevant_MPs$wikidataid, "Q[[:digit:]]+$")
relevant_MPs$id_query <- paste("wd:",str_extract(relevant_MPs$wikidataid, "Q[[:digit:]]+$"), sep = "")
colnames(relevant_MPs)

id_ls <- as.list(relevant_MPs$id_query)

# write.table(id_ls[0:1000], file = "./data/Wikidata_id_for_query_batch1.txt", sep = ", ",
#             row.names = FALSE, col.names = FALSE, quote = FALSE)
# write.table(id_ls[1000: 2000], file = "./data/Wikidata_id_for_query_batch2.txt", sep = ", ",
#             row.names = FALSE, col.names = FALSE, quote = FALSE)
# write.table(id_ls[2000: length(id_ls)], file = "./data/Wikidata_id_for_query_batch3.txt", sep = ", ",
#             row.names = FALSE, col.names = FALSE, quote = FALSE)

# Run Wikidata SPARQL query (see wikidata_query_German_politicans_Wiki_URL.rtf, wikidata_query_language_editions_per_german_politician.rtf); https://query.wikidata.org/
## Import wikidata query results --------------------------
url_query <- read_delim("./data/raw-data/wikidata_query_German_politicans_Wiki_URL.csv", delim = ",")
url_query$id_raw <- str_extract(url_query$politician, "Q[[:digit:]]+$")
sum(is.na(url_query$id_raw))

lang_ed<- rbind(
  read_delim("./data/raw-data/query_lang_ed_batch_1.csv", delim = ","), 
  read_delim("./data/raw-data/query_lang_ed_batch_2.csv", delim = ","), 
  read_delim("./data/raw-data/query_lang_ed_batch_3.csv", delim = ",")
  )

lang_ed$id_raw <- str_extract(lang_ed$item, "Q[[:digit:]]+$")

## Join URLs, language editions to info about MPs --------------------------
# Join URLs
MP_data <- left_join(x = relevant_MPs, 
                     y = url_query[c("wikiurl", "id_raw")], 
                     by = "id_raw")
sum(is.na(MP_data$wikiurl))

# find missing URLs 
MP_data$name[is.na(MP_data$wikiurl)]

# assign missing URLs 
MP_data$wikiurl[MP_data$name == "Willy Brandt"] <- "https://de.wikipedia.org/wiki/Willy_Brandt"
MP_data$wikiurl[MP_data$name == "Annegret Kramp-Karrenbauer"] <- "https://de.wikipedia.org/wiki/Annegret_Kramp-Karrenbauer"
MP_data$wikiurl[MP_data$name == "Carsten Sieling"] <- "https://de.wikipedia.org/wiki/Carsten_Sieling"
MP_data$wikiurl[MP_data$name == "Frank-Walter Steinmeier"] <- "https://de.wikipedia.org/wiki/Frank-Walter_Steinmeier"
MP_data$wikiurl[MP_data$name == "Oliver Wittke"] <- "https://de.wikipedia.org/wiki/Oliver_Wittke"

# Drop Christina Schenk who is not a politician 
MP_data<-subset(MP_data, name!="Christina Schenk")

# Join Language editions
MP_data <- left_join(x = MP_data, 
                     y = lang_ed[c("articles", "id_raw")], 
                     by = "id_raw")
MP_data <- dplyr::rename(MP_data, no_lang_ed = articles)
sum(is.na(MP_data$no_lang_ed))

# find missing language editions 
MP_data$id_raw[is.na(MP_data$no_lang_ed)]

# asssign missing language editions (manual lookup)
MP_data$no_lang_ed[MP_data$name == "Willy Brandt"] <- 95
MP_data$no_lang_ed[MP_data$name == "Stephan Pilsinger"] <- 3


## download Wikidata PageRank data -------------
# source: http://people.aifb.kit.edu/ath/ 
# data downloaded from: https://danker.s3.amazonaws.com/index.html
wikidata_pagerank_df <- read_tsv("./data/raw-data/2020-11-14.allwiki.links.rank", 
                                 col_names = c("id_raw", "pageRankGlobalALL"))
MP_data <- left_join(x = MP_data, 
                     y = wikidata_pagerank_df[c("pageRankGlobalALL", "id_raw")], 
                     by = "id_raw")
sum(is.na(wikidata_pagerank_df$pageRankGlobalALL))

# save results
save(MP_data, file = "./data/output/MP_data.RData")


