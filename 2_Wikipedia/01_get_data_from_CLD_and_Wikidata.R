
# ------------------------------------------------------------------------
# Script name: 01_get_data_from_CLD_and_Wikidata.R
# Purpose of script: retrieve data from Comparative Legislator Database, select only MPs of interest (electoral term 12-19), export data
# Dependencies: none
# Author: Simon Munzert, altered by Alexandra Rottenkolber
# ------------------------------------------------------------------------

# load packages and functions -------------------------------
library(WikipediR)
library(tidyverse)
library(legislatoR)
library(SPARQL) # SPARQL querying package
library(ggplot2)
library(readr)
library(stringr)

#setwd("XXX")

## Get data from CLD
# Politicians part of the parliament between 1990 and 2021 based on the Comparative Legislator Database
relevant_MPs <- data.frame(semi_join(x = get_core(legislature = "deu"),
                                     y = filter(get_political(legislature = "deu"), (session >=12 & session <=19)), 
                                     by = "pageid"))
# Parties
Parties <- get_political(legislature = "deu")
Parties <- Parties[, c("pageid","session","party","constituency2" )]
Parties <- subset(Parties, (Parties$session >= 12 & Parties$session <= 19))

# Offices
Offices <- get_office(legislature = "deu")
cols_names <- c("wikidataid",
                "bundesminister",
                #"chairman_of_the_cdu.csu_bundestag_fraction",
                #"chairman_of_the_social_democratic_party",
                "federal_chancellor_of_germany",
                "party_leader",
                "president_of_the_bundestag", 
                "sprecher",
                "parliamentary_secretary_in_germany",
                "parliamentary_secretary_in_germany",
                "secretary_general_of_the_cdu", 
                "secretary_general_of_the_spd",
                "member_of_parliament",
                "member_of_the_german_bundestag",
                "president_of_the_bundestag",
                "secretary_of_state",
                "minister",
                "party_leader",
                "president",
                "president_by_age",
                "president_of_germany",
                "president_of_the_bundestag",
                "vice.chancellor_of_germany",
                "vice_president_of_the_bundestag")
Offices_sub <- Offices[, cols_names]

# merge data to one frame 
MPs_and_parties <- relevant_MPs[, c("pageid", "wikidataid", "birth")] %>% 
  merge(Parties, by = "pageid", all.x = TRUE) %>% 
  merge(Offices_sub, by = "wikidataid", all.x = TRUE) %>% 
  merge(Professions_melted3, by = "wikidataid", all.x = TRUE)

MPs_meta_info <- relevant_MPs[, c( "pageid","wikidataid","wikititle","name","sex","ethnicity","religion","birth")] %>% 
  merge(Parties, by = "pageid", all.x = TRUE) %>% 
  merge(Offices_sub, by = "wikidataid", all.x = TRUE) %>% 
  merge(Professions_melted3, by = "wikidataid", all.x = TRUE)

#save(MPs_and_parties, file = "./01_data/Wikipedia/output/MP2Party_CLD.RData")
#save(MPs_meta_info, file = "./01_data/Wikipedia/output/MPs_meta_info.RData")

#load(file = "./01_data/Wikipedia/output/MP2Party_CLD.RData")

# write.csv2(relevant_MPs, "./data/relevant_MPs.csv", row.names = FALSE)


# ## Get data from Wikidata --------------------------
## Import politicians entities 
relevant_MPs <- read_csv2("./01_data/Wikipedia/relevant_MPs.csv")
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
url_query <- read_delim("./01_data/Wikipedia/raw-data/wikidata_query_German_politicans_Wiki_URL.csv", delim = ",")
url_query$id_raw <- str_extract(url_query$politician, "Q[[:digit:]]+$")
sum(is.na(url_query$id_raw))

lang_ed<- rbind(
  read_delim("./01_data/Wikipedia/raw-data/query_lang_ed_batch_1.csv", delim = ","), 
  read_delim("./01_data/Wikipedia/raw-data/query_lang_ed_batch_2.csv", delim = ","), 
  read_delim("./01_data/Wikipedia/raw-data/query_lang_ed_batch_3.csv", delim = ",")
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

# assign missing language editions (manual lookup)
MP_data$no_lang_ed[MP_data$name == "Willy Brandt"] <- 95
MP_data$no_lang_ed[MP_data$name == "Stephan Pilsinger"] <- 3


## download Wikidata PageRank data -------------
# source: http://people.aifb.kit.edu/ath/ 
# data downloaded from: https://danker.s3.amazonaws.com/index.html
wikidata_pagerank_df <- read_tsv("./01_data/Wikipedia/raw-data/2020-11-14.allwiki.links.rank", 
                                 col_names = c("id_raw", "pageRankGlobalALL"))
MP_data <- left_join(x = MP_data, 
                     y = wikidata_pagerank_df[c("pageRankGlobalALL", "id_raw")], 
                     by = "id_raw")
sum(is.na(wikidata_pagerank_df$pageRankGlobalALL))

colnames(MP_data)
#[1] "country"           "pageid"            "wikidataid"        "wikititle"         "name"              "sex"               "ethnicity"         "religion"         
#[9] "birth"             "death"             "birthplace"        "deathplace"        "id_raw"            "id_query"          "wikiurl"           "no_lang_ed"       
#[17] "pageRankGlobalALL"

# save results
save(MP_data, file = "./data/output/MP_data.RData")


