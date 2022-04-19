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

colnames(relevant_MPs)

# Parties
Parties <- data.frame(semi_join(x = select(get_political(legislature = "deu"), pageid, party, session, service),
                                y = filter(get_political(legislature = "deu"), (session >=12 & session <=19)), 
                                by = "pageid"))


# Offices
Offices <- get_office(legislature = "deu")
colnames(Offices)

cols_names <- c("wikidataid",
                "bundesminister",
                #"chairman_of_the_cdu.csu_bundestag_fraction",
#"chairman_of_the_social_democratic_party",
"federal_chancellor_of_germany",
# "federal_minister_for_economic_affairs_and_energy",
# "federal_minister_for_foreign_affairs",
# "federal_minister_for_special_affairs_of_germany",
# "federal_minister_for_the_environment._nature_conservation_and_nuclear_safety",
# "federal_minister_for_the_treasury",
# "federal_minister_of_defence",
# "federal_minister_of_economic_cooperation_and_development",
# "federal_minister_of_economics_and_technology",
# "federal_minister_of_education_and_research",
# "federal_minister_of_family_affairs._senior_citizens._women_and_youth",
# "federal_minister_of_finance",
# "federal_minister_of_food_and_agriculture",
# "federal_minister_of_health",
# "federal_minister_of_justice",
# "federal_minister_of_justice_and_consumer_protection",
# "federal_minister_of_labour_and_social_affairs",
# "federal_minister_of_the_interior",
# "federal_minister_of_transport_and_digital_infrastructure",
# "federal_minister_of_transportation",
# "federal_ministry_for_economic_cooperation_and_development",
# "federal_ministry_of_displaced_persons._refugees_and_war_victims",
# "federal_ministry_of_education_and_research",
# "federal_ministry_of_family_affairs._senior_citizens._women_and_youth",
# "federal_ministry_of_finance",
# "federal_ministry_of_food._agriculture_and_consumer_protection",
# "federal_ministry_of_justice",
# "federal_ministry_of_transport_and_digital_infrastructure",
"member_of_parliament",
"member_of_the_german_bundestag",
"president_of_the_bundestag",
"secretary_of_state",
#"minister",
# "party_leader",
"president",
"president_by_age",
"president_of_germany",
"president_of_the_bundestag"#,
# "transport_minister",
# "vice.chancellor_of_germany",
# "vice_president_of_the_bundestag")
)

Offices_sub <- Offices[, cols_names]

colnames(Offices_sub)

Parties <- subset(Parties, (Parties$session >= 12 & Parties$session <= 19))
MPs_and_parties <- relevant_MPs[, c("pageid", "wikidataid")] %>% 
  merge(Parties, by = "pageid", all.x = TRUE) %>% 
  merge(Offices_sub, by = "wikidataid", all.x = TRUE) 

save(MPs_and_parties, file = "./01_data/Wikipedia/output/MP2Party_CLD.RData")

load(file = "./01_data/Wikipedia/output/MP2Party_CLD.RData")

unique(MPs_and_parties$party)
# write.csv2(relevant_MPs, "./data/relevant_MPs.csv", row.names = FALSE)

## Import politicians entities --------------------------
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

colnames(MP_data)


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


