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
# Parties <- data.frame(semi_join(x = select(legislatoR::get_political(legislature = "deu"), pageid, party, session, service),
#                                 y = filter(legislatoR::get_political(legislature = "deu"), (session >=12 & session <=19)), 
#                                 by = "pageid"))

# Parties
Parties <- get_political(legislature = "deu")

colnames(Parties)

Parties <- Parties[, c("pageid","session","party","constituency2" )]

#Professions
Professions <- get_profession(legislature = "deu")
colnames(Professions)

# grouping inspried from ISCO
Associate_professionals_ls <- c("white.collar_worker", "social_worker", "school_teacher", "educator","teacher","pedagogue","high_school_teacher","industrial_management_assistant")
Science_and_engeneering_profs_ls <- c("theologian","scientist","slavicist","sociologist","physicist","political_scientist", "historian",
                                   "historian_of_the_modern_age", "ecologist", "economist", "electrical_engineer", "engineer", "academic", 
                                   "architect", "art_historian", "biochemist", "classical_philologist", "computer_scientist" , "chemist",
                                   "university_teacher","epidemiologist", "professor","philosopher", "mathematician", "literary_scholar","japanologist", "archivist")
Health_profs_ls <- c("psychiatrist","psychologist", "nurse", "psychotherapist", "cardiac_surgeon", "dentist", "pharmacist","neurologist", 
                  "pathologist", "internist","peace_researcher","pharmacologist")
Business_pref_ls <- c( "banker", "business_consultant", "chairperson","manager", "consultant", "businessperson","lobbyist","postgraduate_business_degree_holder",
                    "tax_advisor", "trade_unionist", "entrepreneur","intelligence_officer")
Politics_and_Administration_ls <- c("police_officer", "statesperson","member_of_the_german_bundestag","minister", "diplomat","politician")
Clerical_support_workers_ls <- c("catholic_priest", "pastor")
Service_and_sales_workers_ls <- c("bank_teller", "beauty_pageant_contestant", "beekeeper", "bookseller", "butcher", "civil_servant", "diplom.merchant","tailor", 
                               "physician","physiotherapist","merchant")
Skilled_agricultural_forestry_fishery_workers_ls <- c("agriculturer", "assessor","pundit", "zoologist","veterinarian", "government_veterinarian", "botanist")
law_ls <- c("judge","jurist","jurist.consultant", "justiciar","lawyer","notary","poet_lawyer")
Elementary_occupations_ls <- c("carpenter", "house_painter","locksmith", "technician","miller","miner", "auto_mechanic")
Armed_forces_occupations_ls <- c("career_soldier","temporary_career_soldier","soldier","generalstabsoffizier","resistance_fighter", "military_personnel","military_officer")
Spots_ls <- c("handball_player", "artistic_gymnast", "association_football_referee", 
           "athletics_competitor","speed_skater","sport_cyclist","singer","triathlete", "rowing_official","judoka","javelin_thrower")
creatives_Media_ls <- c("actor", "contributing_editor", "drafter", "film_director","film_producer", 
               "feminist","photographer","screenwriter","publisher","writer", "sculptor", "music_publisher", 
               "non.fiction_writer","pianist","novelist", "human_rights_activist","peace_activist",  "author", "biographer", "librarian","literary_editor",
               "television_presenter","opinion_journalist", "opinion_journalist","journalist","esperantist","translator","translators_and_interpreters"
               )




Professions_melted <- melt(Professions, "wikidataid", variable.name = "occupation")
Professions_melted$occupation <- as.character(Professions_melted$occupation)
Professions_melted <- Professions_melted[Professions_melted$value == TRUE, c("wikidataid", "occupation")]

length(Professions_melted$wikidataid)
length(unique(Professions_melted$wikidataid))


# create classified variable
Professions_melted <- Professions_melted %>% mutate(Associate_professionals = case_when(occupation %in% unlist(Associate_professionals_ls) ~ "yes", TRUE ~ "no")) %>%
  mutate(Science_and_engeneering_profs = case_when(occupation %in% unlist(Science_and_engeneering_profs_ls) ~ "yes", TRUE ~ "no")) %>%
  mutate(Health_profs = case_when(occupation %in% unlist(Health_profs_ls) ~ "yes", TRUE ~ "no")) %>%
  mutate(Business_pref = case_when(occupation %in% unlist(Business_pref_ls) ~ "yes", TRUE ~ "no")) %>%
  mutate(Politics_and_Administration = case_when(occupation %in% unlist(Politics_and_Administration_ls) ~ "yes", TRUE ~ "no")) %>%
  mutate(Clerical_support_workers = case_when(occupation %in% unlist(Clerical_support_workers_ls) ~ "yes", TRUE ~ "no")) %>%
  mutate(Service_and_sales_workers = case_when(occupation %in% unlist(Service_and_sales_workers_ls) ~ "yes", TRUE ~ "no")) %>%
  mutate(Skilled_agricultural_forestry_fishery_workers = case_when(occupation %in% unlist(Skilled_agricultural_forestry_fishery_workers_ls) ~ "yes", TRUE ~ "no")) %>%
  mutate(law = case_when(occupation %in% unlist(law_ls) ~ "yes", TRUE ~ "no")) %>%
  mutate(Elementary_occupations = case_when(occupation %in% unlist(Elementary_occupations_ls) ~ "yes", TRUE ~ "no")) %>%
  mutate(Armed_forces_occupations = case_when(occupation %in% unlist(Armed_forces_occupations_ls) ~ "yes", TRUE ~ "no")) %>%
  mutate(Spots = case_when(occupation %in% unlist(Spots_ls) ~ "yes", TRUE ~ "no")) %>%
  mutate(creatives_Media = case_when(occupation %in% unlist(creatives_Media_ls) ~ "yes", TRUE ~ "no"))


Professions_melted2 <- melt(Professions_melted, "wikidataid", variable.name = "occupation2")
Professions_melted2$occupation2 <- as.character(Professions_melted2$occupation2)
Professions_melted2 <- Professions_melted2[Professions_melted2$value == "yes", c("wikidataid", "occupation2")]

length(Professions_melted2$wikidataid)
length(unique(Professions_melted2$wikidataid))

remove_dups <- subset(Professions_melted2, duplicated(Professions_melted2$wikidataid))
remove_dups<-remove_dups[!(remove_dups$occupation2=="Politics_and_Administration"),]
remove_dups <- distinct(remove_dups)

Professions_melted3 <- Professions_melted2 %>% 
  merge(remove_dups, by = "wikidataid", all.x = TRUE) 


Professions_melted3$occupation <- ifelse(!is.na(Professions_melted3$occupation2.y), Professions_melted3$occupation2.y, Professions_melted3$occupation2.x)
Professions_melted3$occupation2.x <- NULL
Professions_melted3$occupation2.y <- NULL

Professions_melted3 <- distinct(Professions_melted3)

length(unique(Professions_melted3$wikidataid))
length((Professions_melted3$wikidataid))

Professions_melted3 <- Professions_melted3 %>% distinct(wikidataid, .keep_all= TRUE)

length(unique(Professions_melted3$wikidataid))
length((Professions_melted3$wikidataid))


# Research <- c()
# Science_and_engineering_professionals <- c()
# Health_professionals <- c()
# Teaching_professionals <- c()
# Business_and_administration_professionals <- c()
# Information_and_communications_technology_professionals <- c()
# Law <- c()
# Social_and_cultural_professionals <- c()
# Administration <- c()
# Blue.collar.worker <- c()
# White.collor.worker <- c()
# others <- c()
  

# Offices
Offices <- get_office(legislature = "deu")
colnames(Offices)

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

colnames(relevant_MPs)

MPs_and_parties <- relevant_MPs[, c("pageid", "wikidataid", "birth")] %>% 
  merge(Parties, by = "pageid", all.x = TRUE) %>% 
  merge(Offices_sub, by = "wikidataid", all.x = TRUE) %>% 
  merge(Professions_melted3, by = "wikidataid", all.x = TRUE)


MPs_meta_info <- relevant_MPs[, c( "pageid","wikidataid","wikititle","name","sex","ethnicity","religion","birth")] %>% 
  merge(Parties, by = "pageid", all.x = TRUE) %>% 
  merge(Offices_sub, by = "wikidataid", all.x = TRUE) %>% 
  merge(Professions_melted3, by = "wikidataid", all.x = TRUE)

colnames(MPs_and_parties)
colnames(MPs_meta_info)


#save(MPs_and_parties, file = "./01_data/Wikipedia/output/MP2Party_CLD.RData")
#save(MPs_meta_info, file = "./01_data/Wikipedia/output/MPs_meta_info.RData")

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


