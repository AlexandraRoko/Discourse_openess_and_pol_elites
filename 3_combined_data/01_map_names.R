
# ------------------------------------------------------------------------
# Script name: 01_map_names.R
# Purpose of script: extract names that enable merging of Wikipedia data and KL-based measures
# Author: Alexandra Rottenkolber
# ------------------------------------------------------------------------



## load packages and functions -------------------------------
source("./_packages_Munzert.r")
source("./_functions_Munzert.r")

library("data.table")
library("stringr")


## load data ----------------------------------------------
load("./01_data/Wikipedia/output/wikimeasures_df.RData")


#nov_res_df <- read.csv("./01_data/Plenarprotokolle/processed/nov_res_trans_df_naive_window10.csv")
PolID2Info_df <- read.csv("./01_data/Plenarprotokolle/processed/PolIDtoInfo_df_uniqueIDs.csv")
PolID2SpeechID_df <- read.csv("./01_data/Plenarprotokolle/processed/PolID2SpeechID_df.csv")

ID_and_names <- subset(wikimeasures_df, select=c("pageid", "wikidataid", "name"))
ID_and_names <- ID_and_names %>% distinct()

PolID2SpeechID_df$surname_lower <- tolower(PolID2SpeechID_df$lastName)
PolID2SpeechID_df$forename_lower <- tolower(PolID2SpeechID_df$firstName)

extract_surname <- function(name_string){
  return (as.character(tolower(last(str_split(name_string, " ")[[1]]))))
}

extract_forename <- function(name_string){
  ls_names = length(tolower(str_split(name_string, " ")[[1]]))
  num = ls_names-1
  
  return (paste(tolower(first(str_split(name_string, " ")[[1]], n = num)), collapse=" "))
}


ID_and_names$surname_lower <- base::lapply(ID_and_names$name, extract_surname)
ID_and_names$forename_lower <- lapply(ID_and_names$name, extract_forename)

colnames(ID_and_names)
ID_and_names$surname_lower <- as.character(ID_and_names$surname_lower) 
ID_and_names$forename_lower <- as.character(ID_and_names$forename_lower) 

#write.csv(ID_and_names,"./01_data/Plenarprotokolle/processed/ID_and_names_from_Wiki.csv", row.names = FALSE)

