## load packages and functions -------------------------------
source("/Users/alexandrarottenkolber/Documents/02_Hertie_School/Master thesis/Master_Thesis_Hertie/prominence-code_Simon_AR/_packages.r")
source("/Users/alexandrarottenkolber/Documents/02_Hertie_School/Master thesis/Master_Thesis_Hertie/prominence-code_Simon_AR/_functions.r")

library("data.table")
library("stringr")


setwd("/Users/alexandrarottenkolber/Documents/02_Hertie_School/Master thesis/Master_Thesis_Hertie/data_analysis")

## load data ----------------------------------------------
load("./01_data/Wikipedia/output/wikimeasures_df.RData")

colnames(wikimeasures_df)

#nov_res_df <- read.csv("./01_data/Plenarprotokolle/processed/nov_res_trans_df_naive_window10.csv")
PolID2Info_df <- read.csv("./01_data/Plenarprotokolle/processed/PolIDtoInfo_df_uniqueIDs.csv")
PolID2SpeechID_df <- read.csv("./01_data/Plenarprotokolle/processed/PolID2SpeechID_df.csv")

ID_and_names <- subset(wikimeasures_df, select=c("pageid", "wikidataid", "name"))
ID_and_names <- ID_and_names %>% distinct()
#write.csv(ID_and_names,"./01_data/Plenarprotokolle/processed/ID_and_names_from_Wiki.csv", row.names = FALSE)

#nov_res_names <- subset(nov_res_df, select=c("name", "lastName"))
PolID2Info_df_names <- subset(PolID2Info_df, select=c("politicianID", "firstName", "lastName"))

PolID2SpeechID_df$surname_lower <- tolower(PolID2SpeechID_df$lastName)
PolID2SpeechID_df$forename_lower <- tolower(PolID2SpeechID_df$firstName)

extract_surname <- function(name_string){
  return (as.character(tolower(last(str_split(name_string, " ")[[1]]))))
}

name_string <-  "A random name"

str(tolower(last(str_split(name_string, " ")[[1]])))
length(tolower(str_split(name_string, " ")[[1]]))


ls_names = length(tolower(str_split(name_string, " ")[[1]]))
num = ls_names-1
paste(tolower(first(str_split(name_string, " ")[[1]], n = num)), collapse=" ")

extract_forename <- function(name_string){
  ls_names = length(tolower(str_split(name_string, " ")[[1]]))
  num = ls_names-1
  # len_ls = length(ls_names)
  # fore_names = len_ls[c(-len_ls)]
  # return (fore_names)
  return (paste(tolower(first(str_split(name_string, " ")[[1]], n = num)), collapse=" "))
}

?first

ID_and_names$surname_lower <- base::lapply(ID_and_names$name, extract_surname)
ID_and_names$forename_lower <- lapply(ID_and_names$name, extract_forename)

colnames(ID_and_names)
ID_and_names$surname_lower <- as.character(ID_and_names$surname_lower) 
ID_and_names$forename_lower <- as.character(ID_and_names$forename_lower) 

#write.csv(ID_and_names,"./01_data/Plenarprotokolle/processed/ID_and_names_from_Wiki.csv", row.names = FALSE)




