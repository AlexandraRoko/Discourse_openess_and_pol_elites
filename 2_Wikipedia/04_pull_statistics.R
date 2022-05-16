# ------------------------------------------------------------------------
# Script name: 04_pull_statistics.R
# Purpose of script: download Wikipedia statistics per article
# Dependencies: 01_get_data_from_CLD_and_Wikidata.R
# Author: Simon Munzert, altered by Alexandra Rottenkolber
# ------------------------------------------------------------------------

#setwd("XX")

## load packages and functions -------------------------------
source("./_packages_Munzert.r")
source("./_functions_Munzert.r")


## import Wikidata 
load("./data/output/MP_data.RData")
urls <- MP_data$wikiurl
urls_basenames <- basename(urls)


# access xtools API to get article infos + prose statistics ----------------------
endpoint_prose <- "https://xtools.wmflabs.org/api/page/prose/de.wikipedia.org/"
endpoint_info <- "https://xtools.wmflabs.org/api/page/articleinfo/de.wikipedia.org/"

page_prose_statistics_list <- list()
page_info_statistics_list <- list()

# run only to pull data:

# for (i in 1:length(urls_basenames)) {
#   page_prose_statistics_list[[i]] <- try(fromJSON(paste0(endpoint_prose, urls_basenames[i])) %>% unlist)
#   page_info_statistics_list[[i]] <- try(fromJSON(paste0(endpoint_info, urls_basenames[i])) %>% unlist)
#   if (i%%10==0) { print(paste0(i, " calls"))}
#   Sys.sleep(2)
# }

#save(page_prose_statistics_list, page_info_statistics_list, file = "./data/output/pageStatistics_pulled.RData")


load(paste0("./01_data/Wikipedia/output/pageStatistics_pulled", ".RData"))


page_info_statistics_list_full <- list()
page_prose_statistics_list_full <- list()

for (i in 1:1) {
  load(paste0("./01_data/Wikipedia/output/pageStatistics_pulled", ".RData"))
  page_info_statistics_list_full[[i]] <- page_info_statistics_list[sapply(page_info_statistics_list, class) == "character"]
  page_prose_statistics_list_full[[i]] <- page_prose_statistics_list[sapply(page_prose_statistics_list, class) == "character"]
}

page_info_statistics_df_list <- list()
page_prose_statistics_df_list <- list()

for (i in 1:1) {
  page_info_statistics_df_list[[i]] <- data.frame(do.call(rbind, page_info_statistics_list_full[[i]]), stringsAsFactors=FALSE)
  page_prose_statistics_df_list[[i]] <- data.frame(do.call(rbind, page_prose_statistics_list_full[[i]]), stringsAsFactors=FALSE)
}

page_info_statistics_df <- ldply(page_info_statistics_df_list, data.frame)
page_prose_statistics_df <- ldply(page_prose_statistics_df_list, data.frame)

page_info_statistics_df <- distinct(page_info_statistics_df, page, .keep_all = TRUE)
page_prose_statistics_df <- distinct(page_prose_statistics_df, page, .keep_all = TRUE)

colnames(page_info_statistics_df)
colnames(page_prose_statistics_df)


# correct errors originated when merging
flagged_rows <- list()
for (i in 1:length(page_info_statistics_df$author_editcount)) {
  entry <- page_info_statistics_df$author_editcount[i]
  page <- page_info_statistics_df$page[i]
  if (str_detect(entry, "^\\d{4}\\-(0[1-9]|1[012])\\-(0[1-9]|[12][0-9]|3[01])$") == TRUE) {
    flagged_rows <- append(flagged_rows, page)
  }
}  

flagged_df <- page_info_statistics_df[page_info_statistics_df$page %in% flagged_rows,]
unflagged_df <- page_info_statistics_df[!(page_info_statistics_df$page %in% flagged_rows),]

flagged_df <- dplyr::rename(flagged_df, corr = elapsed_time)

flagged_df$corr <- NA
flagged_df <- flagged_df %>% dplyr::rename(elapsed_time = assessment, 
                                           assessment = last_edit_id,
                                           last_edit_id = secs_since_last_edit,
                                           secs_since_last_edit = modified_at,
                                           modified_at = created_rev_id, 
                                           created_rev_id = created_at, 
                                           created_at = author_editcount, 
                                           author_editcount = corr)

page_info_statistics_df <- plyr::rbind.fill(flagged_df, unflagged_df)
sum(is.na(page_info_statistics_df$author_editcount))

#save(page_info_statistics_df, page_prose_statistics_df, file = "./data/output/pageStatistics_df.RData")


## Pull missings
load("./01_data/Wikipedia/output/pageStatistics_df.RData")
MP_data$pagenames <- MP_data$wikititle %>% sapply(URLdecode) %>% str_replace_all("_", " ")

page_info_statistics_nacherhebung <- MP_data$wikiurl[!(MP_data$pagenames %in% page_info_statistics_df$page)] %>% basename
page_prose_statistics_nacherhebung <- MP_data$wikiurl[!(MP_data$wikititle %in% page_prose_statistics_df$page)] %>% basename
urls_basenames_missing <- union(page_info_statistics_nacherhebung,page_prose_statistics_nacherhebung)

colnames(MP_data)
page_prose_statistics_df$page[[1]]


page_prose_statistics_list <- list()
page_info_statistics_list <- list()

for (i in 1:length(urls_basenames_missing)) {
  page_prose_statistics_list[[i]] <- try(fromJSON(paste0(endpoint_prose, urls_basenames_missing[i])) %>% unlist)
  page_info_statistics_list[[i]] <- try(fromJSON(paste0(endpoint_info, urls_basenames_missing[i])) %>% unlist)
  if (i%%10==0) { print(paste0(i, " calls"))}
  Sys.sleep(2)
}


#save(page_prose_statistics_list, page_info_statistics_list, file = "./data/output/pageStatistics_missings_pulled.RData")

load("./01_data/Wikipedia/output/pageStatistics_missings_pulled.RData")

page_info_statistics_list_full <- list()
page_prose_statistics_list_full <- list()

for (i in 1:1) {
  load(paste0("./01_data/Wikipedia/output/pageStatistics_missings_pulled", ".RData"))
  page_info_statistics_list_full[[i]] <- page_info_statistics_list[sapply(page_info_statistics_list, class) == "character"]
  page_prose_statistics_list_full[[i]] <- page_prose_statistics_list[sapply(page_prose_statistics_list, class) == "character"]
}

page_info_statistics_df_list <- list()
page_prose_statistics_df_list <- list()

for (i in 1:1) {
  page_info_statistics_df_list[[i]] <- data.frame(do.call(rbind, page_info_statistics_list_full[[i]]), stringsAsFactors=FALSE)
  page_prose_statistics_df_list[[i]] <- data.frame(do.call(rbind, page_prose_statistics_list_full[[i]]), stringsAsFactors=FALSE)
}

page_info_statistics_df_missings <- ldply(page_info_statistics_df_list, data.frame)
page_prose_statistics_df_missings <- ldply(page_prose_statistics_df_list, data.frame)

page_info_statistics_df_missings <- distinct(page_info_statistics_df_missings, page, .keep_all = TRUE)
page_prose_statistics_df_missings <- distinct(page_prose_statistics_df_missings, page, .keep_all = TRUE)


# correct errors originated when merging missings
flagged_rows <- list()
for (i in 1:length(page_info_statistics_df_missings$author_editcount)) {
  entry <- page_info_statistics_df_missings$author_editcount[i]
  page <- page_info_statistics_df_missings$page[i]
  if (str_detect(entry, "^\\d{4}\\-(0[1-9]|1[012])\\-(0[1-9]|[12][0-9]|3[01])$") == TRUE) {
    flagged_rows <- append(flagged_rows, page)
  }
}  

flagged_df <- page_info_statistics_df_missings[page_info_statistics_df_missings$page %in% flagged_rows,]
unflagged_df <- page_info_statistics_df_missings[!(page_info_statistics_df_missings$page %in% flagged_rows),]

flagged_df <- dplyr::rename(flagged_df, corr = elapsed_time)

flagged_df$corr <- NA
flagged_df <- flagged_df %>% dplyr::rename(elapsed_time = assessment, 
                                           assessment = last_edit_id,
                                           last_edit_id = secs_since_last_edit,
                                           secs_since_last_edit = modified_at,
                                           modified_at = created_rev_id, 
                                           created_rev_id = created_at, 
                                           created_at = author_editcount, 
                                           author_editcount = corr)

page_info_statistics_df_missings <- plyr::rbind.fill(flagged_df, unflagged_df)
sum(is.na(page_info_statistics_df_missings$author_editcount))

#save(page_info_statistics_df_missings, page_prose_statistics_df_missings, file = "./data/output/pageStatistics_df_missings.RData")

load("./01_data/Wikipedia/output/pageStatistics_df_missings.RData")

colnames(page_info_statistics_df_missings)
colnames(page_prose_statistics_df_missings)

# combine both data sets

page_info_statistics_df_complete <- rbind(page_info_statistics_df, page_info_statistics_df_missings)
page_prose_statistics_df_complete <- rbind(page_prose_statistics_df, page_prose_statistics_df_missings)

colnames(page_info_statistics_df_complete)
colnames(page_prose_statistics_df_complete)

#save(page_info_statistics_df_complete, page_prose_statistics_df_complete, file = "./01_data/Wikipedia/output/pageStatistics_df_complete.RData")
