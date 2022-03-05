### Measuring the Significance of Political Elites
### Simon Munzert

getwd()
## load packages and functions -------------------------------
source("/Users/alexandrarottenkolber/Documents/02_Hertie_School/Master thesis/Master_Thesis_Hertie/prominence-code_Simon_AR/_packages.r")
source("/Users/alexandrarottenkolber/Documents/02_Hertie_School/Master thesis/Master_Thesis_Hertie/prominence-code_Simon_AR/_functions.r")

getwd()
setwd("/Users/alexandrarottenkolber/Documents/02_Hertie_School/Master thesis/Master_Thesis_Hertie/data_analysis")

## import Wikidata database
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


# -------------- Alex attempt different data frame ----------------
#attributes(page_info_statistics_list[[1]])
#typeof(page_info_statistics_list[1])
#page_info_statistics_list[[1]]["project"]

#test_frame <- rbind.fill(as.data.frame(t(page_info_statistics_list[[1]])), as.data.frame(t(page_info_statistics_list[[2]])) , as.data.frame(t(page_info_statistics_list[[3]])))
#?data.frame

#list_of_info_dfs <- list()
#list_of_prose_dfs <- list()

#for (i in 1:length(page_info_statistics_list)) {
#  list_of_info_dfs <- append(list_of_info_dfs, data.frame(t(page_info_statistics_list[i])))
#  list_of_prose_dfs <- append(list_of_prose_dfs, data.frame(t(page_prose_statistics_list[i])))
#}

#rbind.fill(list_of_info_dfs)

#list_of_info_dfs2 <- ldply(page_info_statistics_list, data.frame)
#list_of_prose_dfs2 <- ldply(page_prose_statistics_list, data.frame)

#page_info_statistics_list_full <- page_info_statistics_list[sapply(page_info_statistics_list, class) == "character"]

load(paste0("./data/output/pageStatistics_pulled", ".RData"))

page_info_statistics_list_full <- list()
page_prose_statistics_list_full <- list()

for (i in 1:1) {
  load(paste0("./data/output/pageStatistics_pulled", ".RData"))
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

load("./data/output/pageStatistics_df.RData")



## Pull missings
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

load("./data/output/pageStatistics_missings_pulled.RData")

page_info_statistics_list_full <- list()
page_prose_statistics_list_full <- list()


for (i in 1:1) {
  load(paste0("./data/output/pageStatistics_missings_pulled", ".RData"))
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

load("./data/output/pageStatistics_df_missings.RData")

# combine both data sets

page_info_statistics_df_complete <- rbind(page_info_statistics_df, page_info_statistics_df_missings)
page_prose_statistics_df_complete <- rbind(page_prose_statistics_df, page_prose_statistics_df_missings)



save(page_info_statistics_df_complete, page_prose_statistics_df_complete, file = "./data/output/pageStatistics_df_complete.RData")
