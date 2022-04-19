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


## import Wikidata database
load("/Users/alexandrarottenkolber/Documents/02_Hertie_School/Master thesis/Master_Thesis_Hertie/prominence-code_Simon/data/wikidata_df.RData")
load("./01_data/Wikipedia/output/MP_data.RData")
colnames(wikidata_df)

urls <- MP_data$wikiurl

## download articles (careful; takes hours!) -----------
flag <- integer()
for (i in seq_along(urls)) {
  fname <- paste0("./data/output/politicians_DE_wikiarticles/", basename(urls[i]), ".html")
  if (!file.exists(fname)) {
    tryCatch({ wp_content <- page_content("de", "wikipedia", page_name = URLdecode(basename(urls[i])))},
             error = function(err){
               message('On iteration ',i, ' there was an error: ',err)
               flag <<- c(flag,i)
             })
    
    if(class(wp_content)!="try-error") write(wp_content$parse$text$`*`, file = fname)
  }
  if (i%%1000==0) { print(paste0(i, " downloads"))}
}
save(flag, file = "./data/output/flag_downloads.RData")
flag
basename(urls[flag])



# ## Getting historical Wikipedia data. => MOVED TO PYTHON
# flag <- integer()
# history_revision_id<- read.csv("./01_data/Wikipedia/output/history_revisions_ids_names_complete.csv") # prepared by data_prep_for_historical_pagerank.ipynb
# ?page_content
# colnames(history_revision_id)
# 
# urls <- history_revision_id$wikiurl_basenames
# unique(urls)
# rev_ids <- history_revision_id$revision_id
# years <- history_revision_id$year
# 
# for (i in seq_along(rev_ids)) {
#   #print(rev_id)
#   year <- history_revision_id$year[history_revision_id$revision_id == rev_ids[i]]
#   #print(year)
#   url_name <- history_revision_id$wikiurl_basenames[history_revision_id$revision_id == rev_ids[i]]
#   fname <- paste0("./01_data/Wikipedia/output/politicians_DE_wikiarticles_history/politicians_DE_wikiarticles", as.character(year), "/", basename(url_name), ".html")
#   #print(fname)
#   if (!file.exists(fname)) {
#     tryCatch({ rv_content <- revision_content("de", 
#                                               "wikipedia", 
#                                               page_name = URLdecode(basename(url_name)),
#                                               properties = c("content", "ids", "flags", "timestamp", "user",
#                                                              "userid", "size", "sha1", "contentmodel", "comment", "parsedcomment", "tags"),
#                                               clean_response = TRUE,
#                                               revisions = rev_ids[i])},
#              error = function(err){
#                message('On iteration ',rev_ids[i], ' there was an error: ',err)
#                flag <<- c(flag,rev_ids[i])
#              })
#     
#     if(class(rv_content)!="try-error") write(rv_content[[1]]$revisions[[1]]$`*`, file = fname)
#   }
#   if (i%%1000==0) { print(paste0(i, " downloads"))}
# }
# 
# save(flag, file = "./01_data/Wikipedia/output/politicians_DE_wikiarticles_history/flag_downloads.RData")
# 
# 
# 
# wp_content <- page_content("de", "wikipedia", page_name = URLdecode(basename(urls[2])))
# wp_content
# wp_content$parse$text$`*`



rv_content <- revision_content("de", 
                               "wikipedia", 
                               page_name = URLdecode(basename(urls[2])),
                               properties = c("content", "ids", "flags", "timestamp", "user",
                                              "userid", "size", "sha1", "contentmodel", "comment", "parsedcomment", "tags"),
                               clean_response = TRUE,
                               revisions = rev_ids[2])




rv_content[[1]]$revisions[[1]]$`*`
