# ------------------------------------------------------------------------
# Script name: 02_download_Wikipedia_articles.R
# Purpose of script: download Wikipedia article content
# Dependencies: 01_get_data_from_CLD_and_Wikidata.R
# Author: Simon Munzert, altered by Alexandra Rottenkolber
# ------------------------------------------------------------------------


library(WikipediR)
library(tidyverse)
library(legislatoR)
library(SPARQL) # SPARQL querying package
library(ggplot2)
library(readr)
library(stringr)

source("./_packages_Munzert.r")
source("./_functions_Munzert.r")


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


# Getting historical versions of Wikipedia articles is done in Python. See scripts in ./03_pull_historical_Wiki_data/

