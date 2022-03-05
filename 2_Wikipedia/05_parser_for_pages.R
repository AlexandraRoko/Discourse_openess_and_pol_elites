### Measuring the Significance of Political Elites
### Simon Munzert


## load packages and functions -------------------------------
source("_packages.r")
source("_functions.r")


## import article data -------------------
folder <- "./data/output/politicians_DE_wikiarticles/"
files <- list.files(folder, full.names = TRUE)
files_parsed <- lapply(files, read_html)
names(files_parsed) <- basename(files)


# function to get internal links
get_internal_links <- function(one_parsed_file){
  # get all links in html
  link_list <- one_parsed_file %>% html_elements("body") %>% html_elements(xpath = "//a[starts-with(@href, '/wiki/')]")
  
  #find those that link to a different Wikipedia article
  filtered_list <- link_list[which(unlist(lapply(link_list, function(x) {stringr::str_detect(as.character(x), "/wiki/")}) == TRUE))] #Those which link to resource within Wikipedia will be labelled true
  filtered_list <- filtered_list[which(unlist(lapply(link_list, function(x) {stringr::str_detect(as.character(x), "/wiki/Datei") == FALSE} == TRUE)))] #Those that do not like to a file (picture) will be labelled true
  
  link_count = length(filtered_list)
  #return(c(link_count, list(filtered_list)))
  return(link_count)
}


# function to get external links
get_external_links <- function(one_parsed_file){
  # get all links in html
  link_list <- one_parsed_file %>% html_elements("body") %>% html_elements(xpath = "//a[@class='external text']")
  
  #find those that link to a different Wikipedia article
  filtered_list <- link_list[which(unlist(lapply(link_list, function(x) {stringr::str_detect(tolower(as.character(x)), "wiki")}) == FALSE))]
  filtered_list <- lapply(filtered_list, function(x) {as.character(x)}) 
  
  link_count = length(filtered_list)
  #return(c(link_count, list(filtered_list)))
  return(link_count)
}



## gather file size (in string length)
article_size <- sapply(files_parsed, str_length)

# number of references
article_refs <- sapply(files_parsed, function(x) {html_nodes(x, xpath = "//ol[@class='references']/li") %>% length})

# number of external links
article_extlinks <- sapply(files_parsed, get_external_links)

# number of internal links
articles_intlinks <- sapply(files_parsed, get_internal_links)




## build dataset; export ------------------- 
wikipages_df <- data.frame(article_name = str_replace(names(files_parsed), ".html$", ""), article_size, article_refs, article_extlinks, articles_intlinks, stringsAsFactors = FALSE, row.names = NULL)
save(wikipages_df, file = "./data/output/wikipages_df.RData")
