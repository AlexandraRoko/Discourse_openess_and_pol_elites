

## load packages and functions -------------------------------
source("/Users/alexandrarottenkolber/Documents/02_Hertie_School/Master thesis/Master_Thesis_Hertie/prominence-code_Simon_AR/_packages.r")
source("/Users/alexandrarottenkolber/Documents/02_Hertie_School/Master thesis/Master_Thesis_Hertie/prominence-code_Simon_AR/_functions.r")


### GRAPH FOR ALL POLITICIANS --------------------------------

## import article data -------------------
folder <- "./01_data/Wikipedia/output/politicians_DE_wikiarticles/"
files <- list.files(folder, full.names = TRUE)
files_parsed <- lapply(files, read_html)
names(files_parsed) <- basename(files)
article_url <- paste0("/wiki/", str_replace(basename(files), ".html$", ""))


## identify links between articles; paragraphs only -----
connections <- data.frame(from = NULL, to = NULL)
for (i in seq_along(files_parsed)) {
  pslinks <- html_attr(
    html_nodes(files_parsed[[i]], xpath = "//p//a"), # only links in paragraphs; excludes summary tables as they inflate any pageRank-based measure (everything links to everything)
    "href")
  links_in_pslinks <- seq_along(files_parsed)[article_url %in% pslinks]
  links_in_pslinks <- links_in_pslinks[links_in_pslinks != i]
  connections <- rbind(
    connections,
    data.frame(
      from = rep(i, length(links_in_pslinks)),
      to = links_in_pslinks
    )
  )
  if (i%%1000==0) { print(paste0(i, " cases"))}
}


## identify links between articles; all links -----
connections_all <- data.frame(from = NULL, to = NULL)
for (i in seq_along(files_parsed)) {
  pslinks <- html_attr(
    html_nodes(files_parsed[[i]], xpath = "//a"), # all links
    "href")
  links_in_pslinks <- seq_along(files_parsed)[article_url %in% pslinks]
  links_in_pslinks <- links_in_pslinks[links_in_pslinks != i]
  connections_all <- rbind(
    connections_all,
    data.frame(
      from = rep(i, length(links_in_pslinks)),
      to = links_in_pslinks
    )
  )
  if (i%%1000==0) { print(paste0(i, " cases"))}
}


# add artificial edge for last observation to get length of graph right
connections[nrow(connections+1),] <- c(length(article_url), length(article_url)-1)
connections_all[nrow(connections_all+1),] <- c(length(article_url), length(article_url)-1)


## build connections data frame and directed graph  -----
names(connections) <- c("from", "to")
head(connections)
graph_wiki <- graph_from_edgelist(as.matrix(connections), directed = TRUE)

names(connections_all) <- c("from", "to")
head(connections_all)
graph_wiki_all <- graph_from_edgelist(as.matrix(connections_all), directed = TRUE)


## export connections data + graphs --------------------
article_name <- str_replace(basename(files), ".html$", "")
#save(article_name, connections, connections_all, graph_wiki, graph_wiki_all, file = "./data/output/wikiGraph.RData")

pagerank <- graph_wiki %>% page.rank(directed = TRUE) %>%  use_series("vector") 
pagerank_all <- graph_wiki_all %>% page.rank(directed = TRUE) %>%  use_series("vector") 
plot(pagerank, pagerank_all)
data.frame(article_name, as.numeric(pagerank)) %>% View()
length(unique(article_name))




### GRAPH FOR ALL POLITICIANS --- HISTROICAL VERSION  --------------------------------

## import article data -------------------
years <- c("2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021")
#paste0("./01_data/Wikipedia/output/politicians_DE_wikiarticles_history/politicians_DE_wikiarticles", years[1], "/", sep = "")
#years <- c("2021", "2020")

for (k in seq_along(years)){
  folder <- paste0("./01_data/Wikipedia/output/politicians_DE_wikiarticles_history/politicians_DE_wikiarticles", years[k], "/", sep = "")
  folder
  files <- list.files(folder, full.names = TRUE)
  files_parsed <- lapply(files, read_html)
  names(files_parsed) <- basename(files)
  article_url <- paste0("/wiki/", str_replace(basename(files), ".html$", ""))
  print(length(files))
  
  
  ## identify links between articles; paragraphs only -----
  connections <- data.frame(from = NULL, to = NULL)
  for (i in seq_along(files_parsed)) {
    pslinks <- html_attr(
      html_nodes(files_parsed[[i]], xpath = "//p//a"), # only links in paragraphs; excludes summary tables as they inflate any pageRank-based measure (everything links to everything)
      "href")
    links_in_pslinks <- seq_along(files_parsed)[article_url %in% pslinks]
    links_in_pslinks <- links_in_pslinks[links_in_pslinks != i]
    connections <- rbind(
      connections,
      data.frame(
        from = rep(i, length(links_in_pslinks)),
        to = links_in_pslinks
      )
    )
    if (i%%1000==0) { print(paste0(i, " cases"))}
  }
  
  
  ## identify links between articles; all links -----
  connections_all <- data.frame(from = NULL, to = NULL)
  for (i in seq_along(files_parsed)) {
    pslinks <- html_attr(
      html_nodes(files_parsed[[i]], xpath = "//a"), # all links
      "href")
    links_in_pslinks <- seq_along(files_parsed)[article_url %in% pslinks]
    links_in_pslinks <- links_in_pslinks[links_in_pslinks != i]
    connections_all <- rbind(
      connections_all,
      data.frame(
        from = rep(i, length(links_in_pslinks)),
        to = links_in_pslinks
      )
    )
    if (i%%1000==0) { print(paste0(i, " cases"))}
  }
  
  
  # add artificial edge for last observation to get length of graph right
  connections[nrow(connections+1),] <- c(length(article_url), length(article_url)-1)
  connections_all[nrow(connections_all+1),] <- c(length(article_url), length(article_url)-1)
  
  
  ## build connections data frame and directed graph  -----
  names(connections) <- c("from", "to")
  head(connections)
  graph_wiki <- graph_from_edgelist(as.matrix(connections), directed = TRUE)
  
  names(connections_all) <- c("from", "to")
  head(connections_all)
  graph_wiki_all <- graph_from_edgelist(as.matrix(connections_all), directed = TRUE)
  
  
  ## export connections data + graphs --------------------
  article_name <- str_replace(basename(files), ".html$", "")
  save(article_name, connections, connections_all, graph_wiki, graph_wiki_all, file = paste0("./01_data/Wikipedia/output/historical_wikigraphs/wikiGraph", years[k],".RData", sep = ""))
  #paste0("./01_data/Wikipedia/output/politicians_DE_wikiarticles_history/politicians_DE_wikiarticles", years[i], "/", sep = "")
  
  #pagerank <- graph_wiki %>% page.rank(directed = TRUE) %>%  use_series("vector") 
  #pagerank_all <- graph_wiki_all %>% page.rank(directed = TRUE) %>%  use_series("vector") 
  #plot(pagerank, pagerank_all)
  #data.frame(article_name, as.numeric(pagerank)) %>% View()
  #length(unique(article_name))
  
  print(paste0("Done with year: ", years[k]))

}



# ### GRAPH FOR MEMBERS OF 115TH CONGRESS --------------------------------
# 
# ## load Congress data
# load("../data/wikimeasures_congress115_df.RData")
# congress_names <- wikimeasures_congress_df$wikititle
# 
# ## import article data -------------------
# folder <- "../data/wikiPagesEn/politicians_USA_20180126/"
# files <- list.files(folder, full.names = TRUE)
# filenames <- basename(files) %>% str_replace(".html$", "") %>% sapply(URLdecode)
# filenames_congress <- filenames %in% congress_names 
# files_congress <- files[filenames_congress]
# 
# # order according to PageRank (sort of... see plotDescriptives)
# vec_list <- list()
# for(i in 1:10) { 
#   vec_list[[i]] <- seq(i,512,10)
# }
# vec_order <- unlist(vec_list)
# files_congress <- files_congress[order(wikimeasures_congress_df$pagerank)[vec_order]]
# 
# files_parsed <- lapply(files_congress, read_html)
# names(files_parsed) <- basename(files_congress)
# article_url <- paste0("/wiki/", str_replace(basename(files_congress), ".html$", ""))
# 
# 
# ## identify links between articles; paragraphs only -----
# connections <- data.frame(from = NULL, to = NULL)
# for (i in seq_along(files_parsed)) {
#   pslinks <- html_attr(
#     html_nodes(files_parsed[[i]], xpath = "//p//a"), # only links in paragraphs; excludes summary tables as they inflate any pageRank-based measure (everything links to everything)
#     "href")
#   links_in_pslinks <- seq_along(files_parsed)[article_url %in% pslinks]
#   links_in_pslinks <- links_in_pslinks[links_in_pslinks != i]
#   connections <- rbind(
#     connections,
#     data.frame(
#       from = rep(i, length(links_in_pslinks)),
#       to = links_in_pslinks
#     )
#   )
#   if (i%%50==0) { print(paste0(i, " cases"))}
# }
# 
# 
# # add artificial edge for last observation to get length of graph right
# connections[nrow(connections+1),] <- c(length(article_url), length(article_url)-1)
# 
# 
# ## build connections data frame and directed graph  -----
# names(connections) <- c("from", "to")
# head(connections)
# graph_wiki <- graph_from_edgelist(as.matrix(connections), directed = TRUE)
# 
# ## export connections data + graphs --------------------
# article_name <- str_replace(basename(files_congress), ".html$", "")
# save(article_name, connections, graph_wiki, file = "../data/wikiGraphCongress.RData")

