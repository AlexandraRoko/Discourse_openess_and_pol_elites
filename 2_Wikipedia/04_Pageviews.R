### Measuring the Significance of Political Elites
### Simon Munzert


## load packages and functions -------------------------------
source("/Users/alexandrarottenkolber/Documents/02_Hertie_School/Master thesis/Master_Thesis_Hertie/prominence-code_Simon_AR/_packages.r")
source("/Users/alexandrarottenkolber/Documents/02_Hertie_School/Master thesis/Master_Thesis_Hertie/prominence-code_Simon_AR/_functions.r")


## download page view statistics -----------------------------

# import article names
load("./data/output/MP_data.RData")
urls <- MP_data$wikiurl

# define starting and ending date
date_start <- "2001011500" # founding data of Wikipedia
date_end <-   "2021123100"

# destination folder
destfolder <- "./data/output/wikipageViews/"

basename(urls[3])

# download page view statistics as csv files
flag <- integer()
for (i in seq_along(urls)) {
  print(urls[i])
  if(!file.exists(paste0(destfolder, basename(urls[i]), ".csv"))) {
    tryCatch({article_pviews <- article_pageviews(project = "de.wikipedia",
                                                  article = URLdecode(basename(urls[i])),
                                                  platform = "all",
                                                  user_type = "user", 
                                                  start = date_start, end = date_end, 
                                                  reformat = TRUE)},
             error = function(err){
               message('On iteration ',i, ' there was an error: ',err)
               flag <<- c(flag,i)
             })
    try(write.csv(article_pviews, file = paste0(destfolder, basename(urls[i]), ".csv")))
  }
  if (i%%500==0) { print(paste0(i, " downloads"))}
}

#save(flag, file = "./data/output/flag_pageviews.RData")

length(flag)
basename(urls[flag])
file.remove(paste0(destfolder, basename(urls[flag]), ".csv"))




## import and aggregate page view statistics -------
files <- list.files(destfolder, full.names = TRUE, pattern = ".+csv$")
pageviews_avg <- vector()
start_date_average = "2017-01-01"
end_date_average = "2018-01-01"

for (i in seq_along(files)) {
  try(pageviews_df <- read_csv(paste0(destfolder, basename(files[i])), col_types = cols()))
  try(pageviews_df <- filter(pageviews_df, date >= start_date_average, date < end_date_average))
  try(pageviews_avg[i] <- mean(pageviews_df$views, na.rm = TRUE))
  if (i%%1000==0) { print(paste0(i, " imports"))}
}



## make data.frame, export --------------------------
pageviews_df <- data.frame(wikiurl_basename = basename(files), pageviews_avg = pageviews_avg, start_date_average = start_date_average, end_date_average = end_date_average, stringsAsFactors = FALSE)
save(pageviews_df,  file = "./data/output/pageviews_average_2017.RData")
