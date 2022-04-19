### Measuring the Significance of Political Elites
### Simon Munzert


## load packages and functions -------------------------------
source("/Users/alexandrarottenkolber/Documents/02_Hertie_School/Master thesis/Master_Thesis_Hertie/prominence-code_Simon_AR/_packages.r")
source("/Users/alexandrarottenkolber/Documents/02_Hertie_School/Master thesis/Master_Thesis_Hertie/prominence-code_Simon_AR/_functions.r")

getwd()
## download page view statistics -----------------------------

# import article names
load("./01_data/Wikipedia/output/MP_data.RData")
write.csv(MP_data,"./01_data/Wikipedia/output/MP_data.csv", row.names = FALSE)
urls <- MP_data$wikiurl

# define starting and ending date
date_start <- "2001011500" # founding data of Wikipedia
date_end <-   "2021123100"

# destination folder
destfolder <- "./01_data/Wikipedia/output/wikipageViews/"

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

?article_pageviews

#save(flag, file = "./data/output/flag_pageviews.RData")

length(flag)
basename(urls[flag])
file.remove(paste0(destfolder, basename(urls[flag]), ".csv"))




## import and aggregate page view statistics ------- Example for 2017
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






## import and aggregate page view statistics ------- => Do so for every year
options(warn=-1)
files <- list.files(destfolder, full.names = TRUE, pattern = ".+csv$")
pageviews_avg <- vector()
#years = c("2001", "2002", "2003", "2004", "2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022")
years = c("2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022")

for (y in seq_along(years)) {
  year = years[y]
  
  start_date_average = paste(year, "-01-01", sep = "")
  end_date_average = paste(year, "-12-31", sep = "")
  
  for (i in seq_along(files)) {
    try(pageviews_df <- read_csv(paste0(destfolder, basename(files[i])), col_types = cols()))
    try(pageviews_df <- filter(pageviews_df, date >= start_date_average, date < end_date_average))
    try(pageviews_avg[i] <- mean(pageviews_df$views, na.rm = TRUE))
    if (i%%1000==0) { print(paste0(i, " imports"))}
  }
  
  
  ## make data.frame, export --------------------------
  pageviews_df <- data.frame(wikiurl_basename = basename(files), pageviews_avg = pageviews_avg, start_date_average = start_date_average, end_date_average = end_date_average, stringsAsFactors = FALSE)
  save(pageviews_df,  file = paste("./01_data/Wikipedia/output/historical_pageviews/pageviews_average_", year,".RData", sep = ""))
}
options(warn=0)



#############################
load("./01_data/Wikipedia/output/pageviews_average_2017.RData")
load("./01_data/Wikipedia/output/flag_pageviews.RData")


?article_pageviews
