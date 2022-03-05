getwd()
setwd("/Users/alexandrarottenkolber/Documents/02_Hertie_School/Master thesis/Master_Thesis_Hertie/data_analysis")


## import Wikidata database
load("./data/output/wikidata_df.RData")
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

