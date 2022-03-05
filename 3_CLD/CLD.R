# load and attach legislatoR and dplyr
library(legislatoR)
library(dplyr)
library(utils)

getwd()
setwd("/Users/alexandrarottenkolber/Documents/02_Hertie_School/Master thesis/Master_Thesis_Hertie/data_analysis")

# assign entire Core table for the German Bundestag into the environment
deu_politicians <- get_core(legislature = "deu")

deu_politicial <- get_political(legislature = "deu")


# Politicians part of the parliament between 1990 and 2021
relevant_MPs <- data.frame(semi_join(x = get_core(legislature = "deu"),
                                    y = filter(get_political(legislature = "deu"), (session >=12 & session <=19)), 
                                    by = "pageid"))

write.csv2(relevant_MPs, "./data/relevant_MPs.csv", row.names = FALSE)

