# COMMENT: Combining the data was done in python
library(descr)
library(plm)
library(dplyr)
library(stargazer)
library(stringr)
library(gtools)
library(latex2exp)

setwd("/Users/alexandrarottenkolber/Documents/02_Hertie_School/Master thesis/Master_Thesis_Hertie/data_analysis")

source("./00_MPP_thesis_code/4_combined_data/helpers.R")

## load data ----------------------------------------------


NamesWPSpeeches <- read.csv("./01_data/Plenarprotokolle/processed/NamesWPSpeeches_with_AfD.csv")
NamesWPSpeeches_groupedYear <- read.csv("./01_data/Plenarprotokolle/processed/NamesWPSpeeches_grouped_by_IDNameYear_with_AfD.csv")


sum(is.na(NamesWPSpeeches_groupedYear$novelty) == TRUE) / length(NamesWPSpeeches_groupedYear$novelty) # 0.3% are NaNs


load(file = "./01_data/Wikipedia/output/wikimeasures_df_fa_without_PageRankGlobal_with_timedim.RData")
#load("./01_data/Wikipedia/output/wikimeasures_df.RData")
#load("./01_data/Wikipedia/output/wikimeasures_df_fascores.RData")
#load("./01_data/Wikipedia/output/wikimeasures_df_fa.RData")
#wikimeasures_df_fa <- read.csv("./01_data/Wikipedia/output/wikimeasures_df_fa.csv")

#load(file = "./01_data/Wikipedia/output/MP2Party_CLD.RData")
load(file = "./01_data/Wikipedia/output/MPs_meta_info.RData")

#colnames(wikimeasures_df_fa)
colnames(NamesWPSpeeches)
colnames(NamesWPSpeeches_groupedYear)


#----------------------------------------

NamesWPSpeeches <- NamesWPSpeeches[order(NamesWPSpeeches$wikidataid,NamesWPSpeeches$year),]
wikimeasures_df <- wikimeasures_df[order(wikimeasures_df$wikidataid),]

# merged_df <- merge(NamesWPSpeeches[ , c("year", "month", "wikidataid","name", "novelty","transience","resonance","politicianID", "pageid")], 
#                    wikimeasures_df[ , c("wikidataid",
#                                         "YEAR",
#                                         # "name",
#                                         # "wikiurl_basename_clean",
#                                         # "sex", 
#                                         # "ethnicity", 
#                                         # "religion", 
#                                         "wikidataid",
#                                         "fa_importance",
#                                         "fa_importance_rank",
#                                         "fa_influence",
#                                         "fa_prominence",
#                                         "fa_influence_rank",
#                                         "fa_prominence_rank",
#                                         "fa_1",
#                                         "fa_2",
#                                         "fa_3",
#                                         "fa_1_rank",
#                                         "fa_2_rank",
#                                         "fa_3_rank",
#                                         "fa_importance_fa",
#                                         "fa_importance_rank_fa",
#                                         "fa_influence_fa",
#                                         "fa_prominence_fa",
#                                         "fa_influence_rank_fa",
#                                         "fa_prominence_rank_fa",
#                                         "fa_1_fa",
#                                         "fa_2_fa",
#                                         "fa_3_fa",
#                                         "fa_1_rank_fa",
#                                         "fa_2_rank_fa",
#                                         "fa_3_rank_fa")] , by.x=c("wikidataid", "year"), by.y=c("wikidataid", "YEAR"), all.x = TRUE, all.y = TRUE)


# colnames(MPs_meta_info)
# 
# merged_df <- merged_df[order(merged_df$wikidataid),]
# length(unique(merged_df$wikidataid)) #2184 unique politicians

colnames(MPs_meta_info)
#ROLE variable 
MPs_meta_info$ROLE <- "Member of Parliament"
MPs_meta_info$ROLE[MPs_meta_info$bundesminister == TRUE] <- "Minister"
MPs_meta_info$ROLE[MPs_meta_info$secretary_of_state == TRUE] <- "Secretary of state"
MPs_meta_info$ROLE[MPs_meta_info$federal_chancellor_of_germany == TRUE] <- "Chancellor"
MPs_meta_info$ROLE[MPs_meta_info$president_of_the_bundestag == TRUE] <- "President of Bundestag"

MPs_meta_info$bundesminister <- NULL
MPs_meta_info$secretary_of_state <- NULL 
MPs_meta_info$federal_chancellor_of_germany <- NULL 
MPs_meta_info$president_of_the_bundestag <- NULL 
MPs_meta_info$member_of_the_german_bundestag <- NULL 
MPs_meta_info$president <- NULL 
MPs_meta_info$president_by_age <- NULL 
MPs_meta_info$president_of_germany <- NULL 
MPs_meta_info$president_of_the_bundestag.1 <- NULL 
MPs_meta_info$member_of_parliament <- NULL 

colnames(MPs_meta_info)

?merge
merged_df <- base::merge(NamesWPSpeeches[ , c("year", "month", "wikidataid","name", "novelty","transience","resonance","politicianID", "pageid")], 
                   MPs_meta_info, by=c("wikidataid"), all.x = TRUE)
#merged_df_2 <- merge(merged_df, MPs_and_parties, by=c("wikidataid"), all.x = TRUE)
length(unique(merged_df$wikidataid)) #2184 unique politicians

merged_df <- merged_df %>% distinct()
colnames(merged_df)

merged_df$name <- ifelse(is.na(merged_df$name.x), merged_df$name.y, merged_df$name.x)
merged_df$name.x <- NULL
merged_df$name.y <- NULL


# Variable for electoral term
merged_df$year <- as.numeric(merged_df$year)
merged_df$WP <- "NA"
merged_df$WP[(merged_df$year == 1990 & merged_df$month >= 12) | (merged_df$year > 1990 & merged_df$year < 1994) | (merged_df$year == 1994 & merged_df$month < 11)] <- 12
merged_df$WP[(merged_df$year == 1994 & merged_df$month >= 11) | (merged_df$year > 1994 & merged_df$year < 1998) | (merged_df$year == 1998 & merged_df$month < 11)]  <- 13
merged_df$WP[(merged_df$year == 1998 & merged_df$month >= 11) | (merged_df$year > 1998 & merged_df$year < 2002) | (merged_df$year == 2002 & merged_df$month < 11)]  <- 14
merged_df$WP[(merged_df$year == 2002 & merged_df$month >= 11) | (merged_df$year > 2002 & merged_df$year < 2005) | (merged_df$year == 2005 & merged_df$month < 11)]  <- 15
merged_df$WP[(merged_df$year == 2005 & merged_df$month >= 11) | (merged_df$year > 2005 & merged_df$year < 2009) | (merged_df$year == 2009 & merged_df$month < 11)]  <- 16
merged_df$WP[(merged_df$year == 2009 & merged_df$month >= 11) | (merged_df$year > 2009 & merged_df$year < 2013) | (merged_df$year == 2013 & merged_df$month < 11)]  <- 17
merged_df$WP[(merged_df$year == 2013 & merged_df$month >= 11) | (merged_df$year > 2013 & merged_df$year < 2017) | (merged_df$year == 2017 & merged_df$month < 11)]  <- 18
merged_df$WP[(merged_df$year == 2017 & merged_df$month >= 11) | (merged_df$year > 2017 & merged_df$year < 2021) | (merged_df$year == 2021 & merged_df$month < 11)]  <- 19


merged_df <- merged_df %>% distinct()


nrow(merged_df[!is.na(merged_df$party),])
nrow(merged_df[is.na(merged_df$party),]) # none without party
nrow(merged_df[is.na(merged_df$wikidataid),]) # none without party

colnames(merged_df)
merged_df$session <- NULL
merged_df$pageid.x <- NULL


nrow(merged_df[!is.na(merged_df$ROLE),])
nrow(merged_df[is.na(merged_df$ROLE),]) # 4522 without ROLE

colnames(merged_df)

#Year of birth variable to calculate AGE later
get_year_of_birth <- function(birth_str) {
  year <- as.numeric(str_split(birth_str, "-")[[1]][1])
  return(year)
}

merged_df$YEAR_OF_BIRTH <- sapply(merged_df$birth, get_year_of_birth)

#AGE variable 
merged_df$AGE <- "NA"
merged_df$AGE <- as.numeric(merged_df$year) - merged_df$YEAR_OF_BIRTH

merged_df$YEAR_OF_BIRTH <- NULL

#CONSTITUENCY variable
merged_df$PART_OF_GER <- "NA"
merged_df$PART_OF_GER[merged_df$constituency2 == "Nordrhein-Westfalen" ] <- "Alte Bundesländer"
merged_df$PART_OF_GER[merged_df$constituency2 == "Mecklenburg-Vorpommern"] <- "Neue Bundesländer"
merged_df$PART_OF_GER[merged_df$constituency2 == "Saarland"] <- "Alte Bundesländer"
merged_df$PART_OF_GER[merged_df$constituency2 == "Sachsen"] <- "Neue Bundesländer"
merged_df$PART_OF_GER[merged_df$constituency2 == "Bayern"] <- "Alte Bundesländer"
merged_df$PART_OF_GER[merged_df$constituency2 == "Brandenburg"] <- "Neue Bundesländer"
merged_df$PART_OF_GER[merged_df$constituency2 == "Schleswig-Holstein"] <- "Alte Bundesländer"
merged_df$PART_OF_GER[merged_df$constituency2 == "Berlin"] <- "Neue Bundesländer"
merged_df$PART_OF_GER[merged_df$constituency2 == "Hessen"] <- "Alte Bundesländer"
merged_df$PART_OF_GER[merged_df$constituency2 == "Thüringen"] <- "Neue Bundesländer"
merged_df$PART_OF_GER[merged_df$constituency2 == "Sachsen-Anhalt"] <- "Neue Bundesländer"
merged_df$PART_OF_GER[merged_df$constituency2 == "Hamburg"] <- "Alte Bundesländer"
merged_df$PART_OF_GER[merged_df$constituency2 == "Rheinland-Pfalz"] <- "Alte Bundesländer"
merged_df$PART_OF_GER[merged_df$constituency2 == "Baden-Württemberg"] <- "Alte Bundesländer"
merged_df$PART_OF_GER[merged_df$constituency2 == "Niedersachsen"] <- "Alte Bundesländer"
merged_df$PART_OF_GER[merged_df$constituency2 == "Bremen"] <-"Alte Bundesländer"
merged_df$PART_OF_GER <- as.factor(merged_df$PART_OF_GER)

merged_df$constituency2 <- NULL

merged_df$politicianID <- NULL 


# Ethnicity variable
merged_df$ETHN <- "not indicated"
merged_df$ETHN[merged_df$ethnicity == "white"] <- "white"
merged_df$ETHN[merged_df$ethnicity == "asian"] <- "asian"
merged_df$ETHN[merged_df$ethnicity == "black"] <- "black"
merged_df$ETHN <- as.factor(merged_df$ETHN)

merged_df$ethnicity <- NULL

colnames(merged_df)



unique(merged_df$ETHN) 
freq(merged_df$ETHN) # 32% NANs

unique(merged_df$sex)
merged_df$sex <- ifelse(is.na(merged_df$sex), "not indicated", merged_df$sex)
freq(merged_df$sex) # 2% NANs

unique(merged_df$religion)
freq(merged_df$religion) # 35% NANs
merged_df$religion <- ifelse(is.na(merged_df$religion), "not indicated", merged_df$religion)
freq(merged_df$religion) # 

inv <- subset(merged_df, merged_df$party == "PDS")
unique(inv$wikidataid)


unique(merged_df$party)
freq(merged_df$party) # 0% NANs
merged_df$party <- ifelse(merged_df$party == "CDU", "CDU/CSU", merged_df$party)
merged_df$party <- ifelse(merged_df$party == "CSU", "CDU/CSU", merged_df$party)
merged_df$party <- ifelse((merged_df$party == "PDS" & merged_df$year >= 2007), "DIE LINKE", merged_df$party)
freq(merged_df$party) 

inv <- subset(merged_df, is.na(merged_df$party) == TRUE)
inv <- subset(merged_df, merged_df$party == "none")
unique(inv$wikidataid)

merged_df$party[merged_df$wikidataid == "Q108771"] <- "SPD"
merged_df$party[(merged_df$wikidataid == "Q7477615") & (merged_df$year <= 2017)] <- "AfD"


unique(merged_df$year)
freq(merged_df$year) # 0% NANs

unique(merged_df$ROLE)
freq(merged_df$ROLE) # 0% NANs



# prepare data
length(unique(merged_df$wikidataid))

merged_df_means <- merged_df %>%
  filter(year != 1990 & WP != "NA" & is.na(WP) != TRUE, is.na(AGE) != TRUE) %>%
  subset(select = c("AGE", "WP", "wikidataid")) %>%
  distinct() %>%
  group_by(WP) %>%
  summarise(age.mean = mean(AGE), age.sd = sd(AGE), WP = WP) %>%
  distinct()

colnames(merged_df)


DATA01 <-subset(merged_df, is.na(merged_df$ETHN) != TRUE)  # ethnicity
length(unique(DATA01$wikidataid))
DATA02 <-subset(DATA01, DATA01$sex=="female" | DATA01$sex=="male") # sex
length(unique(DATA02$wikidataid))
DATA03 <-subset(DATA02, is.na(DATA02$religion) != TRUE) # religion
length(unique(DATA03$wikidataid))
DATA04 <-subset(DATA03, DATA03$party != "none") # party
length(unique(DATA04$wikidataid))

DATA05 <-subset(DATA04, is.na(DATA04$novelty) != TRUE) # novelty
length(unique(DATA05$wikidataid))
DATA06 <-subset(DATA05, is.na(DATA05$resonance) != TRUE) # resonance
length(unique(DATA06$wikidataid))

rm(DATA02)
rm(DATA03)
rm(DATA04)
rm(DATA05)

colnames(merged_df)

DATA_FIN <- DATA06

DATA_FIN$year <- as.factor(DATA_FIN$year)
DATA_FIN$year_cont <- as.numeric(DATA_FIN$year)
DATA_FIN$religion <- ifelse(DATA_FIN$religion == "protestantism lutheran", "protestantism", DATA_FIN$religion)
DATA_FIN$religion <- ifelse(DATA_FIN$religion == "protestantism evangelical", "protestantism", DATA_FIN$religion)
DATA_FIN$religion <- as.factor(DATA_FIN$religion)


## AGE variable 
colnames(DATA_FIN)
hist(DATA_FIN$AGE)
descr(DATA_FIN$AGE)

DATA_FIN$AGE_CAT <- "NA"
# DATA_FIN$AGE_CAT[DATA_FIN$AGE >= 19 & DATA_FIN$AGE < 36] <- "19-35"
# DATA_FIN$AGE_CAT[DATA_FIN$AGE >= 36 & DATA_FIN$AGE < 46] <- "36-45"
# DATA_FIN$AGE_CAT[DATA_FIN$AGE >= 46 & DATA_FIN$AGE < 56] <- "46-55"
# DATA_FIN$AGE_CAT[DATA_FIN$AGE >= 56 & DATA_FIN$AGE < 66] <- "56-65"
# DATA_FIN$AGE_CAT[DATA_FIN$AGE >= 66 & DATA_FIN$AGE < 86] <- "66-85"

DATA_FIN$AGE_CAT[DATA_FIN$AGE < 31] <- "younger 31"
DATA_FIN$AGE_CAT[DATA_FIN$AGE >= 31 & DATA_FIN$AGE < 41] <- "31-40"
DATA_FIN$AGE_CAT[DATA_FIN$AGE >= 41 & DATA_FIN$AGE < 51] <- "41-50"
DATA_FIN$AGE_CAT[DATA_FIN$AGE >= 51 & DATA_FIN$AGE < 61] <- "51-60"
DATA_FIN$AGE_CAT[DATA_FIN$AGE >= 61] <- "older 60"

# DATA_FIN$AGE_CAT[DATA_FIN$AGE < 36] <- "younger 36"
# DATA_FIN$AGE_CAT[DATA_FIN$AGE >= 36 & DATA_FIN$AGE < 46] <- "36-45"
# DATA_FIN$AGE_CAT[DATA_FIN$AGE >= 46 & DATA_FIN$AGE < 56] <- "46-55"
# DATA_FIN$AGE_CAT[DATA_FIN$AGE >= 56 & DATA_FIN$AGE < 66] <- "56-65"
# DATA_FIN$AGE_CAT[DATA_FIN$AGE >= 66] <- "older 65"

DATA_FIN$AGE_CAT <- as.factor(DATA_FIN$AGE_CAT)

DATA_FIN_younger <- DATA_FIN
DATA_FIN_older <- DATA_FIN

freq(DATA_FIN_younger$AGE_CAT)
freq(DATA_FIN_older$AGE_CAT)

length(unique(DATA_FIN$year))

colnames(DATA_FIN)

#save(merged_df, file = "./01_data/Wikipedia/output/merged_df.RData")
#save(DATA_FIN, file = "./01_data/Wikipedia/output/changes_in_div_over_time.RData")

### Plot comparisons 

# novelty and resonance over time
library(dplyr)
colnames(DATA_FIN)
??group_by

DATA_FIN$YEAR <- as.numeric(DATA_FIN$year)

DATA_FIN_time <- DATA_FIN %>%
  #filter(year != "1990")  %>%
  dplyr::group_by(year) %>%
  dplyr::summarize( mean.resonance = mean(resonance), mean.novelty = mean(novelty), median.resonance = median(resonance), median.novelty = median(novelty))

plot(DATA_FIN_time$year, DATA_FIN_time$mean.resonance)
plot(DATA_FIN_time$year, DATA_FIN_time$mean.novelty)

plot(DATA_FIN_time$year, DATA_FIN_time$median.resonance)
plot(DATA_FIN_time$year, DATA_FIN_time$median.novelty)

DATA_FIN <- merge(DATA_FIN,
                  DATA_FIN_time, by=c("year"), all.x = TRUE)

DATA_FIN$novelty_residual <- DATA_FIN$novelty - DATA_FIN$mean.novelty 
DATA_FIN$resonance_residual <- DATA_FIN$resonance - DATA_FIN$mean.resonance 

#classify Resonance
descr(DATA_FIN$resonance_residual)
quantile(DATA_FIN$resonance_residual, probs = seq(.1, .9, by = .1))
resonance_res_median <- median(DATA_FIN$resonance_residual)
resonance_res_mean <- median(DATA_FIN$resonance_residual)

DATA_FIN$resonance_quart <- "NA"
DATA_FIN$resonance_quart[DATA_FIN$resonance >= 0.141855] <- "4th Quantile"
DATA_FIN$resonance_quart[(DATA_FIN$resonance >= resonance_res_median) & (DATA_FIN$resonance < 0.141855)] <- "3rd Quantile"
DATA_FIN$resonance_quart[(DATA_FIN$resonance >= -0.143428) & (DATA_FIN$resonance < resonance_res_median)] <- "2nd Quantile"
DATA_FIN$resonance_quart[DATA_FIN$resonance < -0.143428] <- "1rst Quantile"

DATA_FIN$resonance_perc <- "NA"
DATA_FIN$resonance_perc[DATA_FIN$resonance >= 0.177983079] <- "top 20%"
DATA_FIN$resonance_perc[DATA_FIN$resonance >= 0.278783775 ] <- "top 10%"

freq(DATA_FIN$resonance_perc)

## upper quantile
upper_quant_resonance <- DATA_FIN %>%
  filter((resonance_quart == "4th Quantile") & (year != 1990))

## upper 20%
upper_perc_resonance <- DATA_FIN %>%
  filter((resonance_perc != "NA") & (year != 1990))

## ranked
DATA_FIN$resonance_rank <- length(DATA_FIN$resonance_residual)+1- rank(DATA_FIN$resonance_residual, ties.method ="max")
upper_50_resonance <- DATA_FIN %>%
  filter((resonance_rank <= 50) & (year != 1990))
length(unique(upper_50_resonance$wikidataid))

### grouped by year
DATA_FIN_gyear_ranked <- DATA_FIN %>%
  filter(year != "1990")  %>%
  dplyr::group_by(year) %>%
  dplyr::summarize(resonance_rank = length(resonance_residual)+1-rank(resonance_residual, ties.method ="max"), 
            wikidataid = wikidataid, 
            resonance_residual = resonance_residual, 
            ROLE = ROLE)

top50speaker_resonance_by_speeches <- DATA_FIN_gyear_ranked %>%
  #filter(year != "1990")  %>%
  filter(year != "1990", ROLE == "Member of Parliament")  %>%
  dplyr::group_by(year) %>%
  dplyr::select(year, wikidataid, resonance_rank) %>%
  dplyr::distinct(year, wikidataid, .keep_all= TRUE) %>%
  dplyr::top_n(-50, resonance_rank)  

length(unique(top50speaker_resonance_by_speeches$wikidataid[top50speaker_resonance_by_speeches$year == 1993]))
length(top50speaker_resonance_by_speeches$wikidataid[top50speaker_resonance_by_speeches$year == 1993])

top50speaker_resonance_by_speeches_meta <- merge(top50speaker_resonance_by_speeches, 
                                                 DATA_FIN[, c("year", "wikidataid","name","sex","ETHN","religion","AGE","PART_OF_GER","party",
                                                              "ROLE")], by=c("wikidataid", "year"), all.x = TRUE) %>%
  dplyr::distinct()

length(unique(top50speaker_resonance_by_speeches_meta$wikidataid[top50speaker_resonance_by_speeches_meta$year == 1993]))
length(top50speaker_resonance_by_speeches_meta$wikidataid[top50speaker_resonance_by_speeches_meta$year == 1993])

### grouped by year and ID
DATA_FIN_gyid_ranked <- DATA_FIN %>%
  dplyr::filter(year != "1990")  %>%
  dplyr::group_by(year, wikidataid) %>%
  dplyr::summarize(resonance_residual_mean = mean(resonance_residual)) %>%
  dplyr::summarize(resonance_rank = (length(resonance_residual_mean)+1-base::rank(resonance_residual_mean, ties.method ="max")), 
            wikidataid = wikidataid, 
            resonance_residual_mean = resonance_residual_mean, 
            year = year)

length(unique(DATA_FIN_gyid_ranked$wikidataid[DATA_FIN_gyid_ranked$year == 1993]))
length(DATA_FIN_gyid_ranked$wikidataid[DATA_FIN_gyid_ranked$year == 1993])

DATA_FIN_gyid_ranked_meta<- merge(DATA_FIN_gyid_ranked, 
                                            DATA_FIN[, c("year", "wikidataid", "ROLE")], by=c("wikidataid", "year"), all.x = TRUE) %>%
  dplyr::distinct()


top50speaker_by_mean_resonance <- DATA_FIN_gyid_ranked_meta %>%
  #filter(year != "1990")  %>%
  filter(year != "1990", ROLE == "Member of Parliament")  %>%
  dplyr::group_by(year) %>%
  dplyr::select(year, wikidataid, resonance_rank) %>%
  dplyr::distinct(year, wikidataid, .keep_all= TRUE) %>%
  dplyr::top_n(-50, resonance_rank) 

length(unique(top50speaker_by_mean_resonance$wikidataid[top50speaker_by_mean_resonance$year == 1993]))
length(top50speaker_by_mean_resonance$wikidataid[top50speaker_by_mean_resonance$year == 1993])

A = unique(top50speaker_by_mean_resonance$wikidataid[top50speaker_by_mean_resonance$year == 1993])
B = unique(top50speaker_resonance_by_speeches$wikidataid[top50speaker_resonance_by_speeches$year == 1993])
length(A)
length(intersect(A,B)) ## 15 detected by method 1 were also detected by second approach. 

## add meta data to selection 
top50speaker_by_mean_resonance_meta<- merge(top50speaker_by_mean_resonance, 
                                      DATA_FIN[, c("year", "wikidataid","name","sex","ETHN","religion","AGE","PART_OF_GER","party",
                                                   "ROLE", "AGE_CAT")], by=c("wikidataid", "year"), all.x = TRUE) %>%
  dplyr::distinct()


DATA_FIN$is_in_top_50_by_mean_resonance <- ifelse((test$wikidataid %in% top50speaker_by_mean_resonance$wikidataid),"yes","no")
DATA_FIN$is_in_top_50_by_mean_novelty <- ifelse((test$wikidataid %in% top50speaker_by_mean_novelty$wikidataid),"yes","no")

length(unique(subset(DATA_FIN, DATA_FIN$is_in_top_50_by_mean_novelty == "yes", select = "wikidataid")$wikidataid))
length(unique(top50speaker_by_mean_novelty$wikidataid))

#test <- within(test,is_in_top_50_by_mean_resonance <- ifelse(wikidataid %in% top50speaker_by_mean_resonance$wikidataid,"yes","no"))

length(unique(top50speaker_by_mean_resonance_meta$wikidataid[top50speaker_by_mean_resonance_meta$year == 1993]))
length(top50speaker_by_mean_resonance_meta$wikidataid[top50speaker_by_mean_resonance_meta$year == 1993])

#classify Novelty
descr(DATA_FIN$novelty_residual)

novel_res_median <- median(DATA_FIN$novelty_residual)
novel_res_mean <- median(DATA_FIN$novelty_residual)

DATA_FIN$novelty_quart <- "NA"
DATA_FIN$novelty_quart[DATA_FIN$novelty_residual >= 0.23243] <- "4th Quantile"
DATA_FIN$novelty_quart[(DATA_FIN$novelty_residual >= novel_res_median) & (DATA_FIN$novelty_residual < 0.23243)] <- "3rd Quantile"
DATA_FIN$novelty_quart[(DATA_FIN$novelty_residual >= -0.27759) & (DATA_FIN$novelty_residual < novel_res_median)] <- "2nd Quantile"
DATA_FIN$novelty_quart[DATA_FIN$novelty_residual < -0.27759] <- "1rst Quantile"

freq(DATA_FIN$novelty_quart)

upper_quant_novelty <- DATA_FIN %>%
  dplyr::filter((novelty_quart == "4th Quantile") & (year != 1990))   #%>%
#group_by(year) %>%
#mutate(percent = value/sum(value))
#mutate(percent = prop.table(ETHN))

### grouped by year
DATA_FIN_gyear_ranked_nov <- DATA_FIN %>%
  dplyr::filter(year != "1990")  %>%
  dplyr::group_by(year) %>%
  dplyr::summarize(novelty_rank = length(novelty_residual)+1-rank(novelty_residual, ties.method ="max"), 
            wikidataid = wikidataid, 
            novelty_residual = novelty_residual, 
            ROLE = ROLE)

top50speaker_novelty_by_speeches <- DATA_FIN_gyear_ranked_nov %>%
  #filter(year != "1990")  %>%
  filter(year != "1990", ROLE == "Member of Parliament")  %>%
  dplyr::group_by(year) %>%
  dplyr::select(year, wikidataid, novelty_rank) %>%
  dplyr::distinct(year, wikidataid, .keep_all= TRUE) %>%
  dplyr::top_n(-50, novelty_rank)  

length(unique(top50speaker_novelty_by_speeches$wikidataid[top50speaker_novelty_by_speeches$year == 1993]))
length(top50speaker_novelty_by_speeches$wikidataid[top50speaker_novelty_by_speeches$year == 1993])

top50speaker_novelty_by_speeches_meta <- merge(top50speaker_novelty_by_speeches, 
                                                 DATA_FIN[, c("year", "wikidataid","name","sex","ETHN","religion","AGE","PART_OF_GER","party",
                                                              "ROLE")], by=c("wikidataid", "year"), all.x = TRUE) %>%
  dplyr::distinct()


### grouped by year and ID
DATA_FIN_gyid_ranked_nov <- DATA_FIN %>%
  #dplyr::filter(year != "1990")  %>%
  filter(year != "1990")  %>%
  dplyr::group_by(year, wikidataid) %>%
  dplyr::summarize(novelty_residual_mean = mean(novelty_residual)) %>%
  dplyr::summarize(novelty_rank = length(novelty_residual_mean)+1-rank(novelty_residual_mean, ties.method ="max"), 
            wikidataid = wikidataid, 
            novelty_residual_mean = novelty_residual_mean)

length(unique(DATA_FIN_gyid_ranked_nov$wikidataid[DATA_FIN_gyid_ranked_nov$year == 1993]))
length(DATA_FIN_gyid_ranked_nov$wikidataid[DATA_FIN_gyid_ranked_nov$year == 1993])

DATA_FIN_gyid_ranked_nov_meta <- merge(DATA_FIN_gyid_ranked_nov, 
                                               DATA_FIN[, c("year", "wikidataid", "ROLE")], by=c("wikidataid", "year"), all.x = TRUE) %>%
  dplyr::distinct()

top50speaker_by_mean_novelty <- DATA_FIN_gyid_ranked_nov_meta %>%
  filter(year != "1990", ROLE == "Member of Parliament")  %>%
  dplyr::group_by(year) %>%
  dplyr::select(year, wikidataid, novelty_rank) %>%
  dplyr::distinct(year, wikidataid, .keep_all= TRUE) %>%
  dplyr::top_n(-50, novelty_rank) 

length(unique(top50speaker_by_mean_novelty$wikidataid[top50speaker_by_mean_novelty$year == 1993]))
length(top50speaker_by_mean_novelty$wikidataid[top50speaker_by_mean_novelty$year == 1993])


## add meta data to selection 
top50speaker_by_mean_novelty_meta<- merge(top50speaker_by_mean_novelty, 
                                            DATA_FIN[, c("year", "wikidataid","name","sex","ETHN","religion","AGE","PART_OF_GER","party",
                                                         "ROLE", "AGE_CAT")], by=c("wikidataid", "year"), all.x = TRUE) %>%
  dplyr::distinct()




### SUMMARY TABLE

table1::table1(~AGE + sex  + ROLE + ETHN + religion + PART_OF_GER + party | year, data = upper_quant_novelty)

### dataset overall trends

OVERALL_TREND <- subset(DATA_FIN, select = c("year","wikidataid","sex","religion","party",
                                             "ROLE","name","AGE","PART_OF_GER","ETHN", "AGE_CAT")) %>%
  distinct() #WP excluded


colnames(DATA_FIN)

## ratio novelty resonance over time
DATA_FIN_nov_res_ratio <- DATA_FIN %>%
  #filter(year != "1990", ROLE == "Member of Parliament")  %>%
  dplyr::filter(year != "1990", ROLE == "Member of Parliament")  %>%
  dplyr::group_by(year, sex) %>%
  dplyr::summarize(novelty_residual_mean = mean(novelty_residual), resonance_residual_mean = mean(resonance_residual)) %>%
  dplyr::summarize(ratio_nov_res = resonance_residual_mean/novelty_residual_mean,
            sex = sex, 
            novelty_residual_mean = novelty_residual_mean, 
            resonance_residual_mean = resonance_residual_mean)

unique(DATA_FIN$ROLE)

ggplot() + geom_point(aes(y=ratio_nov_res, x=year, colour = sex), data=DATA_FIN_nov_res_ratio)



## rank ratio novelty resonance over time

### grouped by year and ID
DATA_FIN_ratio_ranked_nov_res <- DATA_FIN %>%
  #dplyr::filter(year != "1990")  %>%
  filter(year != "1990", ROLE == "Member of Parliament")  %>%
  dplyr::group_by(year, wikidataid) %>%
  dplyr::summarize(novelty_residual_mean = mean(novelty_residual), resonance_residual_mean = mean(resonance_residual)) %>%
  dplyr::summarize(novelty_rank = rank(novelty_residual_mean, ties.method ="max"), 
            resonance_rank = rank(resonance_residual_mean, ties.method ="max"),
            wikidataid = wikidataid, 
            novelty_residual_mean = novelty_residual_mean, 
            resonance_residual_mean = resonance_residual_mean)
DATA_FIN_sex <- DATA_FIN[, c("sex", "ROLE", "wikidataid")] %>% distinct()
DATA_FIN_ratio_ranked_nov_res <- merge(DATA_FIN_ratio_ranked_nov_res, DATA_FIN_sex, by=c("wikidataid"), all.x = TRUE) %>%
  distinct()

hist(DATA_FIN_ratio_ranked_nov_res$novelty_residual_mean)
hist(DATA_FIN_ratio_ranked_nov_res$resonance_residual_mean)



ggplot(DATA_FIN_ratio_ranked_nov_res, aes(y=novelty_residual_mean, x=year, color = sex, alpha = 0.3)) + 
  geom_point() 

ggplot(DATA_FIN_ratio_ranked_nov_res, aes(x = resonance_residual_mean, fill = sex)) + 
  geom_histogram()

ggplot(DATA_FIN_ratio_ranked_nov_res, aes(y=novelty_residual_mean, x=resonance_residual_mean, colour = sex)) + 
  geom_point()



test <- DATA_FIN_ratio_ranked_nov_res %>%
  dplyr::filter(ROLE == "Member of Parliament") %>%
  dplyr::group_by(year, sex) %>%
  dplyr::summarize(mean_rank_res = mean(resonance_rank), mean_rank_nov = mean(novelty_rank)) %>%
  dplyr::summarise(mean_rank_ratio = mean_rank_res/mean_rank_nov, 
            sex = sex, 
            year = year)


unique(DATA_FIN$ROLE)

ggplot(test, aes(y=mean_rank_ratio, x=year, colour = sex)) + 
  #geom_errorbar() +
  geom_point() +
  geom_line()


gg <- ggplot(DATA_FIN_ratio_ranked_nov_res, aes(x=novelty_residual_mean, y=resonance_residual_mean, col=sex)) + 
  geom_point(size=1, alpha = 0.5) +  # Set color to vary based on state categories.
  geom_smooth(method="lm", size=1, se=TRUE, fullrange = T) + 
  ylim(-1, 1) +
  labs(title="TITLE", subtitle=" ", y="Resonance",
       x= TeX('Novelty $Nu$'))
plot(gg)




ggplot() + 
  geom_boxplot(aes(x=reorder(year, sort_order), y=novelty_residual_mean, data = DATA_FIN_ratio_ranked_nov_res))

ggplot(DATA_FIN_ratio_ranked_nov_res, aes(x=year, y=novelty_residual_mean, fill=sex)) + 
  geom_boxplot() +
  stat_summary(aes(group = sex), fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set1")


ggplot(DATA_FIN_ratio_ranked_nov_res, aes(x=year, y=resonance_residual_mean, fill=sex)) + 
  geom_boxplot() +
  stat_summary(aes(group = sex), fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set1")

DATA_FIN_ratio_ranked_nov_res$rank_ratio <- DATA_FIN_ratio_ranked_nov_res$resonance_rank/DATA_FIN_ratio_ranked_nov_res$novelty_rank


ggplot(DATA_FIN_ratio_ranked_nov_res, aes(x=year, y=rank_ratio, fill=sex)) + 
  geom_boxplot() +
  stat_summary(aes(group = sex), fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set1")



