
# ------------------------------------------------------------------------
# Script name: 02_merge_datasets_together.py
# Purpose of script: Statistical analysis of the effect of pol. significance on individual agenda setting capacity 
# Dependencies: -- 
# Author: Alexandra Rottenkolber
# ------------------------------------------------------------------------

# COMMENT: Combining the data was done in python
library(descr)
library(plm)
library(dplyr)
library(stargazer)
library(stringr)
library(lmtest) #for clustering standard errors
library(stargazer)


## load data ----------------------------------------------
setwd("./data_analysis")

NamesWPSpeeches <- read.csv("./01_data/Plenarprotokolle/processed/NamesWPSpeeches.csv")
NamesWPSpeeches_groupedWP <- read.csv("./01_data/Plenarprotokolle/processed/NamesWPSpeeches_grouped_by_IDNameWP_later.csv")
NamesWPSpeeches_groupedYear <- read.csv("./01_data/Plenarprotokolle/processed/NamesWPSpeeches_grouped_by_IDNameYear_later.csv")
NamesWPSpeeches_grouped_month_year<- read.csv("./01_data/Plenarprotokolle/processed/NamesWPSpeeches_grouped_by_Month_Year_later.csv")

load(file = "./01_data/Wikipedia/output/wikimeasures_df_fa_without_PageRankGlobal_with_timedim.RData")
load(file = "./01_data/Wikipedia/output/MP2Party_CLD.RData")

PolID2Info_df <- read.csv("./01_data/Plenarprotokolle/processed/PolIDtoInfo_df_uniqueIDs.csv") #this is maybe needed

#----------------------------------------
NamesWPSpeeches_groupedYear <- NamesWPSpeeches_groupedYear[order(NamesWPSpeeches_groupedYear$wikidataid,NamesWPSpeeches_groupedYear$year),]
wikimeasures_df <- wikimeasures_df[order(wikimeasures_df$wikidataid),]
PolID2Info_df <- PolID2Info_df[order(PolID2Info_df$politicianID),]

NamesWPSpeeches_groupedYear_later <- subset(NamesWPSpeeches_groupedYear, NamesWPSpeeches_groupedYear$year >= 2015)

# add both datasets together
merged_df <- merge(NamesWPSpeeches_groupedYear_later[ , c("year", "wikidataid","name", "novelty","transience","resonance","politicianID", "pageid")], 
                   wikimeasures_df[ , c("wikidataid",
                                        "YEAR",
                                        "name",
                                        "wikiurl_basename_clean",
                                          "sex", 
                                           "ethnicity", 
                                           "religion", 
                                           "wikidataid",
                                           "fa_importance",
                                           "fa_importance_rank",
                                           "fa_influence",
                                           "fa_prominence",
                                           "fa_influence_rank",
                                           "fa_prominence_rank",
                                           "fa_1",
                                           "fa_2",
                                           "fa_3",
                                           "fa_1_rank",
                                           "fa_2_rank",
                                           "fa_3_rank",
                                           "fa_importance_fa",
                                           "fa_importance_rank_fa",
                                           "fa_influence_fa",
                                           "fa_prominence_fa",
                                           "fa_influence_rank_fa",
                                           "fa_prominence_rank_fa",
                                           "fa_1_fa",
                                           "fa_2_fa",
                                           "fa_3_fa",
                                           "fa_1_rank_fa",
                                           "fa_2_rank_fa",
                                           "fa_3_rank_fa")] , by.x=c("wikidataid", "year"), by.y=c("wikidataid", "YEAR"), all.x = TRUE, all.y = TRUE)

merged_df$name <- ifelse(is.na(merged_df$name.x), merged_df$name.y, merged_df$name.x)
merged_df$name.x <- NULL
merged_df$name.y <- NULL


# Variable for electoral term
merged_df$session <- "NA"
merged_df$session[merged_df$year == 2015] <- 18
merged_df$session[merged_df$year == 2016] <- 18
merged_df$session[merged_df$year == 2017] <- 19
merged_df$session[merged_df$year == 2018] <- 19
merged_df$session[merged_df$year == 2019] <- 19
merged_df$session[merged_df$year == 2020] <- 19
merged_df$session[merged_df$year == 2021] <- 19
nrow(merged_df[is.na(merged_df$session),])


merged_df <- merged_df[order(merged_df$wikidataid),]
length(unique(merged_df$wikidataid)) #2184 unique politicians
merged_df_2 <- merge(merged_df, MPs_and_parties[, c("wikidataid", "party", "birth", "constituency2", "occupation")], by=c("wikidataid"), all.x = TRUE)
length(unique(merged_df_2$wikidataid)) #2184 unique politicians
merged_df <- merged_df_2 %>% distinct()

MPs_and_parties$pageid <- NULL 
MPs_and_parties$service <- NULL
MPs_and_parties$session <- as.character(MPs_and_parties$session)

#ROLE variable 
MPs_and_parties$ROLE <- "Member of Parliament"
MPs_and_parties$ROLE[MPs_and_parties$bundesminister == TRUE] <- "Minister"
MPs_and_parties$ROLE[MPs_and_parties$secretary_of_state == TRUE] <- "Secretary of state"
MPs_and_parties$ROLE[MPs_and_parties$federal_chancellor_of_germany == TRUE] <- "Chancellor"
MPs_and_parties$ROLE[MPs_and_parties$president_of_the_bundestag == TRUE] <- "President of Bundestag"

merged_df3 <- merged_df %>% left_join(MPs_and_parties[, c("wikidataid", "party", "session", "ROLE")], by = c("wikidataid", "party", "session"))
merged_df3$ROLE <- ifelse(is.na(merged_df3$ROLE), "Member of Parliament", merged_df3$ROLE)
merged_df <- merged_df3 %>% distinct()

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

merged_df$party <- ifelse(merged_df$party == "CDU", "CDU/CSU", merged_df$party)
merged_df$party <- ifelse(merged_df$party == "CSU", "CDU/CSU", merged_df$party)
merged_df$party <- ifelse(merged_df$party == "PDS", "DIE LINKE", merged_df$party)

merged_df$party[merged_df$wikidataid == "Q108771"] <- "SPD"
merged_df$party[(merged_df$wikidataid == "Q7477615") & (merged_df$year <= 2017)] <- "AfD"

# Ethnicity variable
merged_df$ETHN <- "not indicated"
merged_df$ETHN[merged_df$ethnicity == "white"] <- "white"
merged_df$ETHN[merged_df$ethnicity == "asian"] <- "asian"
merged_df$ETHN[merged_df$ethnicity == "black"] <- "black"
merged_df$ETHN <- as.factor(merged_df$ETHN)

#Summary Stats whole data
table1::label(merged_df$wikidataid) <- "ID"
table1::label(merged_df$year) <- "Year" 
table1::label(merged_df$novelty) <- "Novelty" 
table1::label(merged_df$transience) <- "Transience" 
table1::label(merged_df$resonance) <- "Resonance"
table1::label(merged_df$sex) <- "Gender"
table1::label(merged_df$AGE) <- "Age"
table1::label(merged_df$PART_OF_GER) <- "Region"
table1::label(merged_df$session) <- "Electoral term"
table1::label(merged_df$party) <- "Party"
table1::label(merged_df$ROLE) <- "Position held"
table1::label(merged_df$ETHN) <- "Ethnicity"
table1::label(merged_df$religion) <- "Religion"
table1::label(merged_df$fa_influence) <- "Influence"
table1::label(merged_df$fa_prominence) <- "Prominence"
table1::label(merged_df$occupation) <- "Occupation"

table1::table1(~fa_prominence + fa_influence + novelty + resonance + party| party, data = merged_df)
table1::table1(~AGE + sex  + ROLE + ETHN + religion + PART_OF_GER + party + occupation | party, data = merged_df)
table1::table1(~AGE + sex  + ROLE + ETHN + religion + PART_OF_GER + party + year| party, data = merged_df)

# prepare data
length(unique(merged_df$wikidataid))
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

DATA_FIN <- DATA06

DATA_FIN$year <- as.factor(DATA_FIN$year)
DATA_FIN$religion <- ifelse(DATA_FIN$religion == "protestantism lutheran", "protestantism", DATA_FIN$religion)
DATA_FIN$religion <- ifelse(DATA_FIN$religion == "protestantism evangelical", "protestantism", DATA_FIN$religion)
DATA_FIN$religion <- ifelse(DATA_FIN$religion == "orthodox eastern", "orthodox", DATA_FIN$religion)
DATA_FIN$religion <- as.factor(DATA_FIN$religion)

#Summary Stats
DATA_FIN$religion <- relevel(DATA_FIN$religion, ref = "not indicated")
DATA_FIN$ETHN <- relevel(DATA_FIN$ETHN, ref = "not indicated")

table1::label(DATA_FIN$wikidataid) <- "ID"
table1::label(DATA_FIN$year) <- "Year" 
table1::label(DATA_FIN$novelty) <- "Novelty" 
table1::label(DATA_FIN$transience) <- "Transience" 
table1::label(DATA_FIN$resonance) <- "Resonance"
table1::label(DATA_FIN$sex) <- "Gender"
table1::label(DATA_FIN$AGE) <- "Age"
table1::label(DATA_FIN$PART_OF_GER) <- "Region"
table1::label(DATA_FIN$session) <- "Electoral term"
table1::label(DATA_FIN$party) <- "Party"
table1::label(DATA_FIN$ROLE) <- "Position held"
table1::label(DATA_FIN$ETHN) <- "Ethnicity"
table1::label(DATA_FIN$religion) <- "Religion"
table1::label(DATA_FIN$fa_influence_fa) <- "Influence"
table1::label(DATA_FIN$fa_prominence_fa) <- "Prominence"
table1::label(DATA_FIN$occupation) <- "Occupation"

table1::table1(~fa_prominence_fa + fa_influence_fa + novelty + resonance + party| party, data = DATA_FIN)
table1::table1(~fa_prominence_fa + fa_influence_fa + novelty + resonance | party, data = DATA_FIN) #this one 
table1::table1(~AGE + sex  + ROLE + ETHN + religion + PART_OF_GER + party| party, data = DATA_FIN)
table1::table1(~AGE + sex  + ROLE + ETHN + religion + PART_OF_GER +occupation + party +year| party, data = DATA_FIN)
table1::table1(~AGE + sex  + ROLE + ETHN + religion + PART_OF_GER + party +year| party, data = DATA_FIN) # this one


DATA_FIN$religion <- relevel(DATA_FIN$religion, ref = "catholicism")
DATA_FIN$ETHN <- relevel(DATA_FIN$ETHN, ref = "white")

## correlations
colnames(DATA_FIN)
corr_data <- subset(DATA_FIN, select = c("novelty", "resonance", 
                                        # "fa_importance", "fa_prominence", "fa_influence",
                                        # "fa_importance_fa", 
                                        "fa_prominence_fa", "fa_influence_fa")
                    )
corr_plot <- corrplot::corrplot(cor(corr_data), 
                                order = "alphabet", 
                                method = "circle", 
                                type = "lower", 
                                tl.col='black', 
                                tl.cex=.75, 
                                p.mat = cor(corr_data), 
                                insig = "p-value", 
                                sig.level=-1, 
                                col=colorRampPalette(c("blue","white","darkgreen"))(20), 
                                tl.srt = 45, 
                                tl.offset = 0.5, 
                                addCoef.col = "black", 
                                cl.lim = c(-1,1)
                                )
# - resonance and influence, resonance and prominence are only slightly correlated. 
# - novelty and influence, novelty and prominence not at all.  


## Effect of novelty on resonance
#### FA ANALYSIS
DATA_FIN$religion <- relevel(DATA_FIN$religion, ref = "catholicism")
DATA_FIN$ETHN <- relevel(DATA_FIN$ETHN, ref = "white")
DATA_FIN$ROLE <- relevel(as.factor(DATA_FIN$ROLE), ref = "Member of Parliament")
DATA_FIN$party <- relevel(as.factor(DATA_FIN$party), ref = "CDU/CSU")

# for resonance
OLS0<-lm(resonance ~ fa_influence_fa+sex+ETHN+religion+party+ROLE+year, data=DATA_FIN)
OLS1<-lm(resonance ~ fa_prominence_fa+sex+ETHN+religion+party+ROLE+year, data=DATA_FIN)

OLS0<-lm(resonance ~ fa_influence_fa+AGE+sex+ROLE+ETHN+religion+PART_OF_GER+party+year, data=DATA_FIN)
OLS1<-lm(resonance ~ fa_prominence_fa+AGE+sex+ROLE+ETHN+religion+PART_OF_GER+party+year, data=DATA_FIN)

OLS0_withoutROLE<-lm(resonance ~ fa_influence_fa+AGE+sex+ETHN+religion+PART_OF_GER+party+year, data=DATA_FIN)
OLS1_withoutROLE<-lm(resonance ~ fa_prominence_fa+AGE+sex+ETHN+religion+PART_OF_GER+party+year, data=DATA_FIN)
stargazer(OLS0, OLS1,OLS0_withoutROLE, OLS1_withoutROLE,  type="text")

stargazer(OLS0, OLS1,OLS0_withoutROLE, OLS1_withoutROLE, type="latex", out="./03_figures/part_1/resonance_OLSmodels_plusminROle_named.html", 
          dep.var.labels=c("Resonance"), 
          covariate.labels=c("Influence",
                             "Prominence",
                             "Age", 
                             "Sex: Male",
                             "Role: Chancellor", 
                             "Role: Minister", 
                             "Role: President of Bundestag",
                             "Role: Secretary of state",
                             "Ethnicity: Asian",
                             "Ethnicity: Black",
                             "Ethnicity: Not indicated",
                             "Religion: not indicated",
                             "Religion: atheism",
                             "Religion: islam",
                             "Religion: orthodox",
                             "Religion: orthodox eastern",
                             "Religion: protestantism",
                             "Neue Bundesländer",
                             "AfD", 
                             "BÜNDNIS 90/DIE GRÜNEN", 
                             "DIE LINKE", 
                             "FDP", 
                             "SPD", 
                             "2016",
                             "2017",
                             "2018", 
                             "2019",
                             "2020",
                             "2021", 
                             "Constant"))

FE1<-plm(resonance ~ fa_influence_fa+ROLE+year, data=DATA_FIN, index=c("wikidataid","year"), model = "within")
FE2<-plm(resonance ~ fa_prominence_fa+ROLE+year, data=DATA_FIN, index=c("wikidataid","year"), model = "within")

FE1<-plm(resonance ~ fa_influence_fa+year, data=DATA_FIN, index=c("wikidataid","year"), model = "within")
FE2<-plm(resonance ~ fa_prominence_fa+year, data=DATA_FIN, index=c("wikidataid","year"), model = "within")

FE1_rev<-plm(fa_influence_fa  ~ resonance+ROLE+year, data=DATA_FIN, index=c("wikidataid","year"), model = "within")

stargazer(OLS0, 
          OLS1,
          FE1, 
          coeftest(FE1, vcov = vcovHC, type = "HC1"),
          FE2,  
          coeftest(FE2, vcov = vcovHC, type = "HC1"),
          type="text")

stargazer(OLS0, OLS1,FE1, FE2, type="latex", out="./03_figures/part_1/resonance_OLSmodels_plusminROle_named.html", 
          dep.var.labels=c("Resonance"), 
          covariate.labels=c("Influence",
                             "Prominence",
                             "Age", 
                             "Sex: Male",
                             "Role: Chancellor", 
                             "Role: Minister", 
                             "Role: President of Bundestag",
                             "Role: Secretary of state",
                             "Ethnicity: Asian",
                             "Ethnicity: Black",
                             "Ethnicity: Not indicated",
                             "Religion: not indicated",
                             "Religion: atheism",
                             "Religion: islam",
                             "Religion: orthodox",
                             "Religion: orthodox eastern",
                             "Religion: protestantism",
                             "Neue Bundesländer",
                             "AfD", 
                             "BÜNDNIS 90/DIE GRÜNEN", 
                             "DIE LINKE", 
                             "FDP", 
                             "SPD", 
                             "2016",
                             "2017",
                             "2018", 
                             "2019",
                             "2020",
                             "2021", 
                             "Constant"))


DATA_FIN_men <- subset(DATA_FIN, DATA_FIN$sex == "male") 
DATA_FIN_women <-  subset(DATA_FIN, DATA_FIN$sex == "female") 

FE1_M<-plm(resonance ~ fa_influence_fa+ROLE+year, data=DATA_FIN_men, index=c("wikidataid","year"), model = "within")
FE1_F<-plm(resonance ~ fa_influence_fa+ROLE+year, data=DATA_FIN_women, index=c("wikidataid","year"), model = "within")

FE1_M<-plm(resonance ~ fa_influence_fa+year, data=DATA_FIN_men, index=c("wikidataid","year"), model = "within")
FE1_F<-plm(resonance ~ fa_influence_fa+year, data=DATA_FIN_women, index=c("wikidataid","year"), model = "within")

stargazer(FE1,
          FE1_M,  
          FE1_F,
          coeftest(FE1, vcov = vcovHC, type = "HC1"),
          coeftest(FE1_M, vcov = vcovHC, type = "HC1"),
          coeftest(FE1_F, vcov = vcovHC, type = "HC1"),
          type="text")

stargazer( FE1, FE1_M, FE1_F, type="latex", #out="./03_figures/part_1/resonance_OLSFEmodels_named.html", 
          dep.var.labels=c("Resonance"), 
          covariate.labels=c("Influence",
                             "Prominence",
                             "Age", 
                             "Sex: Male",
                             "Role: Chancellor", 
                             "Role: Minister", 
                             "Role: President of Bundestag",
                             "Role: Secretary of state",
                             "Ethnicity: Asian",
                             "Ethnicity: Black",
                             "Ethnicity: Not indicated",
                             "Religion: not indicated",
                             "Religion: atheism",
                             "Religion: islam",
                             "Religion: orthodox",
                             "Religion: orthodox eastern",
                             "Religion: protestantism",
                             "Neue Bundesländer",
                             "AfD", 
                             "BÜNDNIS 90/DIE GRÜNEN", 
                             "DIE LINKE", 
                             "FDP", 
                             "SPD", 
                             "2016",
                             "2017",
                             "2018", 
                             "2019",
                             "2020",
                             "2021", 
                             "Constant"))


# for novelty
OLS0<-lm(novelty ~ fa_influence_fa+sex+ethnicity+religion+party+ROLE+year, data=DATA_FIN)
OLS1<-lm(novelty ~ fa_prominence_fa+sex+ethnicity+religion+party+ROLE+year, data=DATA_FIN)

OLS0<-lm(novelty ~ fa_influence_fa+AGE+sex+ROLE+ETHN+religion+PART_OF_GER+party+year, data=DATA_FIN)
OLS1<-lm(novelty ~ fa_prominence_fa+AGE+sex+ROLE+ETHN+religion+PART_OF_GER+party+year, data=DATA_FIN)

OLS0_withoutROLE<-lm(novelty ~ fa_influence_fa+AGE+sex+ETHN+religion+PART_OF_GER+party+year, data=DATA_FIN)
OLS1_withoutROLE<-lm(novelty ~ fa_prominence_fa+AGE+sex+ETHN+religion+PART_OF_GER+party+year, data=DATA_FIN)

stargazer(OLS0, OLS1,OLS0_withoutROLE, OLS1_withoutROLE, type="latex", out="./03_figures/part_1/novelty_OLSmodels_named.html", 
          dep.var.labels=c("Novelty"), 
          covariate.labels=c("Influence",
                             "Prominance",
                             "Age", 
                             "Sex: Male",
                             "Role: Chancellor", 
                             "Role: Minister", 
                             "Role: President of Bundestag",
                             "Role: Secretary of state",
                             "Ethnicity: Asian",
                             "Ethnicity: Black",
                             "Ethnicity: Not indicated",
                             "Religion: not indicated",
                             "Religion: atheism",
                             "Religion: islam",
                             "Religion: orthodox",
                             "Religion: orthodox eastern",
                             "Religion: protestantism",
                             "Neue Bundesländer",
                             "AfD", 
                             "BÜNDNIS 90/DIE GRÜNEN", 
                             "DIE LINKE", 
                             "FDP", 
                             "SPD", 
                             "2016",
                             "2017",
                             "2018", 
                             "2019",
                             "2020",
                             "2021", 
                             "Constant"))



FE1<-plm(novelty ~ fa_influence_fa+ROLE+year, data=DATA_FIN, index=c("wikidataid","year"), model = "within")
FE2<-plm(novelty ~ fa_prominence_fa+ROLE+year, data=DATA_FIN, index=c("wikidataid","year"), model = "within")

stargazer(OLS0, OLS1, FE1, FE2, type="latex", #out="./03_figures/part_1/novelty_OLSFEmodels_named.html", 
          dep.var.labels=c("Novelty"), 
          covariate.labels=c("Influence",
                             "Prominence",
                             "Age", 
                             "Sex: Male",
                             "Role: Chancellor", 
                             "Role: Minister", 
                             "Role: President of Bundestag",
                             "Role: Secretary of state",
                             "Ethnicity: Asian",
                             "Ethnicity: Black",
                             "Ethnicity: Not indicated",
                             "Religion: not indicated",
                             "Religion: atheism",
                             "Religion: islam",
                             "Religion: orthodox",
                             "Religion: orthodox eastern",
                             "Religion: protestantism",
                             "Neue Bundesländer",
                             "AfD", 
                             "BÜNDNIS 90/DIE GRÜNEN", 
                             "DIE LINKE", 
                             "FDP", 
                             "SPD", 
                             "2016",
                             "2017",
                             "2018", 
                             "2019",
                             "2020",
                             "2021", 
                             "Constant"))



DATA_FIN_men <- subset(DATA_FIN, DATA_FIN$sex == "male") 
DATA_FIN_women <-  subset(DATA_FIN, DATA_FIN$sex == "female") 

FE1_M<-plm(novelty ~ fa_influence_fa+ROLE +year, data=DATA_FIN_men, index=c("wikidataid","year"), model = "within")
FE1_F<-plm(novelty ~ fa_influence_fa+ROLE +year, data=DATA_FIN_women, index=c("wikidataid","year"), model = "within")

stargazer(OLS0, OLS1, FE1, FE1_M, FE1_F, type="latex", #out="./03_figures/part_1/novelty_OLSFEmodels_named.html", 
          dep.var.labels=c("Novelty"), 
          covariate.labels=c("Influence",
                             "Prominence",
                             "Age", 
                             "Sex: Male",
                             "Role: Chancellor", 
                             "Role: Minister", 
                             "Role: President of Bundestag",
                             "Role: Secretary of state",
                             "Ethnicity: Asian",
                             "Ethnicity: Black",
                             "Ethnicity: Not indicated",
                             "Religion: not indicated",
                             "Religion: atheism",
                             "Religion: islam",
                             "Religion: orthodox",
                             "Religion: orthodox eastern",
                             "Religion: protestantism",
                             "Neue Bundesländer",
                             "AfD", 
                             "BÜNDNIS 90/DIE GRÜNEN", 
                             "DIE LINKE", 
                             "FDP", 
                             "SPD", 
                             "2016",
                             "2017",
                             "2018", 
                             "2019",
                             "2020",
                             "2021", 
                             "Constant"))


colnames(DATA_FIN)

# plot density plot
g_per_year <- ggplot(DATA_FIN, aes(x=fa_influence_fa, y=resonance) ) +
  geom_hex(bins = 100) +
  #scale_fill_continuous(type = "viridis") +
  scale_fill_distiller(palette= "Spectral", direction=-1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  xlab("Influence") +
  ylab("Resonance") +
  #theme(legend.position='none') +
  #xlim(-3.5, 5) +
  #ylim(-2, 5) +
  #theme(legend.key.height= 0.1, legend.key.width= 0.1) +
  theme_light() +
  theme(axis.title.x = element_text(size=rel(2.5)),
        axis.title.y = element_text(size=rel(2.5)), 
        axis.text.x = element_text(size=rel(2.5)), 
        axis.text.y = element_text(size=rel(2.5)))+
  geom_smooth(data = subset(DATA_FIN_men, select = c("resonance", "fa_influence_fa")), method=lm) +
  geom_smooth(data = subset(DATA_FIN_women, select = c("resonance", "fa_influence_fa")), method=lm)
g_per_year


#### scores without time
colnames(DATA_FIN)

DATA_FIN_without_time <- DATA_FIN %>%
  group_by(wikidataid, year) %>%
  filter(year == 2020, party == "DIE LINKE") %>%
  summarise(wikidataid = wikidataid, 
            year = year, 
            party = party, 
            name = name,
            novelty_mean = mean(novelty),
            resonance_mean = mean(resonance), 
            fa_influence_fa_mean = mean(fa_influence_fa), 
            fa_prominence_fa_mean = mean(fa_prominence_fa) ) %>%
  distinct()

