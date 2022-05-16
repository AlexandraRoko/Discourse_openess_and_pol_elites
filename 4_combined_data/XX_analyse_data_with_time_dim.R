

getwd()
setwd("/Users/alexandrarottenkolber/Documents/02_Hertie_School/Master thesis/Master_Thesis_Hertie/data_analysis")
load(file = "./01_data/Wikipedia/output/wikimeasures_df_fa_without_PageRankGlobal_with_timedim.RData")

test_df <- subset(wikimeasures_df, wikimeasures_df$YEAR == 2020)











# OLS Regression
lm(DATA01$SATIS ~ DATA01$EMP)

# Least Square Dummy Variable Regression LSDV
lm(DATA01$SATIS ~ DATA01$EMP +  as.factor(DATA01$ID))

# Difference-in-Differences => Same as for the LDSV
DATA01<- DATA01 %>% arrange(ID, YEAR) %>% group_by(ID) %>% mutate(EMP_L = lead(EMP),
                                                                  SATIS_L   = lead(SATIS))
DATA01$SATIS_NEW <- (DATA01$SATIS_L  -DATA01$SATIS)
DATA01$EMP_NEW   <- (DATA01$EMP_L-DATA01$EMP)
lm(DATA01$SATIS_NEW ~ DATA01$EMP_NEW)

# Fixed effects for individual and year
DATA01$YEAR <-as.factor(DATA01$YEAR) 
plm(SATIS~EMP,data=DATA01, index=c("ID","YEAR"), model="within")

#Insert data
ID <-as.numeric(c(1,1,1,2,2,2))
YEAR <-as.numeric(c(1984,1985,1986,1984,1985,1986)) 
SATIS <-as.numeric(c(10,6,9,10,7,8))
EMP <-as.numeric(c(0,1,1,0,1,1))
DATA01 = data.frame(ID,YEAR,SATIS,EMP)

#FE
plm(SATIS ~ EMP, data=DATA01, model = "within", index=c("ID","YEAR"))


#FE with time trend
plm(SATIS ~ EMP + as.factor(YEAR), data=DATA01, model = "within", index=c("ID","YEAR"))



# regressions (ADD AGE Category if you want)
OLS1<-lm(EUROD ~ PARTNER+GENDER+AGE+COUNTRY+WAVE, data=DATA06)
OLS2<-lm(EUROD ~ PARTNER+AGE+GENDER+EDU+COUNTRY+WAVE, data=DATA06)
OLS3<-lm(EUROD ~ PARTNER+AGEC+GENDER+EDU+COUNTRY+WAVE, data=DATA06)

stargazer(OLS1, OLS2, OLS3, type="text")

FE1<-plm(EUROD ~ PARTNER+WAVE, data=DATA06, index=c("ID","WAVE"), model = "within")
FE2<-plm(EUROD ~ PARTNER+AGEC, data=DATA06, index=c("ID","WAVE"), model = "within") 
FE3<-plm(EUROD ~ PARTNER+age, data=DATA06, index=c("ID","WAVE"), model = "within") 

stargazer(OLS1, FE1, FE2, FE3, type="text")

setwd("/Users/alexandrarottenkolber/Documents/02_Hertie_School/SoSe2022/Applied_Longitudinal_Data_Analysis_Diversity_across_the_Life_Course/")

stargazer(OLS3, FE1, FE2, type="html", out="./03_Exercises/Exercise_6/models.html", 
          dep.var.labels=c("Level of depression"), 
          covariate.labels=c("Partnership status-with partner",
                             "Age category: 60-69",
                             "Age category: 70-79",
                             "Sex-men",
                             "Education-tertiary", 
                             "Education-upper and post", 
                             "Belgium", 
                             "Denmark", 
                             "France", 
                             "Germany", 
                             "Greece", 
                             "Italy", 
                             "Netherlands", 
                             "Spain", 
                             "Sweden", 
                             "Switzerland", 
                             "Wave 2", 
                             "Wave 4", 
                             "Wave 5", 
                             "Wave 6", 
                             "Wave 7", 
                             "Wave 8", 
                             "Constant"))


