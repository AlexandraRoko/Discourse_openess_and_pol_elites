# ------------------------------------------------------------------------
# Script name: 10_factor_analysis.R
# Purpose of script: perform factor analysis and plot influence-prominence figures
# Dependencies: script 08_a and 08_b
# Author: Alexandra Rottenkolber based on a script by Simon Munzert
# ------------------------------------------------------------------------

## load data ----------------------------------------------
load("./01_data/Wikipedia/output/wikimeasures_df.RData")
load("./01_data/Wikipedia/output/wikimeasures_df_with_time_dim.RData")

## load packages and functions -------------------------------
library(ggpubr)

source("_packages.r")
source("_functions.r")

# prepare data for factor analysis ---------------------
wikimeasures_df_fa <- dplyr::select(wikimeasures_df, article_size, pageviews_avg, revisions, editors, pagerank, no_lang_ed) # set as in Simons paper without pageRankGlobalALL
flag_completes <- complete.cases(wikimeasures_df_fa)
table(flag_completes) # 157 non-complete cases
wikimeasures_df_fa <- wikimeasures_df_fa[flag_completes,]
wikimeasures_df_fa[wikimeasures_df_fa == 0] <- 0.1 # correction for log measures
wikimeasures_df_fa <- log(wikimeasures_df_fa)
wikimeasures_df <- wikimeasures_df[flag_completes,]

# Historgram based on logged data
hist(wikimeasures_df_fa$article_size)
hist(wikimeasures_df_fa$pageviews_avg)
hist(wikimeasures_df_fa$revisions)
hist(wikimeasures_df_fa$editors)
hist(wikimeasures_df_fa$pagerank)
hist(wikimeasures_df_fa$pageRankGlobalALL)
hist(wikimeasures_df_fa$no_lang_ed)



## explore correlation matrix of indicators ---------------------
# plot correlation between variables
dat <- wikimeasures_df_fa
colnames(dat)
colnames(dat) <- c("Article size",
                   "Average number of page views",
                   "Number of revisions",
                   "Number of editors",
                   "PageRank",
                   #"Pagerank Global",
                   "Number of language editions")

# plot correlation between variables
pdf(file="./03_figures/part_1/corrplot_green.pdf", height=6, width=7, family="URWTimes")
par(oma=c(0,1,0,0) + .2)
par(mar=c(0, 0, 0, 0))
corr_plot <- corrplot::corrplot(cor(dat), 
                                order = "alphabet", 
                                method = "circle", 
                                type = "lower", 
                                tl.col='black', 
                                tl.cex=.75, 
                                p.mat = cor(dat), 
                                insig = "p-value", 
                                sig.level=-1, 
                                col=colorRampPalette(c("red","white","darkgreen"))(20), 
                                tl.srt = 45, 
                                tl.offset = 0.5, 
                                addCoef.col = "white", 
                                cl.lim = c(0,1)) 
dev.off()



# standardise measures and take a look
wikimeasures_df_fa_standard <- wikimeasures_df_fa

wikimeasures_df_fa_standard <- 
  wikimeasures_df_fa %>% 
  mutate(article_size_s = scale(article_size))  %>% 
  mutate(pageviews_avg_s = scale(pageviews_avg)) %>% 
  mutate(revisions_s = scale(revisions)) %>% 
  mutate(editors_s = scale(editors)) %>% 
  mutate(pagerank_s = scale(pagerank)) %>% 
  mutate(no_lang_ed_s = scale(no_lang_ed)) 

wikimeasures_df_fa_standard <- wikimeasures_df_fa_standard %>% dplyr::select(article_size_s, pageviews_avg_s, revisions_s, editors_s, pagerank_s, no_lang_ed_s)

# Historgram based on standardised data
hist(wikimeasures_df_fa_standard$article_size_s)
hist(wikimeasures_df_fa_standard$pageviews_avg_s)
hist(wikimeasures_df_fa_standard$revisions_s)
hist(wikimeasures_df_fa_standard$editors_s)
hist(wikimeasures_df_fa_standard$pagerank_s)
hist(wikimeasures_df_fa_standard$no_lang_ed_s)


## one factor 
wikimeasures_df_fa.fa1 <- factanal(wikimeasures_df_fa, factors = 1, rotation = "promax", scores = "none")
fact1 <- fa(wikimeasures_df_fa, nfactors = 1, rotate = "promax", scores = "regression", fm = "ml")

wikimeasures_df_fa.fa1
wikimeasures_df_fa.fa1$uniquenesses

## two factors 
wikimeasures_df_fa.fa2 <- factanal(wikimeasures_df_fa, factors = 2, rotation = "promax", scores = "none")
wikimeasures_df_fa.fa2

fact2 <- fa(wikimeasures_df_fa, nfactors = 2, rotate = "promax", scores = "regression", fm = "ml")
wikimeasures_df_fa.fa2$uniquenesses

wikimeasures_df_fa.fa2$loadings
apply(wikimeasures_df_fa.fa2$loadings^2,1,sum) # communality => a good model: high values for communality, low values for uniqueness
1 - apply(wikimeasures_df_fa.fa2$loadings^2,1,sum) # uniqueness

apply(fact2$loadings^2,1,sum) # communality => a good model: high values for communality, low values for uniqueness
1 - apply(fact2$loadings^2,1,sum) # uniqueness

#### with standardised data
wikimeasures_df_fa.fa2_standard <- factanal(wikimeasures_df_fa_standard , factors = 2, rotation = "promax", scores = "none")
wikimeasures_df_fa.fa2_standard 

fact2_standard  <- fa(wikimeasures_df_fa_standard , nfactors = 2, rotate = "promax", scores = "regression", fm = "ml")
wikimeasures_df_fa.fa2_standard $uniquenesses

wikimeasures_df_fa.fa2_standard $loadings
apply(wikimeasures_df_fa.fa2_standard $loadings^2,1,sum) # communality => a good model: high values for communality, low values for uniqueness
1 - apply(wikimeasures_df_fa.fa2_standard $loadings^2,1,sum) # uniqueness

apply(fact2_standard $loadings^2,1,sum) # communality => a good model: high values for communality, low values for uniqueness
1 - apply(fact2_standard $loadings^2,1,sum) # uniqueness

## three factors 
wikimeasures_df_fa.fa3 <- factanal(wikimeasures_df_fa, factors = 3, rotation = "promax", scores = "none")
wikimeasures_df_fa.fa3
fact3 <- fa(wikimeasures_df_fa, nfactors = 3, rotate = "promax", scores = "regression", fm = "ml")

wikimeasures_df_fa.fa3$uniquenesses

## Residual Matrix for the two factor model
Lambda <- wikimeasures_df_fa.fa2$loadings
Psi <- diag(wikimeasures_df_fa.fa2$uniquenesses)
S <- wikimeasures_df_fa.fa2$correlation
Sigma <- Lambda %*% t(Lambda) + Psi

round(S - Sigma, 6)

### Other model type, 2 factors, finding the right rotation => promax is best
wikimeasures_df_fa.fa2.none <- factanal(wikimeasures_df_fa, factors = 2, rotation = "none")
wikimeasures_df_fa.fa2.varimax <- factanal(wikimeasures_df_fa, factors = 2, rotation = "varimax")
wikimeasures_df_fa.fa2.promax <- factanal(wikimeasures_df_fa, factors = 2, rotation = "promax")

par(mfrow = c(1,3))
plot(wikimeasures_df_fa.fa2.none$loadings[,1], 
     wikimeasures_df_fa.fa2.none$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2", 
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "No rotation")
abline(h = 0, v = 0)

plot(wikimeasures_df_fa.fa2.varimax$loadings[,1], 
     wikimeasures_df_fa.fa2.varimax$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2", 
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "Varimax rotation")

# text(wikimeasures_df_fa.fa2.varimax$loadings[,1]-0.08, 
#      wikimeasures_df_fa.fa2.varimax$loadings[,2]+0.08,
#      colnames(wikimeasures_df_fa),
#      col="blue")
abline(h = 0, v = 0)

plot(wikimeasures_df_fa.fa2.promax$loadings[,1], 
     wikimeasures_df_fa.fa2.promax$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2",
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "Promax rotation")
abline(h = 0, v = 0)

text(wikimeasures_df_fa.fa2.promax$loadings[,1]-0.08,
     wikimeasures_df_fa.fa2.promax$loadings[,2]+0.08,
     colnames(wikimeasures_df_fa),
     col="blue")
abline(h = 0, v = 0)


#### finding the right score => Res: Does not make any difference 
wikimeasures_df_fa.fa2.promaxnone <- factanal(wikimeasures_df_fa, factors = 2, rotation = "promax", scores = "none")
wikimeasures_df_fa.fa2.promaxregression <- factanal(wikimeasures_df_fa, factors = 2, rotation = "promax", scores = "regression")
wikimeasures_df_fa.fa2.promaxBartlett <- factanal(wikimeasures_df_fa, factors = 2, rotation = "promax", scores = "Bartlett")

par(mfrow = c(1,3))
plot(wikimeasures_df_fa.fa2.promaxnone$loadings[,1], 
     wikimeasures_df_fa.fa2.promaxnone$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2", 
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "No scores")
abline(h = 0, v = 0)

dev.off()

plot(wikimeasures_df_fa.fa2.promaxregression$loadings[,1], 
     wikimeasures_df_fa.fa2.promaxregression$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2", 
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "Regression scores")

text(wikimeasures_df_fa.fa2.promaxregression$loadings[,1]-0.08, 
     wikimeasures_df_fa.fa2.promaxregression$loadings[,2]+0.08,
     colnames(wikimeasures_df_fa),
     col="blue")
abline(h = 0, v = 0)

plot(wikimeasures_df_fa.fa2.promaxBartlett$loadings[,1], 
     wikimeasures_df_fa.fa2.promaxBartlett$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2",
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "Bartlett scores")
abline(h = 0, v = 0)

text(wikimeasures_df_fa.fa2.promaxBartlett$loadings[,1]-0.08, 
     wikimeasures_df_fa.fa2.promaxBartlett$loadings[,2]+0.08,
     colnames(wikimeasures_df_fa),
     col="blue")
abline(h = 0, v = 0)


pairs.panels(wikimeasures_df_fa,pch='.')


### Plot factor loadings nicely (https://rpubs.com/danmirman/plotting_factor_analysis)
### 2 Factors FA Analysis
names(wikimeasures_df_fa.fa2)
loadings_wiki <- data.frame(wikimeasures_df_fa.fa2$loadings[,c(1,2)])

loadings_wiki$Variable <- row.names(loadings_wiki)
loadings_wiki$Variable <- as.factor(loadings_wiki$Variable)
row.names(loadings_wiki)

loadings_wiki.m <- melt(loadings_wiki, 
                        value.name="Loading", id="Variable")
                   #measure=c("1", "2"), 
                   #variable.name=c("Variable", "Factor"), value.name="Loading")
colnames(loadings_wiki.m)<- c("Variable","Factor","Loading")

loadings_wiki.m$Variable <- relevel(loadings_wiki.m$Variable, ref = "revisions")
loadings_wiki.m$Variable <- relevel(loadings_wiki.m$Variable, ref = "editors")
loadings_wiki.m$Variable <- relevel(loadings_wiki.m$Variable, ref = "article_size")
loadings_wiki.m$Variable <- relevel(loadings_wiki.m$Variable, ref = "pageviews_avg")
loadings_wiki.m$Variable <- relevel(loadings_wiki.m$Variable, ref = "pagerank")
loadings_wiki.m$Variable <- relevel(loadings_wiki.m$Variable, ref = "no_lang_ed")
loadings_wiki.m$Variable <- relevel(loadings_wiki.m$Variable, ref = "pageRankGlobalALL")

loadings_wiki.m
loadings_wiki.m$Factor <- relevel(loadings_wiki.m$Factor, ref = "Factor2")
loadings_wiki.m$Factor_named <- "NA"
loadings_wiki.m$Factor_named[loadings_wiki.m$Factor == "Factor2"] <- "Prominence"
loadings_wiki.m$Factor_named[loadings_wiki.m$Factor == "Factor1"] <- "Influence"
loadings_wiki.m$Factor_named <- as.factor(loadings_wiki.m$Factor_named)
loadings_wiki.m$Factor_named <- relevel(loadings_wiki.m$Factor_named, ref = "Prominence")

p <- ggplot(loadings_wiki.m, aes(Variable, abs(Loading), fill=Loading)) + 
  facet_wrap(~ Factor_named, nrow=1) + #place the factors in separate facets
  geom_bar(stat="identity") + #make the bars
  coord_flip() + #flip the axes so the test names can be horizontal  
  #define the fill color gradient: blue=positive, red=negative
  scale_fill_gradient2(name = "Loading", 
                       high = "blue", mid = "white", low = "red", 
                       midpoint=0, guide="none") +
  ylab("Loading Strength") + #improve y-axis label
  theme_bw(base_size=10) #use a black-and0white theme with set font size
p

# save plot 
pdf(file="./03_figures/FA_Alexandra_without_PageRankGlobal.pdf", height=6, width=7, family="URWTimes")
par(oma=c(0,1,0,0) + .2)
par(mar=c(3, 1, 0, 0))
p
dev.off()


### 1 Factors FA Analysis
loadings_wiki <- data.frame(wikimeasures_df_fa.fa1$loadings[,c(1)])

loadings_wiki$Variable <- c("5-Article size", 
                            "3-Average page views", 
                            "7-Number of revisions", 
                            "6-Number of editors", 
                            "4-PageRank", 
                            "1-PageRankGlobalALL", 
                            "2-Number of language editions")


loadings_wiki.m <- melt(loadings_wiki, 
                        value.name="Loading", id="Variable")

loadings_wiki.m
loadings_wiki.m$Factor_named <- "Importance"

p <- ggplot(loadings_wiki.m, aes(Variable, abs(Loading), fill=Loading)) + 
  facet_wrap(~ Factor_named, nrow=1) + #place the factors in separate facets
  geom_bar(stat="identity") + #make the bars
  coord_flip() + #flip the axes so the test names can be horizontal  
  #define the fill color gradient: blue=positive, red=negative
  scale_fill_gradient2(name = "Loading", 
                       high = "blue", mid = "white", low = "red", 
                       midpoint=0, guide="none") +
  ylab("Loading Strength") + #improve y-axis label
  theme_bw(base_size=10) #use a black-and0white theme with set font size
p


### 3 Factors FA Analysis
loadings_wiki <- data.frame(wikimeasures_df_fa.fa3$loadings[,c(1, 2, 3)])
loadings_wiki$Variable <- row.names(loadings_wiki)
loadings_wiki.m <- melt(loadings_wiki, 
                        value.name="Loading", id = "Variable")

loadings_wiki.m
loadings_wiki.m$Factor_named <- "NA"
loadings_wiki.m$Factor_named[loadings_wiki.m$variable == "Factor1"] <- "Influence"
loadings_wiki.m$Factor_named[loadings_wiki.m$variable == "Factor2"] <- "Prominence"
loadings_wiki.m$Factor_named[loadings_wiki.m$variable == "Factor3"] <- "What?"
loadings_wiki.m$Factor_named <- as.factor(loadings_wiki.m$Factor_named)

loadings_wiki.m$Factor_named <- relevel(loadings_wiki.m$Factor_named, ref = "What?")
loadings_wiki.m$Factor_named <- relevel(loadings_wiki.m$Factor_named, ref = "Prominence")

p <- ggplot(loadings_wiki.m, aes(Variable, abs(Loading), fill=Loading)) + 
  facet_wrap(~ Factor_named, nrow=1) + #place the factors in separate facets
  geom_bar(stat="identity") + #make the bars
  coord_flip() + #flip the axes so the test names can be horizontal  
  #define the fill color gradient: blue=positive, red=negative
  scale_fill_gradient2(name = "Loading", 
                       high = "blue", mid = "white", low = "red", 
                       midpoint=0, guide="none") +
  ylab("Loading Strength") + #improve y-axis label
  theme_bw(base_size=10) #use a black-and0white theme with set font size
p

### Simons FA Analysis
fact2$loadings[,c(1,2)]

loadings_wiki <- data.frame(fact2_standard$loadings[,c(1,2)])
loadings_wiki$Variable <- row.names(loadings_wiki)
loadings_wiki$Variable <- as.factor(loadings_wiki$Variable)

loadings_wiki.m <- melt(loadings_wiki, 
                        value.name="Loading", id="Variable")
colnames(loadings_wiki.m)<- c("Variable","Factor","Loading")

loadings_wiki.m
loadings_wiki.m$Factor_named <- "NA"
loadings_wiki.m$Factor_named[loadings_wiki.m$Factor == "ML1"] <- "Prominence"
loadings_wiki.m$Factor_named[loadings_wiki.m$Factor == "ML2"] <- "Influence"
loadings_wiki.m$Factor_named <- as.factor(loadings_wiki.m$Factor_named)
loadings_wiki.m$Factor_named <- relevel(loadings_wiki.m$Factor_named, ref = "Prominence")


#loadings_wiki.m$Variable <- relevel(loadings_wiki.m$Variable, ref = "pageRankGlobalALL")
loadings_wiki.m$Variable_named <- "NA"

loadings_wiki.m$Variable_named[loadings_wiki.m$Variable == "revisions_s"] <- "Number of revisions"
loadings_wiki.m$Variable_named[loadings_wiki.m$Variable == "editors_s"] <- "Number of editors"
loadings_wiki.m$Variable_named[loadings_wiki.m$Variable == "article_size_s"] <- "Article size"
loadings_wiki.m$Variable_named[loadings_wiki.m$Variable == "pageviews_avg_s"] <- "Average number of page views"
loadings_wiki.m$Variable_named[loadings_wiki.m$Variable == "pagerank_s"] <- "PageRank"
loadings_wiki.m$Variable_named[loadings_wiki.m$Variable == "no_lang_ed_s"] <- "Number of language editions"
loadings_wiki.m$Variable_named <- as.factor(loadings_wiki.m$Variable_named)

loadings_wiki.m$Variable_named <- relevel(loadings_wiki.m$Variable_named, ref = "Number of revisions")
loadings_wiki.m$Variable_named <- relevel(loadings_wiki.m$Variable_named, ref = "Number of editors")
loadings_wiki.m$Variable_named <- relevel(loadings_wiki.m$Variable_named, ref = "Article size")
loadings_wiki.m$Variable_named <- relevel(loadings_wiki.m$Variable_named, ref = "Average number of page views")
loadings_wiki.m$Variable_named <- relevel(loadings_wiki.m$Variable_named, ref = "Number of language editions")
loadings_wiki.m$Variable_named <- relevel(loadings_wiki.m$Variable_named, ref = "PageRank")


table1::label(loadings_wiki.m$Variable_named) <- "Variable"


p <- ggplot(loadings_wiki.m, aes(Variable_named, abs(Loading), fill=Loading)) + 
  facet_wrap(~ Factor_named, nrow=1) + #place the factors in separate facets
  geom_bar(stat="identity") + #make the bars
  coord_flip() + #flip the axes so the test names can be horizontal  
  #define the fill color gradient: darkgreen=positive, orange=negative
  scale_fill_gradient2(name = "Loading", 
                       high = "darkgreen", mid = "white", low = "orange", 
                       midpoint=0, guide="none") +
  ylab("Loading Strength") + 
  xlab("Variable") +#improve y-axis label
  theme_bw(base_size=10) #use a black-and0white theme with set font size
p

# plot correlation between variables
ggsave("./03_figures/part_1/FA_simon_greenwhite_without_PRglobal.jpeg", units="in", width=6, height=3, dpi=350)




## Export results

# generate factor scores
wikimeasures_df$fa_importance <- factor.scores(wikimeasures_df_fa, wikimeasures_df_fa.fa1, method = "tenBerge")$scores[,1] 
# generate ranks of factor scores
wikimeasures_df$fa_importance_rank <- rank(-wikimeasures_df$fa_importance, ties.method = "min")


# generate factor scores
wikimeasures_df$fa_influence <- factor.scores(wikimeasures_df_fa, wikimeasures_df_fa.fa2, method = "tenBerge")$scores[,1] 
wikimeasures_df$fa_prominence <- factor.scores(wikimeasures_df_fa, wikimeasures_df_fa.fa2, method = "tenBerge")$scores[,2]
# generate ranks of factor scores
wikimeasures_df$fa_influence_rank <- rank(-wikimeasures_df$fa_influence, ties.method = "min")
wikimeasures_df$fa_prominence_rank <- rank(-wikimeasures_df$fa_prominence, ties.method = "min")


# generate factor scores
wikimeasures_df$fa_1 <- factor.scores(wikimeasures_df_fa, wikimeasures_df_fa.fa3, method = "tenBerge")$scores[,1] 
wikimeasures_df$fa_2 <- factor.scores(wikimeasures_df_fa, wikimeasures_df_fa.fa3, method = "tenBerge")$scores[,2]
wikimeasures_df$fa_3 <- factor.scores(wikimeasures_df_fa, wikimeasures_df_fa.fa3, method = "tenBerge")$scores[,3] 
# generate ranks of factor scores
wikimeasures_df$fa_1_rank <- rank(-wikimeasures_df$fa_1, ties.method = "min")
wikimeasures_df$fa_2_rank <- rank(-wikimeasures_df$fa_2, ties.method = "min")
wikimeasures_df$fa_3_rank <- rank(-wikimeasures_df$fa_3, ties.method = "min")

#compare results 
options(scipen=999)

## one-factor vs. two-factor solution
# scores
wikimeasures_df %>% dplyr::select(fa_importance, fa_influence, fa_prominence) %>% cor
# ranks
wikimeasures_df %>% dplyr::select(fa_importance_rank, fa_influence_rank, fa_prominence_rank) %>% cor


# generate factor scores
wikimeasures_df$fa_importance_fa <- factor.scores(wikimeasures_df_fa, fact1, method = "tenBerge")$scores[,1] 
# generate ranks of factor scores
wikimeasures_df$fa_importance_rank_fa <- rank(-wikimeasures_df$fa_importance_fa, ties.method = "min")


# generate factor scores
wikimeasures_df$fa_influence_fa <- factor.scores(wikimeasures_df_fa, fact2, method = "tenBerge")$scores[,1] 
wikimeasures_df$fa_prominence_fa <- factor.scores(wikimeasures_df_fa, fact2, method = "tenBerge")$scores[,2]
# generate ranks of factor scores
wikimeasures_df$fa_influence_rank_fa <- rank(-wikimeasures_df$fa_influence_fa, ties.method = "min")
wikimeasures_df$fa_prominence_rank_fa <- rank(-wikimeasures_df$fa_prominence_fa, ties.method = "min")


# generate factor scores
wikimeasures_df$fa_1_fa <- factor.scores(wikimeasures_df_fa, fact3, method = "tenBerge")$scores[,1] 
wikimeasures_df$fa_2_fa <- factor.scores(wikimeasures_df_fa, fact3, method = "tenBerge")$scores[,2]
wikimeasures_df$fa_3_fa <- factor.scores(wikimeasures_df_fa, fact3, method = "tenBerge")$scores[,3] 
# generate ranks of factor scores
wikimeasures_df$fa_1_rank_fa <- rank(-wikimeasures_df$fa_1_fa, ties.method = "min")
wikimeasures_df$fa_2_rank_fa <- rank(-wikimeasures_df$fa_2_fa, ties.method = "min")
wikimeasures_df$fa_3_rank_fa <- rank(-wikimeasures_df$fa_3_fa, ties.method = "min")

#compare results 
options(scipen=999)

## one-factor vs. two-factor solution
# scores
wikimeasures_df %>% dplyr::select(fa_importance_fa, fa_influence_fa, fa_prominence_fa) %>% cor
# ranks
wikimeasures_df %>% dplyr::select(fa_importance_rank_fa, fa_influence_rank_fa, fa_prominence_rank_fa) %>% cor


#save(wikimeasures_df, file = "./01_data/Wikipedia/output/wikimeasures_df_fa_without_PageRankGlobal.RData")
#write.csv(wikimeasures_df, file = "./01_data/Wikipedia/output/wikimeasures_df_fa_without_PageRankGlobal.csv", row.names = FALSE)




load("./01_data/Wikipedia/output/wikimeasures_df_fa_without_PageRankGlobal.RData")

test_df <- subset(wikimeasures_df, select = c("wikidataid", "name", "fa_prominence_fa","fa_influence_fa",  "fa_prominence_rank_fa","fa_influence_rank_fa"))

# plot density plot
g_aggregated <- ggplot(test_df, aes(x=fa_prominence_fa, y=fa_influence_fa) ) +
  geom_hex(bins = 100) +
  #scale_fill_continuous(type = "viridis") +
  scale_fill_distiller(palette= "Spectral", direction=-1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  xlab("Prominence") +
  ylab("Influence") +
  #theme(legend.position='none') +
  xlim(-3.5, 5) +
  ylim(-2, 5) +
  #theme(legend.key.height= 0.1, legend.key.width= 0.1) +
  theme_light() +
  theme(axis.title.x = element_text(size=rel(2.5)),
        axis.title.y = element_text(size=rel(2.5)), 
        axis.text.x = element_text(size=rel(2.5)), 
        axis.text.y = element_text(size=rel(2.5)))
  #guides(shape = guide_legend(override.aes = list(size = 0.1)))
g_aggregated 

ggsave("./03_figures/part_1/density_plot_prominence_influence2_aggregated.png", width = 5, height = 4, dpi = 350)


test_df <- distinct(test_df, wikidataid, .keep_all = TRUE)

#scatterplot with labels 
p_labelled <- ggplot(test_df, aes(x=fa_prominence_fa, y=fa_influence_fa, label=name) ) +
  geom_point(color = "#555555", alpha = 1) +
  # geom_text_repel(data=subset(test_df, fa_prominence_fa > 4 | fa_influence_fa > 4 |
  #                         #name == "Frank-Walter Steinmeier" |
  #                         name == "Dietmar Bartsch" |
  #                         #name == "Alexander Gauland" |
  #                         #name == "Dorothea Szwed" |
  #                         #name == "Karl-Theodor zu Guttenberg" |
  #                         name == "Sahra Wagenknecht"|
  #                         # name == "Thomas de Maizière" |
  #                         name == "Cem Özdemir" |
  #                         name == "Karl Lauterbach" |
  #                         name == "Martin Schulz" |
  #                         name == "Katarina Barley" |
  #                         name == "Alexander Dobrindt" |
  #                         name == "Rita Schwarzelühr-Sutter"|
  #                         #name == "Annalena Baerbock" |
  #                         name == "Annette Schavan" |
  #                         name == "Peer Steinbrück" |
  #                         name == "Lisa Badum" |
  #                         name == "Dieter-Julius Cronenberg" |
  #                         name == "Barbara Weiler" |
  #                         name == "Josephine Ortleb" | 
  #                         name == "Gyde Jensen" |
  #                         name == "Hans-Peter Uhl"), 
  #           check_overlap = TRUE) +
  geom_label_repel(data=subset(test_df, name == "Angela Merkel" |
                                 name == "Helmut Kohl" |
                                 #name == "Gerhard Schröder" |
                                 name == "Martin Schulz" |
                           name == "Frank-Walter Steinmeier" |
                           name == "Dietmar Bartsch" |
                           name == "Alexander Gauland" |
                           #name == "Dorothea Szwed" |
                           #name == "Karl-Theodor zu Guttenberg" |
                           name == "Sahra Wagenknecht"|
                           # name == "Thomas de Maizière" |
                           name == "Cem Özdemir" |
                           name == "Karl Lauterbach" |
                           name == "Martin Schulz" |
                           name == "Katarina Barley" |
                           name == "Alexander Dobrindt" |
                           name == "Rita Schwarzelühr-Sutter"|
                           #name == "Annalena Baerbock" |
                           name == "Annette Schavan" |
                           name == "Peer Steinbrück" |
                           name == "Lisa Badum" |
                           name == "Dieter-Julius Cronenberg" |
                           name == "Barbara Weiler" |
                           name == "Josephine Ortleb" | 
                         name == "Gyde Jensen" |
                         name == "Hans-Peter Uhl")) +
  xlab("Prominence") +
  ylab("Influence") +
  theme_light() +
  theme(axis.title.x = element_text(size=rel(2.5)),
        axis.title.y = element_text(size=rel(2.5)), 
        axis.text.x = element_text(size=rel(2.5)), 
        axis.text.y = element_text(size=rel(2.5)))
p_labelled


ggsave("./03_figures/part_1/density_plot_prominence_influence2_labelled.png", width = 8, height = 7, dpi = 350)




## Export results with time dimension

load("./01_data/Wikipedia/output/wikimeasures_df_with_time_dim.RData")

final_data_df$revisions <- final_data_df$no_of_revisions
final_data_df$editors <- final_data_df$unique_users
final_data_df$no_lang_ed <- final_data_df$cum_sum_no_languages

wikimeasures_df <- final_data_df

inv_df <- subset(final_data_df, final_data_df$YEAR == 2014)


wikimeasures_df_fa <- dplyr::select(final_data_df, article_size, pageviews_avg, revisions, editors, pagerank, no_lang_ed) # set as in Simons paper without pageRankGlobalALL
flag_completes <- complete.cases(wikimeasures_df_fa)
table(flag_completes) # 157 non-complete cases
wikimeasures_df_fa <- wikimeasures_df_fa[flag_completes,]
wikimeasures_df_fa[wikimeasures_df_fa == 0] <- 0.1 # correction for log measures
wikimeasures_df_fa <- log(wikimeasures_df_fa)
wikimeasures_df <- wikimeasures_df[flag_completes,]

# Historgram based on logged data
hist(wikimeasures_df_fa$article_size)
hist(wikimeasures_df_fa$pageviews_avg)
hist(wikimeasures_df_fa$revisions)
hist(wikimeasures_df_fa$editors)
hist(wikimeasures_df_fa$pagerank)
hist(wikimeasures_df_fa$no_lang_ed)

# generate factor scores
wikimeasures_df$fa_importance <- factor.scores(wikimeasures_df_fa, wikimeasures_df_fa.fa1, method = "tenBerge")$scores[,1] 
# generate ranks of factor scores
wikimeasures_df$fa_importance_rank <- rank(-wikimeasures_df$fa_importance, ties.method = "min")


# generate factor scores
wikimeasures_df$fa_influence <- factor.scores(wikimeasures_df_fa, wikimeasures_df_fa.fa2, method = "tenBerge")$scores[,1] 
wikimeasures_df$fa_prominence <- factor.scores(wikimeasures_df_fa, wikimeasures_df_fa.fa2, method = "tenBerge")$scores[,2]
# generate ranks of factor scores
wikimeasures_df$fa_influence_rank <- rank(-wikimeasures_df$fa_influence, ties.method = "min")
wikimeasures_df$fa_prominence_rank <- rank(-wikimeasures_df$fa_prominence, ties.method = "min")


# generate factor scores
wikimeasures_df$fa_1 <- factor.scores(wikimeasures_df_fa, wikimeasures_df_fa.fa3, method = "tenBerge")$scores[,1] 
wikimeasures_df$fa_2 <- factor.scores(wikimeasures_df_fa, wikimeasures_df_fa.fa3, method = "tenBerge")$scores[,2]
wikimeasures_df$fa_3 <- factor.scores(wikimeasures_df_fa, wikimeasures_df_fa.fa3, method = "tenBerge")$scores[,3] 
# generate ranks of factor scores
wikimeasures_df$fa_1_rank <- rank(-wikimeasures_df$fa_1, ties.method = "min")
wikimeasures_df$fa_2_rank <- rank(-wikimeasures_df$fa_2, ties.method = "min")
wikimeasures_df$fa_3_rank <- rank(-wikimeasures_df$fa_3, ties.method = "min")

#compare results 
options(scipen=999)

## one-factor vs. two-factor solution
# scores
wikimeasures_df %>% dplyr::select(fa_importance, fa_influence, fa_prominence) %>% cor
# ranks
wikimeasures_df %>% dplyr::select(fa_importance_rank, fa_influence_rank, fa_prominence_rank) %>% cor


# generate factor scores
wikimeasures_df$fa_importance_fa <- factor.scores(wikimeasures_df_fa, fact1, method = "tenBerge")$scores[,1] 
# generate ranks of factor scores
wikimeasures_df$fa_importance_rank_fa <- rank(-wikimeasures_df$fa_importance_fa, ties.method = "min")


# generate factor scores
wikimeasures_df$fa_influence_fa <- factor.scores(wikimeasures_df_fa, fact2, method = "tenBerge")$scores[,1] 
wikimeasures_df$fa_prominence_fa <- factor.scores(wikimeasures_df_fa, fact2, method = "tenBerge")$scores[,2]
# generate ranks of factor scores
wikimeasures_df$fa_influence_rank_fa <- rank(-wikimeasures_df$fa_influence_fa, ties.method = "min")
wikimeasures_df$fa_prominence_rank_fa <- rank(-wikimeasures_df$fa_prominence_fa, ties.method = "min")


# generate factor scores
wikimeasures_df$fa_1_fa <- factor.scores(wikimeasures_df_fa, fact3, method = "tenBerge")$scores[,1] 
wikimeasures_df$fa_2_fa <- factor.scores(wikimeasures_df_fa, fact3, method = "tenBerge")$scores[,2]
wikimeasures_df$fa_3_fa <- factor.scores(wikimeasures_df_fa, fact3, method = "tenBerge")$scores[,3] 
# generate ranks of factor scores
wikimeasures_df$fa_1_rank_fa <- rank(-wikimeasures_df$fa_1_fa, ties.method = "min")
wikimeasures_df$fa_2_rank_fa <- rank(-wikimeasures_df$fa_2_fa, ties.method = "min")
wikimeasures_df$fa_3_rank_fa <- rank(-wikimeasures_df$fa_3_fa, ties.method = "min")

#compare results 
options(scipen=999)

## one-factor vs. two-factor solution
# scores
wikimeasures_df %>% dplyr::select(fa_importance_fa, fa_influence_fa, fa_prominence_fa) %>% cor
# ranks
wikimeasures_df %>% dplyr::select(fa_importance_rank_fa, fa_influence_rank_fa, fa_prominence_rank_fa) %>% cor

#save(wikimeasures_df, file = "./01_data/Wikipedia/output/wikimeasures_df_fa_without_PageRankGlobal_with_timedim.RData")
#write.csv(wikimeasures_df, file = "./01_data/Wikipedia/output/wikimeasures_df_fa_without_PageRankGlobal_with_timedim.csv", row.names = FALSE)


load("./01_data/Wikipedia/output/wikimeasures_df_fa_without_PageRankGlobal_with_timedim.RData")

colnames(wikimeasures_df)

inv <-  subset(wikimeasures_df, select = c("wikidataid", "YEAR", "name", "fa_influence_fa","fa_prominence_fa","fa_influence_rank_fa","fa_prominence_rank_fa"))

colnames(wikimeasures_df)

hist(wikimeasures_df$fa_prominence_fa)
hist(wikimeasures_df$fa_influence_fa)


# plot density plot
g_per_year <- ggplot(inv, aes(x=fa_prominence_fa, y=fa_influence_fa) ) +
  geom_hex(bins = 100) +
  #scale_fill_continuous(type = "viridis") +
  scale_fill_distiller(palette= "Spectral", direction=-1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  xlab("Prominence") +
  ylab("Influence") +
  #theme(legend.position='none') +
  xlim(-3.5, 5) +
  ylim(-2, 5) +
  #theme(legend.key.height= 0.1, legend.key.width= 0.1) +
  theme_light() +
  theme(axis.title.x = element_text(size=rel(2.5)),
        axis.title.y = element_text(size=rel(2.5)), 
        axis.text.x = element_text(size=rel(2.5)), 
        axis.text.y = element_text(size=rel(2.5)))
#guides(shape = guide_legend(override.aes = list(size = 0.1)))
g_per_year


ggsave("./03_figures/part_1/density_plot_prominence_influence2_per_year.png", width = 5, height = 4, dpi = 350)


p_combined <- ggarrange(g_aggregated, 
          g_per_year,
          labels = c("A", "B"),
          ncol = 2, nrow = 1)
p_combined

ggsave("./03_figures/part_1/density_plot_prominence_influence2_combined.png", width = 8, height = 3, dpi = 350)

#scatterplot with labels 
p_labelled_time_dim <- ggplot(inv, aes(x=fa_prominence_fa, y=fa_influence_fa, label=YEAR)) +
  geom_point(color = "#555555", alpha = 1) +
  geom_point(data = inv %>% filter(name == "Annalena Baerbock"), color = "#3498DB", size = 3) +
  geom_label_repel(data=subset(inv, #name == "Angela Merkel" |
                               #name == "Helmut Kohl" |
                               #name == "Gerhard Schröder" |
                               #name == "Martin Schulz" |
                               #name == "Frank-Walter Steinmeier" |
                               #name == "Dietmar Bartsch" |
                               #name == "Alexander Gauland" |
                               #name == "Dorothea Szwed" |
                               #name == "Karl-Theodor zu Guttenberg" |
                               #name == "Sahra Wagenknecht"|
                               # name == "Thomas de Maizière" |
                               #name == "Cem Özdemir" |
                               #name == "Karl Lauterbach" #|
                               #name == "Martin Schulz" |
                               #name == "Katarina Barley" |
                               #name == "Alexander Dobrindt" #|
                               #name == "Rita Schwarzelühr-Sutter"|
                               name == "Annalena Baerbock" # |
                               #name == "Annette Schavan" |
                               #name == "Peer Steinbrück" |
                               #name == "Lisa Badum" |
                               #name == "Dieter-Julius Cronenberg" |
                               #name == "Barbara Weiler" |
                               #name == "Josephine Ortleb" | 
                               #name == "Gyde Jensen" |
                               #name == "Hans-Peter Uhl"
                                 )) +
  xlab("Prominence") +
  ylab("Influence") +
  theme_light() +
  theme(axis.title.x = element_text(size=rel(2.5)),
        axis.title.y = element_text(size=rel(2.5)), 
        axis.text.x = element_text(size=rel(2.5)), 
        axis.text.y = element_text(size=rel(2.5)))
p_labelled_time_dim

p_labeleld_combined <- ggarrange(p_labelled, 
                                 p_labelled_time_dim,
                        labels = c("A", "B"),
                        ncol = 2, nrow = 1)
p_labeleld_combined

ggsave("./03_figures/part_1/density_plot_prominence_influence2_labelled_combined.png", width = 15, height = 7, dpi = 350)


p_all <- ggarrange(g_aggregated,
                   p_labelled, 
                   g_per_year,
                   p_labelled_time_dim,
  labels = c("A", "B", "C", "D"),
  ncol = 2, nrow = 2,
  font.label = list(size = 30, color = "black", family = NULL))
p_all

ggsave("./03_figures/part_1/density_plot_prominence_influence2_all_combined2_3.png", width = 12, height = 11, dpi = 350)



