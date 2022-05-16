# ------------------------------------------------------------------------
# Script name: 09_PCA.R
# Purpose of script: Principal component analysis to investigate dimensionality of the data
# Dependencies: script 08_a and 08_b
# Author: Alexandra Rottenkolber 
# ------------------------------------------------------------------------

library(ggplot2)

## load data ----------------------------------------------
load("./01_data/Wikipedia/output/wikimeasures_df.RData")

# get variable labels
#variable_labels <- read_csv2("../data/variable_names.csv", col_types = cols())

## prepare data for factor analysis ---------------------
colnames(wikimeasures_df)
wikimeasures_df_fa <- dplyr::select(wikimeasures_df, article_size, pageviews_avg, revisions, editors, pagerank, pageRankGlobalALL, no_lang_ed) # set as in Simons paper
flag_completes <- complete.cases(wikimeasures_df_fa)
table(flag_completes) # 157 non-complete cases
wikimeasures_df_fa <- wikimeasures_df_fa[flag_completes,]
wikimeasures_df_fa[wikimeasures_df_fa == 0] <- 0.1 # correction for log measures

apply(wikimeasures_df_fa, 2, mean)
apply(wikimeasures_df_fa, 2, sd)

wikimeasures_df_fa <- log(wikimeasures_df_fa)

hist(wikimeasures_df_fa$editors)
apply(wikimeasures_df_fa, 2, mean)
apply(wikimeasures_df_fa, 2, sd)

wikimeasures_df_fa.pca = prcomp(wikimeasures_df_fa, center = TRUE, scale = TRUE)
wikimeasures_df_fa.pca$rotation
wikimeasures_df_fa.pca.var = wikimeasures_df_fa.pca$sdev^2
wikimeasures_df_fa.pca.var

wikimeasures_df_fa.pca.ve <- wikimeasures_df_fa.pca.var/sum(wikimeasures_df_fa.pca.var)
wikimeasures_df_fa.pca.ve

## Generate Scree Plot to show variance explained by the different PCs. 
pdf(file="./03_figures/Scree_plot_PCA_Variance_Explaiend.pdf", height=3, width=4, family="URWTimes")
par(oma=c(0,1,0,0) + .2)
par(mar=c(3, 1, 0, 0))
qplot(c(1:7), wikimeasures_df_fa.pca.ve) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1) +
  scale_x_continuous(breaks = round(seq(1, 7, by = 1),1))
dev.off()

biplot(wikimeasures_df_fa.pca, 
       scale = 0, 
       cex = 0.6, 
       ylim  = c(-3, 3))


wikimeasures_df_fa.pca


