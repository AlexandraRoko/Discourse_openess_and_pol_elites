### Measuring the Significance of Political Elites
### Simon Munzert

## load packages and functions -------------------------------
source("_packages.r")
source("_functions.r")

## load data ----------------------------------------------
load("./data/output/wikimeasures_df.RData")


# get variable labels
#variable_labels <- read_csv2("../data/variable_names.csv", col_types = cols())


## prepare data for factor analysis ---------------------
colnames(wikimeasures_df)
#wikimeasures_df_fa <- dplyr::select(wikimeasures_df, revisions, editors, article_size, pageviews_avg, pageRankGlobalALL, pagerank, no_lang_ed, article_extlinks, articles_intlinks, unique_references)
#wikimeasures_df_fa <- dplyr::select(wikimeasures_df, revisions, editors, characters, pageviews_avg,  pageRankGlobalALL, pagerank, no_lang_ed)
wikimeasures_df_fa <- dplyr::select(wikimeasures_df, article_size, pageviews_avg, revisions, editors, pagerank, pageRankGlobalALL, no_lang_ed) # set as in Simons paper
flag_completes <- complete.cases(wikimeasures_df_fa)
table(flag_completes) # 157 non-complete cases
wikimeasures_df_fa <- wikimeasures_df_fa[flag_completes,]
wikimeasures_df_fa[wikimeasures_df_fa == 0] <- 0.1 # correction for log measures
wikimeasures_df_fa <- log(wikimeasures_df_fa)
wikimeasures_df <- wikimeasures_df[flag_completes,]

# export factor variable dataset
save(wikimeasures_df_fa, file = "./data/output/wikimeasures_df_fa.RData")



## explore correlation matrix of indicators ---------------------

# plot correlation between variables
pdf(file="./figures/corrplot.pdf", height=6, width=7, family="URWTimes")
par(oma=c(0,1,0,0) + .2)
par(mar=c(3, 1, 0, 0))
dat <- wikimeasures_df_fa
corrplot::corrplot(cor(dat), 
                   order = "alphabet", 
                   method = "circle", 
                   type = "lower", 
                   tl.col='black', 
                   tl.cex=.75, 
                   p.mat = cor(dat), 
                   insig = "p-value", 
                   sig.level=-1, 
                   color="white", 
                   tl.srt = 45, 
                   tl.offset = 0.5, 
                   addCoef.col = "white", 
                   cl.lim = c(0,1)) 
dev.off()




### CONVENTIONAL FACTOR ANALYSIS --------------

## one-factor solution ---------------------
fact <- fa(wikimeasures_df_fa, nfactors = 1, rotate = "promax", scores = "regression", fm = "ml")
print(fact, cut = 0, digits = 3, sort = TRUE)

# generate factor scores
wikimeasures_df$fa_importance <- factor.scores(wikimeasures_df_fa, fact, method = "tenBerge")$scores[,1] # ten Berge factor scores preserve correlation between factors for oblique solution

# generate ranks of factor scores
wikimeasures_df$fa_importance_rank <- rank(-wikimeasures_df$fa_importance, ties.method = "min")


## two-factor solution ---------------------
fact <- fa(wikimeasures_df_fa, nfactors = 2, rotate = "promax", scores = "regression", fm = "ml")
print(fact, cut = 0, digits = 3, sort = TRUE)

# generate factor scores
wikimeasures_df$fa_prominence <- factor.scores(wikimeasures_df_fa, fact, method = "tenBerge")$scores[,1] # ten Berge factor scores preserve correlation between factors for oblique solution
wikimeasures_df$fa_influence <- factor.scores(wikimeasures_df_fa, fact, method = "tenBerge")$scores[,2]

# generate ranks of factor scores
wikimeasures_df$fa_prominence_rank <- rank(-wikimeasures_df$fa_prominence, ties.method = "min")
wikimeasures_df$fa_influence_rank <- rank(-wikimeasures_df$fa_influence, ties.method = "min")
dplyr::select(wikimeasures_df, wikititle, fa_prominence_rank, fa_influence_rank) %>% View()


## three-factor solution ---------------------
fact <- fa(wikimeasures_df_fa, nfactors = 3, rotate = "promax", scores = "regression", fm = "ml")
print(fact, cut = 0, digits = 3, sort = TRUE)

# generate factor scores
wikimeasures_df$fa_1 <- factor.scores(wikimeasures_df_fa, fact, method = "tenBerge")$scores[,1] 
wikimeasures_df$fa_2 <- factor.scores(wikimeasures_df_fa, fact, method = "tenBerge")$scores[,2]
wikimeasures_df$fa_3 <- factor.scores(wikimeasures_df_fa, fact, method = "tenBerge")$scores[,3] 

# generate ranks of factor scores
wikimeasures_df$fa_1_rank <- rank(-wikimeasures_df$fa_1, ties.method = "min")
wikimeasures_df$fa_2_rank <- rank(-wikimeasures_df$fa_2, ties.method = "min")
wikimeasures_df$fa_3_rank <- rank(-wikimeasures_df$fa_3, ties.method = "min")


## compare one-factor with two-factor and three-factor solution ---------------------

## one-factor vs. two-factor solution
# scores
wikimeasures_df %>% dplyr::select(fa_importance, fa_influence, fa_prominence) %>% cor
# ranks
wikimeasures_df %>% dplyr::select(fa_importance_rank, fa_influence_rank, fa_prominence_rank) %>% cor

## one-factor solution mainly picking up the prominence dimension with 
cor(wikimeasures_df$fa_importance, wikimeasures_df$fa_prominence)
cor(wikimeasures_df$fa_importance, wikimeasures_df$fa_influence)

## two-factor vs. three-factor solution
wikimeasures_df %>% dplyr::select(fa_influence, fa_prominence, fa_1, fa_2, fa_3) %>% cor
# ranks
wikimeasures_df %>% dplyr::select(fa_influence_rank, fa_prominence_rank, fa_1_rank, fa_2_rank, fa_3_rank) %>% cor

# unclear what third factor picks up - in some ways unusual politicians and non-politicians(Bush, Schwarzenegger, Kevorkian)
dplyr::select(wikimeasures_df, fa_1_rank, fa_2_rank, fa_3_rank) %>% arrange(fa_3_rank) %>% head(10)



## explore dimensionality ---------------------

# scree plot to determine the adequate number of dimensions
pdf(file="./figures/screeplot.pdf", height=3, width=6, family="URWTimes")
par(oma=c(0,0,0,0) + .7)
par(mar=c(4, 4, 0, 0))
fa.parallel.refined(wikimeasures_df_fa, fa = "fa", ylabel = "Eigenvalue", xlabel = "Factor number", main = "", sim = FALSE, show.legend = FALSE, fm = "ml")
dev.off()

# proportion of variance explained by different solutions; BIC
fact1 <- fa(wikimeasures_df_fa, nfactors = 1, rotate = "promax", scores = "regression", fm = "ml")
fact1$Vaccounted
fact1$Vaccounted[2,1]
fact1$BIC

?fa

fact2 <- fa(wikimeasures_df_fa, nfactors = 2, rotate = "promax", scores = "regression", fm = "ml")
fact2$Vaccounted
fact2$Vaccounted[3,2]
fact2$BIC

fact3 <- fa(wikimeasures_df_fa, nfactors = 3, rotate = "promax", scores = "regression", fm = "ml")
fact3$Vaccounted
fact3$Vaccounted[3,3]
fact3$BIC

## calculate DIC? https://sourceforge.net/p/mcmc-jags/discussion/610037/thread/ea46dc43/


## assemble and export table
variance_explained <- c(fact1$Vaccounted[2,1], fact2$Vaccounted[3,2], fact3$Vaccounted[3,3])
bic <- c(fact1$BIC, fact2$BIC, fact3$BIC)

dat <- data.frame(variance_explained, bic)
names(dat) <- c("Variance explained", "BIC") # Bayesian information criterion => models with lower BIC are generally preferred
head(dat)

rownames(dat) <- c("1 factor", "2 factors", "3 factors")
cols_align <- c("l", rep("r", ncol(dat)))

print(xtable(dat, align = cols_align, digits = 2, caption = "Model fit statistics for various factor specifications.\\label{tab:modelfit}"), booktabs = TRUE, size = "normalsize", caption.placement = "top", table.placement = "h!t", include.rownames=TRUE, include.colnames = TRUE, sanitize.text.function = identity, file = "../figures/modelfit-factors.tex")



## Wilcoxon Rank Sum test for two-factor solution ---------------------------

wilcox.test(wikimeasures_df$fa_influence, wikimeasures_df$fa_prominence,
            alternative = "two.sided",
            paired = FALSE)
wilcox.test(wikimeasures_df$fa_importance, wikimeasures_df$fa_prominence,
            alternative = "two.sided",
            paired = FALSE)


### BAYESIAN FACTOR ANALYSIS --------
set.seed(123)
dat <- wikimeasures_df_fa
N = nrow(dat)
J = ncol(dat)
for_jags <- list(y = dat,       # data
                 N = nrow(dat), # number of observations
                 J = ncol(dat), # number of indicators
                 halfJ = 4      # split point for items 
)

## two-factor model estimation
if(file.exists("./data/output/factorMCMC.rda")){
  load("./data/output/factorMCMC.rda")
} else {
  factor_mcmc <- jags.parallel(model.file = "/Users/alexandrarottenkolber/Documents/02_Hertie_School/Master thesis/Master_Thesis_Hertie/prominence-code_Simon_AR/factorMCMCcor.jag", 
                               data = for_jags, 
                               parameters.to.save = c("lambda1", "lambda2","phi", "factor"),
                               n.chains = 3,
                               n.burnin = 2000,
                               n.iter = 5000)
  save(factor_mcmc, file = "./data/output/factor_analysis/factorMCMC.rda")
}
#plot(factor_mcmc)
#traceplot(factor_mcmc)

?jags.parallel
??combine.mcmc

# extract estimates
factor_mcmc_list <- as.mcmc(factor_mcmc)
factor_mcmc_sims <- combine.mcmc(factor_mcmc_list)
factor_mcmc_sims_df <- as.data.frame(factor_mcmc_sims)[c(501:1000,1501:2000, 2501:3000),] # cut off first 500 iterations of each chain (not fully converged yet)

# sumamry statistics on full matrix of simulations
factor_mcmc_sum_df_full <- factor_mcmc$BUGSoutput$summary %>% as.data.frame(stringsAsFactors = FALSE)

# summary statistics
factor_mcmc_sum_df <- data.frame(mean = sapply(factor_mcmc_sims_df, mean),
                                 sd = sapply(factor_mcmc_sims_df, sd),
                                 ci95lo = sapply(factor_mcmc_sims_df, quantile, 0.025),
                                 ci95hi = sapply(factor_mcmc_sims_df, quantile, 0.975),
                                 median = sapply(factor_mcmc_sims_df, median),
                                 stringsAsFactors = FALSE)

# construct correct order of variable names (factors!)
parameter_names <- c("deviance", 
                     paste0("factor[", 1:nrow(dat), ",1]"), 
                     paste0("factor[", 1:nrow(dat), ",2]"),  
                     paste0("lambda1[", 1:7, "]"),
                     paste0("lambda2[", 1:7, "]"),
                     "phi")

# get order of variables right
factor_mcmc_sum_df <- factor_mcmc_sum_df[parameter_names,]
factor_mcmc_sims_df <- factor_mcmc_sims_df[,parameter_names] 

# export data
save(factor_mcmc_sims_df, file = "./data/output/factor_analysis/factorMCMCsimsdf.rda")
save(factor_mcmc_sum_df, file = "./data/output/factor_analysis/factorMCMCsumdf.rda")


# plot Rhat values
pdf(file="./figures/model_rhat.pdf", height=5, width=8, family="URWTimes")
par(oma=c(0,0,0,0) + .7)
par(mar=c(4,4,0,0))
hist(factor_mcmc_sum_df_full$Rhat, main = "", xlab = "Rhat value")
dev.off()


# plot traceplots, factor loadings + phi
traceplot_sims <- factor_mcmc_sims_df[,str_detect(colnames(factor_mcmc_sims_df), "lambda|phi")]
traceplot_sims <- select(traceplot_sims, -`lambda1[5]`, -`lambda2[1]`)

pdf(file="./figures/model_traceplots.pdf", height=10, width=7, family="URWTimes")
par(oma=c(0,0,0,0) + .7)
par(mar=c(2,2,2,2))
par(mfrow = c(5, 3))
for(i in 1:ncol(traceplot_sims)){
  plot(traceplot_sims[1:500,i],  type = "l", col = "black", main = colnames(traceplot_sims)[i], ylab = "", xlab = "")
  lines(traceplot_sims[501:1000,i],  type = "l", col = "red")
  lines(traceplot_sims[1001:1500,i],  type = "l", col = "blue")
}
dev.off()

# plot traceplots, sample of factor scores
traceplot_sims <- factor_mcmc_sims_df[,str_detect(colnames(factor_mcmc_sims_df), "factor")]

set.seed(123)
samples <- sample(1:45038, 15) %>% sort
samples2 <- 45038 + samples
#cols <- c(samples, samples2)
#traceplot_sims <- traceplot_sims[, cols]


pdf(file="./figures/model_traceplots2.pdf", height=10, width=7, family="URWTimes")
par(oma=c(0,0,0,0) + .7)
par(mar=c(2,2,2,2))
par(mfrow = c(5, 3))
for(i in 1:ncol(traceplot_sims)){
  plot(traceplot_sims[1:500,i],  type = "l", col = "black", main = colnames(traceplot_sims)[i], ylab = "", xlab = "")
  lines(traceplot_sims[501:1000,i],  type = "l", col = "red")
  lines(traceplot_sims[1001:1500,i],  type = "l", col = "blue")
}
dev.off()



### POSTESTIMATION --------

# load estimates
load("./data/output/factor_analysis/factorMCMCsimsdf.rda")
load("./data/output/factor_analysis/factorMCMCsumdf.rda")

# build result vectors
loadings <- factor_mcmc_sum_df[str_detect(rownames(factor_mcmc_sum_df), "lambda"),] %>% as.data.frame(stringsAsFactors = FALSE)
loadings_df <- data.frame(varname = names(wikimeasures_df_fa), f1 = loadings$median[1:7], f2 = loadings$median[8:14], f1_95lo = loadings$ci95lo[1:7], f1_95hi = loadings$ci95hi[1:7], f2_95lo = loadings$ci95lo[8:14], f2_95hi = loadings$ci95hi[8:14], stringsAsFactors = FALSE)
factor_scores_1 <- factor_mcmc_sum_df[str_detect(rownames(factor_mcmc_sum_df), "factor.+1\\]"),] %>% as.data.frame(stringsAsFactors = FALSE)
factor_scores_2 <- factor_mcmc_sum_df[str_detect(rownames(factor_mcmc_sum_df), "factor.+2\\]"),] %>% as.data.frame(stringsAsFactors = FALSE)
phi <- factor_mcmc_sum_df[str_detect(rownames(factor_mcmc_sum_df), "phi"),] %>% as.data.frame(stringsAsFactors = FALSE)

# export table of factor loadings
loadings_df_tex <- loadings_df
#loadings_df_tex <- merge(loadings_df_tex, variable_labels, by = "varname", all.x = TRUE)
loadings_df_tex
loadings_df_tex <- arrange(loadings_df_tex, desc(f1))
loadings_df_tex$f1_full <- paste0(round(loadings_df_tex$f1, 2), " [", 
                                  round(loadings_df_tex$f1_95lo, 2), ";",
                                  round(loadings_df_tex$f1_95hi, 2), "]")
loadings_df_tex$f2_full <- paste0(round(loadings_df_tex$f2, 2), " [", 
                                  round(loadings_df_tex$f2_95lo, 2), ";",
                                  round(loadings_df_tex$f2_95hi, 2), "]")
loadings_df_tex <- dplyr::select(loadings_df_tex, varname, f1_full, f2_full)

colnames(loadings_df_tex) <- c("Variable", "Factor 1 [95\\% CI]", "Factor 2 [95\\% CI]")
cols_align <- c("r", "r", rep("c", ncol(loadings_df_tex)-1))
print(xtable(loadings_df_tex, 
             align = cols_align, 
             digits = 3, 
             caption = "Estimated loadings from two-factor model.\\label{tab:factormodel}"), 
      booktabs = TRUE, 
      size = "scriptsize", 
      caption.placement = "top", 
      table.placement = "h!t", 
      include.rownames = FALSE, 
      include.colnames = TRUE, 
      sanitize.text.function = identity, 
      file = "./figures/tab-factormodel.tex")


# generate median Bayesian factor scores
wikimeasures_df$bfa_prominence <- factor_scores_1$median
wikimeasures_df$bfa_influence <- factor_scores_2$median
cor(wikimeasures_df$bfa_prominence, wikimeasures_df$bfa_influence)

# generate ranks of Bayesian factor scores
wikimeasures_df$bfa_prominence_rank <- rank(-wikimeasures_df$bfa_prominence, ties.method = "min")
wikimeasures_df$bfa_influence_rank <- rank(-wikimeasures_df$bfa_influence, ties.method = "min")
dplyr::select(wikimeasures_df, name, fa_prominence_rank, fa_influence_rank, bfa_prominence_rank, bfa_influence_rank) %>% View()

# compare conventional with Bayesian factor scores
plot(wikimeasures_df$fa_prominence_rank, wikimeasures_df$bfa_prominence_rank)
cor(wikimeasures_df$fa_prominence_rank, wikimeasures_df$bfa_prominence_rank)

??skew
library(psych)

# skewness of distributions
skew(wikimeasures_df$bfa_prominence)
skew(wikimeasures_df$bfa_influence)

## export dataset with factor scores ---------------------
save(wikimeasures_df, file = "./data/wikimeasures_df_fascores.RData")



## generate bar graph of factor loadings ---------------------
# code source: http://rpubs.com/danmirman/plotting_factor_analysis

# generate data.frame of loadings
#loadings_df_graph <- merge(loadings_df, variable_labels, by = "varname", all.x = TRUE)
#loadings_df_graph <- dplyr::select(loadings_df_graph, -varname)
loadings_df_graph <- loadings_df
names(loadings_df_graph) <- c("Variable","Prominence", "Influence", "f1_95lo", "f1_95hi", "f2_95lo", "f2_95hi")
loadings_df_graph <- arrange(loadings_df_graph, desc(Prominence), desc(Influence))
loadings_df_graph$Variable <- factor(loadings_df_graph$Variable, levels = rev(unique(loadings_df_graph$Variable)))


library(reshape2)
# make data.frame long
loadings.m <- melt(loadings_df_graph, id = "Variable", 
                   measure = c("Prominence", "Influence"), 
                   variable.name = "Factor", value.name = "Loading")
loadings.m$Loading_95lo <- c(loadings_df_graph$f1_95lo, loadings_df_graph$f2_95lo)
loadings.m$Loading_95hi <- c(loadings_df_graph$f1_95hi, loadings_df_graph$f2_95hi)


library(ggplot2)

# plot
pdf(file="./figures/factor_loadings.pdf", height=4, width=7, family="URWTimes")
par(oma=c(0,0,0,0) + .7)
par(mar=c(.5, .5,.5,.5))
ggplot(loadings.m, aes(Variable, abs(Loading), fill = Loading)) + 
  facet_wrap(~ Factor, nrow = 1) + # place the factors in separate facets
  geom_bar(stat = "identity") + # make the bars
  geom_errorbar(aes(ymin=Loading_95lo, ymax=Loading_95hi),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) + 
  coord_flip() + # flip the axes so the test names can be horizontal  
  # define the fill color gradient: blue = positive, red = negative
  scale_fill_gradient2(name = "Loading", 
                       high = "blue", mid = "grey", low = "red", 
                       midpoint = 0, guide = F) +
  ylab("") + 
  xlab("") + # improve y-axis label
  scale_y_continuous(breaks=seq(0,1.1,.2)) +
  theme_bw(base_size=12) # use a black-and-white theme with set font size
dev.off()


## visualize factor scores a.k.a. plotting subjects in latent variable space ---------

pdf(file="./figures/factor_scores_prominence_influence.pdf", height=5, width=8, family="URWTimes")
par(oma=c(0,0,0,0) + .7)
par(mar=c(.5, .5,.5,.5))
# prepare labels
wikimeasures_df$plot_label <- wikimeasures_df$politician_label
wikimeasures_df$much_more_prominent <- (wikimeasures_df$bfa_influence_rank / wikimeasures_df$bfa_prominence_rank) 
wikimeasures_df$much_more_central <- (wikimeasures_df$bfa_prominence_rank / wikimeasures_df$bfa_influence_rank)
wikimeasures_df$much_more_central_top1000 <- order(wikimeasures_df$much_more_central) <= 1000
wikimeasures_df$much_more_prominent_top1000 <- order(wikimeasures_df$much_more_prominent) <= 1000
iffer <- (wikimeasures_df$bfa_influence_rank <= 10 & wikimeasures_df$bfa_prominence_rank <= 10) | # top shots
  (wikimeasures_df$bfa_influence_rank >= 500 & wikimeasures_df$bfa_prominence_rank <= 100) | # prominent guys
  (wikimeasures_df$bfa_influence_rank <=200 & wikimeasures_df$bfa_prominence_rank >= 1000) | # influential guys
  (wikimeasures_df$much_more_prominent > 0.09 & wikimeasures_df$bfa_prominence_rank <= 200 & wikimeasures_df$bfa_influence_rank >= 2000) | 
  (wikimeasures_df$much_more_central > 8  & wikimeasures_df$bfa_influence_rank <= 1000 & wikimeasures_df$bfa_prominence_rank <= 10000) 
wikimeasures_df$plot_label[iffer == TRUE]
wikimeasures_df$plot_label[iffer != TRUE] <- ""


library(ggrepel)

# plot 
ggplot(wikimeasures_df, aes(bfa_prominence, bfa_influence)) +
  geom_point(aes(color=log(much_more_prominent))) + 
  scale_color_gradient2(low=rgb(.8, 0, 0, .6), mid=rgb(.5, .5, .5, .6), high=rgb(0, 0, .8, .6), guide = FALSE) + 
  geom_text_repel(aes(label = plot_label), size = 3) +
  xlab("Prominence") + 
  ylab("Influence") +
  theme_bw()
dev.off()




