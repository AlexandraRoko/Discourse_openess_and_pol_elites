# '''
# ------------------------------------------------------------------------
# Script name: 06_plotting_figures.R
# Purpose of script: Generate figures visualising the development of the most influential group over time
# Dependencies: 05_diversity_in_parliament_over_time.R
# Author: Alexandra Rottenkolber
# ------------------------------------------------------------------------
# '''

library(ggpubr)
library(ggplot2)
library(dplyr)
source("./4_combined_data/helpers.R")


#--------- GENDER -------

#novelty
TABLE1 <- table(upper_quant_novelty$year, upper_quant_novelty$sex)
TABLE2 <- prop.table(TABLE1,1)
TABLE3 <- as.data.frame(TABLE2) 
TABLE3 <- subset(TABLE3, TABLE3$Var1 != 1990)
TABLE3$Percent <- TABLE3$Freq*100
TABLE3$Var2 <- as.factor(as.character(TABLE3$Var2)) 
TABLE3$Var2 <- relevel(TABLE3$Var2,"male")

plot_gender_novelty_upper_quartile <- ggplot(TABLE3, aes(fill=Var2, y=Freq, x=Var1)) + 
  geom_bar(position="fill", stat="identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual(name="Gender", 
                    labels = c( "male", "female"), 
                    values = alpha(c("male"=colour_sheme_gender[3],"female"=colour_sheme_gender[4]),  .9)) +# + 
  labs(title="Gender shares among speakers \nin the highest qartile for novelty",
       subtitle = "Anually demeaned data",
       y="Gender share [%]",
       x = "Year") #,
  #caption="Source: economics")
plot_gender_novelty_upper_quartile

#resonance -- upper quantile
GENDERTABLE <- table(upper_quant_resonance$year, upper_quant_resonance$sex) %>% 
  prop.table(1) %>% 
  as.data.frame() %>% 
  subset(Var1 != 1990) 
GENDERTABLE$Var2 <- relevel(as.factor(as.character(GENDERTABLE$Var2)),"male")
GENDERTABLE$Percent <- GENDERTABLE$Freq*100

plot_gender_resonance_upper_quartile <- ggplot(GENDERTABLE, aes(fill=Var2, y=Freq, x=Var1)) + 
  geom_bar(position="fill", stat="identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual(name="Gender", 
                    labels = c("female", "male"), 
                    values = alpha(c("female"="#52BE80", "male"="#1F618D"), .9)) +# + 
  labs(title="Gender shares among speakers \nin the highest qartile for resonance",
       subtitle = "Anually demeaned data",
       y="Gender share",
       x = "Year")
plot_gender_resonance_upper_quartile

#resonance -- upper 20 percentile
GENDERTABLE <- table(upper_perc_resonance$year, upper_perc_resonance$sex) %>% 
  prop.table(1) %>% 
  as.data.frame() %>% 
  subset(Var1 != 1990)
GENDERTABLE$Var2 <- relevel(as.factor(as.character(GENDERTABLE$Var2)),"male")
GENDERTABLE$Percent <- GENDERTABLE$Freq*100


plot_gender_resonance_20_percentile <- ggplot(GENDERTABLE, aes(fill=Var2, y=Freq, x=Var1)) + 
  theme_minimal() +
  geom_bar(position="fill", stat="identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual(name="Gender", 
                    labels = c("female", "male"), 
                    values = alpha(c("female"="#52BE80", "male"="#1F618D"), .9)) +# + 
  scale_x_discrete(breaks = c(1991, 1993, 1995, 1997, 1999, 2001, 2003, 2005, 2007, 
                              2009, 2011, 2013, 2015, 2017, 
                              2019, 2021), labels = c("1991", "1993", "1995", "1997", "1999", "2001", "2003", "2005", "2007", 
                                                      "2009", "2011", "2013", "2015", "2017", 
                                                      "2019", "2021")) +
  labs(title="Gender shares among speakers \nin the highest 20 percentiles for resonance",
       subtitle = "Anually demeaned data",
       y="Gender share",
       x = "Year")
plot_gender_resonance_20_percentile


#resonance -- top 50 speeches
GENDERTABLE <- table(top50speaker_resonance_by_speeches_meta$year, top50speaker_resonance_by_speeches_meta$sex) %>% 
  prop.table(1) %>% 
  as.data.frame() %>% 
  subset(Var1 != 1990)
GENDERTABLE$Var2 <- relevel(as.factor(as.character(GENDERTABLE$Var2)),"male")
GENDERTABLE$Percent <- GENDERTABLE$Freq*100

plot_gender_resonance_top_50_speeches <- ggplot(GENDERTABLE, aes(fill=Var2, y=Freq, x=Var1)) + 
  theme_minimal() +
  geom_bar(position="fill", stat="identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual(name="Gender", 
                    labels = c("female", "male"), 
                    values = alpha(c("female"="#52BE80", "male"="#1F618D"), .9)) +# + 
  scale_x_discrete(breaks = c(1991, 1993, 1995, 1997, 1999, 2001, 2003, 2005, 2007, 
                              2009, 2011, 2013, 2015, 2017, 
                              2019, 2021), labels = c("1991", "1993", "1995", "1997", "1999", "2001", "2003", "2005", "2007", 
                                                      "2009", "2011", "2013", "2015", "2017", 
                                                      "2019", "2021")) +
  labs(title="Gender shares among speakers \nwho gave the top 50 speeches in terms of resonance",
       subtitle = "Anually demeaned data",
       y="Gender share",
       x = "Year")
plot_gender_resonance_top_50_speeches


#resonance -- top 50 speakers
GENDERTABLE <- table(top50speaker_by_mean_resonance_meta$year, top50speaker_by_mean_resonance_meta$sex) %>% 
  prop.table(1) %>% 
  as.data.frame() %>% 
  subset(Var1 != 1990)
GENDERTABLE$Var2 <- relevel(as.factor(as.character(GENDERTABLE$Var2)),"male")
GENDERTABLE$Percent <- GENDERTABLE$Freq*100

plot_gender_resonance_top_50_speakers <- ggplot(GENDERTABLE, aes(fill=Var2, y=Freq, x=Var1)) + 
  theme_minimal() +
  geom_bar(position="fill", stat="identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual(name="Gender", 
                    labels = c("female", "male"), 
                    values = alpha(c("female"="#52BE80", "male"="#196F3D"), .9)) +# + 
  scale_x_discrete(breaks = c(1991, 1993, 1995, 1997, 1999, 2001, 2003, 2005, 2007, 
                              2009, 2011, 2013, 2015, 2017, 
                              2019, 2021), labels = c("1991", "1993", "1995", "1997", "1999", "2001", "2003", "2005", "2007", 
                                                      "2009", "2011", "2013", "2015", "2017", 
                                                      "2019", "2021")) +
  labs(title="Gender shares among top 50 speakers \n with the on average highest resonance",
       subtitle = "Anually demeaned data, yearly average",
       y="Gender share",
       x = "Year") 
plot_gender_resonance_top_50_speakers


#Gender share in parliament 
GENDERTABLE_ov <- table(OVERALL_TREND$year, OVERALL_TREND$sex) %>% 
  prop.table(1) %>% 
  as.data.frame() %>% 
  subset(Var1 != 1990)
GENDERTABLE_ov$Var2 <- relevel(as.factor(as.character(GENDERTABLE_ov$Var2)),"male")
GENDERTABLE_ov$Percent <- GENDERTABLE_ov$Freq*100


plot_overall_gender_share <- ggplot(GENDERTABLE_ov, aes(fill=Var2, y=Freq, x=Var1)) + 
  theme_minimal() +
  geom_bar(position="fill", stat="identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual(name="Gender", 
                    labels = c("female", "male"), 
                    values = alpha(c("female"="#52BE80", "male"="#1F618D"), .9)) +# + 
  scale_x_discrete(breaks = c(1991, 1993, 1995, 1997, 1999, 2001, 2003, 2005, 2007, 
                              2009, 2011, 2013, 2015, 2017, 
                              2019, 2021), labels = c("1991", "1993", "1995", "1997", "1999", "2001", "2003", "2005", "2007", 
                                                      "2009", "2011", "2013", "2015", "2017", 
                                                      "2019", "2021")) +
  labs(title="Gender share in parliament -- Overall trend",
       subtitle = "--",
       y="Gender share",
       x = "Year")
plot_overall_gender_share


#novelty -- top 50 speakers & overall trend
GENDERTABLE <- table(top50speaker_by_mean_novelty_meta$year, top50speaker_by_mean_novelty_meta$sex) %>% 
  prop.table(1) %>% 
  as.data.frame() %>% 
  subset(Var1 != 1990)
GENDERTABLE$Var2 <- relevel(as.factor(as.character(GENDERTABLE$Var2)),"male")
GENDERTABLE$Percent <- GENDERTABLE$Freq*100
GENDERTABLE <- rename(GENDERTABLE, Freq_spec = Freq, Percent_spec = Percent )


plot_gender_novelty_top_50_speakers <- ggplot() + 
  theme_minimal() +
  geom_bar(aes(y=Freq_spec, fill=Var2, x=Var1), position="fill", stat="identity", alpha = 0.9, data = GENDERTABLE) +
  geom_point(aes(y=Freq_ov, x=Var1),alpha = 1, shape = "-", colour = "black", size = 0.5, data=GENDERTABLE_ov2) +
  geom_hpline(aes(y=Freq_ov, x=Var1), alpha = 1, width = 0.9, colour = "black", size = 0.5, linetype = 1, data=GENDERTABLE_ov2) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual(name="Gender", 
                    labels = c("Share in \nparliament overall", "female", "male"), 
                    values = alpha(c("white", "female"="#3498DB", "male"="#1F618D"), .9)) +# + 
  #scale_colour_manual(values = "Freq_ov") +
  scale_x_discrete(breaks = c(1991, 1993, 1995, 1997, 1999, 2001, 2003, 2005, 2007, 
                              2009, 2011, 2013, 2015, 2017, 
                              2019, 2021), labels = c("1991", "1993", "1995", "1997", "1999", "2001", "2003", "2005", "2007", 
                                                      "2009", "2011", "2013", "2015", "2017", 
                                                      "2019", "2021")) +
  labs(title="Gender shares among the top 50 speakers \nyielding on average the highest novelty scores",
       subtitle = "Anually demeaned data, yearly average",
       y="Gender share",
       x = "Year") 
plot_gender_novelty_top_50_speakers


#resonance -- top 50 speakers & overall trend
GENDERTABLE <- table(top50speaker_by_mean_resonance_meta$year, top50speaker_by_mean_resonance_meta$sex) %>% 
  prop.table(1) %>% 
  as.data.frame() %>% 
  subset(Var1 != 1990)
GENDERTABLE$Var2 <- relevel(as.factor(as.character(GENDERTABLE$Var2)),"male")
GENDERTABLE$Percent <- GENDERTABLE$Freq*100
GENDERTABLE <- dplyr::rename(GENDERTABLE, Freq_spec = Freq, Percent_spec = Percent )
GENDERTABLE_ov <- dplyr::rename(GENDERTABLE_ov, Freq_ov = Freq, Percent_ov = Percent)
GENDERTABLE_ov2 <- subset(GENDERTABLE_ov, GENDERTABLE_ov$Var2=="female")

plot_gender_resonance_top_50_speakers <- ggplot() + 
  theme_minimal() +
  geom_bar(aes(y=Freq_spec, fill=Var2, x=Var1), position="fill", stat="identity", alpha = 0.9, data = GENDERTABLE) +
  geom_point(aes(y=Freq_ov, x=Var1),alpha = 1, shape = "-", colour = "black", size = 0.5, data=GENDERTABLE_ov2) +
  geom_hpline(aes(y=Freq_ov, x=Var1), alpha = 1, width = 0.9, colour = "black", size = 0.5, linetype = 1, data=GENDERTABLE_ov2) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual(name="Gender", 
                    labels = c("Share in \nparliament overall", "female", "male"), 
                    values = alpha(c("white", "female"="#52BE80", "male"="#145A32"), .9)) +# + 
  #scale_colour_manual(values = "Freq_ov") +
  scale_x_discrete(breaks = c(1991, 1993, 1995, 1997, 1999, 2001, 2003, 2005, 2007, 
                              2009, 2011, 2013, 2015, 2017, 
                              2019, 2021), labels = c("1991", "1993", "1995", "1997", "1999", "2001", "2003", "2005", "2007", 
                                                      "2009", "2011", "2013", "2015", "2017", 
                                                      "2019", "2021")) +
  labs(title="Gender shares among the top 50 speakers \nreceiving on average the highest resonance",
       subtitle = "Anually demeaned data, yearly average",
       y="Gender share",
       x = "Year") 
plot_gender_resonance_top_50_speakers

#novelty -- top 50 speakers by speech & overall trend
GENDERTABLE <- table(top50speaker_novelty_by_speeches_meta$year, top50speaker_novelty_by_speeches_meta$sex) %>% 
  prop.table(1) %>% 
  as.data.frame() %>% 
  subset(Var1 != 1990)
GENDERTABLE$Var2 <- relevel(as.factor(as.character(GENDERTABLE$Var2)),"male")
GENDERTABLE$Percent <- GENDERTABLE$Freq*100
GENDERTABLE <- rename(GENDERTABLE, Freq_spec = Freq, Percent_spec = Percent )
GENDERTABLE_ov <- rename(GENDERTABLE_ov, Freq_ov = Freq, Percent_ov = Percent)
GENDERTABLE_ov2 <- subset(GENDERTABLE_ov, GENDERTABLE_ov$Var2=="female")


plot_gender_novelty_top_50_speakers_by_speech <- ggplot() + 
  theme_minimal() +
  geom_bar(aes(y=Freq_spec, fill=Var2, x=Var1), position="fill", stat="identity", alpha = 0.9, data = GENDERTABLE) +
  geom_point(aes(y=Freq_ov, x=Var1),alpha = 1, shape = "-", colour = "black", size = 0.5, data=GENDERTABLE_ov2) +
  geom_hpline(aes(y=Freq_ov, x=Var1), alpha = 1, width = 0.9, colour = "black", size = 0.5, linetype = 1, data=GENDERTABLE_ov2) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual(name="Gender", 
                    labels = c("Share in \nparliament overall", "female", "male"), 
                    values = alpha(c("white", "female"="#3498DB", "male"="#1F618D"), .9)) +# + 
  #scale_colour_manual(values = "Freq_ov") +
  scale_x_discrete(breaks = c(1991, 1993, 1995, 1997, 1999, 2001, 2003, 2005, 2007, 
                              2009, 2011, 2013, 2015, 2017, 
                              2019, 2021), labels = c("1991", "1993", "1995", "1997", "1999", "2001", "2003", "2005", "2007", 
                                                      "2009", "2011", "2013", "2015", "2017", 
                                                      "2019", "2021")) +
  labs(title="Gender shares among the top 50 speakers \nyielding highest novelty by speech",
       subtitle = "Anually demeaned data",
       y="Gender share",
       x = "Year") 
plot_gender_novelty_top_50_speakers_by_speech


#resonance -- top 50 speakers by speech & overall trend
GENDERTABLE <- table(top50speaker_resonance_by_speeches_meta$year, top50speaker_resonance_by_speeches_meta$sex) %>% 
  prop.table(1) %>% 
  as.data.frame() %>% 
  subset(Var1 != 1990)
GENDERTABLE$Var2 <- relevel(as.factor(as.character(GENDERTABLE$Var2)),"male")
GENDERTABLE$Percent <- GENDERTABLE$Freq*100
GENDERTABLE <- rename(GENDERTABLE, Freq_spec = Freq, Percent_spec = Percent )
GENDERTABLE_ov <- rename(GENDERTABLE_ov, Freq_ov = Freq, Percent_ov = Percent)
GENDERTABLE_ov2 <- subset(GENDERTABLE_ov, GENDERTABLE_ov$Var2=="female")


plot_gender_resonance_top_50_speakers_by_speech <- ggplot() + 
  theme_minimal() +
  geom_bar(aes(y=Freq_spec, fill=Var2, x=Var1), position="fill", stat="identity", alpha = 0.9, data = GENDERTABLE) +
  geom_point(aes(y=Freq_ov, x=Var1),alpha = 1, shape = "-", colour = "black", size = 0.5, data=GENDERTABLE_ov2) +
  geom_hpline(aes(y=Freq_ov, x=Var1), alpha = 1, width = 0.9, colour = "black", size = 0.5, linetype = 1, data=GENDERTABLE_ov2) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual(name="Gender", 
                    labels = c("Share in \nparliament overall", "female", "male"), 
                    values = alpha(c("white", "female"="#52BE80", "male"="#145A32"), .9)) +# + 
  scale_x_discrete(breaks = c(1991, 1993, 1995, 1997, 1999, 2001, 2003, 2005, 2007, 
                              2009, 2011, 2013, 2015, 2017, 
                              2019, 2021), labels = c("1991", "1993", "1995", "1997", "1999", "2001", "2003", "2005", "2007", 
                                                      "2009", "2011", "2013", "2015", "2017", 
                                                      "2019", "2021")) +
  #scale_colour_manual(values = "Freq_ov") +
  labs(title="Gender shares among the top 50 speakers \nreceiving highest resonance by speech",
       subtitle = "Anually demeaned data",
       y="Gender share",
       x = "Year") 
plot_gender_resonance_top_50_speakers_by_speech

ggarrange(plot_gender_novelty_upper_quartile,
          plot_gender_resonance_upper_quartile,
          plot_gender_resonance_20_percentile,
          plot_gender_resonance_top_50_speeches,
          plot_gender_resonance_top_50_speakers,
          plot_overall_gender_share, 
          labels = c("A", "B", "C", "D", "E", "F"),
          ncol = 2, nrow = 3)


ggarrange(plot_gender_novelty_top_50_speakers_by_speech, 
          plot_gender_resonance_top_50_speakers_by_speech,
          plot_gender_novelty_top_50_speakers,
          plot_gender_resonance_top_50_speakers,
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)

ggsave("./03_figures/top50_gender_differences2_3_MPs_only.jpeg", units="in", width=10, height=7, dpi=350)

cowplot::plot_grid(plot_gender_novelty_top_50_speakers_by_speech,
                   plot_gender_novelty_top_50_speakers + 
                     theme(axis.text.y = element_blank(),
                           axis.ticks.y = element_blank(),
                           axis.title.y = element_blank() ), 
                   plot_gender_resonance_top_50_speakers_by_speech + 
                     theme(axis.text.y = element_blank(),
                           axis.ticks.y = element_blank(),
                           axis.title.y = element_blank() ),
                   plot_gender_resonance_top_50_speakers + 
                     theme(axis.text.y = element_blank(),
                           axis.ticks.y = element_blank(),
                           axis.title.y = element_blank() ),
                   nrow = 2,
                   labels = "auto",
                   align = "v")

#ggsave("./03_figures/top50_gender_differences.jpeg", units="in", width=8, height=4, dpi=350)

#----- Ethnicity 
#ETHN share in parliament --- overall trend
ETHNTABLE_ov <- table(OVERALL_TREND$year, OVERALL_TREND$ETHN) %>% 
  prop.table(1) %>% 
  as.data.frame() %>% 
  subset(Var1 != 1990)
#ETHNTABLE_ov$Var2 <- relevel(as.factor(as.character(ETHNTABLE_ov$Var2)),"male")
ETHNTABLE_ov$Percent <- ETHNTABLE_ov$Freq*100


plot_overall_ETHN_share <- ggplot(ETHNTABLE_ov, aes(fill=Var2, y=Freq, x=Var1)) + 
  geom_bar(position="fill", stat="identity") + 
  theme_bw() +
  #theme_minimal() +
  scale_x_discrete(breaks = c(1991, 1995, 2000, 2005, 2010, 2015, 2020), labels = c("1991", "1995", "2000", "2005", "2010", "2015", "2020")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual(name="Ethnicity",
                    labels = c("asian", "black", "not indicated", "white"),
                    values = alpha(c("asian"="#52BE80", "black"= "#F39C12", "not indicated" = "#AAB7B8", "white" = "#1F618D"), .5)) +# +
  labs(title="Ethnical background of Members of Parliament",
       #subtitle = "Anually demeaned data",
       y="Share",
       x = "Year") +
  geom_col() +
  facet_zoom2(ylim = c(0.95, 1))
plot_overall_ETHN_share

#ggsave("./03_figures/Overall_ethnicity.jpeg", units="in", width=8, height=4, dpi=350)


ETHNTABLE <- table(top50speaker_resonance_by_speeches_meta$year, top50speaker_resonance_by_speeches_meta$ETHN) %>% 
  prop.table(1) %>% 
  as.data.frame() %>% 
  subset(Var1 != 1990)
ETHNTABLE$Percent <- ETHNTABLE$Freq*100
ETHNTABLE_ov <- dplyr::rename(ETHNTABLE_ov, Freq_ov = Freq, Percent_ov = Percent)
ETHNTABLE_ov1 <- subset(ETHNTABLE_ov, ETHNTABLE_ov$Var2=="asian" | ETHNTABLE_ov$Var2=="black" | ETHNTABLE_ov$Var2=="not indicated" | ETHNTABLE_ov$Var2=="white") %>% 
  dplyr::group_by(Var1) %>%
  dplyr::mutate(Freq_ov_sum = sum(Freq_ov)) #%>%
  #filter(Var2 == "asian")
ETHNTABLE_ov2 <- subset(ETHNTABLE_ov, ETHNTABLE_ov$Var2=="black" | ETHNTABLE_ov$Var2=="not indicated" | ETHNTABLE_ov$Var2=="white") %>% 
  dplyr::group_by(Var1) %>%
  dplyr::mutate(Freq_ov_sum = sum(Freq_ov)) %>%
  dplyr::filter(Var2 == "black")
ETHNTABLE_ov3 <- subset(ETHNTABLE_ov, ETHNTABLE_ov$Var2=="not indicated" | ETHNTABLE_ov$Var2=="white") %>% 
  dplyr::group_by(Var1) %>%
  dplyr::mutate(Freq_ov_sum = sum(Freq_ov)) %>%
  dplyr::filter(Var2 == "not indicated")
  #summarise(Freq_ov_sum = rowSums(Freq_ov), Var1 = Var1, Var2 = "not indicated")
ETHNTABLE_ov4 <- subset(ETHNTABLE_ov, ETHNTABLE_ov$Var2=="white")


# Stacked + percent
require(ggplot2)
plot_top50speaker_resonance_by_speeches_metaETHN <- ggplot(ETHNTABLE, aes(fill=Var2, y=Freq, x=Var1)) + 
  geom_bar(position="fill", stat="identity") + 
  geom_hpline(aes(y=Freq_ov, x=Var1), alpha = 1, width = 0.9, colour = "#1F618D", size = 0.5, linetype = 1, data=ETHNTABLE_ov4) +
  geom_hpline(aes(y=Freq_ov_sum, x=Var1), alpha = 0.8, width = 0.9, colour = "black", size = 0.5, linetype = 1, data=ETHNTABLE_ov3) +
  geom_hpline(aes(y=Freq_ov_sum, x=Var1), alpha = 1, width = 0.9, colour = "#F39C12", size = 0.5, linetype = 1, data=ETHNTABLE_ov2) +
  #geom_hpline(aes(y=Freq_ov_sum, x=Var1), alpha = 1, width = 0.9, colour = "#52BE80", size = 0.5, linetype = 1, data=ETHNTABLE_ov1) +
  theme_bw() +
  scale_x_discrete(breaks = c(1991, 1995, 2000, 2005, 2010, 2015, 2020), labels = c("1991", "1995", "2000", "2005", "2010", "2015", "2020")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual(name="Ethnicity",
                    labels = c("asian", "black", "not indicated", "white"),
                    values = alpha(c("asian"="#52BE80", "black"= "#F39C12", "not indicated" = "#AAB7B8", "white" = "#1F618D"), .5)) +# +
  labs(title="Ethnicity shares among speakers receiving the on average highest resonance scores",
       subtitle = "Anually demeaned data",
       y="Share",
       x = "Year") +
  geom_col() +
  facet_zoom2(ylim = c(0.95, 1))
plot_top50speaker_resonance_by_speeches_metaETHN



ETHNTABLE <- table(top50speaker_by_mean_resonance_meta$year, top50speaker_by_mean_resonance_meta$ETHN) %>% 
  prop.table(1) %>% 
  as.data.frame() %>% 
  subset(Var1 != 1990)
ETHNTABLE$Percent <- ETHNTABLE$Freq*100

# Stacked + percent
require(ggplot2)
plot_top50speaker_by_mean_resonance_metaETHN <- ggplot(ETHNTABLE, aes(fill=Var2, y=Freq, x=Var1)) + 
  geom_bar(position="fill", stat="identity") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual(name="Ethnicity",
                    labels = c("asian", "black", "not indicated", "white"),
                    values = alpha(c("asian"="#52BE80", "black"= "#F39C12", "not indicated" = "#AAB7B8", "white" = "#1F618D"), .5)) +# +
  labs(title="Ethnicity shares among speakers -----",
       subtitle = "Anually demeaned data",
       y="Share",
       x = "Year") +
  geom_col() +
  facet_zoom2(ylim = c(0.95, 1))
plot_top50speaker_by_mean_resonance_metaETHN




ETHNTABLE <- table(top50speaker_novelty_by_speeches_meta$year, top50speaker_novelty_by_speeches_meta$ETHN) %>% 
  prop.table(1) %>% 
  as.data.frame() %>% 
  subset(Var1 != 1990)
ETHNTABLE$Percent <- ETHNTABLE$Freq*100

# Stacked + percent
require(ggplot2)
plot_top50speaker_novelty_by_speeches_metaETHN <- ggplot(ETHNTABLE, aes(fill=Var2, y=Freq, x=Var1)) + 
  geom_bar(position="fill", stat="identity") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual(name="Ethnicity",
                    labels = c("asian", "black", "not indicated", "white"),
                    values = alpha(c("asian"="#52BE80", "black"= "#F39C12", "not indicated" = "#AAB7B8", "white" = "#1F618D"), .5)) +# +
  labs(title="Ethnicity shares among speakers -----",
       subtitle = "Anually demeaned data",
       y="Share",
       x = "Year") +
  geom_col() +
  facet_zoom2(ylim = c(0.95, 1))
plot_top50speaker_novelty_by_speeches_metaETHN



ETHNTABLE <- table(top50speaker_by_mean_novelty_meta$year, top50speaker_by_mean_novelty_meta$ETHN) %>% 
  prop.table(1) %>% 
  as.data.frame() %>% 
  subset(Var1 != 1990)
ETHNTABLE$Percent <- ETHNTABLE$Freq*100

# Stacked + percent
require(ggplot2)
plot_top50speaker_by_mean_novelty_metaETHN <- ggplot(ETHNTABLE, aes(fill=Var2, y=Freq, x=Var1)) + 
  geom_bar(position="fill", stat="identity") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual(name="Ethnicity",
                    labels = c("asian", "black", "not indicated", "white"),
                    values = alpha(c("asian"="#52BE80", "black"= "#F39C12", "not indicated" = "#AAB7B8", "white" = "#1F618D"), .5)) +# +
  labs(title="Ethnicity shares among speakers -----",
       subtitle = "Anually demeaned data",
       y="Share",
       x = "Year") +
  geom_col() +
  facet_zoom2(ylim = c(0.95, 1))
plot_top50speaker_by_mean_novelty_metaETHN



#### ---- religion 

RELIGIONTABLE <- table(upper_quant_resonance$year, upper_quant_resonance$religion) %>% 
  prop.table(1) %>% 
  as.data.frame() %>% 
  subset(Var1 != 1990)
RELIGIONTABLE$Percent <- RELIGIONTABLE$Freq*100

# Stacked + percent
ggplot(RELIGIONTABLE, aes(fill=Var2, y=Freq, x=Var1)) + 
  geom_bar(position="fill", stat="identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  # scale_fill_manual(name="Religion")+#, 
  #                   #labels = c("female", "male"), 
  #                   #values = alpha(c("female"="#52BE80", "male"="#1F618D"), .9)) +# + 
  labs(title="Gender shares among speakers \nin the highest qartile for resonance",
       subtitle = "Anually demeaned data",
       y="Gender share",
       x = "Year")




#Origin share in parliament 
EASTWESTTABLE_ov <- table(OVERALL_TREND$year, OVERALL_TREND$PART_OF_GER) %>% 
  prop.table(1) %>% 
  as.data.frame() %>% 
  subset(Var1 != 1990)
#EASTWESTTABLE_ov$Var2 <- relevel(as.factor(as.character(GENDERTABLE_ov$Var2)),"male")
EASTWESTTABLE_ov$Percent <- EASTWESTTABLE_ov$Freq*100


EASTWESTTABLE <- table(upper_quant_resonance$year, upper_quant_resonance$PART_OF_GER) %>% 
  prop.table(1) %>% 
  as.data.frame() %>% 
  subset(Var1 != 1990, is.na(Var2) == FALSE)
EASTWESTTABLE$Percent <- EASTWESTTABLE$Freq*100

# Stacked + percent
ggplot(EASTWESTTABLE, aes(fill=Var2, y=Freq, x=Var1)) + 
  geom_bar(position="fill", stat="identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  # scale_fill_manual(name="Religion")+#, 
  #                   #labels = c("female", "male"), 
  #                   #values = alpha(c("female"="#52BE80", "male"="#1F618D"), .9)) +# + 
  labs(title="Gender shares among speakers \nin the highest qartile for resonance",
       subtitle = "Anually demeaned data",
       y="Gender share",
       x = "Year")


#resonance -- top 50 speakers & overall trend
EASTWESTTABLE <- table(top50speaker_by_mean_resonance_meta$year, top50speaker_by_mean_resonance_meta$PART_OF_GER) %>% 
  prop.table(1) %>% 
  as.data.frame() %>% 
  subset(Var1 != 1990)
EASTWESTTABLE$Percent <- EASTWESTTABLE$Freq*100
#GENDERTABLE$Var2 <- relevel(as.factor(as.character(GENDERTABLE$Var2)),"male")
EASTWESTTABLE <- dplyr::rename(EASTWESTTABLE, Freq_spec = Freq, Percent_spec = Percent )
EASTWESTTABLE_ov <- dplyr::rename(EASTWESTTABLE_ov, Freq_ov = Freq, Percent_ov = Percent)
EASTWESTTABLE_ov2 <- subset(EASTWESTTABLE_ov, EASTWESTTABLE_ov$Var2=="Neue Bundesländer")


plot_eastwest_resonance_top_50_speakers <- ggplot() + 
  theme_minimal() +
  geom_bar(aes(y=Freq_spec, fill=Var2, x=Var1), position="fill", stat="identity", alpha = 0.9, data = EASTWESTTABLE) +
  geom_point(aes(y=Freq_ov, x=Var1),alpha = 1, shape = "-", colour = "black", size = 0.5, data=EASTWESTTABLE_ov2) +
  geom_hpline(aes(y=Freq_ov, x=Var1), alpha = 1, width = 0.9, colour = "black", size = 0.5, linetype = 1, data=EASTWESTTABLE_ov2) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ 
  scale_x_discrete(breaks = c(1991, 1993, 1995, 1997, 1999, 2001, 2003, 2005, 2007, 
                              2009, 2011, 2013, 2015, 2017, 
                              2019, 2021), labels = c("1991", "1993", "1995", "1997", "1999", "2001", "2003", "2005", "2007", 
                                                      "2009", "2011", "2013", "2015", "2017", 
                                                      "2019", "2021")) +
  scale_fill_manual(name="Origin",
                    labels = c("Share in \nparliament overall", "Neue Bundesländer", "Alte Bundesländer"),
                    values = alpha(c("white", "Neue Bundesländer"="#52BE80", "Alte Bundesländer"="#145A32"), .9)) +# +
  #scale_colour_manual(values = "Freq_ov") +
  labs(title="Shares of origin among the top 50 speakers \nreceiving on average the highest resonance",
       subtitle = "Anually demeaned data, yearly average",
       y="Share",
       x = "Year")
plot_eastwest_resonance_top_50_speakers


#novelty -- top 50 speakers & overall trend
EASTWESTTABLE <- table(top50speaker_by_mean_novelty_meta$year, top50speaker_by_mean_novelty_meta$PART_OF_GER) %>% 
  prop.table(1) %>% 
  as.data.frame() %>% 
  subset(Var1 != 1990)
EASTWESTTABLE$Percent <- EASTWESTTABLE$Freq*100
#GENDERTABLE$Var2 <- relevel(as.factor(as.character(GENDERTABLE$Var2)),"male")
EASTWESTTABLE <- dplyr::rename(EASTWESTTABLE, Freq_spec = Freq, Percent_spec = Percent )
EASTWESTTABLE_ov <- dplyr::rename(EASTWESTTABLE_ov, Freq_ov = Freq, Percent_ov = Percent)
EASTWESTTABLE_ov2 <- subset(EASTWESTTABLE_ov, EASTWESTTABLE_ov$Var2=="Neue Bundesländer")


plot_eastwest_novelty_top_50_speakers <- ggplot() + 
  theme_minimal() +
  geom_bar(aes(y=Freq_spec, fill=Var2, x=Var1), position="fill", stat="identity", alpha = 0.9, data = EASTWESTTABLE) +
  geom_point(aes(y=Freq_ov, x=Var1),alpha = 1, shape = "-", colour = "black", size = 0.5, data=EASTWESTTABLE_ov2) +
  geom_hpline(aes(y=Freq_ov, x=Var1), alpha = 1, width = 0.9, colour = "black", size = 0.5, linetype = 1, data=EASTWESTTABLE_ov2) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ 
  scale_fill_manual(name="Origin",
                  labels = c("Share in \nparliament overall", "Neue Bundesländer", "Alte Bundesländer"),
                  values = alpha(c("white", "Neue Bundesländer"=colour_sheme_gender[3], "Alte Bundesländer"=colour_sheme_gender[4]), .9)) +# +
  scale_x_discrete(breaks = c(1991, 1993, 1995, 1997, 1999, 2001, 2003, 2005, 2007, 
                              2009, 2011, 2013, 2015, 2017, 
                              2019, 2021), labels = c("1991", "1993", "1995", "1997", "1999", "2001", "2003", "2005", "2007", 
                                                      "2009", "2011", "2013", "2015", "2017", 
                                                      "2019", "2021")) +
  #scale_colour_manual(values = "Freq_ov") +
  labs(title="Shares of origin among the top 50 speakers \nreceiving on average the highest novelty",
     subtitle = "Anually demeaned data, yearly average",
     y="Share",
     x = "Year")
plot_eastwest_novelty_top_50_speakers


ggarrange(plot_eastwest_novelty_top_50_speakers,
          plot_eastwest_resonance_top_50_speakers,
          labels = c("A", "B"),
          ncol = 2, nrow = 1)

ggsave("./03_figures/top50_east_west_differences2.jpeg", units="in", width=10, height=3.5, dpi=350)





##### AGE CAT
OVERALL_TREND$WP <- "NA"
OVERALL_TREND$year_cont <- as.numeric(as.character(OVERALL_TREND$year))
OVERALL_TREND$WP[OVERALL_TREND$year_cont > 1990 & OVERALL_TREND$year_cont < 1994] <- 12
OVERALL_TREND$WP[OVERALL_TREND$year_cont > 1994 & OVERALL_TREND$year_cont < 1998] <- 13
OVERALL_TREND$WP[OVERALL_TREND$year_cont > 1998 & OVERALL_TREND$year_cont < 2002] <- 14
OVERALL_TREND$WP[OVERALL_TREND$year_cont > 2002 & OVERALL_TREND$year_cont < 2005] <- 15
OVERALL_TREND$WP[OVERALL_TREND$year_cont > 2005 & OVERALL_TREND$year_cont < 2009] <- 16
OVERALL_TREND$WP[OVERALL_TREND$year_cont > 2009 & OVERALL_TREND$year_cont < 2013] <- 17
OVERALL_TREND$WP[OVERALL_TREND$year_cont > 2013 & OVERALL_TREND$year_cont < 2017] <- 18
OVERALL_TREND$WP[OVERALL_TREND$year_cont > 2017 & OVERALL_TREND$year_cont < 2021] <- 19

OVERALL_TREND$WP <- as.factor(OVERALL_TREND$WP)

OVERALL_TREND_WP <- subset(OVERALL_TREND, OVERALL_TREND$WP != "NA")

OVERALL_TREND_means <- OVERALL_TREND %>%
  filter(year != 1990 & WP != "NA") %>%
  group_by(WP) %>%
  summarise(age.mean = mean(AGE), age.sd = sd(AGE), WP = WP) %>%
  distinct()


top50speaker_by_mean_resonance_meta$year_cont <- as.numeric(as.character(top50speaker_by_mean_resonance_meta$year))
top50speaker_by_mean_resonance_meta_WP <-  top50speaker_by_mean_resonance_meta %>% 
  dplyr::mutate(WP =
                  dplyr::case_when((top50speaker_by_mean_resonance_meta$year_cont > 1990 & top50speaker_by_mean_resonance_meta$year_cont < 1994) ~ 12, 
                                   (top50speaker_by_mean_resonance_meta$year_cont > 1994 & top50speaker_by_mean_resonance_meta$year_cont < 1998) ~ 13, 
                                   (top50speaker_by_mean_resonance_meta$year_cont > 1998 & top50speaker_by_mean_resonance_meta$year_cont < 2002) ~ 14,
                                   (top50speaker_by_mean_resonance_meta$year_cont > 2002 & top50speaker_by_mean_resonance_meta$year_cont < 2005) ~ 15,
                                   (top50speaker_by_mean_resonance_meta$year_cont > 2005 & top50speaker_by_mean_resonance_meta$year_cont < 2009) ~ 16,
                                   (top50speaker_by_mean_resonance_meta$year_cont > 2009 & top50speaker_by_mean_resonance_meta$year_cont < 2013) ~ 17,
                                   (top50speaker_by_mean_resonance_meta$year_cont > 2013 & top50speaker_by_mean_resonance_meta$year_cont < 2017) ~ 18,
                                   (top50speaker_by_mean_resonance_meta$year_cont > 2017 & top50speaker_by_mean_resonance_meta$year_cont < 2021) ~ 19)
                ) %>% 
  subset(WP != "NA")

top50speaker_by_mean_novelty_meta$year_cont <- as.numeric(as.character(top50speaker_by_mean_novelty_meta$year))
top50speaker_by_mean_novelty_meta_WP <-  top50speaker_by_mean_novelty_meta %>% 
  dplyr::mutate(WP =
                  dplyr::case_when((top50speaker_by_mean_novelty_meta$year_cont > 1990 & top50speaker_by_mean_novelty_meta$year_cont < 1994) ~ 12, 
                                   (top50speaker_by_mean_novelty_meta$year_cont > 1994 & top50speaker_by_mean_novelty_meta$year_cont < 1998) ~ 13, 
                                   (top50speaker_by_mean_novelty_meta$year_cont > 1998 & top50speaker_by_mean_novelty_meta$year_cont < 2002) ~ 14,
                                   (top50speaker_by_mean_novelty_meta$year_cont > 2002 & top50speaker_by_mean_novelty_meta$year_cont < 2005) ~ 15,
                                   (top50speaker_by_mean_novelty_meta$year_cont > 2005 & top50speaker_by_mean_novelty_meta$year_cont < 2009) ~ 16,
                                   (top50speaker_by_mean_novelty_meta$year_cont > 2009 & top50speaker_by_mean_novelty_meta$year_cont < 2013) ~ 17,
                                   (top50speaker_by_mean_novelty_meta$year_cont > 2013 & top50speaker_by_mean_novelty_meta$year_cont < 2017) ~ 18,
                                   (top50speaker_by_mean_novelty_meta$year_cont > 2017 & top50speaker_by_mean_novelty_meta$year_cont < 2021) ~ 19)
  ) %>% 
  subset(WP != "NA")

#Origin share in parliament 
AGETABLE_ov <- table(OVERALL_TREND_WP$WP, OVERALL_TREND_WP$AGE_CAT) %>% 
  prop.table(1) %>% 
  as.data.frame() %>% 
  subset(Var1 != 1990)
AGETABLE_ov <- subset(AGETABLE_ov, AGETABLE_ov$Var1 != "NA")
AGETABLE_ov$Var2 <- relevel(as.factor(as.character(AGETABLE_ov$Var2)),"younger 31")
#AGETABLE_ov$Var2 <- relevel(as.factor(as.character(AGETABLE_ov$Var2)),"younger 36")
AGETABLE_ov$Percent <- AGETABLE_ov$Freq*100

colnames(AGETABLE_ov)

# Stacked + percent
ggplot(AGETABLE_ov, aes(fill=Var2, y=Freq, x=Var1)) + 
  geom_bar(position="fill", stat="identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) #+
# scale_fill_manual(name="Religion")+#, 
#                   #labels = c("female", "male"), 
#                   #values = alpha(c("female"="#52BE80", "male"="#1F618D"), .9)) +# + 
# labs(title="Gender shares among speakers \nin the highest qartile for resonance",
#      subtitle = "Anually demeaned data",
#      y="Gender share",
#      x = "Year")


#resonance share
AGETABLE<- table(top50speaker_by_mean_resonance_meta_WP$WP, top50speaker_by_mean_resonance_meta_WP$AGE_CAT) %>% 
  prop.table(1) %>% 
  as.data.frame() %>% 
  subset(Var1 != 1990)
AGETABLE$Var2 <- relevel(as.factor(as.character(AGETABLE$Var2)),"younger 31")
AGETABLE$Percent <- AGETABLE$Freq*100

# Stacked + percent
ggplot(AGETABLE, aes(fill=Var2, y=Freq, x=Var1)) + 
  geom_bar(position="fill", stat="identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) #+
  # scale_fill_manual(name="Religion")+#, 
  #                   #labels = c("female", "male"), 
  #                   #values = alpha(c("female"="#52BE80", "male"="#1F618D"), .9)) +# + 
  # labs(title="Gender shares among speakers \nin the highest qartile for resonance",
  #      subtitle = "Anually demeaned data",
  #      y="Gender share",
  #      x = "Year")


#resonance share
AGETABLE<- table(top50speaker_by_mean_resonance_meta_WP$WP, top50speaker_by_mean_resonance_meta_WP$AGE_CAT) %>% 
  prop.table(1) %>% 
  as.data.frame() %>% 
  subset(Var1 != 1990)
AGETABLE$Var2 <- relevel(as.factor(as.character(AGETABLE$Var2)),"younger 31")
#AGETABLE$Var2 <- relevel(as.factor(as.character(AGETABLE$Var2)),"younger 36")
AGETABLE$Percent <- AGETABLE$Freq*100


# Stacked + percent

AGETABLE_ov_inter <- AGETABLE_ov
#AGETABLE_ov_inter <- rename(AGETABLE_ov_inter, Freq = Freq_ov)
AGETABLE_ov_inter$Var3 <- "NA"
AGETABLE_ov_inter$Var3[AGETABLE_ov_inter$Var2 == "31-40"] <- "31-40_ov"
AGETABLE_ov_inter$Var3[AGETABLE_ov_inter$Var2 == "41-50"] <- "41-50_ov"
AGETABLE_ov_inter$Var3[AGETABLE_ov_inter$Var2 == "51-60"] <- "51-60_ov"
AGETABLE_ov_inter$Var3[AGETABLE_ov_inter$Var2 == "older 60"] <- "older 60_ov"
AGETABLE_ov_inter$Var3[AGETABLE_ov_inter$Var2 == "younger 31"] <- "younger 31_ov"
AGETABLE_ov_inter$Var3 <- as.factor(AGETABLE_ov_inter$Var3)
AGETABLE_ov_inter$Var2 <- NULL
AGETABLE_ov_inter <- rename(AGETABLE_ov_inter, Var2 = Var3)#, Percent = Percent_ov)

AGETABLE_bind <- rbind(AGETABLE_ov_inter, AGETABLE)
AGETABLE_bind$Var2 <- as.factor(AGETABLE_bind$Var2)


AGETABLE_ov_renamed <- rename(AGETABLE_ov, Freq_ov = Freq, Percent_ov = Percent)
AGETABLE_resonance_merged = merge(AGETABLE_ov_renamed, AGETABLE, key = c("Var1", "Var2"))

AGETABLE_resonance_residuals <- AGETABLE_resonance_merged %>%
  mutate(res_res = (Freq - Freq_ov) / Freq_ov) %>%
  summarise(res_res = res_res, Var1 = Var1, Var2 = Var2, Freq_ov = Freq_ov, Freq = Freq)


plot_top50_resonance_age <- AGETABLE_bind %>%
  arrange(Freq) %>%
  mutate(Var2 = factor(Var2, levels=c("younger 31", "younger 31_ov", "31-40", "31-40_ov", "41-50", "41-50_ov", "51-60", "51-60_ov", "older 60", "older 60_ov"))) %>%
  ggplot( aes(fill=Var2, y=Freq, x=Var1)) +
  geom_bar(position="dodge", stat="identity")+
  theme_bw() +
  xlab("") +
  ylim(0, 0.55) +
  scale_x_discrete(breaks = c(12, 13, 14, 15, 16, 17, 18, 19), labels = c("1990-1994", "1994-1998", "1998-2002", "2002-2005", "2005-2009", "2009-2013", "2013-2017", "2017-2021")) +
  scale_fill_manual(name="Age category",
                  labels = c("younger 31", "younger 31 - expected",
                             "31-40", "31-40 - expected",
                             "41-50", "41-50 - expected",
                             "51-60", "51-60 - expected",
                             "older 60", "older 60 - expected"),
                  values = alpha(c("younger 31" = "#F39C12",
                                   "younger 31_ov" = "#FAD7A0",
                                   "31-40" = "#A93226",
                                   "31-40_ov" = "#F5B7B1",
                                   "41-50" = "#633974",
                                   "41-50_ov" = "#E8DAEF",
                                   "51-60" = "#1A5276",
                                   "51-60_ov" = "#A9CCE3",
                                   "older 60" = "#145A32",
                                   "older 60_ov" = "#A9DFBF"), .9)) + #+# +
labs(title="Age shares among speakers in the top 50 speakers for resonance",
     subtitle = "Annually demeand resonance score",
     y="Share",
     x = "Electoral term")
plot_top50_resonance_age


# novelty share
AGETABLE<- table(top50speaker_by_mean_novelty_meta_WP$WP, top50speaker_by_mean_novelty_meta_WP$AGE_CAT) %>% 
  prop.table(1) %>% 
  as.data.frame() %>% 
  subset(Var1 != 1990)
AGETABLE$Var2 <- relevel(as.factor(as.character(AGETABLE$Var2)),"younger 31")
#AGETABLE$Var2 <- relevel(as.factor(as.character(AGETABLE$Var2)),"younger 36")
AGETABLE$Percent <- AGETABLE$Freq*100

AGETABLE_bind <- rbind(AGETABLE_ov_inter, AGETABLE)
AGETABLE_bind$Var2 <- as.factor(AGETABLE_bind$Var2)

AGETABLE_ov_renamed <- rename(AGETABLE_ov, Freq_ov = Freq, Percent_ov = Percent)
AGETABLE_novelty_merged = merge(AGETABLE_ov_renamed, AGETABLE, key = c("Var1", "Var2"))

AGETABLE_novelty_residuals <- AGETABLE_novelty_merged %>%
  mutate(res_res = (Freq - Freq_ov) / Freq_ov) %>%
  summarise(res_res = res_res, Var1 = Var1, Var2 = Var2, Freq_ov = Freq_ov, Freq = Freq)


plot_top50_novelty_age <- AGETABLE_bind %>%
  arrange(Freq) %>%
  mutate(Var2 = factor(Var2, levels=c("younger 31", "younger 31_ov", "31-40", "31-40_ov", "41-50", "41-50_ov", "51-60", "51-60_ov", "older 60", "older 60_ov"))) %>%
  ggplot( aes(fill=Var2, y=Freq, x=Var1)) +
  geom_bar(position="dodge", stat="identity")+
  theme_bw() +
  xlab("") +
  ylim(0, 0.55) +
  scale_x_discrete(breaks = c(12, 13, 14, 15, 16, 17, 18, 19), labels = c("1990-1994", "1994-1998", "1998-2002", "2002-2005", "2005-2009", "2009-2013", "2013-2017", "2017-2021")) +
  scale_fill_manual(name="Age category",
                    labels = c("younger 31", "younger 31 - expected",
                               "31-40", "31-40 - expected",
                               "41-50", "41-50 - expected",
                               "51-60", "51-60 - expected",
                               "older 60", "older 60 - expected"),
                    values = alpha(c("younger 31" = "#F39C12",
                                     "younger 31_ov" = "#FAD7A0",
                                     "31-40" = "#A93226",
                                     "31-40_ov" = "#F5B7B1",
                                     "41-50" = "#633974",
                                     "41-50_ov" = "#E8DAEF",
                                     "51-60" = "#1A5276",
                                     "51-60_ov" = "#A9CCE3",
                                     "older 60" = "#145A32",
                                     "older 60_ov" = "#A9DFBF"), .9)) + #+# +
  labs(title="Age shares among speakers in the top 50 speakers for novelty",
       subtitle = "Annualy demeaned novelty score",
       y="Share",
       x = "Electoral term")
plot_top50_novelty_age



#RESIDUALS

freq(AGETABLE_novelty_residuals$Var2)
AGETABLE_novelty_residuals$Var3 <- "NA"
AGETABLE_novelty_residuals$Var3[AGETABLE_novelty_residuals$Var2 == "younger 31"] <- "younger 31 - novelty"    
AGETABLE_novelty_residuals$Var3[AGETABLE_novelty_residuals$Var2 == "31-40"] <- "31-40 - novelty"
AGETABLE_novelty_residuals$Var3[AGETABLE_novelty_residuals$Var2 == "41-50"] <- "41-50 - novelty"
AGETABLE_novelty_residuals$Var3[AGETABLE_novelty_residuals$Var2 == "51-60"] <- "51-60 - novelty"
AGETABLE_novelty_residuals$Var3[AGETABLE_novelty_residuals$Var2 == "older 60"] <- "older 60 - novelty"
AGETABLE_novelty_residuals$Var2 <- NULL
AGETABLE_novelty_residuals <- rename(AGETABLE_novelty_residuals, Var2 = Var3)

AGETABLE_residuals <- rbind(AGETABLE_novelty_residuals, AGETABLE_resonance_residuals)


plot_top50_resonance_age_res <- AGETABLE_resonance_residuals %>%
  filter(Var2 != "NA", is.na(Var2) != TRUE) %>%
  arrange(Freq) %>%
  mutate(Var2 = factor(Var2, levels=c("younger 31",  "31-40", "41-50", 
                                      "51-60", "older 60"))) %>%
  ggplot( aes(fill=Var2, y=res_res, x=Var1)) +
  geom_bar(position="dodge", stat="identity")+
  theme_bw() +
  xlab("") +
  ylim(-1, 1.1) +
  scale_x_discrete(breaks = c(12, 13, 14, 15, 16, 17, 18, 19), labels = c("1990-1994", "1994-1998", "1998-2002", "2002-2005", "2005-2009", "2009-2013", "2013-2017", "2017-2021")) +
  scale_fill_manual(name="Age category",
                    labels = c("younger 31", 
                               "31-40", 
                               "41-50", 
                               "51-60", 
                               "older 60"),
                    values = alpha(c("younger 31" = "#F39C12",
    
                                     "31-40" = "#A93226",
                                     "41-50" = "#633974",
                                     "51-60" = "#1A5276",
                                     "older 60" = "#145A32"), .9)) + #+# +
  labs(title="Relative differences of age shares for resonance",
       subtitle = "calculated by comparing observed and expected values",
       y="Share",
       x = "Electoral term")
plot_top50_resonance_age_res


plot_top50_novelty_age_res <- AGETABLE_novelty_residuals %>%
  filter(Var2 != "NA", is.na(Var2) != TRUE) %>%
  arrange(Freq) %>%
  mutate(Var2 = factor(Var2, levels=c("younger 31 - novelty", "31-40 - novelty", "41-50 - novelty",
                                      "51-60 - novelty", "older 60 - novelty"))) %>%
  ggplot( aes(fill=Var2, y=res_res, x=Var1)) +
  geom_bar(position="dodge", stat="identity")+
  theme_bw() +
  xlab("") +
  ylim(-1, 1.1) +
  scale_x_discrete(breaks = c(12, 13, 14, 15, 16, 17, 18, 19), labels = c("1990-1994", "1994-1998", "1998-2002", "2002-2005", "2005-2009", "2009-2013", "2013-2017", "2017-2021")) +
  scale_fill_manual(name="Age category",
                    labels = c("younger 31 - novelty", 
                               "31-40 - novelty", 
                               "41-50 - novelty", 
                               "51-60 - novelty", 
                               "older 60 - novelty"), 
                    values = alpha(c("younger 31 - novelty" = "#F39C12",
                                     "31-40 - novelty" = "#A93226",
                                     "41-50 - novelty" = "#633974",
                                     "51-60 - novelty" = "#1A5276",
                                     "older 60 - novelty" = "#145A32"), .9)) + #+# +
  labs(title="Relative differences of age shares for novelty",
       subtitle = "calculated by comparing observed and expected values",
       y="Share",
       x = "Electoral term")
plot_top50_novelty_age_res


plot_top50_age_res_nov_res <- AGETABLE_residuals %>%
  filter(Var2 != "NA", is.na(Var2) != TRUE) %>%
  arrange(Freq) %>%
  mutate(Var2 = factor(Var2, levels=c("younger 31", "younger 31 - novelty", "31-40", "31-40 - novelty", "41-50", "41-50 - novelty", 
                                      "51-60", "51-60 - novelty", "older 60", "older 60 - novelty"))) %>%
  ggplot( aes(fill=Var2, y=res_res, x=Var1)) +
  geom_bar(position="dodge", stat="identity")+
  theme_bw() +
  xlab("") +
  ylim(-1, 1.1) +
  scale_x_discrete(breaks = c(12, 13, 14, 15, 16, 17, 18, 19), labels = c("1990-1994", "1994-1998", "1998-2002", "2002-2005", "2005-2009", "2009-2013", "2013-2017", "2017-2021")) +
  scale_fill_manual(name="Age category",
                    labels = c("younger 31", 
                               "younger 31 - novelty",
                               "31-40", 
                               "31-40 - novelty",
                               "41-50", 
                               "41-50 - novelty",
                               "51-60", 
                               "51-60 - novelty",
                               "older 60", 
                               "older 60 - novelty"),
                    values = alpha(c("younger 31" = "#F39C12",
                                     "younger 31 - novelty" = "#FAD7A0",
                                     "31-40" = "#A93226",
                                     "31-40 - novelty" = "#F5B7B1",
                                     "41-50" = "#633974",
                                     "41-50 - novelty" = "#E8DAEF",
                                     "51-60" = "#1A5276",
                                     "51-60 - novelty" = "#A9CCE3",
                                     "older 60" = "#145A32",
                                     "older 60 - novelty" = "#A9DFBF"), .9)) + #+# +
  labs(title="Relative differences for novelty and resonance",
       subtitle = "calculated by comparing observed and expected values",
       y="Share",
       x = "Electoral term")
plot_top50_age_res_nov_res




ggarrange(plot_top50_novelty_age, 
          plot_top50_resonance_age,
          labels = c("A", "B"),
          common.legend = TRUE, 
          legend="right",
          ncol = 1, nrow = 2)


ggarrange(plot_top50_novelty_age, 
          plot_top50_resonance_age,
          plot_top50_novelty_age_res, 
          plot_top50_resonance_age_res, 
          plot_top50_age_res_nov_res,
          labels = c("A", "B", "C", "D", "E"),
          common.legend = TRUE, 
          legend="right",
          ncol = 1, nrow = 5)


ggarrange(plot_top50_novelty_age, 
          plot_top50_resonance_age,
          plot_top50_novelty_age_res, 
          plot_top50_resonance_age_res, 
          labels = c("A", "B", "C", "D"),
          common.legend = TRUE, 
          legend="right",
          ncol = 1, nrow = 4)


ggsave("./03_figures/top50_age_cat_with_residuals_differences_MPs_only_2.jpeg", units="in", width=8, height=10, dpi=350)




############## appendix ################

AGETABLE <- table(top50speaker_by_mean_resonance_meta_WP$WP, top50speaker_by_mean_resonance_meta_WP$AGE_CAT) %>% 
  prop.table(1) %>% 
  as.data.frame() %>% 
  subset(Var1 != 1990)
AGETABLE$Var2 <- relevel(as.factor(as.character(AGETABLE$Var2)),"younger 31")
AGETABLE$Percent <- AGETABLE$Freq*100
AGETABLE_ov <- dplyr::rename(AGETABLE_ov, Freq_ov = Freq, Percent_ov = Percent)
# AGETABLE_ov1 <- subset(AGETABLE_ov, AGETABLE_ov$Var2=="36-45" | AGETABLE_ov$Var2=="45-55" | AGETABLE_ov$Var2=="56-65"  | AGETABLE_ov$Var2=="66-85") %>% 
#   dplyr::group_by(Var1) %>%
#   dplyr::mutate(Freq_ov_sum = sum(Freq_ov)) #%>%
# #filter(Var2 == "45-55")
# AGETABLE_ov2 <- subset(AGETABLE_ov, AGETABLE_ov$Var2=="45-55" | AGETABLE_ov$Var2=="56-65"  | AGETABLE_ov$Var2=="66-85") %>% 
#   dplyr::group_by(Var1) %>%
#   dplyr::mutate(Freq_ov_sum = sum(Freq_ov)) %>%
#   dplyr::filter(Var2 == "56-65")
# AGETABLE_ov3 <- subset(AGETABLE_ov, AGETABLE_ov$Var2=="56-65"  | AGETABLE_ov$Var2=="66-85") %>% 
#   dplyr::group_by(Var1) %>%
#   dplyr::mutate(Freq_ov_sum = sum(Freq_ov)) %>%
#   dplyr::filter(Var2 == "66-85")
# #summarise(Freq_ov_sum = rowSums(Freq_ov), Var1 = Var1, Var2 = "not indicated")
# AGETABLE_ov4 <- subset(AGETABLE_ov, AGETABLE_ov$Var2=="66-85")
AGETABLE_ov1 <- subset(AGETABLE_ov, AGETABLE_ov$Var2=="31-40" | AGETABLE_ov$Var2=="41-50" | AGETABLE_ov$Var2=="51-60"  | AGETABLE_ov$Var2=="older 60") %>% 
  dplyr::group_by(Var1) %>%
  dplyr::mutate(Freq_ov_sum = sum(Freq_ov)) %>%
filter(Var2 == "31-40")
AGETABLE_ov2 <- subset(AGETABLE_ov, AGETABLE_ov$Var2=="41-50" | AGETABLE_ov$Var2=="51-60"  | AGETABLE_ov$Var2=="older 60") %>% 
  dplyr::group_by(Var1) %>%
  dplyr::mutate(Freq_ov_sum = sum(Freq_ov)) %>%
  dplyr::filter(Var2 == "41-50")
AGETABLE_ov3 <- subset(AGETABLE_ov, AGETABLE_ov$Var2=="51-60"  | AGETABLE_ov$Var2=="older 60") %>% 
  dplyr::group_by(Var1) %>%
  dplyr::mutate(Freq_ov_sum = sum(Freq_ov)) %>%
  dplyr::filter(Var2 == "51-60")
#summarise(Freq_ov_sum = rowSums(Freq_ov), Var1 = Var1, Var2 = "not indicated")
AGETABLE_ov4 <- subset(AGETABLE_ov, AGETABLE_ov$Var2=="older 60")


# Stacked + percent
require(ggplot2)
plot_top50speaker_resonance_by_speeches_meta_AGECAT <- ggplot(AGETABLE, aes(fill=Var2, y=Freq, x=Var1)) + 
  geom_bar(position="fill", stat="identity") + 
  geom_hpline(aes(y=Freq_ov, x=Var1), alpha = 1, width = 0.9, colour = "#1F618D", size = 0.5, linetype = 1, data=AGETABLE_ov4) +
  geom_hpline(aes(y=Freq_ov_sum, x=Var1), alpha = 0.8, width = 0.9, colour = "black", size = 0.5, linetype = 1, data=AGETABLE_ov3) +
  geom_hpline(aes(y=Freq_ov_sum, x=Var1), alpha = 1, width = 0.9, colour = "#F39C12", size = 0.5, linetype = 1, data=AGETABLE_ov2) +
  geom_hpline(aes(y=Freq_ov_sum, x=Var1), alpha = 1, width = 0.9, colour = "#52BE80", size = 0.5, linetype = 1, data=AGETABLE_ov1) +
  theme_bw() 
plot_top50speaker_resonance_by_speeches_meta_AGECAT
