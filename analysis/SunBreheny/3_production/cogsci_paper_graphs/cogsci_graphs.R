# This file should be cleaned up and integrated into the other analyses
# preferably the csv files should be automatically generated from the other scripts
# and saved in this folder automatically

library(tidyverse)
library(lme4)
library(boot)

# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") 
# 1 = orange (light)
# 2 = light blue
# 3 = green
# 4 = yellow
# 5 = dark blue
# 6 = orange (dark)
# 7 = pink


theme_set(theme_bw())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#############
#
# Make Graphs for Experiment 1 and 2
#
#############

extraDF <- read_csv("data/extra_surprisalMeanAndCI.csv")
extraDF <- extraDF %>%
  mutate(experiment = "Exp. 2")
simpleDF <- read_csv("data/simple_surprisalMeanAndCI.csv")
simpleDF <- simpleDF %>%
  mutate(experiment = "Exp. 1")

df <- bind_rows(simpleDF, extraDF)
df$experiment<- df$experiment%>%
  factor()

df <- df %>%
  mutate(detUsed = fct_relevel(detUsed, "all", "some","num","noDet"))

graphSurprisalDetByNoun <- df %>% 
  ungroup() %>% 
  mutate(YMin = mean_surprisal - CI.Low, 
         YMax = mean_surprisal + CI.High) %>% 
  ggplot(aes(y=mean_surprisal, x=detUsed, color = size)) + 
  facet_grid(. ~ experiment) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = YMin, ymax=YMax),width=0.65) + 
  theme(text = element_text(size = 12), 
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 0.9),
        legend.text = element_text(size = 12),
        legend.position ="top") +
  scale_color_manual(values=c(cbPalette[2], cbPalette[7])) +
  guides(color=guide_legend("Size")) + 
  ylab("Surprisal") +
  ylim(0, 14) + 
  scale_x_discrete(labels = c("\"all\"", "\"some\"", "number", "no determiner", "other")) + 
  xlab("Quantifier")
graphSurprisalDetByNoun

ggsave(filename = "graphs/det_surprisal_faceted.pdf", plot = graphSurprisalDetByNoun,
       width = 5, height = 4, device = "pdf")

#############
#
# Make Graphs for Correlation Analysis
#
#############

simpleDF <- read_csv("../3.1_production_simple_practice/main/data/simple_finalCorrelationDetDF.csv")
simpleDF <- simpleDF %>%
  mutate(experiment = "Exp. 1")
extraDF <- read_csv("../3.2_production_extra_practice/main/data/extra_finalCorrelationDetDf.csv")
extraDF <- extraDF %>%
  mutate(experiment = "Exp. 2")

dfCorr <- bind_rows(simpleDF, extraDF)
dfCorr$experiment<- dfCorr$experiment%>%
  factor()

dfCorr$condition <- dfCorr$condition %>%
  factor(levels = c('all', 'some', 'number'))
dfCorr$size <- dfCorr$size %>%
  factor(levels = c('small', 'big'))

graphSurprisalCorr <- dfCorr %>% 
  ggplot(aes(x=surprisal, y=correlation, color=condition, shape = size,group=1)) + 
  facet_grid(. ~ experiment, scales = "free") +
  geom_point(size = 4) +
  geom_smooth(method="lm",se=F,color="gray80",alpha=.5) +
  scale_color_manual(values=c(cbPalette[1], cbPalette[3], cbPalette[5])) +
  theme(text = element_text(size = 12), 
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.text.x = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.position = "top",
        legend.box= "vertical",
        legend.spacing.y = unit(-0.3, 'cm')) +
  guides(color=guide_legend("Quantifier"), shape =guide_legend("Size")) +
  xlab("Surprisal") +
  ylab("Correlation")

graphSurprisalCorr

ggsave(filename = "graphs/surprisal_correlation.pdf", plot = graphSurprisalCorr,
       width = 5, height = 4, device = "pdf")


#############
#
# Make By Quarter Graphs
#
#############

simpleDfByQuarter <- read_csv("../3.1_production_simple_practice/main/data/simple_surprisalMeanAndCI_by_quarter.csv")
simpleDfByQuarter <- simpleDfByQuarter %>%
  mutate(experiment = "Exp. 1")
extraDfByQuarter <- read_csv("../3.2_production_extra_practice/main/data/extra_surprisalMeanAndCI_by_quarter.csv")
extraDfByQuarter <- extraDfByQuarter %>%
  mutate(experiment = "Exp. 2")

dfQuarter <- bind_rows(simpleDfByQuarter, extraDfByQuarter)

dfQuarter$experiment<- dfQuarter$experiment%>%
  factor()

dfQuarter <- dfQuarter %>%
  mutate(detUsed = fct_relevel(detUsed, "all", "some","num","noDet", "other"))

graphSurprisalDetByNounQuarter <- dfQuarter %>% 
  ungroup() %>% 
  mutate(YMin = mean_surprisal - CI.Low, 
         YMax = mean_surprisal + CI.High) %>% 
  ggplot(aes(y=mean_surprisal, x=detUsed, color = size)) + 
  facet_grid(experiment ~ quarter) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = YMin, ymax=YMax),width=0.65) + 
  theme(text = element_text(size = 12), 
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 0.9),
        legend.text = element_text(size = 12),
        legend.position ="top") +
  scale_color_manual(values=c(cbPalette[2], cbPalette[7])) +
  guides(color=guide_legend("Size")) + 
  ylab("Surprisal") +
  ylim(0, 14) + 
  scale_x_discrete(labels = c("\"all\"", "\"some\"", "number", "no determiner", "other")) + 
  xlab("Quantifier")
graphSurprisalDetByNounQuarter

ggsave(filename = "graphs/surprisal_by_quarter.pdf", plot = graphSurprisalDetByNounQuarter,
       width = 5, height = 4, device = "pdf")


########
# By quarter graphs for correlation analysis

simpleDfQ <- read_csv("../3.1_production_simple_practice/main/data/simple_finalCorrelationDetDF_quarter.csv")
simpleDfQ <- simpleDfQ %>%
  mutate(experiment = "Exp. 1")
extraDfQ <- read_csv("../3.2_production_extra_practice/main/data/extra_finalCorrelationDetDf_quarter.csv")
extraDfQ <- extraDfQ %>%
  mutate(experiment = "Exp. 2")

dfCorr <- bind_rows(simpleDfQ, extraDfQ)
dfCorr$experiment<- dfCorr$experiment%>%
  factor()

dfCorr$condition <- dfCorr$condition %>%
  factor(levels = c('all', 'some', 'number'))
dfCorr$size <- dfCorr$size %>%
  factor(levels = c('small', 'big'))

graphSurprisalCorr <- dfCorr %>% 
  ggplot(aes(x=surprisal, y=correlation, color=condition, shape = size,group=1)) + 
  facet_grid(experiment ~ quarter, scales = "free") +
  geom_point(size = 4) +
  geom_smooth(method="lm",se=F,color="gray80",alpha=.5) +
  scale_color_manual(values=c(cbPalette[1], cbPalette[3], cbPalette[5])) +
  theme(text = element_text(size = 12), 
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.text.x = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.position = "top",
        legend.box= "vertical",
        legend.spacing.y = unit(-0.3, 'cm')) +
  guides(color=guide_legend("Quantifier"), shape =guide_legend("Size")) +
  xlab("Surprisal") +
  ylab("Correlation")

graphSurprisalCorr

ggsave(filename = "graphs/surprisal_correlation_quarter.pdf", plot = graphSurprisalCorr,
       width = 8, height = 7, device = "pdf")


