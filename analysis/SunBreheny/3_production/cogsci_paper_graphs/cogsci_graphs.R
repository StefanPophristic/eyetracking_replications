# This file should be cleaned up and integrated into the other analyses
# preferably the csv files should be automatically generated from the other scripts
# and saved in this folder automatically

library(tidyverse)
library(lme4)
library(boot)

# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") 
#1 = orange
# 2 = light blue
# 3 = green
#7 = pink
# 5 = dark blue

theme_set(theme_bw())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#############
#
# Make Graphs for Experiment 1 and 2
#
#############

extraDF <- read_csv("extra_surprisalMeanAndCI.csv")
extraDF <- extraDF %>%
  mutate(experiment = "Exp. 2")
simpleDF <- read_csv("simple_surprisalMeanAndCI.csv")
simpleDF <- simpleDF %>%
  mutate(experiment = "Exp. 1")

df <- bind_rows(simpleDF, extraDF)
df$experiment<- df$experiment%>%
  factor()

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
  scale_color_manual(values=c(cbPalette[3], cbPalette[7])) +
  guides(color=guide_legend("Size")) + 
  ylab("Surprisal") +
  ylim(0, 20) + 
  scale_x_discrete(labels = c("\"all\"", "\"some\"", "number", "no determiner", "other")) + 
  xlab("Determiner")
graphSurprisalDetByNoun

ggsave(filename = "det_surprisal_faceted.pdf", plot = graphSurprisalDetByNoun,
       width = 5, height = 4, device = "pdf")

#############
#
# Make Graphs for Correlation Analysis
#
#############

simpleDF <- read_csv("simple_finalCorrelationDetDF.csv")
simpleDF <- simpleDF %>%
  mutate(experiment = "Exp. 1")
extraDF <- read_csv("extra_finalCorrelationDetDf.csv")
extraDF <- extraDF %>%
  mutate(experiment = "Exp. 2")

dfCorr <- bind_rows(simpleDF, extraDF)
dfCorr$experiment<- dfCorr$experiment%>%
  factor()

dfCorr$condition <- dfCorr$condition %>%
  factor(levels = c('all', 'some', 'number'))


graphSurprisalCorr <- dfCorr %>% 
  ggplot(aes(x=surprisal, y=correlation, color=condition, shape = size,group=1)) + 
  facet_grid(. ~ experiment) +
  geom_point(size = 4) +
  geom_smooth(method="lm",se=F,color="gray80",alpha=.5) +
  scale_color_manual(values=c(cbPalette[3], cbPalette[7], cbPalette[2])) +
  theme(text = element_text(size = 12), 
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.text.x = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.position = "top",
        legend.box= "vertical")

graphSurprisalCorr

ggsave(filename = "surprisal_correlation.pdf", plot = graphSurprisalCorr,
       width = 5, height = 4, device = "pdf")
