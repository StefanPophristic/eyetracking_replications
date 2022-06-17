####
# Linking Hypothesis Analysis
#
# Ryskin et al. 2019 and Incremental study data
#
# ## Include test, train, and filler trials where adjective was used
# That means we excluded the following training trials: 
#   - reliable pragmatic contrast absent condition
#   - unreliable pragmatic contrast present condition
#   - filler trials that in the semantic_contrast condition (i.e. not semantic_control nor contrast_control)
#
#
# note that pragmatic condition and contrast condition will be entirely correlated for 
#   the train and filler trials we chose





library(tidyverse)
library(lme4)
library(stringr)
library(stringi)
library(lmerTest)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("helpers.R")
setwd('../data')
theme_set(theme_bw())

# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") 


# Load in data
df_merged <- read.csv("../data/analysis_by_quarter_data.csv")

df_merged$window <- factor(df_merged$window, levels = c("prior", "adjective", "noun"))

##############
##############
# Plots
##############
##############

##############
# Correlation of Everything
##############


corr_of_everything <- df_merged %>%
  ggplot(aes(x=prop_selections,y=prop_looks,color=Region,shape=pragContext,group=1)) +
  geom_abline(intercept=0,slope=1,color="gray",linetype="dashed") +
  geom_smooth(method="lm") +
  geom_point() +
  scale_color_manual(values=c(cbPalette[7],cbPalette[1],cbPalette[4],cbPalette[5])) +
  labs(
    shape="Contrast condition",
    x="Proportion of selections",
    y="Proportion of looks (Ryskin et al 2019, Exp. 2)") +
  xlim(0,1) +
  ylim(0,1) +
  ggtitle("Overall Correlations")
corr_of_everything
ggsave("../graphs/analysis_by_quarter/corr_everything.pdf", plot = corr_of_everything, width=5.5,height=3.5)


corr_of_everything_by_quarter <- df_merged %>%
  filter(quarter %in% c(1, 4)) %>%
  ggplot(aes(x=prop_selections,y=prop_looks,color=Region,shape=pragContext,group=1)) +
  facet_grid(. ~ quarter,
             labeller = as_labeller(
               c("1" = "Quarter 1", "4" = "Quarter 4"))) + 
  geom_abline(intercept=0,slope=1,color="gray",linetype="dashed") +
  geom_smooth(method="lm") +
  geom_point() +
  scale_color_manual(values=c(cbPalette[7],cbPalette[1],cbPalette[4],cbPalette[5])) +
  labs(
    shape="Contrast condition",
    x="Proportion of selections",
    y="Proportion of looks (Ryskin et al 2019, Exp. 2)") +
  xlim(0,1) +
  ylim(0,1) +
  ggtitle("Overall Correlations")
corr_of_everything_by_quarter
ggsave("../graphs/analysis_by_quarter/corr_of_everything_by_quarter.pdf", plot = corr_of_everything_by_quarter, width=10,height=8)


##############
# Correlation by Window
##############

# Correlation by window collasped across trial type
corr_by_window_all <- df_merged %>%
  ggplot(aes(x=prop_selections,y=prop_looks,color=Region,shape=pragContext,group=1)) +
  geom_abline(intercept=0,slope=1,color="gray",linetype="dashed") +
  geom_smooth(mapping = aes(group = pragContext), method="lm") +
  geom_point() +
  facet_wrap(~window) +
  scale_color_manual(values=c(cbPalette[7],cbPalette[1],cbPalette[4],cbPalette[5])) +
  labs(
    shape="Pragmatic condition",
    # color="Window",
    x="Proportion of selections",
    y="Proportion of looks (Ryskin et al 2019, Exp. 2)") +
  xlim(0,1) +
  ylim(0,1)  +
  ggtitle("Correlations by Window All Trials")
corr_by_window_all
ggsave("../graphs/analysis_by_quarter/corr_by_window_all.pdf", plot = corr_by_window_all, width=6.5,height=3.5)

# Correlation by window collasped across trial types by quarter
corr_by_window_by_quarter <- df_merged %>%
  filter(quarter %in% c(1, 4)) %>%
  ggplot(aes(x=prop_selections,y=prop_looks,color=Region,shape=pragContext,group=1)) +
  geom_abline(intercept=0,slope=1,color="gray",linetype="dashed") +
  geom_smooth(mapping = aes(group = pragContext), method="lm") +
  geom_point() +
  facet_grid(quarter~window) +
  scale_color_manual(values=c(cbPalette[7],cbPalette[1],cbPalette[4],cbPalette[5])) +
  labs(
    shape="Pragmatic condition",
    x="Proportion of selections",
    y="Proportion of looks (Ryskin et al 2019, Exp. 2)") +
  xlim(0,1) +
  ylim(0,1)  +
  ggtitle("Correlations by Window All Trials")
corr_by_window_by_quarter
ggsave("../graphs/analysis_by_quarter/corr_by_window_by_quarter.pdf", plot = corr_by_window_by_quarter, width=6.5,height=3.5)

# By trial type
# Filler
corr_by_window_filler <- df_merged %>%
  filter(quarter %in% c(1, 4) & trialType == "filler") %>%
  ggplot(aes(x=prop_selections,y=prop_looks,color=Region,shape=pragContext,group=1)) +
  geom_abline(intercept=0,slope=1,color="gray",linetype="dashed") +
  geom_smooth(mapping = aes(group = pragContext),
              method="lm") +
  geom_point() +
  facet_grid(quarter~window) +
  scale_color_manual(values=c(cbPalette[7],cbPalette[1],cbPalette[4],cbPalette[5])) +
  theme(text = element_text(size = 12), 
        plot.title = element_text(hjust = 0.5, size = 10),
        axis.text.x = element_text(size = 8),
        legend.text = element_text(size = 8)) + 
  labs(
    shape="Contrast condition",
    x="Proportion of selections",
    y="Proportion of looks (Ryskin et al 2019, Exp. 2)") +
  xlim(0,1) +
  ylim(0,1)  +
  ggtitle("Correlations by Window and quarter for Filler Trials")
corr_by_window_filler
ggsave("../graphs/analysis_by_quarter/corr_by_window_filler.pdf", plot = corr_by_window_filler, width=10,height=8)

# Train
corr_by_window_train <- df_merged %>%
  filter(quarter %in% c(1, 4) & trialType == "train") %>%
  ggplot(aes(x=prop_selections,y=prop_looks,color=Region,shape=pragContext,group=1)) +
  geom_abline(intercept=0,slope=1,color="gray",linetype="dashed") +
  geom_smooth(mapping = aes(group = pragContext),
              method="lm") +
  geom_point() +
  facet_grid(quarter~window) +
  scale_color_manual(values=c(cbPalette[7],cbPalette[1],cbPalette[4],cbPalette[5])) +
  theme(text = element_text(size = 12), 
        plot.title = element_text(hjust = 0.5, size = 10),
        axis.text.x = element_text(size = 8),
        legend.text = element_text(size = 8)) + 
  labs(
    shape="Contrast condition",
    x="Proportion of selections",
    y="Proportion of looks (Ryskin et al 2019, Exp. 2)") +
  xlim(0,1) +
  ylim(0,1)  +
  ggtitle("Correlations by Window and Quarter for Train trials")
corr_by_window_train
ggsave("../graphs/analysis_by_quarter/corr_by_window_train.pdf", plot = corr_by_window_train, width=10,height=8)

# Test
corr_by_window_test <- df_merged %>%
  filter(quarter %in% c(1, 4) & trialType == "test") %>%
  ggplot(aes(x=prop_selections,y=prop_looks,color=Region,shape=pragContext,group=1)) +
  geom_abline(intercept=0,slope=1,color="gray",linetype="dashed") +
  geom_smooth(mapping = aes(group = pragContext),
              method="lm") +
  geom_point() +
  facet_grid(quarter~window) +
  scale_color_manual(values=c(cbPalette[7],cbPalette[1],cbPalette[4],cbPalette[5])) +
  theme(text = element_text(size = 12), 
        plot.title = element_text(hjust = 0.5, size = 10),
        axis.text.x = element_text(size = 8),
        legend.text = element_text(size = 8)) + 
  labs(
    shape="Contrast condition",
    x="Proportion of selections",
    y="Proportion of looks (Ryskin et al 2019, Exp. 2)") +
  xlim(0,1) +
  ylim(0,1)  +
  ggtitle("Correlations by Window and Quarter for Test trials")
corr_by_window_test
ggsave("../graphs/analysis_by_quarter/corr_by_window_test.pdf", plot = corr_by_window_test, width=10,height=8)




##########
# PRIOR WINDOW
##########
corr_prior_test <- df_merged %>%
  filter(quarter %in% c(1, 4) & trialType == "test" & window == "prior") %>%
  ggplot(aes(x=prop_selections,y=prop_looks,color=Region,shape=pragContext,group=1)) +
  geom_abline(intercept=0,slope=1,color="gray",linetype="dashed") +
  geom_smooth(mapping = aes(group = pragContext),
              method="lm") +
  geom_point() +
  facet_grid(cond~quarter) +
  scale_color_manual(values=c(cbPalette[7],cbPalette[1],cbPalette[4],cbPalette[5])) +
  theme(text = element_text(size = 12), 
        plot.title = element_text(hjust = 0.5, size = 10),
        axis.text.x = element_text(size = 8),
        legend.text = element_text(size = 8)) + 
  labs(
    shape="Pragmatic condition",
    x="Proportion of selections",
    y="Proportion of looks (Ryskin et al 2019, Exp. 2)") +
  xlim(0,1) +
  ylim(0,1)  +
  ggtitle("Prior window correlation for test trails")
corr_prior_test
ggsave("../graphs/analysis_by_quarter/corr_prior_test.pdf", plot = corr_prior_test, width=10,height=8)

corr_prior_train <- df_merged %>%
  filter(quarter %in% c(1, 4) & trialType == "train" & window == "prior") %>%
  ggplot(aes(x=prop_selections,y=prop_looks,color=Region,shape=pragContext,group=1)) +
  geom_abline(intercept=0,slope=1,color="gray",linetype="dashed") +
  geom_smooth(mapping = aes(group = pragContext),
              method="lm") +
  geom_point() +
  facet_grid(cond~quarter) +
  scale_color_manual(values=c(cbPalette[7],cbPalette[1],cbPalette[4],cbPalette[5])) +
  theme(text = element_text(size = 12), 
        plot.title = element_text(hjust = 0.5, size = 10),
        axis.text.x = element_text(size = 8),
        legend.text = element_text(size = 8)) + 
  labs(
    shape="Pragmatic condition",
    x="Proportion of selections",
    y="Proportion of looks (Ryskin et al 2019, Exp. 2)") +
  xlim(0,1) +
  ylim(0,1)  +
  ggtitle("Prior window correlation for train trails")
corr_prior_train
ggsave("../graphs/analysis_by_quarter/corr_prior_train.pdf", plot = corr_prior_train, width=10,height=8)

corr_prior_filler <- df_merged %>%
  filter(quarter %in% c(1, 4) & trialType == "filler" & window == "prior") %>%
  ggplot(aes(x=prop_selections,y=prop_looks,color=Region,shape=pragContext,group=1)) +
  geom_abline(intercept=0,slope=1,color="gray",linetype="dashed") +
  geom_smooth(mapping = aes(group = pragContext),
              method="lm") +
  geom_point() +
  facet_grid(cond~quarter) +
  scale_color_manual(values=c(cbPalette[7],cbPalette[1],cbPalette[4],cbPalette[5])) +
  theme(text = element_text(size = 12), 
        plot.title = element_text(hjust = 0.5, size = 10),
        axis.text.x = element_text(size = 8),
        legend.text = element_text(size = 8)) + 
  labs(
    shape="Pragmatic condition",
    x="Proportion of selections",
    y="Proportion of looks (Ryskin et al 2019, Exp. 2)") +
  xlim(0,1) +
  ylim(0,1)  +
  ggtitle("Prior window correlation for Filler trails")
corr_prior_filler
ggsave("../graphs/analysis_by_quarter/corr_prior_filler.pdf", plot = corr_prior_filler, width=10,height=8)

##########
# ADJECTIVE WINDOW
#########

corr_adj_test <- df_merged %>%
  filter(quarter %in% c(1, 4) & window == "adjective" & trialType == "test") %>%
  ggplot(aes(x=prop_selections,y=prop_looks,color=Region,shape=pragContext)) +
  geom_point() +
  geom_smooth(mapping = aes(group = pragContext), method="lm") +
  scale_color_manual(values=c(cbPalette[7],cbPalette[1],cbPalette[4],cbPalette[5])) +
  labs(
    shape="Pragmatic condition",
    x="Proportion of selections",
    y="Proportion of looks (Ryskin et al 2019, Exp. 2)") +
  xlim(0,1) +
  ylim(0,1) +
  facet_grid(cond~quarter)  +
  ggtitle("Adjective Window Correlations for Test trials")
corr_adj_test
ggsave("../graphs/analysis_by_quarter/corr_adj_test.pdf", plot = corr_adj_test, width=10,height=8, units = "in")

# train
corr_adj_train <- df_merged %>%
  filter(quarter %in% c(1, 4) & window == "adjective" & trialType == "train") %>%
  ggplot(aes(x=prop_selections,y=prop_looks,color=Region,shape=pragContext)) +
  geom_point() +
  geom_smooth(mapping = aes(group = pragContext), method="lm") +
  scale_color_manual(values=c(cbPalette[7],cbPalette[1],cbPalette[4],cbPalette[5])) +
  labs(
    shape="Pragmatic condition",
    x="Proportion of selections",
    y="Proportion of looks (Ryskin et al 2019, Exp. 2)") +
  xlim(0,1) +
  ylim(0,1) +
  facet_grid(cond~quarter)  +
  ggtitle("Adjective Window Correlations for Train trials")
corr_adj_train
ggsave("../graphs/analysis_by_quarter/corr_adj_train.pdf", plot = corr_adj_train, width=10,height=8, units = "in")

# Filler
corr_adj_fill <- df_merged %>%
  filter(quarter %in% c(1, 4) & window == "adjective" & trialType == "filler") %>%
  ggplot(aes(x=prop_selections,y=prop_looks,color=Region,shape=pragContext)) +
  geom_point() +
  geom_smooth(mapping = aes(group = pragContext), method="lm") +
  scale_color_manual(values=c(cbPalette[7],cbPalette[1],cbPalette[4],cbPalette[5])) +
  labs(
    shape="Pragmatic condition",
    x="Proportion of selections",
    y="Proportion of looks (Ryskin et al 2019, Exp. 2)") +
  xlim(0,1) +
  ylim(0,1) +
  facet_grid(cond~quarter)  +
  ggtitle("Adjective Window Correlations for Filler trials")
corr_adj_fill
ggsave("../graphs/analysis_by_quarter/corr_adj_fill.pdf", plot = corr_adj_fill, width=10,height=8, units = "in")


##############
##############
# Correlations
##############
##############



# across pragmatic conditions
cors_window_by_trial = df_merged %>% 
  filter(quarter %in% c(1, 4)) %>%
  group_by(quarter, window, trialType) %>% 
  summarize(Correlation=round(cor.test(prop_selections,prop_looks)$estimate,2),P=round(cor.test(prop_selections,prop_looks)$p.value,5))
cors_window_by_trial
# A tibble: 9 × 4
# Groups:   window [3]
# window    trialType Correlation     P
# <fct>     <fct>           <dbl> <dbl>
#   1 prior     filler           0.72     0
# 2 prior     test             0.31     0
# 3 prior     train            0.38     0
# 4 adjective filler          -0.33     0
# 5 adjective test             0.21     0
# 6 adjective train           -0.32     0
# 7 noun      filler           0.62     0
# 8 noun      test             0.87     0
# 9 noun      train            0.28     0


# by pragmatic condition
cors_window_by_trial_condition = df_merged %>% 
  filter(quarter %in% c(1, 4), window == "prior") %>%
  group_by(quarter, window, trialType, cond, pragContext) %>% 
  summarize(Correlation=round(cor.test(prop_selections,prop_looks)$estimate,2),P=round(cor.test(prop_selections,prop_looks)$p.value,5))
cors_window_by_trial_condition
# quarter window trialType pragContext Correlation       P
# <int> <fct>  <fct>     <fct>             <dbl>   <dbl>
# 1       1 prior  filler    contrast    reliable           0.65 0      
# 2       1 prior  filler    no_contrast unreliable         0.74 0      
# 3       1 prior  test      contrast    reliable           0.24 0.0596 
# 4       1 prior  test      contrast    unreliable         0.47 0.00014
# 5       1 prior  test      no_contrast reliable           0.3  0.0215 
# 6       1 prior  test      no_contrast unreliable         0.4  0.00157
# 7       1 prior  train     contrast    reliable           0.49 0      
# 8       1 prior  train     no_contrast unreliable         0.29 0.0012 
# 9       4 prior  filler    contrast    reliable           0.73 0      
# 10       4 prior  filler    no_contrast unreliable         0.75 0      
# 11       4 prior  test      contrast    reliable           0.12 0.351  
# 12       4 prior  test      contrast    unreliable         0.22 0.0950 
# 13       4 prior  test      no_contrast reliable           0.27 0.0441 
# 14       4 prior  test      no_contrast unreliable         0.46 0.0003 
# 15       4 prior  train     contrast    reliable           0.31 0.00065
# 16       4 prior  train     no_contrast unreliable         0.43 0     

# by pragmatic condition
cors_window_by_trial_condition_adj = df_merged %>% 
  filter(quarter %in% c(1, 4), window == "adjective") %>%
  group_by(quarter, window, trialType, cond, pragContext) %>% 
  summarize(Correlation=round(cor.test(prop_selections,prop_looks)$estimate,2),P=round(cor.test(prop_selections,prop_looks)$p.value,5))
cors_window_by_trial_condition_adj
# quarter window    trialType cond        pragContext Correlation       P
# <int> <fct>     <fct>     <fct>       <fct>             <dbl>   <dbl>
# 1       1 adjective filler    contrast    reliable          -0.31 0.00002
# 2       1 adjective filler    no_contrast unreliable        -0.35 0      
# 3       1 adjective test      contrast    reliable           0.56 0      
# 4       1 adjective test      contrast    unreliable         0.12 0.374  
# 5       1 adjective test      no_contrast reliable          -0.02 0.851  
# 6       1 adjective test      no_contrast unreliable         0.14 0.287  
# 7       1 adjective train     contrast    reliable          -0.44 0      
# 8       1 adjective train     no_contrast unreliable        -0.29 0.00112
# 9       4 adjective filler    contrast    reliable          -0.28 0.00014
# 10       4 adjective filler    no_contrast unreliable        -0.37 0      
# 11       4 adjective test      contrast    reliable           0.46 0.00021
# 12       4 adjective test      contrast    unreliable         0.22 0.0853 
# 13       4 adjective test      no_contrast reliable          -0.15 0.278  
# 14       4 adjective test      no_contrast unreliable         0.06 0.676  
# 15       4 adjective train     contrast    reliable          -0.38 0.00002
# 16       4 adjective train     no_contrast unreliable        -0.19 0.0426 