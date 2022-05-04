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

df_merged <- read.csv("../data/analysis_by_item_data.csv")

df_merged$window <- factor(df_merged$window, levels = c("prior", "adjective", "noun"))


##############
##############
# Plots
##############
##############


##############
# Correlation of Everything
##############

df_merged_good <- df_merged %>%
  filter(pragContext == "reliable")

df_merged_bad <- df_merged %>%
  filter(pragContext == "unreliable")

corr_of_everything <- df_merged %>%
  ggplot(aes(x=prop_selections,y=prop_looks,color=Region,shape=pragContext,group=1)) +
  geom_abline(intercept=0,slope=1,color="gray",linetype="dashed") +
  geom_smooth(mapping = aes(group = pragContext),
              method="lm") +
  geom_point() +
  scale_color_manual(values=c(cbPalette[7],cbPalette[1],cbPalette[4],cbPalette[5])) +
  labs(
    shape="Pragmatic condition",
    x="Proportion of selections",
    y="Proportion of looks (Ryskin et al 2019, Exp. 2)") +
  xlim(0,1) +
  ylim(0,1) +
  ggtitle("Overall Correlations")
corr_of_everything
ggsave("../graphs/analysis_by_item/corr_everything.pdf", plot = corr_of_everything, width=5.5,height=3.5)


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
ggsave("../graphs/analysis_by_item/corr_by_window_all.pdf", plot = corr_by_window_all, width=6.5,height=3.5)

# By trial type
corr_by_window_by_trial <- df_merged %>%
  ggplot(aes(x=prop_selections,y=prop_looks,color=Region,shape=pragContext,group=1)) +
  geom_abline(intercept=0,slope=1,color="gray",linetype="dashed") +
  geom_smooth(mapping = aes(group = pragContext), method="lm") +
  geom_point() +
  facet_grid(trialType~window) +
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
  ggtitle("Correlations by Window By Trial Type")
corr_by_window_by_trial

ggsave("../graphs/analysis_by_item/corr_by_window_by_trial.pdf", plot = corr_by_window_by_trial, width=10,height=8)

##############
# Correlation by window and condition
##############

#####
# Collapsed across trial types

# Correlation by window and condition, by contrast condition
corr_by_contrastCondition_all <- df_merged %>%
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
  facet_grid(cond~window)  +
  ggtitle("Correlations by window and condition")
corr_by_contrastCondition_all
ggsave("../graphs/analysis_by_item/corr_by_contrastCondition_all.pdf", plot = corr_by_contrastCondition_all, width=10,height=8, units = "in")

# Correlation by window and condition, by pragmatic condition
corr_by_pragmaticCondition_all <- df_merged %>%
  ggplot(aes(x=prop_selections,y=prop_looks,color=Region,shape=cond)) +
  geom_point() +
  scale_color_manual(values=c(cbPalette[7],cbPalette[1],cbPalette[4],cbPalette[5])) +
  labs(
    shape="Contrast condition",
    x="Proportion of selections",
    y="Proportion of looks (Ryskin et al 2019, Exp. 2)") +
  xlim(0,1) +
  ylim(0,1) +
  facet_grid(pragContext~window)  +
  ggtitle("Correlations by window and condition")
corr_by_pragmaticCondition_all
ggsave("../graphs/analysis_by_item/corr_by_pragmaticCondition_all.pdf", plot = corr_by_pragmaticCondition_all, width=10,height=8, units = "in")


#######
# By trial type

# Correlation by window and condition, for test trials
corr_by_contrastCondition_test <- df_merged %>%
  filter(trialType == "test") %>%
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
  facet_grid(cond~window)  +
  ggtitle("Correlations by window and condition for test trials")
corr_by_contrastCondition_test
ggsave("../graphs/analysis_by_item/corr_by_contrastCondition_test.pdf", plot = corr_by_contrastCondition_test, width=10,height=8, units = "in")

# Correlation by window and condition, by train trials
corr_by_pragmaticCondition_test <- df_merged %>%
  filter(trialType == "test") %>%
  ggplot(aes(x=prop_selections,y=prop_looks,color=Region,shape=cond)) +
  geom_point() +
  scale_color_manual(values=c(cbPalette[7],cbPalette[1],cbPalette[4],cbPalette[5])) +
  labs(
    shape="Contrast condition",
    x="Proportion of selections",
    y="Proportion of looks (Ryskin et al 2019, Exp. 2)") +
  xlim(0,1) +
  ylim(0,1) +
  facet_grid(pragContext~window)  +
  ggtitle("Correlations by window and condition for test trials")
corr_by_pragmaticCondition_test
ggsave("../graphs/analysis_by_item/corr_by_pragmaticCondition_test.pdf", plot = corr_by_pragmaticCondition_test, width=10,height=8, units = "in")



# Correlation by window and condition, for train trials
corr_by_contrastCondition_train <- df_merged %>%
  filter(trialType == "train") %>%
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
  facet_grid(cond~window)  +
  ggtitle("Correlations by window and condition for train trials")
corr_by_contrastCondition_train
ggsave("../graphs/analysis_by_item/corr_by_contrastCondition_train.pdf", plot = corr_by_contrastCondition_train, width=10,height=8, units = "in")

# Correlation by window and condition, by train trials
corr_by_pragmaticCondition_train <- df_merged %>%
  filter(trialType == "train") %>%
  ggplot(aes(x=prop_selections,y=prop_looks,color=Region,shape=cond)) +
  geom_point() +
  geom_smooth(mapping = aes(group = cond), method="lm") +
  scale_color_manual(values=c(cbPalette[7],cbPalette[1],cbPalette[4],cbPalette[5])) +
  labs(
    shape="Contrast condition",
    x="Proportion of selections",
    y="Proportion of looks (Ryskin et al 2019, Exp. 2)") +
  xlim(0,1) +
  ylim(0,1) +
  facet_grid(pragContext~window)  +
  ggtitle("Correlations by window and condition for train trials")
corr_by_pragmaticCondition_train
ggsave("../graphs/analysis_by_item/corr_by_pragmaticCondition_train.pdf", plot = corr_by_pragmaticCondition_train, width=10,height=8, units = "in")



# Correlation by window and condition, for filler trials
corr_by_contrastCondition_filler <- df_merged %>%
  filter(trialType == "filler") %>%
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
  facet_grid(cond~window)  +
  ggtitle("Correlations by window and condition for filler trials")
corr_by_contrastCondition_filler
ggsave("../graphs/analysis_by_item/corr_by_contrastCondition_filler.pdf", plot = corr_by_contrastCondition_filler, width=10,height=8, units = "in")

# Correlation by window and condition, by filler trials
corr_by_pragmaticCondition_filler <- df_merged %>%
  filter(trialType == "filler") %>%
  ggplot(aes(x=prop_selections,y=prop_looks,color=Region,shape=cond)) +
  geom_point() +
  geom_smooth(mapping = aes(group = cond), method="lm") +
  scale_color_manual(values=c(cbPalette[7],cbPalette[1],cbPalette[4],cbPalette[5])) +
  labs(
    shape="Contrast condition",
    x="Proportion of selections",
    y="Proportion of looks (Ryskin et al 2019, Exp. 2)") +
  xlim(0,1) +
  ylim(0,1) +
  facet_grid(pragContext~window)  +
  ggtitle("Correlations by window and condition for filler trials")
corr_by_pragmaticCondition_filler
ggsave("../graphs/analysis_by_item/corr_by_pragmaticCondition_filler.pdf", plot = corr_by_pragmaticCondition_filler, width=10,height=8, units = "in")


##############
##############
# Correlations
##############

# Correlations between eye-movements (looks to an object) and selections (clicks on that object)

# Overall correlation
cor.test(df_merged$prop_selections,df_merged$prop_looks) 
# .410. df=2158, p < 0.0001

######
# Correlations by window

# Correlation by window collapsed across trial type
cors_window_all = df_merged %>% 
  group_by(window) %>% 
  summarize(Correlation=round(cor.test(prop_selections,prop_looks)$estimate,2),P=round(cor.test(prop_selections,prop_looks)$p.value,5))
cors_window_all
#   window    Correlation P
# 1 prior            0.67 0      
# 2 adjective       -0.11 0.00209
# 3 noun             0.65 0    

# Correlation by window by trial type
cors_window_by_trial = df_merged %>% 
  group_by(window, trialType, pragContext) %>% 
  summarize(Correlation=round(cor.test(prop_selections,prop_looks)$estimate,2),P=round(cor.test(prop_selections,prop_looks)$p.value,5))
cors_window_by_trial
# window    trialType pragContext Correlation       P
# 1 prior     filler    reliable           0.84 0      
# 2 prior     filler    unreliable         0.84 0      
# 3 prior     test      reliable           0.62 0      
# 4 prior     test      unreliable         0.76 0      
# 5 prior     train     reliable           0.73 0      
# 6 prior     train     unreliable         0.65 0      
# 7 adjective filler    reliable          -0.38 0      
# 8 adjective filler    unreliable        -0.49 0      
# 9 adjective test      reliable           0.47 0      
# 10 adjective test      unreliable         0.21 0.0196 
# 11 adjective train     reliable          -0.51 0      
# 12 adjective train     unreliable        -0.28 0.00183
# 13 noun      filler    reliable           0.71 0      
# 14 noun      filler    unreliable         0.7  0      
# 15 noun      test      reliable           0.94 0      
# 16 noun      test      unreliable         0.92 0      
# 17 noun      train     reliable           0.73 0      
# 18 noun      train     unreliable         0.18 0.0541  

######
# Correlations by window and condition

# Correlation by window and contrast condition collapsed across trial type
cors_window_contrast_all = df_merged %>% 
  group_by(window, cond) %>% 
  summarize(Correlation=round(cor.test(prop_selections,prop_looks)$estimate,2),P=round(cor.test(prop_selections,prop_looks)$p.value,5))
cors_window_contrast_all
# window    cond        Correlation      P
# 1 prior     no_contrast        0.54 0     
# 2 prior     contrast           0.8  0     
# 3 adjective no_contrast        0.12 0.0225
# 4 adjective contrast          -0.36 0     
# 5 noun      no_contrast        0.56 0     
# 6 noun      contrast           0.74 0  

# Correlation by window and contrast condition by trial type
cors_window_contrast_by_trial = df_merged %>% 
  group_by(window, cond, trialType) %>% 
  summarize(Correlation=round(cor.test(prop_selections,prop_looks)$estimate,2),P=round(cor.test(prop_selections,prop_looks)$p.value,5))
cors_window_contrast_by_trial
#  window    cond        trialType Correlation      P
# 1 prior     no_contrast filler           0.84 0     
# 2 prior     no_contrast test             0.8  0     
# 3 prior     no_contrast train           -0.19 0.0402
# 4 prior     contrast    filler           0.86 0     
# 5 prior     contrast    test             0.73 0     
# 6 prior     contrast    train            0.82 0     
# 7 adjective no_contrast filler          -0.49 0     
# 8 adjective no_contrast test            -0.02 0.879 
# 9 adjective no_contrast train            0.73 0     
# 10 adjective contrast    filler          -0.4  0     
# 11 adjective contrast    test             0.35 0.0058
# 12 adjective contrast    train           -0.49 0     
# 13 noun      no_contrast filler           0.7  0     
# 14 noun      no_contrast test             0.9  0     
# 15 noun      no_contrast train            0.18 0.0541
# 16 noun      contrast    filler           0.71 0     
# 17 noun      contrast    test             0.94 0     
# 18 noun      contrast    train            0.73 0      

# Correlation by window and pragmatic condition collapsed across trial type
cors_window_pragmatic_condition_all = df_merged %>% 
  group_by(window, pragContext) %>% 
  summarize(Correlation=round(cor.test(prop_selections,prop_looks)$estimate,2),P=round(cor.test(prop_selections,prop_looks)$p.value,5))
cors_window_pragmatic_condition_all
#   window    pragContext Correlation       P
# 1 prior     reliable           0.81 0      
# 2 prior     unreliable         0.54 0      
# 3 adjective reliable          -0.43 0      
# 4 adjective unreliable         0.14 0.00405
# 5 noun      reliable           0.7  0      
# 6 noun      unreliable         0.63 0       

# Correlation by window and pragmatic condition by trial type
cors_window_pragmatic_condition_by_trial = df_merged %>% 
  group_by(window, pragContext, trialType) %>% 
  summarize(Correlation=round(cor.test(prop_selections,prop_looks)$estimate,2),P=round(cor.test(prop_selections,prop_looks)$p.value,5))
cors_window_pragmatic_condition_by_trial

#    window    pragContext trialType Correlation      P
#  1 prior     reliable    filler           0.86 0     
#  2 prior     reliable    train            0.82 0     
#  3 prior     unreliable  filler           0.84 0     
#  4 prior     unreliable  test             0.76 0     
#  5 prior     unreliable  train           -0.19 0.0402
#  6 adjective reliable    filler          -0.4  0     
#  7 adjective reliable    train           -0.49 0     
#  8 adjective unreliable  filler          -0.49 0     
#  9 adjective unreliable  test             0.2  0.0308
# 10 adjective unreliable  train            0.73 0     
# 11 noun      reliable    filler           0.71 0     
# 12 noun      reliable    train            0.73 0     
# 13 noun      unreliable  filler           0.7  0     
# 14 noun      unreliable  test             0.92 0     
# 15 noun      unreliable  train            0.18 0.0541



