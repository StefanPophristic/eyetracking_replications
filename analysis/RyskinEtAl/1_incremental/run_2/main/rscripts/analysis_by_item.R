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


# Filler trial check
corr_fillers <- df_merged %>%
  filter(trialType == "filler") %>%
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
corr_fillers

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

corr_by_contrastCondition_test_comparison <- df_merged %>%
  filter(trialType == "test") %>%
  filter((cond == "contrast" & pragContext == "reliable") | (cond == "no_contrast" & pragContext == "unreliable")) %>%
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
corr_by_contrastCondition_test_comparison
ggsave("../graphs/analysis_by_item/corr_by_contrastCondition_test_comparison.pdf", plot = corr_by_contrastCondition_test_comparison, width=10,height=8, units = "in")


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
# 0.619966. df = 2158, p-value < 2.2e-16

######
# Correlations by window

# Correlation by window collapsed across trial type
cors_window_all = df_merged %>% 
  group_by(window) %>% 
  summarize(Correlation=round(cor.test(prop_selections,prop_looks)$estimate,2),P=round(cor.test(prop_selections,prop_looks)$p.value,5))
cors_window_all
# window    Correlation     P
# <fct>           <dbl> <dbl>
# 1 prior            0.76     0
# 2 adjective        0.2      0
# 3 noun             0.83     0 

# Correlation by window by trial type
cors_window_by_trial = df_merged %>% 
  group_by(window, trialType, pragContext) %>% 
  summarize(Correlation=round(cor.test(prop_selections,prop_looks)$estimate,2),P=round(cor.test(prop_selections,prop_looks)$p.value,5))
cors_window_by_trial
# window    trialType pragContext Correlation       P
# 1 prior     filler    reliable           0.81 0      
# 2 prior     filler    unreliable         0.82 0      
# 3 prior     test      reliable           0.45 0      
# 4 prior     test      unreliable         0.28 0.00163
# 5 prior     train     reliable           0.52 0      
# 6 prior     train     unreliable         0.84 0      
# 7 adjective filler    reliable          -0.05 0.576  
# 8 adjective filler    unreliable        -0.07 0.447  
# 9 adjective test      reliable           0.57 0      
# 10 adjective test      unreliable         0.29 0.00128
# 11 adjective train     reliable           0.66 0      
# 12 adjective train     unreliable        -0.22 0.0154 
# 13 noun      filler    reliable           0.91 0      
# 14 noun      filler    unreliable         0.94 0      
# 15 noun      test      reliable           0.94 0      
# 16 noun      test      unreliable         0.92 0      
# 17 noun      train     reliable           0.95 0      
# 18 noun      train     unreliable         0.18 0.0541 

######
# Correlations by window and condition

# Correlation by window and contrast condition collapsed across trial type
cors_window_contrast_all = df_merged %>% 
  group_by(window, cond) %>% 
  summarize(Correlation=round(cor.test(prop_selections,prop_looks)$estimate,2),P=round(cor.test(prop_selections,prop_looks)$p.value,5))
cors_window_contrast_all
# window    cond        Correlation      P
# 1 prior     contrast           0.7  0    
# 2 prior     no_contrast        0.8  0    
# 3 adjective contrast           0.36 0    
# 4 adjective no_contrast        0.04 0.402
# 5 noun      contrast           0.93 0    
# 6 noun      no_contrast        0.72 0      

# Correlation by window and contrast condition by trial type
cors_window_contrast_by_trial = df_merged %>% 
  group_by(window, cond, trialType) %>% 
  summarize(Correlation=round(cor.test(prop_selections,prop_looks)$estimate,2),P=round(cor.test(prop_selections,prop_looks)$p.value,5))
cors_window_contrast_by_trial
#  window    cond        trialType Correlation      P
# 1 prior     contrast    filler           0.81 0      
# 2 prior     contrast    test             0.3  0.00078
# 3 prior     contrast    train            0.52 0      
# 4 prior     no_contrast filler           0.82 0      
# 5 prior     no_contrast test             0.41 0      
# 6 prior     no_contrast train            0.84 0      
# 7 adjective contrast    filler          -0.05 0.576  
# 8 adjective contrast    test             0.54 0      
# 9 adjective contrast    train            0.66 0      
# 10 adjective no_contrast filler          -0.07 0.447  
# 11 adjective no_contrast test             0.24 0.00925
# 12 adjective no_contrast train           -0.22 0.0154 
# 13 noun      contrast    filler           0.91 0      
# 14 noun      contrast    test             0.95 0      
# 15 noun      contrast    train            0.95 0      
# 16 noun      no_contrast filler           0.94 0      
# 17 noun      no_contrast test             0.9  0      
# 18 noun      no_contrast train            0.18 0.0541 

# Correlation by window and pragmatic condition collapsed across trial type
cors_window_pragmatic_condition_all = df_merged %>% 
  group_by(window, pragContext) %>% 
  summarize(Correlation=round(cor.test(prop_selections,prop_looks)$estimate,2),P=round(cor.test(prop_selections,prop_looks)$p.value,5))
cors_window_pragmatic_condition_all
#   window    pragContext Correlation       P
# 1 prior     reliable           0.72 0    
# 2 prior     unreliable         0.79 0    
# 3 adjective reliable           0.36 0    
# 4 adjective unreliable         0.06 0.230
# 5 noun      reliable           0.92 0    
# 6 noun      unreliable         0.73 0  


# Correlation by window x pragmatic condition x contrast condition
cors_window_pragmatic_condition_all = df_merged %>% 
  group_by(window, pragContext, cond) %>% 
  summarize(Correlation=round(cor.test(prop_selections,prop_looks)$estimate,2),P=round(cor.test(prop_selections,prop_looks)$p.value,5))
cors_window_pragmatic_condition_all
# 1 prior     reliable    contrast           0.75 0      
# 2 prior     reliable    no_contrast        0.43 0.00067
# 3 prior     unreliable  contrast           0.12 0.363  
# 4 prior     unreliable  no_contrast        0.82 0      
# 5 adjective reliable    contrast           0.36 0      
# 6 adjective reliable    no_contrast        0.26 0.0439 
# 7 adjective unreliable  contrast           0.35 0.00652
# 8 adjective unreliable  no_contrast        0.02 0.720  
# 9 noun      reliable    contrast           0.93 0      
# 10 noun      reliable    no_contrast        0.9  0      
# 11 noun      unreliable  contrast           0.94 0      
# 12 noun      unreliable  no_contrast        0.69 0      

# Correlation by window and pragmatic condition by trial type
cors_window_pragmatic_condition_by_trial = df_merged %>% 
  group_by(window, trialType, pragContext) %>% 
  summarize(Correlation=round(cor.test(prop_selections,prop_looks)$estimate,2),P=round(cor.test(prop_selections,prop_looks)$p.value,5))
cors_window_pragmatic_condition_by_trial

#    window    pragContext trialType Correlation      P
# 1 prior     filler    reliable           0.81 0      
# 2 prior     filler    unreliable         0.82 0      
# 3 prior     test      reliable           0.45 0      
# 4 prior     test      unreliable         0.28 0.00163
# 5 prior     train     reliable           0.52 0      
# 6 prior     train     unreliable         0.84 0      
# 7 adjective filler    reliable          -0.05 0.576  
# 8 adjective filler    unreliable        -0.07 0.447  
# 9 adjective test      reliable           0.57 0      
# 10 adjective test      unreliable         0.29 0.00128
# 11 adjective train     reliable           0.66 0      
# 12 adjective train     unreliable        -0.22 0.0154 
# 13 noun      filler    reliable           0.91 0      
# 14 noun      filler    unreliable         0.94 0      
# 15 noun      test      reliable           0.94 0      
# 16 noun      test      unreliable         0.92 0      
# 17 noun      train     reliable           0.95 0      
# 18 noun      train     unreliable         0.18 0.0541 

# Correlation by window and pragmatic condition by trial type
cors_filler_contrst = df_merged %>% 
  filter(trialType == "test") %>%
  group_by(window, cond, pragContext) %>% 
  summarize(Correlation=round(cor.test(prop_selections,prop_looks)$estimate,2),P=round(cor.test(prop_selections,prop_looks)$p.value,5))
cors_filler_contrst
# 1 prior     contrast    reliable           0.48 0.00009
# 2 prior     contrast    unreliable         0.12 0.363  
# 3 prior     no_contrast reliable           0.43 0.00067
# 4 prior     no_contrast unreliable         0.41 0.00114
# 5 adjective contrast    reliable           0.72 0      
# 6 adjective contrast    unreliable         0.35 0.00652
# 7 adjective no_contrast reliable           0.26 0.0439 
# 8 adjective no_contrast unreliable         0.22 0.0917 
# 9 noun      contrast    reliable           0.98 0      
# 10 noun      contrast    unreliable         0.94 0      
# 11 noun      no_contrast reliable           0.9  0      
# 12 noun      no_contrast unreliable         0.9  0      
