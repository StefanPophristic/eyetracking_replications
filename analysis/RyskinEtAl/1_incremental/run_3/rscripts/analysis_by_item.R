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
# Facet by pragmatic condition
##############


# test trials
test_trials <- df_merged %>%
  filter(trialType == "test") %>%
  ggplot(aes(x=prop_selections,y=prop_looks,color=Region,shape=cond, linetype=cond)) +
  geom_abline(intercept=0,slope=1,color="gray",linetype="dashed") +
  geom_smooth(mapping = aes(group = cond),
              formula="y~x",
              method="lm") +
  geom_point() +
  facet_wrap(pragContext~window) +
  scale_linetype_discrete(name="Pragmatic Condition corr. line", 
                          breaks=c("contrast", "no_contrast"), 
                          labels = c("contrast", "no_contrast"))+ 
  scale_color_manual(values=c(cbPalette[7],cbPalette[1],cbPalette[4],cbPalette[5])) +
  labs(
    shape="Pragmatic condition",
    x="Proportion of selections",
    y="Proportion of looks (Ryskin et al 2019, Exp. 2)") +
  xlim(0,1) +
  ylim(0,1) +
  facet_grid(pragContext~window)  +
  ggtitle("Correlations of test trials")
test_trials
ggsave("../graphs/analysis_by_item/test_trials.pdf", plot = test_trials, width=11,height=7)


# train trials
train_trials <- df_merged %>%
  filter(trialType == "train") %>%
  ggplot(aes(x=prop_selections,y=prop_looks,color=Region,shape=cond, linetype=cond)) +
  geom_abline(intercept=0,slope=1,color="gray",linetype="dashed") +
  geom_smooth(mapping = aes(group = cond),
              formula="y~x",
              method="lm") +
  geom_point() +
  facet_wrap(pragContext~window) +
  scale_linetype_discrete(name="Pragmatic Condition corr. line", 
                          breaks=c("contrast", "no_contrast"), 
                          labels = c("contrast", "no_contrast"))+ 
  scale_color_manual(values=c(cbPalette[7],cbPalette[1],cbPalette[4],cbPalette[5])) +
  labs(
    shape="Pragmatic condition",
    x="Proportion of selections",
    y="Proportion of looks (Ryskin et al 2019, Exp. 2)") +
  xlim(0,1) +
  ylim(0,1) +
  facet_grid(pragContext~window)  +
  ggtitle("Correlations of train trials")
train_trials
ggsave("../graphs/analysis_by_item/train_trials.pdf", plot = train_trials, width=11,height=7)

# filler trials
filler_trials <- df_merged %>%
  filter(trialType == "filler") %>%
  ggplot(aes(x=prop_selections,y=prop_looks,color=Region,shape=cond, linetype=cond)) +
  geom_abline(intercept=0,slope=1,color="gray",linetype="dashed") +
  geom_smooth(mapping = aes(group = cond),
              formula="y~x",
              method="lm") +
  geom_point() +
  facet_wrap(pragContext~window) +
  scale_linetype_discrete(name="Pragmatic Condition corr. line", 
                          breaks=c("contrast", "no_contrast"), 
                          labels = c("contrast", "no_contrast"))+ 
  scale_color_manual(values=c(cbPalette[7],cbPalette[1],cbPalette[4],cbPalette[5])) +
  labs(
    shape="Pragmatic condition",
    x="Proportion of selections",
    y="Proportion of looks (Ryskin et al 2019, Exp. 2)") +
  xlim(0,1) +
  ylim(0,1) +
  facet_grid(pragContext~window)  +
  ggtitle("Correlations of filler trials")
filler_trials
ggsave("../graphs/analysis_by_item/filler_trials.pdf", plot = filler_trials, width=11,height=7)


# Test trials contrast set present
test_trials_contrast <- df_merged %>%
  filter(trialType == "test" & cond == "contrast") %>%
  ggplot(aes(x=prop_selections,y=prop_looks,color=Region,shape=cond, linetype=cond)) +
  geom_abline(intercept=0,slope=1,color="gray",linetype="dashed") +
  geom_smooth(mapping = aes(group = cond),
              formula="y~x",
              method="lm") +
  geom_point() +
  facet_wrap(pragContext~window) +
  scale_linetype_discrete(name="Pragmatic Condition corr. line", 
                          breaks=c("contrast", "no_contrast"), 
                          labels = c("contrast", "no_contrast"))+ 
  scale_color_manual(values=c(cbPalette[7],cbPalette[1],cbPalette[4],cbPalette[5])) +
  labs(
    shape="Pragmatic condition",
    x="Proportion of selections",
    y="Proportion of looks (Ryskin et al 2019, Exp. 2)") +
  xlim(0,1) +
  ylim(0,1) +
  facet_grid(pragContext~window)  +
  ggtitle("Correlations of test trials (contrast)")
test_trials_contrast
ggsave("../graphs/analysis_by_item/test_trials_contrast.pdf", plot = test_trials_contrast, width=11,height=7)


##############
##############
# Correlations
##############

# Correlations between eye-movements (looks to an object) and selections (clicks on that object)

# Overall correlation
cor.test(df_merged$prop_selections,df_merged$prop_looks) 
# 0.6901901 df = 2158, p-value < 2.2e-16

######
# Correlations by window

# Correlation by window collapsed across trial type
cors_window_all = df_merged %>% 
  group_by(window) %>% 
  summarize(Correlation=round(cor.test(prop_selections,prop_looks)$estimate,2),P=round(cor.test(prop_selections,prop_looks)$p.value,5))
cors_window_all
# window    Correlation     P
# # <fct>           <dbl> <dbl>
# 1 prior            0.7      0
# 2 adjective        0.27     0
# 3 noun             0.91     0

# Correlation by window by trial type
cors_window_by_trial = df_merged %>% 
  group_by(trialType, pragContext, window, cond) %>% 
  summarize(Correlation=round(cor.test(prop_selections,prop_looks)$estimate,2),P=round(cor.test(prop_selections,prop_looks)$p.value,5))
cors_window_by_trial
