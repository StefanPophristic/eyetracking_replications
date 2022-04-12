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


##############
##############
# Data Wrangling
##############
##############

# Notes:
## Combine eyetracking and selection data
## Include test, train, and filler trials where adjective was used
## The "Wrangle raw eyetracking data" and "Wrangle raw selectiond data"
##   were taken from Casey Butcher's graphs.R analysis script without any
##   edits. The "Merge eyetracking and selectiond ata" section was taken from
##   the same analysis script, but edited to keep training and filler items.

##############
# Wrangle raw eyetracking data
##############

# prior window data
prior_df = read_tsv(paste0(getwd(),"/ryskin_eyetracking/PragTrain3_baseline_window_timesummLongForm.txt"),
                      col_names=c('target_dur','contrast_dur','compet_dur','other_dur','currsubj','subject','trialnum','order','timebin','trialID','condWith','condBet','targ_loc','contrast_loc','compet_loc','list','trialType','accuracy','target_fix','target_AR1','compet_fix','compet_AR1')) %>% 
  mutate(contrast_cond = case_when(
    condWith == 0 ~ 'filler', 
    condWith == 1 ~ 'no_contrast',
    condWith == 2 ~ 'contrast'),
    prag_context_cond = case_when(
      condBet == 10 ~ 'reliable',
      condBet == 9 ~ 'unreliable',
      condBet == 0 ~ 'filler'),
    trial_type = case_when(
      trialType == 1 ~ 'test',
      trialType == 99 ~ 'train',
      trialType == 999 ~ 'filler'),
    uniqueID = str_c('l',as.character(list),'_',trial_type,trialID),
    total_dur = (target_dur + contrast_dur + compet_dur + other_dur),
    target_prop = target_dur/total_dur,
    contrast_prop = contrast_dur/total_dur,
    compet_prop = compet_dur/total_dur,
    other_prop = other_dur/total_dur,
    timebin_rel = timebin/10 - 17)

pragtrain3_list1 = read_tsv(paste0(getwd(),'/ryskin_eyetracking/experiments_2-3_trials_list_1.txt'))
pragtrain3_list2 = read_tsv(paste0(getwd(),'/ryskin_eyetracking/experiments_2-3_trials_list_2.txt'))
pragtrain3_lists = bind_rows(pragtrain3_list1,pragtrain3_list2) %>% 
  mutate(unique_ID = str_c(as.character(counterbalance),'_',trialID))
prior_df = prior_df %>% 
  left_join(pragtrain3_lists, by = c('uniqueID'='unique_ID'))
rm(pragtrain3_list1, pragtrain3_list2)

# expt2 reading in adj+noun window data for glmm
pragtrain3_data = read_tsv('../data/ryskin_eyetracking/PragTrain3_adjnoun_for_GLMM_timesummLongForm.txt',
                           col_names=c('target_dur','contrast_dur','compet_dur','other_dur','currsubj','subject','trialnum','order','timebin','trialID','condWith','condBet','targ_loc','contrast_loc','compet_loc','list','trialType','accuracy','target_fix','target_AR1','compet_fix','compet_AR1')) %>% 
  mutate(contrast_cond = case_when(
    condWith == 0 ~ 'filler', 
    condWith == 1 ~ 'no_contrast',
    condWith == 2 ~ 'contrast'),
    prag_context_cond = case_when(
      condBet == 10 ~ 'reliable',
      condBet == 9 ~ 'unreliable',
      condBet == 0 ~ 'filler'),
    trial_type = case_when(
      trialType == 1 ~ 'test',
      trialType == 99 ~ 'train',
      trialType == 999 ~ 'filler'),
    uniqueID = str_c('l',as.character(list),'_',trial_type,trialID),
    total_dur = (target_dur + contrast_dur + compet_dur + other_dur),
    target_prop = target_dur/total_dur,
    contrast_prop = contrast_dur/total_dur,
    compet_prop = compet_dur/total_dur,
    other_prop = other_dur/total_dur,
    timebin_rel = timebin/10 - 17)

pragtrain3_data = pragtrain3_data %>% 
  left_join(pragtrain3_lists, by = c('uniqueID'='unique_ID'))
rm(pragtrain3_lists)

#adds prior window to pragtrain3_data
pragtrain3_data <- rbind(prior_df, pragtrain3_data)

# We are including the following trial types:
# Test trials
# Training trials with the adjective overtly stated, which includes 
#   - pragmatic reliable contrast present condition
#   - pragmatic unreliable contrast absent condition
# Filler trials

pragtrain3_crit = pragtrain3_data %>% 
  group_by(subject,trialnum) %>% 
  mutate(target_AR1_corrected = lag(target_fix),
         compet_AR1_corrected = lag(compet_fix)) %>% 
  filter(target_AR1 != 999) %>%
  filter(trial_type == 'test' | 
           (trial_type == 'train' & prag_context_cond == "reliable" & contrast_cond == "contrast") | 
           (trial_type == 'train' & prag_context_cond == "unreliable" & contrast_cond == "no_contrast") |
           (trial_type == 'filler' & cond == "semantic_contrast")) %>%
  #filter(list == 1) %>%
  ungroup()

# rename semantic_contrast in cond column to be consistent
# this only affects filler trials
pragtrain3_crit <- pragtrain3_crit %>%
  mutate(contrast_cond = case_when((cond == "semantic_contrast" & prag_context_cond == "reliable") ~ "contrast",
                                   (cond == "semantic_contrast" & prag_context_cond == "unreliable") ~ "no_contrast",
                                   TRUE ~ cond))

# Get the target noun and corresponding adjective
pragtrain3_crit <- pragtrain3_crit %>%
  mutate(noun = sub("_.*", "", target_pic),
         stringWithFeature = sub(".*_", "", target_pic)) %>%
  mutate(feature = sub(".jpg.*", "", stringWithFeature)) %>%
  select(!stringWithFeature)

# expt2 setting up factors and contrasts
pragtrain3_crit$timebin_rel_c = scale(pragtrain3_crit$timebin_rel,scale=F, center = T)
pragtrain3_crit$prag_context_cond = factor(pragtrain3_crit$prag_context_cond)
pragtrain3_crit$contrast_cond = factor(pragtrain3_crit$contrast_cond)

contrasts(pragtrain3_crit$prag_context_cond)<-c(-0.5,0.5)
contrasts(pragtrain3_crit$contrast_cond)<-c(0.5,-0.5)

contrasts(pragtrain3_crit$prag_context_cond)
contrasts(pragtrain3_crit$contrast_cond)


# Ryskin et al reports the following:
## Two lists were created to allow target items to appear bothin the 
## Contrast and No Contrast conditions across subjects (available at https://osf.io/5geba/).
## However, due to a counterbalancing error, participants in the reliable 
## pragmaticcontext were always assigned to list one and participants 
## in the unreliable pragmatic con-text only received list two.

# I section out the test trials to see how how the pragmatic condition maps the the lists
allTestRyskin <- pragtrain3_crit %>%
  filter(trial_type == "test") %>%
  select(contrast_cond, prag_context_cond, list, audio) %>%
  unique()

view(allRyskin)
# As you can see, all the unreliable pragmatic conditions are found in list 1
# and all reliable pragmatic conditions are found in list 2
# which is the oposite of what they report

# I now check whether this affected the training items
allTrainRyskin <- pragtrain3_crit %>%
  filter(trial_type == "train") %>%
  select(contrast_cond, prag_context_cond, trial_type, list, audio) %>%
  unique()
view(allTrainRyskin)
nrow(allTrainRyskin) #80
# It does, the two groups of people saw different objects as the target 
# across pragmatic reliable versus unreliable conditions.
# However, we find this in the selection data too, which leads me to believe
# that they used different trials 

# this code has to be run after running the code below
# test <- df %>% 
#   filter(trialType == "train") %>% 
#   select(cond, pragContext, trialType, feature, noun) %>%
#   unique()
# view(test)
# nrow(test) # 160
# So there are 160 training trials
# There are a total of 80 items that appear in both pragmatic good
# and pragmatic bad conditions.
# But these 80 items ONLY appear in the pragmatic good condition for
# the eye tracking study. Therefore we should see a gap in the final
# merged data for these 80 items in the ureliable condition. However, 
# we don't! This is because the 80 items in the unreliable condition 
# get deleted because there is no over adjective marking. 
# therefore training trials lead to no loss in data. 

# I now check whether this affected the filler items
allFillerRyskin <- pragtrain3_crit %>%
  filter(trial_type == "filler") %>%
  select(contrast_cond, prag_context_cond, trial_type, list, audio) %>%
  unique()
view(allFillerRyskin)
# It is balanced, so that the participants saw the exact same items,
# regardless of condition
# therefore it doesn't matter that the lsits are different


##############
# Wrangle raw selection data
#############

# Fillers:
#   The conditions of fillers were coded as: {semantic_contrast, semantic_control, contrast_control}
#     Semantic_control and contrast_control conditions never had adjective utterances, so we will
#     exclude them. the semantic_contrast condition fillers always included adjectives. They differed
#     as to whether the contrast set was present or absent depending on whether the participant was in  
#     the pragmatic good or bad condition (pragmatic good/reliable = contrast set present, pragmatic bad/
#     unreliable = contrast set absent). The semantic_contrast condition label will be recoded as contrast
#     versus no_contrast to be consistent with the training and test trials. 

df = read_csv("ryskin-trials.csv")
demo = read.csv("ryskin-subject_information.csv", header = TRUE)

#remove error column
df$error <- NULL
df$proliferate.condition <- NULL
`%notin%` <- Negate(`%in%`)

#remove pilot participants
df <- df[df$workerid %notin% c(17, 13, 16, 19, 14, 18),]
length(unique(df$workerid)) #63 participants

#trial numbers have to be reduced by 2
df$trial_number <- df$trial_number-2

#removing periods from the loc ids
df$loc_big_filler <- gsub("\\.", "", df$loc_big_filler)
df$loc_contrast = gsub("\\.","",df$loc_contrast)
df$loc_small_filler = gsub("\\.","",df$loc_small_filler)
df$loc_target_pic = gsub("\\.","",df$loc_target_pic)

df = df %>% 
  separate(response,into = c("click_prior", "click2", "click3"), sep=",")

#clean click values
df$click_prior <- gsub("\\[", "", df$click_prior)
df$click_prior <- gsub("\\'", "", df$click_prior)
df$click2 <- gsub("\\]", "", df$click2)
df$click2 <- gsub("\\'", "", df$click2)
df$click2 <- gsub(" ", "", df$click2)
df$click3 <- gsub("\\]", "", df$click3)
df$click3 <- gsub("\\'", "", df$click3)
df$click3 <- gsub(" ", "", df$click3)

df = df %>% 
  mutate(click_noun = case_when(is.na(click3) ~ click2,
                                TRUE ~ click3)) %>% 
  mutate(click_adj = case_when(is.na(click3) ~ "NA",
                               TRUE ~ click2)) %>% 
  select(-click2, -click3)

df = df %>% 
  separate(target_pic, into = c("noun","feature"), sep="[_.]", remove=F, extra="drop") %>% 
  mutate(loc_competitor_pic = case_when(feature == "small" ~ loc_small_filler,
                                        feature == "big" ~ loc_big_filler,
                                        TRUE ~ "NA"))


## Get response times

df$response_temp = NA
df$response_temp <- gsub("\\[", "", df$response_times)
df$response_temp <- gsub("\\]", "", df$response_temp)

# response times (hence t) for each window
df = df %>% 
  separate(response_temp,into = c("t_prior", "t2", "t3"), sep=", ")

# accounting for modified vs unmodified cases
df = df %>% 
  mutate(t_noun = case_when(is.na(t3) ~ t2,
                            TRUE ~ t3)) %>% 
  mutate(t_adj = case_when(is.na(t3) ~ "NA",
                           TRUE ~ t2)) %>% 
  select(-t2, -t3)

#char to int
df$t_prior <- as.integer(df$t_prior)
df$t_adj <- as.integer(df$t_adj)
df$t_noun <- as.integer(df$t_noun)

# Exclusions of original data

df = df %>% 
  mutate(selection_correct = click_noun == loc_target_pic)

table(df$selection_correct) # 267 incorrect responses for pilot, 3431 for main

table(df$trialType)

# exclude anyone with < 90% correct selections on all trials where correct selection was possible from linguistic signal

# NOTE: This take on "accuracy" differs from the above in that "correct" reflects
# the percent of all correct selections where correct selection was possible from linguistic signal.
# The "accuracy" commented out above counts only test trials in calculating "correct" 
bad_accuracy <- df %>%
  subset(df$pragContext == "bad") %>%
  subset(cond != 'contrast_control' & cond != 'semantic_control') %>%
  subset(!(cond =='no_contrast' & trialType == "train"))  %>% #removes train
  group_by(workerid,pragContext) %>%
  tally(selection_correct) %>%
  mutate(correct=n/140)

good_accuracy <- df %>%
  subset(df$pragContext == "good") %>%
  group_by(workerid,pragContext) %>%
  tally(selection_correct) %>%
  mutate(correct=n/300)

accuracy <- rbind(good_accuracy,bad_accuracy)
View(accuracy %>% arrange(correct))

toexclude = accuracy %>%
  filter(correct <.9)

length(toexclude$workerid) # 4 for pilot, 4 for main
length(toexclude$workerid)/length(accuracy$workerid) #0.06349206 data loss

df = df %>% 
  filter(!workerid %in% toexclude$workerid)

# # remove trials with incorrect selections
df = df %>%
  filter(selection_correct==1)

nrow(df) # 14745 for main

# Only keep the types of trials were are interested in
d_test = df %>% 
  filter(trialType == 'test' | 
           (trialType == 'train' & pragContext == "good" & cond == "contrast") | 
           (trialType == 'train' & pragContext == "bad" & cond == "no_contrast") |
           (trialType == 'filler'& cond == "semantic_contrast")) %>%
  droplevels()
  
# rename semantic_contrast in cond column to be consistent
# This only affects filler items
d_test <- d_test %>%
  mutate(cond = case_when((cond == "semantic_contrast" & pragContext == "good") ~ "contrast",
                          (cond == "semantic_contrast" & pragContext == "bad") ~ "no_contrast",
                          TRUE ~ cond))
  


no_contrast_train <- df %>% 
  select(cond, pragContext, trialType, utterance_type, instruction) %>%
  filter(cond == "no_contrast" & trialType == "train") %>%
  unique()

contrast_train <- df %>% 
  select(cond, pragContext, trialType, utterance_type, instruction) %>%
  filter(cond == "contrast" & trialType == "train") %>%
  unique()

view(no_contrast_train)

view(contrast_train)


##############
# Merge Eyetracking and selection data
##############

# Eye tracking data
toplot_eye = pragtrain3_crit %>% 
  separate(audio,into=c("noun","size","wav")) %>% 
  select(contrast_cond,prag_context_cond,trial_type, noun, feature, timebin,target_fix,compet_fix) %>% #,
  filter(!(target_fix == 1 & compet_fix == 1)) %>% 
  mutate(other_fix = ifelse(target_fix + compet_fix == 0, 1, 0),
         window = ifelse(timebin <=200, "prior", ifelse(timebin <= 830, "adjective","noun"))) %>%
  pivot_longer(names_to = "Region", values_to = "fixation",cols=target_fix:other_fix) %>%
  mutate(Region = fct_recode(Region,"target" = "target_fix","competitor"="compet_fix","other"="other_fix")) %>% 
  group_by(contrast_cond,prag_context_cond,window,trial_type, feature, noun, Region) %>%
  summarize(prop_looks=mean(fixation)) %>% 
  ungroup() %>% 
  rename(pragContext = prag_context_cond, cond = contrast_cond, trialType = trial_type)


# Selection data
toplot_select =  d_test %>%
  select(workerid,pragContext,cond,click_prior,click_adj,click_noun,loc_target_pic,loc_competitor_pic,loc_contrast,loc_big_filler,loc_small_filler,instruction,trial_number,trial,trialType, noun, feature) %>%
  pivot_longer(names_to = "window", values_to = "selection",cols=click_prior:click_noun) %>% 
  mutate(target = case_when(loc_target_pic==selection ~ 1,
                            TRUE ~ 0)) %>% 
  mutate(competitor = case_when(loc_competitor_pic==selection ~ 1,
                                TRUE ~ 0)) %>% 
  mutate(other = case_when(target + competitor == 0 ~ 1,
                                TRUE ~ 0)) %>% 
  group_by(cond,pragContext,window, trialType, feature, noun) %>% #added trialType
  summarize(m_target=mean(target),m_competitor=mean(competitor),m_other=mean(other),ci_low_target=ci.low(target),ci_high_target=ci.high(target),ci_low_competitor=ci.low(competitor),ci_high_competitor=ci.high(competitor),ci_low_other=ci.low(other),ci_high_other=ci.high(other)) %>%
  ungroup() %>%
  mutate(info_target = paste(m_target, m_target-ci_low_target, m_target+ci_high_target), info_competitor=paste(info_competitor = paste(m_competitor, m_competitor-ci_low_competitor, m_competitor+ci_high_competitor)),info_other = paste(m_other, m_other-ci_low_other, m_other+ci_high_other)) %>% 
  select(cond,pragContext,window,trialType,feature, noun, info_target,info_competitor,info_other) %>% #added trialType
  pivot_longer(names_to="location",values_to="info_selections",cols=info_target:info_other) %>%
  separate(info_selections,into=c("prop_selections","ymin_selections","ymax_selections"),sep=" ") %>% 
  mutate(Region=fct_recode(location,"competitor"="info_competitor","target"="info_target","other"="info_other")) %>%
  mutate(Region=fct_rev(Region)) %>%
  ungroup() %>%  
  mutate(window=fct_recode(window,prior="click_prior",adjective="click_adj",noun="click_noun")) %>% 
  mutate(pragContext=fct_recode(pragContext,reliable="good",unreliable="bad")) %>% 
  mutate(pragContext = fct_relevel(pragContext,"reliable","unreliable")) %>%
  mutate(cond = fct_relevel(cond,"no_contrast","contrast")) %>% 
  select(cond, pragContext, window, Region, trialType, feature, noun, prop_selections, ymin_selections, ymax_selections)




# The merge does not work for the pragmatic reliable test conditions
# Explanation below these 5 lines of code
# Merge the two together
df_merged = left_join(toplot_select, toplot_eye, by=c("cond","pragContext","trialType","noun","feature","window","Region")) %>%
  mutate(window = fct_relevel(window,"prior","adjective")) %>% 
  mutate(ymin_selections = as.numeric(ymin_selections),ymax_selections = as.numeric(ymax_selections),prop_selections = as.numeric(prop_selections))
nrow(df_merged)
names(df_merged)



# The issue should show up in the test and training trials
# whereby data will be missing for all test and training trials
# for the pragmatic reliable condition. 
df_merged_na <- df_merged %>%
  filter(is.na(prop_looks)) %>%
  select(pragContext, trialType) %>%
  unique()

numDroppedTrials <- df_merged %>%
  filter(is.na(prop_looks)) %>%
  nrow()
  
numDroppedTrials #360

nrow(df_merged) # 2520

numDroppedTrials/nrow(df_merged) 
# These leads to a 14% exclusion of data

df_merged %>%
  filter(trialType == "test" & pragContext == "reliable") %>%
  nrow()
# 360
# therefore we are sure that all test trials in the reliable condition
# are missing data


# Get rid of the missing data
df_merged <- df_merged %>%
  filter(!is.na(prop_looks))


# Looking at why merge doesn't work
# eyeMistakes <- toplot_eye %>%
#   filter(window == "adjective" & Region == "target" & trialType =="test" & pragContext == "reliable") %>%
#   select(-prop_looks)%>%
#   rename(eyeCondition = cond)
# 
# selectMistakes <- toplot_select %>%
#   filter(window == "adjective" & Region == "target" & trialType =="test" & pragContext == "reliable") %>%
#   select(-prop_selections, - ymin_selections, -ymax_selections) %>%
#   rename(selectCondition = cond)
# 
# mistakes <- left_join(eyeMistakes, selectMistakes, by = c("pragContext", "window", "trialType", "feature", "noun", "Region")) %>%
#   select (-window, -Region)
# 
# mistakes <- mistakes[, c("eyeCondition", "selectCondition", "feature", "noun", "pragContext", "trialType")]
# 
# view(mistakes)

# It seems like the contrast {present, absent} conditions were switched between the two experiments
# but overall the average out to the same thing, which is why this was not caught until now
# here is a subset of the output:
#
# eyeCondition selectCondition feature noun      pragContext trialType
# <fct>        <fct>           <fct>   <fct>     <fct>       <chr>    
# 1 contrast     no_contrast     big     brush     reliable    test     
# 2 contrast     no_contrast     big     bunny     reliable    test     
# 3 contrast     no_contrast     big     camera    reliable    test     
# 4 contrast     no_contrast     big     dress     reliable    test     
# 5 contrast     no_contrast     big     grapes    reliable    test  
# 6 no_contrast  contrast        big     ball      reliable    test     
# 7 no_contrast  contrast        big     ballerina reliable    test     
# 8 no_contrast  contrast        big     corn      reliable    test     
# 9 no_contrast  contrast        big     dog       reliable    test     
#10 no_contrast  contrast        big     hat       reliable    test 

# eyeCondition = contrast present vs. absent for eye trackinge xperiment
# selectCondition = contrast present vs. absent for incremental decision (selection) experiment
# feature = adjective used
# noun = noun used


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
ggsave("../graphs/analysis_by_item/corr_everything.pdf", plot = corr_of_everything, width=5.5,height=3.5)


##############
# Correlation by Window
##############

# Correlation by window collasped across trial type
corr_by_window_all <- df_merged %>%
  ggplot(aes(x=prop_selections,y=prop_looks,color=Region,shape=pragContext,group=1)) +
  geom_abline(intercept=0,slope=1,color="gray",linetype="dashed") +
  geom_smooth(method="lm") +
  geom_point() +
  facet_wrap(~window) +
  scale_color_manual(values=c(cbPalette[7],cbPalette[1],cbPalette[4],cbPalette[5])) +
  labs(
    shape="Contrast condition",
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
  geom_smooth(method="lm") +
  geom_point() +
  facet_grid(trialType~window) +
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
  group_by(window, trialType) %>% 
  summarize(Correlation=round(cor.test(prop_selections,prop_looks)$estimate,2),P=round(cor.test(prop_selections,prop_looks)$p.value,5))
cors_window_by_trial
#   window    trialType Correlation       P
# 1 prior     filler           0.85 0      
# 2 prior     test             0.76 0      
# 3 prior     train            0.29 0.00001
# 4 adjective filler          -0.45 0      
# 5 adjective test             0.2  0.0308 
# 6 adjective train            0.19 0.00396
# 7 noun      filler           0.71 0      
# 8 noun      test             0.92 0      
# 9 noun      train            0.38 0     

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



