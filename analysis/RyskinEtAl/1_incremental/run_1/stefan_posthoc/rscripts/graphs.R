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
# ryskin fig 2: dark purple, light purple, dark green, light green
ryskinPalette <- c("#831CBF", "#cf97EF", "#00C19b", "#90D1C4")

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

##############
# Get response times (OG data)
#############

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


##############
# Exclusions of original data
#############

df = df %>% 
  mutate(selection_correct = click_noun == loc_target_pic)

table(df$selection_correct) # 267 incorrect responses for pilot, 3431 for main

table(df$trialType)

# exclude anyone with < 90% correct selections on all trials where correct selection was possible from linguistic signal
# UPDATE THIS CODE

# accuracy <- df %>%
#   filter(trialType == "test") %>%
#   group_by(workerid, pragContext) %>%
#   tally(selection_correct) %>%
#   mutate(correct=n/40) #40 test trials total

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

nrow(df) # 936 for pilot, 14745 for main

# In this second analysis, we are keeping training trials
d_test = df %>% 
  filter(trialType %in% c("test", "train")) %>%
  droplevels()


##############
# Plot data from Replication Task
#############
# Is this the eyetracking replication that we did, or just recreating the graphs using teh OG data?
# This section recreates Ryskin et al graphs


# plot proportion of selections by condition
toplot =  d_test %>%
  select(workerid,pragContext,cond,click_prior,click_adj,click_noun,loc_target_pic,loc_competitor_pic,loc_contrast,loc_big_filler,loc_small_filler,instruction,trial_number,trial) %>%
  pivot_longer(names_to = "window", values_to = "selection",cols=click_prior:click_noun) %>% 
  mutate(target = case_when(loc_target_pic==selection ~ 1,
                            TRUE ~ 0)) %>% 
  mutate(competitor = case_when(loc_competitor_pic==selection ~ 1,
                                TRUE ~ 0)) %>% 
  mutate(distractor = case_when(loc_target_pic!=selection & loc_competitor_pic !=selection ~ 1,
                                TRUE ~ 0)) %>% 
  group_by(cond,pragContext,window) %>%
  summarize(m_target=mean(target),m_competitor=mean(competitor), m_distractor=mean(distractor),ci_low_target=ci.low(target),ci_high_target=ci.high(target),ci_low_competitor=ci.low(competitor),ci_high_competitor=ci.high(competitor), ci_low_distractor=ci.low(distractor),ci_high_distractor=ci.high(distractor)) %>%
  pivot_longer(names_to="location",values_to="Mean",cols=m_target:m_distractor) %>%
  mutate(CILow=ifelse(location=="m_target",ci_low_target,ifelse(location=="m_competitor",ci_low_competitor,ifelse(location=="m_distractor",ci_low_distractor,0)))) %>%
  mutate(CIHigh=ifelse(location=="m_target",ci_high_target,ifelse(location=="m_competitor",ci_high_competitor,ifelse(location=="m_distractor",ci_high_distractor,0)))) %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh) %>%
  mutate(Region=fct_recode(location,"competitor"="m_competitor","target"="m_target", "distractor"="m_distractor")) %>%
  mutate(Region=fct_rev(Region)) %>%
  ungroup() %>% 
  mutate(window=fct_recode(window,prior="click_prior",adjective="click_adj",noun="click_noun")) %>% 
  mutate(window = fct_relevel(window,"prior","adjective"))

proportions = ggplot(toplot, aes(x=window, y=Mean, group=Region)) +
  geom_line(aes(color=Region),size=1.3) +
  geom_point(aes(color=Region),size=2.5,shape="square") +
  geom_errorbar(aes(ymin=YMin, ymax=YMax), width=.2, alpha=.3) +
  facet_grid(cond ~ pragContext ) + 
  scale_color_manual(values=c("darkgreen","blue", "orange")) +
  xlab("Window") +
  ylab("Proportion of selections") +
  theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1))

proportions
ggsave(proportions, file="../graphs/proportions.pdf",width=9,height=4.5)

# recreate Fig 13 from Sun & Breheny 2020
toplot =  df %>%
  filter(cond %in% c('contrast','no_contrast')) %>% 
  select(workerid,pragContext,cond,click_prior,click_adj,click_noun,loc_target_pic,loc_competitor_pic,loc_contrast,loc_big_filler,loc_small_filler,instruction,trial_number,trial) %>%
  pivot_longer(names_to = "window", values_to = "selection",cols=click_prior:click_noun) %>% 
  mutate(target = case_when(loc_target_pic==selection ~ 1,
                            TRUE ~ 0)) %>% 
  mutate(competitor = case_when(loc_competitor_pic==selection ~ 1,
                                TRUE ~ 0)) %>% 
  mutate(distractor = case_when(loc_target_pic!=selection & loc_competitor_pic !=selection ~ 1,
                                TRUE ~ 0)) %>%
  # when target == 0 and comp == 0, turn target 0 into .5? or exclude? excluding for time being (leads to exclusion of 3230 data points)
  filter(target == 1 | competitor == 1) %>% 
  group_by(pragContext,cond,window,workerid) %>%
  summarize(m_target=mean(target),m_competitor=mean(competitor)+.00000001) %>% 
  ungroup() %>% 
  mutate(prop=(m_target/m_competitor)+.00000001,targetadvantage=log(prop)) %>% 
  group_by(pragContext,cond,window) %>%
  summarize(target=mean(targetadvantage),ci_low_target=ci.low(targetadvantage),ci_high_target=ci.high(targetadvantage)) %>%
  ungroup() %>% 
  mutate(YMin=target-ci_low_target,YMax=target+ci_high_target) %>% 
  mutate(cond=fct_relevel(cond,"contrast", "no_contrast")) %>%
  mutate(window=fct_recode(window,prior="click_prior",adjective="click_adj",noun="click_noun")) %>% 
  mutate(window = fct_relevel(window,"prior","adjective"))
  dodge=position_dodge(0)

fig13 <- ggplot(toplot, aes(x=window, y=target, color=cond, linetype=pragContext,group=interaction(cond,pragContext))) +
  geom_line(size=1.3,position=dodge) +
  geom_point(size=2.5,shape="square",position=dodge) +
  geom_errorbar(aes(ymin=YMin, ymax=YMax), width=.2, alpha=.7,  linetype="solid",position=dodge) +
  # facet_grid(size ~condition ) + 
  scale_color_manual(values=c(cbPalette[2],cbPalette[6])) +
  scale_x_discrete(breaks=c("click_prior","click_adj","click_noun"),
                   labels=c("Prior", "Adj", "Noun")) +
  xlab("Window") +
  ylab("log(P(Target)/P(Competitor))") #+
# theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1))
fig13
ggsave("../graphs/results-idt.pdf",width=4.5,height=2.5)

#ryskin fig 2
#line type as contrast? and alpha as target/competitor?
toplot =  d_test %>%
  select(workerid,pragContext,cond,click_prior,click_adj,click_noun,loc_target_pic,loc_competitor_pic,loc_contrast,loc_big_filler,loc_small_filler,instruction,trial_number,trial) %>%
  pivot_longer(names_to = "window", values_to = "selection",cols=click_prior:click_noun) %>% 
  mutate(target = case_when(loc_target_pic==selection ~ 1,
                            TRUE ~ 0)) %>% 
  mutate(competitor = case_when(loc_competitor_pic==selection ~ 1,
                                TRUE ~ 0)) %>% 
  # mutate(distractor = case_when(loc_target_pic!=selection & loc_competitor_pic !=selection ~ 1,
  #                               TRUE ~ 0)) %>% 
  group_by(cond,pragContext,window) %>%
  summarize(m_target=mean(target),m_competitor=mean(competitor),ci_low_target=ci.low(target),ci_high_target=ci.high(target),ci_low_competitor=ci.low(competitor),ci_high_competitor=ci.high(competitor)) %>%
  pivot_longer(names_to="location",values_to="Mean",cols=m_target:m_competitor) %>%
  mutate(CILow=ifelse(location=="m_target",ci_low_target,ifelse(location=="m_competitor",ci_low_competitor,0))) %>%
  mutate(CIHigh=ifelse(location=="m_target",ci_high_target,ifelse(location=="m_competitor",ci_high_competitor,0))) %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh) %>%
  #exclude distractor to better resemble ryskin fig. 2
  mutate(Region=fct_recode(location,"competitor"="m_competitor","target"="m_target")) %>%
  mutate(Region=fct_rev(Region)) %>%
  ungroup() %>% 
  mutate(window=fct_recode(window,prior="click_prior",adjective="click_adj",noun="click_noun")) %>% 
  mutate(window = fct_relevel(window,"prior","adjective")) %>% 
  mutate(pragContext=fct_recode(pragContext,reliable="good",unreliable="bad")) %>% 
  mutate(pragContext = fct_relevel(pragContext,"reliable","unreliable")) %>% 
  mutate(cond = fct_relevel(cond,"no_contrast","contrast"))

#reverse dark and light
#consider switching contrast and no contrast positions/roles
ryskin_f2 = ggplot(toplot, aes(x=window, y=Mean, color=pragContext, alpha = cond, group=interaction(pragContext, cond, Region))) +
  geom_line(aes(color=pragContext, linetype= Region),size=1.3) +
  geom_point(aes(color=pragContext),size=2.5,shape="square") +
  geom_errorbar(aes(ymin=YMin, ymax=YMax), width=.2, alpha=.3) +
  labs(color="Pragmatic context", alpha = "Condition") +
  scale_alpha_discrete(limits = c("contrast", "no_contrast"),range=c(1,.3))+
  scale_color_manual(values=c(ryskinPalette[1], ryskinPalette[3])) +
  xlab("Window") +
  ylab("Proportion of selections") +
  theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1))
ryskin_f2
ggsave(ryskin_f2, file="../graphs/ryskin_fig2_rep.pdf",width=9,height=4.5)

# Does the bad pragContext make us increasingly less likely to draw a contrastive inference?
# Subset to Adjective window bc that's where contrast effect shows up. 
# Plot contrast v no contrast and bad s good just in the adj window. X axis is trial number (over time analysis)
toplot =  d_test %>%
  select(workerid,response_times, pragContext,cond,click_adj,loc_target_pic,loc_competitor_pic,loc_contrast,loc_big_filler,loc_small_filler,instruction,trial_number) %>%  
  mutate(selection = click_adj) %>% 
  mutate(response_adj = str_extract(response_times, '\\s(.*?),')) %>% 
  mutate(response_adj = as.integer(gsub('\\s|,', "", response_adj))) %>% 
  mutate(target = case_when(loc_target_pic==selection ~ 1,
                            TRUE ~ 0)) %>% 
  mutate(competitor = case_when(loc_competitor_pic==selection ~ 1,
                                TRUE ~ 0)) %>%
  group_by(cond,pragContext, trial_number) %>%
  mutate(m_target=mean(target),m_competitor=mean(competitor),ci_low_target=ci.low(target),ci_high_target=ci.high(target),ci_low_competitor=ci.low(competitor),ci_high_competitor=ci.high(competitor)) %>%
  pivot_longer(names_to="location",values_to="Mean",cols=m_target:m_competitor) %>%
  mutate(CILow=ifelse(location=="m_target",ci_low_target,ifelse(location=="m_competitor",ci_low_competitor,0))) %>%
  mutate(CIHigh=ifelse(location=="m_target",ci_high_target,ifelse(location=="m_competitor",ci_high_competitor,0))) %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh) %>%
  mutate(Region=fct_recode(location,"competitor"="m_competitor","target"="m_target")) %>%
  mutate(Region=fct_rev(Region)) %>%
  ungroup() %>% 
  mutate(pragContext=fct_recode(pragContext,reliable="good",unreliable="bad")) %>% 
  mutate(pragContext = fct_relevel(pragContext,"reliable","unreliable")) #%>% 
#mutate(cond = fct_relevel(cond,"no_contrast","contrast"))
view(toplot)
toplot_target <- toplot %>% 
  filter(Region=='target')
contrastive_inf = ggplot(toplot_target, aes(x=trial_number, y=Mean, color=pragContext, linetype=cond, shape = cond, group=interaction(pragContext, cond))) +
  geom_smooth(method='lm')+
  geom_point() +
  labs(color="Pragmatic context", linetype = "Condition") +
  #scale_alpha_discrete(range=c(.33,.5,1))+
  #facet_grid(cond ~ pragContext ) + 
  scale_color_manual(values=c(ryskinPalette[1], ryskinPalette[3])) +
  xlab("Trial number (sequentially)") +
  ylab("Proportion of selections") +
  labs(title = "Target selections during adjective window") +
  theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1))
contrastive_inf
ggsave(contrastive_inf, file="../graphs/contrastive_inf_over_time.pdf",width=9,height=4.5)

# TODO:
# create a binary variable: TRUE if participant has seen filler trial before test trials?
# create a continuous var: how many filler trials have they seen before the first test trial?
# then, recreate the above plot with those ^^ vars as an additional variable. facet the plot using the above. 
d_test2 = df %>% 
  filter(trialType != "train") %>%
  droplevels()
toplot_trialType =  d_test2 %>%
  select(workerid,response_times, pragContext,cond,click_adj,loc_target_pic,loc_competitor_pic,loc_contrast,loc_big_filler,loc_small_filler,instruction,trial_number, trialType) %>%  
  mutate(selection = click_adj) %>% 
  mutate(response_adj = str_extract(response_times, '\\s(.*?),')) %>% 
  mutate(response_adj = as.integer(gsub('\\s|,', "", response_adj))) %>% 
  mutate(target = case_when(loc_target_pic==selection ~ 1,
                            TRUE ~ 0)) %>% 
  mutate(competitor = case_when(loc_competitor_pic==selection ~ 1,
                                TRUE ~ 0)) %>%
  group_by(cond,pragContext, trial_number) %>%
  mutate(m_target=mean(target),m_competitor=mean(competitor),ci_low_target=ci.low(target),ci_high_target=ci.high(target),ci_low_competitor=ci.low(competitor),ci_high_competitor=ci.high(competitor)) %>%
  pivot_longer(names_to="location",values_to="Mean",cols=m_target:m_competitor) %>%
  mutate(CILow=ifelse(location=="m_target",ci_low_target,ifelse(location=="m_competitor",ci_low_competitor,0))) %>%
  mutate(CIHigh=ifelse(location=="m_target",ci_high_target,ifelse(location=="m_competitor",ci_high_competitor,0))) %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh) %>%
  mutate(Region=fct_recode(location,"competitor"="m_competitor","target"="m_target")) %>%
  mutate(Region=fct_rev(Region)) %>%
  ungroup() %>% 
  mutate(pragContext=fct_recode(pragContext,reliable="good",unreliable="bad")) %>% 
  mutate(pragContext = fct_relevel(pragContext,"reliable","unreliable"))

#group_by: workerid
#df_f: filter for fillers
#df_t: filter for tests
# toplot_trialType$filler_first = NA # creates a new variable filled with NAs
# ind_f <- d_test2 %>% 
#   ???group_by(workerid) %>% 
#   filter(trialType=='filler') %>% 
#   min(trial_number)
# ind  <-  toplot_trialType$trial_number = 1 & toplot_trialType$trialType == 'filler'
# toplot_trialType$filler_first[ind] = v1


view(toplot_target)
contrastive_inf = ggplot(toplot_target, aes(x=trial_number, y=Mean, color=pragContext, linetype=cond, shape = cond, group=interaction(pragContext, cond))) +
  geom_smooth(method='lm')+
  geom_point() +
  labs(color="Pragmatic context", linetype = "Condition") +
  #scale_alpha_discrete(range=c(.33,.5,1))+
  #facet_grid(cond ~ pragContext ) + 
  scale_color_manual(values=c(ryskinPalette[1], ryskinPalette[3])) +
  xlab("Trial number (sequentially)") +
  ylab("Proportion of selections") +
  labs(title = "Target selections during adjective window") +
  theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1))
contrastive_inf
ggsave(contrastive_inf, file="../graphs/contrastive_inf_over_time.pdf",width=9,height=4.5)


##############
# Plot categorical versus eye movement data
#############

# load eye-tracking data from Ryskin 2019
# adj_tar <- read.delim("ryskin_eyetracking/PragTrain3_adj_window_for_tardur_tarDURdatastruct.txt")
# adjn_GLMM <- read.delim("ryskin_eyetracking/PragTrain3_adjnoun_for_GLMM_timesummLongForm.txt")
# adjn_tar <- read.delim("ryskin_eyetracking/PragTrain3_adjnoun_window_for_tardur_tarDURdatastruct.txt")
# noun_GLMM <- read.delim("ryskin_eyetracking/PragTrain3_noun_for_GLMM_timesummLongForm.txt")
# noun_tar <- read.delim("ryskin_eyetracking/PragTrain3_noun_window_for_tardur_tarDURdatastruct.txt")
# baseline = read.csv("sb_eyetracking/exp200ms_beselinedata.csv", header = TRUE)

# prior window
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
# pragtrain3_list1 = read_tsv('../data/ryskin_eyetracking/experiments_2-3_trials_list_1.txt')
# pragtrain3_list2 = read_tsv('../data/ryskin_eyetracking/experiments_2-3_trials_list_2.txt')
pragtrain3_lists = bind_rows(pragtrain3_list1,pragtrain3_list2) %>% 
  mutate(unique_ID = str_c(as.character(counterbalance),'_',trialID))
prior_df = prior_df %>% 
  left_join(pragtrain3_lists, by = c('uniqueID'='unique_ID'))
rm(pragtrain3_list1, pragtrain3_list2)

# expt2 reading in adj+noun window data for glmm
# reads in a large file - can be downloaded locally from Ryskin 2019's OSF page
pragtrain3_data = read_tsv('../data/ryskin_eyetracking/PragTrain3_adjnoun_for_GLMM_timesummLongForm.txt',
# pragtrain3_data = read_tsv("../data/PragTrain3_adjnoun_for_GLMM_timesummLongForm.txt",
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

pragtrain3_crit = pragtrain3_data %>% 
  group_by(subject,trialnum) %>% 
  mutate(target_AR1_corrected = lag(target_fix),
         compet_AR1_corrected = lag(compet_fix)) %>% 
  filter((trial_type == 'test' | trial_type == "train") & target_AR1 != 999) %>% 
  ungroup()

# Changed above line to include "train" trial types

# expt2 setting up factors and contrasts
pragtrain3_crit$timebin_rel_c = scale(pragtrain3_crit$timebin_rel,scale=F, center = T)
pragtrain3_crit$prag_context_cond = factor(pragtrain3_crit$prag_context_cond)
pragtrain3_crit$contrast_cond = factor(pragtrain3_crit$contrast_cond)

contrasts(pragtrain3_crit$prag_context_cond)<-c(-0.5,0.5)
contrasts(pragtrain3_crit$contrast_cond)<-c(0.5,-0.5)

contrasts(pragtrain3_crit$prag_context_cond)
contrasts(pragtrain3_crit$contrast_cond)

# expt2 adding half contrasts
pragtrain3_crit2 = pragtrain3_crit %>% 
  mutate(half= if_else(trialnum<151,"first","second"))

pragtrain3_crit2$trialorder_c = scale(pragtrain3_crit2$trialnum,scale=F,center=T)
pragtrain3_crit2$half<-factor(pragtrain3_crit2$half)
contrasts(pragtrain3_crit2$half)<-c(-0.5,0.5)
contrasts(pragtrain3_crit2$half)




##############
##############
# Plots for Linking Function
##############
##############

##############
# Merge Eyetracking and selection data
##############

# Eye tracking data by trial type
toplot_eye = pragtrain3_crit %>% 
  separate(audio,into=c("noun","size","wav")) %>% 
  select(contrast_cond,prag_context_cond,trial_type,timebin,target_fix,compet_fix) %>% #,
  filter(!(target_fix == 1 & compet_fix == 1)) %>% 
  mutate(other_fix = ifelse(target_fix + compet_fix == 0, 1, 0),
         window = ifelse(timebin <=200, "prior", ifelse(timebin <= 830, "adjective","noun"))) %>%
  pivot_longer(names_to = "Region", values_to = "fixation",cols=target_fix:other_fix) %>%
  mutate(Region = fct_recode(Region,"target" = "target_fix","competitor"="compet_fix","other"="other_fix")) %>% 
  group_by(contrast_cond,prag_context_cond,window,trial_type,Region) %>%
  summarize(prop_looks=mean(fixation)) %>% 
  ungroup() %>% 
  rename(pragContext = prag_context_cond, cond = contrast_cond, trialType = trial_type)

# Eye tracking data collapsed across trial type
toplot_eye_all = pragtrain3_crit %>% 
  separate(audio,into=c("noun","size","wav")) %>% 
  select(contrast_cond,prag_context_cond, timebin,target_fix,compet_fix) %>% #,
  filter(!(target_fix == 1 & compet_fix == 1)) %>% 
  mutate(other_fix = ifelse(target_fix + compet_fix == 0, 1, 0),
         window = ifelse(timebin <=200, "prior", ifelse(timebin <= 830, "adjective","noun"))) %>%
  pivot_longer(names_to = "Region", values_to = "fixation",cols=target_fix:other_fix) %>%
  mutate(Region = fct_recode(Region,"target" = "target_fix","competitor"="compet_fix","other"="other_fix")) %>% 
  group_by(contrast_cond,prag_context_cond,window,Region) %>%
  summarize(prop_looks=mean(fixation)) %>% 
  ungroup() %>% 
  rename(pragContext = prag_context_cond, cond = contrast_cond)

# Selection data by trial type
toplot_select =  d_test %>%
  select(workerid,pragContext,cond,click_prior,click_adj,click_noun,loc_target_pic,loc_competitor_pic,loc_contrast,loc_big_filler,loc_small_filler,instruction,trial_number,trial,trialType) %>%
  pivot_longer(names_to = "window", values_to = "selection",cols=click_prior:click_noun) %>% 
  mutate(target = case_when(loc_target_pic==selection ~ 1,
                            TRUE ~ 0)) %>% 
  mutate(competitor = case_when(loc_competitor_pic==selection ~ 1,
                                TRUE ~ 0)) %>% 
  mutate(other = case_when(target + competitor == 0 ~ 1,
                                TRUE ~ 0)) %>% 
  group_by(cond,pragContext,window, trialType) %>% #added trialType
  summarize(m_target=mean(target),m_competitor=mean(competitor),m_other=mean(other),ci_low_target=ci.low(target),ci_high_target=ci.high(target),ci_low_competitor=ci.low(competitor),ci_high_competitor=ci.high(competitor),ci_low_other=ci.low(other),ci_high_other=ci.high(other)) %>%
  ungroup() %>%
  mutate(info_target = paste(m_target, m_target-ci_low_target, m_target+ci_high_target), info_competitor=paste(info_competitor = paste(m_competitor, m_competitor-ci_low_competitor, m_competitor+ci_high_competitor)),info_other = paste(m_other, m_other-ci_low_other, m_other+ci_high_other)) %>% 
  select(cond,pragContext,window,trialType,info_target,info_competitor,info_other) %>% #added trialType
  pivot_longer(names_to="location",values_to="info_selections",cols=info_target:info_other) %>%
  separate(info_selections,into=c("prop_selections","ymin_selections","ymax_selections"),sep=" ") %>% 
  mutate(Region=fct_recode(location,"competitor"="info_competitor","target"="info_target","other"="info_other")) %>%
  mutate(Region=fct_rev(Region)) %>%
  ungroup() %>%  
  mutate(window=fct_recode(window,prior="click_prior",adjective="click_adj",noun="click_noun")) %>% 
  mutate(pragContext=fct_recode(pragContext,reliable="good",unreliable="bad")) %>% 
  mutate(pragContext = fct_relevel(pragContext,"reliable","unreliable")) %>%
  mutate(cond = fct_relevel(cond,"no_contrast","contrast")) %>% 
  select(cond, pragContext, window, Region, trialType, prop_selections, ymin_selections, ymax_selections)


# Selection data collapsed across trial types
toplot_select_all =  d_test %>%
  select(workerid,pragContext,cond,click_prior,click_adj,click_noun,loc_target_pic,loc_competitor_pic,loc_contrast,loc_big_filler,loc_small_filler,instruction,trial_number,trial) %>%
  pivot_longer(names_to = "window", values_to = "selection",cols=click_prior:click_noun) %>% 
  mutate(target = case_when(loc_target_pic==selection ~ 1,
                            TRUE ~ 0)) %>% 
  mutate(competitor = case_when(loc_competitor_pic==selection ~ 1,
                                TRUE ~ 0)) %>% 
  mutate(other = case_when(target + competitor == 0 ~ 1,
                           TRUE ~ 0)) %>% 
  group_by(cond,pragContext,window) %>%
  summarize(m_target=mean(target),m_competitor=mean(competitor),m_other=mean(other),ci_low_target=ci.low(target),ci_high_target=ci.high(target),ci_low_competitor=ci.low(competitor),ci_high_competitor=ci.high(competitor),ci_low_other=ci.low(other),ci_high_other=ci.high(other)) %>%
  ungroup() %>%
  mutate(info_target = paste(m_target, m_target-ci_low_target, m_target+ci_high_target), info_competitor=paste(info_competitor = paste(m_competitor, m_competitor-ci_low_competitor, m_competitor+ci_high_competitor)),info_other = paste(m_other, m_other-ci_low_other, m_other+ci_high_other)) %>% 
  select(cond,pragContext,window,info_target,info_competitor,info_other) %>% 
  pivot_longer(names_to="location",values_to="info_selections",cols=info_target:info_other) %>%
  separate(info_selections,into=c("prop_selections","ymin_selections","ymax_selections"),sep=" ") %>% 
  mutate(Region=fct_recode(location,"competitor"="info_competitor","target"="info_target","other"="info_other")) %>%
  mutate(Region=fct_rev(Region)) %>%
  ungroup() %>%  
  mutate(window=fct_recode(window,prior="click_prior",adjective="click_adj",noun="click_noun")) %>% 
  mutate(pragContext=fct_recode(pragContext,reliable="good",unreliable="bad")) %>% 
  mutate(pragContext = fct_relevel(pragContext,"reliable","unreliable")) %>%
  mutate(cond = fct_relevel(cond,"no_contrast","contrast")) %>% 
  select(cond, pragContext, window, Region, prop_selections, ymin_selections, ymax_selections)


# Merge the two together (by trial type)
toplot = left_join(toplot_select, toplot_eye, by=c("cond","pragContext","trialType", "window","Region")) %>% 
  mutate(window = fct_relevel(window,"prior","adjective")) %>% 
  mutate(ymin_selections = as.numeric(ymin_selections),ymax_selections = as.numeric(ymax_selections),prop_selections = as.numeric(prop_selections))
nrow(toplot)
names(toplot)

# Merge the two together (collapsed across trial type)
toplot_all = left_join(toplot_select_all, toplot_eye_all, by=c("cond","pragContext","window","Region")) %>% 
  mutate(window = fct_relevel(window,"prior","adjective")) %>% 
  mutate(ymin_selections = as.numeric(ymin_selections),ymax_selections = as.numeric(ymax_selections),prop_selections = as.numeric(prop_selections))
nrow(toplot_all)
names(toplot_all)


##############
# By condition and window plots
##############

# Plot proportion of looks vs. proportion of selections
# faceted by pragmatic condition
# collapsed across train and test trials
corr_by_pragCondition_all <- 
  ggplot(toplot_all, aes(x=prop_selections,y=prop_looks,color=Region,shape=cond)) +
  geom_point() +
  scale_color_manual(values=c(cbPalette[7],cbPalette[1],cbPalette[4],cbPalette[5])) +
  labs(
    shape="Contrast condition",
    # color="Window",
    x="Proportion of selections",
    y="Proportion of looks (Ryskin et al 2019, Exp. 2)") +
  xlim(0,1) +
  ylim(0,1) +
  facet_grid(pragContext~window) +
  ggtitle("Linking Hypothesis Test + Train Trials")
corr_by_pragCondition_all
ggsave("../graphs/corr_by_pragCondition_all.pdf", plot = corr_by_pragCondition_all, width = 5, units = "in")


# Plot proportion of looks vs. proportion of selections
# faceted by pragmatic condition
# For train trials
corr_by_pragCondition_train <- toplot %>%
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
  ggtitle("Linking Hypothesis Train Trials")
corr_by_pragCondition_train
ggsave("../graphs/corr_by_pragCondition_train.pdf", plot = corr_by_pragCondition_train, width = 5, units = "in")


# Plot proportion of looks vs. proportion of selections
# faceted by pragmatic condition
# For test trials
corr_by_pragCondition_test <- toplot %>%
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
  facet_grid(pragContext~window) +
  ggtitle("Linking Hypothesis Test Trials")
corr_by_pragCondition_test
ggsave("../graphs/corr_by_pragCondition_test.pdf", plot = corr_by_pragCondition_test, width = 5, units = "in")



##########
# Contrast Condition Plots
##########

# Plot proportion of looks vs. proportion of selections
# faceted by contrast set condition
# Collapsed across train and test trials
corr_by_contrastCondition_all <- toplot_all %>%
  ggplot(aes(x=prop_selections,y=prop_looks,color=Region,shape=pragContext)) +
  geom_point() +
  scale_color_manual(values=c(cbPalette[7],cbPalette[1],cbPalette[4],cbPalette[5])) +
  labs(
    shape="Contrast condition",
    x="Proportion of selections",
    y="Proportion of looks (Ryskin et al 2019, Exp. 2)") +
  xlim(0,1) +
  ylim(0,1) +
  facet_grid(cond~window)  +
  ggtitle("Linking Hypothesis Test + Train Trials")
corr_by_contrastCondition_all
ggsave("../graphs/corr_by_contrastCondition_all.pdf", plot = corr_by_contrastCondition_all, width = 5, units = "in")


# Plot proportion of looks vs. proportion of selections
# faceted by contrast set condition
# For train trials
corr_by_contrastCondition_train <- toplot %>%
  filter(trialType == "train") %>%
  ggplot(aes(x=prop_selections,y=prop_looks,color=Region,shape=pragContext)) +
  geom_point() +
  scale_color_manual(values=c(cbPalette[7],cbPalette[1],cbPalette[4],cbPalette[5])) +
  labs(
    shape="Contrast condition",
    x="Proportion of selections",
    y="Proportion of looks (Ryskin et al 2019, Exp. 2)") +
  xlim(0,1) +
  ylim(0,1) +
  facet_grid(cond~window)  +
  ggtitle("Linking Hypothesis Train Trials")
corr_by_contrastCondition_train
ggsave("../graphs/corr_by_contrastCondition_train.pdf", plot = corr_by_contrastCondition_train, width = 5, units = "in")

# Plot proportion of looks vs. proportion of selections
# faceted by contrast set condition
# For train test
corr_by_contrastCondition_test <- toplot %>%
  filter(trialType == "test") %>%
  ggplot(aes(x=prop_selections,y=prop_looks,color=Region,shape=pragContext)) +
  geom_point() +
  scale_color_manual(values=c(cbPalette[7],cbPalette[1],cbPalette[4],cbPalette[5])) +
  labs(
    shape="Contrast condition",
    x="Proportion of selections",
    y="Proportion of looks (Ryskin et al 2019, Exp. 2)") +
  xlim(0,1) +
  ylim(0,1) +
  facet_grid(cond~window) +
  ggtitle("Linking Hypothesis Test Trials")
corr_by_contrastCondition_test
ggsave("../graphs/corr_by_contrastCondition_test.pdf", plot = corr_by_contrastCondition_test, width = 5, units = "in")



##########
# Correlation By Window Plots
##########

# Correlation by window collasped across trial type
corr_by_window_all <- toplot_all %>%
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
  ggtitle("Correlations by Window Train + Test Trials")
corr_by_window_all
ggsave("../graphs/corr_by_window_all.pdf", plot = corr_by_window_all, width=6.5,height=3.5)

# Correlation by window train trials
corr_by_window_train <- toplot %>%
  filter(trialType == "train") %>%
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
  ylim(0,1) +
  ggtitle("Correlations by Window Train Trials")
corr_by_window_train
ggsave("../graphs/corr_by_window_train.pdf", plot = corr_by_window_train, width=6.5,height=3.5)

# Correlation by window test
corr_by_window_test <- toplot %>%
  filter(trialType == "test") %>%
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
  ylim(0,1) +
  ggtitle("Correlations by Window Test Trials")
corr_by_window_train
ggsave("../graphs/corr_by_window_test.pdf", plot = corr_by_window_test, width=6.5,height=3.5)



##########
# Overall Correlation Plots
##########
# Recreation casey's graph proportion of looks x proportion of selections
# no facets, just everything together

# Collapsed across train and tes ttrials
corr_of_everything_all <- toplot_all %>%
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
  ggtitle("Overall Correlations Train + Test Trials")
corr_of_everything_all
ggsave("../graphs/corr_everything_all.pdf", plot = corr_of_everything_all, width=5.5,height=3.5)


# For train trials
corr_of_everything_train <- toplot %>%
  filter(trialType == "train") %>%
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
  ggtitle("Overall Correlations Train Trials")
corr_of_everything_train
ggsave("../graphs/corr_everything_train.pdf", plot = corr_of_everything_train, width=5.5,height=3.5)

# For train trials
corr_of_everything_test <- toplot %>%
  filter(trialType == "test") %>%
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
  ggtitle("Overall Correlations Test Trials")
corr_of_everything_test
ggsave("../graphs/corr_everything_test.pdf", plot = corr_of_everything_test, width=5.5,height=3.5)


##########
# Correlation Values
##########

toplot_train <- toplot %>%
  filter(trialType == "train")

toplot_test <- toplot %>%
  filter(trialType == "test")

# overall correlation between eye movement and decision task data
cor.test(toplot_all$prop_selections,toplot_all$prop_looks) 
# .514. df=34, p = 0.001

# overall correlation between eye movement and decision task data
# Train trials
cor.test(toplot_train$prop_selections,toplot_train$prop_looks) 
# .308. df=34, p = 0.07

# overall correlation between eye movement and decision task data
# Test trials
cor.test(toplot_test$prop_selections,toplot_test$prop_looks) 
# .88. df=34, p < 0.0001



# correlation between eye movement and decision task data separately by time window
# across test and train
cors_window_all = toplot_all %>% 
  group_by(window) %>% 
  summarize(Correlation=round(cor.test(prop_selections,prop_looks)$estimate,2),P=round(cor.test(prop_selections,prop_looks)$p.value,5))
cors_window_all
#   Window           correlation    P
# 1 prior            0.89           0.00011
# 2 adjective        0.27           0.391  
# 3 noun             0.84           0.00063

# correlation between eye movement and decision task data separately by time window
# Train trials
cors_window_train = toplot_train %>% 
  group_by(window) %>% 
  summarize(Correlation=round(cor.test(prop_selections,prop_looks)$estimate,2),P=round(cor.test(prop_selections,prop_looks)$p.value,5))
cors_window_train
#   window    Correlation      P
# 1 prior            0.64 0.0252
# 2 adjective        0.37 0.238 
# 3 noun             0.5  0.0983

# correlation between eye movement and decision task data separately by time window
# Test trials
cors_window_test = toplot_test %>% 
  group_by(window) %>% 
  summarize(Correlation=round(cor.test(prop_selections,prop_looks)$estimate,2),P=round(cor.test(prop_selections,prop_looks)$p.value,5))
cors_window_test
#   window    Correlation     P
# 1 prior            0.97 0    
# 2 adjective        0.4  0.195
# 3 noun             0.99 0    


# correlation between eye movement and decision task data separately by pragmatic reliability condition
# Across train + test
cors_prag_all = toplot_all %>% 
  filter(window != "prior") %>% 
  group_by(pragContext) %>% 
  summarize(Correlation=round(cor.test(prop_selections,prop_looks)$estimate,2),P=round(cor.test(prop_selections,prop_looks)$p.value,5))
cors_prag_all
#   pragContext Correlation       P
# 1 reliable           0.77 0.00347
# 2 unreliable         0.59 0.0434 

# correlation between eye movement and decision task data separately by pragmatic reliability condition
# Across train
cors_prag_train = toplot_train %>% 
  filter(window != "prior") %>% 
  group_by(pragContext) %>% 
  summarize(Correlation=round(cor.test(prop_selections,prop_looks)$estimate,2),P=round(cor.test(prop_selections,prop_looks)$p.value,5))
cors_prag_train
#   pragContext Correlation       P
# 1 reliable           0.62 0.0325
# 2 unreliable         0.25 0.439 

# correlation between eye movement and decision task data separately by pragmatic reliability condition
# Across train
cors_prag_test = toplot_test %>% 
  filter(window != "prior") %>% 
  group_by(pragContext) %>% 
  summarize(Correlation=round(cor.test(prop_selections,prop_looks)$estimate,2),P=round(cor.test(prop_selections,prop_looks)$p.value,5))
cors_prag_test
#   pragContext Correlation       P
# 1 reliable           0.87 0.00023
# 2 unreliable         0.91 0.00005


# Stopped here


# Explicit beliefs by window and contrast
corr_window_contrast = toplot %>%
  group_by(window, cond) %>% 
  summarize(Correlation=round(cor.test(prop_selections,prop_looks)$estimate,2),P=round(cor.test(prop_selections,prop_looks)$p.value,5))
corr_window_contrast
# window    cond        Correlation       P
# <fct>     <fct>             <dbl>   <dbl>
# 1 prior     no_contrast        0.79 0.0630 
# 2 prior     contrast           0.99 0.00028
# 3 adjective no_contrast        0.98 0.0006 
# 4 adjective contrast          -0.29 0.575  
# 5 noun      no_contrast        0.83 0.0431 
# 6 noun      contrast           0.87 0.0233 

# Explicit beliefs by window and pragmatic context
corr_window_prag = toplot %>%
  group_by(window, pragContext) %>% 
  summarize(Correlation=round(cor.test(prop_selections,prop_looks)$estimate,2),P=round(cor.test(prop_selections,prop_looks)$p.value,5))
corr_window_prag
# window    pragContext Correlation       P
# <fct>     <fct>             <dbl>   <dbl>
# 1 prior     reliable           0.99 0.00009
# 2 prior     unreliable         0.78 0.0667 
# 3 adjective reliable           0.27 0.599  
# 4 adjective unreliable         0.34 0.507  
# 5 noun      reliable           0.96 0.00186
# 6 noun      unreliable         0.69 0.126  





# model summary: 
# - main effect of explicit beliefs in predicted direction
# - effect of explicit beliefs varies by window: bigger in noun than adj window
# effect of explicit beliefs varies by region: beliefs have less explanatory power for distractor looks (but no diff between target and competitor) --> overall, the correlation plots show greater "other" looks across the board than predicted by selection proportions

# contrasts(toplot$window)
# contrasts(toplot$window) = cbind("det.to.baseline"=c(1,0,0,0),"det.to.gender"=c(0,1,0,0),"det.to.noun"=c(0,0,0,1))
tomodel = cbind(toplot,myCenter(toplot[,c("prop_selections","pragContext","cond")]))

#cogsci 2020 paper model:
m = lm(prop_looks ~ cprop_selections + cprop_selections:window + cprop_selections:Region, data=tomodel)
summary(m)

# THIS IS THE ONE THAT IS GIVE IN CASEY's POSTER

# adding interactions with experimental conditions of interest also doesn't change anything
m = lm(prop_looks ~ cprop_selections + cprop_selections:window + cprop_selections:Region+ cprop_selections:cpragContext + cprop_selections:ccond, data=tomodel)
summary(m)

# model for only targets (because full model violates assumption of independence of samples)
targ = toplot %>% 
  filter(Region == "target") %>% 
  mutate(cprop_selections = prop_selections - mean(prop_selections))

m.targ = lm(prop_looks ~ cprop_selections + cprop_selections:window, data=targ)
summary(m.targ)

# Coefficients:
#                                   Estimate Std. Error    t value Pr(>|t|)    
#   (Intercept)                        0.35309    0.01954  18.067  < 2e-16 ***
#   cprop_selections                   2.02466    0.27601   7.336 3.60e-08 ***
#   cprop_selections:windowadjective  -1.98350    0.27683  -7.165 5.68e-08 ***
#   cprop_selections:windownoun       -1.82676    0.26931  -6.783 1.60e-07 ***
#   cprop_selections:Regionother      -0.01934    0.13127  -0.147   0.8838    
#   cprop_selections:Regioncompetitor  0.30878    0.16865   1.831   0.0771 . 


# submodels for each window -- NOT YET RUN
d_baseline = toplot %>% 
  filter(window == "baseline") %>% 
  mutate(cprop_selections = prop_selections - mean(prop_selections))

m.baseline = lm(prop_looks ~ cprop_selections*Region, data=d_baseline)
summary(m.baseline)

d_gender = toplot %>% 
  filter(window == "gender") %>% 
  mutate(cprop_selections = prop_selections - mean(prop_selections))

m.gender = lm(prop_looks ~ cprop_selections*Region, data=d_gender)
summary(m.gender)

d_det = toplot %>% 
  filter(window == "determiner+name") %>% 
  mutate(cprop_selections = prop_selections - mean(prop_selections))

m.det = lm(prop_looks ~ cprop_selections*Region, data=d_det)
summary(m.det)

d_noun = toplot %>% 
  filter(window == "noun") %>% 
  mutate(cprop_selections = prop_selections - mean(prop_selections))

m.noun = lm(prop_looks ~ cprop_selections*Region, data=d_noun)
summary(m.noun)

# Coefficients: (3 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                        0.28831    0.02183  13.204 3.40e-07 ***
#   cprop_selections                   0.31368    0.04143   7.572 3.43e-05 ***
#   Regionother                        0.13507    0.04143   3.260  0.00984 ** 
# Regioncompetitor                        NA         NA      NA       NA    
# cprop_selections:Regionother            NA         NA      NA       NA    
# cprop_selections:Regioncompetitor       NA         NA      NA       NA    

# TODO: make me a plot!
names(pragtrain3_data)
# ggplot(toplot, aes(x=prop_selections,y=prop_looks,color=Region,shape=pragContext)) +
names(toplot_eye)

toplot_time = pragtrain3_crit %>% 
  separate(audio,into=c("noun","size","wav")) %>% 
  # cat_small.
  # select(contrast_cond,prag_context_cond,timebin,noun,target_fix,compet_fix) %>% 
  select(contrast_cond,prag_context_cond,timebin,target_fix,compet_fix) %>% 
  filter(!(target_fix == 1 & compet_fix == 1)) %>% 
  mutate(other_fix = ifelse(target_fix + compet_fix == 0, 1, 0),
         window = ifelse(timebin <=200, "prior", ifelse(timebin <= 830, "adjective","noun"))) %>% 
  pivot_longer(names_to = "Region", values_to = "fixation",cols=target_fix:other_fix) %>%
  mutate(Region = fct_recode(Region,"target" = "target_fix","competitor"="compet_fix","other"="other_fix")) %>% 
  # group_by(contrast_cond,prag_context_cond,window,Region,noun) %>% 
  group_by(contrast_cond,prag_context_cond,window,Region, timebin) %>% 
  #summarize(prop_looks=mean(fixation),ci_low_looks=ci.low(fixation),ci_high_looks=ci.high(fixation)) %>%
  summarize(prop_looks=mean(fixation)) %>% 
  ungroup() %>% 
  #mutate(ymin_looks=prop_looks-ci_low_looks,ymax_looks=prop_looks+ci_high_looks) %>% 
  rename(pragContext = prag_context_cond, cond = contrast_cond)
toplot_time <- filter(toplot_time, Region != "other")
toplot_time$condition <- paste0(toplot_time$cond, ", ",toplot_time$pragContext)
toplot_time <- mutate(toplot_time, Region = fct_relevel(Region,"target","competitor"))

unique(toplot_time$condition)
view(ryskinPalette)

ggplot(toplot_time, aes(x=timebin, y=prop_looks)) +
  geom_line(size=1, aes(color=condition,linetype=Region)) +
  #geom_ribbon(aes(ymin=ymin,ymax=ymax,fill=determiner,group=interaction(determiner,size)),alpha=.3) +
  scale_fill_manual(values=c(ryskinPalette[1],ryskinPalette[3],ryskinPalette[2], ryskinPalette[4])) +
  scale_color_manual(values=c(ryskinPalette[1],ryskinPalette[3],ryskinPalette[2], ryskinPalette[4])) +
  geom_vline(aes(xintercept=200)) +
  geom_vline(aes(xintercept=830)) +
  #geom_text(data=windows,aes(label=window,x=x),y=.9,size=2) +
  xlab("Time in ms relative to audio onset") +
  ylab("Proportion of looks") +  
  scale_x_continuous(breaks=seq(-200,1800,by=200),minor_breaks = seq(-200,1800,by=200))
  #facet_wrap(~cond)
# theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1))
ggsave("../graphs/ryskin_fig2c_recreated.pdf",width=5.5,height=3.5)


