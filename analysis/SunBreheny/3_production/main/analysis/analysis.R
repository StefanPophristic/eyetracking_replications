#####################
#####################
# Stefan Pophristic
# May 10th, 2022
# ALPS LAB
# Sun and Breheny replication production study
##################### 
# This script is an analysis for the Sun and Breheny 2020 replication production
# study.
#
# The script takes in a csv file of the raw response data, cleans up the data
# be deleting participants with high error rates and incorrect trials, analyzes
# the surprisal of each word that occured in the original Sun and Breheny study
# based on the production data. It then takes the correlations from the Sun and 
# Breheny Incremental replication study (Degen, Kursat, and Leigh 2021), and finds
# how well the surprisal ratings are correlated to those correlations. 
#
# Surprisal is calculated as: 
# Surprisal = -log(P(Utterance))
# P(Utterance) = [number of instances of that utterance given a condition] / 
#               [total number of utterances given a condition]
#
# Input: 
#     production data:
#         3.1-trials_cleaned.csv = cleaned production data from experiment 3.1
#         3.2-trials_cleaned.csv = cleaned production data from experiment 3.2
#     selection data (from Degen, Kursat, Leigh 2021):
#         trials_merged.csv = raw data from incremental decision (selection) study
#     eye tracking data (from Sun and Breheny 2020:
#         exp200ms_beselinedata.csv = eyetracking data from "click on the" window
#         exp200ms_genderdata.csv = eyetracking data from the gender window
#         exp200ms_determiner.csv = eyetracking data from the determiner window
#         exp200ms_namedata.csv = eyetracking data from the name window
#         exp200ms_enddata.csv = eyetracking data from the noun window
#
#         
# The production data csv files were run through the analyzeResposes.R script and then 
# manually processed for any mistakes (as outlined in the SunBreheny > 3_production >
# main > readme.md) prior to importing it into this analysis. 
#
#
# Output:
#     determiner_surprisal.pdf: plot of surprisal values for determines in experiments 3.1 and 3.2 
#                                 = fig. 2 in Degen & Pophristic 2022
#     surprisal_correlation.pdf: plot of correlation between surprisal of determiners and
#                                 eyetracking and selection data correlations
#                                 = fig. 3 in Degen & Pophristic 2022
#     gender_surprisal_by_gender.pdf: plot of surprisal values for gender terms (boy/girl) in 
#                                 experiments 3.1 and 3.2, faceted by the gender.
#     gender_surprisal_by_size.pdf: plot of surprisal values for gender terms (boy/girl) in 
#                                 experiments 3.1 and 3.2, faceted by the set size.
#     noun_surprisal_exp1.pdf: plot of surprisal values for noun terms in experiment 3.1
#     noun_surprisal_exp2.pdf: plot of surprisal values for noun terms in experiment 3.2
#     
#
# all these output files can be found in the graphs folder
##################### 

# Load necessary packages

library(tidyverse)
library(lme4)
library(boot)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#load helper functions we use in ALPS lab analysis scripts
source("../shared/helpers.R") 

# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
theme_set(theme_bw())

#########
# Import Production Data
#########

df_3.1 = read.csv("../data/3.1/3.1-trials_cleaned.csv", header = TRUE)
df_3.2 = read.csv("../data/3.2/3.2-trials_cleaned.csv", header = TRUE)


#########
# Experiment Information
#########

nrow(df_3.1) # 2550 data points
nrow(df_3.2) # 2752 data points

length(unique(df_3.1$workerid)) # 51 participants run
length(unique(df_3.2$workerid)) # 51 participants run

totalNumTrials <- df_3.1 %>% filter(workerid == 63)
length(totalNumTrials)
# 56 total trials

totalExpTrials <- totalNumTrials %>% filter(ExpFiller == "Exp")
nrow(totalExpTrials)
# 36 experimental trials

totalExpTrials %>%
  group_by(target_figure3) %>%
  count()
# boy condition, n = 18
# girl condition, n = 18

totalExpTrials %>%
  group_by(size) %>%
  count()
# big condition, n = 18
# small condition, n = 18

totalExpTrials %>%
  group_by(setting) %>%
  count()
# 1 fruit           12
# 2 kitchenwares    12
# 3 stationeries    12

totalExpTrials %>%
  group_by(condition) %>%
  count()
# 1 all          12
# 2 num          12
# 3 some         12


totalFillerTrials <- totalNumTrials %>% filter(ExpFiller == "Filler")
nrow(totalFillerTrials)
#12 filler trials

totalPracTrials <- totalNumTrials %>% filter(ExpFiller == "Prac")
nrow(totalPracTrials)
# 2 practice trials for experiment 1

df_3.2 %>% filter(workerid == 120 & ExpFiller == "Prac") %>% nrow()
# 2 practice + 4 exposure trials for experiment 2


#############################################
#############################################
#
# Data Pre-processing
#
#############################################
#############################################

#Get rid of practice trials
df_3.1 <- df_3.1 %>%
  filter(df_3.1$ExpFiller == "Exp" | df_3.1$ExpFiller == "Filler")
df_3.2 <- df_3.2 %>%
  filter(df_3.2$ExpFiller == "Exp" | df_3.2$ExpFiller == "Filler")


# Get the columns that you will use
df_3.1 <- df_3.1 %>%
  select(workerid, ExpFiller, condition, determiner, size, response, setting, target_figure3, target_object3, trial_number, response_times,
         genderUsed, correctGender, correctNoun, detUsed, correctDet)

df_3.2 <- df_3.2 %>%
  select(workerid, ExpFiller, condition, determiner, size, response, setting, target_figure3, target_object3, trial_number, response_times,
         genderUsed, correctGender, correctNoun, detUsed, correctDet)



#############################################
#############################################
#
# Participant Exclusion
#
#############################################
#############################################


# Participant Exclusion Function
# Excludes participants based on: 
#   error rates >5%
#   no object label >50% of trials
#   single word response >50% of trials
#  
# Input: Data frame with raw data (df_3.1 or df_3.2)
# Output: Same data frame but with participants excluded

participantExclusion <- function(DF) {
  # Get by participant accuracy statistics
  participantStatsNoun <- DF %>%
    group_by(workerid, correctNoun) %>%
    count(vars = "correctNoun") %>%
    select(-vars) %>%
    rename(correctNounCount = n)
  
  participantStatsGender <- DF %>%
    group_by(workerid, correctGender) %>%
    count(vars = "correctGender") %>%
    select(-vars) %>%
    rename(correctGenderCount = n)
  
  participantStatsDetUsed <- DF %>%
    group_by(workerid, detUsed) %>%
    count(vars = "detUsed") %>%
    select(-vars) %>%
    rename(detCount = n)
  
  participantStatsCorrectDet <- DF %>%
    group_by(workerid, correctDet) %>%
    count(vars = "correctDet") %>%
    select(-vars) %>%
    rename(correctDetCount = n)
  
  numTrialsPerParticipant <- nrow(DF)
  
  #list that will hold all workerids of people with high error rates
  ids <- list()
  
  #######
  # Exclude participants if they had an error rate > 5%
  #######
  highErrorRatePreExclusionNum <- nrow(DF)
  
  ids <- DF %>%
    group_by(workerid) %>%
    mutate(exclusion = case_when(
      correctGender == 0 ~ 0,
      correctNoun == 0 ~ 0,
      correctDet == 0 ~ 0,
      TRUE ~ 1)) %>%
    count(exclusion) %>%
    filter(exclusion == 0 & n >= 24) %>%
    select(workerid) %>%
    pull(workerid)
  
  highErrorRatePostExclusionNum <- nrow(DF)
  
  highErrorRateTotalExclusionNum <- highErrorRatePreExclusionNum - highErrorRatePostExclusionNum
  print(paste("Total number of participants excluded due to 5% error rate: ", 
              highErrorRateTotalExclusionNum))
  
  # Get rid of all participants with >5% error rates
  DF <- DF %>%
    filter(!(workerid %in% ids))
  
  #######
  # Exclude participants that do not use an object label > 50% of the time
  #######
  
  numTrialsPreNoNounExclusion <- nrow(DF)
  
  DF <- DF %>%
    mutate(noNoun = case_when(
      correctNoun == "noNoun" ~ 1,
      TRUE ~ 0)) %>%
    group_by(workerid) %>%
    mutate(noNounResponseRate = sum(noNoun)/48) %>%
    filter(noNounResponseRate < 0.5) %>%
    select(-c(noNoun, noNounResponseRate))
  
  # Print number of participants excluded due to criterion
  numTrialsPostNoNounExclusion <- nrow(DF)
  noNounParticipantExclusionNum <- (numTrialsPreNoNounExclusion - numTrialsPostNoNounExclusion)/48
  print(paste("Number of participants excluded due to > 50% rate of use of no noun responses: ", 
              noNounParticipantExclusionNum))
  
  #######
  # Exclude participants that answer with single word utterances > 50% of the time
  #######
  
  numTrialsPreSingleExclusion <- nrow(DF)
  
  DF <- DF %>%
    mutate(single = case_when(
      grepl(" ", response) ~ 0,
      TRUE ~ 1)) %>%
    group_by(workerid) %>%
    mutate(singleResponseRate = sum(single)/48) %>%
    filter(singleResponseRate < 0.5) %>%
    select(-c(single, singleResponseRate))
  
  numTrialsAfterSingleExclusion <- nrow(DF)
  
  postSingleWordParticipantExclusionNum <- (numTrialsPreSingleExclusion - numTrialsAfterSingleExclusion)/48
  
  print(paste("Number of participants excluded due to > 50% rate of use of single word utterances: ", 
              postSingleWordParticipantExclusionNum))
  
  # Total exclusion rate:
  print(paste("Number of participants excluded: ",(length(ids) + noNounParticipantExclusionNum + postSingleWordParticipantExclusionNum)))
  
  return(DF)
}

df_3.1 <- participantExclusion(df_3.1)
# "Total number of participants excluded due to 5% error rate:  0"
# "Number of participants excluded due to > 50% rate of use of no noun responses:  0"
# "Number of participants excluded due to > 50% rate of use of single word utterances:  0"
# "Number of participants excluded:  0"

df_3.2 <- participantExclusion(df_3.2)
# "Total number of participants excluded due to 5% error rate:  0"
# "Number of participants excluded due to > 50% rate of use of no noun responses:  1"
# "Number of participants excluded due to > 50% rate of use of single word utterances:  1"
# "Number of participants excluded:  2"

#############################################
#############################################
#
# Response Time Analysis
#
#############################################
#############################################

# Response Time Graph Function
#
# Plot graph of response times
# Input: data frame with raw data
# Output: ggplot of response time distribution

responseTimeGraph <- function(DF) {
  # log-transform response times
  DF$logRT = log(DF$response_times)
  
  # get mean response time and potential SD cutoff, in this case 2.5 SDs
  mean_rt = mean(DF$logRT)
  sd_cutoff = 2.5*sd(DF$logRT)
  
  # see trials that would get cut off
  View(DF[DF$response_times < exp(mean_rt-sd_cutoff),]) # there is nobody on the low end, but if you search for response times < 2500 (ie 2.5 seconds), you'll see this is mostly one person who never typed the gender. we may want to exclude participants who never or rarely produced gender, since they are arguably not doing the same task as the others (these appear to be workers 95 and 128)
  View(DF[DF$response_times > exp(mean_rt+sd_cutoff),]) # the very slow people produce reasonable referring expressions. my hunch is that because we're having people type and there are big differences in how fast people type, some are just turning up as very slow, but they shouldn't be excluded.
  
  
  # show what would get cut off
  ggplot(DF, aes(x=logRT)) + 
    geom_histogram(binwidth=.1) +
    geom_vline(xintercept=mean_rt-sd_cutoff) +
    geom_vline(xintercept=mean_rt+sd_cutoff) %>%
    return()
}

# Commented out to make running the rest of the code faster
#responseTimeGraph(df_3.1)
#responseTimeGraph(df_3.2)


#############################################
#############################################
#
# Trial Exclusions
#
#############################################
#############################################


# Trial Exclusion Function
# Excludes trials based on:
#   wrong object label (noun) used 
#   wrong child label (gender) used
#   wrong determiner used
#   no object label (noun) used
#   single word response
# Input: data frame with raw data
# Ouput: data frame with only experimental trials and without excluded trials

trialExclusion <- function(DF) {
  
  # Exclude trials of JUST experimental trails
  DF <- DF %>%
    filter(ExpFiller == "Exp")
  
  print(paste("Number of trials pre exclusions: ", nrow(DF)))
  
  # Get rid of trials with wrong nouns
  preNounExclusionNum <- nrow(DF)
  DF <- DF %>%
    filter(correctNoun != 0)
  
  postNounExclusionNum <- nrow(DF)
  trialExclusionNoun <- preNounExclusionNum - postNounExclusionNum
  print(paste("Number of trials excluded due to wrong Noun: ",trialExclusionNoun))

  # get rid of trials with wrong gender
  preGenderExclusionNum <- nrow(DF)
  DF <- DF %>%
    filter(correctGender != 0)
  
  postGenderExclusionNum <- nrow(DF)
  trialExclusionGender <- preGenderExclusionNum - postGenderExclusionNum
  print(paste("Number of trials excluded due to wrong Gender: ",trialExclusionGender))

  # Get rid of trials with wrong determiner
  preDetExclusionNum <- nrow(DF)
  DF <- DF %>%
    filter(correctDet != 0)
  
  postDetExclusionNum <- nrow(DF)
  trialExclusionDet <- preDetExclusionNum - postDetExclusionNum
  print(paste("Number of trials excluded due to wrong Determiner: ",trialExclusionDet))
  
  # Get rid of trails where the participant did not use a noun
  preNoNounExclusionNum <- nrow(DF)
  test <- DF %>%
    filter(correctNoun != "noNoun")
  
  postNoNounExclusionNum <- nrow(test)
  trialExclusionNoNoun <- preNoNounExclusionNum - postNoNounExclusionNum
  print(paste("Number of trials excluded due to no noun in response: ",trialExclusionNoNoun))

  # Get rid of trials where the participant used only a single word
  preSingleWordExclusionNum <- nrow(DF)
  DF <- DF %>%
    filter(grepl(" ", response))
  
  postSingleWordExclusionNum <- nrow(DF)
  trialExclusionSingleWord <- preSingleWordExclusionNum - postSingleWordExclusionNum
  print(paste("Number of trials excluded due to single word responses: ",trialExclusionSingleWord))

  # Print total number of trials excluded
  trialsExcluded <- trialExclusionNoun + trialExclusionGender + trialExclusionDet + trialExclusionNoNoun + trialExclusionSingleWord
  print(paste("Total number of trials excluded due to incorrect answers: ", trialsExcluded))

  print(paste("Number of trials post exclusion: ", nrow(DF)))
  
  return(DF)
}

df_3.1 <- trialExclusion(df_3.1)
# "Number of trials pre exclusions:  1836"
# "Number of trials excluded due to wrong Noun:  17"
# "Number of trials excluded due to wrong Gender:  3"
# "Number of trials excluded due to wrong Determiner:  6"
# "Number of trials excluded due to no noun in response:  0"
# "Number of trials excluded due to single word responses:  0"
# "Total number of trials excluded due to incorrect answers:  26"
# "Number of trials post exclusion:  1810"

df_3.2 <- trialExclusion(df_3.2)
# "Number of trials pre exclusions:  1764"
# "Number of trials excluded due to wrong Noun:  16"
# "Number of trials excluded due to wrong Gender:  2"
# "Number of trials excluded due to wrong Determiner:  6"
# "Number of trials excluded due to no noun in response:  0"
# "Number of trials excluded due to single word responses:  13"
# "Total number of trials excluded due to incorrect answers:  37"
# "Number of trials post exclusion:  1727"


##########
# Merge two data frames
##########

df_3.1 <- df_3.1 %>%
  mutate(experiment = 1)

df_3.2 <- df_3.2 %>%
  mutate(experiment = 2)

df <- rbind(df_3.1, df_3.2)

# Change target object labels for convenience
df <- df %>%
  mutate(target_object3 = as.character(target_object3)) %>%
  mutate(target_object3 = case_when(
    target_object3 == "2_apples" | target_object3 == "3_apples" ~ "apples",
    target_object3 == "2_bananas" | target_object3 == "3_bananas" ~ "bananas",
    target_object3 == "2_pears" | target_object3 == "3_pears" ~ "pears",
    target_object3 == "2_oranges" | target_object3 == "3_oranges" ~ "oranges",
    
    target_object3 == "2_forks" | target_object3 == "3_forks" ~ "forks",
    target_object3 == "2_knives" | target_object3 == "3_knives" ~ "knives",
    target_object3 == "2_spoons" | target_object3 == "3_spoons" ~ "spoons",
    target_object3 == "2_plates" | target_object3 == "3_plates" ~ "plates",
    
    target_object3 == "2_pencils" | target_object3 == "3_pencils" ~ "pencils",
    target_object3 == "2_erasers" | target_object3 == "3_erasers" ~ "erasers",
    target_object3 == "2_rulers" | target_object3 == "3_rulers" ~ "rulers",
    target_object3 == "2_scissors" | target_object3 == "3_scissors" ~ "scissors")) %>%
  rename(noun = target_object3,
         figure = target_figure3)
  

#############################################
#############################################
#
# Surprisal Calculations + Graphs
#
#############################################
#############################################


###############
# Determiner Surprisal
###############

# Cleanup of determiner names:
#   Change determiners not originally found in 
#       Sun and Breheny 2020 to "other" category
#   Change "two" and "three" to "num"
dfDet <- df %>%
  mutate(detUsed = as.character(detUsed)) %>%
  mutate(detUsed = case_when(
    detUsed != "noDet" & detUsed != "some" & detUsed != "all" &
      detUsed != "two" & detUsed != "three" ~ "other",
    detUsed == "two" | detUsed == "three" ~ "num",
    TRUE ~ detUsed))

# Keep only columns you need (size, noun, and determiner used)
# And count the number of instances of determiner per (size x noun)
dfDet <- dfDet %>%
  group_by(experiment, size, noun, detUsed) %>%
  count() %>%
  ungroup()

# Add in all the size x noun x determiner combinations which were not observed
allCombinations <- unique(expand.grid(c(1,2), dfDet$size, dfDet$noun, c("all", "some", "num", "noDet", "other")))
allCombinations <- allCombinations %>%
  rename(experiment = Var1, size = Var2, noun = Var3, detUsed = Var4)

dfDet <- right_join(dfDet, allCombinations, by = c("experiment", "size", "noun", "detUsed"))

# Change NA occurances with 0
dfDet <- dfDet %>%
  mutate(n = replace(n, is.na(n), 0))

# Count occurances
occuranceCount <- dfDet %>%
  group_by(experiment, detUsed) %>%
  mutate(newN = sum(n)) %>%
  select(experiment, detUsed, newN) %>%
  unique()
occuranceCount
# experiment detUsed  newN
# <dbl> <chr>   <dbl>
# 1 noDet     566
# 1 num      1182
# 1 some       48
# 1 other      14
# 1 all         0
# 2 all       240
# 2 noDet     305
# 2 num      1019
# 2 some      106
# 2 other      57

# Get total number of utterances per condition (size x noun)
totalSums <- dfDet %>%
  group_by(experiment, size, noun) %>%
  summarize(sum = sum(n))

# Merge the two together
dfDet <- merge(dfDet, totalSums, by = c("experiment", "size", "noun"))

#Calculate suprisal
# Surprisal = -log([# of instances of determiner use given size and noun condition] /
# [total number of trials of the given size and noun condition])
dfDet <- dfDet %>%
  mutate(probability = (n/sum)) %>%
  mutate(probability = case_when(probability == 0 ~ 0.0001,
                                 TRUE ~ probability)) %>%
  mutate(surprisal = -log2(probability))

lmer_data <- dfDet %>%
  mutate(detUsed = fct_relevel(detUsed,"some"))

lmer_data_3.1 <- lmer_data %>%
  filter(experiment == "1")
  
lmer_data_3.2 <- lmer_data %>%
  filter(experiment == "2")

# Run linear model tobackup the claims
m_3.1 = lmer(surprisal ~ detUsed*size + (1|noun), data=lmer_data_3.1)
summary(m_3.1)

m_3.2 = lmer(surprisal ~ detUsed*size + (1|noun), data=lmer_data_3.2)
summary(m_3.2)

#Check that probability values were computed correctly
# this should yield values of 1
probability <- dfDet %>%
  mutate(new = (2^(-surprisal))) %>%
  group_by(experiment, size, noun) %>%
  summarise(probs = sum(new))

########
# Graph determiner surprisal
########

# add error bars into data frame
surprisalMeanAndCI <- dfDet %>%
  group_by(experiment, size,detUsed) %>%
  summarize(
    mean_surprisal = mean(surprisal),
    CI.Low = ci.low(surprisal),
    CI.High = ci.high(surprisal))

surprisalMeanAndCI$experiment<- surprisalMeanAndCI$experiment%>%
  factor()

surprisalMeanAndCI <- surprisalMeanAndCI %>%
  mutate(detUsed = fct_relevel(detUsed, "all", "some","num","noDet"))

# New facet label names for supp variable
exp.labs <- c("Exp. 1", "Exp. 2")
names(exp.labs) <- c("1", "2")

# Create graph
graphSurprisalDetByNoun <- surprisalMeanAndCI %>% 
  ungroup() %>% 
  mutate(YMin = mean_surprisal - CI.Low, 
         YMax = mean_surprisal + CI.High) %>% 
  ggplot(aes(y=mean_surprisal, x=detUsed, color = size)) + 
  facet_grid(. ~ experiment,
             labeller = labeller(experiment = exp.labs)) +
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

ggsave(filename = "../graphs/determiner_surprisal.pdf", plot = graphSurprisalDetByNoun,
       width = 5, height = 4, device = "pdf")


########
# Gender Surprisal
########

# This is not reported in the cogsci paper

# Create data frame with counts of all gender utterances
# based off of size and target figure conditions
gender <- df %>%
  group_by(experiment, figure, genderUsed, size, condition) %>%
  count()

# Code all utterances that were not "boy" "girl" and "noGender" as "other"
gender <- gender %>%
  mutate(genderUsed = as.character(genderUsed)) %>%
  mutate(genderUsed = case_when(
    genderUsed != "boy" & genderUsed != "girl" & genderUsed != "noGender" ~ "other",
    TRUE ~ genderUsed
  ))

# Resum values after collapsing different gender utterances into "other"
gender <- gender %>%
  group_by(experiment, figure, genderUsed, size, condition) %>%
  summarize(n = sum(n))

# Get number of instances of the target utterance (e.g. "boy" for boy-condition)
# and append that as a new column
genderTemp <- gender %>%
  filter(figure == genderUsed) %>%
  mutate(targetUtteranceN = n)

gender <- left_join(gender, genderTemp, by = c("experiment", "figure", "size", "condition")) %>%
  select(-c(genderUsed.y, n.y)) %>%
  rename(genderUsed = genderUsed.x, n = n.x)

# If no target utterances were observed, they will come up as NA in the targetUtteranceN
# column --> replace those values with 0
gender <- mutate_at(gender, "targetUtteranceN", ~replace(., is.na(.), 0))


# In order to have correlation analysis, we have to have surprisals of not INF
#   which means we can't have observations of target utterances of 0
#   so we replace all observations of target utterances of 0 with 0.0001
gender <- gender %>%
  mutate(targetUtteranceN = replace(targetUtteranceN, targetUtteranceN == 0, 0.0001))

# Calculate Surprisal
# Surprisal = -log(P(Utterance))
# P(Utterance) = [number of instances of that utterance given a condition] / [total number of utterances given a condition]
surprisalGender<- gender %>%
  group_by(experiment, figure, size, condition) %>%
  summarize(surprisal = -log2(targetUtteranceN / sum(n))) %>%
  distinct()


########
# Graph Gender Surprisal
########

graphSurprisalGenderBySize <- surprisalGender %>%
  ggplot(aes(fill = condition, y=surprisal, x=figure, color = condition)) + 
  facet_grid(experiment ~ size,
             labeller = labeller(experiment = exp.labs)) +
  geom_point(size = 4) +
  scale_color_manual(values=c("olivedrab", "lightsalmon3", "skyblue3")) +
  theme(text = element_text(size = 16), plot.title = element_text(hjust = 0.5, size = 20)) +
  ylab("Surprisal") +
  xlab("Expected Gender Term") +
  ggtitle("Surprisal for two expected gender terms") 

graphSurprisalGenderBySize
ggsave(filename = "../graphs/gender_surprisal_by_size.pdf", plot = graphSurprisalGenderBySize,
       width = 5, height = 4, device = "pdf")

graphSurprisalGenderByGender <- surprisalGender %>%
  ggplot(aes(fill = size, y=surprisal, x=condition, color = size)) + 
  facet_grid(experiment ~ figure,
             labeller = labeller(experiment = exp.labs)) +
  geom_point(size = 4) +
  scale_color_manual(values=c("olivedrab", "lightsalmon3", "skyblue3")) +
  theme(text = element_text(size = 16), plot.title = element_text(hjust = 0.5, size = 20)) +
  ylab("Surprisal") +
  xlab("Condition") +
  ggtitle("Surprisal for two expected gender terms") 

graphSurprisalGenderByGender
ggsave(filename = "../graphs/gender_surprisal_by_gender.pdf", plot = graphSurprisalGenderByGender,
       width = 5, height = 4, device = "pdf")


########
# Calculate Noun Surprisal
########

# This is not reported in the cogsci paper

# Create data frame with counts of all gender utterances
# based off of size and target figure conditions
noun <- df %>%
  group_by(experiment, condition, size, figure, noun, correctNoun) %>%
  count()

# Get number of instances of the target utterance (e.g. "apples" for apples-condition)
# and append that as a new column
# The noun data frame contains counts of both the target utterance and other utterances
#   per condition
nounTemp <- noun %>%
  filter(correctNoun == 1) %>%
  mutate(targetUtteranceN = n)

noun <- left_join(noun, nounTemp, by = c("experiment", "figure", "noun", "size", "condition")) %>%
  select(-c(correctNoun.y, n.y)) %>%
  rename(correctNoun = correctNoun.x, n = n.x)

# If no target utterances were observed, they will come up as NA in the targetUtteranceN
# column --> replace those values with 0
noun <- mutate_at(noun, "targetUtteranceN", ~replace(., is.na(.), 0))


# In order to have correlation analysis, we have to have surprisals of not INF
#   which means we can't have observations of target utterances of 0
#   so we replace all observations of target utterances of 0 with 0.0001
noun <- noun %>%
  mutate(targetUtteranceN = replace(targetUtteranceN, targetUtteranceN == 0, 0.0001))

# Calculate Surprisal
# Surprisal = -log(P(Utterance))
# P(Utterance) = [number of instances of that utterance given a condition] / [total number of utterances given a condition]
surprisalNoun <- noun %>%
  group_by(experiment, noun, size, condition, figure) %>%
  summarize(surprisal = -log2(targetUtteranceN / sum(n))) %>%
  distinct()

########
# Graph Noun Surprisal
########

# Plot noun surprisal for Experiment 1
graphSurprisalNounExp1 <- surprisalNoun %>%
  filter(experiment == 1) %>%
  ggplot(aes(fill=condition, y=surprisal, x=noun, color=condition, shape = condition)) + 
  facet_grid(size ~ figure) + 
  geom_point(size = 4) +
  theme(text = element_text(size = 16), plot.title = element_text(hjust = 0.5, size = 20)) +
  scale_color_manual(values=c("olivedrab", "lightsalmon3", "skyblue3")) + #"darkorange2", "steelblue4"
  scale_shape_manual(values=c(19, 3, 4)) +
  ylim(-1, 1) + 
  ylab("Surprisal") +
  xlab("Expected Noun") +
  ggtitle("Surprisal for expected nouns for Experiment 1") 

graphSurprisalNounExp1
ggsave(filename = "../graphs/noun_surprisal_exp1.pdf", plot = graphSurprisalNounExp1,
       width = 5, height = 4, device = "pdf")

# Plot noun surprisal for Experiment 2
graphSurprisalNounExp2 <- surprisalNoun %>%
  filter(experiment == 2) %>%
  ggplot(aes(fill=condition, y=surprisal, x=noun, color=condition, shape = condition)) + 
  facet_grid(size ~ figure) + 
  geom_point(size = 4) +
  theme(text = element_text(size = 16), plot.title = element_text(hjust = 0.5, size = 20)) +
  scale_color_manual(values=c("olivedrab", "lightsalmon3", "skyblue3")) + #"darkorange2", "steelblue4"
  scale_shape_manual(values=c(19, 3, 4)) +
  ylim(-1, 1) + 
  ylab("Surprisal") +
  xlab("Expected Noun") +
  ggtitle("Surprisal for expected nouns for Experiment 2") 

graphSurprisalNounExp2
ggsave(filename = "../graphs/noun_surprisal_exp2.pdf", plot = graphSurprisalNounExp2,
       width = 5, height = 4, device = "pdf")

##############################################
#############################################
#
# Correlation Analysis + Graphs
#
#############################################
#############################################

##############################################
##
## Surprisal Calculation
## 
#############################################

# Talk about how the split up is different here!

# Create data frame with counts of all determiner utterances
# based off of size and target figure conditions
det <- df %>%
  group_by(experiment, condition, size, figure, detUsed) %>%
  count()

# Code all number utterances as "num"
# note: only possible numbers are "two" and "three"
#  the numbers "one" and "four" were only found in filler trials
det <- det %>%
  mutate(detUsed = as.character(detUsed)) %>%
  mutate(detUsed = case_when(
    detUsed == "two" | detUsed == "three" ~ "num",
    TRUE ~ detUsed
  ))

# Resum values after collapsing different det utterances into "other"
det <- det %>%
  group_by(experiment, condition, size, figure, detUsed) %>%
  summarize(n = sum(n))

# Get number of instances of the target utterance (e.g. "all" for all-condition)
# and append that as a new column
detTemp <- det %>%
  filter(condition == detUsed) %>%
  mutate(targetUtteranceN = n)

det <- left_join(det, detTemp, by = c("experiment", "condition", "size", "figure")) %>%
  select(-c(detUsed.y, n.y)) %>%
  rename(determiner = detUsed.x, n = n.x)

# If no target utterances were observed, they will come up as NA in the targetUtteranceN
# column --> replace those values with 0
det <- mutate_at(det, "targetUtteranceN", ~replace(., is.na(.), 0))

surprisalDet <-det %>%
  group_by(experiment, condition, size, figure) %>%
  mutate(probability = (targetUtteranceN/sum(n))) %>%
  mutate(probability = case_when(probability == 0 ~ 0.0001,
                                 TRUE ~ probability)) %>%
  mutate(surprisal = -log2(probability))

##############################################
##
## IMPORT ANALYSIS FROM INCREMENTAL STUDY
## 
#############################################

# This analysis was (ever so slightly) adapted from Degen, Kursat, and Leigh 2021
# You can find the original analysis script under the following link:
# https://github.com/thegricean/eyetracking_replications.git
# the analysis is taken from: analysis > SunBreheny > 1_incremental >
# main > rscripts > graphs.R

df = read.csv("../../../1_incremental/main/data/trials_merged.csv", header = TRUE)
demo = read.csv("../../../1_incremental/main/data/subject_info_merged.csv", header = TRUE)

#formatting
df$response = gsub(" ","",df$response)
df$response = gsub("\\[","",df$response)
df$response = gsub("\\]","",df$response)
df$response = gsub("\\'","",df$response)
df$response = gsub("AOI","",df$response)

df = df %>%
  group_by(workerid)%>%
  mutate(trial_number = seq(1:n())) %>%
  ungroup() %>%
  mutate(trial_group = ifelse(trial_number<31,"first_half","second_half")) %>% 
  mutate(item=word(as.character(instruction3), -1))

df = separate(df,response,into=c("click1","click2","click3","click4"),sep=",")

### EXCLUSIONS
df = df %>%
  mutate(selection_correct = ifelse(as.numeric(click4) == as.numeric(target1),1,ifelse(as.numeric(click4) == as.numeric(target2),1,0)))

table(df$selection_correct) # 665 incorrect responses

# exclude anyone with < 95% correct selections
accuracy = df %>% 
  filter(ExpFiller != "Prac") %>% 
  group_by(workerid) %>% 
  tally(selection_correct) %>% 
  mutate(correct=n/48) 

toexclude = accuracy %>% 
  filter(correct < .95)

length(toexclude$workerid) # exclude 29 subjects
length(toexclude$workerid)/length(accuracy$workerid) # exclude 24% of subjects

df = df %>% 
  filter(!workerid %in% toexclude$workerid)

unique(demo$language) # no exlusions (some people left it blank?)

# trials with incorrect selections
df = df %>% 
  filter(selection_correct==1)

nrow(df) # 4348

# get only experimental trials (no fillers) for further analysis
df = df %>% 
  filter(ExpFiller=="Exp") %>%
  droplevels()

# ### PART II: PLOT CATEGORICAL DATA AGAINST EYE MOVEMENT DATA
# 
baseline = read.csv("../../../1_incremental/main/data/sb_eyetracking/exp200ms_beselinedata.csv", header = TRUE)
gender = read.csv("../../../1_incremental/main/data/sb_eyetracking/exp200ms_genderdata.csv", header = TRUE)
determiner = read.csv("../../../1_incremental/main/data/sb_eyetracking/exp200ms_determiner.csv", header = TRUE)
name = read.csv("../../../1_incremental/main/data/sb_eyetracking/exp200ms_namedata.csv", header = TRUE)
end = read.csv("../../../1_incremental/main/data/sb_eyetracking/exp200ms_enddata.csv", header = TRUE)

# order should be: baseline / gender / determiner + name / noun
g = rbind(baseline,gender,determiner,name,end)  %>%
  mutate(item=word(as.character(instruction), -1))

# re-load incremental decision data
s = read.csv("../../../1_incremental/main/data/trials_merged.csv", header = TRUE)  %>%
  mutate(item=word(as.character(instruction3), -1))

s$response = gsub(" ","",s$response)
s$response = gsub("\\[","",s$response)
s$response = gsub("\\]","",s$response)
s$response = gsub("\\'","",s$response)
s$response = gsub("AOI","",s$response)


selection = s %>%
  filter(ExpFiller=="Exp") %>%
  separate(response,into=c("baseline","gender","determiner+name","noun"),sep=",") %>%
  gather(window,location,baseline:noun) %>%
  select(workerid,Prime,condition,determiner,size,target_figure3,window,location,target1,target2,competitor1,competitor2,item) %>%
  mutate(targetclick=ifelse(location==target1,1,ifelse(location==target2,1,0))) %>%
  mutate(competitorclick=ifelse(location==competitor1,1,ifelse(location==competitor2,1,0))) %>%
  mutate(distractorclick=ifelse(targetclick=="1",0,ifelse(competitorclick=="1",0,1))) %>%
  group_by(determiner,size,window,item, target_figure3) %>%
  summarize(Mean_target_selection=mean(targetclick),Mean_competitor_selection=mean(competitorclick),Mean_distractor_selection=mean(distractorclick)) %>%
  rename(gender = target_figure3)


gaze =  g %>%
  filter(TrackLoss=="FALSE") %>%
  select(Prime,condition,determiner,size,targetlook,target.figure, competitorlook,residuelook,whichword,item) %>%
  mutate(distractorlook=ifelse(targetlook=="1",0,ifelse(competitorlook=="1",0,ifelse(residuelook=="1",0,1)))) %>%
  mutate(targetdistractorlook = ifelse(targetlook=="1",1,ifelse(distractorlook=="1",1,0))) %>%
  mutate(competitordistractorlook = ifelse(competitorlook=="1",1,ifelse(distractorlook=="1",1,0))) %>%
  mutate(window=as.character(whichword)) %>%
  mutate(window = ifelse(whichword =="determiner","determiner+name", ifelse(whichword=="name","determiner+name",ifelse(whichword=="end","noun",window)))) %>%
  group_by(determiner,size,window,item, target.figure) %>%
  summarize(Mean_target_look=mean(targetlook),Mean_competitor_look=mean(competitorlook),Mean_distractor_look=mean(distractorlook),Mean_targetdistractor_look=mean(targetdistractorlook),Mean_competitordistractor_look=mean(competitordistractorlook)) %>%
  rename(gender = target.figure)

# This Production study analysis script added in gender distinction in the above two
# blocks of code

df = merge(selection, gaze, by=c("determiner","size","window","item", "gender"))
df$window_re<- factor(df$window, levels = c("baseline","gender","determiner+name","noun"))

nrow(selection) #376
nrow(gaze) #376
nrow(df) #376
nrow(g) #206174
nrow(s) # 6480

# CORRELATIONAL ANALYSES

# compute and visualize overall correlation
longer_selections = df %>% 
  select(-Mean_target_look,-Mean_competitor_look,-Mean_distractor_look,-Mean_targetdistractor_look,-Mean_competitordistractor_look,-window_re) %>% 
  pivot_longer(cols=c("Mean_target_selection","Mean_competitor_selection","Mean_distractor_selection"),names_to=c("delete_this","Region","remove_this"),names_sep=c("_"),values_to="prop_selections") %>% 
  select(-delete_this,-remove_this)

longer_looks = df %>% 
  select(-Mean_target_selection,-Mean_competitor_selection,-Mean_distractor_selection,-Mean_targetdistractor_look,-Mean_competitordistractor_look,-window_re) %>% 
  pivot_longer(cols=c("Mean_target_look","Mean_competitor_look","Mean_distractor_look"),names_to=c("delete_this","Region","remove_this"),names_sep=c("_"),values_to="prop_looks") %>% 
  select(-delete_this,-remove_this)

toplot = longer_looks %>% 
  left_join(longer_selections,by=c("determiner","size","window","Region","item", "gender")) %>% 
  mutate(determiner=fct_recode(determiner,"number"="two","number"="three")) %>% 
  mutate(Region=fct_relevel(Region,"target","competitor"),window=fct_relevel(window,"baseline","gender")) %>% 
  droplevels()

# overall correlation between eye movement and decision task data
cor.test(toplot$prop_looks,toplot$prop_selections) # .86

# correlation between eye movement and decision task data separately by time window
cors_window = toplot %>% 
  group_by(window) %>% 
  summarize(Correlation=round(cor.test(prop_selections,prop_looks)$estimate,2),P=round(cor.test(prop_selections,prop_looks)$p.value,5))
cors_window
# window          Correlation     P
# <fct>                 <dbl> <dbl>
# 1 baseline               0.55     0
# 2 gender                 0.8      0
# 3 determiner+name        0.91     0
# 4 noun                   0.95     0

# correlation between eye movement and decision task data separately by condition within determiner window
cors_determiner = toplot %>% 
  filter(window == "determiner+name") %>% 
  group_by(determiner, size, gender) %>% 
  summarize(Correlation=round(cor.test(prop_selections,prop_looks)$estimate,2),P=round(cor.test(prop_selections,prop_looks)$p.value,5))
cors_determiner
# determiner size  gender Correlation     P
# <fct>      <fct> <fct>        <dbl> <dbl>
# 1 all        big   boy           0.94     0
# 2 all        big   girl          0.92     0
# 3 all        small boy           0.84     0
# 4 all        small girl          0.79     0
# 5 some       big   boy           0.91     0
# 6 some       big   girl          0.87     0
# 7 some       small boy           0.84     0
# 8 some       small girl          0.86     0
# 9 number     big   boy           0.95     0
# 10 number     big   girl          0.95     0
# 11 number     small boy           0.95     0
# 12 number     small girl          0.96     0

cors_determiner_by_noun = toplot %>%
  filter(window == "determiner+name") %>%
  group_by(determiner, size) %>%
  summarize(Correlation=round(cor.test(prop_selections,prop_looks)$estimate,2),P=round(cor.test(prop_selections,prop_looks)$p.value,5))
cors_determiner_by_noun
# determiner size  Correlation     P
# <fct>      <fct>       <dbl> <dbl>
#   1 all        big          0.93     0
# 2 all        small        0.81     0
# 3 some       big          0.88     0
# 4 some       small        0.84     0
# 5 number     big          0.95     0
# 6 number     small        0.95     0

# correlation between eye movement and decision task data separately by condition within noun window
cors_noun = toplot %>% 
  filter(window == "noun") %>% 
  group_by(determiner, size, gender, item) %>% 
  summarize(Correlation=round(cor.test(prop_selections,prop_looks)$estimate,2),P=round(cor.test(prop_selections,prop_looks)$p.value,5))
cors_noun
# dataframe too long to copy and paste

# correlation between eye movement and decision task data separately by condition within noun window
cors_gender = toplot %>% 
  filter(window == "gender") %>% 
  group_by(determiner, size, gender) %>% 
  summarize(Correlation=round(cor.test(prop_selections,prop_looks)$estimate,2),P=round(cor.test(prop_selections,prop_looks)$p.value,5))
cors_gender
# determiner size  gender Correlation       P
# <fct>      <fct> <fct>        <dbl>   <dbl>
# 1 all        big   boy           0.89 0      
# 2 all        big   girl          0.85 0      
# 3 all        small boy           0.8  0      
# 4 all        small girl          0.86 0      
# 5 some       big   boy           0.71 0.00009
# 6 some       big   girl          0.89 0      
# 7 some       small boy           0.82 0      
# 8 some       small girl          0.79 0      
# 9 number     big   boy           0.82 0.00003
# 10 number     big   girl          0.73 0.00019
# 11 number     small boy           0.71 0.00012
# 12 number     small girl          0.74 0.00011

# End copying of analysis script from Degen, Kursat, Leigh 2021

##############################################
## 
## Plot Correlations
## 
#############################################

# rename columns from our calculation of surprisal and correlations
# so that they match
cors_determiner <- cors_determiner %>%
  rename(condition = determiner, correlation = Correlation)

surprisalDet <- surprisalDet %>%
  rename(gender = figure) %>%
  mutate(condition=fct_recode(condition, "number"="num"))

uniqueSurprisal = surprisalDet %>% 
  select(experiment,condition,size,gender,surprisal) %>% 
  unique() %>% 
  rename(determiner=condition)

# Merge surprisal and correlation data
dfCorr <- uniqueSurprisal %>%
  left_join(cors_determiner, by = c("determiner", "size", "gender"))

dfCorr = dfCorr %>%
  mutate(determiner = fct_relevel(determiner, "all","some","number"), size=fct_relevel(size,"small","big"))

# New facet label names for supp variable
exp.labs <- c("Exp. 1", "Exp. 2")
names(exp.labs) <- c("1", "2")

graphSurprisalCorr <- dfCorr %>% 
  ggplot(aes(x=surprisal, y=Correlation, color=determiner, shape = size,group=1)) + 
  facet_grid(. ~ experiment, 
             scale = "free",
             labeller = labeller(experiment = exp.labs)) +
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

ggsave(filename = "../graphs/surprisal_correlation.pdf", plot = graphSurprisalCorr,
       width = 5, height = 4, device = "pdf")


##############################################
## 
## Correlation Statistics
## 
#############################################

dfCorr_3.1 <- dfCorr %>%
  filter(experiment == 1)

dfCorr_3.2 <- dfCorr %>%
  filter(experiment == 2)

# Run linear model to test for how predictive surprisal is of correlation for simple exposure
m_3.1 = lm(Correlation ~ surprisal, data=dfCorr_3.1) 
summary(m_3.1) 
# surprisal estimate:  -0.005497
# Suprisal estimate Pr(>|t|): 0.00015 ***
# Adjusted R-squared:  0.2703

# with the unique:
# surprisal   -0.005514  p = 0.073 .
# Adjusted R-squared:  0.2151

# Run linear model to test for how predictive surprisal is of correlation for extra exposure
m_3.2 = lm(Correlation ~ surprisal, data=dfCorr_3.2) 
summary(m_3.2) 
# surprisal estimate:  -0.038322
# Suprisal estimate Pr(>|t|): 8.75e-15 ***
# Adjusted R-squared:  0.5479 

# With the unique:
# surprisal   -0.03852   p =  0.00635 ** 
# Adjusted R-squared:  0.4959



# Calculate correlation between surprisal and incremental study correlations
dfCorrFinal_3.1 <- dfCorr_3.1 %>%
  summarize(Correlation=round(cor.test(surprisal,correlation)$estimate,2),
            P=round(cor.test(surprisal,correlation)$p.value,5))

dfCorrFinal_3.1
# Correlation       P
# 1       -0.54 0.00015

# Calculate correlation between surprisal and incremental study correlations
dfCorrFinal_3.2 <- dfCorr_3.2 %>%
  summarize(Correlation=round(cor.test(surprisal,correlation)$estimate,2),
            P=round(cor.test(surprisal,correlation)$p.value,5))

dfCorrFinal_3.2
# Correlation P
# -0.74        0
