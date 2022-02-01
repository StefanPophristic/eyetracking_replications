#####################
#####################
# Stefan Pophristic
# December 16th, 2021
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
# Input: 3_production_extra_practice_pilot-trials_preprocessed_checked.csv
#
# This csv file has to be run through the analyzeResposes.R script and then 
# manually processed for any mistakes (as outlined in the readme.md) prior 
# to importing it into this analysis. 
# This csv file can be found in the data folder
#
# Output: trialExclusion_comparison.jpeg
#             contains comparison of number of trials excluded across two pilot studies
#         gender_comparison.jpeg
#             comparison of use of gender in responses across two pilot studies and 
#             experimental / filler conditions
#         noun_comparison.jpeg
#             comparison of use of nouns in responses across two pilot studies and 
#             experimental / filler conditions
#         determiner_comparison.jpeg
#             comparison of use of determiners in responses across two pilot studies, 
#             experimental / filler conditions, and big/small conditions
#
# all these output files can be found in the graphs folder
#####################
#####################

library(tidyverse)
library(lme4)
library(boot)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("helpers.R")
theme_set(theme_bw())

# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") 


df = read.csv("../data/3_production_simple-trials_cleaned.csv", header = TRUE)

#Information about the experiment

nrow(df) # 2550 data points
length(unique(df$workerid)) # 51 participants run

totalNumTrials <- df %>% filter(workerid == 63)
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
# 2 practice trials

#############################################
#############################################
#
# Data Pre-processing
#
#############################################
#############################################

#Get rid of practice trials
dfExp <- df %>%
  filter(df$ExpFiller == "Exp" | df$ExpFiller == "Filler")

nrow(df) # 2550 data points
length(unique(df$workerid)) # 51 participants

# Get the columns that you will use
dfExp <- dfExp %>%
  select(workerid, ExpFiller, condition, determiner, size, response, setting, target_figure3, target_object3, trial_number, response_times,
         genderUsed, correctGender, correctNoun, detUsed, correctDet)

#############################################
#############################################
#
# Get rid of participants with high error rates
#
#############################################
#############################################

# Get by participant accuracy statistics

participantStatsNoun <- dfExp %>%
  group_by(workerid, correctNoun) %>%
  count(vars = "correctNoun") %>%
  select(-vars) %>%
  rename(correctNounCount = n)
  
participantStatsGender <- dfExp %>%
  group_by(workerid, correctGender) %>%
  count(vars = "correctGender") %>%
  select(-vars) %>%
  rename(correctGenderCount = n)

participantStatsDetUsed <- dfExp %>%
  group_by(workerid, detUsed) %>%
  count(vars = "detUsed") %>%
  select(-vars) %>%
  rename(detCount = n)

participantStatsCorrectDet <- dfExp %>%
  group_by(workerid, correctDet) %>%
  count(vars = "correctDet") %>%
  select(-vars) %>%
  rename(correctDetCount = n)

numTrialsPerParticipant <- nrow(dfExp)

#list that will hold all workerids of people with high error rates
ids <- list()

# Exclude participants based on the 5% exclusion cutoff
highErrorRatePreExclusionNum <- nrow(dfExp)

ids <- dfExp %>%
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

highErrorRatePostExclusionNum <- nrow(dfExp)
highErrorRateTotalExclusionNum <- highErrorRatePreExclusionNum - highErrorRatePostExclusionNum
print(paste("Total number of participants excluded due to 5% error rate: ", 
            highErrorRateTotalExclusionNum))
# "Total number of participants excluded due to 5% error rate:  0"

# Get rid of all participants with >5% error rates
dfExp <- dfExp %>%
  filter(!(workerid %in% ids))

# Get rid of participants that do not use nouns > 50% of the time
numTrialsPreNoNounExclusion <- nrow(dfExp)

dfExp <- dfExp %>%
  mutate(noNoun = case_when(
    correctNoun == "noNoun" ~ 1,
    TRUE ~ 0)) %>%
  group_by(workerid) %>%
  mutate(noNounResponseRate = sum(noNoun)/48) %>%
  filter(noNounResponseRate < 0.5) %>%
  select(-c(noNoun, noNounResponseRate))

# Print number of participants excluded due to criterion
numTrialsPostNoNounExclusion <- nrow(dfExp)
noNounParticipantExclusionNum <- (numTrialsPreNoNounExclusion - numTrialsPostNoNounExclusion)/48
print(paste("Number of participants excluded due to > 50% rate of use of no noun responses: ", 
            noNounParticipantExclusionNum))
# Number of participants excluded due to > 50% rate of use of no noun responses:  0


# Get rid of participants that answer with single word utterances > 50% of the time
numTrialsPreSingleExclusion <- nrow(dfExp)

dfExp <- dfExp %>%
  mutate(single = case_when(
    grepl(" ", response) ~ 0,
    TRUE ~ 1)) %>%
  group_by(workerid) %>%
  mutate(singleResponseRate = sum(single)/48) %>%
  filter(singleResponseRate < 0.5) %>%
  select(-c(single, singleResponseRate))

numTrialsAfterSingleExclusion <- nrow(dfExp)

# print number of participants excluded due to >50% rate of single word responses

postSingleWordParticipantExclusionNum <- (numTrialsPreSingleExclusion - numTrialsAfterSingleExclusion)/48

print(paste("Number of participants excluded due to > 50% rate of use of single word utterances: ", 
            postSingleWordParticipantExclusionNum))
# [1] "Number of participants excluded due to > 50% rate of use of single word utterances:  0"


# Print total number of participants excluded

print(paste("Number of participants excluded: ",(length(ids) + noNounParticipantExclusionNum + postSingleWordParticipantExclusionNum)))
# "Number of participants excluded:  0"


#############################################
#############################################
#
# Investigation of long and short response times
#
#############################################
#############################################

# log-transform response times
dfExp$logRT = log(dfExp$response_times)

# get mean response time and potential SD cutoff, in this case 2.5 SDs
mean_rt = mean(dfExp$logRT)
sd_cutoff = 2.5*sd(dfExp$logRT)

# show what would get cut off
ggplot(dfExp, aes(x=logRT)) + 
  geom_histogram(binwidth=.1) +
  geom_vline(xintercept=mean_rt-sd_cutoff) +
  geom_vline(xintercept=mean_rt+sd_cutoff)

# see trials that would get cut off
# View(dfExp[dfExp$response_times < exp(mean_rt-sd_cutoff),]) # there is nobody on the low end, but if you search for response times < 2500 (ie 2.5 seconds), you'll see this is mostly one person who never typed the gender. we may want to exclude participants who never or rarely produced gender, since they are arguably not doing the same task as the others (these appear to be workers 95 and 128)
# View(dfExp[dfExp$response_times > exp(mean_rt+sd_cutoff),]) # the very slow people produce reasonable referring expressions. my hunch is that because we're having people type and there are big differences in how fast people type, some are just turning up as very slow, but they shouldn't be excluded.

#############################################
#############################################
#
# Get rid of trials with errors
#
#############################################
#############################################

# Exclude trials of JUST experimental trails

dfExp <- dfExp %>%
  filter(ExpFiller == "Exp")

print(nrow(dfExp)) #1836 trials pre trial exclusions

# Get rid of trials with wrong nouns
preNounExclusionNum <- nrow(dfExp)
dfExp <- dfExp %>%
  filter(correctNoun != 0)

postNounExclusionNum <- nrow(dfExp)
trialExclusionNoun <- preNounExclusionNum - postNounExclusionNum
print(paste("Number of trials excluded due to wrong Noun: ",trialExclusionNoun))
# "Number of trials excluded due to wrong Noun:  17"

# get rid of trials with wrong gender
preGenderExclusionNum <- nrow(dfExp)
dfExp <- dfExp %>%
  filter(correctGender != 0)

postGenderExclusionNum <- nrow(dfExp)
trialExclusionGender <- preGenderExclusionNum - postGenderExclusionNum
print(paste("Number of trials excluded due to wrong Gender: ",trialExclusionGender))
# "Number of trials excluded due to wrong Gender:  3"

# Get rid of trials with wrong determiner
preDetExclusionNum <- nrow(dfExp)
dfExp <- dfExp %>%
  filter(correctDet != 0)

postDetExclusionNum <- nrow(dfExp)
trialExclusionDet <- preDetExclusionNum - postDetExclusionNum
print(paste("Number of trials excluded due to wrong Determiner: ",trialExclusionDet))
# "Number of trials excluded due to wrong Determiner:  6"


# Get rid of trails where the participant did not use a noun
preNoNounExclusionNum <- nrow(dfExp)
test <- dfExp %>%
  filter(correctNoun != "noNoun")

postNoNounExclusionNum <- nrow(test)
trialExclusionNoNoun <- preNoNounExclusionNum - postNoNounExclusionNum
print(paste("Number of trials excluded due to no noun in response: ",trialExclusionNoNoun))
# "Number of trials excluded due to no noun in response:  0"

# Get rid of trials where the participant used only a single word
preSingleWordExclusionNum <- nrow(dfExp)
dfExp <- dfExp %>%
  filter(grepl(" ", response))

postSingleWordExclusionNum <- nrow(dfExp)
trialExclusionSingleWord <- preSingleWordExclusionNum - postSingleWordExclusionNum
print(paste("Number of trials excluded due to single word responses: ",trialExclusionSingleWord))
"Number of trials excluded due to single word responses:  0"

# Print total number of trials excluded
trialsExcluded <- trialExclusionNoun + trialExclusionGender + trialExclusionDet + trialExclusionNoNoun + trialExclusionSingleWord
print(paste("Total number of trials excluded due to incorrect answers: ", trialsExcluded))
# "Total number of trials excluded due to incorrect answers:  26"

print(nrow(dfExp)) #1810 trials post trial exclusion


#############################################
#############################################



nrow(dfExp) # 1810 data points to analyze after exclusions

#############################################
#############################################
#
# Calculate Gender Surprisal
#
#############################################
#############################################

# Create data frame with counts of all gender utterances
# based off of size and target figure conditions
gender <- dfExp %>%
  group_by(target_figure3, genderUsed, size, condition) %>%
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
  group_by(target_figure3, genderUsed, size, condition) %>%
  summarize(n = sum(n))

# Get number of instances of the target utterance (e.g. "boy" for boy-condition)
# and append that as a new column
genderTemp <- gender %>%
  filter(target_figure3 == genderUsed) %>%
  mutate(targetUtteranceN = n)

gender <- left_join(gender, genderTemp, by = c("target_figure3", "size", "condition")) %>%
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
  group_by(target_figure3, size, condition) %>%
  summarize(surprisal = -log2(targetUtteranceN / sum(n))) %>%
  distinct()

#############################################
#############################################
#
# Calculate Determiner Surprisal
#
#############################################
#############################################


# Create data frame with counts of all determiner utterances
# based off of size and target figure conditions
det <- dfExp %>%
  group_by(condition, size, target_figure3, detUsed) %>%
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
  group_by(condition, size, target_figure3, detUsed) %>%
  summarize(n = sum(n))

# Get number of instances of the target utterance (e.g. "all" for all-condition)
# and append that as a new column
detTemp <- det %>%
  filter(condition == detUsed) %>%
  mutate(targetUtteranceN = n)

det <- left_join(det, detTemp, by = c("condition", "size", "target_figure3")) %>%
  select(-c(detUsed.y, n.y)) %>%
  rename(determiner = detUsed.x, n = n.x)

# If no target utterances were observed, they will come up as NA in the targetUtteranceN
# column --> replace those values with 0
det <- mutate_at(det, "targetUtteranceN", ~replace(., is.na(.), 0))

surprisalDet <-det %>%
  group_by(condition, size, target_figure3) %>%
  mutate(probability = (targetUtteranceN/sum(n))) %>%
  mutate(probability = case_when(probability == 0 ~ 0.0001,
                                 TRUE ~ probability)) %>%
  mutate(surprisal = -log2(probability))

# # In order to have correlation analysis, we have to have surprisals of not INF
# #   which means we can't have observations of target utterances of 0
# #   so we replace all observations of target utterances of 0 with 0.0001
# det <- det %>%                            
#   mutate(targetUtteranceN = replace(targetUtteranceN, targetUtteranceN == 0, 0.0001))
# 
# # Calculate Surprisal
# # Surprisal = -log(P(Utterance))
# # P(Utterance) = [number of instances of target utterance given a condition] / [total number of utterances given a condition]
# surprisalDet<- det %>%
#   group_by(condition, size, target_figure3) %>%
#   summarize(probability = targetUtteranceN / sum(n), 
#             surprisal = -log2(targetUtteranceN / sum(n))) %>%
#   distinct()

#############################################
#############################################
#
# Calculate Noun Surprisal
#
#############################################
#############################################

# Nomenclature note
# target_object3 == the target object
# correctNoun == {1, "other"}

# Create data frame with counts of all gender utterances
# based off of size and target figure conditions
noun <- dfExp %>%
  group_by(condition, size, target_figure3, target_object3, correctNoun) %>%
  count()

# Change target_object3 labels for convenience
noun <- noun %>%
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
    target_object3 == "2_scissors" | target_object3 == "3_scissors" ~ "scissors"))

# Get number of instances of the target utterance (e.g. "apples" for apples-condition)
# and append that as a new column
# The noun data frame contains counts of both the target utterance and other utterances
#   per condition
nounTemp <- noun %>%
  filter(correctNoun == 1) %>%
  mutate(targetUtteranceN = n)

noun <- left_join(noun, nounTemp, by = c("target_object3", "target_figure3", "size", "condition")) %>%
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
  group_by(target_object3, size, condition, target_figure3) %>%
  summarize(surprisal = -log2(targetUtteranceN / sum(n))) %>%
  distinct()


#############################################
#############################################
#
# Plot Surprisal Plots
#
#############################################
#############################################


# Plot Gender Surprisal

graphSurprisalGenderBySize <- surprisalGender %>%
  ggplot(aes(fill = condition, y=surprisal, x=target_figure3, color = condition)) + 
  facet_grid(. ~ size) +
  geom_point(size = 4) +
  scale_color_manual(values=c("olivedrab", "lightsalmon3", "skyblue3")) +
  theme(text = element_text(size = 16), plot.title = element_text(hjust = 0.5, size = 20)) +
  ylab("Surprisal") +
  xlab("Expected Gender Term") +
  ggtitle("Surprisal for two expected gender terms") 

jpeg(file="../graphs/gender_surprisal_rate_by_size.jpeg", width = 500, height = 500)
plot(graphSurprisalGenderBySize)
dev.off()

graphSurprisalGenderByGender <- surprisalGender %>%
  ggplot(aes(fill = size, y=surprisal, x=condition, color = size)) + 
  facet_grid(. ~ target_figure3) +
  geom_point(size = 4) +
  scale_color_manual(values=c("olivedrab", "lightsalmon3", "skyblue3")) +
  theme(text = element_text(size = 16), plot.title = element_text(hjust = 0.5, size = 20)) +
  ylab("Surprisal") +
  xlab("Condition") +
  ggtitle("Surprisal for two expected gender terms") 

jpeg(file="../graphs/gender_surprisal_rate_by_gender.jpeg", width = 500, height = 500)
plot(graphSurprisalGenderByGender)
dev.off()


# Plot Determiner Surprisal
surprisalDet$condition <- surprisalDet$condition %>%
  factor(levels = c('all', 'some', 'num'))
surprisalDet$size <- surprisalDet$size %>%
  factor(levels = c('big', 'small'))
surprisalDet$target_figure3 <- surprisalDet$target_figure3 %>%
  factor(levels = c('boy', 'girl'))

graphSurprisalDet <- surprisalDet %>%
  select(condition, size, target_figure3, surprisal) %>%
  ggplot(aes(fill=size, y=surprisal, x=condition, color=size)) + 
  facet_grid(. ~ target_figure3) +
  geom_point(size = 4) +
  scale_color_manual(values=c("olivedrab", "lightsalmon3")) + #"darkorange2", "steelblue4"
  theme(text = element_text(size = 16), plot.title = element_text(hjust = 0.5, size = 20)) +
  ylab("Surprisal") +
  xlab("Expected Determiner Term") +
  ggtitle("Surprisal for expected determiners") 

jpeg(file="../graphs/det_surprisal_rate.jpeg", width = 500, height = 500)
plot(graphSurprisalDet)
dev.off()


# Plot noun surprisal
graphSurprisalNoun <- surprisalNoun %>%
  ggplot(aes(fill=condition, y=surprisal, x=target_object3, color=condition, shape = condition)) + 
  facet_grid(size ~ target_figure3) + 
  geom_point(size = 4) +
  theme(text = element_text(size = 16), plot.title = element_text(hjust = 0.5, size = 20)) +
  scale_color_manual(values=c("olivedrab", "lightsalmon3", "skyblue3")) + #"darkorange2", "steelblue4"
  scale_shape_manual(values=c(19, 3, 4)) +
  ylim(-1, 1) + 
  ylab("Surprisal") +
  xlab("Expected Noun") +
  ggtitle("Surprisal for expected nouns") 

jpeg(file="../graphs/noun_surprisal_rate.jpeg", width = 500, height = 500)
plot(graphSurprisalNoun)
dev.off()

#############################################
#############################################
#
# CogSci Paper Graphs
#
#############################################
#############################################

# Calculate surprisal of conditions by noun rather than gender

# Change target object labels for convenience
dfDet <- dfExp %>%
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
  rename(noun = target_object3)

# Change all other determiners to "other" category
# Change "two" and "three" to "num"
dfDet <- dfDet %>%
  mutate(detUsed = as.character(detUsed)) %>%
  mutate(detUsed = case_when(
    detUsed != "noDet" & detUsed != "some" & detUsed != "all" &
      detUsed != "two" & detUsed != "three" ~ "other",
    detUsed == "two" | detUsed == "three" ~ "num",
    TRUE ~ detUsed))

# Keep only columns you need (size, noun, and determiner used)
# And count the number of instances of determiner per (size x noun)
dfDet <- dfDet %>%
  group_by(size, noun, detUsed) %>%
  count() %>%
  ungroup()

# Add in all the size x noun x determiner used combinations which were not observed
allCombinations <- unique(expand.grid(dfDet$size, dfDet$noun, c("all", "some", "num", "noDet", "other")))
allCombinations <- allCombinations %>%
  rename(size = Var1, noun = Var2, detUsed = Var3)

dfDet <- right_join(dfDet, allCombinations, by = c("size", "noun", "detUsed"))


# Change NA occurances with 0
dfDet <- dfDet %>%
  mutate(n = replace(n, is.na(n), 0))

# Count occurances for CogSci Paper
occuranceCount <- dfDet %>%
  group_by(detUsed) %>%
  mutate(newN = sum(n)) %>%
  select(detUsed, newN) %>%
  unique()
occuranceCount
# Groups:   detUsed [5]
# detUsed        newN
# <chr>         <dbl>
#   1 noDet    566       
# 2 num     1182       
# 3 some      48.0     
# 4 other     14.0     
# 5 all        0.000024


# Get total number of utterances per condition (size x noun)
totalSums <- dfDet %>%
  group_by(size, noun) %>%
  summarize(sum = sum(n))

# Merge the two together
dfDet <- merge(dfDet, totalSums, by = c("size", "noun"))

#Calculate suprisal
# Surprisal = -log([# of instances of determiner use given size and noun condition] / 
# [total number of trials of the given size and noun condition])
dfDet <- dfDet %>%
  mutate(probability = (n/sum)) %>%
  mutate(probability = case_when(probability == 0 ~ 0.0001,
                                 TRUE ~ probability)) %>%
  mutate(surprisal = -log2(probability))
  
tmp <- dfDet %>%
  mutate(detUsed = fct_relevel(detUsed,"some"))

# Run linear model tobackup the claims
m = lmer(surprisal ~ detUsed*size + (1|noun), data=tmp)

summary(m)

# # Graph it

surprisalMeanAndCI <- dfDet %>%
  group_by(size,detUsed) %>% 
  summarize(
    mean_surprisal = mean(surprisal),
    CI.Low = ci.low(surprisal),
    CI.High = ci.high(surprisal))

write.csv(surprisalMeanAndCI, "simple_surprisalMeanAndCI.csv", row.names = FALSE) 

graphSurprisalDetByNoun <- surprisalMeanAndCI %>% 
  ungroup() %>% 
  mutate(YMin = mean_surprisal - CI.Low, 
         YMax = mean_surprisal + CI.High) %>% 
  ggplot(aes(y=mean_surprisal, x=detUsed, color = size)) + 
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = YMin, ymax=YMax),width=0.4) + 
  theme(text = element_text(size = 12), 
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.text.x = element_text(size = 8, angle = 45, hjust = 0.9),
        legend.text = element_text(size = 8)) +
  scale_color_manual(values=c("olivedrab", "lightsalmon3", "skyblue3")) + #"darkorange2", "steelblue4"
  guides(color=guide_legend("Size")) + 
  ylab("Surprisal") +
  ylim(0, 14) + 
  scale_x_discrete(labels = c("\"all\"", "\"some\"", "number", "no \n determiner", "other")) + 
  xlab("Determiner")
graphSurprisalDetByNoun
  
ggsave(filename = "../graphs/simple_det_surprisal_by_noun.pdf", plot = graphSurprisalDetByNoun,
       width = 3.5, height = 3.5, units = "in", device = "pdf")



#############################################
#############################################
#
# CogSci Paper Graphs With Facets
#
#############################################
#############################################

# Calculate surprisal of conditions by noun rather than gender
# 
# # Change target object labels for convenience
# dfDet <- dfExp %>%
#   mutate(target_object3 = as.character(target_object3)) %>%
#   mutate(target_object3 = case_when(
#     target_object3 == "2_apples" | target_object3 == "3_apples" ~ "apples",
#     target_object3 == "2_bananas" | target_object3 == "3_bananas" ~ "bananas",
#     target_object3 == "2_pears" | target_object3 == "3_pears" ~ "pears",
#     target_object3 == "2_oranges" | target_object3 == "3_oranges" ~ "oranges",
#     
#     target_object3 == "2_forks" | target_object3 == "3_forks" ~ "forks",
#     target_object3 == "2_knives" | target_object3 == "3_knives" ~ "knives",
#     target_object3 == "2_spoons" | target_object3 == "3_spoons" ~ "spoons",
#     target_object3 == "2_plates" | target_object3 == "3_plates" ~ "plates",
#     
#     target_object3 == "2_pencils" | target_object3 == "3_pencils" ~ "pencils",
#     target_object3 == "2_erasers" | target_object3 == "3_erasers" ~ "erasers",
#     target_object3 == "2_rulers" | target_object3 == "3_rulers" ~ "rulers",
#     target_object3 == "2_scissors" | target_object3 == "3_scissors" ~ "scissors")) %>%
#   rename(noun = target_object3)
# 
# # Change all other determiners to "other" category
# # Change "two" and "three" to "num"
# dfDet <- dfDet %>%
#   mutate(detUsed = as.character(detUsed)) %>%
#   mutate(detUsed = case_when(
#     detUsed != "noDet" & detUsed != "some" & detUsed != "all" &
#       detUsed != "two" & detUsed != "three" ~ "other",
#     detUsed == "two" | detUsed == "three" ~ "num",
#     TRUE ~ detUsed))
# 
# # Keep only columns you need (size, noun, and determiner used)
# # And count the number of instances of determiner per (size x noun)
# dfDet <- dfDet %>%
#   group_by(condition, size, noun, detUsed) %>%
#   count() %>%
#   ungroup()
# 
# # Add in all the size x noun x determiner used combinations which were not observed
# allCombinations <- expand.grid(unique(dfDet$condition), unique(dfDet$size), unique(dfDet$noun), c("all", "some", "num", "noDet", "other"))
# allCombinations <- allCombinations %>%
#   rename(condition = Var1, size = Var2, noun = Var3, detUsed = Var4)
# 
# dfDet <- right_join(dfDet, allCombinations, by = c("condition", "size", "noun", "detUsed"))
# 
# # Change 0 observations to 0.0001 so that we can calculate surprisal
# dfDet <- dfDet %>%
#   mutate(n = replace(n, is.na(n), 0.0001))
# 
# # Get total number of utterances per condition (condition x size x noun)
# totalSums <- dfDet %>%
#   group_by(condition, size, noun) %>%
#   summarize(sum = sum(n))
# 
# # Merge the two together
# dfDet <- merge(dfDet, totalSums, by = c("condition", "size", "noun"))
# 
# #Calculate suprisal
# # Surprisal = -log([# of instances of determiner use given size and noun condition] / 
# # [total number of trials of the given size and noun condition])
# dfDet <- dfDet %>%
#   mutate(surprisal = -log2(n / sum))
# 
# # Graph it
# dfDet$detUsed <- dfDet$detUsed %>%
#   factor(levels = c('all', 'some', 'num', 'noDet', 'other'))
# 
# graphSurprisalDetByNounFaceted <- dfDet %>%
#   group_by(condition, size,detUsed) %>% 
#   summarize(
#     mean_surprisal = mean(surprisal),
#     CI.Low = ci.low(surprisal),
#     CI.High = ci.high(surprisal)
#   ) %>% 
#   ungroup() %>% 
#   mutate(YMin = mean_surprisal - CI.Low, 
#          YMax = mean_surprisal + CI.High) %>% 
#   ggplot(aes(y=mean_surprisal, x=detUsed, color = size)) + 
#   facet_grid(. ~ condition) + 
#   geom_point(size = 4) +
#   geom_errorbar(aes(ymin = YMin, ymax=YMax),width=0.15) + 
#   theme(text = element_text(size = 16), plot.title = element_text(hjust = 0.5, size = 20)) +
#   scale_color_manual(values=c("olivedrab", "lightsalmon3", "skyblue3")) + #"darkorange2", "steelblue4"
#   guides(color=guide_legend("Size")) + 
#   ylab("Surprisal") +
#   ylim(0, 27) + 
#   scale_x_discrete(labels = c("\"all\"", "\"some\"", "number", "no determiner", "other")) + 
#   xlab("Determiner")
# graphSurprisalDetByNounFaceted
# 
# ggsave(filename = "../graphs/simple_det_surprisal_by_noun_faceted.pdf", plot = graphSurprisalDetByNounFaceted,
#        width = 17, height = 7, units = "in", device = "pdf")
# 
# 
# #############################################
#############################################
#
# IMPORT ANALYSIS FROM INCREMENTAL STUDY
#
#############################################
#############################################

# This analysis was adapted from Degen, Kursat, and Leigh 2021
# You can find the original analysis script under the following link:
# https://github.com/thegricean/eyetracking_replications.git
# the analysis is taken from: analysis > SunBreheny > 1_incremental >
# main > rscripts > graphs.R

df = read.csv("1_incremental_data/trials_merged.csv", header = TRUE)
demo = read.csv("1_incremental_data/subject_info_merged.csv", header = TRUE)

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
baseline = read.csv("1_incremental_data/sb_eyetracking/exp200ms_beselinedata.csv", header = TRUE)
gender = read.csv("1_incremental_data/sb_eyetracking/exp200ms_genderdata.csv", header = TRUE)
determiner = read.csv("1_incremental_data/sb_eyetracking/exp200ms_determiner.csv", header = TRUE)
name = read.csv("1_incremental_data/sb_eyetracking/exp200ms_namedata.csv", header = TRUE)
end = read.csv("1_incremental_data/sb_eyetracking/exp200ms_enddata.csv", header = TRUE)
preview = read.csv("1_incremental_data/sb_eyetracking/exp200ms_previewdata.csv", header = TRUE)

# order should be: baseline / gender / determiner + name / noun 
# we ignore the "preview" window since there's no corresponding 
# window in the incremental decision experiment
g = rbind(baseline,gender,determiner,name,end)  %>%
  mutate(item=word(as.character(instruction), -1))

# re-load incremental decision data
s = read.csv("1_incremental_data/trials_merged.csv", header = TRUE)  %>%
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

# correlation between eye movement and decision task data separately by condition within determiner window
cors_determiner = toplot %>% 
  filter(window == "determiner+name") %>% 
  group_by(determiner, size, gender) %>% 
  summarize(Correlation=round(cor.test(prop_selections,prop_looks)$estimate,2),P=round(cor.test(prop_selections,prop_looks)$p.value,5))
cors_determiner

# correlation between eye movement and decision task data separately by condition within noun window
cors_noun = toplot %>% 
  filter(window == "noun") %>% 
  group_by(determiner, size, gender, item) %>% 
  summarize(Correlation=round(cor.test(prop_selections,prop_looks)$estimate,2),P=round(cor.test(prop_selections,prop_looks)$p.value,5))
cors_noun


# correlation between eye movement and decision task data separately by condition within noun window
cors_gender = toplot %>% 
  filter(window == "gender") %>% 
  group_by(determiner, size, gender) %>% 
  summarize(Correlation=round(cor.test(prop_selections,prop_looks)$estimate,2),P=round(cor.test(prop_selections,prop_looks)$p.value,5))
cors_gender

# End copying of analysis script from Degen, Kursat, Leigh 2021

#############################################
#############################################
#
# ANALYZE INCREMENTAL STUDY CORRELATIONS 
# AND SURPRISALS
#
#############################################
#############################################


#####
# Determiner Correlations


# rename columns from our calculation of surprisal and correlations
# so that they match
cors_determiner <- cors_determiner %>%
  rename(condition = determiner, correlation = Correlation)

surprisalDet <- surprisalDet %>%
  rename(gender = target_figure3) %>%
  mutate(condition=fct_recode(condition, "number"="num"))

# Merge surprisal and correlation data
finalCorrelationDetDF <- surprisalDet %>%
  select(condition, size, gender, surprisal) %>%
  merge(cors_determiner, by = c("condition", "size", "gender"))

write.csv(finalCorrelationDetDF, "simple_finalCorrelationDetDF.csv", row.names = FALSE)

# Plot the correlation for visual inspection
graphDetCorrelation <- ggplot(finalCorrelationDetDF, aes(x=surprisal, y=correlation, color=condition, shape = size,group=1))+
  geom_smooth(method="lm",se=F,color="gray80",alpha=.5) +
  geom_point(size=2) 

graphDetCorrelation

ggsave(filename = "../graphs/simple_det_correlation.pdf", plot = graphDetCorrelation,
       width = 3.5, height = 2.5, units = "in", device = "pdf")

# Run linear model to test for how predictive surprisal is of correlation
m = lm(correlation ~ surprisal, data=finalCorrelationDetDF) 
summary(m) 
# surprisal estimate:  -0.005497
# Suprisal estimate Pr(>|t|): 0.00015 ***

# Calculate correlation between surprisal and incremental study correlations
finalCorrelationDet <- finalCorrelationDetDF %>%
  summarize(Correlation=round(cor.test(surprisal,correlation)$estimate,2),
            P=round(cor.test(surprisal,correlation)$p.value,5))

finalCorrelationDet
# Correlation       P
# 1       -0.54 0.00015

#####
# Noun Correlations

# rename columns from our calculation of surprisal and correlations
# so that they match
cors_noun <- cors_noun %>%
  rename(condition = determiner, correlation = Correlation, noun = item)

surprisalNoun <- surprisalNoun %>%
  rename(noun = target_object3, gender = target_figure3)

# Merge surprisal and correlation data
finalCorrelationNounDF <- surprisalNoun %>%
  merge(cors_noun, by = c("condition", "size", "gender", "noun"))

# Plot the correlation for visual inspection
graphNounCorrelation <- ggplot(finalCorrelationNounDF, aes(x=surprisal, y=correlation, color=condition, shape = size))+
  geom_point()

graphNounCorrelation

ggsave(filename = "../graphs/simple_noun_correlation.pdf", plot = graphNounCorrelation,
       width = 3.5, height = 2.5, units = "in", device = "pdf")

# Calculate correlation between surprisal and incremental study correlations
finalCorrelationNoun <- finalCorrelationNounDF %>%
  summarize(Correlation=round(cor.test(surprisal,correlation)$estimate,2),
            P=round(cor.test(surprisal,correlation)$p.value,5))
finalCorrelationNoun
#   Correlation       P
# 1       -0.12 0.33269



#####
# Gender Correlations

# rename columns from our calculation of surprisal and correlations
# so that they match
cors_gender <- cors_gender %>%
  rename(condition = determiner, correlation = Correlation)

surprisalGender <- surprisalGender %>%
  rename(gender = target_figure3)

# Merge surprisal and correlation data
finalCorrelationGenderDF <- surprisalGender %>%
  merge(cors_gender, by = c("condition", "size", "gender"))

# Plot the correlation for visual inspection
graphGenderCorrelation <- ggplot(finalCorrelationGenderDF, aes(x=surprisal, y=correlation, color=condition, shape = size,group=1))+
  geom_smooth(method="lm",se=F,color="gray80",alpha=.5) +
  geom_point()

graphGenderCorrelation

ggsave(filename = "../graphs/simple_gender_correlation.pdf", plot = graphGenderCorrelation,
       width = 3.5, height = 2.5, units = "in", device = "pdf")

# Calculate correlation between surprisal and incremental study correlations
finalCorrelationgender <- finalCorrelationGenderDF %>%
  summarize(Correlation=round(cor.test(surprisal,correlation)$estimate,2),
            P=round(cor.test(surprisal,correlation)$p.value,5))
finalCorrelationgender

# Correlation       P
#1        0.52 0.18176
