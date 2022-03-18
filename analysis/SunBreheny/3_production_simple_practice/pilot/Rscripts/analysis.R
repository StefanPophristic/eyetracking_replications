#####################
#####################
# Stefan Pophristic
# December 16th, 2021
# ALPS LAB
# Sun and Breheny replication production study Pilot
#####################
# This script produces graphs in which the use of gender, determiner, and noun
# in each of the two pilot studies are compared. Number of trials dropped are 
# also compared.
#
# Input: 3_production_extra_practice_pilot-trials_preprocessed_checked.csv
#        3_production_simple_practice_pilot-trials_preprocessed_checked.csv
#
# Both of these csv files have been run through the analyzeResposes.R script
# and were then manually processed for any mistakes (as outlined in the readme.md 
# found in the pilot analysis document).
# All these csv files can be found in the data folder
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
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
theme_set(theme_bw())

dfExtra = read.csv("../data/3_production_extra_practice_pilot-trials_preprocessed_checked.csv", header = TRUE)
dfSimple = read.csv("../data/3_production_simple_practice_pilot-trials_preprocessed_checked.csv", header = TRUE)

# Get rid of practice trials for Extra Exposure experiment
dfExtraExp <- dfExtra %>%
  filter(dfExtra$ExpFiller == "Exp" | dfExtra$ExpFiller == "Filler")

# Get the columns that you will use for Extra Exposure experiment
dfExtraExp <- dfExtraExp %>%
  select(workerid, ExpFiller, condition, size, determiner, response, setting, target_figure3, target_object3, trial_number, response_times,
         genderUsed, correctGender, correctNoun, detUsed, correctDet)

# Get rid of practice trials for Simple Exposure experiment
dfSimpleExp <- dfSimple %>%
  filter(dfSimple$ExpFiller == "Exp" | dfSimple$ExpFiller == "Filler")

# Get the columns that you will use for Simple Exposure experiment
dfSimpleExp <- dfSimpleExp %>%
  select(workerid, ExpFiller, condition, size, determiner, response, setting, target_figure3, target_object3, trial_number, response_times,
         genderUsed, correctGender, correctNoun, detUsed, correctDet)


#############################################
#############################################
#
# GET EXPERIMENT STATISTICS
#
#############################################
#############################################
# This is done before we exclude any trials
# due to participant errors

numTrialsPerParticipant <- nrow(dfExtraExp) / length(unique(dfExtraExp$workerid))

expectedGender <- dfExtraExp %>%
  group_by(target_figure3, ExpFiller) %>% #add "size" here if you want to graph this by size condition too
  count()

expectedNoun <- dfExtraExp %>%
  group_by(ExpFiller) %>%
  count()

expectedDet <- dfExtraExp %>%
  group_by(condition, ExpFiller, size) %>%
  count()


#############################################
#############################################
#
# EXCLUDE PARTICIPANTS WITH HIGHER THAN 20%
# ERROR RATES
#
#############################################
#############################################


# PARTICIPANT EXCLUSION FOR EXTRA EXPOSURE TRIAL EXPERIMENT


# Get participant accuracy statistics

participantStatsExtraNoun <- dfExtraExp %>%
  group_by(workerid, correctNoun) %>%
  count(vars = "correctNoun") %>%
  select(-vars) %>%
  rename(correctNounCount = n)
  
participantStatsExtraGender <- dfExtraExp %>%
  group_by(workerid, correctGender) %>%
  count(vars = "correctGender") %>%
  select(-vars) %>%
  rename(correctGenderCount = n)

participantStatsExtraDetUsed <- dfExtraExp %>%
  group_by(workerid, detUsed) %>%
  count(vars = "detUsed") %>%
  select(-vars) %>%
  rename(detCount = n)

participantStatsExtraCorrectDet <- dfExtraExp %>%
  group_by(workerid, correctDet) %>%
  count(vars = "correctDet") %>%
  select(-vars) %>%
  rename(correctDetCount = n)

# List that will hold workerIDs of all participants that should be excluded
idsExtra <- list()

# get worker id's of people with higher than 20% noun error rate
wrongNounErrorExtra <- participantStatsExtraNoun %>%
  filter(correctNoun == 0 & correctNounCount > numTrialsPerParticipant*0.2) %>%
  pull(workerid)

idsExtra <- append(idsExtra, wrongNounErrorExtra)

# get worker id's of people with higher than 20% gender error rate
wrongGenderErrorExtra <- participantStatsExtraGender %>%
  filter(correctGender == 0 & correctGenderCount > numTrialsPerParticipant*0.2) %>%
  pull(workerid)

idsExtra <- append(idsExtra, wrongGenderErrorExtra)


# get worker id's of people with higher than 20% determiner error rate
wrongDetErrorExtra <- participantStatsExtraCorrectDet %>%
  filter(correctDet == 0 & correctDetCount > numTrialsPerParticipant*0.2) %>%
  pull(workerid)

idsExtra <- append(idsExtra, wrongDetErrorExtra)
idsExtra <- unname(unlist(idsExtra))

print(paste("Number of participants excluded: ",length(idsExtra)))
# "Number of participants excluded:  0"

# Get rid of all participants with >20% error rates
dfExtraExp <- dfExtraExp %>%
  filter(!(workerid %in% idsExtra))

# Get rid of trials with wrong nouns
preNounExclusionNumExtra <- nrow(dfExtraExp)
dfExtraExp <- dfExtraExp %>%
  filter(correctNoun == 1)

postNounExclusionNumExtra <- nrow(dfExtraExp)
trialExclusionNounExtra <- preNounExclusionNumExtra - postNounExclusionNumExtra
print(paste("Number of trials excluded due to wrong Noun: ",trialExclusionNounExtra))
# "Number of trials excluded due to wrong Noun:  3"

# get rid of trials with wrong gender
preGenderExclusionNumExtra <- nrow(dfExtraExp)
dfExtraExp <- dfExtraExp %>%
  filter(correctGender == 1)

postGenderExclusionNumExtra <- nrow(dfExtraExp)
trialExclusionGenderExtra <- preGenderExclusionNumExtra - postGenderExclusionNumExtra
print(paste("Number of trials excluded due to wrong Gender: ",trialExclusionGenderExtra))
#"Number of trials excluded due to wrong Gender:  0"

# Get rid of trails with wrong determiner (and not trials with no determiner)
preDetExclusionNumExtra <- nrow(dfExtraExp)
dfExtraExp <- dfExtraExp %>%
  filter(correctDet == 1)

postDetExclusionNumExtra <- nrow(dfExtraExp)
trialExclusionDetExtra <- preDetExclusionNumExtra - postDetExclusionNumExtra
print(paste("Number of trials excluded due to wrong Determiner: ",trialExclusionDetExtra))
# "Number of trials excluded due to wrong Determiner:  0"

# Total number of trials that were excluded
trialsExcludedExtra <- trialExclusionNounExtra + trialExclusionGenderExtra + trialExclusionDetExtra
print(paste("Total number of trials excluded due to incorrect answers in Extra Practice: ", trialsExcludedExtra))
# "Total number of trials excluded due to incorrect answers in Extra Practice:  3"



# PARTICIPANT EXCLUSION FOR SIMPLE EXPOSURE TRIAL EXPERIMENT
# Same as code above


# Get by participant accuracy statistics

participantStatsSimpleNoun <- dfSimpleExp %>%
  group_by(workerid, correctNoun) %>%
  count(vars = "correctNoun") %>%
  select(-vars) %>%
  rename(correctNounCount = n)

participantStatsSimpleGender <- dfSimpleExp %>%
  group_by(workerid, correctGender) %>%
  count(vars = "correctGender") %>%
  select(-vars) %>%
  rename(correctGenderCount = n)

participantStatsSimpleDetUsed <- dfSimpleExp %>%
  group_by(workerid, detUsed) %>%
  count(vars = "detUsed") %>%
  select(-vars) %>%
  rename(detCount = n)

participantStatsSimpleCorrectDet <- dfSimpleExp %>%
  group_by(workerid, correctDet) %>%
  count(vars = "correctDet") %>%
  select(-vars) %>%
  rename(correctDetCount = n)


# List that will collect worker IDs for participants that should be excluded
idsSimple <- list()

# get worker id's of people with higher than 20% noun error rate
wrongNounErrorSimple <- participantStatsSimpleNoun %>%
  filter(correctNoun == 0 & correctNounCount > numTrialsPerParticipant*0.2) %>%
  pull(workerid)

idsSimple <- append(idsSimple, wrongNounErrorSimple)

# get worker id's of people with higher than 20% gender error rate
wrongGenderErrorSimple <- participantStatsSimpleGender %>%
  filter(correctGender == 0 & correctGenderCount > numTrialsPerParticipant*0.2) %>%
  pull(workerid)

idsSimple <- append(idsSimple, wrongGenderErrorSimple)


# get worker id's of people with higher than 20% determiner error rate
wrongDetErrorSimple <- participantStatsSimpleCorrectDet %>%
  filter(correctDet == 0 & correctDetCount > numTrialsPerParticipant*0.2) %>%
  pull(workerid)

idsSimple <- append(idsSimple, wrongDetErrorSimple)
idsSimple <- unname(unlist(idsSimple))

print(paste("Number of participants excluded: ",length(idsSimple)))
# "Number of participants excluded:  0"


# Get rid of all participants with >20% error rates
dfSimpleExp <- dfSimpleExp %>%
  filter(!(workerid %in% idsSimple))

# Get rid of trials with wrong nouns
preNounExclusionNumSimple <- nrow(dfSimpleExp)
dfSimpleExp <- dfSimpleExp %>%
  filter(correctNoun == 1)

postNounExclusionNumSimple <- nrow(dfSimpleExp)
trialExclusionNounSimple <- preNounExclusionNumSimple - postNounExclusionNumSimple
print(paste("Number of trials excluded due to wrong Noun: ",trialExclusionNounSimple))
# "Number of trials excluded due to wrong Noun:  1"

# get rid of trials with wrong gender
preGenderExclusionNumSimple <- nrow(dfSimpleExp)
dfSimpleExp <- dfSimpleExp %>%
  filter(correctGender == 1)

postGenderExclusionNumSimple <- nrow(dfSimpleExp)
trialExclusionGenderSimple <- preGenderExclusionNumSimple - postGenderExclusionNumSimple
print(paste("Number of trials excluded due to wrong Gender: ",trialExclusionGenderSimple))
# "Number of trials excluded due to wrong Gender:  1"

# manually go in and get rid of trails with wrong determiner (and not trials with no determiner)
preDetExclusionNumSimple <- nrow(dfSimpleExp)
dfSimpleExp <- dfSimpleExp %>%
  filter(correctDet == 1)

postDetExclusionNumSimple <- nrow(dfSimpleExp)
trialExclusionDetSimple <- preDetExclusionNumSimple - postDetExclusionNumSimple
print(paste("Number of trials excluded due to wrong Determiner: ",trialExclusionDetSimple))
# "Number of trials excluded due to wrong Determiner:  2"

# Total number of trials excluded
trialsExcludedSimple <- trialExclusionNounSimple + trialExclusionGenderSimple + trialExclusionDetSimple
print(paste("Total number of trials excluded due to incorrect answers in Simple Practice: ", trialsExcludedSimple))
# "Total number of trials excluded due to incorrect answers in Simple Practice:  4"


#############################################
#############################################
#
# COMPARE EXCLUSION RATES FOR TRIALS OF
# TWO EXPERIMENTS
#
#############################################
#############################################


dfExclusion <- data.frame(
  experiment <- c("complex", "simple"),
  values <- c(trialsExcludedExtra, trialsExcludedSimple)
)

graphExclusionComparison <- dfExclusion %>%
  ggplot(aes(fill=experiment, y=values, x=experiment)) + 
  geom_bar(position="dodge", stat="identity") +
  theme(text = element_text(size = 16), plot.title = element_text(hjust = 0.5, size = 20)) +
  scale_fill_manual(values = c("complex" = "darkseagreen3",
                               "simple" = "plum3",
                               "expected" = "gray41")) +
  ylab("Number of trials excluded") +
  xlab("Pilot study type") +
  ggtitle("Number of trials excluded per pilot study")


jpeg(file="../graphs/trialExclusion_comparison.jpeg", width = 500, height = 500)
plot(graphExclusionComparison)
dev.off()


#############################################
#############################################
#
# COMPARE OVERALL USE OF GENDER TERMS
#
#############################################
#############################################


simpleGender <- dfSimpleExp %>%
  group_by(genderUsed, ExpFiller) %>% #add "size" here if you want to graph this by size condition too
  count()

simpleGender <- simpleGender %>%
  add_column(trialType = "simple")

complexGender <- dfExtraExp %>%
  group_by(genderUsed, ExpFiller) %>%
  count()

complexGender <- complexGender %>%
  add_column(trialType = "complex")

# expectedGender is initialized at the top of the script 

expectedGender <- expectedGender %>%
  add_column(trialType = "expected")

expectedGender <- expectedGender %>%
  rename(genderUsed = "target_figure3")

# rename all values in genderUsed column that are not {"boy", "girl", "noGender"} as "other"
simpleGender <- simpleGender %>%
  mutate(genderUsed = as.character(genderUsed)) %>%
  mutate(genderUsed = case_when(
    genderUsed != "boy" & genderUsed != "girl" & genderUsed != "noGender" ~ "other",
    TRUE ~ genderUsed
  ))

dfGenderComparison <- rbind(simpleGender, complexGender, expectedGender)

dfGenderComparison$trialType <- dfGenderComparison$trialType %>%
  factor(levels = c('expected', 'complex', 'simple'))

#Note that this graph does not include wrong answers for the simple and complex experiments

graphGenderComparison <- dfGenderComparison %>%
  ggplot(aes(fill=trialType, y=n, x=genderUsed)) + 
  facet_grid(. ~ ExpFiller) +
  geom_bar(position="dodge", stat="identity") +
  theme(text = element_text(size = 16), plot.title = element_text(hjust = 0.5, size = 20)) +
  scale_fill_manual(values = c("complex" = "darkseagreen3",
                              "simple" = "plum3",
                              "expected" = "gray41")) +
  ylab("Number of Responses") +
  xlab("Gender used in response") +
  ggtitle("The use of gender in the two pilot studies")

jpeg(file="../graphs/gender_comparison.jpeg", width = 750, height = 500)
plot(graphGenderComparison)
dev.off()

#############################################
#############################################
#
# COMPARE OVERALL USE OF NOUNS
#
#############################################
#############################################


simpleNoun <- dfSimpleExp %>%
  filter(correctNoun == 1) %>%
  group_by(ExpFiller) %>% #add "size" here if you want to graph this by size condition too
  count()

simpleNoun <- simpleNoun %>%
  add_column(trialType = "simple")

complexNoun <- dfExtraExp %>%
  filter(correctNoun == 1) %>%
  group_by(ExpFiller) %>%
  count()

complexNoun <- complexNoun %>%
  add_column(trialType = "complex")

#expectedNoun is initialized at the top of the script

expectedNoun <- expectedNoun %>%
  add_column(trialType = "expected")

dfNounComparison <- rbind(simpleNoun, complexNoun, expectedNoun)

dfNounComparison$trialType <- dfNounComparison$trialType %>%
  factor(levels = c('expected', 'complex', 'simple'))

#Note that this graph does not include wrong answers for the simple and complex experiments

graphNounComparison <- dfNounComparison %>%
  ggplot(aes(fill=trialType, y=n, x=trialType)) + 
  facet_grid(. ~ ExpFiller) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values = c("complex" = "darkseagreen3",
                               "simple" = "plum3",
                               "expected" = "gray41")) +
  theme(text = element_text(size = 16), plot.title = element_text(hjust = 0.5, size = 20)) +
  ylab("Number of Responses") +
  xlab("Pilot study type") +
  ggtitle("The use of nouns in the two pilot studies")

jpeg(file="../graphs/noun_comparison.jpeg", width = 750, height = 500)
plot(graphNounComparison)
dev.off()


#############################################
#############################################
#
# COMPARE OVERALL USE OF DETERMINERS
#
#############################################
#############################################

simpleDet <- dfSimpleExp %>%
  group_by(detUsed, ExpFiller, size) %>%
  count()

simpleDet <- simpleDet %>%
  add_column(trialType = "simple")

complexDet <- dfExtraExp %>%
  group_by(detUsed, ExpFiller, size) %>%
  count()

complexDet <- complexDet %>%
  add_column(trialType = "complex")

# expectedDet <- this variable is created at the start of the code
# in order to include all the trials that were deleted subsequently due to 
# incorrect participant responses

expectedDet <- expectedDet %>%
  add_column(trialType = "expected")

expectedDet <- expectedDet %>%
  rename(detUsed = condition)

dfDetComparison <- rbind(complexDet, simpleDet, expectedDet)

dfDetComparison$trialType <- dfDetComparison$trialType %>%
   factor(levels = c('expected', 'complex', 'simple'))

# rename all number values in detUsed by number
dfDetComparison$detUsed <- recode(dfDetComparison$detUsed, a = "num", one = "num",
                                  two = "num", three = "num", four = "num")
# resum values
dfDetComparison <- dfDetComparison %>%
  group_by(ExpFiller, size, trialType, detUsed) %>%
  summarize(n = sum(n))


# Note: number utterances can always apply, therefore we can get number values
# that are higher than the expected values

# Note: the filler trials are included in this graph. The filler trials with
# one or four items are encoded as having the expected determiner "Num". However,
# this does not mean that other determiners cannot apply. 

graphDetComparison <- dfDetComparison %>%
  ggplot(aes(fill=trialType, y=n, x = size)) + 
  facet_grid(ExpFiller ~ detUsed) +
  geom_bar(position="dodge", stat="identity") +
  theme(text = element_text(size = 16), plot.title = element_text(hjust = 0.5, size = 20)) +
  scale_fill_manual(values = c("complex" = "darkseagreen3",
                               "simple" = "plum3",
                               "expected" = "gray41")) +
  ylab("Number of Responses") +
  xlab("Size Condition") +
  ggtitle("The use of determiners in the two pilot studies") 

jpeg(file="../graphs/determiner_comparison.jpeg", width = 1000, height = 750)
plot(graphDetComparison)
dev.off()

