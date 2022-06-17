---
title: "Predicting proportion of looks from click data beliefs and utterance expectations"
author: "jdegen"
date: "Nov 11, 2017"
output: html_document
---
  
require(tidyverse)
library(forcats)
library(scales)
library(knitr)

this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source("helpers.R")

# read in eye movement data
eyedat = read.csv("../data/eyedata.csv")

# read in click data (same as Data/InterpretationTlessC_01262017/Jan-26-2017-Batch_2666254_batch_results_intp_preprocessed.csv)
clickdat = read.csv("../data/clickdata.csv")

# read in production probabilities (same as Data/ImprecisionPracticeListenerFreeProdFull40trials 10-5-17imprecision_freeprod_oct2017_120participants_117native_prodprobs.csv)
proddat = read.csv("../data/production.csv")

ed = eyedat %>%
  select(condition, itemid, Window, Region, totalLooks, Proportion) %>%
  rename(prop.eye = Proportion, freq.eye = totalLooks) %>%
  mutate(Region = recode(Region, distractor.contrast = "contrast"))

cd = clickdat %>%
  filter(Condition %in% c("Contrast","NoContrast")) %>%
  select(SceneID, Condition, Answer.choicePrior, Answer.choiceAdj, Answer.choiceWhole) %>%
  gather(Window, Region, -SceneID, -Condition) %>%
  mutate(Region = tolower(Region), Condition = tolower(Condition)) %>%
  mutate(Window = recode(Window, Answer.choicePrior = "prior",  Answer.choiceAdj = "adjective", Answer.choiceWhole = "noun")) %>%
  group_by(SceneID, Condition, Window, Region) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  rename(condition = Condition, itemid = SceneID, prop.click=freq, freq.click=n)

ecd = full_join(ed,cd,by=c("condition","itemid","Window","Region")) %>%
  filter(!is.na(prop.eye)) %>%
  replace_na(list(prop.click = 0, freq.click = 0))

prod = proddat %>%
  rename(condition = Condition, itemid = SceneID) %>% 
  mutate(condition = tolower(condition))
# add tiny probability to production probs to avoid zero probabilities
prod[,c(8,9,10,11,12,13,14,15,16,17)] = prod[,c(8,9,10,11,12,13,14,15,16,17)] + .000001
prod = prod %>%
  mutate(surpExactFirstWordUniPrior = -log(ExactFirstWordUniPrior), surpInitialMatchUniPrior = -log(InitialMatchUniPrior), surpMorphemeIncludedUniPrior = -log(MorphemeIncludedUniPrior), surpSynFirstUniPrior = -log(SynFirstUniPrior), surpSynIncludedUniPrior = -log(SynIncludedUniPrior), surpExactFirstWordEmpPrior = -log(ExactFirstWordEmpPrior), surpInitialMatchEmpPrior = -log(InitialMatchEmpPrior), surpMorphemeIncludedEmpPrior = -log(MorphemeIncludedEmpPrior), surpSynFirstEmpPrior = -log(SynFirstEmpPrior), surpSynIncludedEmpPrior = -log(SynIncludedEmpPrior))

fulld = full_join(ecd,prod,by=c("condition","itemid")) %>%
  filter(!is.na(Region)) %>%
  droplevels()
nrow(fulld)
head(fulld)
summary(fulld)

### EXPLORATION OF PRODUCTION PROBABILITIES ###

probmeasures = c("ExactFirstWordEmpPrior", "ExactFirstWordUniPrior", "InitialMatchUniPrior", "InitialMatchEmpPrior", "MorphemeIncludedEmpPrior", "MorphemeIncludedUniPrior", "SynFirstEmpPrior", "SynFirstUniPrior", "SynIncludedEmpPrior", "SynIncludedUniPrior")
gprod = prod[,c(2,3,5,7:27)] %>%
  gather(Measure, Value, -AdjectiveType, -itemid, -condition, -TargetAdjective) %>%
  group_by(Measure) %>%
  mutate(RescaledWeight=rescale(Value)) %>%
  mutate(MeasureType=ifelse(Measure %in% probmeasures, "probability", "surprisal"), Prior=gsub('^surp',"",Measure,perl=T)) 
# for the rescaled surprisal values subtract them from 1 (because higher needs to mean more weight on empirical values)
gprod[gprod$MeasureType == "surprisal",]$RescaledWeight = 1 - gprod[gprod$MeasureType == "surprisal",]$RescaledWeight

# plot probabilities/surprisals for different measures
ggplot(gprod, aes(x=Value)) +
  geom_density() +
  facet_wrap(~Measure, scales="free")

# test for mean diffs in prod probs by adjective type with raw values (probs and surprisals)
agr = gprod %>%
  group_by(AdjectiveType, MeasureType, Prior) %>%
  summarize(MeanProductionProbability = mean(Value), CILow=ci.low(Value), CIHigh=ci.high(Value)) %>%
  mutate(Ymin=MeanProductionProbability-CILow, Ymax=MeanProductionProbability+CIHigh)

ggplot(agr, aes(x=AdjectiveType, y=MeanProductionProbability)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=Ymin,ymax=Ymax),width=.25) +
  facet_grid(MeasureType~Prior, scales="free")

# test for mean diffs in prod probs by adjective type with rescaled values between 0 and 1 (ie probs and surprisals projected into [0,1] interval)
agr = gprod %>%
  group_by(AdjectiveType, MeasureType, Prior) %>%
  summarize(MeanProductionProbability = mean(RescaledWeight), CILow=ci.low(RescaledWeight), CIHigh=ci.high(RescaledWeight)) %>%
  mutate(Ymin=MeanProductionProbability-CILow, Ymax=MeanProductionProbability+CIHigh)

ggplot(agr, aes(x=AdjectiveType, y=MeanProductionProbability)) +
  geom_bar(stat="identity") +
  xlab("Mean rescaled probability / surprisal of adjective") +
  geom_errorbar(aes(ymin=Ymin,ymax=Ymax),width=.25) +
  facet_grid(MeasureType~Prior, scales="free")


# Correlation between exact first word probs for uniform and empirical prior
cor(prod$ExactFirstWordEmpPrior,prod$ExactFirstWordUniPrior) # .95
ggplot(prod, aes(x=ExactFirstWordUniPrior, y=ExactFirstWordEmpPrior, color=TargetAdjective)) +
  geom_point() +
  xlim(0,.2) +
  ylim(0,.2) +
  geom_abline(intercept=0, slope=1, linetype="dashed", color="gray60")

# Correlation between initial match word probs for uniform and empirical prior
cor(prod$InitialMatchUniPrior,prod$InitialMatchEmpPrior) # .93
ggplot(prod, aes(x=InitialMatchUniPrior, y=InitialMatchEmpPrior, color=TargetAdjective)) +
  geom_point() +
  xlim(0,.3) +
  ylim(0,.3) +
  geom_abline(intercept=0, slope=1, linetype="dashed", color="gray60")

# Correlation between "morpheme included" word probs for uniform and empirical prior
cor(prod$MorphemeIncludedUniPrior,prod$MorphemeIncludedEmpPrior) # .92
ggplot(prod, aes(x=MorphemeIncludedUniPrior, y=MorphemeIncludedEmpPrior, color=TargetAdjective)) +
  geom_point() +
  xlim(0,.3) +
  ylim(0,.3) +
  geom_abline(intercept=0, slope=1, linetype="dashed", color="gray60")

# Correlation between "synonym first" word probs for uniform and empirical prior
cor(prod$SynFirstUniPrior,prod$SynFirstEmpPrior) # .92
ggplot(prod, aes(x=SynFirstUniPrior, y=SynFirstEmpPrior, color=TargetAdjective)) +
  geom_point() +
  xlim(0,.3) +
  ylim(0,.3) +
  geom_abline(intercept=0, slope=1, linetype="dashed", color="gray60")

# Correlation between "synonym included" word probs for uniform and empirical prior
cor(prod$SynIncludedUniPrior,prod$SynIncludedEmpPrior) # .92
ggplot(prod, aes(x=SynIncludedUniPrior, y=SynIncludedEmpPrior, color=TargetAdjective)) +
  geom_point() +
  xlim(0,.35) +
  ylim(0,.35) +
  geom_abline(intercept=0, slope=1, linetype="dashed", color="gray60")

# So generally, the correlation between the different ways of estimating probs (pairwise) is high.


### EXPLORATION OF MIXING EMPIRICAL AND UNIFORM BELIEF DISTRIBUTION BY ADJECTIVE SURPRISAL/PROBABILITY ###
# correlations of click and eye data in different windows
fulld %>%
  group_by(Window) %>%
  summarize(Cor = cor(prop.click,prop.eye))

# correlations of click and eye data in different windows, separately for max and rel adjs
fulld %>%
  group_by(AdjectiveType, Window) %>%
  summarize(Cor = cor(prop.click,prop.eye))

# plot click data against eye data
ggplot(fulld, aes(x=prop.click,y=prop.eye,color=Region)) +
  geom_abline(intercept=0,slope=1,linetype="dashed",color="gray40") +
  geom_point() +
  geom_smooth(method="lm",aes(group=1)) +
  facet_grid(Window~AdjectiveType)

ggplot(fulld, aes(x=prop.click,y=prop.eye,color=Region)) +
  geom_abline(intercept=0,slope=1,linetype="dashed",color="gray40") +
  geom_point() +
  geom_text(aes(label=TargetAdjective), size=2.5,color="black") +
  geom_smooth(method="lm",aes(group=1)) +
  facet_grid(Window~AdjectiveType)

# join the gathered (scaled) production probabilities and the eye / click data for easy plotting
testd = fulld %>%
  filter(Window == "adjective") %>%
  select(condition,itemid,Region,prop.eye,prop.click,TargetAdjective) %>%
  full_join(gprod, by=c("itemid","condition","TargetAdjective")) %>%
  rowwise() %>%
  mutate(predicted.prop.eye = weighted.mean(x=c(prop.click,0.25),w=c(RescaledWeight, 1-RescaledWeight))) %>%
  filter(!is.na(predicted.prop.eye) & !is.na(prop.click)) %>%
  droplevels()

# the money plot: predicted data against eye data
ggplot(testd, aes(x=predicted.prop.eye,y=prop.eye,color=Region)) +
  geom_abline(intercept=0,slope=1,linetype="dashed",color="gray40") +
  geom_point() +
  xlim(0,1) +
  ylim(0,1) +
  geom_smooth(method="lm",aes(group=1)) +
  facet_grid(Measure~AdjectiveType)
ggsave("../graphs/predicted.eye.data.scaled.pdf",height=35,width=6)

results = testd %>%
  group_by(AdjectiveType,Measure) %>%
  summarize(Correlation=cor(prop.eye,predicted.prop.eye)) %>%
  arrange(Correlation)
View(results)
