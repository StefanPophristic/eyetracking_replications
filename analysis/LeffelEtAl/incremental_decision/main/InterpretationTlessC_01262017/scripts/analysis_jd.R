library(tidyverse)
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

df <- read.csv("Jan-26-2017-Batch_2666254_batch_results_intp_preprocessed.csv")
dfcrit <- df[df$SceneID %in% 1:40, ]

# Stefan Pophristic added this line (jun 17, 2022) to make script work
dfcrit <- dfcrit %>%
  rename(wrongShape = Answer.wrongShape,
         choicePrior = Answer.choicePrior,
         choiceAdj = Answer.choiceAdj,
         choiceWhole = Answer.choiceWhole)

# drop levels in filler conditions
dfcrit$Condition <- factor(dfcrit$Condition)
dfcrit$choicePrior <- factor(dfcrit$choicePrior)
dfcrit$choiceAdj <- factor(dfcrit$choiceAdj)
dfcrit$choiceWhole <- factor(dfcrit$choiceWhole)
dfcrit$AdjectiveType <- factor(dfcrit$AdjectiveType)
dfcrit$SceneID <- factor(dfcrit$SceneID)

xtabs(~Condition + choiceAdj + AdjectiveType, dfcrit)

dfwrong <- dfcrit[dfcrit$choiceWhole != "Target", ]
sort(table(as.factor(dfwrong$SceneID)), decreasing = TRUE)

sort(table(dfwrong$WorkerId), decreasing = TRUE)

tf = function(m) {
  m[1,] = m[1,]/sum(m[1,])
  m[2,] = m[2,]/sum(m[2,])
  return(m)
}

resultsByItem = function(id) {
  dat = filter(dfcrit, SceneID == id) 
  return(list(
    prior = xtabs(~ dat$Condition + dat$choicePrior),
    adj = xtabs(~ dat$Condition + dat$choiceAdj),
    noun = xtabs(~ dat$Condition + dat$choiceWhole)
  ))
}

propsByItem = function(id) {
  dat = filter(dfcrit, SceneID == id) 
  return(list(
    prior = tf(xtabs(~ dat$Condition + dat$choicePrior)),
    adj = tf(xtabs(~ dat$Condition + dat$choiceAdj)),
    noun = tf(xtabs(~ dat$Condition + dat$choiceWhole))
  ))
}

itemResults = lapply(1:40, FUN=resultsByItem)
itemProps = lapply(1:40, FUN=propsByItem)

priorObs = dfcrit
priorObs$choice = priorObs$choicePrior
priorObs$input = 'prior'
priorObs$window = 1

adjObs = dfcrit
adjObs$choice = adjObs$choiceAdj
adjObs$input = 'adj'
adjObs$window = 2

nounObs = dfcrit
nounObs$choice = nounObs$choiceWhole
nounObs$input = 'noun'
nounObs$window = 3

byObs = rbind(priorObs, adjObs, nounObs) %>% 
  select(condition = Condition, input, window, choice, WorkerId)
  
selectID = function(i) filter(byObs, SceneID == i)


ggplot(data = selectID(2) %>% select(c1=choicePrior, c2=choiceAdj, c3=choiceWhole)) +
  geom_hist()


