library(tidyverse)
# library('BBmisc')
# Stefan: this library has the normalize() function, but it does not work with a table
# whereas Ciyang's does

this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)



df <- read.csv("../data/Jan-26-2017-Batch_2666254_batch_results_intp_preprocessed.csv")
dfcrit <- df[df$SceneID %in% 11:40, ]

# Stefan Pophristic added this line (jun 17, 2022) to make script work
dfcrit <- dfcrit %>%
  rename(wrongAdj = Answer.wrongAdj,
         wrongShape = Answer.wrongShape,
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
wrong.table = sort(table(as.factor(dfwrong$SceneID)), decreasing = TRUE)
more.than.20.percent.wrong = as.numeric(names(wrong.table[which(wrong.table > 20)]))
length(more.than.20.percent.wrong) # 10 of 30 items!

# hypothesis: the probability of the target item counting as 'empty' empty is low enough that people prefer to reinterpret the head noun (arrow is a kind of line, cube is a kind of cylinder)
# this is neutral btw vness vs imprevision analyses

# full table: 
# 18 13 31 21 29 30 39 40 20 15 27 14 22 36 33 16 37 38 11 17 34 12 25 35 19 28 32 24 26 23 
# 86 74 49 39 35 35 30 29 24 23 19 13 13 11 10  9  9  9  7  7  7  6  6  5  3  3  2  1  1  0 

barplot(sort(table(as.factor(dfwrong$SceneID)), decreasing = TRUE), main='Incorrect responses to full sentence', xlab='Item number', ylab='Number "wrong" of 100')

normalize(table(filter(dfcrit, SceneID %in% more.than.20.percent.wrong)$AdjectiveType))
  # 40% Max, 60% Rel

# the following scenes had more than 20% errors in the click data
  # i.e., chose the 'wrong' item, without time pressure, given the whole stim sentence
  # 18 13 31 21 29 30 39 40 20 15
  # all involving lines & arrows except 20 and 15, which involve cylinders and cubes

responses = function(sc) xtabs(~ Condition + choiceWhole, data = filter(dfcrit, SceneID == sc))
# e.g., 
responses(18) # 42/44 to competitor
responses(20) # 8/16 to competitor (contrast/nocontrast)
responses(15) # 11/12 to competitor

lapply(more.than.20.percent.wrong, FUN=responses)

  # THIS IS VERY COOL AND WORTH MENTIONING --- ERRORS REVEAL A NEW KIND OF CONTRAST EFFECT INTERACTING WITH NOUN CLASSIFICATION
    # the Gricean contrast effect identified by Sedivy also seems to play a role in the reinterpretation data
    # i.e., in most of these cases -- all but 18 and 15 -- there are more incorrect answers in the NoContrast condition
    # in other words, the Contrast conditions provided them with a way to satisfy the goal of inferring a contrast 
    # however, when there was no contrast in the 'official' labels, they were more likely to create a contrast one by reinterpreting one of the items as an instance of the noun category (usually, arrow => line, sometimes cube => cylinder)
# is there a difference in which adjectives are affected? 

Normalize(table(filter(dfcrit, SceneID %in% more.than.20.percent.wrong[2:9])$AdjectiveType))
table(filter(dfcrit, SceneID %in% more.than.20.percent.wrong[2:9])$TargetAdjective)
# Contrast vs lexical interp contrast:
  # 2 x long
  # 2 x thin
  # 2 x thick
  # 1 x straight
# with cubes & cylinders
  # 1 x empty (with cubes & cylinders)

# now look at remaining data, adj window, for rel/max & contrast effects 
priorObs = dfcrit
priorObs$choice = priorObs$choicePrior
priorObs$window = 'prior'
priorObs$windowNum = 1

adjObs = dfcrit
adjObs$choice = adjObs$choiceAdj
adjObs$window = 'adjective'
adjObs$windowNum = 2

nounObs = dfcrit
nounObs$choice = nounObs$choiceWhole
nounObs$window = 'noun'
nounObs$windowNum = 3

byObs = rbind(priorObs, adjObs, nounObs) %>% 
  select(condition = Condition, scene = SceneID, adj = TargetAdjective, noun = TargetNoun, color=TargetColor, window, choice, adjType = AdjectiveType)
byObs$condition = ifelse(byObs$condition == 'Contrast', 'contrast', 'nocontrast')
byObs$condition = factor(byObs$condition)
byObs$scene = factor(byObs$scene)
byObs$adj = factor(byObs$adj)
byObs$noun = factor(byObs$noun)
byObs$window = factor(byObs$window)
byObs$choice = factor(byObs$choice)
byObs$adjType = factor(ifelse(byObs$adjType == 'Max', 'max', 'rel'))

unfiltered.adj.window.props = filter(byObs, window=='adjective') %>% 
  group_by(condition, adjType, choice) %>% 
  summarize(count = n())  # 50 total observations per condition
ct = unfiltered.adj.window.props$count
unfiltered.adj.window.props$total = c(
  rep(sum(ct[1:4]), 4),
  rep(sum(ct[5:8]), 4),
  rep(sum(ct[9:12]), 4),
  rep(sum(ct[13:16]), 4)
)
unfiltered.adj.window.props$proportion = unfiltered.adj.window.props$count / unfiltered.adj.window.props$total

bootstrap.sample = function(counts) {
  dat = c(
    rep('a', counts[1]),
    rep('b', counts[2]),
    rep('c', counts[3]),
    rep('d', counts[4])
  )
  samp = sample(dat, size=length(dat), replace=T)
  return(Normalize(table(samp)))
}
bootstrap.ci = function(counts, n=10000) {
  samps = sapply(1:n, FUN=function(i) bootstrap.sample(counts))
  return(sapply(1:4, FUN=function(i) quantile(samps[i, ], c(.025, .975))))
}

unfiltered.cis = t(cbind(
  bootstrap.ci(unfiltered.adj.window.props$count[1:4]),
  bootstrap.ci(unfiltered.adj.window.props$count[5:8]),
  bootstrap.ci(unfiltered.adj.window.props$count[9:12]),
  bootstrap.ci(unfiltered.adj.window.props$count[13:16])
))

unfiltered.adj.window.props$lower.ci = unfiltered.cis[,1]
unfiltered.adj.window.props$upper.ci = unfiltered.cis[,2]

ggplot(data = unfiltered.adj.window.props, aes(x = choice, y = proportion, fill=choice)) +
  geom_bar(position=position_dodge(), stat='identity') +
  geom_errorbar(aes(ymin=lower.ci, ymax=upper.ci),
             width=.2,                    # Width of the error bars
            position=position_dodge(.9)) +
  facet_grid(condition ~ adjType) +
  ggtitle('Unfiltered click data, adjective window')



filtered.by.adjType.and.condition = filter(byObs, window=='adjective', !(scene %in% more.than.20.percent.wrong)) %>% 
  group_by(condition, adjType, choice) %>% 
  summarize(count = n())  # 50 total observations per condition
ct = filtered.by.adjType.and.condition$count
filtered.by.adjType.and.condition$total = c(
  rep(sum(ct[1:4]), 4),
  rep(sum(ct[5:8]), 4),
  rep(sum(ct[9:12]), 4),
  rep(sum(ct[13:16]), 4)
)
filtered.by.adjType.and.condition$proportion = filtered.by.adjType.and.condition$count / filtered.by.adjType.and.condition$total
# note totals: we've eliminated 300/1000 = 30% of the rel data and 200/500 = 40% of the max data



filtered.cis = t(cbind(
  bootstrap.ci(filtered.by.adjType.and.condition$count[1:4]),
  bootstrap.ci(filtered.by.adjType.and.condition$count[5:8]),
  bootstrap.ci(filtered.by.adjType.and.condition$count[9:12]),
  bootstrap.ci(filtered.by.adjType.and.condition$count[13:16])
))

filtered.by.adjType.and.condition$lower.ci = filtered.cis[,1]
filtered.by.adjType.and.condition$upper.ci = filtered.cis[,2]
  
ggplot(data = filtered.by.adjType.and.condition, aes(x = choice, y = proportion, fill=choice)) +
  geom_bar(position=position_dodge(), stat='identity') +
  geom_errorbar(aes(ymin=lower.ci, ymax=upper.ci),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  facet_grid(condition ~ adjType) +
  ggtitle('Click data, adjective window')
  
# general results:
  # rel adjectives: tendency to prefer target when contrast, competitor when not
  # max adjectives: tendency to prefer competitor in both conditions
    # this pattern makes sense given that it's T < C
    # competitor is always a better choice if you
# both models can rationalize this pattern: explain
  # 1-process model: 
  # 2-process model: 
# then, go back to eye data and see if the analysis sheds any light on those results

#  
# prep click data for comparison with eye data  
# 
 
choice.props = group_by(byObs, scene, condition, adj, noun, window, choice, adjType) %>% 
  summarize(proportion = n()/50)  # 50 total observations per condition
choice.props = as.data.frame(choice.props)
choice.props$scene = factor(choice.props$scene)
choice.props$condition = factor(choice.props$condition)                  
choice.props$adj = factor(choice.props$adj)
choice.props$noun = factor(choice.props$noun)
choice.props$window = factor(choice.props$window)
choice.props$choice = factor(choice.props$choice)
choice.props$adjType = factor(choice.props$adjType)

#
# compare to proportions Judith extracted from Leffel et al. ET data
#   Note that this data only has 11-40, i.e., no min adjectives
#

ET.data.long.window = read.csv('/Users/dan/git-repos/rel-abs-ET/salt-vwp-imprecise-data-2016/data/proportions-long-window.csv', header=T)
ET.data.short.window = read.csv('/Users/dan/git-repos/rel-abs-ET/salt-vwp-imprecise-data-2016/data/proportions-short-window.csv', header=T)
ET.data.2.adj.windows = read.csv('/Users/dan/git-repos/rel-abs-ET/salt-vwp-imprecise-data-2016/data/proportions-2-adj-windows.csv', header=T)

ET.data = ET.data.2.adj.windows

ET.data = filter(ET.data, group == 'TlessC') %>% # remove TmoreC data, since we didn't run this experiment yet
  select(scene=itemid, condition, adj = target.adjective, noun=target.noun, window=Window, choice=Region, adjType=adjType, proportion=Proportion) 
ET.data = ET.data[order(ET.data$scene, method='radix'),]

# recode distractor.contrast as contrast
ET.data$choice = ifelse(ET.data$choice == 'target', 'Target', 
  ifelse(ET.data$choice == 'competitor', 'Competitor', 
         ifelse(ET.data$choice == 'distractor', 'Distractor', 
                'Contrast')))

ET.data$scene = factor(ET.data$scene)
ET.data$condition = factor(ET.data$condition)
ET.data$adj = factor(ET.data$adj)
ET.data$noun = factor(ET.data$noun)
ET.data$window = factor(ET.data$window)
ET.data$choice = factor(ET.data$choice)
ET.data$adjType = factor(ET.data$adjType)
ET.data.sorted = ET.data[order(ET.data$scene, ET.data$condition, ET.data$adj, ET.data$noun, ET.data$window, ET.data$choice, method='radix'),]

ET.prior.data = filter(ET.data, window=='prior')
ET.prior.data = ET.prior.data[order(ET.prior.data$scene, ET.prior.data$condition, ET.prior.data$adj, ET.prior.data$noun, ET.prior.data$window, ET.prior.data$choice, method='radix'),]

ET.adj.early.data = filter(ET.data, window=='adj_early')
ET.adj.early.data = ET.adj.early.data[order(ET.adj.early.data$scene, ET.adj.early.data$condition, ET.adj.early.data$adj, ET.adj.early.data$noun, ET.adj.early.data$window, ET.adj.early.data$choice, method='radix'),]

ET.adj.late.data = filter(ET.data, window=='adj_late')
ET.adj.late.data = ET.adj.late.data[order(ET.adj.late.data$scene, ET.adj.late.data$condition, ET.adj.late.data$adj, ET.adj.late.data$noun, ET.adj.late.data$window, ET.adj.late.data$choice, method='radix'),]

ET.noun.data = filter(ET.data, window=='noun')
ET.noun.data = ET.noun.data[order(ET.noun.data$scene, ET.noun.data$condition, ET.noun.data$adj, ET.noun.data$noun, ET.noun.data$window, ET.noun.data$choice, method='radix'),]

# get it ready to create zeros in click data
ET.data = filter(ET.data, window != 'adj_late')
ET.data$window = ifelse(ET.data$window == 'adj_early', 'adjective', ifelse(ET.data$window == 'prior', 'prior', 'noun'))
ET.data$window = factor(ET.data$window)

length(ET.data$condition)
length(choice.props$condition) # number of observations doesn't match bc some objects were never chosen in the click experiment. make sure analysis treats these as '0' counts.

ET.data.sorted = ET.data[order(ET.data$scene, ET.data$condition, ET.data$adj, ET.data$noun, ET.data$window, ET.data$choice, method='radix'),]

choice.props.sorted = choice.props[order(choice.props$scene, choice.props$condition, choice.props$adj, choice.props$noun, choice.props$window, choice.props$choice, method='radix'),]

for (i in 1:8) {
  print(all(levels(ET.data[,i]) == levels(choice.props[,i])))
}
# all matched up in terms of levels...

h = sapply(11:40, FUN=function(i) {
  dd = filter(ET.data.sorted, scene==i)
  return(length(levels(factor(dd$window))) == 3)
})

# no eye movement noun data in scenes 12, 15, 17, 22, 24, 25, 30, 32, 34, 35!!

# option 1: drop these scenes

#missing.data.scenes = c(12, 15, 17, 22, 24, 25, 30, 32, 34, 35)
#choice.props.with.zeros = choice.props.sorted[!(choice.props.sorted$scene %in% missing.data.scenes),]
#ET.data.sorted = ET.data.sorted[!(ET.data.sorted$scene %in% missing.data.scenes),]

# option 2: just analyze prior and adjective windows

ET.data.sorted = ET.data.sorted[ET.data.sorted$window != 'noun',] 
choice.props.with.zeros = choice.props.sorted[choice.props.sorted$window != 'noun',]


# fill in zeros for click data
matched = F
while (!matched) {
  unmatched = sapply(1:length(ET.data.sorted[,1]), FUN=function(i) { 
    !all(ET.data.sorted[i,1:6] == choice.props.with.zeros[i,1:6])
  })
  first.unmatched = which(unmatched)[1]
  if (is.na(first.unmatched)) matched = T
  else {
    choice.props.with.zeros = rbind(choice.props.with.zeros, rep(NA, 7))
    ln = length(choice.props.with.zeros[,1])
    choice.props.with.zeros[ln, 1] = ET.data.sorted[first.unmatched, 1]
    choice.props.with.zeros[ln, 2] = ET.data.sorted[first.unmatched, 2]
    choice.props.with.zeros[ln, 3] = ET.data.sorted[first.unmatched, 3]
    choice.props.with.zeros[ln, 4] = ET.data.sorted[first.unmatched, 4]
    choice.props.with.zeros[ln, 5] = ET.data.sorted[first.unmatched, 5]
    choice.props.with.zeros[ln, 6] = ET.data.sorted[first.unmatched, 6]
    choice.props.with.zeros[ln, 7] = ET.data.sorted[first.unmatched, 7]
    choice.props.with.zeros[ln, 8] = 0
    choice.props.with.zeros = choice.props.with.zeros[order(choice.props.with.zeros$scene, choice.props.with.zeros$condition, choice.props.with.zeros$adj, choice.props.with.zeros$noun, choice.props.with.zeros$window, choice.props.with.zeros$choice, method='radix'),]
  }
}

length(choice.props.with.zeros[,1]) == length(ET.data.sorted[,1])
  # same length?
all(sapply(1:length(choice.props.with.zeros[,1]), FUN=function(i) { 
  all(ET.data.sorted[i,1:7] == choice.props.with.zeros[i,1:7])
})) # are they all matched up?

combined = 

#
# No reason to remove troublesome scenes! According to linking hypothesis,  uncertainty in click data should extend to eye movements too

click.prior.data = filter(choice.props.with.zeros, window=='prior')
click.adj.data = filter(choice.props.with.zeros, window=='adjective')
click.noun.data = filter(choice.props.with.zeros, window=='noun')

cor(ET.prior.data$proportion, click.prior.data$proportion) # prior window
  # cor = 0.07
cor(ET.adj.early.data$proportion, click.adj.data$proportion) # adj window, ET early
  # cor = 0.21
cor(ET.adj.late.data$proportion, click.adj.data$proportion) # adj window, ET late
# cor = 0.41

# no correlation in prior window (looks random in plot)
# pretty high correlation in adjective window
# what about nouns?

# adj window by adj type
cor(filter(ET.adj.early.data, adjType=='rel')$proportion, filter(click.adj.data, adjType=='rel')$proportion) 
  # .18
cor(filter(ET.adj.early.data, adjType=='max')$proportion, filter(click.adj.data, adjType=='max')$proportion) 
# .28
cor(filter(ET.adj.late.data, adjType=='rel')$proportion, filter(click.adj.data, adjType=='rel')$proportion) 
# .42
cor(filter(ET.adj.late.data, adjType=='max')$proportion, filter(click.adj.data, adjType=='max')$proportion) 
# .40


cor(filter(ET.adj.early.data, adjType=='rel', condition=='contrast')$proportion, filter(click.adj.data, adjType=='rel', condition=='contrast')$proportion) 
# .06
cor(filter(ET.adj.early.data, adjType=='max', condition=='contrast')$proportion, filter(click.adj.data, adjType=='max', condition=='contrast')$proportion) 
# .14
cor(filter(ET.adj.late.data, adjType=='rel', condition=='contrast')$proportion, filter(click.adj.data, adjType=='rel', condition=='contrast')$proportion) 
# .26
cor(filter(ET.adj.late.data, adjType=='max', condition=='contrast')$proportion, filter(click.adj.data, adjType=='max', condition=='contrast')$proportion) 
# .18

cor(filter(ET.adj.early.data, adjType=='rel', condition=='nocontrast')$proportion, filter(click.adj.data, adjType=='rel', condition=='nocontrast')$proportion) 
# .37
cor(filter(ET.adj.early.data, adjType=='max', condition=='nocontrast')$proportion, filter(click.adj.data, adjType=='max', condition=='nocontrast')$proportion) 
# .50
cor(filter(ET.adj.late.data, adjType=='rel', condition=='nocontrast')$proportion, filter(click.adj.data, adjType=='rel', condition=='nocontrast')$proportion) 
# .62
cor(filter(ET.adj.late.data, adjType=='max', condition=='nocontrast')$proportion, filter(click.adj.data, adjType=='max', condition=='nocontrast')$proportion) 
# .64




# adj window by adj type and condition
cor(filter(combined, window=='adjective', adjType=='rel', condition=='contrast')$ETprop, filter(combined, window=='adjective', adjType=='rel', condition=='contrast')$clickprop) 
# .82 
cor(filter(combined, window=='adjective', adjType=='rel', condition=='nocontrast')$ETprop, filter(combined, window=='adjective', adjType=='rel', condition=='nocontrast')$clickprop) 
# .74
cor(filter(combined, window=='adjective', adjType=='max', condition=='contrast')$ETprop, filter(combined, window=='adjective', adjType=='max', condition=='contrast')$clickprop) 
# .43
cor(filter(combined, window=='adjective', adjType=='max', condition=='nocontrast')$ETprop, filter(combined, window=='adjective', adjType=='max', condition=='nocontrast')$clickprop) 
# .47

par(mfrow=c(1,2))
plot(filter(combined, window=='prior')$ETprop, filter(combined, window=='prior')$clickprop, col='blue', pch=20, main='Raw proportions, prior window', xlab='Eye data', ylab='Choice data', xlim=c(0,1), ylim=c(0,1)) # prior window
plot(filter(combined, window=='adjective')$ETprop, filter(combined, window=='adjective')$clickprop, col='blue', pch=20, main='Raw proportions, adjective window', xlab='Eye data', ylab='Choice data', xlim=c(0,1), ylim=c(0,1)) # adj window


# plots, adj window, by adj type
plot(filter(combined, window=='adjective', adjType=='rel')$ETprop, filter(combined, window=='adjective', adjType=='rel')$clickprop, col='blue', pch=20, main='Adj window, Relative adjs', xlab='Eye data', ylab='Choice data', xlim=c(0,1), ylim=c(0,1)) # adj window, relative
plot(filter(combined, window=='adjective', adjType=='max')$ETprop, filter(combined, window=='adjective', adjType=='max')$clickprop, col='blue', pch=20, main='Adj window, Absolute adjs', xlab='Eye data', ylab='Choice data', xlim=c(0,1), ylim=c(0,1)) # adj window, maximum

ggplot(data = combined) +
  geom_point(mapping = aes(x=ETprop, y=clickprop), color='blue') +
  geom_smooth(mapping = aes(x=ETprop, y=clickprop), color='blue') +
  facet_grid(window ~ adjType)

ggplot(data = filter(combined, choice=='Target')) +
  geom_point(mapping = aes(x=ETprop, y=clickprop), color='blue') +
  geom_smooth(mapping = aes(x=ETprop, y=clickprop), color='blue') +
  facet_grid(window ~ adjType)

ggplot(data = filter(combined, window=='adjective', choice=='Target')) +
  geom_smooth(mapping = aes(x=ETprop, y=clickprop), color='blue') +
  geom_point(mapping = aes(x=ETprop, y=clickprop), color='blue') +
  facet_grid(condition ~ adjType)

ggplot(data = filter(combined, window=='adjective', choice=='Competitor')) +
  geom_smooth(mapping = aes(x=ETprop, y=clickprop), color='blue') +
  geom_point(mapping = aes(x=ETprop, y=clickprop), color='blue') +
  facet_grid(condition ~ adjType)

ggplot(data = filter(combined, window=='adjective', choice=='Contrast')) +
  geom_smooth(mapping = aes(x=ETprop, y=clickprop), color='blue') +
  geom_point(mapping = aes(x=ETprop, y=clickprop), color='blue') +
  facet_grid(condition ~ adjType)

ggplot(data = filter(combined, window=='adjective')) +
  geom_smooth(mapping = aes(x=ETprop, y=clickprop), color='blue') +
  geom_point(mapping = aes(x=ETprop, y=clickprop), color='blue') +
  facet_grid(condition ~ adjType)

# the most informative one so far ...
ggplot(data = filter(combined, window=='adjective')) +
  #geom_smooth(mapping = aes(x=ETprop, y=clickprop, color=choice)) +
  geom_point(mapping = aes(x=ETprop, y=clickprop, color=choice)) +
  facet_grid(adjType ~ condition) +
  xlim(0,1) +
  ylim(0,1)
  
# click data matches much more closely for relative adjectives.
# looks like our subjects were much more likely to choose the competitor during the adj window, for max adjectives.
# also, the ET data have a lot more looks to contrast & distractors than the choice data have clicks.


#QUESTIONS:
  # examples where adj window is better/worse
    # eg, click subjects appear much more likely to choose the competitor during the adj window, for max adjectives
    # look at individual examples for clues
  # systematicities in ET vs click data in adj window
    # rel vs abs
    # just target vs competitor looks vs clicks
  # ANOVA linear models with vs without rel/abs as predictor
  # other??

combined$diff = combined$ETprop - combined$clickprop

ggplot(data=filter(combined, window=='adjective')) +
  geom_density(mapping = aes(x=diff))
# looks fairly normal-ish, but very unevenly distributed among conditions:

ggplot(data=filter(combined, window=='adjective')) +
  geom_density(mapping = aes(x=diff, color=choice)) +
  facet_grid(condition ~ adjType) +
  ggtitle('Diffs')

# some descriptive stats
m0 = lm(combined$ETprop ~ combined$clickprop)
m1 = lm(combined$ETprop ~ combined$clickprop * combined$adjType)
anova(m0, m1) # highly significant - F = 8.3, p < .001
m2 = lm(combined$ETprop ~ combined$clickprop * combined$adjType * combined$condition)
anova(m1, m2) # p = .92 --- adding in condition as a predictor doesn't help

m3 = lm(combined$ETprop ~ combined$clickprop * combined$condition)
anova(m0, m3) # doesn't help even without adjType

#
# compare non-target looks by condition for click vs ET data.
# 

click.correct.contrast = dfcrit[dfcrit$choiceWhole == "Target" & dfcrit$Condition=='Contrast',]
click.correct.noContrast = dfcrit[dfcrit$choiceWhole == "Target" & dfcrit$Condition=='NoContrast', ]
sort(table(as.factor(click.correct.contrast$SceneID)), decreasing = F)/50
mean(sort(table(as.factor(click.correct.contrast$SceneID)), decreasing = F)/50) 
  # 84% in the contrast condition
mean(sort(table(as.factor(click.correct.noContrast$SceneID)), decreasing = F)/50)
  # 78% in the no-contrast condition

plot(density(table(as.factor(click.correct.contrast$SceneID))/50))
lines(density(table(as.factor(click.correct.noContrast$SceneID))/50), col='red')
# visually, it looks like click responses converged on the 'correct' answer more often in contrast conditions.



all(sapply(c(1:4,6,7), FUN=function(i) all(ET.adj.late.data[,i] == ET.adj.early.data[,i])))
all(sapply(c(1:4,6,7), FUN=function(i) all(ET.adj.late.data[,i] == click.adj.data[,i])))
all(sapply(c(1:4,6,7), FUN=function(i) all(ET.noun.data[,i] == click.adj.data[,i])))

click.adj.data$modality = 'click'
ET.adj.early.data$modality = 'ET_early'
ET.adj.late.data$modality = 'ET_late'
adj.data = rbind(click.adj.data, ET.adj.early.data, ET.adj.late.data)
adj.data$modality = factor(adj.data$modality)

ggplot(data = filter(adj.data, adjType=='rel'), aes(x = choice, y = proportion, color=condition)) +
  stat_summary(fun.data = "mean_cl_boot") +
  facet_grid(condition ~ modality) +
  ggtitle('Relative adjectives: 3 kinds of measurements')

ggplot(data = filter(adj.data, adjType=='max'), aes(x = choice, y = proportion, color=condition)) +
  stat_summary(fun.data = "mean_cl_boot") +
  facet_grid(condition ~ modality) +
  ggtitle('Maximum adjectives: 3 kinds of measurements')
