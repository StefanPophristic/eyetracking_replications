library(tidyverse)

Normalize = function(v) v/sum(v)

#df <- read.csv("/Users/dan/git-repos/rel-abs-ET/MTurkExp/interpretation/3-9-17-TMoreC-Batch_2718136_batch_results_intp_preprocessed.csv")
df <- read.csv("/Users/dan/git_repos/rel-abs-ET/Experiments/InterpretationTmoreC_03092017/3-9-17-TMoreC-Batch_2718136_batch_results_remove5_intp_preprocessed.csv")

levels(factor(df$TargetAdjective))

# don't forget to fix extra whitespace in adjective names in .csv file!
# 'empty '
# 'full '
# 'long '
# 'short '
# 'straight '

dfcrit <- df[df$SceneID %in% 11:40, ]

# drop levels in filler conditions
dfcrit$Condition <- factor(dfcrit$Condition)
dfcrit$choicePrior <- factor(dfcrit$Answer.choicePrior)
dfcrit$choiceAdj <- factor(dfcrit$Answer.choiceAdj)
dfcrit$choiceWhole <- factor(dfcrit$Answer.choiceWhole)
dfcrit$AdjectiveType <- factor(dfcrit$AdjectiveType)
dfcrit$SceneID <- factor(dfcrit$SceneID)

xtabs(~Condition + choiceAdj + AdjectiveType, dfcrit)

dfwrong <- dfcrit[dfcrit$Answer.choiceWhole != "Target", ]
wrong.table = sort(table(as.factor(dfwrong$SceneID)), decreasing = TRUE)
#more.than.20.percent.wrong = as.numeric(names(wrong.table[which(wrong.table > 20)]))
length(more.than.20.percent.wrong) # only 2 of 30, as compared to 10 in TlessC condition!   # both are empty cube vs. cylinder prompts.

# full table: 

# 15 20 31 14 22 36 38 33 35 25 28 11 13 26 27 19 29 39 12 16 18 23 24 32 34 17 21 30  37 40  
# 33 22 10  9  9  9  7  6  6  5  5  4  4  4  4  2  2  2  1  1  1  1  1  1  1  0  0  0  0  0 

# From TlessC:
# 18 13 31 21 29 30 39 40 20 15 27 14 22 36 33 16 37 38 11 17 34 12 25 35 19 28 32 24 26 23 
# 86 74 49 39 35 35 30 29 24 23 19 13 13 11 10  9  9  9  7  7  7  6  6  5  3  3  2  1  1  0 

# huge difference!! only 2 very confusing items, 15 and 20 - both similarly confusing to level in TLessC.

barplot(sort(table(as.factor(dfwrong$SceneID)), decreasing = TRUE), main='Incorrect responses to full sentence', xlab='Item number', ylab='Number "wrong" of 100')

responses = function(sc) xtabs(~ Condition + choiceWhole, data = dfcrit[dfcrit$SceneID == sc,])
# e.g., 
responses(20) # 8/16 to competitor (contrast/nocontrast)
responses(15) # 11/12 to competitor

lapply(more.than.20.percent.wrong, FUN=responses)

  # Unlike the TLessC data, no clear evidence of contrast effect in 'errors' 

table(filter(dfcrit, SceneID %in% more.than.20.percent.wrong[2:9])$TargetAdjective)

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
  #geom_errorbar(aes(ymin=lower.ci, ymax=upper.ci),
        #     width=.2,                    # Width of the error bars
         #   position=position_dodge(.9)) +
  facet_grid(condition ~ adjType) +
  ggtitle('Unfiltered click data, adjective window')

filtered.by.adjType.and.condition = filter(byObs, window=='adjective') %>% #, !(scene %in% more.than.20.percent.wrong)) %>% 
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
  # geom_errorbar(aes(ymin=lower.ci, ymax=upper.ci),
    #            width=.2,                    # Width of the error bars
       #         position=position_dodge(.9)) +
  facet_grid(condition ~ adjType) +
  ggtitle('Click data, adjective window')
  
# general results: not much going on. Maybe a small advantage for competitor in relative adjectives, but could be noise. 

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

ET.data.long.window = read.csv('/Users/dan/git_repos/rel-abs-ET/salt-vwp-imprecise-data-2016/data/proportions-long-window.csv', header=T)
ET.data.short.window = read.csv('/Users/dan/git_repos/rel-abs-ET/salt-vwp-imprecise-data-2016/data/proportions-short-window.csv', header=T)
ET.data.2.adj.windows = read.csv('/Users/dan/git_repos/rel-abs-ET/salt-vwp-imprecise-data-2016/data/proportions-2-adj-windows.csv', header=T)

ET.data = ET.data.long.window

ET.data = filter(ET.data, group == 'TmoreC') %>% # remove TmoreC data, since we didn't run this experiment yet
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

# ET.adj.early.data = filter(ET.data, window=='adj_early')
# ET.adj.early.data = ET.adj.early.data[order(ET.adj.early.data$scene, ET.adj.early.data$condition, ET.adj.early.data$adj, ET.adj.early.data$noun, ET.adj.early.data$window, ET.adj.early.data$choice, method='radix'),]
# 
# ET.adj.late.data = filter(ET.data, window=='adj_late')
# ET.adj.late.data = ET.adj.late.data[order(ET.adj.late.data$scene, ET.adj.late.data$condition, ET.adj.late.data$adj, ET.adj.late.data$noun, ET.adj.late.data$window, ET.adj.late.data$choice, method='radix'),]

ET.adj.data = filter(ET.data, window=='adjective')
ET.adj.data = ET.adj.data[order(ET.adj.data$scene, ET.adj.data$condition, ET.adj.data$adj, ET.adj.data$noun, ET.adj.data$window, ET.adj.data$choice, method='radix'),]

ET.noun.data = filter(ET.data, window=='noun')
ET.noun.data = ET.noun.data[order(ET.noun.data$scene, ET.noun.data$condition, ET.noun.data$adj, ET.noun.data$noun, ET.noun.data$window, ET.noun.data$choice, method='radix'),]

# get it ready to create zeros in click data
#ET.data = filter(ET.data, window != 'adj_late')
#ET.data$window = ifelse(ET.data$window == 'adj_early', 'adjective', ifelse(ET.data$window == 'prior', 'prior', 'noun'))
ET.data$window = factor(ET.data$window)

length(ET.data$condition)
length(choice.props$condition) # number of observations doesn't match bc some objects were never chosen in the click experiment. make sure analysis treats these as '0' counts.

ET.data.sorted = ET.data[order(ET.data$scene, ET.data$condition, ET.data$adj, ET.data$noun, ET.data$window, ET.data$choice, method='radix'),]

choice.props.sorted = choice.props[order(choice.props$scene, choice.props$condition, choice.props$adj, choice.props$noun, choice.props$window, choice.props$choice, method='radix'),]

# remove scenes 15 and 20 from click data, since no data from eye movements here
choice.props.sorted = choice.props.sorted[!(choice.props.sorted$scene %in% c(15,20)),]

for (i in 1:8) {
  print(all(levels(ET.data[,i]) == levels(choice.props[,i])))
}
# all matched up in terms of levels?

h = sapply(11:40, FUN=function(i) {
  dd = filter(ET.data.sorted, scene==i)
  return(length(levels(factor(dd$window))) == 2)
})
h
# no eye movement noun data in scenes 12, 15, 17, 22, 24, 25, 30, 32, 34, 35

# no eye movement adj data in scenes 15 and 20!

# option 1: drop these scenes

#missing.data.scenes = c(12, 15, 17, 22, 24, 25, 30, 32, 34, 35)
#choice.props.with.zeros = choice.props.sorted[!(choice.props.sorted$scene %in% missing.data.scenes),]
#ET.data.sorted = ET.data.sorted[!(ET.data.sorted$scene %in% missing.data.scenes),]

# option 2: just analyze prior and adjective windows

ET.data.sorted = ET.data.sorted[ET.data.sorted$window != 'noun',] 
choice.props.with.zeros = choice.props.sorted[choice.props.sorted$window != 'noun',]
choice.props.with.zeros$scene = factor(choice.props.with.zeros$scene)
choice.props.with.zeros$adj = factor(choice.props.with.zeros$adj)
ET.data.sorted$adj = factor(ET.data.sorted$adj)

h = sapply(11:40, FUN=function(i) {
    dd = filter(ET.data.sorted, scene==i)
    return(length(levels(factor(dd$window))) == 2)
})
h

# fill in zeros for click data
matched = F
while (!matched) {
  unmatched = sapply(1:length(choice.props.with.zeros[,1]), FUN=function(i) { 
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

#
# No reason to remove troublesome scenes! According to linking hypothesis,  uncertainty in click data should extend to eye movements too

click.prior.data = filter(choice.props.with.zeros, window=='prior')
click.adj.data = filter(choice.props.with.zeros, window=='adjective')
click.noun.data = filter(choice.props.with.zeros, window=='noun')

cor(ET.prior.data$proportion, click.prior.data$proportion) # prior window
  # cor = 0.04
cor(ET.adj.data$proportion, click.adj.data$proportion) # adj window, ET early
  # cor = 0.86 !!

# no correlation in prior window (looks random in plot)
# very high correlation in adjective window
# what about nouns?

# adj window by adj type
cor(filter(ET.adj.data, adjType=='rel')$proportion, filter(click.adj.data, adjType=='rel')$proportion) 
  # .87
cor(filter(ET.adj.data, adjType=='max')$proportion, filter(click.adj.data, adjType=='max')$proportion) 
# .85


cor(filter(ET.adj.data, adjType=='rel', condition=='contrast')$proportion, filter(click.adj.data, adjType=='rel', condition=='contrast')$proportion) 
# .88
cor(filter(ET.adj.data, adjType=='max', condition=='contrast')$proportion, filter(click.adj.data, adjType=='max', condition=='contrast')$proportion) 
# .80

cor(filter(ET.adj.data, adjType=='rel', condition=='nocontrast')$proportion, filter(click.adj.data, adjType=='rel', condition=='nocontrast')$proportion) 
# .85
cor(filter(ET.adj.data, adjType=='max', condition=='nocontrast')$proportion, filter(click.adj.data, adjType=='max', condition=='nocontrast')$proportion) 
# .90

# make combined df
# sanity check - all rows matched?
length(choice.props.with.zeros[,1]) == length(ET.data.sorted[,1])
all(
    sapply(1:length(choice.props.with.zeros[,1]), FUN=function(i) {
        all(choice.props.with.zeros[i,1:7] == ET.data.sorted[i,1:7])
    })
)

# now combine, with separate columns for 2 kinds of proportions
choice.props.with.zeros$clickprop = choice.props.with.zeros$proportion
combined = choice.props.with.zeros
combined$ETprop = ET.data.sorted$proportion



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

ggplot(data = filter(combined, window=='adjective')) +
    geom_point(mapping = aes(x=ETprop, y=clickprop, color=choice)) +
    facet_grid(adjType ~ condition) +
    xlim(0,1) +
    ylim(0,1) +
    ggtitle("All clicks/looks, adjective window")

ggplot(data = filter(combined, window=='adjective')) +
    geom_smooth(mapping = aes(x=ETprop, y=clickprop, color=choice)) +
    geom_point(mapping = aes(x=ETprop, y=clickprop, color=choice)) +
    facet_grid(adjType ~ condition) +
    xlim(0,1) +
    ylim(0,1) +
    ggtitle("All clicks/looks, adjective window")

ggplot(data = filter(combined, choice=='Target')) +
  geom_point(mapping = aes(x=ETprop, y=clickprop), color='blue') +
  geom_smooth(mapping = aes(x=ETprop, y=clickprop), color='blue') +
  facet_grid(window ~ adjType) + 
    ggtitle("Clicks/looks to target only")

ggplot(data = filter(combined, window=='adjective', choice=='Target')) +
  #geom_smooth(mapping = aes(x=ETprop, y=clickprop), color='blue') +
  geom_point(mapping = aes(x=ETprop, y=clickprop), color='blue') +
  facet_grid(condition ~ adjType) + 
    ggtitle("Clicks/looks to target, adjective window")
# wtf is ggplot doing here when I include geom_smooth?

ggplot(data = filter(combined, window=='adjective', choice=='Competitor')) +
  # geom_smooth(mapping = aes(x=ETprop, y=clickprop), color='blue') +
  geom_point(mapping = aes(x=ETprop, y=clickprop), color='blue') +
  facet_grid(condition ~ adjType) +
    ggtitle("Clicks/looks to competitor, adjective window")

ggplot(data = filter(combined, window=='adjective', choice=='Contrast')) +
  #geom_smooth(mapping = aes(x=ETprop, y=clickprop), color='blue') +
  geom_point(mapping = aes(x=ETprop, y=clickprop), color='blue') +
  facet_grid(condition ~ adjType) +
    ggtitle("Clicks/looks to contrast, adjective window")




  
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
anova(m0, m1) # not significant: p = .47. [TLessC: highly significant, p < .001]
m2 = lm(combined$ETprop ~ combined$clickprop * combined$adjType * combined$condition)
anova(m1, m2) # p > .99 --- adding in condition as a predictor doesn't help

m3 = lm(combined$ETprop ~ combined$clickprop * combined$condition)
anova(m0, m3) # p > .99 - doesn't help even without adjType

#
# compare non-target looks by condition for click vs ET data.
# 

click.correct.contrast = dfcrit[dfcrit$choiceWhole == "Target" & dfcrit$Condition=='Contrast',]
click.correct.noContrast = dfcrit[dfcrit$choiceWhole == "Target" & dfcrit$Condition=='NoContrast', ]
sort(table(as.factor(click.correct.contrast$SceneID)), decreasing = F)/50
mean(sort(table(as.factor(click.correct.contrast$SceneID)), decreasing = F)/50) 
  # 91% in the contrast condition
mean(sort(table(as.factor(click.correct.noContrast$SceneID)), decreasing = F)/50)
  # 90% in the no-contrast condition

plot(density(table(as.factor(click.correct.contrast$SceneID))/50))
lines(density(table(as.factor(click.correct.noContrast$SceneID))/50), col='red')
