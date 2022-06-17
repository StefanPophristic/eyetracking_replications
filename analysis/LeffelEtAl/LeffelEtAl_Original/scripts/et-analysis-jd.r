###############################################################################
# this script takes preprocessed eye-tracking from 2x2 grid VWP data
# and generates summary statistics and spaghetti plots
# 
# see leffel, xiang, + kennedy 2016 SALT paper for details
# 
#   -tim (tjleffel@gmail.com)
###############################################################################

# load required packages 
# (load plyr before dplyr)
# library(plyr) 
# library(dplyr) 
# library(ggplot2)
# library(reshape2)

# setwd("/Users/titlis/cogsci/projects/stanford/projects/rel-abs-ET/salt-vwp-imprecise-data-2016/")
setwd('/Users/dan/git-repos/rel-abs-ET/salt-vwp-imprecise-data-2016')
source("helpers.R")

# read in wide format data
dat <- read.csv("data/vwp_impr_data.csv")

# cut to TlessC and TmoreC (the SALT 2016 data)
dat <- droplevels(dat[dat$group %in% c("TmoreC","TlessC"), ])

# code subj id as a factor
dat$subject <- as.factor(dat$subject)

# check nouns
table(dat$group,dat$target.noun)

# correct misspelled factor level (noun "cirlce")
dat$target.noun <- as.character(dat$target.noun)
dat$target.noun <- ifelse(
  dat$target.noun=="cirlce","circle",dat$target.noun
)
dat$target.noun <- as.factor(dat$target.noun)

# check adjectives
# note 'empty' items had wrong pics in TmoreC and TequalC, were thrown out
table(dat$group,dat$target.adjective)

# check number of subj (track loss subj's already removed)
length(levels(droplevels(dat[dat$group=="TmoreC", ]$subject))) # 26 subj TmoreC
length(levels(droplevels(dat[dat$group=="TlessC", ]$subject))) # 21 subj TlessC

# get frame w only relative and maximum (analyzed in SALT paper)
dat <- droplevels(dat[dat$adjectivetype %in% c("max","rel"), ])

# check that there are 72 rows per item per subj (except 15/20, 'empty')
xtabs(~itemid+subject, data=dat)

# want to reshape to long format w.r.t. display objects 
# (target, competitor, distractor.contrast, distractor)
# reshape to long format:
datLong = dat %>%
  gather(variable, value, -trialPosition, -subject, -trialid, -condition, -trialtype, -adjectivetype, -adjectiveonset, -nounonset, -itemid, -target.noun, -target.adjective, -target.color, -timestampsec, -timestampmicrosec, -validityrighteye, -preview.window, -adjective.window, -noun.window, -group, - timeline, - timeslice)

# eliminate lost data
datLong <- datLong[datLong$value != -1, ]

# get time in ms, post adj-onset (timeslice 0 is time-locked to adj onset)
datLong$time <- datLong$timeslice * 16.66

more.than.20.percent.wrong = c(18, 13, 31, 21, 29, 30, 39, 40, 20, 15)
datLong.filtered.by.clicks = filter(datLong, !(itemid %in% more.than.20.percent.wrong))
  # filter out conditions where more than 20% of people chose the wrong answer in click experiment

# going to calculate stats for each combination of splitVars
#plotDat = datLong %>%
plotDat = datLong.filtered.by.clicks %>% 
  group_by(timeslice, condition, variable, adjectivetype, time, group) %>%
  summarise(n=length(value),mean=mean(value),ci.low=ci.low(value),ci.high=ci.high(value)) %>%
  mutate(lower=mean-ci.low,higher=mean+ci.high)

# mean max adj duration 642ms, mean rel adj duration 669
plotDat$adjDuration <-
  ifelse(plotDat$adjectivetype=="max", 642, 669)

# make a df of mean adj durations by adjtype
vlines = data.frame(
  adjectivetype=levels(plotDat$adjectivetype), 
  vline = c(642, 669)
)

# split TlessC from TmoreC + plot separately
less <- droplevels(plotDat[plotDat$group=="TlessC", ])
more <- droplevels(plotDat[plotDat$group=="TmoreC", ])


# can also plot just target + competitor, since that is main comp of interest
#lessTC = less
#lessTC <- droplevels(less[less$variable %in% c("target","competitor","distractor.contrast"), ])
lessTC <- droplevels(less[less$variable %in% c("target","competitor"), ])
moreTC <- droplevels(more[more$variable %in% c("target","competitor"), ])

# can plot dfs:
#   - less   --> TlessC, all four objects
#   - more   --> TmoreC, all four objects
#   - lessTC --> TlessC, target + competitor only
#   - moreTC --> TmoreC, target + competitor only

# define color palettes for spaghetti plots
tarcomPal <- c("olivedrab4", "orange") # for target + competitor only  
fourPal = c("#3399cc", "#FF9900", "#669900", "#993300") # for all four objs

# plot style for final version of SALT paper 
# they are 4x7in LANDSCAPE, oct29-{more,less}-4x7L.pdf
# NOTE: OLDER VERSIONS OF ggplot2:: CAN CORRUPT OUTPUT
theme_set(theme_minimal(base_size = 16))
ggplot(lessTC, aes(x = time, y = mean, color = variable, linetype=variable)) +
  facet_grid(condition ~ adjectivetype) +
  #scale_colour_manual(values = tarcomPal) +
  scale_colour_manual(values = fourPal) +
  # can also distinguish by linetype, nice for target/competitor plots
  # scale_linetype_manual(values=c("solid","longdash")) + 
  geom_ribbon(aes(ymin = lower, ymax = higher), alpha = .275) +
  geom_point(aes(color = variable), size = .5) + 
  geom_line(aes(color = variable), size = .5) +
  theme(legend.title = element_blank(), legend.position = "right", 
        axis.line.x = element_line(color = "black"), 
        axis.text.x = element_text(hjust = 1, vjust=1, angle = 30),
        axis.line.y = element_line(color = "black")) +
  scale_x_continuous(breaks = seq(0,1200, by = 200), limits = c(0,1200), 
                     "time after adjective onset (ms)", expand = c(0,0)) + 
  scale_y_continuous("proportion of looks", limits = c(0,.9)) +
  geom_vline(aes(xintercept = vline), data = vlines, 
             linetype = 2, size = .2, color = "blue") +
  theme(axis.ticks.x=element_line(color="black"),
        axis.ticks.length = unit(.5, "lines"),
        axis.ticks.y=element_line(color="black")) +
  theme(panel.margin=unit(.5, "lines"),
        strip.background=element_rect(color="white",fill="white"),
        strip.text.x=element_text(face="bold"),
        strip.text.y=element_text(face="bold"),
        panel.grid.minor.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.background=element_rect(color="black")) +
  ggtitle("T < C, filtered")


#compute proportions by time window
head(datLong)
summary(datLong)
table(datLong$time)
#datLong$meanAdjDuration = ifelse(datLong$adjectivetype=="max", 642, 669)
#datLong = droplevels(datLong[datLong$time < datLong$meanAdjDuration,])

# FAULTY BECAUSE TIME IS ALIGNED AT ADJ ONSET
# prior window: 300ms prior to adjective onset+200ms
# datLong$window_prior = ifelse(datLong$time < datLong$adjectiveonset + 200 & datLong$time > datLong$adjectiveonset - 300, T, F)
# # adjective window: adjective onset + 200ms to noun onset + 200
# datLong$window_adjective = ifelse(datLong$time >= datLong$adjectiveonset + 200 & datLong$time < datLong$nounonset + 200, T, F)
# # noun window: noun onset + 200ms to 500ms later
# datLong$window_noun = ifelse(datLong$time >= datLong$nounonset + 200 & datLong$time < datLong$nounonset + 200 + 500, T, F)

# prior window: 300ms prior to adjective onset+200ms
datLong$window_prior = ifelse(datLong$time < 200, T, F)
# adjective window: adjective onset + 200ms to noun onset + 200
datLong$window_adjective = ifelse(datLong$time >= 200 & datLong$time < (datLong$nounonset - datLong$adjectiveonset + 400), T, F)
# noun window: noun onset + 200ms to 500ms later
datLong$window_noun = ifelse(datLong$time >= (datLong$nounonset - datLong$adjectiveonset + 400) & datLong$time < (datLong$nounonset - datLong$adjectiveonset + 400 + 500), T, F)


# exclude those cases that fall into no window
datLong = droplevels(datLong[datLong$window_noun | datLong$window_prior | datLong$window_adjective,])
# datLong = droplevels(datLong[datLong$window_noun | datLong$window_prior | datLong$window_adjective_early | datLong$window_adjective_late | datLong$window_adjective_all | datLong$window_noun | datLong$window_adj_noun,])
datLong$Region = as.factor(datLong$variable)
datLong$Window = "prior"
datLong[datLong$window_adjective,]$Window = "adjective"
# datLong[datLong$window_adjective_early,]$Window = "adj_early"
# datLong[datLong$window_adjective_late,]$Window = "adj_late"
datLong[datLong$window_noun,]$Window = "noun"
summary(datLong)

# get only eye data for TlessC condition
props_diff = datLong %>%
  filter(group == "TlessC" & value == 1) %>%
  group_by(condition,target.adjective,target.noun,target.color,itemid,adjectivetype,Window,Region) %>%
  summarise (totalLooks = n()) %>%
  mutate(Proportion = totalLooks / sum(totalLooks))

# old, unwieldy way of computing
# sums = datLong %>%
#   group_by(condition,target.adjective,target.noun,target.color,group,itemid,Window,adjectivetype) %>%
#   summarise(totalLooks=sum(value))
# sums = as.data.frame(sums)
# row.names(sums) = paste(sums$condition,sums$target.adjective,sums$target.noun,sums$target.color,sums$group,sums$itemid,sums$Window,sums$adjectivetype)
# 
# props = datLong %>%
#   group_by(condition,target.adjective,target.noun,target.color,group,itemid,Window,Region,adjType=adjectivetype) %>%
#   summarise(totalLooks=sum(value))
# props = as.data.frame(props)
# props$Sum = sums[paste(sums$condition,sums$target.adjective,sums$target.noun,sums$target.color,sums$group,sums$itemid,sums$Window,sums$adjectivetype),]$totalLooks
# props$Proportion = props$totalLooks/props$Sum
# nrow(props)
# table(props$target.color)
# head(props)

props[props$condition == "contrast" & props$target.adjective == "full" & props$group == "TmoreC",]

props[props$condition == "contrast" & props$itemid == 12 & props$group == "TlessC",]

# props$Combo = paste(props$target.adjective,props$target.color,props$target.noun)
# ggplot(props, aes(x=group,y=Proportion,fill=Region)) +
#   geom_bar(stat="identity",position="dodge",color="black") +
#   facet_grid(Combo~condition) +
#   theme(axis.text.x = element_text(angle=45,hjust=1,vjust=1),strip.text.y=element_text(size=8))
# ggsave("graphs/item_variation.pdf",height=40,width=6)
# 
# un=as.data.frame(table(unique(datLong[,c("adjectivetype","target.adjective","target.noun","target.color")])))
# un = droplevels(un[un$Freq > 0,])
# nrow(un)

# write.csv(props, file="data/output/proportions-2-adj-windows.csv",row.names=F,quote=F)
#write.csv(props, file="data/output/proportions-short-window.csv",row.names=F,quote=F)
#write.csv(props, file="data/output/proportions-long-window.csv",row.names=F,quote=F)
write.csv(props_diff, file="data/output/proportions-windows.csv",row.names=F,quote=F)
