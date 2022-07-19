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
library(plyr) 
library(dplyr) 
library(ggplot2)
library(reshape2)

# read in wide format data
dat <- read.csv("vwp_impr_data.csv")

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
idCols <- 
  c("trialPosition","subject","trialid","condition","trialtype",
    "adjectivetype","adjectiveonset","nounonset","itemid","target.noun",
    "target.adjective","target.color","timestampsec","timestampmicrosec",
    "validityrighteye","preview.window","adjective.window","noun.window",
    "group", "timeline", "timeslice")
# reshape to long format:
datLong <- melt(dat, id=idCols)

# eliminate lost data
datLong <- datLong[datLong$value != -1, ]

# get time in ms, post adj-onset (timeslice 0 is time-locked to adj onset)
datLong$time <- datLong$timeslice * 16.66

# going to calculate stats for each combination of splitVars
splitVars <- 
  c("timeslice","condition","variable","adjectivetype","time","group")

# get stats 
plotDat <- 
  ddply(datLong, splitVars, summarise,
        n=length(value),
        mean=mean(value),
        sd=sd(value),
        se=sd/sqrt(n),
        lower=mean-se, 
        higher=mean+se)

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
  scale_colour_manual(values = tarcomPal) +
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
        panel.background=element_rect(color="black"))

ggsave("leffelEtAl2016TlessC.pdf", width=7, height=4, units = "in")


# ddply(dat, c("subject","trialid"), summarise,
#       n=length(timeslice))


