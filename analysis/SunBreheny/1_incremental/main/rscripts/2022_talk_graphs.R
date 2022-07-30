library(tidyverse)
library(lme4)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("helpers.R")
setwd('../data')
theme_set(theme_bw())

# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") 

df = read.csv("trials_merged.csv", header = TRUE)
demo = read.csv("subject_info_merged.csv", header = TRUE)

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

### PART I: PLOT DATA FROM REPLICATION TASK

# plot proportion of selections by condition
toplot =  df %>%
  filter(ExpFiller=="Exp") %>%
  select(workerid,condition,size,click1,click2,click3,click4,target1,target2,competitor1,competitor2,instruction3, target_figure3) %>%
  mutate(ID = row_number()) %>%
  gather(click_number,location,click1:click4) %>%
  mutate(target=ifelse(location==target1,1,ifelse(location==target2,1,0))) %>%
  mutate(competitor=ifelse(location==competitor1,1,ifelse(location==competitor2,1,0))) %>%
  group_by(condition,size,target_figure3, click_number) %>%
  summarize(m_target=mean(target),m_competitor=mean(competitor),ci_low_target=ci.low(target),ci_high_target=ci.high(target),ci_low_competitor=ci.low(competitor),ci_high_competitor=ci.high(competitor)) %>%
  gather(location,Mean,m_target:m_competitor) %>%
  mutate(CILow=ifelse(location=="m_target",ci_low_target,ifelse(location=="m_competitor",ci_low_competitor,0))) %>%
  mutate(CIHigh=ifelse(location=="m_target",ci_high_target,ifelse(location=="m_competitor",ci_high_competitor,0))) %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh) %>%
  mutate(Region=fct_recode(location,"competitor"="m_competitor","target"="m_target")) %>%
  mutate(Region=fct_rev(Region)) %>%
  ungroup() %>%
  mutate(click_number=fct_recode(click_number,prior="click1",gender="click2",determiner="click3",noun="click4"))

toplotIDT <- toplot

# Stefan 2022 Degen Pophristic Talk Graphs
IDT <- toplotIDT %>%
  filter(condition == "some" & size == "big" & target_figure3 == "boy") %>%
  mutate(Region = case_when(Region == "target" ~ "Target Object",
                            Region == "competitor" ~ "Other Objects")) %>%
  mutate(Region = relevel(factor(Region), "Target Object")) %>%
  ggplot(aes(x =click_number, y = Mean, group=Region)) +
  geom_line(aes(color=Region),size=1.3) +
  geom_point(aes(color=Region),size=2.5,shape="square") +
  geom_errorbar(aes(ymin=YMin, ymax=YMax), width=.2, alpha=.3) +
  scale_fill_discrete(labels=c('Target Object', 'Other Objects')) +
  scale_color_manual(values=c("#227100","#BBEA73")) +
  xlab("Window") +
  ylab("Mean Proportion of Selections") 
  # +
  # ggtitle("Incremental Decision Task Data (one condition)")
  
IDT

ggsave("../graphs/stefan/idt.pdf",width=6,height=4)



# plot proportion of selections by condition and experiment half
toplot =  df %>%
  filter(ExpFiller=="Exp") %>%
  select(workerid,condition,size,click1,click2,click3,click4,target1,target2,competitor1,competitor2,instruction3,trial_group) %>%
  mutate(ID = row_number()) %>%
  gather(click_number,location,click1:click4) %>%
  mutate(target=ifelse(location==target1,1,ifelse(location==target2,1,0))) %>%
  mutate(competitor=ifelse(location==competitor1,1,ifelse(location==competitor2,1,0))) %>%
  group_by(condition,size,click_number,trial_group) %>%
  summarize(m_target=mean(target),m_competitor=mean(competitor),ci_low_target=ci.low(target),ci_high_target=ci.high(target),ci_low_competitor=ci.low(competitor),ci_high_competitor=ci.high(competitor)) %>%
  gather(location,Mean,m_target:m_competitor) %>%
  mutate(CILow=ifelse(location=="m_target",ci_low_target,ifelse(location=="m_competitor",ci_low_competitor,0))) %>%
  mutate(CIHigh=ifelse(location=="m_target",ci_high_target,ifelse(location=="m_competitor",ci_high_competitor,0))) %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh) %>%
  mutate(Region=fct_recode(location,"competitor"="m_competitor","target"="m_target")) %>%
  mutate(Region=fct_rev(Region)) %>%
  ungroup() %>%
  mutate(click_number=fct_recode(click_number,prior="click1",gender="click2",determiner="click3",noun="click4"))


### PART II: PLOT CATEGORICAL DATA AGAINST EYE MOVEMENT DATA

# load eye-tracking data from Sun&Breheny ---> not sure about these variables: TrialId, mean, subject, unique,TETTime, RTTime, time
baseline = read.csv("sb_eyetracking/exp200ms_beselinedata.csv", header = TRUE)
gender = read.csv("sb_eyetracking/exp200ms_genderdata.csv", header = TRUE)
determiner = read.csv("sb_eyetracking/exp200ms_determiner.csv", header = TRUE)
name = read.csv("sb_eyetracking/exp200ms_namedata.csv", header = TRUE)
end = read.csv("sb_eyetracking/exp200ms_enddata.csv", header = TRUE)
preview = read.csv("sb_eyetracking/exp200ms_previewdata.csv", header = TRUE)

# order should be: baseline / gender / determiner + name / noun ---> will ignore  "preview" since there's no corresponding window in the incremental decision experiment

g = rbind(baseline,gender,determiner,name,end)  %>% 
  mutate(item=word(as.character(instruction), -1))

# re-load incremental decision data 
s = read.csv("trials_merged.csv", header = TRUE)  %>% 
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
  select(workerid,Prime,condition,determiner,size,window,location,target1,target2,competitor1,competitor2,item, target_figure3) %>%
  mutate(targetclick=ifelse(location==target1,1,ifelse(location==target2,1,0))) %>%
  mutate(competitorclick=ifelse(location==competitor1,1,ifelse(location==competitor2,1,0))) %>%
  mutate(distractorclick=ifelse(targetclick=="1",0,ifelse(competitorclick=="1",0,1))) %>%
  group_by(determiner,size,target_figure3,window,item) %>%
  summarize(Mean_target_selection=mean(targetclick),Mean_competitor_selection=mean(competitorclick),Mean_distractor_selection=mean(distractorclick)) %>%
  rename(gender = "target_figure3")

gaze =  g %>%
  filter(TrackLoss=="FALSE") %>%
  select(Prime,condition,determiner,size,targetlook,competitorlook,residuelook,whichword,item, target.figure) %>%
  mutate(distractorlook=ifelse(targetlook=="1",0,ifelse(competitorlook=="1",0,ifelse(residuelook=="1",0,1)))) %>%
  mutate(targetdistractorlook = ifelse(targetlook=="1",1,ifelse(distractorlook=="1",1,0))) %>%
  mutate(competitordistractorlook = ifelse(competitorlook=="1",1,ifelse(distractorlook=="1",1,0))) %>%
  mutate(window=as.character(whichword)) %>%
  mutate(window = ifelse(whichword =="determiner","determiner+name", ifelse(whichword=="name","determiner+name",ifelse(whichword=="end","noun",window)))) %>%
  group_by(determiner,size, target.figure, window,item) %>%
  summarize(Mean_target_look=mean(targetlook),Mean_competitor_look=mean(competitorlook),Mean_distractor_look=mean(distractorlook),Mean_targetdistractor_look=mean(targetdistractorlook),Mean_competitordistractor_look=mean(competitordistractorlook)) %>%
  rename(gender = "target.figure")

df = merge(selection, gaze, by=c("determiner","size","gender", "window","item"))
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
  left_join(longer_selections,by=c("determiner","size", "gender", "window","Region","item")) %>% 
  mutate(determiner=fct_recode(determiner,"number"="two","number"="three")) %>% 
  mutate(Region=fct_relevel(Region,"target","competitor"),window=fct_relevel(window,"baseline","gender")) %>% 
  droplevels()

# overall correlation between eye movement and decision task data
cor.test(toplot$prop_looks,toplot$prop_selections) # .87

# correlation between eye movement and decision task data separately by time window
cors_window = toplot %>%
  group_by(window) %>%
  summarize(Correlation=round(cor.test(prop_selections,prop_looks)$estimate,2),P=round(cor.test(prop_selections,prop_looks)$p.value,5))


# collapsing across items
# agr = toplot %>%
#   group_by(window,Region,determiner,size) %>%
#   summarize(prop_selections = mean(prop_selections),prop_looks=mean(prop_looks))
# 
# cors_window_it = agr %>%
#   group_by(window) %>%
#   summarize(Correlation=round(cor.test(prop_selections,prop_looks)$estimate,2),P=round(cor.test(prop_selections,prop_looks)$p.value,5))
# 
# 
# # correlation between eye movement and decision task data separately by condition within determiner window
# cors_determiner = toplot %>%
#   filter(window == "determiner+name") %>%
#   group_by(determiner, size) %>%
#   summarize(Correlation=round(cor.test(prop_selections,prop_looks)$estimate,2),P=round(cor.test(prop_selections,prop_looks)$p.value,5))

# collapsing across items
agr = toplot %>%
  filter(window == "determiner+name") %>%
  group_by(window,Region,determiner,size, gender) %>%
  summarize(prop_selections = mean(prop_selections),prop_looks=mean(prop_looks))

# cors_determiner_it = agr %>%
#   group_by(determiner, size) %>%
#   summarize(Correlation=round(cor.test(prop_selections,prop_looks)$estimate,2),P=round(cor.test(prop_selections,prop_looks)$p.value,5))
# cors_determiner_it # all close to 1 but p > .05 (too few data points)
# 
# cors_determiner_it = agr %>%
#   group_by(determiner, size, gender) %>%
#   summarize(Correlation=round(cor.test(prop_selections,prop_looks)$estimate,2),P=round(cor.test(prop_selections,prop_looks)$p.value,5))
# cors_determiner_it # all close to 1 but p > .05 (too few data points)

cors_determiner_it = toplot %>%
  group_by(determiner, size, gender) %>%
  summarize(Correlation=round(cor.test(prop_selections,prop_looks)$estimate,2),P=round(cor.test(prop_selections,prop_looks)$p.value,5))
cors_determiner_it # all close to 1 but p > .05 (too few data points)
# 1 all        big   boy           0.89     0
# 2 all        big   girl          0.9      0
# 3 all        small boy           0.85     0
# 4 all        small girl          0.84     0
# 5 some       big   boy           0.82     0
# 6 some       big   girl          0.89     0
# 7 some       small boy           0.86     0
# 8 some       small girl          0.86     0
# 9 number     big   boy           0.86     0
# 10 number     big   girl          0.9      0
# 11 number     small boy           0.85     0
# 12 number     small girl          0.85     0


# # correlation between eye movement and decision task data separately by region
# cors_reg = toplot %>%
#   group_by(Region) %>%
#   summarize(Correlation=round(cor.test(prop_selections,prop_looks)$estimate,2),P=round(cor.test(prop_selections,prop_looks)$p.value,5))
# 

# Stefan 2022 talk plots
# oneSceneCorrelation = toplot %>%
  
# oneCorrelation <- toplot %>%
#   filter(determiner == "some", gender == "boy", size == "big", window == "determiner+name", Region == "target") %>%
#   ggplot(aes(x=prop_selections, y=prop_looks, group=1)) +
#   geom_point(size=2,aes(color=window,shape=Region),alpha=.7) +
#   geom_smooth(method='lm',size=1,color="grey26") +
#   geom_abline(slope=1,linetype="dotted",color="gray40") +
#   # geom_text(data=cors, aes(label=paste("r=",Correlation)), x=.2,y=.9) +
#   scale_color_manual(values=c(cbPalette[1],cbPalette[1],cbPalette[5],cbPalette[7])) +
#   labs(shape="Region",
#        color="Window",
#        x="Proportion of selections (Exp. 1)",
#        y="Proportion of looks (S. & B., 2020)") +
#   xlim(0,1) +
#   ylim(0,1) +
#   # coord_fixed() +
#   facet_grid(size~determiner) +
#   theme(legend.direction = "horizontal", legend.box = "vertical") +
#   theme(legend.position="top",
#         legend.margin=margin(0,0,0,0),
#         legend.box.margin=margin(-10,-10,-10,-10),legend.spacing.y = unit(0.001, 'cm'))#,legend.box.spacing = unit(0.01, 'cm'),) 
# 
# oneCorrelation
# 
# allCorrelationBoy <- toplot %>%
#   filter(Region == "target", window == "determiner+name", gender == "boy") %>%
#   ggplot(aes(x=prop_selections, y=prop_looks, group=gender)) +
#   geom_point(size=2,aes(color=gender),alpha=.7) +
#   geom_smooth(method='lm',size=1,color="grey26") +
#   geom_abline(slope=1,linetype="dotted",color="gray40") +
#   # geom_text(data=cors, aes(label=paste("r=",Correlation)), x=.2,y=.9) +
#   scale_color_manual(values=c(cbPalette[1],cbPalette[5])) +
#   labs(shape="Region",
#        color="Window",
#        x="Proportion of selections (Exp. 1)",
#        y="Proportion of looks (S. & B., 2020)") +
#   facet_grid(size~determiner, scales = "free") +
#   theme(legend.direction = "horizontal", legend.box = "vertical") +
#   theme(legend.position="top",
#         legend.margin=margin(0,0,0,0),
#         legend.box.margin=margin(-10,-10,-10,-10),legend.spacing.y = unit(0.001, 'cm'))#,legend.box.spacing = unit(0.01, 'cm'),) 
# 
# allCorrelationBoy
# 
# allCorrelationGirl <- toplot %>%
#   filter(Region == "target", window == "determiner+name", gender == "girl") %>%
#   ggplot(aes(x=prop_selections, y=prop_looks, group=gender)) +
#   geom_point(size=2,aes(color=gender),alpha=.7) +
#   geom_smooth(method='lm',size=1,color="grey26") +
#   geom_abline(slope=1,linetype="dotted",color="gray40") +
#   # geom_text(data=cors, aes(label=paste("r=",Correlation)), x=.2,y=.9) +
#   scale_color_manual(values=c(cbPalette[5],cbPalette[1])) +
#   labs(shape="Region",
#        color="Window",
#        x="Proportion of selections (Exp. 1)",
#        y="Proportion of looks (S. & B., 2020)") +
#   facet_grid(size~determiner, scales = "free") +
#   theme(legend.direction = "horizontal", legend.box = "vertical") +
#   theme(legend.position="top",
#         legend.margin=margin(0,0,0,0),
#         legend.box.margin=margin(-10,-10,-10,-10),legend.spacing.y = unit(0.001, 'cm'))#,legend.box.spacing = unit(0.01, 'cm'),) 
# 
# allCorrelationGirl


toplotCorrelations <- toplot


allCorrelation <- toplotCorrelations %>%
  filter(Region == "target", window == "determiner+name") %>%
  ggplot(aes(x=prop_selections, y=prop_looks, color=gender)) +
  geom_point(size=2,alpha=.7) +
  geom_smooth(method='lm',size=1, se=F) +
  geom_abline(slope=1,linetype="dotted",color="gray40") +
  # geom_text(data=cors, aes(label=paste("r=",Correlation)), x=.2,y=.9) +
  scale_color_manual(values=c("#F5C4D7","#BD4C79")) +
  labs(shape="Region",
       color="Gender",
       x="Clicks on object (Degen et al. 2021)",
       y="Looks to object (Sun & Breheny 2020)") +
  facet_grid(size~determiner, scales = "free") 

allCorrelation

ggsave("../graphs/stefan/allCorrelation.pdf",width=6,height=4)



allCorrelation <- toplotCorrelations %>%
  filter(window == "determiner+name") %>%
  ggplot(aes(x=prop_selections, y=prop_looks, color=gender)) +
  geom_point(size=2,alpha=.7) +
  geom_smooth(method='lm',size=1, se=F) +
  geom_abline(slope=1,linetype="dotted",color="gray40") +
  # geom_text(data=cors, aes(label=paste("r=",Correlation)), x=.2,y=.9) +
  scale_color_manual(values=c("#F5C4D7","#BD4C79")) +
  labs(shape="Region",
       color="Gender",
       x="Clicks on object (Degen et al. 2021)",
       y="Looks to object (Sun & Breheny 2020)") +
  facet_grid(size~determiner, scales = "free") 

allCorrelation

ggsave("../graphs/stefan/allCorrelation.pdf",width=6,height=4)


correlationOne <- toplot %>%
  filter(Region == "target", window == "determiner+name", determiner == "some", size == "big", gender == "boy") %>%
  ggplot(aes(x=prop_selections, y=prop_looks, color=gender)) +
  geom_point(size=2,alpha=.7) +
  geom_smooth(method='lm',size=1, se=F) +
  geom_abline(slope=1,linetype="dotted",color="gray40") +
  # geom_text(data=cors, aes(label=paste("r=",Correlation)), x=.2,y=.9) +
  scale_color_manual(values=c("#F5C4D7","#BD4C79")) +
  labs(shape="Region",
       color="Gender",
       x="Clicks on target (Explicit Belief)",
       y="Looks to target ") +
  facet_grid(size~determiner, scales = "free") +
  coord_cartesian(xlim = c(0.40, 0.85))

correlationOne

ggsave("../graphs/stefan/allCorrelation.pdf",width=6,height=4)




toplot$cprop_selections = toplot$prop_selections - mean(toplot$prop_selections)


# model summary: 
# - big main effect of explicit beliefs
# - effect of explicit beliefs doesn't vary by window or region, except: beliefs have much smaller explanatory power for distractor looks
# NOT IMPORTANT: - overall fewer looks to distractor in all windows except baseline (interactions with window); overall fewer looks to competitor in noun window
contrasts(toplot$window)
contrasts(toplot$window) = cbind("det.to.baseline"=c(1,0,0,0),"det.to.gender"=c(0,1,0,0),"det.to.noun"=c(0,0,0,1))

# model for only targets (because full model violates assumption of independence of samples)
targ = toplot %>% 
  filter(Region == "target") %>% 
  mutate(cprop_selections = prop_selections - mean(prop_selections))



# submodels for each window
d_baseline = toplot %>% 
  filter(window == "baseline") %>% 
  mutate(cprop_selections = prop_selections - mean(prop_selections))


d_gender = toplot %>% 
  filter(window == "gender") %>% 
  mutate(cprop_selections = prop_selections - mean(prop_selections))

d_det = toplot %>% 
  filter(window == "determiner+name") %>% 
  mutate(cprop_selections = prop_selections - mean(prop_selections))

d_noun = toplot %>% 
  filter(window == "noun") %>% 
  mutate(cprop_selections = prop_selections - mean(prop_selections))

# PLOT PROPORTIONS OF LOOKS

# proportion of looks to target, competitor, and residue
gazer =  g %>%
  filter(TrackLoss=="FALSE" & time < 300) %>%
  select(Prime,condition,determiner,size,targetlook,competitorlook,residuelook,whichword,item,time, target.figure) %>%
  mutate(distractorlook=ifelse(targetlook=="1",0,ifelse(competitorlook=="1",0,ifelse(residuelook=="1",0,1)))) %>%
  # mutate(window=as.character(whichword)) %>%
  # mutate(window = ifelse(whichword =="determiner","determiner+name", ifelse(whichword=="name","determiner+name",ifelse(whichword=="end","noun",window)))) %>%
  filter(targetlook == 1 | competitorlook == 1 | residuelook== 1) %>% #CHANGE
  #filter(targetlook == 1 | competitorlook == 1) %>% #CHANGE
  group_by(time,condition,size, target.figure) %>%
  summarize(Mean_target_look=mean(targetlook),Mean_competitor_look=mean(competitorlook),Mean_residue_look=mean(residuelook),target_ci_low=ci.low(targetlook),target_ci_high=ci.high(targetlook),competitor_ci_low=ci.low(competitorlook),competitor_ci_high=ci.high(competitorlook),residue_ci_low=ci.low(residuelook),residue_ci_high=ci.high(residuelook)) %>%
  ungroup() %>%
  mutate(YMin_target=Mean_target_look-target_ci_low,YMax_target=Mean_target_look+target_ci_high,YMin_competitor=Mean_competitor_look-competitor_ci_low,YMax_competitor=Mean_competitor_look+competitor_ci_high,YMin_residue=Mean_residue_look-residue_ci_low,YMax_residue=Mean_residue_look+residue_ci_high)

# prepare data for plotting
long_props = gazer %>% 
  select(condition,size,time,Mean_target_look,Mean_residue_look,Mean_competitor_look, target.figure) %>%  #,other_prop) %>% 
  pivot_longer(cols = Mean_target_look:Mean_competitor_look,names_to=c("region"),values_to=c("proportion")) %>% 
  separate(region,c(NA,"region",NA))

long_ymin = gazer %>% 
  select(condition,size,time,YMin_target,YMin_residue,YMin_competitor, target.figure) %>%  #,other_prop) %>% 
  pivot_longer(cols = YMin_target:YMin_competitor,names_to=c("region"),values_to=c("ymin")) %>% 
  separate(region,c(NA,"region"))

long_ymax = gazer %>% 
  select(condition,size,time,YMax_target,YMax_residue,YMax_competitor, target.figure) %>%  #,other_prop) %>% 
  pivot_longer(cols = YMax_target:YMax_competitor,names_to=c("region"),values_to=c("ymax")) %>% 
  separate(region,c(NA,"region"))

toplot = long_props %>%
  left_join(long_ymin,by=c("condition","size","time","region")) %>%
  left_join(long_ymax,by=c("condition","size","time","region")) %>%
  mutate(region = fct_relevel(region,"target","competitor"),determiner = fct_relevel(as.factor(condition),"all","some"))

offset = 71
windowsize = 15.9 #ms
onsets = g %>% 
  summarize(gender=mean(gender_onset)*windowsize-1000,determiner=mean(determiner_onset)*windowsize-1000,name=mean(name_onset)*windowsize-1000,noun=mean(noun_onset)*windowsize-1000)

windows = tibble(window=c("baseline","gender","determiner","name","noun"),x=c(24+offset,70+offset,118+offset,159+offset,188+offset)*windowsize-1000)
vlinesize=.5

toplot$ttime = (toplot$time*windowsize)-1000

# only target and residue looks
ttoplot = toplot %>% 
  filter(region == "target" | region == "residue") %>% 
  droplevels()

# only target looks
ttoplot = toplot %>% 
  filter(region == "target") %>% 
  droplevels()

# Stefan 2022 graph

windows2 <- windows %>%
  filter(window != "name")
singleProportionLooks <- toplot %>%
  filter(target.figure == "boy" & condition == "some" & size == "big" & region != "residue") %>%
  ggplot(aes(x=ttime, y=proportion)) +
  geom_line(size=1, aes(color=region)) +
  geom_ribbon(aes(ymin=ymin,ymax=ymax,fill=region,group=interaction(region,size)),alpha=.3) +
  scale_fill_manual(values=c("#227100","#BBEA73",cbPalette[3])) +
  scale_color_manual(values=c("#227100","#BBEA73",cbPalette[3])) +
  geom_vline(aes(xintercept=onsets$gender),size=vlinesize) +
  geom_vline(aes(xintercept=onsets$determiner),size=vlinesize) +
  geom_vline(aes(xintercept=onsets$noun),size=vlinesize) +
  # geom_text(data=windows2,aes(label=window,x=x),y=.9,size=5) +
  xlab("Time in ms relative to audio onset") +
  ylab("Proportion of looks") +  
  scale_x_continuous(breaks=seq(0,4000,by=400),minor_breaks = seq(200,3800,by=400)) 

singleProportionLooks

ggsave("../graphs/stefan/eyetracking.pdf",plot=singleProportionLooks, width=6,height=4)





# proportion of looks to target and competitor
gaze =  g %>%
  filter(TrackLoss=="FALSE" & time < 300) %>%
  select(Prime,condition,determiner,size,targetlook,competitorlook,residuelook,whichword,item,time) %>%
  mutate(distractorlook=ifelse(targetlook=="1",0,ifelse(competitorlook=="1",0,ifelse(residuelook=="1",0,1)))) %>%
  # mutate(window=as.character(whichword)) %>%
  # mutate(window = ifelse(whichword =="determiner","determiner+name", ifelse(whichword=="name","determiner+name",ifelse(whichword=="end","noun",window)))) %>%
  filter(targetlook == 1 | competitorlook == 1) %>% #CHANGE
  #filter(targetlook == 1 | competitorlook == 1) %>% #CHANGE
  group_by(time,condition,size) %>%
  summarize(Mean_target_look=mean(targetlook),Mean_competitor_look=mean(competitorlook),target_ci_low=ci.low(targetlook),target_ci_high=ci.high(targetlook),competitor_ci_low=ci.low(competitorlook),competitor_ci_high=ci.high(competitorlook)) %>%
  ungroup() %>%
  mutate(YMin_target=Mean_target_look-target_ci_low,YMax_target=Mean_target_look+target_ci_high,YMin_competitor=Mean_competitor_look-competitor_ci_low,YMax_competitor=Mean_competitor_look+competitor_ci_high)

# prepare data for plotting
long_props = gaze %>% 
  select(condition,size,time,Mean_target_look,Mean_competitor_look) %>%  #,other_prop) %>% 
  pivot_longer(cols = Mean_target_look:Mean_competitor_look,names_to=c("region"),values_to=c("proportion")) %>% 
  separate(region,c(NA,"region",NA))

long_ymin = gaze %>% 
  select(condition,size,time,YMin_target,YMin_competitor) %>%  #,other_prop) %>% 
  pivot_longer(cols = YMin_target:YMin_competitor,names_to=c("region"),values_to=c("ymin")) %>% 
  separate(region,c(NA,"region"))

long_ymax = gaze %>% 
  select(condition,size,time,YMax_target,YMax_competitor) %>%  #,other_prop) %>% 
  pivot_longer(cols = YMax_target:YMax_competitor,names_to=c("region"),values_to=c("ymax")) %>% 
  separate(region,c(NA,"region"))

toplot = long_props %>%
  left_join(long_ymin,by=c("condition","size","time","region")) %>%
  left_join(long_ymax,by=c("condition","size","time","region")) %>%
  mutate(region = fct_relevel(region,"target","competitor"),determiner = fct_relevel(as.factor(condition),"all","some"))

# dodge=position_dodge(.9)
offset = 71
windowsize = 15.9 #ms
onsets = g %>% 
  summarize(gender=mean(gender_onset)*windowsize-1000,determiner=mean(determiner_onset)*windowsize-1000,name=mean(name_onset)*windowsize-1000,noun=mean(noun_onset)*windowsize-1000)

windows = tibble(window=c("baseline","gender","determiner","name","noun"),x=c(24+offset,70+offset,118+offset,159+offset,188+offset)*windowsize-1000)
vlinesize=.5

toplot$ttime = (toplot$time*windowsize)-1000
# only target looks
ttoplot = toplot %>% 
  filter(region == "target") %>% 
  droplevels()
# MODELS
tomodel = g %>%
  select(Prime,Subject,item,condition,determiner,size,time,targetlook,competitorlook,whichword) %>%
  filter(targetlook == 1 | competitorlook == 1) 
  
determiner_window = tomodel %>%
  filter(whichword=="determiner") %>%
  mutate(targetlook=as.factor(targetlook),size=as.factor(size),time=as.factor(time)) %>%
  mutate(csize=as.numeric(size)-mean(as.numeric(size))) %>%
  mutate(ctime=as.numeric(time)-mean(as.numeric(time)))

name_window = tomodel %>%
  filter(whichword=="name") %>%
  mutate(targetlook=as.factor(targetlook),size=as.factor(size),time=as.factor(time)) %>%
  mutate(csize=as.numeric(size)-mean(as.numeric(size))) %>%
  mutate(ctime=as.numeric(time)-mean(as.numeric(time)))

m.determiner = glmer(targetlook ~ determiner*csize*ctime + (determiner*csize*ctime|Subject) + (1+determiner*csize*ctime|item), family="binomial",data=determiner_window)
summary(m.determiner)

m.name = glmer(targetlook ~ determiner*csize*ctime + (determiner*csize*ctime|Subject) + (1+determiner*csize*ctime|item), family="binomial",data=name_window)
summary(m.name)

