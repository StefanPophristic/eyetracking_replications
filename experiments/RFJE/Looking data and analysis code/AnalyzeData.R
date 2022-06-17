library(tidyverse)
library(boot)

# In AvgLooks.csv, each row contains the average proportion of target fixations in each condition for a different participant (109 rows/participants total).
# Due to power issues, two Wolof participants only completed the 2-Competitors condition.
PropLooks <- read_csv("AvgLooks.csv")
# Each proportion was calculated as follows:
# For each combination of language (Catalan, Hindi, Hungarian, Wolof), condition (0-C, 1-C, 2-C), and adjective (material, color, scalar)
# we calculate the average utterance length (thus averaging across all items in each group).
# We next average these results across adjectives, such that we have an average utterance length for each combination of condition and language.
# We then take the average utterance length across conditions, such that we have a single average per language.

# Having an average utterance length per language, we next applied that cutoff to all trials (using each language's cutoff point)
# and we removed all trials where participants did not select the correct target.
# Finally, we computed each participant's proportion of fixations on the target for each combination of condition and language.

# 1: Test prediction of post-nominal advantage ----------------

#1.	When you have a property competitor (blue triangle, when there are two blue shapes).
#Pre-nominal languages give you temporary ambiguity. Post-nominal languages allow you to immediately extract meaning.
  #a.	Pre-nominal: 1 competitor is slower than 0 competitors.
    #i.	Lower target fixations (green dots in Fig 1a).
  #b.	Post-nominal: no difference with baseline.
    #i.	Roughly equal target fixations between 1 competitor and baseline (red dots on Fig 1a).

# 1.1: Test predictions 1a and 1b -------------

set.seed(283)
PropLooks %>% 
  mutate(D = `No competitors` - `1 competitor`) %>% filter(!is.na(D)) %>%
  group_by(Language) %>% summarise(Prop=mean(D)*100,Lower=LowerCI(D)*100,Top=TopCI(D)*100)

#Language     Prop Lower   Top
#Catalan    0.0904 -1.84  1.98
#Hindi      4.98    2.13  8.05
#Hungarian  8.22    6.27 10.1 
#Wolof     -0.311  -2.61  1.90

# 1.2: Check subject level --------------
set.seed(283)
Proportions <- PropLooks %>%
  filter(!is.na(`1 competitor`)) %>%
  mutate(D=`No competitors`-`1 competitor`) %>%
  mutate(Wins=(D>0)) %>%
  group_by(Language) %>% summarise(Total=n(),NC_Advantage=sum(Wins),NC_Disadvantage=Total-NC_Advantage)
# Now get CIs for each case:
GetCI <- function(n,t){
  data <- c(rep(1,n),rep(0,t-n))
  samples <- boot::boot(data,function(x,id){return(mean(x[id]))},R=10000)
  ci = tryCatch({
    ci <- boot.ci(samples,type="basic")$basic[4:5]
    ci <- c(max(0,ci[1]),min(1,ci[2]))
  }, error = function(e) {
    ci <- c(NA,NA)
  })
  return(ci)
}
set.seed(4927)
CIs <- sapply(1:4,function(x){return(GetCI(Proportions[x,]$NC_Advantage,Proportions[x,]$Total))})

Proportions
#Language  Total NC_Advantage NC_Disadvantage
#1 Catalan      25           11              14
#2 Hindi        27           22               5
#3 Hungarian    29           28               1
#4 Wolof        26           11              15

CIs
# Catalan, Hindi, Hungarian, Wolof
# 0.24 0.6666667 0.9310345 0.2307692
# 0.64 0.9629630 1.0000000 0.6153846

# 2: Test prediction of pre-nominal advantage ----------------

#2.	When you have a property and a kind competitor
#(blue triangle, when you have two blue shapes and two triangles).
#Pre-nominal languages allow you to anticipate word and extract meaning. Post-nominal languages give you temporary ambiguity.
  #a.	Pre-nominal: two competitors is faster than one competitor.
    #i.	Higher target fixations (green dots in Fig 1b).
  #b.	Post-nominal: two competitors is slower than one competitor.
    #i.	Lower target fixations (red dots in Fig 1b).

# 2.1: Test predictions 2a and 2b -----------
set.seed(283)
PropLooks %>% 
  mutate(D = `1 competitor` - `2 competitors`) %>% filter(!is.na(D)) %>%
  group_by(Language) %>% summarise(Prop=mean(D)*100,Lower=LowerCI(D)*100,Top=TopCI(D)*100)

#Language    Prop  Lower     Top
#Catalan    11.7   9.87  13.6  
#Hindi      -4.12 -6.98  -1.40 
#Hungarian  -2.22 -4.37  -0.132
#Wolof      11.1   8.51  13.7  

# 2.2: Check subject level -------------
set.seed(283)
Proportions <- PropLooks %>%
  filter(!is.na(`1 competitor`)) %>%
  mutate(D=`1 competitor`-`2 competitors`) %>%
  mutate(Wins=(D>0)) %>%
  group_by(Language) %>% summarise(Total=n(),OneC_Advantage=sum(Wins),OC_Disadvantage=Total-OneC_Advantage)

# Now get CIs for each case:
GetCI <- function(n,t){
  data <- c(rep(1,n),rep(0,t-n))
  samples <- boot::boot(data,function(x,id){return(mean(x[id]))},R=10000)
  ci = tryCatch({
    ci <- boot.ci(samples,type="basic")$basic[4:5]
    ci <- c(max(0,ci[1]),min(1,ci[2]))
  }, error = function(e) {
    ci <- c(NA,NA)
  })
  return(ci)
}
set.seed(4927)
CIs_a <- sapply(1:4,function(x){return(GetCI(Proportions[x,]$OneC_Advantage,Proportions[x,]$Total))})
CIs_b <- sapply(1:4,function(x){return(GetCI(Proportions[x,]$OC_Disadvantage,Proportions[x,]$Total))})

Proportions
#Language  Total OneC_Advantage OC_Disadvantage
#Catalan      25             25               0
#Hindi        27              7              20
#Hungarian    29              9              20
#Wolof        26             25               1

CIs_a
# Catalan one is actually 100-100 on bootstrap. 0 get return because of NA in function (Due to no variance during resampling)
# 1C Advantage: for Catalan and Wolof
# Catalan, Hindi, Hungarian, Wolof
#0 0.07407407 0.1379310 0.9230769
#1 0.40740741 0.4827586 1.0000000

CIs_b
# 2C Advantage: for Hindi and Hungarian
# Catalan, Hindi, Hungarian, Wolof
#0 0.5925926 0.5172414 0.00000000
#0 0.9259259 0.8620690 0.07692308

# Test prediction of cross-linguistic differences ----------------

# In 2C_minus_1C.csv, each row contains the proportion of target fixations in the 2-competitor condition, minus the 
# proportion of target fixations in the 1-competitor condition.

TargetAdvantages <- read_csv("2C_minus_1C.csv")

# The data was processed in the same way as AvgLooks.csv with the difference that we used
# the same cutoff for all languages, rather than a different cutoff per language
# (because this analysis requires comparing data form different languages).

Difference <- TargetAdvantages %>% group_by(Language) %>% 
  summarise(Delta=mean(Delta))

set.seed(49283)
WolofSamples <- boot(filter(TargetAdvantages,Language=="Wolof"),statistic=function(x,id){return(mean(x[id,]$Delta))},R=10000)
CatalanSamples <- boot(filter(TargetAdvantages,Language=="Catalan"),statistic=function(x,id){return(mean(x[id,]$Delta))},R=10000)
HungarianSamples <- boot(filter(TargetAdvantages,Language=="Hungarian"),statistic=function(x,id){return(mean(x[id,]$Delta))},R=10000)
HindiSamples <- boot(filter(TargetAdvantages,Language=="Hindi"),statistic=function(x,id){return(mean(x[id,]$Delta))},R=10000)

# Is there a difference between Wolof and Catalan?
# The samples are split, so to get the difference we'll
# stitch the resamples, and use the perc method because it doesn't use other statistics from the boot function
Diff <- WolofSamples
Diff$t <- WolofSamples$t-CatalanSamples$t0
boot.ci(Diff,type="perc") # Use perc method because it doesn't use other statistics.
# No: (-0.0177,  0.0327 )
# Delta = 0.0065 (obtain through: -0.0995+0.106)

# Difference between Hungarian and Hindi?
Diff <- HungarianSamples
Diff$t <- HungarianSamples$t-HindiSamples$t0
boot.ci(Diff,type="perc")
# No: (-0.0320,  0.0112)
# Delta: -0.0104 (obt through 0.0167-0.0271)

# Difference between Hungarian vs Wolof and Catalan?
Diff <- HungarianSamples
Diff$t <- HungarianSamples$t-WolofSamples$t0
boot.ci(Diff,type="perc")
# Yes: ( 0.0947,  0.1378 )
# Delta =  0.1162 (obt through 0.0167+0.0995)

Diff$t <- HungarianSamples$t-CatalanSamples$t0
boot.ci(Diff,type="perc")
# Yes: ( 0.1014,  0.1446 )   
# Difference = 0.1227 (obt through 0.0167+0.106)

# Difference between Hindi vs Wolof and Catalan?
Diff <- HindiSamples
Diff$t <- HindiSamples$t-WolofSamples$t0
boot.ci(Diff,type="perc")
# Yes: ( 0.1004,  0.1508 ))  
# Difference = 0.1266 (obt through 0.0271+0.0995)

Diff$t <- HindiSamples$t-CatalanSamples$t0
boot.ci(Diff,type="perc")
# Yes: ( 0.1072,  0.1575 )
# Difference = 0.1331 (obt through 0.0271+0.106)

# Combine everything into a single data frame.
FullData = data.frame(
  LA = c(rep(c("Catalan","Hindi","Hungarian","Wolof"),4)),
  LB = c(rep(c("Catalan","Hindi","Hungarian","Wolof"),each=4)),
  Dif = c(NA,0.1331,0.1227,0.0065,NA,NA,-0.0104,NA,NA,NA,NA,NA,NA,0.1266,0.1162,NA)*100,
  Low = c(NA,0.1072, 0.1014,-0.0177,NA,NA,-0.0320,NA,NA,NA,NA,NA,NA,0.1004,0.0947,NA)*100,
  Top = c(NA,0.1575,0.1446,0.0327,NA,NA,0.0112,NA,NA,NA,NA,NA,NA,0.1508,0.1378,NA)*100
) %>% filter(!is.na(Dif))
