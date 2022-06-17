library(tidyverse)
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

df <- read.csv("../data/Batch_2682432_batch_results_freeprod_preprocessed.csv")

# remove null reponses due to a technical problem where pressing enter immediately 
# ends the task 

df <- df %>% 
  filter(Answer.freeGenCrit != "{}") 

df_freeprod <- 
  df %>%
  group_by(Input.List, SceneID, Condition, refType, Answer.freeGenCrit) %>%
  count() %>%
  group_by(Input.List, SceneID, Condition, refType) %>%
  mutate(prodprob = n / sum(n))
  
df_freeprod_ndata <- 
  df %>% 
  group_by(Input.List, SceneID, Condition, refType) %>%
  count() 

summary(df_freeprod_ndata$n)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.00   13.00   15.00   14.95   17.00   24.00 


dd = as.data.frame(df_freeprod)
summary(dd)

dd[dd$SceneID == 39,c("Answer.freeGenCrit","n")]

dd[dd$SceneID == 4 & dd$Condition == "Contrast" & dd$refType == 1,c("Answer.freeGenCrit","n")]
dd[dd$SceneID == 4 & dd$Condition == "NoContrast" & dd$refType == 1,c("Answer.freeGenCrit","n")]

ggplot(dd, aes(x=n)) +
  geom_histogram(stat="count")
