library(tidyverse)

stimuli_all <- 
  read.csv("../stimuli_all.csv") %>% 
  select(Input.List, SceneID, Condition, 
         TargetNoun, TargetAdjective, TargetColor, AdjectiveType)



# stimuli_TlessC1 <- read.csv("../stimuli_TlessC1.csv")
# stimuli_TlessC2 <- read.csv("../stimuli_TlessC2.csv")
# stimuli_TmoreC1 <- read.csv("../stimuli_TmoreC1.csv")
# stimuli_TmoreC2 <- read.csv("../stimuli_TmoreC2.csv")

df_raw <- read.csv("imprecision_freeprod_oct2017_120participants_raw.csv")


# filter out non-native speakers (3 out of 120)
df_raw <-
  df_raw %>%
  filter(Answer.nativeEnglish == "yes")


df_refTypes <- 
  df_raw %>% 
  select(WorkerId, Answer.refTypes) %>% 
  separate(Answer.refTypes, as.character(1:40)) %>%
  gather(SceneID, RefType, -WorkerId) %>%
  arrange(WorkerId)

df_descriptions <- 
  df_raw %>% 
  select(WorkerId, Input.List, starts_with("Answer.freeGenCrit"), 
         -Answer.freeGenCrit) %>%
  gather(SceneID, Description, -WorkerId, -Input.List) %>%
  mutate(SceneID=sub("Answer[.]freeGenCrit", "", SceneID)) %>%
  left_join(df_refTypes) %>%
  mutate(SceneID=as.integer(SceneID)) %>% 
  left_join(stimuli_all) %>%
  arrange(Input.List, SceneID, RefType) %>%
  mutate(Description = tolower(Description)) %>%
  mutate(Description = sub("click on the ", "", Description)) %>% 
  mutate(Description = sub("[.]", "", Description)) %>%  # one person typing in the whole sentence
  mutate(FirstWord = sapply(strsplit(Description, " "), function(x)x[1]))


df_freeprod_ndata <- 
  df_descriptions %>% 
  group_by(Input.List, SceneID, Condition, RefType) %>%
  count() %>%
  rename(N=n)

df_freeprod <- 
  df_descriptions %>%
  group_by(Input.List, SceneID, Condition, RefType, Description) %>%
  count() %>%
  group_by(Input.List, SceneID, Condition, RefType) %>%
  mutate(prodprob = n / sum(n)) %>%
  left_join(df_freeprod_ndata) %>%
  left_join(stimuli_all)

df_firstword <- 
  df_descriptions %>%
  group_by(Input.List, SceneID, Condition, RefType, FirstWord) %>%
  count() %>%
  group_by(Input.List, SceneID, Condition, RefType) %>%
  mutate(prodprob = n / sum(n)) %>%
  left_join(df_freeprod_ndata) %>%
  left_join(stimuli_all)

df_annotated <- 
  read.csv("imprecision_freeprod_oct2017_120participants_117native_long_annotated.csv")


df_modificationType <- 
  df_annotated %>%
  group_by(Input.List, SceneID, Condition, RefType, TargetModificationType) %>%
  count() %>%
  group_by(Input.List, SceneID, Condition, RefType) %>%
  mutate(prodprob = n / sum(n)) %>%
  ungroup() %>%
  mutate(RefType = as.character(RefType)) %>%
  left_join(df_freeprod_ndata) %>%
  left_join(stimuli_all)

#write.csv(df_descriptions, "Batch_2962714_batch_results_preprocessed.csv")

write.csv(df_descriptions, 
          "imprecision_freeprod_oct2017_120participants_117native_long.csv", 
          row.names = FALSE)

