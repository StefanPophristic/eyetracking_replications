# production probabilities of the target adjective derived in various ways
# The free production experiment can provide estimates of the production 
#  probability of the target adjective for a particular target object
# In order to calculate the overall production probability, we need to
#  aggregate over target objects.
# This is done by either assuming a uniform prior over target objects,
# or using the empirical prior from the click data

library(tidyverse)

stimuli_all <-
  read.csv("stimuli_all.csv") %>%
  select(Input.List, SceneID, Condition,
         TargetNoun, TargetAdjective, TargetColor, AdjectiveType)


df_annotated <- 
  read.csv("freeProductionResponsesAnnotated.csv") 

df_freeprod_ndata <-
  df_annotated %>%
  group_by(Input.List, SceneID, Condition, RefType) %>%
  count() %>%
  rename(N=n)


df_modificationType <- 
  df_annotated %>%
  group_by(Input.List, SceneID, Condition, RefType, TargetModificationType) %>%
  count() %>%
  group_by(Input.List, SceneID, Condition, RefType) %>%
  mutate(prodprob = n / sum(n)) %>%
  ungroup() %>%
  #mutate(RefType = as.character(RefType)) %>%
  left_join(df_freeprod_ndata) %>%
  left_join(stimuli_all)


df_exactfirst_uniprior <- 
  df_annotated %>%
  group_by(Input.List, SceneID, Condition, ExactMatch) %>%
  count() %>%
  group_by(Input.List, SceneID, Condition) %>%
  mutate(ExactFirstWordUniPrior = n / sum(n)) %>%
  filter(ExactMatch == "0") %>%
  mutate(ExactFirstWordUniPrior = 1 - ExactFirstWordUniPrior) %>%
  ungroup() %>%
  select(-ExactMatch, -n) %>%
  left_join(stimuli_all) %>%
  mutate(ExactFirstWordUniPrior = ExactFirstWordUniPrior / 2)


df_initialmatch_uniprior <- 
  df_annotated %>%
  group_by(Input.List, SceneID, Condition, InitialMatch) %>%
  count() %>%
  group_by(Input.List, SceneID, Condition) %>%
  mutate(InitialMatchUniPrior = n / sum(n)) %>%
  filter(InitialMatch == "0") %>%
  mutate(InitialMatchUniPrior = 1 - InitialMatchUniPrior) %>%
  ungroup() %>%
  select(-InitialMatch, -n) %>%
  left_join(stimuli_all) %>%
  mutate(InitialMatchUniPrior = InitialMatchUniPrior / 2)

df_morphincl_uniprior <- 
  df_annotated %>%
  group_by(Input.List, SceneID, Condition, MorphemeIncluded) %>%
  count() %>%
  group_by(Input.List, SceneID, Condition) %>%
  mutate(MorphemeIncludedUniPrior = n / sum(n)) %>%
  filter(MorphemeIncluded == "0") %>%
  mutate(MorphemeIncludedUniPrior = 1 - MorphemeIncludedUniPrior) %>%
  ungroup() %>%
  select(-MorphemeIncluded, -n) %>%
  left_join(stimuli_all) %>%
  mutate(MorphemeIncludedUniPrior = MorphemeIncludedUniPrior / 2)

df_synfirst_uniprior <- 
  df_annotated %>%
  mutate(SynFirst = as.character(as.integer(SynFirst != "0"))) %>%
  group_by(Input.List, SceneID, Condition, SynFirst) %>%
  count() %>%
  group_by(Input.List, SceneID, Condition) %>%
  mutate(SynFirstUniPrior = n / sum(n)) %>%
  filter(SynFirst == "0") %>%
  mutate(SynFirstUniPrior = 1 - SynFirstUniPrior) %>%
  ungroup() %>%
  select(-SynFirst, -n) %>%
  left_join(stimuli_all) %>%
  mutate(SynFirstUniPrior = SynFirstUniPrior / 2)

df_synincl_uniprior <- 
  df_annotated %>%
  mutate(SynIncluded = as.character(as.integer(SynIncluded != "0"))) %>%
  group_by(Input.List, SceneID, Condition, SynIncluded) %>%
  count() %>%
  group_by(Input.List, SceneID, Condition) %>%
  mutate(SynIncludedUniPrior = n / sum(n)) %>%
  filter(SynIncluded == "0") %>%
  mutate(SynIncludedUniPrior = 1 - SynIncludedUniPrior) %>%
  ungroup() %>%
  select(-SynIncluded, -n) %>%
  left_join(stimuli_all) %>%
  mutate(SynIncludedUniPrior = SynIncludedUniPrior / 2)

# versions that take into account empirical priors
df_intp <- read.csv("clickdata.csv")

df_prior <- 
  df_intp %>%
  filter(Condition == "Contrast" | Condition == "NoContrast") %>%
  group_by(Input.List, SceneID, Condition, Answer.choicePrior) %>%
  count() %>%
  group_by(Input.List, SceneID, Condition) %>%
  mutate(ObjPrior = n / sum(n)) %>%
  filter(Answer.choicePrior == "Target" | Answer.choicePrior == "Competitor") %>%
  mutate(RefType = ifelse(Answer.choicePrior == "Target", 1, 2))

df_exactfirst_empprior <- 
  df_annotated %>%
  group_by(Input.List, SceneID, Condition, RefType, ExactMatch) %>%
  count() %>%
  group_by(Input.List, SceneID, Condition, RefType) %>%
  mutate(ExactFirstWordProd = n / sum(n)) %>%
  filter(ExactMatch == "0") %>%
  mutate(ExactFirstWordProd = 1 - ExactFirstWordProd) %>%
  ungroup() %>%
  select(-ExactMatch, -n) %>%
  left_join(df_prior) %>%
  select(-n, -RefType) %>%
  rename(RefType = Answer.choicePrior) %>%
  mutate(ExactFirstWordEmpPrior = ExactFirstWordProd * ObjPrior) %>%
  group_by(Input.List, SceneID, Condition) %>%
  summarise(ExactFirstWordEmpPrior = sum(ExactFirstWordEmpPrior)) %>%
  left_join(stimuli_all)


df_initialmatch_empprior <- 
  df_annotated %>%
  group_by(Input.List, SceneID, Condition, RefType, InitialMatch) %>%
  count() %>%
  group_by(Input.List, SceneID, Condition, RefType) %>%
  mutate(InitialMatchProd = n / sum(n)) %>%
  filter(InitialMatch == "0") %>%
  mutate(InitialMatchProd = 1 - InitialMatchProd) %>%
  ungroup() %>%
  select(-InitialMatch, -n) %>%
  left_join(df_prior) %>%
  select(-n, -RefType) %>%
  rename(RefType = Answer.choicePrior) %>%
  mutate(InitialMatchEmpPrior = InitialMatchProd * ObjPrior) %>%
  group_by(Input.List, SceneID, Condition) %>%
  summarise(InitialMatchEmpPrior = sum(InitialMatchEmpPrior)) %>%
  left_join(stimuli_all)

df_morphincl_empprior <- 
  df_annotated %>%
  group_by(Input.List, SceneID, Condition, RefType, MorphemeIncluded) %>%
  count() %>%
  group_by(Input.List, SceneID, Condition, RefType) %>%
  mutate(MorphemeIncludedProd = n / sum(n)) %>%
  filter(MorphemeIncluded == "0") %>%
  mutate(MorphemeIncludedProd = 1 - MorphemeIncludedProd) %>%
  ungroup() %>%
  select(-MorphemeIncluded, -n) %>%
  left_join(df_prior) %>%
  select(-n, -RefType) %>%
  rename(RefType = Answer.choicePrior) %>%
  mutate(MorphemeIncludedEmpPrior = MorphemeIncludedProd * ObjPrior) %>%
  group_by(Input.List, SceneID, Condition) %>%
  summarise(MorphemeIncludedEmpPrior = sum(MorphemeIncludedEmpPrior)) %>%
  left_join(stimuli_all)
  
df_synfirst_empprior <- 
  df_annotated %>%
  group_by(Input.List, SceneID, Condition, RefType, SynFirst) %>%
  count() %>%
  group_by(Input.List, SceneID, Condition, RefType) %>%
  mutate(SynFirstProd = n / sum(n)) %>%
  filter(SynFirst == "0") %>%
  mutate(SynFirstProd = 1 - SynFirstProd) %>%
  ungroup() %>%
  select(-SynFirst, -n) %>%
  left_join(df_prior) %>%
  select(-n, -RefType) %>%
  rename(RefType = Answer.choicePrior) %>%
  mutate(SynFirstEmpPrior = SynFirstProd * ObjPrior) %>%
  group_by(Input.List, SceneID, Condition) %>%
  summarise(SynFirstEmpPrior = sum(SynFirstEmpPrior)) %>%
  left_join(stimuli_all)

df_synincl_empprior <- 
  df_annotated %>%
  group_by(Input.List, SceneID, Condition, RefType, SynIncluded) %>%
  count() %>%
  group_by(Input.List, SceneID, Condition, RefType) %>%
  mutate(SynIncludedProd = n / sum(n)) %>%
  filter(SynIncluded == "0") %>%
  mutate(SynIncludedProd = 1 - SynIncludedProd) %>%
  ungroup() %>%
  select(-SynIncluded, -n) %>%
  left_join(df_prior) %>%
  select(-n, -RefType) %>%
  rename(RefType = Answer.choicePrior) %>%
  mutate(SynIncludedEmpPrior = SynIncludedProd * ObjPrior) %>%
  group_by(Input.List, SceneID, Condition) %>%
  summarise(SynIncludedEmpPrior = sum(SynIncludedEmpPrior)) %>%
  left_join(stimuli_all)


df_prodprob_summary <- 
  df_exactfirst_uniprior %>%
  select(-ExactFirstWordUniPrior) %>%
  left_join(df_exactfirst_uniprior) %>%
  left_join(df_initialmatch_uniprior) %>%
  left_join(df_morphincl_uniprior) %>%
  left_join(df_synfirst_uniprior) %>%
  left_join(df_synincl_uniprior) %>%
  left_join(df_exactfirst_empprior) %>%
  left_join(df_initialmatch_empprior) %>%
  left_join(df_morphincl_empprior) %>%
  left_join(df_synfirst_empprior) %>%
  left_join(df_synincl_empprior) 

write.csv(df_prodprob_summary, 
          "production.csv",
          row.names = FALSE)
