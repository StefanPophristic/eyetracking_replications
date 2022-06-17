library(tidyverse)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

stimuli_all <- 
  read.csv("../../../shared/stimuli_lists/stimuli_all.csv") %>% 
  select(Input.List, SceneID, Condition, 
         TargetNoun, TargetAdjective, TargetColor, AdjectiveType)



# stimuli_TlessC1 <- read.csv("../../../shared/stimuli_lists/stimuli_TlessC1.csv")
# stimuli_TlessC2 <- read.csv("../../../shared/stimuli_lists/stimuli_TlessC2.csv")
# stimuli_TmoreC1 <- read.csv("../../../shared/stimuli_lists/stimuli_TmoreC1.csv")
# stimuli_TmoreC2 <- read.csv("../../../shared/stimuli_lists/stimuli_TmoreC2.csv")

df_raw <- read.csv("../data/imprecision_freeprod_oct2017_120participants_raw.csv")


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
  mutate(Description = sub("click on th  ", "", Description)) %>% 
  mutate(Description = sub("[.]", "", Description)) %>%  # one person typing in the whole sentence
  mutate(Description = sub("^the ", "", Description)) %>%  # getting rid of the initial "the"
  mutate(Description = sub("^click the ", "", Description)) %>%  # getting rid of the initial "click the"
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

write.csv(df_descriptions, 
          "../data/imprecision_freeprod_oct2017_120participants_117native_long.csv", 
          row.names = FALSE)


df_annotated <- 
  read.csv("../data/imprecision_freeprod_oct2017_120participants_117native_long_annotated.csv") 


# write.csv(df_annotated %>%
#             mutate(ExactMatch = as.integer(as.character(TargetAdjective) == as.character(FirstWord) )) %>%
#             mutate(InitialMatch = as.integer(TargetModificationType == "1" | TargetModificationType == "N" | 
#                                    TargetModificationType == "C" |TargetModificationType == "S" )) %>%
#             mutate(MorphemeIncluded = as.integer(TargetModificationType == "1" | TargetModificationType == "N" | 
#                                   TargetModificationType == "C" |TargetModificationType == "S" | 
#                                     TargetModificationType == "M" | TargetModificationType == "MS"  )) 
#           , 
#           "imprecision_freeprod_oct2017_120participants_117native_long_annotated.csv", 
#           row.names = FALSE)

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


df_intp <- read.csv("../InterpretationTlessC_01262017/Jan-26-2017-Batch_2666254_batch_results_intp_preprocessed.csv")

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
          "imprecision_freeprod_oct2017_120participants_117native_prodprobs.csv",
          row.names = FALSE)
