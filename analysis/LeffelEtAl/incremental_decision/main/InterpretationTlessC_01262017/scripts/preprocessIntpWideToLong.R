library(reshape2)

stimuli_TlessC1 <- read.csv("stimuli_TlessC1.csv")
stimuli_TlessC2 <- read.csv("stimuli_TlessC2.csv")
stimuli_TmoreC1 <- read.csv("stimuli_TmoreC1.csv")
stimuli_TmoreC2 <- read.csv("stimuli_TmoreC2.csv")

stimuliColNames <- c("Condition", "TargetNoun", "TargetAdjective", "TargetColor", "AdjectiveType")


measureVariableNames_freeprodDefault <-  c("Answer.freeGenCrit")

measureVariableNames_intpDefault <-  c("Answer.choicePrior", "Answer.choiceAdj", "Answer.choiceWhole")

# measureVariableNames_freeprodDefault <-  c("Input.Condition", "Input.Referent", "Input.Noun", "Input.Adj", 
#                                            "Input.Target", "Input.Competitor", "Input.Distractor","Input.Contrast",
#                                            "Answer.freeGenCrit"
# )
# 
idVariableNames_freeprodDefault <- c("WorkerId", "Answer.wrongShape" ,"Input.List")
idVariableNames_intpDefault <- c("WorkerId", "Answer.wrongAdj", "Answer.wrongShape", "Input.List")

meltOneVariable <- function(dframe, idVariableNames, measureVariableName, numTrials){
  melt(dframe, idVariableNames, paste(measureVariableName, 1:numTrials, sep=""), value.name=measureVariableName)
}

meltOneParticipant <- function(dframe, idVariableNames, measureVariableNames, numTrials, expType = "interpretation"){
  result <- meltOneVariable(dframe, idVariableNames, measureVariableNames[1], numTrials)[,1:length(idVariableNames)]
  result$SceneID <- 1:numTrials
  
  for (i in measureVariableNames){
    result <- cbind(result, meltOneVariable(dframe, c("WorkerId","Input.List"), i, numTrials)[,4, drop=FALSE])
  }
  
  getReferentPicInLineX <- function(x, referent){
    return(unlist(result[x, c("Input.Target", "Input.Competitor", "Input.Distractor", "Input.Contrast")][referent]))
  }
  
  if ("Input.Referent" %in% measureVariableNames){
    result$ReferentImage <- sapply(1:numTrials, 
                                   function(i) getReferentPicInLineX(i, result[i, "Input.Referent"])) }
  
  if (expType == "interpretation"){
    turnOrder <- as.integer(unlist(strsplit(as.character(dframe[1, "Answer.turnOrder"]), ";")))
    result$Turn <- match(1:numTrials, turnOrder)
    result$sceneConfigs <- unlist(strsplit(as.character(dframe[1, "Answer.sceneConfigs"]), ";"))[result$Turn]
    result$timePrior <- unlist(strsplit(as.character(dframe[1, "Answer.timePrior"]), ";"))[result$Turn]
    result$timeAdj <- unlist(strsplit(as.character(dframe[1, "Answer.timeAdj"]), ";"))[result$Turn]
    result$timeWhole <- unlist(strsplit(as.character(dframe[1, "Answer.timeWhole"]), ";"))[result$Turn]
  }
  else if (expType == "freeProduction"){
    turnOrder <- as.integer(unlist(strsplit(as.character(dframe[1, "Answer.turnOrder"]), ";")))
    result$Turn <- match(1:numTrials, turnOrder)
    result$sceneConfigs <- unlist(strsplit(as.character(dframe[1, "Answer.sceneConfigs"]), ";"))[result$Turn]
    result$RT <- unlist(strsplit(as.character(dframe[1, "Answer.sceneTime"]), ";"))[result$Turn]
    result$refType <- unlist(strsplit(as.character(dframe[1, "Answer.refTypes"]), ";"))[result$Turn]
  }
  
  
  if (as.character(dframe[1,"Input.List"]) == "TlessC1"){
    result <- cbind(result, stimuli_TlessC1[1:numTrials, stimuliColNames]) 
  }
  else if (as.character(dframe[1,"Input.List"]) == "TlessC2"){
    result <- cbind(result, stimuli_TlessC2[1:numTrials, stimuliColNames]) 
  }
  else if (as.character(dframe[1,"Input.List"]) == "TmoreC1"){
    result <- cbind(result, stimuli_TmoreC1[1:numTrials, stimuliColNames]) 
  }
  else if (as.character(dframe[1,"Input.List"]) == "TmoreC2"){
    result <- cbind(result, stimuli_TmoreC2[1:numTrials, stimuliColNames]) 
  }
  
  return(result)
}

meltAllParticipants <- function(dframe, idVariableNames, measureVariableNames, numTrials, expType = "interpretation"){
  result <- meltOneParticipant(dframe[1, ], idVariableNames, measureVariableNames, numTrials, expType)
  
  numParticipants <- nrow(dframe)
  if (numTrials == 1){
    return(result)
  }

  for (i in 2:numParticipants){
    result <- rbind(result, meltOneParticipant(dframe[i, ], idVariableNames, measureVariableNames, numTrials, expType))
  }
  
  return(result)
}


preprocessFreeProduction <- function(rawPath, idVariableNames=idVariableNames_freeprodDefault, 
                                     measureVariableNames=measureVariableNames_freeprodDefault, numTrials=40){
  rawData <- read.csv(rawPath)
  processedData <- meltAllParticipants(rawData, idVariableNames, measureVariableNames, numTrials, "freeProduction")
  
  newPath <- gsub("(^.)*[.]csv", "\\1_freeprod_preprocessed.csv", rawPath)
  write.csv(processedData, newPath, row.names = FALSE)
}

preprocessInterpretation <- function(rawPath, idVariableNames=idVariableNames_intpDefault, 
                                     measureVariableNames=measureVariableNames_intpDefault, numTrials=120){
  rawData <- read.csv(rawPath)
  processedData <- meltAllParticipants(rawData, idVariableNames, measureVariableNames, numTrials)
  
  newPath <- gsub("(^.)*[.]csv", "\\1_intp_preprocessed.csv", rawPath)
  write.csv(processedData, newPath, row.names = FALSE)
}

preprocessInterpretation("Jan-26-2017-Batch_2666254_batch_results.csv")
preprocessFreeProduction("FreeProduction02102017/Batch_2682432_batch_results.csv")
