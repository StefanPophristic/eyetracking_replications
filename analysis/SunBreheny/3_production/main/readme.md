---
title: "readme"
author: "Stefan Pophristic"
output: html_document
---

## Basics

This is the analysis folder for for the Sun and Breheny 2020 Production replication
study.      

## Structure

**Data**
> csv files with raw and pre-processed data
> preprocessResponses.py: python script responsible for pre-processing data

**Graphs**
> graphs exported from the analysis R script

**Rscripts**
> 1_incremental_data folder: contains csv files of raw data and analysis scripts
from Degen, Kursat, Leigh 2021's incremental replication study of Sun and Breheny
2021 . This is used in the analysis script
> analysis.R: main analysis script
> helpers.R: helper functions for analysis.R

## Pipeline
All csv raw data files will be found in the data folder.

First run the preprocessResponses.py program. This program takes the raw data from the csv file ending in *-trials.csv* and appends a column with the following information:
1. The gender that the participant used {boy, girl, noGender}
2. Whether the correct gender was used {1,0}. 1 is assumed if no gender term was used
3. Whether the correct noun was used {1, 0}
4. Which determiner was used {a, one, two, three, four, some, all, 0}
5. Whether the correct determiner was used {1, 0, NoDet}. 0 means that the incorrect determiner was used (e.g. using "all" in a scene where the target image did not have all the objects). NoDet means that the participant did not use a determiner, which is not technically incorrect

The script then outputs the same csv file, but with all this information as new columns

Please see the preprocessResponses.py program for more information, including which assumptions the program makes as to what counts as a correct or incorrect word.

The second step is to manually go in and edit the output csv files. You must check all of the following cases: genderUsed = "noGender", detUsed = "noDet", correctGender = 0, correctNoun = 0, and correctDet = 0. These are the values that the program will automatically give spelling errors and participants using unexpected words: e.g. typing "spon" instead of "spoon" and "male" instead of "boy".  These should be coded as either not mistakes, or separate lexical items.

Finally run the analysis.R script

## Data Annotation Schema
After running the preprocessResponses.py program, you will get the file [name_of_study]_cleaned.csv. The following are the column names, what they represent, and the possible values they can take. Each row represents a single trial of a single iteration of the experiment. A lot of these columns are legacy columns from the Degen, Kursat, and Leigh 2020 and are unimportant for the production analysis. The first 4 practice trials are not found in the 3.2 simple exposure experiment.

**first column**: Number corresponding to row of data.
**workerid**: Prolific workerid. Each participant received an id number.
**proliferate.condition**: *{List1, List2, List3}* condition that each participant/trial was assigned to. Conditions are . Each participant was assigned to a condition. The lists correspond to the original stimuli lists in Sun and Breheny 2020.
**ExpFiller**: *{Prac, Exp, Filler}* Type of trial: Prac(tice) (exposure), Exp(erimental) (target), or Filler
**intro_object**: *{Fruit_Susan_Monday, Fruit_Susan_Tuesday, Stationary_Michael_Monday, Stationary_Michael_Tuesday, kitchware_Amy_Monday, kitchware_Amy_Tuesday}* Type of practice/exposure trial. The first word refers to the type of object presented (fruit, stationary, or kitchenware). The second word refers to the person distributing the objects in the story (Susan, Michael, Amy). The third word refers to the day of week that the person distributed the objects (Monday, Tuesday). The story went as follows: "This is Amy. Amy gives out kitchenwares to children every day. Here is what Amy has on Monday. Amy has plates, forks, spoons and knives. She always brings more than enough. The leftover kitchenwares are put in the middle.". This story was only found in the practice rounds and was not referenced in the rest of the experiment.
**Prime**: single string version of the target utterance in the following format: {gender of target child: "boy", "girl"} + {determiner: "all", "some", number} + {Person distributing objects from story: "Michael", "Amy", "Susan"} + {target object}
**Res_object**: Objects that are found in the middle of the screen (i.e. not belonging to any child) for the practice rounds only.
**clicking_practice_trial_number**: Trial number for the practice/exposure trials using the methodology from Degen, Kursat, Leigh 2020 (that is clicking on the target rather than typing in what participant thinks the utterance is). There were 4 such trials. The last 2 practice trials were production rather than clicking trials, therefore they did not get this trial number.
**competitor1**: *{1, 2, 3, 4}* Refers to location of competitor object (see fig. 1), which is the non-target object possessed by the child of the same gender as the target object (e.g. if the scene has a girl with bananas and a girl with oranges and target is the "girl with all the bananas", competitor 1 would be the location of the oranges).
**competitor2**: *{7, 8, 9, 10}* Refers to the location of the competitor child (see fig. 1), which is the child of the same gender as the target child.
**condition**: *{all, some, num, one, four}* Condition from original experiment. In the original experiment this determines which determiner was used in the utterance. *Num* refers to the number condition which included numbers *two* and *three*. Numbers *one* and *four* we used exclusively for filler trials.
**condition1**: Same as **condition** but for practice trials.
**correctAns1**: *AOI+{1, 2, 3, 4}* The number refers to the location of the target object (see fig. 1).
**correctAns2**: *AOI+{7, 8, 9, 10}* The number refers to the location of the target child (see fig. 1).
**determiner**: *{all, some, two, three, one, four}* Same as **condition**, but the *num* variable is expanded to *two* and *three*.
**determiner1**: same as **determiner** but for practice/exposure trials.
**displayID**: Unique ID that identifies the trial display to the corresponding chunk in the stimuli.js script which lists all trials.
**display_type**: *{44-22, 33-33}* Total number of each of four items on the screen. This can be either 44-22: there are 4 of two of the items (4x bananas, 4x apples) and 2 of the other 2 items (2x pears, 2x oranges); OR 33-33: there are 3 of all 4 items (3x bananas, 3x apples, 3x pears, 3x oranges)
**figure**: *{Susan, Michael, Amy}* for practice/exposure trials, *{Susan's, Michael's, Amy's}* for rest of trials. Refers to the person distributing objects from the story. This has a 1-1 correspondence to the type of objects in display: fruit, stationary, kitchenware respectively.
**instruction1**: instruction utterances for the practice trials. These were only used for the first 4 practice trials which were of clicking type. The last 2 practice trials were production (type in the utterance into a box), and therefore did not include these instructions.
**instruction3**: Corresponding instructions/utterance given in Degen, Kursat, Leigh 2020. These were not shown for this experiment.
**location1**: object stimulus found in location 1 (see fig. 1)
**location10**: child stimulus found in location 10 (see fig. 1)
**location2**: object stimulus found in location 2 (see fig. 1)
**location3**: object stimulus found in location 3 (see fig. 1)
**location4**: object stimulus found in location 4 (see fig. 1)
**location5**: object stimulus found in location 5 (see fig. 1)
**location6**: object stimulus found in location 6 (see fig. 1)
**location7**: child stimulus found in location 1 (see fig. 1)
**location8**: child stimulus found in location 1 (see fig. 1)
**location9**: child stimulus found in location 1 (see fig. 1)
**object1:4**: Legacy columns referring to objects in the practice trials. These remain constant for all participants.
**practiceTrialType**: *{clicking practice, production practice}* Type of practice trial, either the clicking practice (same task as Degen, Kursat, Leigh 2020) or the production practice (same as the rest of the experiment). Practice trials 1:4 will always be clicking, and the last two will always be production.
**practice_trial_number**: *{1, 2}* trial number for the production practice trials.  
**prime**: Same as **Prime**, but for practice trials.
**response**: For the first 4 practice trials responses are in the format of *['aoi{number}', 'aoi{number}', 'aoi{number}', 'aoi{number}']*. Each number refers to where the participant clicked (see fig. 1). Since there were 4 clicks per trial (as the utterance was incrementally exposed), there are three responses per trial. The rest of the responses are what the participants typed into the input box.
**response_times**: time it took for the participant to respond in milliseconds. This was only recorded for non-practice/exposure trials.
**setting**: *{fruit, stationaries, kitchenwares}* Type of objects given in a scene. For practice trials, *stationaries* is coded as *office supplies*.
**size**: *{big, small}* What is the target object set size. *Big* always refers to a child in possession of 3 objects and *small* always refers to 2 objects.
**size1**: Just like **size** but for practice trials.
**target1**: *{1, 2, 3, 4}* Location of the target object (see fig. 1).
**target2**: *{7,8, 9, 10}* Location of target child (see fig. 1).
**target_figure1**: *{boy, girl}* gender of target child for practice/exposure trials.
**traget_figure3**: *{boy, girl}* gender of target child for non-practice/exposure trials.
**target_object1**: Target object for practice/exposure trials.
**target_object3**: Target object for non-practice/exposure trials.
**trial_number**: Trial number after first practice/exposure trials.
**error**: I think this is if the experiment throws an error, but I'm not sure since this is a legacy column.

**Columns after this were added via preprocessResponses.py script and checked manually**
These columns do not annotate practice/exposure trials and therefore should be ignored for any practice/exposure trials.
**genderUsed**: *{boy, girl, noGender}* The term that the participant used to refer to the child
**correctGender**: *{1, 0}* 1 if the correct gender was used. 0 if incorrect gender term was used. 1 is assumed if no gender term was used
**correctNoun**: *{1,0, [word]}* 1 if correct noun was used. Spelling errors count as correct. If the participant used an unexpected, but still accurate word, the word itself is written into this column (e.g. for "pencil" the participant said "pen"). This will be collapsed into an "other" variable in the analysis script. 
**detUsed**: *{a, one, two, three, four, some, all, 0}* 0 indicates that no determiner was used. Practice trials are automatically given the value of 0.
**correctDet**: *{1,0}* 1 if the correct determiner was used.

![](shared/object_locations.jpg?raw=true "location of objects")
