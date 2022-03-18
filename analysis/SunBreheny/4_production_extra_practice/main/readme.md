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

First run the preprocessResponses.py program. This program takes the raw data
from the csv file ending in *-trials.csv* and appends a column with the following
information:
1. The gender that the participant used {boy, girl, noGender}
2. Whether the correct gender was used {1,0}. 1 is assumed if no gender term was used
3. Whether the correct noun was used {1, 0}
4. Which determiner was used {a, one, two, three, four, some, all, 0}
5. Whether the correct determiner was used {1, 0, NoDet}. 0 means that the
incorrect determiner was used (e.g. using "all" in a scene where the
target image did not have all the objects). NoDet means that the participant
did not use a determiner, which is not technically incorrect

The script then outputs the same csv file, but with all this information as new columns

Please see the preprocessResponses.py program for more information, including
which assumptions the program makes as to what counts as a correct or incorrect word.

The second step is to manually go in and edit the output csv files. You must check
all of the following cases: genderUsed = "noGender", detUsed = "noDet", correctGender = 0,
correctNoun = 0, and correctDet = 0. These are the values that the program will
automatically give spelling errors and participants using unexpected words: e.g.
typing "spon" instead of "spoon" and "male" instead of "boy". These manual corrections
should be encoded as follows:

1. genderUsed: go through all cases of "noGender" and make sure that in fact no
word refering to the children was used. If a word was in fact used (e.g. "person",
"child", "woman"), replace the "noGender" with the lexical item used
2. correctGender: if you replaced "noGender" with a lexical item in the genderUsed
column, make sure that the application can be used correctly (e.g. the participant
didn't say "click on the woman" when referring to the boy)
3. detUsed: go through all the cases of "noDet". If a determiner was used, replace
"noDet" with the determiner used. For example: "single", "most", "almostall", etc.
4. correctNoun: go through all cases of "0", Make sure that in fact no noun was used.
If an unexpected word was used, replace "0" with that word (e.g. "item", "blackandwhitething", etc.).
In the case that no noun was used, if the object is still uniquely identifiable
without the noun, replace the "0" with the term "noNoun". This may arise in cases
when the participant writes "click on the boy with two", and there is only a single
boy that has two items in the display.
5. correctDet: go through cases of "0" to make sure that is not because of spelling
errors.

To help you with the coding, the section below contains a column naming guide

Finally run the analysis.R script

## Guide to variable names

The column names were mostly directly taken from Sun and Breheny 2020. They are as
follows:

**first column**: row number
**workerid**: Anonymous and unique ID of the participant
**proliferate.condition**: {List1, List2, List3} condition that the participant was in.
**ExpFiller**: {Prac, Exp, Filler} trial type. Either practice (prac), experimental (exp),
or filler (Filler)
**intro_object**: name of practice trial used in the JS program to identify which
trial to display. This is unimportant for the analysis.
**Prime**:{Girl,Boy} + {All, Some, One, Two, Three, Four} +{Amy, Michael, Susan} + {a noun}
This is an identifier for the trial. {girl, boy} refers to the target child.
 {All, Some, One, Two, Three, Four} refers to the condition (see **determiner** column).
 {Amy, Michael, Susan} this is the name of the person giving the objects to the children
 in the original study.
 {a noun} the noun refering to the target object.
 This column is unimportant for the analysis.
**Res_object**: Something to do with the intro practice items. I'm not actually sure what exactly.
**clicking_practice**: trial number for the practice trials that involve clicking
on the target object (i.e. the first four trials of any experiment)

I'll finish these on another day.
