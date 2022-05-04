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
typing "spon" instead of "spoon" and "male" instead of "boy".  These should be
coded as either not mistakes, or separate lexical items.

Finally run the analysis.R script
