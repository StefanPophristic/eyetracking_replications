# Incremental Study of Ryskin Et al.

This is a replication of Ryskin Et al. 2019's experiment 2 as an incremental decision task. Three versions of this incremental decision task were run. Details for each are given below.

Ryskin et al had a counterbalancing error, whereby all participants in the **pragmatic reliable** condition were assigned to list 2 and participants in the **pragmatic unreliable** condition were assigned to list 1. This was misreported in their paper as the opposite (pragmatic reliable condition with list 1, and unreliable condition with list 2).

## Run 1
This is the version of the study that was headed by Casey Butcher and was run during the Summer of 2021. The analysis for run 1 is for this experiment, and the poster in the output folder is likewise for this experiment.

This replication of Ryskin et al.'s design, as an incremental design, did not replicate the original study on two counts:
1. The study assigned both **pragmatic reliable** and **unreliable condition** participants to just list 1. This assignment method is what Ryskin et al. originally intended, but not what they did. However, participants were never assigned to list 2.

2. The training trials were miscoded, such that the all training trials for the pragmatic reliable condition appeared with a visually presented contrast set, whereas the pragmatic unreliable condition appeared with no visually presented contrast set.

## Run 2
This is the version of the study that was headed by Stefan Pophristic in the Spring of 2022. The run_2 folder is associated with data from this version of the experiment. The experiment was run such that both pragmatic conditions (**reliable** and **unreliable**) were run on both list 1 and list 2 in order to get the complete set of data.

The **training trials** for the **pragmatic unreliable** condition were miscoded, such that they were identical to the **pragmatic reliable** training trials. Since the **filler trials** are different across pragmatic conditions, this data cannot simply be used as **pragmatic reliable** data.

## Run 3
This is the version of the study that was headed by Stefan Pophristic in the Spring of 2022. The run_3 folder is associated with data from this version of the experiment. This experiment was run to make up for the error in run 2. That is, this experiment was only run for the **pragmatic unreliable** condition on both lists. All of the data in this run was run without mistakes.


## Final analysis

The data that replicates the stimuli from Ryskin et al.'s original experiment are:
- Run 2, **pragmatic reliable**, list 2 data
- Run 3, **pragmatic unreliable**, list 1 data


### Shared Folder
This folder includes all the files used to create the stimuli for the incremental version (both lists) and the files used to inspect these stimuli to make sure the discrepencies in the run_1 stimuli were resolved.

**stimuli_creation**  

The text files in this folder were taken from *https://osf.io/5geba*. These are the original stimuli files used in Ryskin et al. The .csv documents are csv versions of these text files, with the audio file column (from the original text files) removed. The *stim_script.py* is a python script that extracts the information from the csv columns, and converts it into .js stimuli files that can be used by the incremental experimental files. The stimuli files found in the run_2 experimental folders are copies of *stimuli_list1.js* and *stimuli_list2.js*.

**stimuli_tests**  

Once the new stimuli files were made for run_2, and the experiment script was edited accordingly, I ran the study (list 1) in both the pragmatic reliable (good) and unreliable (bad) conditions about 1/4 of the way through. The output data from the experiment was copied over into the python files found in this folder. The script then picks out relevant columns, and outputs csv files. These files can then be visually inspected to make sure that the stimuli look the way they should, and that the discrepencies found in run_1 were fixed.
