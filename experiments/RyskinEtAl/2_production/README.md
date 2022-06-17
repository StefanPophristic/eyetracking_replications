# Ryskin et al. Production study


Unlike other experiments in this repo (Sun and Breheny experiments 1-4 and Ryskin et al 1), all condition lists are found in a single stimuli.js file, and thus all conditions are run from the same experimental script.

# Stimuli Creation
The stimuli are the exact same as in the 1_incremental study. You can find the stimuli lists in the shared_stimuli_creation folder. To these additional stimuli lists, three parameters were added: target_rotation and production_list.

Explain the logic of selecting a target.

Therefore we need to counterbalance which object will be the target. This only applies to training and test (i.e. not filler) trials, since training and test trials had distractors to which the same modifier could apply (e.g. 2 "small" objects or 2 "big" objects), whereas for the filler trials, the modifier could apply only to the given target (e.g. "the red swimsuit").

The colors below refer to the color schema in shared > stimuli_creation > list1_production.xlx

Therefore, the following groups will split up into two:
- test items with contrast set present (blue)
- test items with contrast set absent (orange)
- train items with contrast set present (green)
- test items with contrast set absent (yellow)


Within each of these groups, the first half of the items were assigned to the **normal** production_list condition (darker colors), which means that the target from ryskin et al. and 1_incremental will appear as the target in this study. The second half were assigned to the **switch** production_list condition (lighter colors), which means that the target and filler/contrast item were switched.

The **R1** target_rotation had the first half of items in each group assigned to the **normal** condition and the second half to the **switch** condition. The **R2** target_rotation condition had the second half of items in each group assigned to the **normal** condition and the first half to the **switch** condition.


HOW DOES THIS INTERACT WITH PRAGCONTEXT CONDITION FOR TRAINING TRIALS?????


# Data output
The pilot and main experiments output data with the following variables (found in the analysis folder under the name XXX). Each variable corresponds to a column in the output file:

**stim_trial_num**: A number referring to a the "Trial" column in the stimuli lists. This is a legacy column from the original Ryskin data. This variable can be used to identify which stimuli were used in the trial. This is NOT the experiment trail number.
**trialType**: {filler, train, test}. This is specified in the stimuli lists as well as the output. This encodes whether the particular trial was a filler, training, or test trial.
**trialID**: This is specified in the stimuli lists as well as the output. This serves as a unique ID for each trial. It is in the format of TRIALTYPE+Number.
**cond**: This is specified in teh stimuli lists as well as the output. For test and training trials specifies contrast condition {contrast, no_contrast}. For filler trials, specifies filler trial type {semantic_contrast, semantic_control, contrast_control}.
**target_pic**: target picture name
**target_contrast_good**: contrast image for pragmatic good condition
**target_contrast_bad**: contrast image for pragmatic bad condition
**big_filler**: filler image with a "big" object
**small_filler**: filler image with a "small" object

*the competitor refers to either big_filler or small_filler depending on whether the target_pic was NOUN_big.jpg or NOUN_small.jpg respectively*

**loc_target_pic**: location of target pic
**loc_contrast**: location of contrast pic (either target_contrast_good or target_contrast_bad depending on which one was displayed in the trial)
**loc_big_filler**: location of filler pic
**loc_small_filler**: location of filler pic

*Location 1 = top left*
*Location 2 = top right*
*Location 3 = bottom left*
*Location 4 = bottom right*

**trial_number**: trial number of the experiment
**pragContext**: {good, bad}; pragmatic condition. This should be the same for all trials of a single participant.
**noun**:  This is specified in the stimuli lists as well as the output. The noun of the target image.
**modifier**: This is specified in the stimuli lists as well as the output. The modifier (adjective) of the target image.
**list**: {l1, l2} This is specified in the stimuli lists as well as the output. Which of the original 2 lists were these trials taken from. l1 = list 1; l2 = list 2
**rotationList**: {R1, R2} This is specified in the stimuli lists as well as the output. Which rotation (the counterbalancing system we are using to vary whether the target image is the original Ryskin et al. target image, or the competitor with the same modifier) were the stimuli taken from. For more details, see above.
**productionList**: {normal, switch} This is specified in the stimuli lists as well as the output. Normal means that the target displayed was the original target in the same trial in the Ryskin et al study. Switch means that the target displayed in the production study is the competitor that shares a modifier with the target in the original Ryskin et al. study.
**response**: participant response.
