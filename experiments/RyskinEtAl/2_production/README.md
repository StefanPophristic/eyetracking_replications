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
