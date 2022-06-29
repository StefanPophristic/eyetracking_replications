---
#
# By default, content added below the "---" mark will appear in the home page
# between the top bar and the list of recent posts.
# To change the home page layout, edit the _layouts/home.html file.
# See: https://jekyllrb.com/docs/themes/#overriding-theme-defaults
#
layout: home
tagLine: Assessing the linking hypothesis in visual world paradigm studies
---


# The Linking Hypothesis and Visual World Paradigm

The visual world paradigm (VWP) is a widely used methodology in psycholinguistics, whereby participants' eye movements are tracked as they hear linguistic input. Since it has been shown eye-movements are sensitive to linguistic input, researchers use the VWP to create a fine-grained record of participants' language-directed attention given linguistic input.


However, the *linking hypothesis*–the link between eye-movements, linguistic input, and cognitive processes–for the VWP is still poorly understood. One explicit proposal, for active referential tasks (in which a participant must identify and select a speaker's intended referent) is:

$$ p_{emirical}(r | u) \propto	p_{belief}(r = target | u) $$

That is, the proportion of looks ($$ p_{empirical} $$) to an object *r* in response to a speaker's utterance (*u*) reflects the participant's degree of belief ($$ p_{belief} $$) that *r* is the referent that the speaker intended.

However, assessment of this linking hypothesis has given mixed results (). We believe that this is because this linking function only accounts for *exploitation* and not *exploration*. That is, the linking function explains where participants may look, if they are familiar with the objects presented in the VWP and the way in which the target object is referred to. However, if the linguistic input or visually presented images are unexpected, participants may want to *explore* their visual space in order to establish 1) what a speaker may refer to the images as, and 2) why a speaker may have chosen an unexpected way to refer to the images.

For example, FIND A GOOD INTUITIVE EXAMPLE.


# The Project

In order to assess the explicit linking function, we re-implement active-refential-task eye-tracking experiments as an incremental decision study. The incremental decision study gives us a direct measure of participants' explicit belief that an object is the speaker's intended target. We can thus compare eye-movements ($$ p_{emirical} $$) with participants' explicit belief ($$ p_{belief} $$).

In order to assess the role of *exploration* due to unexpected linguistic input, we re-implement the eye-tracking study and incremental decision study as a production study. Through participant produced utterances, we can calculate the surprisal (and thus the effect) of the linguistic input used in the eye-tracking and incremental decision study.

Click through the slides below for a more explicit step-by-step explanation of the project:


# Our Studies
