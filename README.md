# Intro
Welcome to the ALPS Lab repo for testing the visual world paradigm.

You can find the experimental files, raw and pre-processed data, and analysis files for the following experiments in this repo:

1. Degen, Kursat, Leigh 2021
    - Exp. 1
      - [experimental files](experiments/SunBreheny/1_incremental/main/)
      - [analysis + data](analysis/SunBreheny/1_incremental/main)
    - Exp. 2
      - [experimental files](experiments/SunBreheny/2_webgazer/main/)
      - [analysis & data](analysis/SunBreheny/2_webgazer/main/)
2. Degen & Pophristic 2022
    - [Exp. 1 experimental files](experiments/SunBreheny/3_production/3.1/main/)
    - [Exp. 2 experimental files](experiments/SunBreheny/3_production/3.2/main/)
    - [All analysis & data](analysis/SunBreheny/3_production/main/)  
3. Qing, Lassiter, Degen 2018
    - All relevant files will be added shortly.   

The goal of this project is to replicate visual world paradigm (eye-tracking) studies as incremental decision tasks and production tasks in order to test the role of surprisal in implicit linking hypotheses in eye-tracking tasks.

The original studies along with our studies can be found here:
- Eye-tracking ([Ryskin et al. 2019](https://onlinelibrary.wiley.com/doi/full/10.1111/cogs.12769))
  - Incremental Decision Task — work in progress
  - Production Task - work in progress
- Eye-tracking ([Sun and Breheny 2020](https://www.tandfonline.com/doi/full/10.1080/23273798.2019.1678759))
  - Web based eye-tracking replication ([Degen, Kursat, & Leigh 2021](https://alpslab.stanford.edu//papers/2021_DegenKursatLeigh.pdf))
  - Incremental Decision Task ([Degen, Kursat, & Leigh 2021](https://alpslab.stanford.edu//papers/2021_DegenKursatLeigh.pdf))
  - Production Task ([Degen & Pophristic 2022](https://alpslab.stanford.edu//papers/2022_DegenPophristic.pdf))
- Eye-tracking ([Leffel et al. 2016](http://journals.linguisticsociety.org/proceedings/index.php/SALT/article/view/26.836))
  -Incremental Decision Task ([Qing, Lassiter, Degen 2018](https://alpslab.stanford.edu//papers/2018_QingLassiterDegen.pdf))
  -Production Task ([Qing, Lassiter, Degen 2018](https://alpslab.stanford.edu//papers/2018_QingLassiterDegen.pdf))

# Repo organization

This repo is organized as follows:

- [**analysis**](analysis/): folder containing all raw data, all processed/cleaned data, analysis scripts, and graphs
- [**experiments**](experiments/): folder containing all experimental scripts. Opening the *index.html* file of any given experiment ([for example](experiments/SunBreheny/3_production/3.1/main/list1/public/index.html)) should allow you to run the experiment locally. Stimuli lists of each experiment can be found in the subfolder *js>stimuli.js* ([for example](experiments/SunBreheny/3_production/3.1/main/list1/public/js/stimuli.js)).
- [**materials**](materials/): This folder contains copies of the original studies if they are not available elsewhere.
- [**output**](output/): This folder contains finished papers, conference submissions, and presentations pertaining to this project.
- [**readings**](readings/): this folder contains copies of the original papers for easy access.

Studies are labelled after the original study they are based off of: SunBreheny and RyskinEtAl. The individual experiments can be found in folders of the same name.

The Sun and Breheny 2020 production study (3_production) has two sub-experiments. The first one (3.1) has 2 practice trials whereas the second one (3.2) also has 4 exposure trials.


# Background

For this project, we are explicitly testing the implicit linking hypothesis found in many visual world paradigm studies. The linking hypothesis often used is that looks to an object represent belief that the object is the target of the referential utterance.

As a first step, we re-implement eye-tracking studies as incremental deicision tasks. Therefore, instead of utterances being presented auditorily and measuring looks to an object, the utterance is presented in written format incrementally. Participants then read part of the sentence and click on the object they believe the utterance refers to. We take these clicks to an object as a measure of explicit belief that the object is the intended target. We can then compare looks to the object with explicit beliefs that the object is the intended target.

We then implement a production task, using the same visual stimuli. However, unlike the incremental decision and eye-tracking tasks, in this task, the target is explicitly marked by a red box. Participants are then given an input box where they communicate to another (fictitious) participant what the target utterance is. We compute surprisal by comparing the instances where a word (adjective/quantifier or noun) occurred in the original experiment stimuli with how often (i.e. the probability) that that word is naturally produced by participants in this production task.

In order to assess the linking hypothesis, we follow the following general pipeline. After running the incremental task, we calculate the correlation between looks to an object (from the original experiment) and clicks to the object (from the incremental task) given a specific utterance/stimulus. The higher the correlation, the more support we have for the linking hypothesis. After running the production task, to calculate surprisal, we take the negative log of the probability of a word being produced in the production task given a stimulus. In order to assess the role of surprisal, we calculate the correlation between the surprisal and the previously computed correlation between looks and selections. The higher this correlation, the more support we have for surprisal modulating how strongly the linking hypothesis holds.

The details of the individual studies we conduct, the original data set we base these studies off of, and the organization of the repo, is given below.

# The studies

## 1. Ryskin et al. 2019 replications:
We conduct our studies based off of Ryskin et al. 2019 experiment 2. The original paper can be found [here](https://onlinelibrary.wiley.com/doi/full/10.1111/cogs.12769). Original data and stimuli from the study can be found [here](https://osf.io/5geba/).  Their experiment aims to test the type of evidence necessary (bottom up versus top down information) for participants to modulate interpretation of contrastive inferences.

The stimuli:
Their stimuli consisted of intermixed filler, exposure, and test trials. Since we are interested in the role of surprisal of adjectives and quantifiers, we only look at their trials that had adjectives in the auditory stimulus. This includes all test trials, half the exposure trials, and 1/3 of the of the filler trials (semantic_contrast fillers). All trials consisted of 4 items in the display. Examples of test and exposure trials are given below (from Ryskin et al. 2019 fig. 4):

![](experiments/RyskinEtAl/1_incremental/shared/example_ryskin_trails.png?raw=true "Ryskin Trial")

The test trails occurred only with the adjectives "big" and "small" and fell into either the contrast set present or absent condition (e.g. if the target is the "big pickle" in the contrast set present condition there would be a big and small pickle whereas in the contrast set absent condition there would only be the big pickle). These trials were identical across pragmatic conditions. The exposure trials that we looked at (only half of all exposure trials) were identical to the test trials. Given the way their trials were designed paired with the fact that we are looking only at trials with adjectives, exposure trials in the contrast set present condition always occurred in the pragmatic good/reliable condition whereas exposure trials in the contrast set absent condition only occurred in the pragmatic bad/unreliable condition. The filler trials consisted of various adjectives ranging from colors and materials to words like "zebra" and "barn". The contrast set presence was likewise determined by pragmatic condition.

As of now, we are still working on the analysis of the incremental study.

**1_incremental**: This is the incremental version of this study. The study was ran twice (marked as *run_1* and *run_2*). *run_1* was ran during the Summer of 2021 by Casey Butcher. Casey presented the results as a poster for an undergrad research symposium, a copy of which can be found in the output folder. *run_2* was ran due to the discovery of an error in run_1. That is, only list 1 items were run (leading to missing data) and in the exposure trails contrast set condition was conflated with the pragmatic condition, as shown in the image below (compared to the image above).

![](experiments/RyskinEtAl/1_incremental/shared/run_1_ryskin_trials.png?raw=true "Ryskin Trial run 1")

**2_production**: This study is ready to be run. However, we are waiting to finish the analysis of 1_incremental before we run this study.


## 2. Sun and Breheny 2020

We conduct our studies using the data and stimuli from Sun and Breheny 2020 experiment 3. Their study can be found [here](https://www.tandfonline.com/doi/full/10.1080/23273798.2019.1678759). Their study was designed to investigate the processing speed of quantifiers. Their experiment 3 presented 4 children (2 identical boys and 2 identical girls) with various objects (kitchenware, school supplies, or fruit) along with several objects in the middle of the screen.

![](experiments/SunBreheny/3_production/3.1_production_simple_practice/main/list1/public/images/instruction_image_clicking.png?raw=true "SunAndBreheny Stimuli")

The quantifiers used to refer to objects in the scene were: "all", "some", "two" or "three". Certain quantifiers (i.e. "all" and "some") required additional verification to the objects in the middle of the screen (the 'residue set') to ensure that these quantifiers applied truthfully.

**1_incremental**: This is the incremental version of this study. It was published as Degen, Kursat, and Leigh 2021. You can find the published version in the output folder or [here](https://alpslab.stanford.edu//papers/2021_DegenKursatLeigh.pdf). They found that the selection data in the Quantifier window was highly correlated with looking data (all r > .78), thus supporting the linking hypothesis. You can find a copy of the analysis and the experiment in this repo.

**2_webgazer**: This is a replication of the original Sun and Breheny 2020 eye-tracking study, but as an online eye-tracking study. You can find the published report on this study in the output folder or [here](https://alpslab.stanford.edu//papers/2021_DegenKursatLeigh.pdf). You can find a copy of the analysis and the experiment in this repo.

**3_production**: This is the production version of this study. It was published as Degen & Pophristic 2022. You can find the published version in the output folder (online link forthcoming). We ran 2 versions of this production experiment: the version labeled 3.1 had no exposure whereas the version labelled 3.2 had 4 exposure trials using all four quantifiers from the original Sun and Breheny experiment ("all", "some", "two", and "three"). You can find near duplicates of both the experimental files and the analyses for both of these versions in this repo. In the analysis folder there is likewise a cogsci_papers folder which stitches together the graphs from both individual experimental analyses.
