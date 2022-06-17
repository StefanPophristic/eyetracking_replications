# Intro

This is an example experimental set-up for a production study, as implemented in the ALPS Lab eye-tracking replication pipeline. This script is meant to help you decipher the scripts used in our past studies (e.g. Degen & Pophristic 2022) and create your own. Feel free to copy this script and modify it as you see fit.

If you open *index.html* on your browser and add "?list=list1" or "?list=list2" to the URL, you will be able to see the experiment that the code in this folder produces. (that is, type the following URL into your browser: *file:///Users/* **[path_to_this_folder]** */production/index.html/?list=list1*)

This readme file explains the overall structure of the experiment and how individual files interact. The individual files have comments that will provide more detailed explanations about their structure.

# structure

- [CSS](css/): This folder contains style guides for html elements in the experiment
  - [local-style.css](css/local-style.css): this css file contains the style guides for elements that we may potentially want to change for the experiment.
- [images](images/): this folder contains all the images used in the experiment.
- [js](js/): all JavaScript files
  - [index.js](js/index.js): this JavaScript file is the backbone of the game, it is responsible for setting up the order in which screens are presented, the functionality and dynamic items of each screen in the game, etc.
  - [stimuli.js](js/stimuli.js): this file contains all the stimuli data (for all conditions).
- [shared](shared/): This folder contains the backend code for making the script work. There are very few cases that require you to use anything in this folder. This backend was created by the [CoColab](https://cocolab.stanford.edu/) at Stanford. Please note that not all of the code in this folder is properly commented.
- [index.html](index.html): The html file that contains all the text (e.g. instructions) and builds the components into a user-friendly web output.

Now that you know the individual components, let's talk about how they interact.

index.html contains all the components for all the slides: consent form page, instructions page, trial page, post-experiment survey page, and progress bar. At the top of the document, the html file imports relevant files from the *shared* folder, so that it has the underlying functionality of the experiment, from *index.js* in order to make the game dynamic, *stimuli.js* so that it has access to the stimuli information, and *proliferate* or *MTURK*, the online tools we use to record participant responses.

index.js contains all the code to make the experiment dynamic. It has the following main functionalities:
1. **Slides**: Dictates which html objects are shown at any given moment in order to create sequential "slides". In other words, it ensures that when the participant opens the experiment, the participant will only see the consent-form page html objects. And when the participant clicks *continue*, only the instruction page html objects will show up. etc.
2. **Stimuli**: The index.html file has a static set up for trial slides (i.e. when the participant sees stimuli). However, the index.js file is responsible for picking stimuli and feeding them into the html objects that will display them. Here we can randomize where each image appears on the screen, and (using URL parameters) determine which stimuli will appear at all based on conditions. The index.js file pulls stimuli from the stimuli.js file. After picking the location of the target picture, the index.js file also applies a red box around that target picture, by feeding a "red box" class attribute to the index.html file. More on that in the index.js file.
3. **Input boxes**: In these production studies, participants input text for each trial. This JS file is responsibly for making the input box work. It reads in the participant input from the box, saves is along with that trial's stimuli information, and clears the input box when the participant moves onto the next trial. it also prevents the participant from moving onto the next trial if the input did not meet certain criteria.

If you want to get better acquainted with this experimental set up, I recommend first looking at index.js + stimuli.js and reading the comments there, then looking at index.html.

If you want to implement your own production experiment, you need to make changes to: stimuli.js, index.js, and index.html (potentially local-style.css).

This script follows the same logic as the incremental decision task script and therefore has a lot of identical code.
