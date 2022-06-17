/*
ALPS lab
May 2022
______________

This is the file that controls the experimental logic:
- it determines the order in which slides (e.g. instructions, trial, survey) appear
- it determines which objects in index.html appear for each slide
- provides functionality for the html objects (e.g. makes the "continue" button work)
- determines stimuli display (randomizing order)
- saves the stimuli and participant input data from each trial along with participant
  answers to the post-experiment survey

The script consists of the following parts:
init(): found at the end of the script. When the experiment starts, this function
  is called on first. It initializes the whole experiment, sets up the slide order,
  executes each slide by calling make_slides(), and then saves the data recorded
  in make_slides() at the end of the experiment. It is called upon in the index.html script.
make_slides(): This function is called on in init() and provides functionality
  for each type of slide, gives html() objects functionality, and records output
  from each trial
param code: the first several lines of code get URL parameters which determine
  experimental condition (i.e. which stimuli to use)

This experiment assumes the following URL parameters:
index.html?list={list1, list2}

the exp.go() function moves the experiment from one type of slide to the next.
This is useful for debugging. In the console in your browser's development tools
(command + option + j for chrome on macs), if you type in exp.go(); and press enter
it will skip to the next type of slide. Likewise, all statements that you console.log()
(i.e. print) in this script, will appear in the console of the browser's development
tools when you run the experiment locally.

At the end of the experiment you will see an output titled DEBUG MODE. This will
include all data that is saved by the experiment. Before publishing your experiment,
be sure that all the output you want is found there. This debug mode is automatically
turned off when the experiment is posted online and not run locally.
*/



// URL parameters code
// parameters are used to determine conditions (i.e. which stimuli will be presented)

// get the URL parameter
var urlParams = window.location.search.substring(1);
// if you have multiple URL parameters, you split them up using the &
// split the URL parameter string to get the individual paramters (if there are multiple of them)
var urlParamArray = urlParams.split("&");
// array that holds the parameter and its value
// e.g. for the URL "?list=list1&rotation=R2"
// paramArray = [["list", "list1"],["rotation", "R2"]]
var paramArray = [];

// loop through each URL parameter, split the parameter and its value
// add it to paramArray
for(i = 0; i< urlParamArray.length; i++) {
  splitParam = urlParamArray[i].split("=");
  paramArray.push([splitParam[0], splitParam[1]])
};

/*
  get value of given URL parameter
  Input: URL parameter name
  output: the URL parameter value
  e.g. for "index.html?list=L1"
    input: "list"
    output: "list1"
*/
function getUrlParameter(sParam) {
  for(i = 0; i < paramArray.length; i++) {
    if(paramArray[i][0] == sParam) {
      return paramArray[i][1];
    }
  }
};

// get URL parameter value for all conditions you have
var list = getUrlParameter('list');


/*

This function defines the functionality for all the
types of slides. The order of these slides are defined
in in the init() function, and this function is
called upon in the init() function.

Each slide is defined as a dictionary entry in the slides dictionary.
These dictionary entries include the following keys:
name: "string" = this corresponds to the id of the html <div> object
      that will be shown whenever a slide of that name is initiated.
start: function that is automatically executed whenever slide is initiated.
the other keys that are defined are called upon within start()

*/
function make_slides(f) {
  // variable that will hold all the slides
  var slides = {};

  /*
  define "i0" slide (defined in the html by id= i0)
  this slide has no functionality other than the start button.
  the functionality of the start button is
  defined in the init() function. Otherwise,
  everything under the html id = i0 will be shown.
  */
  slides.i0 = slide({
    name : "i0",
    start: function() {
      // log the start time of the experiment
      exp.startT = Date.now();
    }
  });

  /*
  This is the second slide in the order. This slide has
  no functionality other than to show everything under the
  html id = instructions. When the "continue" button is pressed,
  move onto the next type of slide.
  */
  slides.instructions = slide({
    name : "instructions",
    button : function() {
      // move onto the next type of slide when the button is pressed.
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  /*
  This defines the trial slides

  present_handle() is automatically called upon when the trial slide is initiated.
  It is subsequently called upon for every stimuli you have listed in exp.stims_shuffled
  (defined in init()) are exhausted. present_handle() is called upon through
  backend code found in the shared file.
  */
  slides.trial = slide({
    name : "trial",
    // present key holds all the experiments in shuffled order
    // exp.stims_shuffled is defined in init()
    present: exp.stims_shuffled,
    //every element in exp.stims is passed to present_handle one by one as 'stim'
    start: function(){
      exp.counter = 0;
    },

    present_handle : function(stim) {
      // save the start time of the trial so that we can calculate
      // how long the trial took
      exp.trial_start = Date.now();

      // print how long the trial takes
      console.log("time:"+(Date.now()-exp.trial_start));
      // print which stimulus we are working with
      console.log(stim);

      // hide all the error messages
      $(".err").hide();
      // show the grid-container, which will house all the images
      // in the midle of the screen
      $(".grid-container").show();
      // make the input textbox blank at the start of the trail
      $("#textInput").val("");
      // remove the html Class redSquare which has a red square border
      // defined for it in the CSS document
      $(".loc1").removeClass("redSquare");
      $(".loc2").removeClass("redSquare");
      $(".loc3").removeClass("redSquare");
      $(".loc4").removeClass("redSquare");


      // define this trials stim value as the "stim" that was fed into
      // the present_handle function
      // here we store this information in the slide so you can record it later
      this.stim = stim;

      // shuffle the order of the target and competitors between each trial
      // assign each of the four locations to a random order in this variable
      // loc1 = top left; loc2 = top right
      // loc3 = bottom left; loc 4 = bottom right
      loc_shuffled = _.shuffle([".loc1", ".loc2", ".loc3", ".loc4"])

      // assign the html code with the location of all four images
      // (target, competitor, and fillers)
      var loc_target = '<img src="images/'+stim.target_pic+'" style="width: 90%; height: 90%" class="img-scale-down">';
      var loc_competitor_pic = '<img src="images/'+stim.competitor_pic+'" style="width: 90%; height: 90%" class="img-scale-down">';
      var loc_filler_1 = '<img src="images/'+stim.filler_1+'" style="width: 90%; height: 90%" class="img-scale-down">';
      var loc_filler_2 = '<img src="images/'+stim.filler_2+'" style="width: 90%; height: 90%" class="img-scale-down">';

      // if you have multiple conditions, you can insert if else statements here
      // which determine which stimuli are assigned per trial

      // feed the html image object for each picture to one of the four locations
      // in <div class="grid-container">. The locations are randomized since
      // {".loc1", ..., ".loc4"} were assigned to random locations in loc_shuffled[0]
      $(loc_shuffled[0]).html(loc_target);
      $(loc_shuffled[1]).html(loc_competitor_pic);
      $(loc_shuffled[2]).html(loc_filler_1);
      $(loc_shuffled[3]).html(loc_filler_2);

      // add the redSquare class to the target location <div>
      // as defined in the CSS, and <div> with the redSquare class will have a
      // red border around it
      $(loc_shuffled[0]).addClass("redSquare");

      /*
        Everytime a key is pressed, the function checkKey(e) is executed
        which checks whether the button pressed was the "enter" key (
        keyCode = 13). If so, this function will execute buttonAfterInput().
        Therefore, you can press the enter key instead of pressing the continue
        button.
      */
      document.onkeypress = checkKey;
      function checkKey(e) {
        if (e.keyCode == 13) {
          _s.buttonAfterInput()
        }
      }
    },

    /*
      This function is called on by the html script after the
      "continue" button is pressed.

      It checks whether the input in the textbox is valid (defined here as
      3 characters or longer), and if so, logs the response and moves onto
      the next stimulus (i.e. back to the top of slides.trial())
    */
    buttonAfterInput : function() {
      // hide the error messages
      // the first $(".err").hide hides the error messages when you see a new
      // stimulus screen. This one hides subsequent error messages (e.g. if you
      // you type an input less than 3 characters long)
      $(".err").hide();

      // define exp.response as a variable that will hold the
      // participant response
      exp.response = $("#textInput").val();
      console.log(exp.response);

      // Check if the participant input is not nothing and the input is
      // at least 3 characters long
      if (exp.response == "") {
        $("#noAnswer").show();
      } else if(exp.response.length < 3) {
        $("#shortAnswer").show();
      } else {

        // get the time that the trail ended in order to compute trial time duration
        exp.trial_end = Date.now();

        // save the responses by calling the log_responses() function
        // (defined below)
        this.log_responses();

        // move onto the next stimulus item
        // after the last stimulus item, this function will work just like
        // exp.go()
        _stream.apply(this);
      }
    },

    /*
      Save the data that you want as a new entry in exp.data_trails
      which is defined in init().
      this.stim.X refers to a value found for this stimulus in stimuli.js
      exp.X refers to a value we saved in present_handle() or buttonAfterInput()
    */
    log_responses : function() {
      exp.data_trials.push({
          "trial" : this.stim.trial,
          "trialType" : this.stim.trialType,
          "trialID" : this.stim.trialID,
          "cond" : this.stim.cond,
          "target_pic": this.stim.target_pic,
          "competitor_pic": this.stim.competitor_pic,
          "filler_1_pic": this.stim.filler_1,
          "filler_2_pic": this.stim.filler_2,
          "loc_target_pic": loc_shuffled[0],
          "loc_competitor_pic": loc_shuffled[1],
          "loc_filler_1": loc_shuffled[2],
          "loc_filler_2": loc_shuffled[3],
          "response" : exp.response,
          "trial_number": exp.phase,
          "noun": this.stim.noun,
          "modifier": this.stim.modifier,
      });
    }
  });

  /*
  This is the third slide in the order. It comes after all the trial slides.
  This slide shows everything in the <div> element with the subj_info ID.
  This includes all the post-experiment survey questions, which are defined
  in the html script.

  submit(), when called from the html script, saves all the values from the
  survey questions.
  */
  slides.subj_info =  slide({
    name : "subj_info",
    submit : function(e){
      // save all the values input by the participants in the post-experiment
      // survey
      exp.subj_data = {
        language : $("#language").val(),
        enjoyment : $("#enjoyment").val(),
        asses : $('input[name="assess"]:checked').val(),
        age : $("#age").val(),
        gender : $("#gender").val(),
        education : $("#education").val(),
        comments : $("#comments").val(),
        problems: $("#problems").val(),
        fairprice: $("#fairprice").val()
      };
      // move onto the next slide once the submit funciton is called
      // in other words, submit the results!
      exp.go();
    }
  });

  /*
  Final slides of the experiment.
  When this slide is initialized, the start() function runs, which saves
  the trials, the catch trials, expeirmental condition, subject information,
  the amount of time it took to complete the experiment, and submits all of this
  using either proliferate or mturk.
  */
  slides.thanks = slide({
    name : "thanks",
    start : function() {
      exp.data= {
          "trials" : exp.data_trials,
          "catch_trials" : exp.catch_trials,
          "system" : exp.system,
          "condition" : exp.condition,
          "subject_information" : exp.subj_data,
          "time_in_minutes" : (Date.now() - exp.startT)/60000
      };

      // submit results to the proliferate tool. We use the proliferate tool when
      // running participants on prolific.
      proliferate.submit(exp.data);

      //if you are using Mturk, replace the above line of code with the following:
      /*
      setTimeout(function() {turk.submit(exp.data);}, 1000);
      */
    }
  });

  return slides;
}

/*
The init() function is what is called on first.
It initializes the whole expeirment, gets the
URl parameters and assignes the appropriate stimuli
based on those parameters, sets up the order in which
different parts of the experiment will be executed,
and then starts the expeirment.

This function is called on at the start of the index.html script.
*/
function init() {

  // I don't actually know what this does (XXXX)
  exp.trials = [];

  // exp.catch_trials is an array that will hold all
  // trials with errors. It should be empty by the end
  // of the experiment
  exp.catch_trials = [];

  // get participant computer info
  exp.system = {
      Browser : BrowserDetect.browser,
      OS : BrowserDetect.OS,
      screenH: screen.height,
      screenUH: exp.height,
      screenW: screen.width,
      screenUW: exp.width
    };

  // get the correct stimuli list based on the experimental condition
  // as determined by the URL parameters
  if (list == "list1") {
    exp.stims_shuffled = _.shuffle(exp.stims_cb_list1);
  } else {
    exp.stims_shuffled = _.shuffle(exp.stims_cb_list2);
  }

  // define order of experimental blocks
  // each of these corresponds to a slides.NAME variable
  // in the make_slides() function.
  exp.structure=["i0", "instructions", "trial", 'subj_info', 'thanks'];

  //exp.trials is an array that will hold all trial data
  // that we tell it to. This data is added at the end of
  // each trial.
  exp.data_trials = [];

  //make the slides by calling the make_slides() function,
  // defined above
  exp.slides = make_slides(exp);

  //prints the number of trials in the experiment
  exp.nQs = utils.get_exp_length();
  console.log(exp.nQs);
  //relies on structure and slides being defined

  // hide everything in the html document
  // so that we can begin with a blank slate
  $('.slide').hide();


  // Creates functionality for the html button
  // with the id start_button
  // when you press the button, move onto the next slide
  // (i.e. the instructions slide)
  $("#start_button").click(function() {
    exp.go();
  });

  // If you are running the expeirment on MTURK, replace the
  // above two lines with the following code
  /*
  $("#start_button").click(function() {
    if (turk.previewMode) {
      $("#mustaccept").show();
    } else {
      $("#start_button").click(function() {$("#mustaccept").show();});
      exp.go();
    }
  });
  */

  // Show the first slide (consent form slide)
  exp.go();
}
