/*


This experiment assumes the following URL parameters:

index.html?rotation={R1,R2}&list={list1, list2}&pragContext={g, b}

Figure out what the base cases for url are

The data saved can be found in the readme file. 
*/

// Code that gets the url parameters
var urlParams = window.location.search.substring(1);
var urlParamArray = urlParams.split("&");
var paramArray = [];
for(i = 0; i< urlParamArray.length; i++) {
  splitParam = urlParamArray[i].split("=");
  paramArray.push([splitParam[0], splitParam[1]])
};

// Input: URL parameter name
// output: the URL parameter value
// e.g. "index.html?rotation=R1"
// input: "rotation"
// output: "R1"
function getUrlParameter(sParam) {
  for(i = 0; i < paramArray.length; i++) {
    if(paramArray[i][0] == sParam) {
      return paramArray[i][1];
    }
  }
};

var rotation = getUrlParameter('rotation');
console.log(rotation);

var list = getUrlParameter('list');
console.log(list);

var pragParameter = getUrlParameter('pragContext');

if (pragParameter == "g") {
  pragContext = "good";
} else {
  pragContext = "bad";
}
console.log(pragContext);

function make_slides(f) {
  var slides = {};

  slides.i0 = slide({
    name : "i0",
    start: function() {
    exp.startT = Date.now();
    }
  });

  slides.instructions = slide({
    name : "instructions",
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  slides.trial = slide({
    name : "trial",
    present: exp.stims_shuffled, //every element in exp.stims is passed to present_handle one by one as 'stim'
    start: function(){
      exp.counter = 0;
    },

    present_handle : function(stim) {

      exp.trial_start = Date.now();
      console.log("time:"+(Date.now()-exp.trial_start));
      console.log(stim);
      $(".err").hide();
      $(".grid-container").show();
      $("#textInput").val(""); // make input textbox blank at start of trial

      // remove the html Class redSquare which has a red square border
      // defined for it in the CSS document
      $(".loc1").removeClass("redSquare");
      $(".loc2").removeClass("redSquare");
      $(".loc3").removeClass("redSquare");
      $(".loc4").removeClass("redSquare");

      this.stim = stim; // store this information in the slide so you can record it later

      loc_shuffled = _.shuffle([".loc1", ".loc2", ".loc3", ".loc4"]) //shuffles the ordering of the target and competitors between trials
      var loc_target = '<img src="images/'+stim.target_pic+'" style="width: 90%; height: 90%" class="img-scale-down">';
      if (pragContext === "good") {
        var loc_contrast = '<img src="images/'+stim.target_contrast_good+'" style="width: 90%; height: 90%" class="img-scale-down">';
      } else {
        var loc_contrast = '<img src="images/'+stim.target_contrast_bad+'" style="width: 90%; height: 90%" class="img-scale-down">';
      }
      var loc_big_filler = '<img src="images/'+stim.big_filler+'" style="width: 90%; height: 90%" class="img-scale-down">';
      var loc_small_filler = '<img src="images/'+stim.small_filler+'" style="width: 90%; height: 90%" class="img-scale-down">';

      // load images into their position
      $(loc_shuffled[0]).html(loc_target);
      $(loc_shuffled[1]).html(loc_contrast);
      $(loc_shuffled[2]).html(loc_big_filler);
      $(loc_shuffled[3]).html(loc_small_filler);

      // add the redSquare class to the target location <div>
      // as defined in the CSS, and <div> with the redSquare class will have a
      // red border around it
      $(loc_shuffled[0]).addClass("redSquare");

      // If you press the enter key (keyCode = 13), act as if you pressed
      // the continue button
      document.onkeypress = checkKey;
      function checkKey(e) {
        if (e.keyCode == 13) {
          _s.buttonAfterInput()
        }
      }
    },

    // logs response from button press
    // you want to code it so that this function is for the continue3 button (under text input)
    buttonAfterInput : function() {
      $(".err").hide();
      console.log("entered");
      exp.response = $("#textInput").val(); // we define exp.response as a new variable here
      console.log(exp.response);

      if (exp.response == "") {
        $("#noAnswer").show();
      } else if(exp.response.length < 3) {
        $("#shortAnswer").show();
      } else {
        console.log("success")
        exp.trial_end = Date.now();
        this.log_responses(); // log responses
        _stream.apply(this); // go to next item in present
      }
    },

    log_responses : function() {
    exp.data_trials.push({
      // XXX log whether this is "normal" or "switch", and the rotation list and all that stuff
        "stim_trial_num" : this.stim.trial,
        "trialType" : this.stim.trialType,
        "trialID" : this.stim.trialID,
        "cond" : this.stim.cond,
        "target_pic": this.stim.target_pic,
        "target_contrast_good" : this.stim.target_contrast_good,
        "target_contrast_bad" : this.stim.target_contrast_bad,
        "big_filler" : this.stim.big_filler,
        "small_filler" : this.stim.small_filler,
        "loc_target_pic": loc_shuffled[0],
        "loc_contrast": loc_shuffled[1],
        "loc_big_filler": loc_shuffled[2],
        "loc_small_filler": loc_shuffled[3],
        "trial_number": exp.phase,
        "pragContext": pragContext,
        "noun": this.stim.noun,
        "modifier": this.stim.modifier,
        "list": this.stim.counterbalance,
        "rotationList":this.stim.target_rotation,
        "productionList":this.stim.production_list,
        "response": exp.response
    });
    }
  });

  slides.subj_info =  slide({
    name : "subj_info",
    submit : function(e){
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
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

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
      proliferate.submit(exp.data);
    }
  });

  return slides;
}

/// init ///
function init() {
  exp.trials = [];
  exp.catch_trials = [];

  exp.system = {
      Browser : BrowserDetect.browser,
      OS : BrowserDetect.OS,
      screenH: screen.height,
      screenUH: exp.height,
      screenW: screen.width,
      screenUW: exp.width
    };

  console.log(list);
  console.log(rotation);
  // specify which condition we should get stimuli from
  if (list == "list1") {
    if(rotation == "R1") {
      exp.stims_shuffled = _.shuffle(exp.stims_cb_list1_R1);
    } else {
      exp.stims_shuffled = _.shuffle(exp.stims_cb_list1_R2);
    }
  } else {
    if(rotation == "R1") {
      exp.stims_shuffled = _.shuffle(exp.stims_cb_list2_R1);
    } else {
      exp.stims_shuffled = _.shuffle(exp.stims_cb_list2_R2);
    }
  }

  //blocks of the experiment:
  //exp.structure=["i0", "instructions", "practice", "afterpractice", "trial", 'subj_info', 'thanks'];
  exp.structure=["i0", "instructions", "trial", 'subj_info', 'thanks'];

  exp.data_trials = [];
  //make corresponding slides:
  exp.slides = make_slides(exp);

  exp.nQs = utils.get_exp_length(); //this does not work if there are stacks of stims (but does work for an experiment with this structure)
                    //relies on structure and slides being defined

  $('.slide').hide(); //hide everything

  //make sure turkers have accepted HIT (or you're not in mturk)
  $("#start_button").click(function() {
    if (turk.previewMode) {
      $("#mustaccept").show();
    } else {
      $("#start_button").click(function() {$("#mustaccept").show();});
      exp.go();
    }
  });

  exp.go(); //show first slide
}
