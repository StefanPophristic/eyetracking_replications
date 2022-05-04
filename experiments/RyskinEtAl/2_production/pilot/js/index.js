/*


This experiment assumes the following URL parameters:

index.html?rotation={R1,R2}&list={list1, list2}
*/



var pragContext;
var contexts = ["good", "bad"];
pragContext = contexts[Math.floor(Math.random() * contexts.length)];
console.log(pragContext);



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

// variable to hold each trials instruction
var instruction = "";

// variable to hold whether each trial's instruction used a modifier or not
var modifier_use = "";

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
      exp.selection_array=[];
      exp.time_array=[];
      exp.trial_start = Date.now();
      console.log("time:"+(Date.now()-exp.trial_start));
      console.log(stim);
      $(".err").hide();
      $(".grid-container").show();
      $("#textInput").val(""); // make input textbox blank at start of trial
      $(".loc").css( "border", "" ); // erase red border from previous target

      this.stim = stim; // store this information in the slide so you can record it later

      // semantic_contrast fillers, test trials, training contrast set present pragmatic good,
      // and training contrast set absent pragmatic bad conditions have the modifier
      // all other trials do not

      if (this.stim.trialType == "test" ||
      (this.stim.trialType == "train" && this.stim.cond == "contrast" && pragContext == "good") ||
      (this.stim.trialType == "train" && this.stim.cond == "no_contrast" && pragContext == "bad") ||
      (this.stim.trialType == "filler" && this.stim.cond == "semantic_contrast")) {
        modifier_use = "modifier";
      }
      else {
        modifier_use = "no_modifier";
      }

      loc_shuffled = _.shuffle([".loc1", ".loc2", ".loc3", ".loc4"]) //shuffles the ordering of the target and competitors between trials
      var loc_target = '<img src="images/'+stim.target_pic+'" style="width: 90%; height: 90%" class="img-scale-down">';
      $(loc_shuffled[0]).html(loc_target);
      if (pragContext === "good") {
        var loc_contrast = '<img src="images/'+stim.target_contrast_good+'" style="width: 90%; height: 90%" class="img-scale-down">';
        $(loc_shuffled[1]).html(loc_contrast);
      } else {
        var loc_contrast = '<img src="images/'+stim.target_contrast_bad+'" style="width: 90%; height: 90%" class="img-scale-down">';
        $(loc_shuffled[1]).html(loc_contrast);
      }
      var loc_big_filler = '<img src="images/'+stim.big_filler+'" style="width: 90%; height: 90%" class="img-scale-down">';
      $(loc_shuffled[2]).html(loc_big_filler);
      var loc_small_filler = '<img src="images/'+stim.small_filler+'" style="width: 90%; height: 90%" class="img-scale-down">';
      $(loc_shuffled[3]).html(loc_small_filler);


      // Randomly pick whether the target will be the real target from Ryskin et al.
      // Or the competitor item to which the adjective applies to

      // this variable will log whether the selection target for this experiment is
      // a "ryskinTarget" or "ryskinCompetitor"
      productionTargetType = "";
      productionTarget = "";


      // REDO THIS WITH A BALANCED LIST!!!!

      // // This only applies to target and training trials:
      // if (stim.trialType == "test" || (stim.trialType == "train" && stim.cond == "contrast")) {
      //   // If a random int is 0: Selection target is the target from Ryskin et al.
      //   // otherwise: the selection target is the competitor from Ryskin et al.
      //   if (getRandomInt(2) == 0) {
      //     // get location of target ("loc1", "loc2", "loc3", or "loc4")
      //     classID = $(loc_shuffled[0]).selector;
      //
      //     productionTargetType = "ryskinTarget";
      //     productionTarget = stim.target_pic;
      //
      //   } else {
      //     productionTargetType = "ryskinCompetitor";
      //
      //     // if adjective used is big get location of big filler
      //     // otherwise get location of small filler
      //     if (stim.target_pic.includes("big")) {
      //       classID = $(loc_shuffled[2]).selector;
      //       productionTarget = stim.big_filler;
      //     } else {
      //       classID = $(loc_shuffled[3]).selector;
      //       productionTarget = stim.small_filler;
      //     }
      //   }
      // } else {
      //   // If the item is not a test trial nor a training trial in the contrast condition
      //   // then keep the original target
      //
      //   // get location of target ("loc1", "loc2", "loc3", or "loc4")
      //   classID = $(loc_shuffled[0]).selector;
      //
      //   productionTargetType = "ryskinTarget";
      //   productionTarget = stim.target_pic;
      // }
      //
      // // add a red boundary around the target
      // $(classID).css( "border", "5px solid red" );

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
        "trial" : this.stim.trial,
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
        "instruction" : instruction,
        "response_times" : exp.time_array,
        "response" : exp.selection_array,
        "trial_number": exp.phase,
        "pragContext": pragContext,
        "modifier_use": modifier_use,
        "noun": this.stim.noun,
        "modifier": this.stim.modifier,
        "productionTargetType": productionTargetType,
        "productionTarget": productionTarget
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
      console.log("enter");
      exp.stims_shuffled = _.shuffle(exp.stims_cb_list1_R1);
      console.log(exp.stims_shuffled);
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
  console.log("exit if");

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
