function make_slides(f) {
  var slides = {};

  slides.i0 = slide({
    name : "i0",
    start: function() {
    exp.startT = Date.now();
    }
  });

  slides.instructionsClickingPractice = slide({
    name : "instructionsClickingPractice",
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  slides.clickingPractice = slide({
    name : "clickingPractice",
    start: function(){
      exp.counter = 0;
      $(".err").hide();
    },
    // exp.clickingPractice --> if we want it to be the same as other practice stimuli
    // then just copy the other practice trials and save them as clickingPractice
    present: exp.clickingPractice,

    present_handle : function(stim) {

      exp.selection_array=[];
      this.stim = stim;
      $(".second_slide").hide();
      $(".grid_button").hide();
      $(".grid-container").hide();


      exp.objects =     ["","apples, pears, bananas and oranges.",
                        "apples, pears, bananas and oranges.",
                        "scissors, pencils, erasers and rulers.",
                        "scissors, pencils, erasers and rulers.",
                        "plates, forks, spoons and knives.",
                        "plates, forks, spoons and knives."]


      if ((stim.displayID == 1)| (stim.displayID == 3)| (stim.displayID == 5)) {
        var init_sentence = "This is " + stim.figure + ". " + stim.figure + " gives out " + stim.setting + " to children every day."
        var init_image = '<img src="images/'+ stim.figure + '.png" style="height:300px" class="center">';
        $(".sentence").html(init_sentence);
        $(".image").html(init_image);
        $(".second_slide").show();

      }
      else if ((stim.displayID == 2)| (stim.displayID == 4)| (stim.displayID == 6)) {
        var second_sentence = "Here is what " + stim.figure + " has on Tuesday. " + stim.figure + " has " + exp.objects[stim.displayID]
        $(".sentence").html(second_sentence);
        var second_image = '<img src="images/p.trial_'+ stim.displayID+ '.jpg" style="height:300px" class="center">';
        $(".image").html(second_image);
        $(".grid_button").show();
      }

    },

    second_slide: function(){
      $(".second_slide").hide();
      var second_sentence = "Here is what " + this.stim.figure + " has on Monday. " + this.stim.figure + " has " + exp.objects[this.stim.displayID] + " " + this.stim.pronoun + " always brings more than enough. The leftover " + this.stim.setting + " are put in the middle."
      $(".sentence").html(second_sentence);
      var second_image = '<img src="images/p.trial_'+ this.stim.displayID+ '.jpg" style="height:300px" class="center">';
      $(".image").html(second_image);
      $(".grid_button").show();
    },

    grid: function(){
      $(".err").hide();
      $(".image").html("  ");
      $(".grid_button").hide();
      $("#textInputPreludePractice").hide();
      $("#textInputPractice").hide();
      $(".buttonAfterInput").hide();

      if(exp.clicking_practice_trial_number) {
        exp.clicking_practice_trial_number = exp.clicking_practice_trial_number + 1;
      } else {
        exp.clicking_practice_trial_number = 1;
      }

      var instruction = this.stim.instruction1;
      words = instruction.split(" ")
      init_instruction = words[0]+ " " + words[1] + " " + words[2] + " ..."; // click on the
      instruction1 = words[0]+ " " + words[1] + " " + words[2] + " " + words[3] + " " + words[4] + " " + words[5] + " ..."; // click on the boy that has
      instruction2 = words[0]+ " " + words[1] + " " + words[2] + " " + words[3] + " " + words[4] + " " + words[5]+ " " + words[6] + " " + words[7] + " " + words[8] + " ..."; // click on the boy that has two of Susan's
      instruction3 = words[0]+ " " + words[1] + " " + words[2] + " " + words[3] + " " + words[4] + " " + words[5]+ " " + words[6] + " " + words[7] + " " + words[8] + " " + words[9] + ".";  // click on the boy that has two of Susan's pears

      const instruction_array=[instruction1,instruction2,instruction3]

      $(".sentence").html(init_instruction);

      var loc1_img = '<img src="images/'+this.stim.location1+'.png"style="height:100px" class="left">';
      $(".loc1").html(loc1_img);
      var loc2_img = '<img src="images/'+this.stim.location2+'.png" style="height:100px" class="center">';
      $(".loc2").html(loc2_img);
      var loc3_img = '<img src="images/'+this.stim.location3+'.png" style="height:100px" class="center">';
      $(".loc3").html(loc3_img);
      var loc4_img = '<img src="images/'+this.stim.location4+'.png" style="height:100px" class="center">';
      $(".loc4").html(loc4_img);
      var loc5_img = '<img src="images/'+this.stim.location5+'.png" style="height:90px" class="right">';
      $(".loc5").html(loc5_img);
      var loc6_img = '<img src="images/'+this.stim.location6+'.png" style="height:90px" class="left">';
      $(".loc6").html(loc6_img);

      //make the boys and girls clickable too
      var boy = '<img src="images/boy.png" style="height:200px" align="buttom">';
      var girl = '<img src="images/girl.png" style="height:200px" align="buttom">';
      $(".loc7").html(boy);
      $(".loc8").html(boy);
      $(".loc9").html(girl);
      $(".loc10").html(girl);
      $(".grid-container").show();

      $(".loc").bind("click",function(e){
        $(".err").hide();
        e.preventDefault();
        var loc = $(this).data().loc
        if (["AOI5","AOI6"].includes(loc)) {
          $(".err").show();
        }
        else {
          if (exp.counter>2){
            exp.selection_array.push(loc)
            exp.counter = 0;
            $(".loc").unbind('click')
            _s.button();
          } else {
            exp.selection_array.push(loc)
            $(".sentence").html(instruction_array[exp.counter])
            exp.counter++;
          }
        }
       });
    },

    button : function() {
      this.log_responses();
      _stream.apply(this); /* use _stream.apply(this); if and only if there is
      "present" data. (and only *after* responses are logged) */
    },

    log_responses : function() {
      exp.data_trials.push({
          "practiceTrialType" : "clicking practice",
          "displayID" : this.stim.displayID,
          "ExpFiller" : this.stim.ExpFiller,
          "setting" : this.stim.setting,
          "figure" : this.stim.figure,
          "Intro_object" : this.stim.Intro_object,
          "Res_object" : this.stim.Res_object,
          "object1" : this.stim.object1,
          "object2" :  this.stim.object2,
          "object3" :  this.stim.object3,
          "object4" : this.stim.object4,
          "display_type" : this.stim.display_type,
          "location1" : this.stim.location1,
          "location2" : this.stim.location2,
          "location3" : this.stim.location3,
          "location4" : this.stim.location4,
          "location5" : this.stim.location5,
          "location6" : this.stim.location6,
          "location7" : this.stim.location7,
          "location8" : this.stim.location8,
          "location9" : this.stim.location9,
          "location10" : this.stim.location10,
          "condition1" : this.stim.condition1,
          "size1" : this.stim.size1,
          "target1" : this.stim.target1,
          "competitor1" : this.stim.competitor1,
          "target_object1" : this.stim.target_object1,
          "target_figure1" : this.stim.target_figure1,
          "determiner1" : this.stim.determiner1,
          "object1" : this.stim.object1,
          "instruction1" : this.stim.instruction1,
          "prime" : this.stim.prime,
          "correctAns1"  : this.stim.correctAns1,
          "correctAns2" : this.stim.correctAns2,
          "clicking_practice_trial_number" : exp.clicking_practice_trial_number,
          "response" : exp.selection_array,
        });
      }
  });

  slides.instructions = slide({
    name : "instructions",
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  slides.practice = slide({
    name : "practice",
    start: function(){
      exp.counter = 0;
      $(".err").hide();
    },
    present: exp.practice,
    present_handle : function(stim) {

      // all exp.variablename variables are global
      this.stim = stim;
      $(".second_slide").hide();
      $(".grid_button").hide();
      $(".grid-container").hide();
      $("#textInputPractice").hide();
      $(".buttonAfterInput").hide();
      $("#textInputPreludePractice").hide();
      $(".err").hide();
      $(".sentence").show();

      exp.objects =     ["","apples, pears, bananas and oranges.",
                        "apples, pears, bananas and oranges.",
                        "scissors, pencils, erasers and rulers.",
                        "scissors, pencils, erasers and rulers.",
                        "plates, forks, spoons and knives.",
                        "plates, forks, spoons and knives."]


      if ((stim.displayID == 1)| (stim.displayID == 3)| (stim.displayID == 5)) {
        var init_sentence = "This is " + stim.figure + ". " + stim.figure + " gives out " + stim.setting + " to children every day."
        var init_image = '<img src="images/'+ stim.figure + '.png" style="height:300px" class="center">';
        $(".sentence").html(init_sentence);
        $(".image").html(init_image);
        $(".second_slide").show();

      }
      else if ((stim.displayID == 2)| (stim.displayID == 4)| (stim.displayID == 6)) {
        var second_sentence = "Here is what " + stim.figure + " has on Tuesday. " + stim.figure + " has " + exp.objects[stim.displayID]
        $(".sentence").html(second_sentence);
        var second_image = '<img src="images/p.trial_'+ stim.displayID+ '.jpg" style="height:300px" class="center">';
        $(".image").html(second_image);
        $(".grid_button").show();
      }

    },

    second_slide: function(){
      $(".second_slide").hide();
      $(".err").hide();
      $(".sentence").show();
      var second_sentence = "Here is what " + this.stim.figure + " has on Monday. " + this.stim.figure + " has " + exp.objects[this.stim.displayID] + " " + this.stim.pronoun + " always brings more than enough. The leftover " + this.stim.setting + " are put in the middle."
      $(".sentence").html(second_sentence);
      var second_image = '<img src="images/p.trial_'+ this.stim.displayID+ '.jpg" style="height:300px" class="center">';
      $(".image").html(second_image);
      $(".grid_button").show();
    },

    grid: function(){
      if(exp.practice_trial_number) {
        exp.practice_trial_number = exp.practice_trial_number + 1;
      } else {
        exp.practice_trial_number = 1;
      }

      $(".err").hide();
      $(".image").html("  ");
      $(".grid_button").hide();

      $("#textInputPreludePractice").show();
      $("#textInputPractice").show();
      $(".buttonAfterInput").show();
      $("#textInputPractice").val("");
      exp.response = ""; // set the response for this trial back to nothing
      $("#selectionPractice").removeClass();// set selection square back to nothing
      $(".sentence").hide();

      var loc1_img = '<img src="images/'+this.stim.location1+'.png"style="height:100px" class="left">';
      $(".loc1").html(loc1_img);
      var loc2_img = '<img src="images/'+this.stim.location2+'.png" style="height:100px" class="center">';
      $(".loc2").html(loc2_img);
      var loc3_img = '<img src="images/'+this.stim.location3+'.png" style="height:100px" class="center">';
      $(".loc3").html(loc3_img);
      var loc4_img = '<img src="images/'+this.stim.location4+'.png" style="height:100px" class="center">';
      $(".loc4").html(loc4_img);
      // the following two locations are the things in the middle (not clickable)
      var loc5_img = '<img src="images/'+this.stim.location5+'.png" style="height:90px" class="right">';
      $(".loc5").html(loc5_img);
      var loc6_img = '<img src="images/'+this.stim.location6+'.png" style="height:90px" class="left">';
      $(".loc6").html(loc6_img);

      $("#selectionPractice").addClass("squareProperties");

      // the following code just gets the target objects location from
      // stimuli.js and maps it to the css class associated with the red
      // box around its location
      var targetL = this.stim.target1;
      var targetClass = function(targetLocation = targetL) {
        if(targetLocation == "1") {
          return "locTopLeft";
        } else if (targetLocation == "2") {
          return "locBottomLeft";
        } else if (targetLocation == "3") {
          return "locTopRight";
        } else {
          return "locBottomRight";
        }
      }();

      // add the appropriate class to draw the red box around the target images
      $("#selectionPractice").addClass(targetClass);

      //make the boys and girls clickable too
      var boy = '<img src="images/boy.png" style="height:200px" align="buttom">';
      var girl = '<img src="images/girl.png" style="height:200px" align="buttom">';
      $(".loc7").html(boy);
      $(".loc8").html(boy);
      $(".loc9").html(girl);
      $(".loc10").html(girl);
      $(".grid-container").show();
    },

    // logs response from button press
    // you want to code it so that this function is for the continue3 button (under text input)
    buttonAfterInput : function() {
      exp.response = $("#textInputPractice").val(); // we define exp.response as a new variable here
      if (exp.response == "") {
        $(".err").hide()
        $("#noAnswer").show();
      } else if(exp.response.length < 4) {
        $(".err").hide()
        $("#shortAnswer").show();
      } else {
        $(".err").hide()
        this.log_responses(); // log responses
        _stream.apply(this); // go to next item in present


      }
    },

    log_responses : function() {
      exp.data_trials.push({
          "practiceTrialType" : "production practice",
          "displayID" : this.stim.displayID,
          "ExpFiller" : this.stim.ExpFiller,
          "setting" : this.stim.setting,
          "figure" : this.stim.figure,
          "Intro_object" : this.stim.Intro_object,
          "Res_object" : this.stim.Res_object,
          "object1" : this.stim.object1,
          "object2" :  this.stim.object2,
          "object3" :  this.stim.object3,
          "object4" : this.stim.object4,
          "display_type" : this.stim.display_type,
          "location1" : this.stim.location1,
          "location2" : this.stim.location2,
          "location3" : this.stim.location3,
          "location4" : this.stim.location4,
          "location5" : this.stim.location5,
          "location6" : this.stim.location6,
          "location7" : this.stim.location7,
          "location8" : this.stim.location8,
          "location9" : this.stim.location9,
          "location10" : this.stim.location10,
          "condition1" : this.stim.condition1,
          "size1" : this.stim.size1,
          "target1" : this.stim.target1,
          "competitor1" : this.stim.competitor1,
          "target_object1" : this.stim.target_object1,
          "target_figure1" : this.stim.target_figure1,
          "determiner1" : this.stim.determiner1,
          "object1" : this.stim.object1,
          "instruction1" : this.stim.instruction1,
          "prime" : this.stim.prime,
          "correctAns1"  : this.stim.correctAns1,
          "correctAns2" : this.stim.correctAns2,
          "practice_trial_number" : exp.practice_trial_number,
          "response" : exp.response, // since we initialized this as a variable of interest in the button function
          // we need to define it here too so that it is saved
        });
      }

  });

  slides.afterpractice = slide({
    name : "afterpractice",
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
      if(exp.trial_number) {
        exp.trial_number = exp.trial_number + 1;
      } else {
        exp.trial_number = 1;
      }

      exp.trial_start = Date.now();

      $(".err").hide();
      $(".grid-container").show();

      $("#textInputPreludeTrial").show();
      $("#textInputTrial").show();
      $(".buttonAfterInput").show();
      $("#textInputTrial").val("");
      this.response = ""; // set the response for this trial back to nothing
      $("#selectionTrial").removeClass();// set selection square back to nothing
      $(".sentence").hide();

      this.stim = stim; // store this information in the slide so you can record it later


      var loc1_img = '<img src="images/'+stim.location1+'.png"style="height:100px" class="left">';
      $(".loc1").html(loc1_img);
      var loc2_img = '<img src="images/'+stim.location2+'.png" style="height:100px" class="center">';
      $(".loc2").html(loc2_img);
      var loc3_img = '<img src="images/'+stim.location3+'.png" style="height:100px" class="center">';
      $(".loc3").html(loc3_img);
      var loc4_img = '<img src="images/'+stim.location4+'.png" style="height:100px" class="center">';
      $(".loc4").html(loc4_img);

      var loc5_img = '<img src="images/'+stim.location5+'.png" style="height:90px" class="right">';
      $(".loc5").html(loc5_img);
      var loc6_img = '<img src="images/'+stim.location6+'.png" style="height:90px" class="left">';
      $(".loc6").html(loc6_img);

      $("#selectionTrial").addClass("squareProperties");

      // the following code just gets the target objects location from
      // stimuli.js and maps it to the css class associated with the red
      // box around its location
      var targetL = this.stim.target1;
      var targetClass = function(targetLocation = targetL) {
        if(targetLocation == "1") {
          return "locTopLeft";
        } else if (targetLocation == "2") {
          return "locBottomLeft";
        } else if (targetLocation == "3") {
          return "locTopRight";
        } else {
          return "locBottomRight";
        }
      }();

      // add the appropriate class to draw the red box around the target images
      $("#selectionTrial").addClass(targetClass);

      var boy = '<img src="images/boy.png" style="height:200px" align="buttom">';
      var girl = '<img src="images/girl.png" style="height:200px" align="buttom">';
      $(".loc7").html(boy);
      $(".loc8").html(boy);
      $(".loc9").html(girl);
      $(".loc10").html(girl);

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
      exp.response = $("#textInputTrial").val(); // we define exp.response as a new variable here
      if (exp.response == "") {
        $("#noAnswerExp").show();
      } else if(exp.response.length < 4) {
        $("#shortAnswerExp").show();
      } else {
        console.log("success")
        exp.trial_end = Date.now();
        this.log_responses(); // log responses
        _stream.apply(this); // go to next item in present
      }
    },

    log_responses : function() {
    exp.data_trials.push({
        "displayID" : this.stim.displayID,
        "setting" : this.stim.setting,
        "figure" : this.stim.figure,
        "display_type" : this.stim.display_type,
        "location1" : this.stim.location1,
        "location2" : this.stim.location2,
        "location3" : this.stim.location3,
        "location4" : this.stim.location4,
        "location5" : this.stim.location5,
        "location6" : this.stim.location6,
        "location7" : this.stim.location7,
        "location8" : this.stim.location8,
        "location9" : this.stim.location9,
        "location10" : this.stim.location10,
        "Prime" : this.stim.Prime,
        "target1" : this.stim.target1,
        "target2" : this.stim.target2,
        "competitor1" : this.stim.competitor1,
        "competitor2" : this.stim.competitor2,
        "condition" : this.stim.condition,
        "determiner" : this.stim.determiner,
        "size" : this.stim.size,
        "ExpFiller" : this.stim.ExpFiller,
        "correctAns1" : this.stim.correctAns1,
        "correctAns2" : this.stim.correctAns2,
        "list" : this.stim.list,
        "target_object3" :this.stim.target_object3,
        "target_figure3" :this.stim.target_figure3,
        "instruction3" : this.stim.instruction3,
        "trial_number" : exp.trial_number,
        "response_times" : exp.trial_end - exp.trial_start,
        "response" : exp.response,
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

  exp.stims_shuffled = _.shuffle(exp.stims);

  //blocks of the experiment:
  exp.structure=["i0", "instructionsClickingPractice", "clickingPractice", "instructions", "practice", "afterpractice", "trial", 'subj_info', 'thanks'];
  //exp.structure=["i0", "instructions", "trial", 'subj_info', 'thanks'];

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
