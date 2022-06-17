/*
ALPS lab
May 2022
______________

This is an example stimuli page.

Each list or rotation in a rotating latin square design should be encoded in its
own section and labelled accrodingly.
exp.NAME = [
	{... trial info 1... },
	{... trial info 2 ...},
	{... trial info 3 ...}
];

Which exp.NAME will be called is determined by the URL parameter, as defined
in the init() function in index.js (lines XXX).

Each stimulus should have the information encoded in the original experiment
you are replicating, including all conditions. Even if these names are redundant, it is better practice to have all variables collected in the original experiment,
so that you can merge this data with the original experiment data more easily
(and if any issues arose, more easily find where your data and the original
data mismatch).
*/

exp.stims_cb_list1 = [
	{
		trial: "1",
		list: "l1",
		trialType: "test",
		trialID: "test1",
		cond: "condition1",
		target_pic: "pickle_big.jpg",
		competitor_pic: "pickle_small.jpg",
		filler_1: "lobster.jpg",
		filler_2: "backpack.jpg",
		noun: "pickle",
		modifier: "big"
	},
	{
		trial: "1",
		list: "l2",
		trialType: "test",
		trialID: "test1",
		cond: "condition2",
		target_pic: "bucket_small.jpg",
		competitor_pic: "bucket_big.jpg",
		filler_1: "feather.jpg",
		filler_2: "microwave.jpg",
		noun: "bucket",
		modifier: "small",
	},
	{
		trial: "2",
		list: "l1",
		trialType: "filler",
		trialID: "filler1",
		cond: "condition1",
		target_pic: "alarmclock.jpg",
		competitor_pic: "bandage.jpg",
		filler_1: "blender.jpg",
		filler_2: "burrito.jpg",
		noun: "pickle",
		modifier: "big"
	}
];

exp.stims_cb_list2 = [
	{
		trial: "1",
		list: "l2",
		trialType: "test",
		trialID: "test1",
		cond: "condition2",
		target_pic: "bucket_small.jpg",
		competitor_pic: "bucket_big.jpg",
		filler_1: "feather.jpg",
		filler_2: "microwave.jpg",
		noun: "bucket",
		modifier: "small",
	},
	{
		trial: "1",
		list: "l2",
		trialType: "filler",
		trialID: "filler1",
		cond: "condition2",
		target_pic: "microscope.jpg",
		competitor_pic: "onion.jpg",
		filler_1: "pie.jpg",
		filler_2: "scroll.jpg",
		noun: "bucket",
		modifier: "small",
	},
];
