# Data

## vwp_impr_data.csv

Leffel et al.'s (2016) eye-tracking data

## stimuli_all.csv

Stimuli information

## clickdata.csv

Click data from our MTurk experiment

## freeProductionResponsesAnnotated.csv

Free production data from our MTurk experiment,
with additional annotations as explained in productionAnnotation.txt

(The results of the free production experiment reported in the CogSci paper did not in fact rely on these annotations. They are included here for future exploration.)

## production.csv

output of processFreeProduction.R containing production probabilities of the target adjective derived in various ways, for this paper we used exact match of the first word because arguably this is closest to the online process in the VWP



# R code

## main.R

Generate the plots in our paper

## processFreeProduction.R

Takes freeProductionResponsesAnnotated.csv and returns
production.csv (see above for details)

## tim-salt2016-plotting-summary-nov17.r

Leffel et al.'s original code to generate the plot in their paper.


# Production Data Annotation Notes

Annotated with type of target modification

>> 0 means no modification relating to target adj

>> 1 means target adj is used in positive form; most likely it is the 1st word,
but it can be preceded by some other intersective adjectives,
e.g., horizontal thick line

>> N means the nominal form of the target adj is used,
e.g., bumpy -> with bumps; spotted -> with spots
bent arrow -> arrow bend; curved line -> curve

Note that some of these might be just typos: e.g., curve arrow

>> C means comparative form of the target adj is used

>> S means superlative form of the target adj is used

>> M means the target adjective is further modified
e.g., almost straight, mostly empty

>> MS means the superlative form is modified

e.g., second longest

>> ! means that the response seems obviously wrong, possibly due to misunderstanding or technical errors. If a participant systematically makes such responses we might consider removing them (e.g., participants #44, #65, #76, #103)


>> U means underinformative; possibly due to error
(the annotation quality on this might be less good because sometimes it is not clear
whether the description is truly underinformative)

>> A means some sort of antonym is used (rare)
e.g., "flat circle" -> single bump circle


Sometimes it can get tricky: big/large; bent/curved/curvy/curving; thick/wide/fat; antonyms
Right now the standard is quite strict; but the annotation scheme can be further revised


ExactMatch

First word matches the target word

InitialMatch

Description and target word phonetically share the same initial sequence
E.g., curved/curve/curvy/curving

MorphemeIncluded
E.g., bent arrow/arrowed bend

SynFirst: a synonym is used as the first word
E.g., for "spotted circle," "dotted circle" is included, but "circle with dots" is not


>> bc for bent/curved
>> bca for bent/crooked/angled/angular   (angle as a noun not included)
>> sd for spotted/dotted  (amongst other such as mottled, dappled, speckled, flecked, freckled, freckly, stippled, brindle, brindled)
>> ca for curved/curvy/arc/arch/arcing/arching
>> ff for full/filled
>> bl for big/large
>> wtf for wide/thick/fat
>> nts for narrow/thin/skinny/slim

SynIncluded: a synonym is included somewhere

Maybe stripes and lines lined should also be included?
Flat and straight?
