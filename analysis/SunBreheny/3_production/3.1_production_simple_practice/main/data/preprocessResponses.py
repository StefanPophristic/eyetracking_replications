# Stefan Pophristic
# December 8th, 2021
# Clean Up Script for Sun and Breheny Production Study
#
# This script takes as its input a csv file produced from running the Sun and Breheny
#   production study (part of the eye tracking linking hypothesis project).
#   The script then takes the responses given by participants and sections out
#   the following information:
#   1. The gender that the participant used {boy, girl, noGender}
#   2. Whether the correct gender was used {1,0}. 1 is assumed if no gender term
#       was used
#   3. Whether the correct noun was used {1, 0}
#   4. Which determiner was used {a, one, two, three, four, some, all, 0}
#   5. Whether the correct determiner was used {1, 0, NoDet}. 0 means that the
#       incorrect determiner was used (e.g. using "two" in a scene where the
#       target image had 3 objects objects). NoDet means that the participant
#       did not use a determiner, which is not technically incorrect
#   The script then outputs the same csv file, but with all this information as
#   new columns
#
# #### NOTES ABOUT THE SCRIPT ####
#
# This script does not deal with any of practice trials, and will automatically
#   output that the responses were false
#
# This script cannot catch spelling errors, so before importing the csv file into
#   the R script, be sure to go over all false instances to make sure that they
#   are actually false and not a case of a spelling error (e.g. the participant
#   saying "spon" when the target was "spoon" should count as correct
#
# If a participant says "plate" when the target was "plates", this still counts
#   as correct
#
# If a participant uses the determiner "some" when the determiner "all" could
#   have applied, this still counts as correct (i.e. the script does not assume
#   a some to not all implicature)
#
#
# This code assumes "click on the boy with an orange" is a true statement when
#   e.g. the boy has two oranges
# However the code assumes "click on the boy with one orange" to be a false
#  statement when e.g. the boy has two oranges
#
# #### HOW TO USE THE SCRIPT ####
#
# This script is ready to be used. The only things that should be changed are
#   line 83, specifying the name of the csv file that should be imported, and
#   line 304, specifying the name of the csv file that will be exported.
#
# Once you finished this script, manually go through the output csv files and
#   check all the genderUsed = "noGender", detUsed = "noDet", correctGender = 0,
#   correctNoun = 0, and correctDet = 0 cases. These may arise due to spelling errors
#   e.g. "spon" instead of "spoon", or participants using unexpected words, e.g.
#   saying "male" instead of "boy". These cases should still count in the data.
#
# #### GUIDE TO VARIABLE NAMES ####
#
# responses: a list of strings of all the participant responses
# numRows: number of rows in the csv file
# genderUsed: list of the gender used in the participant response {"boy", "girl", 0}
# targetGender: the gender of the target picture {"boy", "girl"} and 0 for
#   practice trials
# correctGender: list of whether the gender that the participant used matches
#   the gender of the target picture {0, 1}. Assumes 0 for practice trials. If no
#   gender was used, this is counted as 0
# targetNoun: Noun of the objects in the target picture {apples, apple, scissors, scissor, etc.}
#   Assumes 0 for practice trials
# targetNum: Number of the objects in the target picture {1, 2, 3, 4}
#   Assumes 0 for practice trials
# correctNoun: List of whether the noun that the participant used matches
#   the noun of the object
# detUsed: List of strings of the determiner that the participant used {a, one,
#   two, three, four, some, all, 0}. 0 indicates no determiner was used or
#   that the trial is a practice trial.
# correctDet: List of whether the participant used a determiner that applies
#   given the total number of objects of the target type found in the scene
#   Assumes 0 for practice trials


import pandas as pd

# read in csv file of responses
df = pd.read_csv(r'3_production_simple-trials.csv')

#make all responses lower case
df['response'] = df['response'].str.lower()

responses = df['response'].tolist()
numRows = len(df.index)

# Check which gender participants used
genderUsed = []
for a in range(0, numRows):
    if "boy" in responses[a]:
        genderUsed.append("boy")
    elif "girl" in responses[a]:
        genderUsed.append("girl")
    else:
        genderUsed.append("noGender")

# Check whether they used the correct gender
targetGender = df['target_figure3'].tolist()
correctGender = []
for a in range(0, numRows):
    if genderUsed[a] == targetGender[a]:
        correctGender.append(1)
    elif genderUsed[a] == "noGender":
        correctGender.append(1)
    else:
        correctGender.append(0)

# Get the noun for and number of objects in the target picture
targetNounUnformatted = df['target_object3'].tolist()
targetNoun = []
targetNum = []
for a in range(0, numRows):
    if isinstance(targetNounUnformatted[a], str):
        temp = targetNounUnformatted[a].split("_")
        targetNoun.append(temp[1])
        targetNum.append(temp[0])
    else:
        targetNoun.append("0")
        targetNum.append("0")

# Check whether the participant used the correct noun
correctNoun = []
for a in range(0, numRows):
    if targetNoun[a] == "0":
        correctNoun.append("0")
    elif targetNoun[a].rstrip("s") in responses[a].rstrip("s"):
        correctNoun.append("1")
    elif targetNoun[a].rstrip("s") == "eraser" in responses[a]:
        correctNoun.append("1")
    else:
        correctNoun.append("0")


# Check which determiner the participant used
# possible determiners = some, all, a, an, 1, 2, 3, 4, one, two, three, four
detUsed = []
for a in range(0, numRows):
    if "AOI" in responses[a]:
        detUsed.append("0")
    elif "some" in responses[a]:
        detUsed.append("some")
    elif "all" in responses[a]:
        detUsed.append("all")
    elif " a " in responses[a]:
        detUsed.append("a")
    elif " an " in responses[a]:
        detUsed.append("a")
    elif "1" in responses[a]:
        detUsed.append("one")
    elif "2" in responses[a]:
        detUsed.append("two")
    elif "3" in responses[a]:
        detUsed.append("three")
    elif "4" in responses[a]:
        detUsed.append("four")
    elif "one" in responses[a]:
            detUsed.append("one")
    elif "two" in responses[a]:
            detUsed.append("two")
    elif "three" in responses[a]:
            detUsed.append("three")
    elif "four" in responses[a]:
            detUsed.append("four")
    else:
        detUsed.append("noDet")

# check if its the correct determiner

# targetLocation1-6 in the csv file contain labels of all the images on the screen
#   in the format of [number of the object]_[label for the object]
targetLocation1 = df['location1'].tolist()
targetLocation2 = df['location2'].tolist()
targetLocation3 = df['location3'].tolist()
targetLocation4 = df['location4'].tolist()
targetLocation5 = df['location5'].tolist()
targetLocation6 = df['location6'].tolist()
correctDet = []
for a in range(0, numRows):

    currentDet = detUsed[a]
    currentTargetNum = targetNum[a]

    #get total number of the target objects on the screen

    #get the words found in all 6 locations on the screen
    currentTargetNoun = targetNoun[a]
    currentLoc1 = targetLocation1[a]
    currentLoc2 = targetLocation2[a]
    currentLoc3 = targetLocation3[a]
    currentLoc4 = targetLocation4[a]
    currentLoc5 = targetLocation5[a]
    currentLoc6 = targetLocation6[a]

    #Strip all the words from their plural markings
    currentTargetNoun = currentTargetNoun.rstrip("s")
    currentLoc1 = currentLoc1.rstrip("s")
    currentLoc2 = currentLoc2.rstrip("s")
    currentLoc3 = currentLoc3.rstrip("s")
    currentLoc4 = currentLoc4.rstrip("s")
    currentLoc5 = currentLoc5.rstrip("s")
    currentLoc6 = currentLoc6.rstrip("s")

    #This variable will count up all instances of the target objects on the screen
    #   resulting in the count of total number of those objects on the screen
    totalNumOfTargetInScene = 0

    # Get the total number of the target noun seen in the display
    # Cover the case of knife <--> knives
    if currentTargetNoun == "knife" or currentTargetNoun == "knive":
        if ("knife" in currentLoc1) or ("knive" in currentLoc1):
            temp = currentLoc1.split("_")
            totalNumOfTargetInScene = totalNumOfTargetInScene + int(temp[0])
        if ("knife" in currentLoc2) or ("knive" in currentLoc2):
            temp = currentLoc2.split("_")
            totalNumOfTargetInScene = totalNumOfTargetInScene + int(temp[0])
        if ("knife" in currentLoc3) or ("knive" in currentLoc3):
            temp = currentLoc3.split("_")
            totalNumOfTargetInScene = totalNumOfTargetInScene + int(temp[0])
        if ("knife" in currentLoc4) or ("knive" in currentLoc4):
            temp = currentLoc4.split("_")
            totalNumOfTargetInScene = totalNumOfTargetInScene + int(temp[0])
        if ("knife" in currentLoc5) or ("knive" in currentLoc5):
            temp = currentLoc5.split("_")
            totalNumOfTargetInScene = totalNumOfTargetInScene + int(temp[0])
        if ("knife" in currentLoc6) or ("knive" in currentLoc6):
            temp = currentLoc6.split("_")
            totalNumOfTargetInScene = totalNumOfTargetInScene + int(temp[0])

    # Cover the case of all other nouns
    else:
        if currentTargetNoun in currentLoc1:
            temp = currentLoc1.split("_")
            totalNumOfTargetInScene = totalNumOfTargetInScene + int(temp[0])
        if currentTargetNoun in currentLoc2:
            temp = currentLoc2.split("_")
            totalNumOfTargetInScene = totalNumOfTargetInScene + int(temp[0])
        if currentTargetNoun in currentLoc3:
            temp = currentLoc3.split("_")
            totalNumOfTargetInScene = totalNumOfTargetInScene + int(temp[0])
        if currentTargetNoun in currentLoc4:
            temp = currentLoc4.split("_")
            totalNumOfTargetInScene = totalNumOfTargetInScene + int(temp[0])
        if currentTargetNoun in currentLoc5:
            temp = currentLoc5.split("_")
            totalNumOfTargetInScene = totalNumOfTargetInScene + int(temp[0])
        if currentTargetNoun in currentLoc6:
            temp = currentLoc6.split("_")
            totalNumOfTargetInScene = totalNumOfTargetInScene + int(temp[0])


    # Check whether the participant used the right determiner
    if (currentDet == "a"):
        correctDet.append(1)
    elif (currentDet == "one"):
        if (int(currentTargetNum) == 1):
            correctDet.append(1)
        else:
            correctDet.append(0)
    elif (currentDet == "two"):
        if (int(currentTargetNum) == 2):
            correctDet.append(1)
        else:
            correctDet.append(0)
    elif(currentDet == "three"):
        if (int(currentTargetNum) == 3):
            correctDet.append(1)
        else:
            correctDet.append(0)
    elif(currentDet == "four"):
        if (int(currentTargetNum) == 4):
            correctDet.append(1)
        else:
            correctDet.append(0)
    elif(currentDet == "some"):
        if (int(currentTargetNum) <= totalNumOfTargetInScene):
            correctDet.append(1)
        else:
            correctDet.append(0)
    elif(currentDet == "all"):
        correctDet.append(1)
    else:
        correctDet.append(1)

# Put all the new values back into the main pandas dataframe
df['genderUsed'] = genderUsed
df['correctGender'] = correctGender
df['correctNoun'] = correctNoun
df['detUsed'] = detUsed
df['correctDet'] = correctDet

# Export the data frame as a csv file
df.to_csv('3_production_simple-trials_cleaned.csv')
