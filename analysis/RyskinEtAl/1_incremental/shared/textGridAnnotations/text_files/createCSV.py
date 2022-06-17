# code that takes in a praat TextGrid File
# and returns a csv file with the values we want

import re
import pandas as pd

filename = []
intro_onset = []
intro_offset = []
adj_onset = []
adj_offset = []
noun_onset = []
noun_offset = []

allMerged = ['mergedA.txt', 'mergedB.txt', 'mergedC.txt', 'mergedD.txt', 'mergedE.txt' , 'mergedF.txt', 'mergedG.txt', 'mergedH.txt', 'mergedIJ.txt', 'mergedK.txt', 'mergedL.txt', 'mergedM.txt', 'mergedN.txt', 'mergedO.txt', 'mergedP.txt', "mergedR.txt", 'mergedS.txt', 'mergedT.txt', 'mergedUVWXYZ.txt']

for txt in allMerged:
    f = open(txt,'r')

    allLines = f.read()

    splitStimuli = allLines.split("item")
    splitStimuli.pop(0)
    splitStimuli.pop(0)

    for s in splitStimuli:
        start = 'name = \"'
        end = "\"\n        xmin"
        filename.append(re.search('%s(.*)%s' % (start, end), s).group(1))

        # get onset
        splitS = s.split("intervals")

        for subEntry in splitS:
            if "intro" in subEntry:
                intro_onset.append(re.search('%s(.*)%s' % ("xmin = ", "\n"), subEntry).group(1))
                intro_offset.append(re.search('%s(.*)%s' % ("xmax = ", "\n"), subEntry).group(1))
            elif "noun" in subEntry:
                noun_onset.append(re.search('%s(.*)%s' % ("xmin = ", "\n"), subEntry).group(1))
                noun_offset.append(re.search('%s(.*)%s' % ("xmax = ", "\n"), subEntry).group(1))
            elif "adj" in subEntry:
                adj_onset.append(re.search('%s(.*)%s' % ("xmin = ", "\n"), subEntry).group(1))
                adj_offset.append(re.search('%s(.*)%s' % ("xmax = ", "\n"), subEntry).group(1))
            # if on last loop
            # and you did not encounter an "adj"
            # then add NA
            if subEntry == splitS[-1] and len(adj_onset) < len(intro_onset):
                adj_onset.append("NA")
                adj_offset.append("NA")

d = {'filename':filename,'intro_onset':intro_onset, 'intro_offset':intro_offset, 'adj_onset':adj_onset, "adj_offset":adj_offset, 'noun_onset':noun_onset,'noun_offset':noun_offset}

df = pd.DataFrame(d)

df.to_csv('stimuliTimeStamps.csv')
