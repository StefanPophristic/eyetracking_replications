f = open('data3.csv')
file = open('stimuli3.js','a')

message = f.read()
line = message.split('\n') #line[0] is header , line[1]

length = len(line) -1

for x in range(1,length):
    variable = line[x].split(',')
    file.write('{')
    file.write('\t'+'displayID: "'+ variable[0] + '",')
    file.write('\n'+'\t')
    file.write('setting: "' + variable[1] + '",')
    file.write('\n'+'\t')
    file.write('figure: "' + variable[2] + '",')
    file.write('\n'+'\t')
    file.write('display_type: "' + variable[3] + '",')
    file.write('\n'+'\t')
    file.write('location1: "' + variable[4].replace(" ","_") + '",')
    file.write('\n'+'\t')
    file.write('location2: "' + variable[5].replace(" ","_") + '",')
    file.write('\n'+'\t')
    file.write('location3: "' + variable[6].replace(" ","_") + '",')
    file.write('\n'+'\t')
    file.write('location4: "' + variable[7].replace(" ","_") + '",')
    file.write('\n'+'\t')
    file.write('location5: "' + variable[8].replace(" ","_") + '",')
    file.write('\n'+'\t')
    file.write('location6: "' + variable[9].replace(" ","_") + '",')
    file.write('\n'+'\t')
    file.write('location7: "' + variable[10] + '",')
    file.write('\n'+'\t')
    file.write('location8: "' + variable[11] + '",')
    file.write('\n'+'\t')
    file.write('location9: "' + variable[12] + '",')
    file.write('\n'+'\t')
    file.write('location10: "' + variable[13] + '",')
    file.write('\n'+'\t')
    file.write('Prime: "' + variable[14] + '",')
    file.write('\n'+'\t')
    file.write('target1: "' + variable[15] + '",')
    file.write('\n'+'\t')
    file.write('target2: "' + variable[16] + '",')
    file.write('\n'+'\t')
    file.write('competitor1: "' + variable[17] + '",')
    file.write('\n'+'\t')
    file.write('competitor2: "' + variable[18] + '",')
    file.write('\n'+'\t')
    file.write('condition: "' + variable[19] + '",')
    file.write('\n'+'\t')
    file.write('determiner: "' + variable[20] + '",')
    file.write('\n'+'\t')
    file.write('size: "' + variable[21] + '",')
    file.write('\n'+'\t')
    file.write('ExpFiller: "' + variable[22] + '",')
    file.write('\n'+'\t')
    file.write('correctAns1: "' + variable[23] + '",')
    file.write('\n'+'\t')
    file.write('correctAns2: "' + variable[24] + '",')
    file.write('\n'+'\t')
    file.write('list: "' + variable[25] + '",')
    file.write('\n'+'\t')
    file.write('target_object3: "' + variable[26].replace(" ","_") + '",')
    file.write('\n'+'\t')
    file.write('target_figure3: "' + variable[27] + '",')
    file.write('\n'+'\t')
    file.write('object3: "' + variable[28] + '",')
    file.write('\n'+'\t')
    file.write('instruction3: "' + variable[29].rstrip() + '"')
    file.write('\n')
    file.write('},')
    file.write('\n')