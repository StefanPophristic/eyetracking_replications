f = open('practicetrials.csv')
file = open('ptrials.js','a')

message = f.read()
line = message.split('\r') #line[0] is header , line[1]

length = len(line)

for x in range(1,length):
    variable = line[x].split(',')
    file.write('{')
    file.write('\t'+'displayID: "'+ variable[0] + '",')
    file.write('\n'+'\t')
    file.write('ExpFiller: "' + variable[1] + '",')
    file.write('\n'+'\t')
    file.write('setting: "' + variable[2] + '",')
    file.write('\n'+'\t')
    file.write('figure: "' + variable[3] + '",')
    file.write('\n'+'\t')
    file.write('Intro_object: "' + variable[4] + '",')
    file.write('\n'+'\t')
    file.write('Res_object: "' + variable[5].replace(" ","_") + '",')
    file.write('\n'+'\t')
    file.write('object1: "' + variable[6].replace(" ","_") + '",')
    file.write('\n'+'\t')
    file.write('object2: "' + variable[7].replace(" ","_") + '",')
    file.write('\n'+'\t')
    file.write('object3: "' + variable[8].replace(" ","_") + '",')
    file.write('\n'+'\t')
    file.write('object4: "' + variable[9].replace(" ","_") + '",')
    file.write('\n'+'\t')
    file.write('display_type: "' + variable[10] + '",')
    file.write('\n'+'\t')
    file.write('location1: "' + variable[11].replace(" ","_") + '",')
    file.write('\n'+'\t')
    file.write('location2: "' + variable[12].replace(" ","_") + '",')
    file.write('\n'+'\t')
    file.write('location3: "' + variable[13].replace(" ","_") + '",')
    file.write('\n'+'\t')
    file.write('location4: "' + variable[14].replace(" ","_") + '",')
    file.write('\n'+'\t')
    file.write('location5: "' + variable[15].replace(" ","_") + '",')
    file.write('\n'+'\t')
    file.write('location6: "' + variable[16].replace(" ","_") + '",')
    file.write('\n'+'\t')
    file.write('location7: "' + variable[17] + '",')
    file.write('\n'+'\t')
    file.write('location8: "' + variable[18] + '",')
    file.write('\n'+'\t')
    file.write('location9: "' + variable[19] + '",')
    file.write('\n'+'\t')
    file.write('location10: "' + variable[20] + '",')
    file.write('\n'+'\t')
    file.write('condition1: "' + variable[21] + '",')
    file.write('\n'+'\t')
    file.write('size1: "' + variable[22] + '",')
    file.write('\n'+'\t')
    file.write('target1: "' + variable[23] + '",')
    file.write('\n'+'\t')
    file.write('competitor1: "' + variable[24] + '",')
    file.write('\n'+'\t')
    file.write('target_object1: "' + variable[25].replace(" ","_") + '",')
    file.write('\n'+'\t')
    file.write('target_figure1: "' + variable[26] + '",')
    file.write('\n'+'\t')
    file.write('determiner1: "' + variable[27] + '",')
    file.write('\n'+'\t')
    file.write('object1: "' + variable[28] + '",')
    file.write('\n'+'\t')
    file.write('instruction1: "' + variable[29] + '"')
    file.write('\n'+'\t')
    file.write('prime: "' + variable[30] + '"')
    file.write('\n'+'\t')
    file.write('correctAns1: "' + variable[31] + '"')
    file.write('\n'+'\t')
    file.write('correctAns2: "' + variable[32] + '"')
    file.write('\n')
    file.write('},')
    file.write('\n')