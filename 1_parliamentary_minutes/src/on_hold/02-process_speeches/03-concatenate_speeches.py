

import pandas as pd
import json

# load data
with open('../../../../../01_data/Plenarprotokolle/processed/id2speech_12_19.txt', 'w') as infile:
    id2speech_12_19 = json.load(infile)
infile.close()

with open('../../../../../01_data/Plenarprotokolle/processed/day2id2speech.txt', 'r') as infile:
    day2id2speech = json.load(infile)
infile.close()

'''
Concatenate speeches of same speaker
'''

for day in day2id2speech.keys():
    for rede in day2id2speech[day].keys():
        day2id2speech[day][rede]["full_name"] = day2id2speech[day][rede]['firstName'] + " " + day2id2speech[day][rede]['lastName']

day2name2speech_concat = {}
for day in day2id2speech.keys():
    day2name2speech_concat[day] = {}

fullnames = []
for day in day2id2speech.keys():
    for rede in day2id2speech[day].keys():
        fullnames.append(day2id2speech[day][rede]["full_name"])
fullnames = list(pd.Series(fullnames).unique())

for day in day2id2speech.keys():
    for name in fullnames:
        day2name2speech_concat[day][name] = {}
        day2name2speech_concat[day][name]["concatSpeech"] = str()
        day2name2speech_concat[day][name]["concatCleaned_text"] = list()
        day2name2speech_concat[day][name]["length"] = 0
        day2name2speech_concat[day][name]["id"] = int(0)

# concatenate speeches
for day in list(day2id2speech.keys()):
    sorted_keys = sorted([int(key) for key in day2id2speech[day].keys()])
    for ID in sorted_keys:
        name = day2id2speech[day][str(ID)]["full_name"]

        current_length = len(day2id2speech[day][str(ID)]["speechContent"].split(" "))
        past_length = int(day2name2speech_concat[day][name]["length"])

        current_text = day2name2speech_concat[day][name]["concatSpeech"]
        current_cleaned = day2name2speech_concat[day][name]["concatCleaned_text"]

        additional_text = day2id2speech[day][str(ID)]["speechContent"]
        additional_cleaned = day2id2speech[day][str(ID)]["cleaned_text"]

        day2name2speech_concat[day][name]["concatSpeech"] = current_text + additional_text
        day2name2speech_concat[day][name]["concatCleaned_text"] = current_cleaned + additional_cleaned
        day2name2speech_concat[day][name]["length"] = current_length

        if current_length > past_length:
            day2name2speech_concat[day][name]["id"] = ID


with open('../../../../../01_data/Plenarprotokolle/processed/day2name2speech_concat.txt', 'w') as outfile:
    json.dump(day2name2speech_concat, outfile)
outfile.close()

# based on politician ID
day2polID2speech_concat = {}

for day in day2id2speech.keys():
    day2polID2speech_concat[day] = {}

PolIDs = []
for day in day2id2speech.keys():
    for rede in day2id2speech[day].keys():
        PolIDs.append(str(day2id2speech[day][rede]["politicianId"]) + " " + day2id2speech[day][rede]["full_name"])

PolIDs = list(pd.Series(PolIDs).unique())

for day in day2id2speech.keys():
    for PolID in PolIDs:
        day2polID2speech_concat[day][PolID] = {}
        day2polID2speech_concat[day][PolID]["concatSpeech"] = str()
        day2polID2speech_concat[day][PolID]["concatSpeech"] = str()
        day2polID2speech_concat[day][PolID]["concatCleaned_text"] = list()
        day2polID2speech_concat[day][PolID]["length"] = 0
        day2polID2speech_concat[day][PolID]["id"] = int(0)
        day2polID2speech_concat[day][PolID]["full_name"] = str()

for day in list(day2id2speech.keys()):
    sorted_keys = sorted([int(key) for key in day2id2speech[day].keys()])
    for ID in sorted_keys:
        name = str(day2id2speech[day][str(ID)]["politicianId"]) + " " + day2id2speech[day][str(ID)]["full_name"]

        current_length = len(day2id2speech[day][str(ID)]["speechContent"].split(" "))
        past_length = int(day2polID2speech_concat[day][name]["length"])

        current_text = day2polID2speech_concat[day][name]["concatSpeech"]
        current_cleaned = day2polID2speech_concat[day][name]["concatCleaned_text"]

        additional_text = day2id2speech[day][str(ID)]["speechContent"]
        additional_cleaned = day2id2speech[day][str(ID)]["cleaned_text"]

        day2polID2speech_concat[day][name]["concatSpeech"] = current_text + additional_text
        day2polID2speech_concat[day][name]["concatCleaned_text"] = current_cleaned + additional_cleaned
        day2polID2speech_concat[day][name]["length"] = current_length

        if current_length > past_length:
            day2polID2speech_concat[day][name]["id"] = ID

#save data
with open('../01_data/Plenarprotokolle/processed/day2polID_name2speech_concat.txt', 'w') as outfile:
    json.dump(day2polID2speech_concat, outfile)
outfile.close()


'''
Restructuring 
'''

day2id2speech_concat = {}

for day in day2name2speech_concat.keys():
    day2id2speech_concat[day] = {}

for day in day2name2speech_concat.keys():
    for name in day2name2speech_concat[day].keys():
        ID = day2name2speech_concat[day][name]["id"]
        speech = day2name2speech_concat[day][name]["concatSpeech"]

        day2id2speech_concat[day][str(ID)] = {}
        day2id2speech_concat[day][str(ID)]["concatSpeech"] = speech
        day2id2speech_concat[day][str(ID)]["name"] = name

id2speech_concat = {}
for day in day2name2speech_concat.keys():
    for name in day2name2speech_concat[day].keys():
        ID = day2name2speech_concat[day][name]["id"]
        speech = day2name2speech_concat[day][name]["concatSpeech"]

        id2speech_concat[str(ID)] = {}
        id2speech_concat[str(ID)]["concatSpeech"] = speech
        id2speech_concat[str(ID)]["name"] = name


# save data
with open('../01_data/Plenarprotokolle/processed/day2id2speech_concat.txt', 'w') as outfile:
    json.dump(day2id2speech_concat, outfile)
outfile.close()

with open('../01_data/Plenarprotokolle/processed/id2speech_concat.txt', 'w') as outfile:
    json.dump(id2speech_concat, outfile)
outfile.close()



