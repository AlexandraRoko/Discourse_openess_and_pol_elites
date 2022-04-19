import sys
import csv
import pandas as pd
import json
from datetime import datetime


csv.field_size_limit(sys.maxsize)

'''
Add full WP19 data to Open Discourse data
'''

# read in open-discourse data for WP 1-19 (WP19 only partiell)
id2rede = {}

with open('../../../../../01_data/Plenarprotokolle/originals/speeches.csv', newline='') as csvfile:
    reader = csv.DictReader(csvfile)
    for row in reader:
        id_ = row["id"]
        row["speechContent"] = row["speechContent"].replace("\n", " ").replace("\xa0", " ")
        id2rede[id_] = row


# get fractional WP 19 data and ID for WP 19 politicans from open-discourse data
id2rede_19_opendis = {}
polIx2info = {}

for key in id2rede.keys():
    if id2rede[key]["electoralTerm"] == "19":
        id2rede_19_opendis[key] = id2rede[key]
        id_ = id2rede_19_opendis[key]["firstName"] + "_" + id2rede_19_opendis[key]["lastName"] + "_" + \
              id2rede_19_opendis[key]["positionShort"]

        temp_dic = {}
        temp_dic["firstName"] = id2rede[key]["firstName"]
        temp_dic["politicianId"] = id2rede[key]["politicianId"]
        temp_dic["lastName"] = id2rede[key]["lastName"]
        temp_dic["factionId"] = id2rede[key]["factionId"]
        temp_dic["positionShort"] = id2rede[key]["positionShort"]
        temp_dic["positionLong"] = id2rede[key]["positionLong"]
        polIx2info[id_] = temp_dic


#read in full WP 19 data -- Politician IDs are missing
WP19_speeches = pd.read_pickle("../../../../01_data/Plenarprotokolle/output/XX_finals/speech_content.pkl").reset_index().drop(columns = ["index"])


# adjust Id for merging with other WPs
WP19_speeches["id"] = WP19_speeches["id"]+1000000
WP19_speeches = WP19_speeches.rename(columns={"electoral_term": "electoralTerm",
                                              "first_name": "firstName",
                                              "last_name": "lastName",
                                              "faction_id": "factionId",
                                              "position_short": "positionShort",
                                              "position_long": "positionLong",
                                              "politician_id": "politicianId",
                                              "speech_content": "speechContent"})


# convert dataframe to dictionary
id2rede_19 = {}
id2rede_19 = WP19_speeches.to_dict(orient = "index")

# match correct politician ID to WP_19 data
for key in id2rede_19.keys():
    IX = id2rede_19[key]["firstName"] + "_" + id2rede_19[key]["lastName"] + "_" + id2rede_19[key]["positionShort"]
    id2rede_19[key]["speechContent"] = id2rede_19[key]["speechContent"].replace("\n", " ").replace("\xa0", " ")
    id2rede_19[key]["electoralTerm"] = str(id2rede_19[key]["electoralTerm"])

    if IX in polIx2info.keys():
        id2rede_19[key]["politicianId"] = polIx2info[IX]["politicianId"]
        id2rede_19[key]["factionId"] = polIx2info[IX]["factionId"]


# Add WP 19 to dictionary
id2speech = {}
last_day_in_id2rede = max([datetime.strptime(id2rede[key]['date'], "%Y-%m-%d") for key in id2rede.keys()])
for key in id2rede.keys():
    id2speech[key] = id2rede[key]
for key in id2rede_19.keys():
    if  datetime.fromtimestamp(id2rede_19[key]["date"]) > last_day_in_id2rede:
        id2speech[str(id2rede_19[key]["id"])] = id2rede_19[key]

# save data
with open('../../../../../01_data/Plenarprotokolle/processed/id2speech.txt', 'w') as outfile:
    json.dump(id2speech, outfile)
outfile.close()




'''
Limit data to years after 1990
'''

# filter only relevant data
id2speech_12_19 = {}

for key in id2speech.keys():
    if id2speech[key]["electoralTerm"] in ['12', '13', '14', '15', '16', '17', '18', '19']:
        id2speech_12_19[key] = id2speech[key]

for key in id2speech_12_19.keys():
    if type(id2speech_12_19[key]['date']) == str:
        id2speech_12_19[key]['timestamp'] = datetime.timestamp(datetime.strptime(id2speech_12_19[key]['date'],"%Y-%m-%d"))
    if type(id2speech_12_19[key]['date']) == float:
        id2speech_12_19[key]['timestamp'] = id2speech_12_19[key]['date']
        id2speech_12_19[key]['date'] = datetime.utcfromtimestamp(id2speech_12_19[key]['date']).strftime('%Y-%m-%d')

# save data
with open('../../../../../01_data/Plenarprotokolle/processed/id2speech_12_19.txt', 'w') as outfile:
    json.dump(id2speech_12_19, outfile)
outfile.close()


'''
Data per day and MP
'''

sorted_id2speech_12_19_keys = [str(entry) for entry in sorted([int(key) for key in id2speech_12_19.keys()])]

dates = []
datetime_dates = []
id2date = {}
for key in sorted_id2speech_12_19_keys:
    dates.append(id2speech_12_19[key]["date"])
    datetime_dates.append(datetime.strptime(id2speech_12_19[key]["date"], '%Y-%m-%d'))
    id2date[key] = datetime.strptime(id2speech_12_19[key]["date"], '%Y-%m-%d')

unique_dates = pd.Series(dates).unique()

day2id2speech = {}
for date in unique_dates:
    day2id2speech[date] = {}

for key in id2speech_12_19.keys():
    temp_key = id2speech_12_19[key]["date"]
    temp_dic = day2id2speech[temp_key].copy()
    temp_dic[key] = id2speech_12_19[key]

    day2id2speech[temp_key] = temp_dic

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

'''''
DAS IST ERST MÖGLICH NACH TEXTREINIGUNG
'''''

for day in day2id2speech.keys():
    for name in fullnames:
        day2name2speech_concat[day][name] = {}
        day2name2speech_concat[day][name]["concatSpeech"] = str()
        #day2name2speech_concat[day][name]["concatCleaned_text"] = list()
        day2name2speech_concat[day][name]["length"] = 0
        day2name2speech_concat[day][name]["id"] = int(0)

for day in list(day2id2speech.keys()):
    sorted_keys = sorted([int(key) for key in day2id2speech[day].keys()])
    for ID in sorted_keys:
        name = day2id2speech[day][str(ID)]["full_name"]

        current_length = len(day2id2speech[day][str(ID)]["speechContent"].split(" "))
        past_length = int(day2name2speech_concat[day][name]["length"])

        current_text = day2name2speech_concat[day][name]["concatSpeech"]
        #current_cleaned = day2name2speech_concat[day][name]["concatCleaned_text"]

        additional_text = day2id2speech[day][str(ID)]["speechContent"]
        #additional_cleaned = day2id2speech[day][str(ID)]["cleaned_text"]

        day2name2speech_concat[day][name]["concatSpeech"] = current_text + additional_text
        #day2name2speech_concat[day][name]["concatCleaned_text"] = current_cleaned + additional_cleaned
        day2name2speech_concat[day][name]["length"] = current_length

        if current_length > past_length:
            day2name2speech_concat[day][name]["id"] = ID

#flatten concatenated clean text
#for day in day2name2speech_concat.keys():
#    for name in day2name2speech_concat[day].keys():
#        ls = day2name2speech_concat[day][name]["concatCleaned_text"]#.keys()
#        day2name2speech_concat[day][name]["concatCleaned_text"] = [item for sublist in ls for item in sublist]

#save data
with open('../../../../../01_data/Plenarprotokolle/processed/day2name2speech_concat.txt', 'w') as outfile:
    json.dump(day2name2speech_concat, outfile)
outfile.close()


'''
Restructure data
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
        id2speech_concat[str(ID)]["date"] = day


#save data
with open('../../../../../01_data/Plenarprotokolle/processed/day2id2speech_concat.txt', 'w') as outfile:
    json.dump(day2id2speech_concat, outfile)
outfile.close()

#save data
with open('../../../../../01_data/Plenarprotokolle/processed/id2speech_concat.txt', 'w') as outfile:
    json.dump(id2speech_concat, outfile)
outfile.close()
