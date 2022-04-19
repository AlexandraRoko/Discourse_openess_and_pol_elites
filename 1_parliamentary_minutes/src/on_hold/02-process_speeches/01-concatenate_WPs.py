import sys
import csv
import pandas as pd
import json


csv.field_size_limit(sys.maxsize)

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
for key in id2rede.keys():
    if id2rede[key]["electoralTerm"] != "19":
        id2speech[key] = id2rede[key]
for key in id2rede_19.keys():
    id2speech[str(id2rede_19[key]["id"])] = id2rede_19[key]


# save data
with open('../../../../../01_data/Plenarprotokolle/processed/id2speech.txt', 'w') as outfile:
    json.dump(id2speech, outfile)
outfile.close()


'''
Extract Politician IDs
'''

id2MdB = {}
for key in id2speech.keys():
    temp_dic = {}

    _id = id2speech[key]['politicianId']
    temp_dic['firstName'] = id2speech[key]['firstName']
    temp_dic['lastName'] = id2speech[key]['lastName']
    temp_dic['positionShort'] = id2speech[key]['positionShort']
    temp_dic['positionLong'] = id2speech[key]['positionLong']

    id2MdB[_id] = temp_dic


# save data
with open('../../../../../01_data/Plenarprotokolle/processed/id2MdB.txt', 'w') as outfile:
    json.dump(id2MdB, outfile)
outfile.close()


'''
Limit data to relevant WPs
'''

# filter only relevant data
id2speech_12_19 = {}

for key in id2speech:
    if id2speech[key]["electoralTerm"] in ['12', '13', '14', '15', '16', '17', '18', '19']:
        id2speech_12_19[key] = id2speech[key]

# save data
with open('../../../../../01_data/Plenarprotokolle/processed/id2speech_12_19_raw.txt', 'w') as outfile:
    json.dump(id2speech_12_19, outfile)
outfile.close()

id2MdB_12_19 = {}
for key in id2speech_12_19.keys():
    temp_dic = {}

    _id = id2speech[key]['politicianId']
    temp_dic['firstName'] = id2speech[key]['firstName']
    temp_dic['lastName'] = id2speech[key]['lastName']
    temp_dic['positionShort'] = id2speech[key]['positionShort']
    temp_dic['positionLong'] = id2speech[key]['positionLong']

    id2MdB_12_19[_id] = temp_dic

# save data
with open('../../../../../01_data/Plenarprotokolle/processed/id2MdB_12_19.txt', 'w') as outfile:
    json.dump(id2MdB_12_19, outfile)
outfile.close()