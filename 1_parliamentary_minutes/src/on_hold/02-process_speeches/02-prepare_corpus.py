# packages
import logging
logging.propagate = False
from __future__ import print_function, unicode_literals
import warnings
warnings.filterwarnings("ignore",category=DeprecationWarning)
import spacy
spacy_de = spacy.load('de_core_news_sm')

# import data

with open('../../../../../01_data/listofnames.txt', 'r') as infile:
    listofnames = json.load(infile)

with open('../../../../../01_data/Plenarprotokolle/processed/id2speech.txt', 'r') as infile:
    id2speech = json.load(infile)
infile.close()

# filter only relevant data
id2speech_12_19 = {}

for key in id2speech.keys():
    if id2speech[key]["electoralTerm"] in ['12', '13', '14', '15', '16', '17', '18', '19']:
        id2speech_12_19[key] = id2speech[key]

# apply text preprocessing
# for key in id2speech_12_19.keys():
#     prepared_data = [id2speech_12_19[key]["speechContent"]]
#     id2speech_12_19[key]["cleaned_text"] = create_bigrams(stemming(tokenise_n_grams_lemma(prepared_data)))

for key in id2speech_12_19.keys():
    if id2speech_12_19[key]["electoralTerm"] != "19":
        id2speech_12_19[key]['timestamp'] = datetime.timestamp(datetime.strptime(id2speech_12_19[key]['date'],"%Y-%m-%d"))
    if id2speech_12_19[key]["electoralTerm"] == "19":
        id2speech_12_19[key]['timestamp'] = id2speech_12_19[key]['date']
        id2speech_12_19[key]['date'] = datetime.utcfromtimestamp(id2speech_12_19[key]['date']).strftime('%Y-%m-%d')

# save data
with open('../../../../../01_data/Plenarprotokolle/processed/id2speech_12_19.txt', 'w') as outfile:
    json.dump(id2speech_12_19, outfile)
outfile.close()




# creation of day2id2speech
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

# save data
with open('../../../../../01_data/Plenarprotokolle/processed/day2id2speech.txt', 'w') as outfile:
    json.dump(day2id2speech, outfile)
outfile.close()