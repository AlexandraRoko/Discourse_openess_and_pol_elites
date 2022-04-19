import json
from helper_functions import stemming, tokenise_n_grams_lemma_spacy_POSTag

# load data
with open('../../../../../01_data/Plenarprotokolle/processed/id2speech_12_19.txt', 'w') as infile:
    id2speech_12_19 = json.load(infile)
infile.close()

with open('../../../../../01_data/Plenarprotokolle/processed/day2id2speech.txt', 'r') as infile:
    day2id2speech = json.load(infile)
infile.close()


data_12_19_keys = [str(item) for item in sorted([int(key) for key in id2speech_12_19.keys()])]
id2rede_concat_keys = [str(item) for item in sorted([int(key) for key in id2rede_concat.keys()])]

data_selected = []
for key in id2rede_concat_keys:
    if key in data_12_19_keys:
        data_selected.append(id2rede_concat[key]["concatSpeech"])


for key in id2speech_concat.keys():
    id2speech_concat[key]["data_ready"] = stemming(tokenise_n_grams_lemma_spacy_POSTag([id2rede_concat[key]["concatSpeech"]]))


with open('../../../../../01_data/Plenarprotokolle/processed/id2speech_concat_POSTag.txt', 'w') as outfile:
    json.dump(id2speech_concat, outfile)
outfile.close()