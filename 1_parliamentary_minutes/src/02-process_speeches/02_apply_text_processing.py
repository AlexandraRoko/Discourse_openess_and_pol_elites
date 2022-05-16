'''
------------------------------------------------------------------------
Script name: 02_apply_text_processing.py
Purpose of script: text pre-processing
Dependencies: 01-prepare_data.py, helper_functions.py
Author: Alexandra Rottenkolber
------------------------------------------------------------------------
'''


import json
from helper_functions import stemming, tokenise_n_grams_lemma_spacy_POSTag

# read in data
with open('../../../../../01_data/Plenarprotokolle/processed/id2speech_concat.txt', 'w') as infile:
    id2speech_concat = json.load(infile)
infile.close()

'''
Text pre-processing (stemming, tokenising, POS tagging)
'''

# apply text processing
for key in id2speech_concat.keys():
    id2speech_concat[key]["data_ready"] = stemming(tokenise_n_grams_lemma_spacy_POSTag([id2speech_concat[key]["concatSpeech"]]))

with open('../../../../../01_data/Plenarprotokolle/processed/id2speech_concat_POSTag.txt', 'w') as outfile:
    json.dump(id2speech_concat, outfile)
outfile.close()


'''
Extracting and saving processed speeches only for LDA modelling
'''

id2speech_concat_keys = sorted(list(id2speech_concat.keys()))
id2speech_concat_keys_sorted = sorted([int(key) for key in id2speech_concat_keys])[1:]

data_ready_concat_POSTag = []
for key in id2speech_concat_keys_sorted:
    data_ready_concat_POSTag.append(id2speech_concat[str(key)]["data_ready"])

data_ready_concat_POSTag = [ls[0] for ls in data_ready_concat_POSTag]

with open('../../../../../01_data/Plenarprotokolle/processed/data_ready_concat_POSTag_all_sorted.txt', 'w') as outfile:
    json.dump(data_ready_concat_POSTag, outfile)
outfile.close()
