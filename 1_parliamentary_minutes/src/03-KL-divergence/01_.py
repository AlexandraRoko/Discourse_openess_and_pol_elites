# packages
import numpy as np
import pandas as pd
import json
import time
from datetime import datetime

# Novelty measures
from gensim.models import LdaModel
from gensim.corpora import Dictionary
from helper_functions import KLdivergence_from_probdist_arrays

entropy_fix = np.log2(np.e)

PATH = "/Users/alexandrarottenkolber/Documents/02_Hertie_School/Master thesis/Master_Thesis_Hertie/data_analysis/01_data/Plenarprotokolle/"

# Import LDA model
file = PATH + "LDA_models/models/lda_model_concat_POSTag_self_tuned_all_sorted_shuffle_num_topics_100"
lda_model = LdaModel.load(file)

# Import base data
with open(PATH + 'processed/data_ready_concat_POSTag_all_sorted.txt', 'r') as infile:
    data_ready = json.load(infile)
infile.close()

with open(PATH + 'processed/id2speech_concat_POSTag.txt', 'r') as infile:
    id2rede_12_19 = json.load(infile)
infile.close()

'''
Preparation
'''

sorted_id2rede_12_19_keys = sorted(list(id2rede_12_19.keys()))  # [:10]

dates = []
datetime_dates = []
id2date = {}
for key in sorted_id2rede_12_19_keys:
    dates.append(id2rede_12_19[key]["date"])
    datetime_dates.append(datetime.strptime(id2rede_12_19[key]["date"], '%Y-%m-%d'))
    id2date[key] = datetime.strptime(id2rede_12_19[key]["date"], '%Y-%m-%d')

unique_dates = pd.Series(dates).unique()

day2id2rede = {}
for date in unique_dates:
    day2id2rede[date] = {}

for key in id2rede_12_19.keys():
    temp_key = id2rede_12_19[key]["date"]
    temp_dic = day2id2rede[temp_key].copy()
    temp_dic[key] = id2rede_12_19[key]

    day2id2rede[temp_key] = temp_dic

sortes_dates = [date.strftime("%Y-%m-%d") for date in sorted(datetime_dates)]
sortes_dates = pd.Series(sortes_dates).unique()

data_ready_sorted = []
for day in sortes_dates:
    sorted_day_keys = sorted(list(day2id2rede[day].keys()))
    for key in sorted_day_keys:
        data_ready_sorted.append(day2id2rede[day][key]["data_ready"])
data_ready_sorted = [item for sublist in data_ready_sorted for item in sublist]

sorted_keys = []
for day in list(day2id2rede.keys()):
    [sorted_keys.append(int(key)) for key in day2id2rede[day].keys()]
sorted_keys = [str(key) for key in sorted(sorted_keys)]

key2num = {}
for num, key in enumerate(sorted_keys):
    key2num[str(key)] = num

for day in day2id2rede.keys():
    for speech in day2id2rede[day].keys():
        day2id2rede[day][speech]["num_in_row"] = key2num[speech]

'''
Extracting topic distribution per speech
'''
# Create a dictionary representation of the documents.
dictionary_sorted = Dictionary(data_ready)

# rare words that would otherwise be excluded
keep_list = ["moria", "luftfilt", "bubendorfer-licht", "covid-19", "curevac", "coronainfektion", "impfstoffproduktion",
             "coronapandemi", "coronasituation",
             "coronakris", "testkapazitat", "coronapatient", "wirtschaftsstabilisierungsfond", "kontaktbeschrank",
             "sozialschutz-paket", "maskenpflicht", "lockdown",
             "coronazeit", "ffp2-mask", "kontaktnachverfolg", "covid-19-pandemi", "pandemiezeit", "coronamassnahm",
             "coronahilf", "coronabeding", "offnungsstrategi",
             "corona-app", "irini", "unternehmerlohn", "coronapolit", "lockdown-kris", "coronaleugn",
             "corona-steuerhilfegesetz", "coronaimpfstoff", "corona-warn-app",
             "pandemierat", "covid", "biontech", "arbeitsschutzkontrollgesetz", "krankenhauszukunftsgesetz",
             "tichanowskaja", "lockdown-massnahm", "astrazeneca",
             "baulandmobilisierungsgesetz", "inzidenzwert", "impfzentr", "novemberhilf", "dezemberhilf",
             "lockdown-polit", "covax", "geimpft", "impfstoffbeschaff",
             "mutant", "bundesnotbrems"]

dictionary_sorted.filter_extremes(no_below=1, no_above=0.6, keep_tokens=keep_list)

# Bag-of-words representation of the documents. (This is the CORPUS!!!!)
doc2bow_sorted = [dictionary_sorted.doc2bow(doc) for doc in data_ready]

for day in day2id2rede.keys():
    for key in day2id2rede[day].keys():
        day2id2rede[day][key]["doc2bow"] = doc2bow_sorted[key2num[key]]

# create a list with sorted keys
IDs_filtered = []

for date in day2id2rede.keys():
    for ID in day2id2rede[date].keys():
        IDs_filtered.append(int(ID))
IDs_filtered_sorted = [str(ID) for ID in sorted(IDs_filtered)]

id2rede_filtered = {}
error_for = []
for date in day2id2rede.keys():
    for ID in day2id2rede[date].keys():
        try:
            id2rede_filtered[ID] = day2id2rede[date][ID]
            id2rede_filtered[ID]["theta_array"] = np.array(
                lda_model.get_document_topics(doc2bow_sorted[day2id2rede[date][ID]['num_in_row']],
                                              minimum_probability=0.0))[:, 1]
        except IndexError:
            error_for.append((date, ID))

# change data type to save dictionary
id2rede_filtered_save = {}
for date in day2id2rede.keys():
    for ID in day2id2rede[date].keys():
        id2rede_filtered_save[ID] = day2id2rede[date][ID]
        id2rede_filtered_save[ID]["theta_array"] = [tup[1].item() for tup in lda_model.get_document_topics(
            doc2bow_sorted[day2id2rede[date][ID]['num_in_row']], minimum_probability=0.0)]

## export data
with open(PATH + 'processed/id2speech_concat_POSTag_inshape_withThetas.txt', 'w') as outfile:
    json.dump(id2rede_filtered_save, outfile)
outfile.close()

'''
Filter out very short speeches
'''

short_keys = []
for key in id2rede_filtered.keys():
    if len(id2rede_filtered[key]['concatSpeech'].split(" ")) < 50:
        short_keys.append(key)

print("Short speeches are: ", len(short_keys) / len(id2rede_filtered), "%")

for key in short_keys:
    del id2rede_filtered[key]

# create updated list with sorted keys (without short speeches)
IDs_filtered = []

for ID in id2rede_filtered.keys():
    IDs_filtered.append(int(ID))
IDs_filtered_sorted = [str(ID) for ID in sorted(IDs_filtered)]

ID2num_filtered = {}
for num, ID in enumerate(IDs_filtered_sorted):
    ID2num_filtered[ID] = num

for ID in id2rede_filtered.keys():
    id2rede_filtered[ID]["filtered_no"] = ID2num_filtered[ID]

'''
Calculating distance measures
'''

speech2KL = {}
windowsize_list = [1, 3, 5, 7, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 500,
                   1000]  # list(range(5, 100, 10)) #[4, 10] #list(range(0, 50, 5))

start_time = time.time()

for windowsize in windowsize_list:

    print(windowsize)
    speech2KL[windowsize] = {}

    for j in list(range(len(IDs_filtered_sorted) - windowsize - 1)):

        # find center
        theta_center = id2rede_filtered[IDs_filtered_sorted[j]]["theta_array"]

        # define window:
        after_boxend = j + windowsize + 1
        before_boxstart = j - windowsize

        before_theta_arr = []
        after_theta_arr = []
        for w in range(windowsize):
            before_theta_arr.append(id2rede_filtered[IDs_filtered_sorted[j - w - 1]]["theta_array"])
            after_theta_arr.append(id2rede_filtered[IDs_filtered_sorted[j + w + 1]]["theta_array"])

        before_theta_arr = np.array(before_theta_arr)
        beforenum = before_theta_arr.shape[0]
        before_centertheta_arr = np.tile(theta_center, reps=(beforenum, 1))

        after_theta_arr = np.array(after_theta_arr)
        afternum = after_theta_arr.shape[0]
        after_centertheta_arr = np.tile(theta_center, reps=(afternum, 1))

        # Calculate KLDs.
        before_KLDs = KLdivergence_from_probdist_arrays(before_centertheta_arr, before_theta_arr)
        after_KLDs = KLdivergence_from_probdist_arrays(after_centertheta_arr, after_theta_arr)

        # Calculate means of KLD.
        novelty = np.mean(before_KLDs)
        transience = np.mean(after_KLDs)
        resonance = novelty - transience

        # Final measures for this center speech.
        theta_center = id2rede_filtered[IDs_filtered_sorted[j]]["theta_array"]

        temp_dic = {}

        temp_dic["novelty"] = novelty
        temp_dic["transience"] = transience
        temp_dic["resonance"] = resonance

        speech2KL[windowsize][IDs_filtered_sorted[j]] = temp_dic

print(f'Time taken : {(time.time() - start_time) / 60:.2f} mins')

with open(PATH + 'processed/speech2KL_lda_model_concat_POSTag_self_tuned_all_without_very_short_speeches.txt',
          'w') as outfile:
    json.dump(speech2KL, outfile)
outfile.close()
