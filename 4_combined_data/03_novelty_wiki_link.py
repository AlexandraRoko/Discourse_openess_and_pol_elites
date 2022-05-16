import numpy as np
import pandas as pd
import json
from datetime import datetime

# set path
PATH = "./data_analysis/01_data/Plenarprotokolle/"

# import data
with open(PATH + 'processed/id2speech_concat_POSTag.txt', 'r') as infile:
    id2rede_12_19 = json.load(infile)
infile.close()
del id2rede_12_19["0"]

with open(PATH + 'processed/speech2KL_lda_model_concat_POSTag_self_tuned_all_sorted.txt', 'r') as infile:
    speech2KL = json.load(infile)
infile.close()

with open(PATH + 'processed/PolID2info.txt', 'r') as infile:
    PolID2info = json.load(infile)
infile.close()

NamesAndSpeeches = pd.read_csv(PATH + "processed/NamesAndSpeeches_full.csv").drop(columns = ["Unnamed: 0"])

# process data
def string_to_list(string):
    if string is np.nan:
        res = np.nan
    elif type(string) == str:
        res = string.replace("[", "").replace("]", "").split(", ")

    return res

NamesAndSpeeches["speeches"] = NamesAndSpeeches["speeches"].map(lambda x : string_to_list(x))

# add electoral term indicator
NamesAndSpeeches_per_WP = pd.DataFrame()

speech_ids = []
WPs = []
date_stamp = []

for key in id2rede_12_19.keys():
    speech_ids.append(key)
    WPs.append(id2rede_12_19[key]["electoralTerm"])
    date_stamp.append(id2rede_12_19[key]["date"])

speeches_and_WPs = pd.DataFrame(list(zip(speech_ids, WPs, date_stamp)), columns =['speeches', "WPs", "date"])

speech2wikidataid = {}

for ID in list(NamesAndSpeeches["wikidataid"]):
    if list(NamesAndSpeeches["speeches"][NamesAndSpeeches["wikidataid"] == ID])[0] is np.nan:
        pass
    else:
        for speech in list(NamesAndSpeeches["speeches"][NamesAndSpeeches["wikidataid"] == ID])[0]:
            speech2wikidataid[speech] = ID


def apply_dic_mapping(item):
    if item in speech2wikidataid.keys():
        res = speech2wikidataid[item]

    else:
        res = np.nan

    return res

speeches_and_WPs["wikidataid"] = speeches_and_WPs["speeches"].map(lambda x : apply_dic_mapping(x))


# add novelty, transience, resonance score to dataframe
novelty = []
transience = []
resonance = []
window = 25

no_value = []

ls_speeches = list(speeches_and_WPs["speeches"])

for i in range(len(ls_speeches)):
    if ls_speeches[i] in speech2KL[str(window)].keys():
        if "novelty" in speech2KL[str(window)][ls_speeches[i]].keys():
            novelty.append(speech2KL[str(window)][ls_speeches[i]]["novelty"])
        else:
            print(i)
            novelty.append(np.nan)

        if "transience" in speech2KL[str(window)][ls_speeches[i]].keys():
            transience.append(speech2KL[str(window)][ls_speeches[i]]["transience"])
        else:
            transience.append(np.nan)

        if "resonance" in speech2KL[str(window)][ls_speeches[i]].keys():
            resonance.append(speech2KL[str(window)][ls_speeches[i]]["resonance"])
        else:
            resonance.append(np.nan)

    else:
        no_value.append(i)
        novelty.append(np.nan)
        transience.append(np.nan)
        resonance.append(np.nan)

speeches_and_WPs["novelty"] = pd.Series(novelty)
speeches_and_WPs["transience"] = pd.Series(transience)
speeches_and_WPs["resonance"] = pd.Series(resonance)

# merge data together
NamesWPSpeeches = pd.merge(speeches_and_WPs, NamesAndSpeeches[['politicianID', 'surname_lower_comb', 'forename_lower_comb',
       'pageid', 'wikidataid', 'name']], how = "left", left_on=["wikidataid"], right_on=["wikidataid"])

NamesWPSpeeches = NamesWPSpeeches.rename(columns = {"speeches" : "speech"})

NamesWPSpeeches["year"] = NamesWPSpeeches["date"].map(lambda x: datetime.strptime(x, '%Y-%m-%d').year)
NamesWPSpeeches["month"] = NamesWPSpeeches["date"].map(lambda x: datetime.strptime(x, '%Y-%m-%d').month)

# save data
NamesWPSpeeches.to_csv(PATH + "./processed/NamesWPSpeeches.csv")

# map missing names and rename parties
party_dic = {'BÜNDNIS 90/DIE GRÜNEN': 'BÜNDNIS 90/DIE GRÜNEN',
             'PDS': 'PDS',
             'CDU': "CDU/CSU",
             'SPD': 'SPD',
             'FDP': 'FDP',
             'CSU': "CDU/CSU",
             'DIE LINKE.': 'DIE LINKE.',
             'Die Linke.': 'DIE LINKE.',
             'PDS/LL': 'PDS',
             'Plos': 'Parteilos',
             'AfD': 'AfD',
            "CDU/CSU" : "CDU/CSU",
            'Parteilos': 'Parteilos'}


MissingName2Party = {'Otto Graf Lambsdorff': "FDP",
                     'Jörg Wolfgang Ganschow': "FDP",
                     'Jürgen Möllemann': "FDP",
                     'Rainer Ortleb': "FDP",
                     'Harald B. Schäfer': "SPD",
                     'Joachim Graf von Schönburg-Glauchau': "CDU",
                     'Peter Reuschenbach': "SPD",
                     'Uwe Lühr': "FDP",
                     'Cornelia von Teichman und Logischen': "FDP",
                     'Heinrich Leonhard Kolb': "FDP",
                     'Verena Wohlleben': "SPD",
                     'Lieselott Blunck': "SPD",
                     'Kurt Rossmanith': "CSU",
                     'Hans H. Gattermann': "FDP",
                     'Karsten Voigt': "SPD",
                     'Gerhard O. Pfeffermann': "CDU",
                     'Ina Albowitz': "FDP",
                     'Petra Bläss': "PDS",
                     'Lutz Stavenhagen': "CDU",
                     'Ferdinand Tillmann': "CDU",
                     'Claudia Nolte': "CDU",
                     'Gunter Weißgerber': "SPD",
                     'Peter Paterna': "SPD",
                     'Ursula Lehr': "CDU",
                     'Klaus Lippold': "CDU",
                     'Hans-Werner Müller': "CDU",
                     'Roswitha Verhülsdonk': "CDU",
                     'Ursula Seiler-Albring': "FDP",
                     'Karl-Heinz Schröter': "SPD",
                     'Herbert Meißner': "SPD",
                     'Hedda Meseke': "CDU",
                     'Erich G. Fritz': "CDU",
                     'Heinz-Dieter Hackel': "FDP",
                     'Jochen Welt': "SPD",
                     'Paul Friedhoff': "FDP",
                     'Hans Georg Wagner': "SPD",
                     'Peter Harry Carstensen': "CDU",
                     'Werner Skowron': "CDU",
                     'Walter Franz Altherr': "CDU",
                     'Karl Hermann Haack': "SPD",
                     'Claire Marienfeld': "CDU",
                     'Werner Schuster': "SPD",
                     'Klaus-Dieter Uelhoff': "CDU",
                     'Karl H. Fell': "CDU",
                     'Erich Maaß': "CDU",
                     'Hans P. H. Schuster': "FDP",
                     'Reinhard Meyer zu Bentrup': "CDU",
                     'Hans A. Engelhard': "FDP",
                     'Heinrich Graf von Einsiedel': "SPD",
                     'Gila Altmann': "Bündnis 90/Die Grünen",
                     'Halo Saibold': "Bündnis 90/Die Grünen",
                     'Wolfgang Ilte': "SPD",
                     'Dagmar Wöhrl': "CSU",
                     'Franz Peter Basten': "CDU",
                     'Gottfried Tröger': "CDU",
                     'Wilma Glücklich': "CDU",
                     'Bernd Klaußner': "CDU",
                     'Karl A. Lamers': "CDU",
                     'Ulrike Merten': "SPD",
                     'Sabine Jünger': "Die Linke.",
                     'Axel Fischer': "CDU",
                     'Sylvia Bonitz': "CDU",
                     'Monika Balt': "PDS",
                     'René Röspel': "SPD",
                     'Gudrun Serowiecki': "FDP",
                     'Melanie Oßwald': "CSU",
                     'Elvira Drobinski-Weiß': "SPD",
                     'Jutta Krüger-Jacob': "Bündnis 90/Die Grünen",
                     'Ursula von der Leyen': "CDU",
                     'Dorothée Menzner': "Die Linke.",
                     'Sevim Dağdelen': "Die Linke.",
                     'Lutz Heilmann': "Die Linke.",
                     'Andreas Lämmel': "CDU",
                     'Philipp Mißfelder': "CDU",
                     'Henning Otte': "CDU",
                     'Matthias Birkwald': "Die Linke.",
                     'Hans-Georg von der Marwitz': "CDU",
                     'Claudia Bögel': "FDP",
                     'Aydan Özoğuz': "SPD",
                     'Alexander Neu': "Die Linke.",
                     'Daniela De Ridder': "SPD",
                     'Charles M. Huber': "CDU",
                     'Philipp Graf von und zu Lerchenfeld': "CSU",
                     'Sylvia Jörrißen': "CDU",
                     'André Berghegger': "CDU",
                     'Alexander Graf Lambsdorff': "FDP",
                     'Amira Mohamed Ali': "Die Linke.",
                     'Olaf in der Beek': "FDP"}


polIds = NamesWPSpeeches['politicianID']
polNames = NamesWPSpeeches['name']

Parties = []
for i in range(len(polIds)):
    if np.isnan(polIds[i]) == False:
        if str(int(polIds[i])) in PolID2info.keys():
            if "PARTY" in PolID2info[str(int(polIds[i]))].keys():
                Parties.append(PolID2info[str(int(polIds[i]))]["PARTY"])
            elif type(polNames[i]) == str:
                Parties.append(MissingName2Party[polNames[i]])
            else:
                Parties.append(np.nan)
        elif type(polNames[i]) == str:
                Parties.append(MissingName2Party[polNames[i]])
        else:
            Parties.append(np.nan)
    elif type(polNames[i]) == str:
        Parties.append(MissingName2Party[polNames[i]])
    else:
        Parties.append(np.nan)

# save data
NamesWPSpeeches.to_csv(PATH + "./processed/NamesWPSpeeches_withParty.csv")

# group per year and save again
NamesWPSpeeches_later = NamesWPSpeeches[NamesWPSpeeches["year"] >= 2015].copy()

NamesWPSpeeches_grouped_WP = NamesWPSpeeches_later[['speech', 'WPs', 'wikidataid', 'novelty', 'transience',
       'resonance', 'politicianID', 'name']].groupby(["WPs", "wikidataid", "name", "politicianID"]).mean().reset_index()
NamesWPSpeeches_grouped_year = NamesWPSpeeches_later.groupby(["year", "wikidataid", "name"]).mean().reset_index()
NamesWPSpeeches_grouped_month_year = NamesWPSpeeches_later.groupby(["month", "year", "wikidataid", "name"]).mean().reset_index()
#NamesWPSpeeches_grouped_year_Party = NamesWPSpeeches.groupby(["year", "PARTY"]).mean().reset_index()


NamesWPSpeeches_grouped_WP.to_csv(PATH + "./processed/NamesWPSpeeches_grouped_by_IDNameWP_with_AfD_later.csv")
NamesWPSpeeches_grouped_year.to_csv(PATH + "./processed/NamesWPSpeeches_grouped_by_IDNameYear_with_AfD_later.csv")
#NamesWPSpeeches_grouped_year_Party.to_csv(PATH + "./processed/NamesWPSpeeches_grouped_by_YearPARTY_with_AfD.csv")
NamesWPSpeeches_grouped_month_year.to_csv(PATH + "./processed/NamesWPSpeeches_grouped_by_Month_Year_with_AfD_later.csv")


