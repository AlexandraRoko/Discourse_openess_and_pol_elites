'''
------------------------------------------------------------------------
Script name: 06_2_match_name_to_speech.py
Purpose of script: clean text in speeches
Dependencies: 02_extract_speeches_from_text.py, 03_fractions.py
Author: Alexandra Rottenkolber based on code from open-discourse (script 02_clean_speeches.py)  (https://github.com/open-discourse/open-discourse)
------------------------------------------------------------------------
'''


import pandas as pd
import regex
import os

from helper_functions import insert_politician_id_into_speech_content


PATH = "/Users/alexandrarottenkolber/Documents/02_Hertie_School/Master thesis/Master_Thesis_Hertie/data_analysis/01_data/Plenarprotokolle/"

# input directory
SPEECH_CONTENT_INPUT = PATH + "output/01_speeches/stage_2"
DATA_FINAL = PATH + "output/XX_finals" # politicians_csv pulled form open_discourse database

# output directory
SPEECH_CONTENT_OUTPUT = PATH + "output/01_speeches/stage_3"



if not os.path.exists(SPEECH_CONTENT_OUTPUT):
    os.makedirs(SPEECH_CONTENT_OUTPUT)

# MDBS
#politicians = pd.read_csv(os.path.join(DATA_FINAL, "politicians.csv"))
politicians = pd.read_pickle(os.path.join(PATH, "output/02_politicians/stage_1/mps.pkl"))
politicians = politicians[[
        "ui",
        "electoral_term",
        #"faction_id",
        "first_name",
        "last_name",
        "gender",
        "profession",
        "constituency",
        "institution_type",
    ]].copy()

politicians = politicians.astype(dtype={"ui": "int64"})

# Some cleaning to make matching easier.
politicians.constituency = politicians.constituency.fillna("")

politicians.first_name = politicians.first_name.str.lower()
politicians.last_name = politicians.last_name.str.lower()
politicians.constituency = politicians.constituency.str.lower()

politicians.first_name = politicians.first_name.str.replace("ß", "ss", regex=False)
politicians.last_name = politicians.last_name.str.replace("ß", "ss", regex=False)

politicians.first_name = politicians.first_name.apply(str.split)

politicians.profession = politicians.profession.str.lower()

# iterate over all electoral_term_folders __________________________________________________
for electoral_term_folder in sorted(os.listdir(SPEECH_CONTENT_INPUT)):
    working = []
    if electoral_term_folder == ".DS_Store":
        continue
    electoral_term_folder_path = os.path.join(
        SPEECH_CONTENT_INPUT, electoral_term_folder
    )

    print(electoral_term_folder)

    save_path = os.path.join(SPEECH_CONTENT_OUTPUT, electoral_term_folder)
    if not os.path.exists(save_path):
        os.makedirs(save_path)

    electoral_term = int(regex.sub("-data", "", regex.sub("pp", "", electoral_term_folder)))

    # Only select politicians of the election period.
    politicians_electoral_term = politicians.loc[
        politicians.electoral_term == electoral_term
    ]
    mgs_electoral_term = politicians_electoral_term.loc[
        politicians_electoral_term.institution_type == "Regierungsmitglied"
    ]

    # iterate over every speech_content file
    for speech_content_file in sorted(os.listdir(electoral_term_folder_path)):

        # check if session file is a pickle file
        if ".pkl" not in speech_content_file:
            continue
        filepath = os.path.join(electoral_term_folder_path, speech_content_file)

        print(speech_content_file)

        # read the spoken content pickle file
        speech_content = pd.read_pickle(filepath)

        speech_content_matched, _ = insert_politician_id_into_speech_content(
            speech_content, politicians_electoral_term, mgs_electoral_term, politicians
        )

        speech_content_matched.to_pickle(os.path.join(save_path, speech_content_file))
