'''
------------------------------------------------------------------------
Script name: 04_1_fractions.py
Purpose of script:
Dependencies: 03_mps_from_base.py
Author: Alexandra Rottenkolber based on code from open-discourse (script 01_create_fractions.py)
Output: fractions.pkl
Date created: 05.03.2022
Date last modified:
------------------------------------------------------------------------
'''


import pandas as pd
import numpy as np
import os


PATH = "/Users/alexandrarottenkolber/Documents/02_Hertie_School/Master thesis/Master_Thesis_Hertie/data_analysis/01_data/Plenarprotokolle/"

# input directory
POLITICIANS_STAGE_01 = PATH + "output/02_politicians"
save_path = os.path.join(POLITICIANS_STAGE_01, "mps.pkl")

# output directory
FACTIONS_STAGE_01 = PATH + "output/03_fractions"
save_path_factions = os.path.join(FACTIONS_STAGE_01, "factions.pkl")


if not os.path.exists(FACTIONS_STAGE_01):
    os.makedirs(FACTIONS_STAGE_01)

# read data.
mps = pd.read_pickle(os.path.join(POLITICIANS_STAGE_01, "mps.pkl"))

factions = mps.institution_name.loc[(mps.institution_type == "Fraktion/Gruppe")]

unique_factions = np.unique(factions)
unique_factions = np.append(
    unique_factions,
    ["Südschleswigscher Wählerverband", "Gast", "Gruppe Nationale Rechte"],
)

unique_factions = pd.DataFrame(unique_factions, columns=["faction_name"])


unique_factions.to_pickle(save_path_factions)
