





import pandas as pd
import os

PATH = "/Users/alexandrarottenkolber/Documents/02_Hertie_School/Master thesis/Master_Thesis_Hertie/data_analysis/01_data/Plenarprotokolle/"


# input directory
POLITICIANS_INPUT = PATH + "output/02_politicians/stage_1"
FACTIONS_INPUT = PATH + "output/XX_finals"

# output directory
POLITICIANS_OUTPUT = PATH + "output/02_politicians/stage_2"

if not os.path.exists(POLITICIANS_OUTPUT):
    os.makedirs(POLITICIANS_OUTPUT)

factions = pd.read_pickle(os.path.join(FACTIONS_INPUT, "factions.pkl"))
mps = pd.read_pickle(os.path.join(POLITICIANS_INPUT, "mps.pkl"))

mps.insert(2, "faction_id", -1)

for faction_name, faction_id in zip(factions.faction_name, factions.id):
    mps.faction_id.loc[mps.institution_name == faction_name] = faction_id

mps.to_pickle(os.path.join(POLITICIANS_OUTPUT, "mps.pkl"))
