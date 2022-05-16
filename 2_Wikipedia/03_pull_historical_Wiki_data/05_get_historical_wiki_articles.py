'''
# Script name: 05_get_historical_wiki_articles.py
# Purpose of script: scrape historical versions of Wikipedia articles
# Dependencies: 01_data_prep_for_historical_pagerank.py
# Author: Alexandra Rottenkolber
'''


# import packages
import pandas as pd
import glob
import requests

# set input and output directory
PATH = "./data_analysis/01_data/Wikipedia/output/"
out_dir = "./data_analysis/01_data/Wikipedia/output/politicians_DE_wikiarticles_history/politicians_DE_wikiarticles"

# import revision IDs that should be retrieved
history_revision_id = pd.read_csv(PATH +"history_one_rep_revision_per_year_ids_names_complete.csv").drop(columns = ["Unnamed: 0"])
year_ls = history_revision_id["year"]
basenames_ls = history_revision_id["wikiurl_basenames"]
revison_id_ls = history_revision_id["revision_id"]

# pull data
for i in range(len(revison_id_ls)):

    if i % 1000 == 0:
        print(f'processed {i} pages.')

    rev_id = revison_id_ls[i]
    basename = basenames_ls[i]
    year = year_ls[i]

    if len(glob.glob(out_dir + f"{year}/{basename}.html")) == 0:
        url = f'https://de.wikipedia.org/w/index.php?title={basename}&oldid={rev_id}'
        r = requests.get(url, allow_redirects=True)

        open(out_dir + f"{year}/{basename}.html", 'wb').write(r.content)