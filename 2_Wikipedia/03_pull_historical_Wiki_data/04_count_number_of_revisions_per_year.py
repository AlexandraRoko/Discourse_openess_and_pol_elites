"""
Script name: 04_count_number_of_revisions_per_year.py
Purpose of script: count number of revisions per month and year
Dependencies: 01_data_prep_for_historical_pagerank.py
Author: Alexandra Rottenkolber
"""


import pandas as pd
import re
from datetime import datetime

# input directory
PATH = "/Users/alexandrarottenkolber/Documents/02_Hertie_School/Master thesis/Master_Thesis_Hertie/data_analysis/01_data/Wikipedia/output/"

# read in data
data = pd.read_csv(PATH + "history_revisions_df_complete.csv")
MP_data = pd.read_csv(PATH + "MP_data.csv")


# clean text
def regex_for_special_chars(string):
    string = re.sub('%C3%A4', 'ä', string)
    string = re.sub('%C3%B6', 'ö', string)
    string = re.sub('%C3%BC', 'ü', string)
    string = re.sub('%E1%BA%9E', 'ß', string)
    string = re.sub('%C3%9F', 'ß', string)
    string = re.sub('%C3%A8', 'è', string)
    string = re.sub('%C3%96', 'Ö', string)
    string = re.sub('%C3%A9', 'é', string)
    string = re.sub('%C4%B1', 'ı', string)
    string = re.sub('%C4%9F', 'ğ', string)
    string = re.sub('%C5%A1', 'š', string)
    string = re.sub('%C4%87', 'ć', string)
    string = re.sub('%C3%A7', 'ç', string)
    string = re.sub('%C5%BB', 'Ż', string)

    string = re.split("/", string)[-1]
    string = re.sub('_', ' ', string)

    return string


MP_data["wikiurl_clean"] = MP_data["wikiurl"].map(
    lambda x: regex_for_special_chars(x))  # wikiurl_clean in MP_data and data["title"] match

data = data[["revid", "user", "timestamp", "title"]].copy()
data["year"] = data["timestamp"].map(lambda x: datetime.strptime(x, '%Y-%m-%dT%H:%M:%SZ').year)
data["month"] = data["timestamp"].map(lambda x: datetime.strptime(x, '%Y-%m-%dT%H:%M:%SZ').month)
data["day"] = data["timestamp"].map(lambda x: datetime.strptime(x, '%Y-%m-%dT%H:%M:%SZ').day)
data["date"] = data["timestamp"].map(lambda x: datetime.strptime(x, '%Y-%m-%dT%H:%M:%SZ'))

# monthly aggregated
counted_per_month = data.groupby(by=["year", "month", "title"]).nunique().drop(columns=["day", "date", "timestamp"])
counted_per_month = counted_per_month.reset_index()
counted_per_month = counted_per_month.rename(columns={"user": "unique_users", "revid": "no_of_revisions"})
# count revisions
counted_per_month['cum_sum_revisions'] = counted_per_month[["title", "no_of_revisions"]].groupby(by=["title"]).cumsum()
counted_per_month['cum_sum_users'] = counted_per_month[["title", "unique_users"]].groupby(by=["title"]).cumsum()

# yearly aggregated
counted_per_year = data.groupby(by=["year", "title"]).nunique().drop(columns=["day", "month", "date", "timestamp"])
counted_per_year = counted_per_year.reset_index()
counted_per_year = counted_per_year.rename(columns={"user": "unique_users", "revid": "no_of_revisions"})
# count revisions
counted_per_year['cum_sum_revisions'] = counted_per_year[["title", "no_of_revisions"]].groupby(by=["title"]).cumsum()
counted_per_year['cum_sum_users'] = counted_per_year[["title", "unique_users"]].groupby(by=["title"]).cumsum()

# get keys for merging
keys_df = MP_data[["wikidataid", "wikiurl_clean"]].copy()

# generate data to export
result_counts_per_year = pd.merge(counted_per_year, keys_df, left_on="title", right_on="wikiurl_clean").drop(
    columns=["wikiurl_clean"])
result_counts_per_month = pd.merge(counted_per_month, keys_df, left_on="title", right_on="wikiurl_clean").drop(
    columns=["wikiurl_clean"])

# export data
result_counts_per_month.to_csv(
    "./data_analysis/01_data/Wikipedia/output/history_norevisions_nousers_counted_per_month.csv")
result_counts_per_year.to_csv(
    "./data_analysis/01_data/Wikipedia/output/history_norevisions_nousers_counted_per_year.csv")
