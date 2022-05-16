"""
Script name: 01_data_prep_for_historical_pagerank.py
Purpose of script: scrape historical versions of Wikipedia articles
Dependencies: ./2_Wikipedia/01_get_data_from_CLD_and_Wikidata.R
Author: Alexandra Rottenkolber
"""



# import relevant packages
import requests
import pandas as pd
import datetime as dt
import numpy as np
import regex as re
from datetime import datetime
import random

# set path and load data
PATH = "./data_analysis/01_data/Wikipedia/output/"
MP_data = pd.read_csv(PATH + "MP_data.csv")


# function to clean url
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


MP_data["wikiurl_clean"] = MP_data["wikiurl"].map(lambda x: regex_for_special_chars(x))

# start request session to pull revision meta data
Ses = requests.Session()
URL = "https://de.wikipedia.org/w/api.php"
titles = list(MP_data["wikiurl_clean"])

# function that queries the Wikipedia page history and creates dataframe for plotting a histogram
# get date of today to specify start date
now = dt.datetime.utcnow()

# bring date in the right format
time_today = now.strftime("%Y-%m-%dT%H:%M:%SZ")


def extract_revisions_to_df(title, number_of_revisions=500, startdate=f"{time_today}"):
    """A function that quieries the Wiki page history and creates a list of dataframes dataframe for plotting a histogram

    Arguments:
        title: title of the Wikipedia Page
        number_of_revisions: how many revisions will be extraced. Number must be between 0 and 500. Insert as integer.
        startdate: offset in format 2020-04-06T14:32:32Z

    Returns:
        - A dataframe with the number of revisions for a certain wikipedia page.
        - The date of the earliest revision
    """

    RVLIMIT = str(number_of_revisions)
    RVSTART = str(startdate)
    PARAMS = {
        "action": "query",
        "format": "json",
        "prop": "revisions",
        "rvlimit": f"{RVLIMIT}",
        "srprop": "size|wordcount|timestamp|snippet|titlesnippet",
        "titles": f"{title}",
        "rvslots": "main",
        "rvdir": "older",
        "rvstart": f"{RVSTART}"
    }

    R = Ses.get(url=URL, params=PARAMS)

    history_data = R.json()

    pageid = list(history_data["query"]["pages"].keys())[0]

    try:

        revision_df = pd.json_normalize(history_data["query"]["pages"][f"{pageid}"]["revisions"])

        revision_df["time_edited"] = revision_df["timestamp"].map(
            lambda x: dt.datetime.strptime(x, '%Y-%m-%dT%H:%M:%SZ'))
        revision_df["date"] = revision_df["timestamp"].map(lambda x: x[:10]).map(
            lambda x: dt.datetime.strptime(x, '%Y-%m-%d'))
        revision_df["title"] = f"{title}"
        # earliest_revision = list(revision_df["timestamp"])[-1]
        # last_revision = list(revision_df["timestamp"])[0]

        date_string = list(revision_df["timestamp"])[-1]
        a = dt.datetime.strptime(date_string, '%Y-%m-%dT%H:%M:%SZ')
        b = a + dt.timedelta(seconds=60)  # add five seconds for offset to not count the earliest revision twice
        offset_string = b.strftime("%Y-%m-%dT%H:%M:%SZ")

        # print(f'''\tCovering the time period from {earliest_revision} to {last_revision}.\n''')
        # If earlier revisions are of interest, use {date_string} as offset for the next query.\n''')

        return revision_df, offset_string

    except:
        print(f'''=> Problem occured for {title}. Adding empty dataframe. \n\n''')
        ls = pd.DataFrame([])
        earliest_revision = np.nan

        return ls, earliest_revision


# create function that qeries several pages at once and returns dataframe and a dictionary with the date of the first revision

def combine_different_page_histories_to_one_df(list_of_page_titels, offsets_dic):
    """A function that quieries the Wiki page history and creates a list of dataframes dataframe for plotting a histogram

    Arguments:
        list_of_page_titels: a list of Wikipedia page titles
        offsets_dic: a dictiornary with the dates where to start the query of revisions

    Returns:
        - A merged data frame, containing the revision history for the wikipida pages mentioned in list_of_page_titels
        - A dictionary containing the titele and date of the last revision which can be used to request ealier revisions
    """

    ls_of_dfs = []
    offset_dic = {}
    for title in list_of_page_titels:
        # print("Processed: \t"+title)
        temporal_df, earliest_revision = extract_revisions_to_df(title, startdate=offsets_dic[title])
        ls_of_dfs.append(temporal_df)
        offset_dic[title] = earliest_revision

    temp_merged_df = pd.concat(ls_of_dfs)
    merged_df = temp_merged_df.reset_index()

    return merged_df, offset_dic


# specify start dates
start_dates_dic = {}
for title in titles:
    start_dates_dic[title] = f"{time_today}"

# run request
result_1, offset_dic_1 = combine_different_page_histories_to_one_df(titles, start_dates_dic)
result_2, offset_dic_2 = combine_different_page_histories_to_one_df([titles[i] for i in range(len(titles))],
                                                                    offset_dic_1)
result_3, offset_dic_3 = combine_different_page_histories_to_one_df([titles[i] for i in range(len(titles))],
                                                                    offset_dic_2)
result_4, offset_dic_4 = combine_different_page_histories_to_one_df([titles[i] for i in range(len(titles))],
                                                                    offset_dic_3)
result_5, offset_dic_5 = combine_different_page_histories_to_one_df([titles[i] for i in range(len(titles))],
                                                                    offset_dic_4)
result_6, offset_dic_6 = combine_different_page_histories_to_one_df([titles[i] for i in range(len(titles))],
                                                                    offset_dic_5)
result_7, offset_dic_7 = combine_different_page_histories_to_one_df([titles[i] for i in range(len(titles))],
                                                                    offset_dic_6)
result_8, offset_dic_8 = combine_different_page_histories_to_one_df([titles[i] for i in range(len(titles))],
                                                                    offset_dic_7)
result_9, offset_dic_9 = combine_different_page_histories_to_one_df([titles[i] for i in range(len(titles))],
                                                                    offset_dic_8)
result_10, offset_dic_10 = combine_different_page_histories_to_one_df([titles[i] for i in range(len(titles))],
                                                                      offset_dic_9)
result_11, offset_dic_11 = combine_different_page_histories_to_one_df([titles[i] for i in range(len(titles))],
                                                                      offset_dic_10)
result_12, offset_dic_12 = combine_different_page_histories_to_one_df([titles[i] for i in range(len(titles))],
                                                                      offset_dic_11)
result_13, offset_dic_13 = combine_different_page_histories_to_one_df([titles[i] for i in range(len(titles))],
                                                                      offset_dic_12)
result_14, offset_dic_14 = combine_different_page_histories_to_one_df([titles[i] for i in range(len(titles))],
                                                                      offset_dic_13)
result_15, offset_dic_15 = combine_different_page_histories_to_one_df([titles[i] for i in range(len(titles))],
                                                                      offset_dic_14)
result_16, offset_dic_16 = combine_different_page_histories_to_one_df([titles[i] for i in range(len(titles))],
                                                                      offset_dic_15)
result_17, offset_dic_17 = combine_different_page_histories_to_one_df([titles[i] for i in range(len(titles))],
                                                                      offset_dic_16)
result_18, offset_dic_18 = combine_different_page_histories_to_one_df([titles[i] for i in range(len(titles))],
                                                                      offset_dic_17)

# concatenate the resulting dataframes to one
revisions_df = pd.concat([result_1, result_2, result_3, result_4, result_5, result_6,
                          result_7, result_8, result_9, result_10, result_11, result_12,
                          result_13, result_14, result_15, result_16, result_17, result_18]).reset_index()

# reshape dataframe
revisions_count = revisions_df.groupby(["title", "date"]).count().sort_values(by=['date']).reset_index()
revisions_count = revisions_count.drop(columns=["level_0", "timestamp", "time_edited"]).rename(
    columns={"index": "no_of_revisions"}).copy()

# save data
revisions_df.to_csv("/Users/alexandrarottenkolber/Documents/02_Hertie_School/Master thesis/Master_Thesis_Hertie/data_analysis/01_data/Wikipedia/output/history_revisions_df_complete.csv")

# import data
revisions_df = pd.read_csv(
    "/Users/alexandrarottenkolber/Documents/02_Hertie_School/Master thesis/Master_Thesis_Hertie/data_analysis/01_data/Wikipedia/output/history_revisions_df_complete.csv")

revisions_df["year"] = revisions_df["timestamp"].map(lambda x: datetime.strptime(x, "%Y-%m-%dT%H:%M:%SZ").year)
revisions_df["month"] = revisions_df["timestamp"].map(lambda x: datetime.strptime(x, "%Y-%m-%dT%H:%M:%SZ").month)
revisions_df_clean = revisions_df[
    ["revid", "parentid", "user", "timestamp", "comment", "time_edited", "date", "title", "year", "month"]].copy()

# reverse sting cleaning to merge data
def regex_for_special_chars_reversed(string):
    string = re.sub('ä', '%C3%A4', string)
    string = re.sub('ö', '%C3%B6', string)
    string = re.sub('ü', '%C3%BC', string)
    string = re.sub('ß', '%E1%BA%9E', string)
    string = re.sub('ß', '%C3%9F', string)
    string = re.sub('è', '%C3%A8', string)
    string = re.sub('Ö', '%C3%96', string)
    string = re.sub('é', '%C3%A9', string)
    string = re.sub('ı', '%C4%B1', string)
    string = re.sub('ğ', '%C4%9F', string)
    string = re.sub('š', '%C5%A1', string)
    string = re.sub('ć', '%C4%87', string)
    string = re.sub('ç', '%C3%A7', string)
    string = re.sub('Ż', '%C5%BB', string)

    string = re.split("/", string)[-1]
    string = re.sub(' ', '_', string)

    return string

# apply string cleaning reversed
revisions_df_clean["wikiurl_basename"] = revisions_df_clean["title"].map(lambda x: regex_for_special_chars_reversed(x))

# generate final dataframe
rev_ids = []
MP_basenames = []
years = []

# select one revision mid-year for each year which will be pulled for further analysis
for MP in list(revisions_df_clean["wikiurl_basename"].unique()):
    year_ls = sorted(
        [2021, 2020, 2019, 2018, 2017, 2016, 2015, 2014, 2013, 2012, 2011, 2010, 2009, 2008, 2007, 2006, 2005, 2004,
         2003, 2002, 2001])
    for year in year_ls:
        if year in list(revisions_df_clean["year"][revisions_df_clean["wikiurl_basename"] == MP].unique()):
            try:
                revision_id = random.choice(list(revisions_df_clean[((revisions_df_clean["month"] == 6) | (
                        revisions_df_clean["month"] == 7) | (revisions_df_clean["month"] == 8)) &
                                                                    (revisions_df_clean["wikiurl_basename"] == MP) &
                                                                    (revisions_df_clean["year"] == year)]["revid"]))
                wikiurl = revisions_df_clean["wikiurl_basename"][revisions_df_clean["revid"] == revision_id].unique()[0]

                rev_ids.append(revision_id)
                MP_basenames.append(wikiurl)
                years.append(year)

            except IndexError:
                # print(MP, year)
                revision_id = random.choice(list(revisions_df_clean[(revisions_df_clean["wikiurl_basename"] == MP) &
                                                                    (revisions_df_clean["year"] == year)]["revid"]))
                wikiurl = revisions_df_clean["wikiurl_basename"][revisions_df_clean["revid"] == revision_id].unique()[0]

                rev_ids.append(revision_id)
                MP_basenames.append(wikiurl)
                years.append(year)
        else:
            year_ls_temp = list(revisions_df_clean["year"][revisions_df_clean["wikiurl_basename"] == MP].unique())
            year_ls_temp.append(year)
            year_ls_temp = sorted(year_ls_temp)

            idx = year_ls_temp.index(year)

            if idx > 1:
                year = year_ls_temp[idx - 1]

                try:
                    revision_id = random.choice(list(revisions_df_clean[((revisions_df_clean["month"] == 6) | (
                            revisions_df_clean["month"] == 7) | (revisions_df_clean["month"] == 8)) &
                                                                        (revisions_df_clean["wikiurl_basename"] == MP) &
                                                                        (revisions_df_clean["year"] == year)]["revid"]))
                    wikiurl = \
                        revisions_df_clean["wikiurl_basename"][revisions_df_clean["revid"] == revision_id].unique()[0]

                    rev_ids.append(revision_id)
                    MP_basenames.append(wikiurl)
                    years.append(year_ls_temp[idx - 1 + 1])

                except IndexError:
                    revision_id = random.choice(list(revisions_df_clean[(revisions_df_clean["wikiurl_basename"] == MP) &
                                                                        (revisions_df_clean["year"] == year)]["revid"]))
                    wikiurl = \
                        revisions_df_clean["wikiurl_basename"][revisions_df_clean["revid"] == revision_id].unique()[0]

                    rev_ids.append(revision_id)
                    MP_basenames.append(wikiurl)
                    years.append(year_ls_temp[idx - 1 + 1])

data = pd.DataFrame()

data["revision_id"] = rev_ids
data["wikiurl_basenames"] = MP_basenames
data["year"] = years

# one representative revision per year, save data
data.to_csv(
    "./data_analysis/01_data/Wikipedia/output/history_one_rep_revision_per_year_ids_names_complete.csv")
