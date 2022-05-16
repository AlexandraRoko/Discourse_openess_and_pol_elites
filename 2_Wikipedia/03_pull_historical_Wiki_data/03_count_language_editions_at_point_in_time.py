"""
Script name: 03_count_language_editions_at_point_in_time.py
Purpose of script: count language editions per month and year
Dependencies: 02_get_language_edition_history_wikidata.py
Author: Alexandra Rottenkolber
"""


import pandas as pd

# read in data
creation_date_df = pd.read_csv("./data_analysis/01_data/Wikipedia/output/history_lang_eds_df_complete.csv").drop(columns = ["Unnamed: 0"])

# preparation to count language editions across time
years = list((creation_date_df["creation_year_lang"]).unique())
months = list((creation_date_df["creation_month_lang"]).unique())
days = list((creation_date_df["creation_day_lang"]).unique())
IDs = list((creation_date_df["wikidataid"]).unique())

ID_ls = []
years_ls = []
no_lan_eds_ls = []
languages_ls = []

for ID in IDs:
    years = list(creation_date_df[creation_date_df["wikidataid"] == ID]["creation_year_lang"])
    for year in years:
        no_lang_eds = len(
            creation_date_df[(creation_date_df["wikidataid"] == ID) & (creation_date_df["creation_year_lang"] <= year)])
        languages = list(
            creation_date_df[(creation_date_df["wikidataid"] == ID) & (creation_date_df["creation_year_lang"] <= year)][
                "language"].unique())

        ID_ls.append(ID)
        years_ls.append(year)
        no_lan_eds_ls.append(no_lang_eds)
        languages_ls.append(languages)

res_df = pd.DataFrame()
res_df["wikidataid"] = ID_ls
res_df["year"] = years_ls
res_df["no_lang_eds"] = no_lan_eds_ls
res_df["languages"] = languages_ls

per_year = creation_date_df.groupby(by = ["wikidataid", "creation_year_lang"]).nunique().drop(columns = ["creation_month_lang", "creation_day_lang", "wikititle", "creation_date_lang"]).reset_index()
per_year = per_year.rename(columns = {"language" : "no_languages_created", "creation_year_lang" : "year"})
per_year.head(2)

years = sorted(
    [2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020,
     2021])

WIKIDATAID = []
YEAR = []
NO_LANG_CREATED = []
CUM_SUM_LANG = []

for ID in set(per_year["wikidataid"]):
    years_ID = sorted(per_year["year"][per_year["wikidataid"] == ID])
    for year in years:
        if year in years_ID:

            WIKIDATAID.append(ID)
            YEAR.append(year)
            NO_LANG_CREATED.append(
                list(per_year["no_languages_created"][(per_year["wikidataid"] == ID) & (per_year["year"] == year)])[0])
            CUM_SUM_LANG.append(
                list(per_year["cum_sum_no_languages"][(per_year["wikidataid"] == ID) & (per_year["year"] == year)])[0])

        elif year not in years_ID:
            temp_years = sorted(per_year["year"][per_year["wikidataid"] == ID])
            temp_years.append(year)
            temp_years = sorted(temp_years)
            idx_ = temp_years.index(year)

            if idx_ >= 1:
                cum_sum_lang = list(per_year["cum_sum_no_languages"][
                                        (per_year["wikidataid"] == ID) & (per_year["year"] == temp_years[idx_ - 1])])[0]
            else:
                cum_sum_lang = 0

            WIKIDATAID.append(ID)
            YEAR.append(year)
            NO_LANG_CREATED.append(0)
            CUM_SUM_LANG.append(cum_sum_lang)

        else:
            print(ID, year)


full_results_per_year = pd.DataFrame()
full_results_per_year["wikidataid"] = WIKIDATAID
full_results_per_year["year"] = YEAR
full_results_per_year["no_languages_created"] = NO_LANG_CREATED
full_results_per_year["cum_sum_no_languages"] = CUM_SUM_LANG

full_results_per_year.to_csv("./data_analysis/01_data/Wikipedia/output/history_nolanguages_counted_all_years.csv")

