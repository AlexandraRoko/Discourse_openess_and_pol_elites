"""
Script name: 02_get_language_edition_history_wikidata.py
Purpose of script: get history of language editions
Dependencies: ./2_Wikipedia/01_get_data_from_CLD_and_Wikidata.R
Author: Alexandra Rottenkolber
"""



# import relevant packages
import pandas as pd
import requests
import regex as re
from datetime import datetime
from bs4 import BeautifulSoup
import urllib.request

# set path, import data
PATH = "./data_analysis/01_data/Wikipedia/output/"
MP_data = pd.read_csv(PATH + "MP_data.csv")

# list of language editions
lang_eds = ["af",
            "als",
            "am",
            "an",
            "ar",
            "ary",
            "arz",
            "ast",
            "ay",
            "azb",
            "az",
            "bar",
            "ba",
            "be",
            "bg",
            "bh",
            "bi",
            "bn",
            "bo",
            "br",
            "bs",
            "ca",
            "ce",
            "ckb",
            "cs",
            "cv",
            "cy",
            "da",
            "de",
            "diq",
            "dsb",
            "dty",
            "el",
            "en",
            "eo",
            "es",
            "et",
            "eu",
            "ext",
            "fa",
            "fi",
            "fo",
            "frp",
            "frr",
            "fr",
            "fy",
            "ga",
            "gd",
            "gl",
            "gn",
            "gv",
            "hak",
            "ha",
            "he",
            "hi",
            "hr",
            "hsb",
            "hu",
            "hy",
            "ia",
            "id",
            "ie",
            "ig",
            "ilo",
            "io",
            "is",
            "ja",
            "jv",
            "ka",
            "kbp",
            "kk",
            "kn",
            "koi",
            "ko",
            "ku",
            "kw",
            "ky",
            "lad",
            "la",
            "lb",
            "lfn",
            "li",
            "lmo",
            "lo",
            "lt",
            "lv",
            "main",
            "mg",
            "mhr",
            "mk",
            "ml",
            "mn",
            "mr",
            "ms",
            "mt",
            "my",
            "mzn",
            "nds",
            "ne",
            "nia",
            "nl",
            "nn",
            "nov",
            "no",
            "oc",
            "or",
            "pag",
            "pap",
            "pa",
            "pdc",
            "pih",
            "pl",
            "pms",
            "pnb",
            "ps",
            "pt",
            "qu",
            "ro",
            "ru",
            "sah",
            "scn",
            "sco",
            "se",
            "sh",
            "simple",
            "sk",
            "el",
            "so",
            "sq",
            "ssr",
            "stq",
            "sv",
            "sw",
            "szl",
            "ta",
            "tet",
            "te",
            "tg",
            "th",
            "tl",
            "tr",
            "tt",
            "ty",
            "uk",
            "ur",
            "uz",
            "vec",
            "vi",
            "vo",
            "war",
            "wa",
            "wuu",
            "yi",
            "yo",
            "zea",
            "zh_min_nan",
            "zh_yue",
            "zh"]

wikidata_ids = list(MP_data["wikidataid"])
id2name = MP_data[["wikidataid", "wikititle", "name"]].drop_duplicates().set_index("wikidataid").to_dict(orient='dict')

wikidataids = []
url_lang_eds = []
lang_eds = []

for id_ in wikidata_ids:

    url = "https://www.wikidata.org/wiki/" + str(id_)

    html = urllib.request.urlopen(url).read()
    soup = BeautifulSoup(html, 'html.parser')

    div_uls = soup.find_all("div", {"class": "wikibase-sitelinklistview"})[0].find_all("ul", {
        "class": "wikibase-sitelinklistview-listview"})

    div_uls_str = str(div_uls[0])
    div_uls_ls = div_uls_str.split("</span></span>")
    # div_uls_ls = [item.replace("/span>", "").replace("<span", "").replace("<a", "").replace("a>", "").replace("<", "").replace(">", "").replace("li", "").replace("ul", "").replace("\n", "") for item in div_uls_ls]

    r'(?:(?:https?|ftp):\/\/)?[\w/\-?=%.,]+\.[\w/\-&?=%().,]+'

    # urls = [re.findall(r'(?:(?:https?|ftp):\/\/)?[\w/\-?=%.,]+\.[\w/\-&?=%.,]+', entry) for entry in div_uls_ls]
    urls = [re.findall(r'(?:(?:https?|ftp):\/\/)?[\w/\-?=%.,]+\.[\w/\-&?=%().,]+', entry) for entry in div_uls_ls]
    urls = [url for url in urls if len(url) > 0]  # remove empty detection
    lang_ed = [url[0].split(".")[0].split("//")[1] for url in urls]

    for url in urls:
        wikidataids.append(id_)
        url_lang_eds.append(url[0])
        lang_eds.append(url[0].split(".")[0].split("//")[1])

results = pd.DataFrame()
results["wikidataid"] = wikidataids
results["url_lang_ed"] = url_lang_eds
results["lang_ed"] = lang_eds
results["name"] = results["wikidataid"].map(id2name["name"])
results["wikititle"] = results["wikidataid"].map(id2name["wikititle"])

# map creation date of page
Ses = requests.Session()

wikidataids = list(results["wikidataid"])
url_lang_eds = list(results["url_lang_ed"])
lang_eds = list(results["lang_ed"])
names = list(results["name"])
wikititles = list(results["wikititle"])


# function to get creation date of different language pages
def get_creation_date_lang_ed(wikidataids, url_lang_eds, lang_eds):
    wikiID = []
    date_of_creation = []
    language = []
    titles = []
    problem_id_ls = []
    problem_url_ls = []

    for i in range(len(url_lang_eds)):

        if i % 1000 == 0:
            print(f'processed {i} pages.')

        lang = lang_eds[i]
        title = urllib.parse.unquote(url_lang_eds[i].split("/")[-1])
        # print(i, lang, title)

        URL = f"https://{lang}.wikipedia.org/w/api.php"

        PARAMS = {
            "action": "query",
            "prop": "revisions",
            "titles": f"{title}",
            "rvlimit": "1",
            "rvprop": "timestamp",
            "rvdir": "newer",
            "format": "json"
        }

        R = Ses.get(url=URL, params=PARAMS)

        try:
            history_data = R.json()

            pageid = list(history_data["query"]["pages"].keys())[0]
            creation_date = history_data['query']["pages"][f"{pageid}"]["revisions"][0]["timestamp"]

            wikiID.append(wikidataids[i])
            date_of_creation.append(creation_date)
            language.append(lang)
            titles.append(title)

        except KeyError:
            print(url_lang_eds[i])
            problem_id_ls.append(wikidataids[i])
            problem_url_ls.append(url_lang_eds[i])

    # everything into one dataframe
    creation_date_df = pd.DataFrame()
    creation_date_df['wikidataid'] = wikiID
    creation_date_df['creation_date_lang'] = date_of_creation
    creation_date_df['language'] = language
    creation_date_df['wikititle'] = titles

    creation_date_df['creation_year_lang'] = creation_date_df['creation_date_lang'].map(
        lambda x: datetime.strptime(x, "%Y-%m-%dT%H:%M:%SZ").year)
    creation_date_df['creation_month_lang'] = creation_date_df['creation_date_lang'].map(
        lambda x: datetime.strptime(x, "%Y-%m-%dT%H:%M:%SZ").month)
    creation_date_df['creation_day_lang'] = creation_date_df['creation_date_lang'].map(
        lambda x: datetime.strptime(x, "%Y-%m-%dT%H:%M:%SZ").day)

    problem_df = pd.DataFrame()
    problem_df['prob_id'] = problem_id_ls
    problem_df['prob_url'] = problem_url_ls

    return creation_date_df, problem_df


# run function
creation_date_df, problem_df = get_creation_date_lang_ed(wikidataids, url_lang_eds, lang_eds)

# export results
creation_date_df.to_csv("./data_analysis/01_data/Wikipedia/output/history_lang_eds_df_complete.csv")
problem_df.to_csv("./data_analysis/01_data/Wikipedia/output/history_lang_eds_problem_df.csv")
results.to_csv("./data_analysis/01_data/Wikipedia/output/lang_urls_per_MP.csv")
