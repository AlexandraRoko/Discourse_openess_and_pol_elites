'''
------------------------------------------------------------------------
Script name: helper_functions.py
Author: Alexandra Rottenkolber based on code from open-discourse (https://github.com/open-discourse/open-discourse)
------------------------------------------------------------------------
'''

import numpy as np
import regex


def clean(filetext, remove_pdf_header=True):
    # Replaces all the misrecognized characters
    filetext = filetext.replace(r"", "-")
    filetext = filetext.replace(r"", "-")
    filetext = filetext.replace("—", "-")
    filetext = filetext.replace("–", "-")
    filetext = filetext.replace("•", "")
    filetext = regex.sub(r"\t+", " ", filetext)
    filetext = regex.sub(r"  +", " ", filetext)

    # Remove pdf artifact
    if remove_pdf_header:
        filetext = regex.sub(
            r"(?:Deutscher\s?Bundestag\s?-(?:\s?\d{1,2}\s?[,.]\s?Wahlperiode\s?-)?)?\s?\d{1,3}\s?[,.]\s?Sitzung\s?[,.]\s?(?:(?:Bonn|Berlin)[,.])?\s?[^,.]+,\s?den\s?\d{1,2}\s?[,.]\s?[^\d]+\d{4}.*",  # noqa: E501
            r"\n",
            filetext,
        )
        filetext = regex.sub(r"\s*(\(A\)|\(B\)|\(C\)|\(D\))", "", filetext)

    # Remove delimeter
    filetext = regex.sub(r"-\n+(?![^(]*\))", "", filetext)

    # Deletes all the newlines in brackets
    bracket_text = regex.finditer(r"\(([^(\)]*(\(([^(\)]*)\))*[^(\)]*)\)", filetext)

    for bracket in bracket_text:
        filetext = filetext.replace(
            str(bracket.group()),
            regex.sub(
                r"\n+",
                " ",
                regex.sub(
                    r"(^((?<!Abg\.).)+|^.*\[.+)(-\n+)",
                    r"\1",
                    str(bracket.group()),
                    flags=regex.MULTILINE,
                ),
            ),
        )
    return filetext


def clean_name_headers(filetext, names, contributions_extended_filter=False):
    """Cleans lines a given text which remained from the pdf header.
    Usually something like: "Präsident Dr. Lammert"
    Keep in mind this also deletes lines from voting lists.
    """
    names = np.unique(names)
    if contributions_extended_filter:
        for counter, name in enumerate(names):
            names[counter] = regex.sub(r"[()\[\]\{\}]", "", name)

    names_to_clean = "(" + "|".join(np.unique(names)) + ")"
    names_to_clean = regex.sub(r"\+", "\\+", names_to_clean)
    names_to_clean = regex.sub(r"\*", "\\*", names_to_clean)
    names_to_clean = regex.sub(r"\?", "\\?", names_to_clean)
    pattern = (
        r"\n((?:Parl\s?\.\s)?Staatssekretär(?:in)?|Bundeskanzler(?:in)?|Bundesminister(?:in)?|Staatsminister(:?in)?)?\s?"  # noqa: E501
        + names_to_clean
        + r" *\n"
    )
    filetext = regex.sub(pattern, "\n", filetext)

    pattern = r"\n\d+ *\n"

    filetext = regex.sub(pattern, "\n", filetext)

    return filetext



'''
helper functions for speeches
'''

from fuzzywuzzy import fuzz
import numpy as np
import pandas as pd
import regex


# Note: This matching script is a total mess, I know. But it works quite fine and has
# some optimization logic already included. Would still be nice to clean this up
# a little together with the preceeding scripts.


def get_fuzzy_names(df, name_to_check, fuzzy_threshold=70):
    return df.loc[
        df.last_name.apply(fuzz.ratio, args=[name_to_check]) >= fuzzy_threshold
    ]


def get_possible_matches(df, **columns):
    """Returns possible matches in df with respect to specified columns."""

    for col_name, col_value in columns.items():
        df = df.loc[df[col_name] == col_value]

    return df


def check_unique(possible_matches, col="ui"):
    return len(np.unique(possible_matches[col])) == 1


def set_id(df, index, possible_matches, col_set, col_check):
    """Sets the ID in column "col_set" of "df" at "index" to the value in
    "col_check" in possible_matches. Expects a unique col_check value in
    possible_matches.
    """
    df[col_set].at[index] = int(possible_matches[col_check].iloc[0])


def set_value(df, index, col, value):
    """Sets the value of col in df based on given value."""
    df[col].at[index] = value


def check_last_name(df, index, possible_matches, last_name):
    # Get possible matches according to last name.
    possible_matches = get_possible_matches(possible_matches, last_name=last_name)

    if check_unique(possible_matches):
        set_id(df, index, possible_matches, col_set="politician_id", col_check="ui")
        return True, possible_matches
    else:
        return False, possible_matches


def check_first_name(df, index, possible_matches, first_name):
    first_name_set = set(first_name)

    possible_matches = possible_matches.loc[
        ~possible_matches.first_name.apply(lambda x: set(x).isdisjoint(first_name_set))
    ]

    if check_unique(possible_matches):
        set_id(df, index, possible_matches, col_set="politician_id", col_check="ui")
        return True, possible_matches
    else:
        return False, possible_matches


def check_faction_id(df, index, possible_matches, faction_id):
    # Get possible matches according to faction_id.
    possible_matches = get_possible_matches(possible_matches, faction_id=faction_id)

    # Check if IDs unique.
    if check_unique(possible_matches):
        set_id(df, index, possible_matches, col_set="politician_id", col_check="ui")
        return True, possible_matches
    else:
        return False, possible_matches


def check_location_info(df, index, possible_matches, constituency, fuzzy_threshold=70):
    possible_matches = possible_matches.loc[
        possible_matches.constituency.apply(fuzz.ratio, args=[constituency])
        > fuzzy_threshold
    ]

    if len(np.unique(possible_matches.ui)) == 1:
        set_id(df, index, possible_matches, col_set="politician_id", col_check="ui")
        return True, possible_matches
    else:
        return False, possible_matches


def check_name_and_profession(
    df, index, last_name, profession_regex, politicians_df, fuzzy_threshold=75
):
    possible_matches = get_possible_matches(politicians_df, last_name=last_name)

    if len(possible_matches) == 0:
        possible_matches = get_fuzzy_names(
            politicians_df, name_to_check=last_name, fuzzy_threshold=fuzzy_threshold
        )

    if check_unique(possible_matches):
        set_id(df, index, possible_matches, col_set="politician_id", col_check="ui")
        return True, possible_matches
    else:
        boolean_indexer = possible_matches.profession.str.contains(
            profession_regex, regex=True, na=False
        )
        possible_matches = possible_matches[boolean_indexer]

        if check_unique(possible_matches):
            set_id(df, index, possible_matches, col_set="politician_id", col_check="ui")
            return True, possible_matches
        else:
            return False, possible_matches


def check_government(df, index, last_name, mgs_electoral_term, fuzzy_threshold=80):
    possible_matches = get_possible_matches(mgs_electoral_term, last_name=last_name)

    if len(possible_matches) == 0:
        possible_matches = get_fuzzy_names(
            mgs_electoral_term, name_to_check=last_name, fuzzy_threshold=fuzzy_threshold
        )

    if check_unique(possible_matches):
        set_id(df, index, possible_matches, col_set="politician_id", col_check="ui")
        return True, possible_matches
    else:
        return False, possible_matches


def check_member_of_parliament(
    df,
    index,
    first_name,
    last_name,
    politicians,
    faction_id,
    constituency,
    acad_title,
    fuzzy_threshold=80,
):
    # Check Last Name.
    found, possible_matches = check_last_name(df, index, politicians, last_name)
    if found:
        return True, possible_matches

    # Fuzzy search, if last_name can't be found.
    if len(possible_matches) == 0:
        possible_matches = get_fuzzy_names(politicians, name_to_check=last_name)

    if len(possible_matches) == 0:
        return False, possible_matches

    # Check Faction ID.
    if faction_id >= 0:
        found, possible_matches = check_faction_id(
            df, index, possible_matches, faction_id
        )
        if found:
            return found, possible_matches

    # Check First Name.
    if first_name:
        found, possible_matches = check_first_name(
            df, index, possible_matches, first_name
        )
        if found:
            return found, possible_matches

    # Match with location info.
    if constituency:
        found, possible_matches = check_location_info(
            df, index, possible_matches, constituency
        )
        if found:
            return found, possible_matches
    elif constituency == "":
        # Probably someone joined during the period, e.g. there
        # is an entry in STAMMDATEN for the correct person
        # without the location info, as there was only one
        # person with the last name before.
        possible_matches = get_possible_matches(possible_matches, constituency="")
        if check_unique(possible_matches, col="ui"):
            set_id(df, index, possible_matches, col_set="politician_id", col_check="ui")
            return True, possible_matches

    # Check Gender.
    found, possible_matches = check_woman(df, index, acad_title, possible_matches)
    if found:
        return True, possible_matches
    else:
        return False, possible_matches


def check_woman(df, index, acad_title, possible_matches):
    if "Frau" in acad_title:
        possible_matches = possible_matches.loc[possible_matches.gender == "weiblich"]

        if check_unique(possible_matches):
            set_id(df, index, possible_matches, col_set="politician_id", col_check="ui")
            return True, possible_matches
    return False, possible_matches


def insert_politician_id_into_speech_content(
    df, politicians_electoral_term, mgs_electoral_term, politicians
):
    "Appends a politician id column with matched IDs"

    df = df.fillna("")

    last_name_copy = df.last_name.copy()
    first_name_copy = df.first_name.copy()

    problem_df = []

    # Lower case to ease up matching. Note: first_name is a list of strings.
    df.first_name = df.first_name.apply(
        lambda first: [str.lower(string) for string in first]
    )

    df.constituency = df.constituency.fillna("")
    df.constituency = df.constituency.str.lower()
    df.last_name = df.last_name.str.lower()
    df.last_name = df.last_name.str.replace("ß", "ss", regex=False)
    df.insert(4, "politician_id", -1)
    df.position_long = df.position_long.str.lower()

    for index, row in df.iterrows():

        # ##################################################################
        # ######## Start Matching ##########################################
        # ##################################################################

        if row.position_short == "Presidium of Parliament":

            if row.position_long in [
                "präsident",
                "präsidentin",
                "vizepräsident",
                "vizepräsidentin",
            ]:
                if row.last_name == "jäger":
                    # The president of the Bundestag is saves as "jaeger" in the "politicians" data
                    # Maybe manually changing last name as below would work. But must be checked
                    # if this does maybe change also other politicians which should not be changed.
                    # row.last_name = "jaeger"
                    pass
                elif row.last_name == "bläss":
                    row.last_name = "bläss-rafajlovski"

                profession_pattern = "präsident dbt|präsidentin dbt|vizepräsident dbt|vizepräsidentin dbt|vizeprä. dbt"
                found, possible_matches = check_name_and_profession(
                    df,
                    index,
                    row.last_name,
                    profession_pattern,
                    politicians_electoral_term,
                )

                if found:
                    continue
                else:
                    found, possible_matches = check_member_of_parliament(
                        df,
                        index,
                        row.first_name,
                        row.last_name,
                        politicians_electoral_term,
                        row.faction_id,
                        row.constituency,
                        row.acad_title,
                        fuzzy_threshold=80,
                    )

                    if found:
                        continue
                    else:
                        problem_df.append(row)

            elif regex.search("schriftführer", row.position_long):
                profession_pattern = "schriftführer"
                found, possible_matches = check_name_and_profession(
                    df,
                    index,
                    row.last_name,
                    profession_pattern,
                    politicians_electoral_term,
                )

                if found:
                    continue
                else:
                    found, possible_matches = check_member_of_parliament(
                        df,
                        index,
                        row.first_name,
                        row.last_name,
                        politicians_electoral_term,
                        row.faction_id,
                        row.constituency,
                        row.acad_title,
                        fuzzy_threshold=80,
                    )

                    if found:
                        continue
                    else:
                        problem_df.append(row)

            else:
                found, possible_matches = check_member_of_parliament(
                    df,
                    index,
                    row.first_name,
                    row.last_name,
                    politicians_electoral_term,
                    row.faction_id,
                    row.constituency,
                    row.acad_title,
                    fuzzy_threshold=80,
                )

                if found:
                    continue
                else:
                    problem_df.append(row)

        elif row.position_short == "Minister":
            found, possible_matches = check_government(
                df, index, row.last_name, mgs_electoral_term, fuzzy_threshold=75
            )

            if found:
                continue
            else:
                found, possible_matches = check_member_of_parliament(
                    df,
                    index,
                    row.first_name,
                    row.last_name,
                    politicians_electoral_term,
                    row.faction_id,
                    row.constituency,
                    row.acad_title,
                    fuzzy_threshold=80,
                )

                if found:
                    continue
                else:
                    problem_df.append(row)

        elif row.position_short == "Chancellor":
            found, possible_matches = check_government(
                df, index, row.last_name, mgs_electoral_term
            )

            if found:
                continue
            else:
                problem_df.append(row)

        elif row.position_short == "Secretary of State":

            # Look for "Parlamentarische Staatsekretäre"
            if regex.search("parl", row.position_long):

                profession_pattern = (
                    "Parl. Staatssekretär|Parlamentarischer Staatssekretär"
                )

                found, possible_matches = check_name_and_profession(
                    df,
                    index,
                    row.last_name,
                    profession_pattern,
                    politicians_electoral_term,
                )

                if found:
                    continue
                else:
                    found, possible_matches = check_member_of_parliament(
                        df,
                        index,
                        row.first_name,
                        row.last_name,
                        politicians_electoral_term,
                        row.faction_id,
                        row.constituency,
                        row.acad_title,
                        fuzzy_threshold=80,
                    )

                    if found:
                        continue
                    else:
                        problem_df.append(row)

            # "Beamtete Staatsekretäre" are not included in "politicians" data.
            elif regex.search("staatssekretär", row.position_long):
                problem_df.append(row)
                continue

            else:
                problem_df.append(row)
                continue

        elif row.position_short == "Member of Parliament":
            found, possible_matches = check_member_of_parliament(
                df,
                index,
                row.first_name,
                row.last_name,
                politicians_electoral_term,
                row.faction_id,
                row.constituency,
                row.acad_title,
                fuzzy_threshold=80,
            )

            if found:
                continue
            else:
                problem_df.append(row)

        else:
            # Some other notes
            # Example: Meyer in 01033. Have the same last name and
            # are in the same faction at same period. In this
            # particular case the location information in the toc
            # "Westhagen", does not match with the two possible
            # location informations "Hagen", "Bremen"
            # probably "Hagen" == "Westfalen" is meant.
            # Other things: "Cornelia <Nachname>" ist in dem spoken content
            # mit "Conny <Nachname>" abgespeichert. Findet Vornamen natürlich
            # nicht.

            problem_df.append(row)

        df.first_name = first_name_copy
        df.last_name = last_name_copy

    problem_df = pd.DataFrame(problem_df)

    return df, problem_df


def insert_politician_id_into_contributions_extended(
    df, politicians_electoral_term, mgs_electoral_term
):
    "Appends a politician id column with matched IDs"

    assert {
        "last_name",
        "first_name",
        "faction_id",
        "acad_title",
        "constituency",
    }.issubset(df.columns)

    if len(df) == 0:
        return df, pd.DataFrame()

    last_name_copy = df.last_name.copy()
    first_name_copy = df.first_name.copy()

    problem_df = []

    # Lower case to ease up matching
    df.first_name = df.first_name.apply(
        lambda first: [str.lower(string) for string in first]
    )
    df.constituency = df.constituency.fillna("")
    df.constituency = df.constituency.str.lower()
    df.last_name = df.last_name.str.lower()
    df.last_name = df.last_name.str.replace("ß", "ss", regex=False)
    df.insert(4, "politician_id", -1)

    for index, row in df.iterrows():

        # Start Matching

        # E.g. Präsident, Bundeskanzler, Staatssekretär etc.
        if not row.last_name:
            problem_df.append(row)
            continue
        else:
            found, possible_matches = check_last_name(
                df, index, politicians_electoral_term, row.last_name
            )
            if found:
                if check_unique(possible_matches, col="faction_id"):
                    set_id(
                        df,
                        index,
                        possible_matches,
                        col_set="faction_id",
                        col_check="faction_id",
                    )
                    continue
                else:
                    continue

        # Fuzzy search, if last_name can't be found.
        if len(possible_matches) == 0:
            possible_matches = get_fuzzy_names(
                politicians_electoral_term, row.last_name
            )

        if len(possible_matches) == 0:
            problem_df.append(row)
            continue

        # Check Faction ID.
        if row.faction_id >= 0:
            found, possible_matches = check_faction_id(
                df, index, possible_matches, row.faction_id
            )
            if found:
                if check_unique(possible_matches, col="faction_id"):
                    df.faction_id.at[index] = int(possible_matches.faction_id.iloc[0])
                    continue
                else:
                    continue

        # Check First Name.
        if row.first_name:
            found, possible_matches = check_first_name(
                df, index, possible_matches, row.first_name
            )
            if found:
                continue

        # Match with location info.
        if row.constituency:
            found, possible_matches = check_location_info(
                df, index, possible_matches, row.constituency
            )
            if found:
                continue
        elif row.constituency == "":
            # Probably someone joined during the period, e.g. there
            # is an entry in STAMMDATEN for the correct person
            # without the location info, as there was only one
            # person with the last name before.
            possible_matches = get_possible_matches(possible_matches, constituency="")

            if check_unique(possible_matches):
                set_id(
                    df, index, possible_matches, col_set="politician_id", col_check="ui"
                )
                continue

        # Check Gender.
        found, possible_matches = check_woman(
            df, index, row.acad_title, possible_matches
        )
        if found:
            continue

        # Example: Meyer in 01033. Have the same last name and
        # are in the same faction at same period. In this
        # particular case the location information in the toc
        # "Westhagen", does not match with the two possible
        # location informations "Hagen", "Bremen"
        # probably "Hagen" == "Westfalen" is meant.
        # Other things: Cornelia Irgendwas, ist in dem spoken content
        # mit "Conny Irgendwas abgespeichert. Findet Vornamen natürlich
        # nicht.
        problem_df.append(row)

        df.first_name = first_name_copy
        df.last_name = last_name_copy

    problem_df = pd.DataFrame(problem_df)
    return df, problem_df


'''
extract funcations
'''



import pandas as pd
import regex
import copy

# Party Patterns:
parties = {
    "CDU/CSU": r"(?:Gast|-)?(?:\s*C\s*[DSMU]\s*S?[DU]\s*(?:\s*[/,':!.-]?)*\s*(?:\s*C+\s*[DSs]?\s*[UÙ]?\s*)?)(?:-?Hosp\.|-Gast|1)?",
    "SPD": r"\s*'?S(?:PD|DP)(?:\.|-Gast)?",
    "FDP": r"\s*F\.?\s*[PDO][.']?[DP]\.?",
    "BÜNDNIS 90/DIE GRÜNEN": r"(?:BÜNDNIS\s*(?:90)?/?(?:\s*D[1I]E)?|Bündnis\s*90/(?:\s*D[1I]E)?)?\s*[GC]R[UÜ].?\s*[ÑN]EN?(?:/Bündnis 90)?|BÜNDNISSES 90/DIE GRÜNEN|Grünen|BÜNDNISSES 90/ DIE GRÜNEN|BÜNDNIS 90/DIE GRÜNEN",
    "DIE LINKE": r"DIE LIN\s?KEN?|LIN\s?KEN",
    "PDS/Linke Liste": r"(?:Gruppe\s*der\s*)?PDS(?:/(?:LL|Linke Liste))?",
    "fraktionslos": r"(fraktionslos|Parteilos)",
    "GB/BHE": r"(?:GB[/-]\s*)?BHE(?:-DG)?",
    "DP": "DP",
    "KPD": "KPD",
    "Z": r"Z\s|Zentrum",
    "BP": "BP|Bayernpartei",
    "FU": "FU",
    "WAV": "WAV",
    "DRP": r"DRP(\-Hosp\.)?",
    "FVP": "FVP",
    "SSW": "SSW",
    "SRP": "SRP",
    "DA": "DA",
    "Gast": "Gast",
    "DBP": "DBP",
    "NR": "NR",
}
left_right_Pattern = r"([Rr]echts|[Ll]inks|[Mm]itte)"


# Other Patterns:
prefix_Pattern = r"\b(?:\s*bei\s+der|\s*im|\s*bei\s+Abgeordneten|\s*bei\s+Abgeordneten\s+der|\s*beim|\s*des|)\b"
suffix_Pattern = r"\s*?(?!der)(?![-––])(?P<initiator>(?:(?!\s[-––]\s)[^:])*)\s*"
text_Pattern = (
    r"[^––:(){{}}[\]\n{}]"  # Basic Text Pattern that can be extended if needed
)

# Bracket Patterns (can also be extended modularly):
start_contributions_opening_bracket_Pattern = (
    r"(?:(?<=\()|(?<=[-––]\s)|(?<=[––])|(?<=[-––]\.\s)|(?<=\s[-––]){})"
)
start_contributions_closing_bracket_Pattern = (
    r"(?=\)|–[^\)\(]+\)|{{|—[^\)\(]+\)|\)|-[^\)\(]+\){})"
)

opening_bracket_Pattern = r"[({\[]"
closing_bracket_Pattern = r"[)}\]]"

# Base Patterns:
base_applause_Pattern = (
    r"(?P<delete>(?:(?:[Ll]ang)?[Aa]nhaltender\s(?:[Ll]ebhafter\s)?|[Ll]ebhafter\s|[Ee]rneuter\s|[Dd]emonstrativer|[Aa]llseitiger)?Beifall"
    + prefix_Pattern
    + suffix_Pattern
    + r")"
)
base_person_interjection_Pattern = (
    r"(?P<delete>(?!Beifall){}:\s(?P<content>[^-)—–{{}}]*))"
)
base_shout_Pattern = (
    r"(?P<delete>(und\s?|[Ee]rneute\s|[Aa]nhaltende\s|[Ee]rregte\s|[Vv]ielfache)?(Zurufe?|Gegenrufe?|Rufe?)(?:(?::|"
    + prefix_Pattern
    + r"{0}:)\s*(?P<content>{1}*)|"
    + prefix_Pattern
    + suffix_Pattern
    + r"))"
)
base_cheerfulness_Pattern = (
    r"(?P<delete>(Große)?Heiterkeit" + prefix_Pattern + suffix_Pattern + r")"
)
base_objection_Pattern = (
    r"(?P<delete>Widerspruch" + prefix_Pattern + suffix_Pattern + r")"
)
base_laughter_Pattern = r"(?P<delete>Lachen" + prefix_Pattern + suffix_Pattern + r")"
base_approval_Pattern = (
    r"(?<delete>(Sehr\srichtig[.!]?|Zustimmung|Lebhafte\sZustimmung|Sehr\swahr[.!]?|Bravo[-—\s]?[Rr]ufe[.!]?|Bravo[.!]?|Sehr\sgut[.!]?)"
    + prefix_Pattern
    + suffix_Pattern
    + r")"
)
base_interruption_Pattern = r"Unterbrechung[^)]*"
base_disturbance_Pattern = (
    r"(?P<delete>[Uu]nruhe" + prefix_Pattern + suffix_Pattern + r")"
)

# Modular Patterns:
name_Pattern = {
    0: r"(?P<name_raw>(?:(?!\sund\s)(?!sowie\sdes)"
    # Formatting has to be done this way because of python formatting errors
    + text_Pattern.format("").replace(
        "{}", "{{}}"
    )  # Text Pattern can be extended if needed
    + r")+)(\s*{0}(?P<constituency>"
    + text_Pattern.format("").replace(
        "{}", "{{}}"
    )  # Text Pattern can be extended if needed
    + r"+){1})*\s*{0}(?P<faction>"
    + text_Pattern.format("").replace(
        "{}", "{{}}"
    )  # Text Pattern can be extended if needed
    + r"*){1}(\s*{0}(?P<constituency>"
    + text_Pattern.format("").replace(
        "{}", "{{}}"
    )  # Text Pattern can be extended if needed
    + r"+){1})*",
    1: r"(?P<name_raw>(?:(?!\sund\s)(?!sowie\sdes)"
    + text_Pattern.format("").replace(
        "{}", "{{}}"
    )  # Text Pattern can be extended if needed
    + r")+)(?P<constituency>{0}"
    + text_Pattern.format("").replace(
        "{}", "{{}}"
    )  # Text Pattern can be extended if needed
    + r"+{1})*",
}


def get_government_factions(electoral_term):
    """Get the government factions for the given electoral_term"""
    government_electoral_term = {
        1: ["CDU/CSU", "FDP", "DP"],
        2: ["CDU/CSU", "FDP", "DP"],
        3: ["CDU/CSU", "DP"],
        4: ["CDU/CSU", "FDP"],
        5: ["CDU/CSU", "SPD"],
        6: ["SPD", "FDP"],
        7: ["SPD", "FDP"],
        8: ["SPD", "FDP"],
        9: ["SPD", "FDP"],
        10: ["CDU/CSU", "FDP"],
        11: ["CDU/CSU", "FDP"],
        12: ["CDU/CSU", "FDP"],
        13: ["CDU/CSU", "FDP"],
        14: ["SPD", "BÜNDNIS 90/DIE GRÜNEN"],
        15: ["SPD", "BÜNDNIS 90/DIE GRÜNEN"],
        16: ["CDU/CSU", "SPD"],
        17: ["CDU/CSU", "FDP"],
        18: ["CDU/CSU", "SPD"],
        19: ["CDU/CSU", "SPD"],
    }

    return government_electoral_term[electoral_term]


def convert_to_string(string):
    return "" if string is None else str(string)


def clean_person_name(name_raw):
    """cleans the person name_raw"""
    # Remove any newlines from the name_raw
    name_raw = regex.sub(r"\n", " ", convert_to_string(name_raw))
    # Remove any Additional stuff
    name_raw = regex.sub(
        r"(Gegenrufe?\sdes\s|Gegenrufe?\sder\s|Zurufe?\sdes\s|Zurufe?\sder\s)(Abg\s?\.\s)*",
        "",
        name_raw,
    )
    name_raw = regex.sub(r"(Abg\s?\.\s?|Abgeordneten\s)", "", name_raw)
    # Remove any Pronouns
    name_raw = regex.sub(r"(^\s?der\s?|^\s?die\s?|^\s?das\s?|^\s?von\s?)", "", name_raw)
    # Remove whitespaces at the beginning and at the end
    name_raw = name_raw.lstrip(" ").rstrip(" ")

    # Return the name_raw
    return name_raw


def add_entry(frame, id, type, name_raw, faction, constituency, content, text_position):
    """adds an entry for every Contribution into the given frame"""
    # Append the corresponding variables to the dictionary
    frame["id"].append(id)
    frame["type"].append(type)
    frame["name_raw"].append(clean_person_name(name_raw))
    frame["faction"].append(convert_to_string(faction))
    frame["constituency"].append(convert_to_string(constituency))
    frame["content"].append(convert_to_string(content))
    frame["text_position"].append(int(text_position))

    # Return the frame
    return frame


def extract_initiators(
    initiators, electoral_term, session, identity, text_position, frame, type
):
    """extracts the initators and creates and entry in the frame (for each initiator)
    Tries extracting politicians (twice - different methods); parties by themselves;
    'links', 'rechts', 'mitte' and government parties"""

    initiators_not_removed = copy.copy(initiators)
    # Remove wrongly placed contributions from initiators and pass them recursively
    other_contributions = regex.search(
        r"(?P<type>[Bb]eifall|[Zz]uruf|[Gg]egenruf|[Rr]uf|[Hh]eiterkeit|[Ww]iderspruch|[Ll]achen|[Zz]ustimmung|[Uu]nterbrechung|[Uu]nruhe)(?P<initiators>(?:(?!\s[-––]\s).)*)\s*",
        initiators,
    )
    if other_contributions:
        frame, _ = methods[other_contributions.group("type").lower()](
            "(" + other_contributions.group() + ")",
            electoral_term,
            session,
            identity,
            text_position,
            frame,
        )
        initiators = initiators.replace(other_contributions.group(), "")

    if session < 7115:
        # Set name pattern to the second name pattern (second row in name_Pattern)
        name_Pattern_id = 1
    else:
        # Set name pattern to the first name pattern (first row in name_Pattern)
        name_Pattern_id = 0

    # Create the first_person_search_Pattern (looking for key Abg.)
    first_person_search_Pattern = r"Abg\s?\.\s?{}(?:(?<=!:)|(?!:))".format(
        name_Pattern[name_Pattern_id].format(
            opening_bracket_Pattern, closing_bracket_Pattern,
        )
    )
    # Find match
    first_person_match = regex.search(first_person_search_Pattern, initiators,)
    if first_person_match:
        # Remove name_raw from the search text
        initiators = initiators.replace(first_person_match.group(), "")
        # Check if the person was just asking a "Zwischenfrage"
        if not regex.search("[Zz]wischenfrage", initiators):
            # Get the persons name_raw
            name_raw = first_person_match.group("name_raw")
            # Try to get the persons faction
            try:
                faction = first_person_match.group("faction")
            except IndexError:
                faction = ""
            # Try to get the persons location information
            try:
                constituency = first_person_match.group("constituency")
            except IndexError:
                constituency = ""
            # Add an entry to the frame
            frame = add_entry(
                frame,
                identity,
                type,
                name_raw,
                faction,
                constituency,
                "",
                text_position,
            )

    # Create the first_person_search_Pattern (looking for key und)
    second_person_search_Pattern = r"(?:\sund|sowie\sdes)\s+(?:des|der)?{}(?:(?<=!:)|(?!:))".format(
        name_Pattern[name_Pattern_id].format(
            opening_bracket_Pattern, closing_bracket_Pattern,
        )
    )

    # Find match
    second_person_match = regex.search(second_person_search_Pattern, initiators,)
    if second_person_match:
        # Remove the person name_raw from the search text
        initiators = initiators.replace(second_person_match.group(), "")
        # Check if the person was just asking a "Zwischenfrage"
        if not regex.search("[Zz]wischenfrage", initiators):
            # Get the persons name_raw
            name_raw = second_person_match.group("name_raw")
            # Try to get the persons faction
            try:
                faction = second_person_match.group("faction")
            except IndexError:
                faction = ""
            # Try to get the persons location information
            try:
                constituency = second_person_match.group("constituency")
            except IndexError:
                constituency = ""
            # Add an entry to the frame
            frame = add_entry(
                frame,
                identity,
                type,
                name_raw,
                faction,
                constituency,
                "",
                text_position,
            )

    # Iterate over all parties
    for faction in parties:
        # Create the faction_search_Pattern
        faction_search_Pattern = r"(?<!\[)(" + parties[faction] + r")(?![^[\s]*\])"
        # Find match for faction
        faction_match = regex.search(faction_search_Pattern, initiators)
        # Check if there is a match
        if faction_match:
            # Remove the faction from the search text
            initiators = initiators.replace(faction_match.group(), "")
            # Add an entry to the frame
            frame = add_entry(frame, identity, type, "", faction, "", "", text_position)

    # Create the left_right_search_Pattern
    left_right_search_Pattern = left_right_Pattern
    # Find matches
    left_right_matches = list(regex.finditer(left_right_search_Pattern, initiators))
    for direction in left_right_matches:
        # Remove the direction from the search text
        initiators = initiators.replace(direction.group(), "")
        # Add an entry to the frame
        frame = add_entry(
            frame, identity, type, "", "", "", direction.group(), text_position
        )

    # Search for Regierungsparteien in the initiators
    government_matches = regex.search(r"[Rr]egierungspar[^\s]+", initiators)
    if government_matches:
        initiators = initiators.replace(government_matches.group(), "")
        # iterate over every faction get_government_factions returns
        for faction in get_government_factions(electoral_term):
            # Add and entry to the frame for every faction in the government
            frame = add_entry(frame, identity, type, "", faction, "", "", text_position)

    search_stuff = [
        ": Das haben Sie 16 Jahre lang versäumt!",
    ]
    for stuff in search_stuff:
        if (
            stuff
            == initiators
            # or stuff in initiators
            # or stuff == initiators_not_removed
            # or stuff in initiators_not_removed
        ):
            print(
                initiators_not_removed, session, first_person_search_Pattern,
            )
    # Return the frame
    return frame, initiators


def extract_applause(text, electoral_term, session, identity, text_position, frame):
    """Extracts applause from the given text"""

    # creates the Pattern modularly
    applause_Pattern = (
        start_contributions_opening_bracket_Pattern.format(
            ""  # Nothing to extend, so .format("")
        )
        + base_applause_Pattern
        + start_contributions_closing_bracket_Pattern.format(
            ""  # Nothing to extend, so .format("")
        )
    )

    matches = list(regex.finditer(applause_Pattern, text,))

    for match in matches:
        # replace everything except the delimeters
        text = text.replace(match.group("delete"), " ")
        # Extract the initiators and create entries to the dataframe
        frame, returned = extract_initiators(
            match.group("initiator"),
            electoral_term,
            session,
            identity,
            text_position,
            frame,
            "Beifall",
        )

    # Return the frame
    return frame, text


def extract_person_interjection(
    text, electoral_term, session, identity, text_position, frame
):
    """Extracts person interjections from the given text"""

    # Check if session is under 7115
    if session < 7115:
        # Set name pattern to the second name pattern (second row in name_Pattern)
        name_Pattern_id = 1
        extra_Pattern = r"(?:Abg\s?\.\s?)"
    else:
        # Set name pattern to the first name pattern (first row in name_Pattern)
        name_Pattern_id = 0
        extra_Pattern = ""

    # creates the Pattern "very" modularly
    person_interjection_Pattern = (
        start_contributions_opening_bracket_Pattern.format("")
        + base_person_interjection_Pattern.format(
            extra_Pattern
            + name_Pattern[name_Pattern_id].format(
                opening_bracket_Pattern, closing_bracket_Pattern,
            )
        )
        + start_contributions_closing_bracket_Pattern.format("")
    )

    # Match person interjections
    matches = list(regex.finditer(person_interjection_Pattern, text))

    # Iterate over matches
    for match in matches:
        # replace everything except the delimeters
        text = text.replace(match.group("delete"), " ")
        name_raw = match.group("name_raw")
        content = match.group("content")

        try:
            faction = match.group("faction")
        except IndexError:
            faction = ""

        try:
            constituency = match.group("constituency")
        except IndexError:
            constituency = ""

        # Add entry to the frame
        frame = add_entry(
            frame,
            identity,
            "Personen-Einruf",
            name_raw,
            faction,
            constituency,
            content,
            text_position,
        )

    return frame, text


def extract_shout(text, electoral_term, session, identity, text_position, frame):
    """Extracts shouts from the given text"""

    # Check if session is under 7115
    if session < 7115:
        # Set name pattern to the second name pattern (second row in name_Pattern)
        name_Pattern_id = 1
    else:
        # Set name pattern to the first name pattern (first row in name_Pattern)
        name_Pattern_id = 0

    # creates the Pattern modularly
    shout_Pattern = (
        start_contributions_opening_bracket_Pattern.format(
            r"|(?<=[Hh]eiterkeit\s)|(?<=[Ll]achen\s)|(?<=[Ww]eiterer\s)|(?<=[Ww]eitere\s)|(?<=[Ee]rneuter\s)|(?<=[Ee]rneute\s)|(?<=[Ff]ortgesetzte\s)|(?<=[Ll]ebhafte\s)|(?<=[Ww]eitere\s[Ll]ebhafte\s|(?<=Andauernde\s)|(?<=Fortdauernde\s))"  # Extending the opening_bracket_Pattern
        )
        + base_shout_Pattern.format(
            r"\s*Abg\s?\.\s?{}".format(
                name_Pattern[name_Pattern_id].format(
                    opening_bracket_Pattern, closing_bracket_Pattern,
                )
            ),
            text_Pattern.format("").replace("{}", "{{}}"),
        )
        + start_contributions_closing_bracket_Pattern.format(
            ""  # Nothing to extend, so .format("")
        )
    )

    matches = list(regex.finditer(shout_Pattern, text,))
    for match in matches:
        if match.group("initiator"):
            # replace everything except the delimeters
            text = text.replace(match.group("delete"), " ")
            # Extract the initiators and create entries to the dataframe
            frame, _ = extract_initiators(
                match.group("initiator"),
                electoral_term,
                session,
                identity,
                text_position,
                frame,
                "Zuruf",
            )
        else:
            # replace everything except the delimeters
            text = text.replace(match.group("delete"), " ")

            try:
                name_raw = match.group("name_raw")
            except IndexError:
                name_raw = ""
            content = match.group("content")

            try:
                faction = match.group("faction")
            except IndexError:
                faction = ""

            try:
                constituency = match.group("constituency")
            except IndexError:
                constituency = ""

            # Add an entry to the frame
            frame = add_entry(
                frame,
                identity,
                "Zuruf",
                name_raw,
                faction,
                constituency,
                content,
                text_position,
            )

    # Extract faction shouts
    # creates the Pattern modularly
    faction_shout_Pattern = (
        start_contributions_opening_bracket_Pattern.format(
            r"|(?<=[Hh]eiterkeit\s)|(?<=[Ll]achen\s)|(?<=[Ww]eiterer\s)|(?<=[Ww]eitere\s)|(?<=[Ee]rneuter\s)|(?<=[Ee]rneute\s)|(?<=[Ff]ortgesetzte\s)|(?<=[Ll]ebhafte\s)|(?<=[Ww]eitere\s[Ll]ebhafte\s|(?<=Andauernde\s)|(?<=Fortdauernde\s))"  # Extending the opening_bracket_Pattern
        )
        + r"(?P<delete>(?P<initiator>"
        + text_Pattern.format("").replace("{}", "{{}}")
        + r"+):\s*(?P<content>"
        + text_Pattern
        + r"+))"
        + start_contributions_closing_bracket_Pattern.format(
            ""  # Nothing to extend, so .format("")
        )
    )

    matches = list(regex.finditer(faction_shout_Pattern, text,))
    for match in matches:
        # replace everything except the delimeters
        text = text.replace(match.group("delete"), " ")
        content = match.group("content")
        initiators = match.group("initiator")

        # Iterate over all parties
        for faction in parties:
            # Create the faction_search_Pattern
            faction_search_Pattern = r"(?<!\[)(" + parties[faction] + r")(?![^[\s]*\])"
            # Find match for faction
            faction_match = regex.search(faction_search_Pattern, initiators)
            # Check if there is a match
            if faction_match:
                # Remove the faction from the search text
                initiators = initiators.replace(faction_match.group(), "")
                # Add an entry to the frame
                frame = add_entry(
                    frame, identity, "Zuruf", "", faction, "", content, text_position
                )

    # Return the frame
    return frame, text


def extract_cheerfulness(text, electoral_term, session, identity, text_position, frame):
    """Extracts cheerfulness from the given text"""

    # creates the Pattern modularly
    cheerfulness_Pattern = (
        start_contributions_opening_bracket_Pattern.format(
            ""  # Nothing to extend, so .format("")
        )
        + base_cheerfulness_Pattern
        + start_contributions_closing_bracket_Pattern.format(
            ""  # Nothing to extend, so .format("")
        )
    )

    matches = list(regex.finditer(cheerfulness_Pattern, text,))
    for match in matches:
        # replace everything except the delimeters
        text = text.replace(match.group("delete"), " ")
        # Extract the initiators and create entries to the dataframe
        frame, _ = extract_initiators(
            match.group("initiator"),
            electoral_term,
            session,
            identity,
            text_position,
            frame,
            "Heiterkeit",
        )

    # Return the frame
    return frame, text


def extract_objection(text, electoral_term, session, identity, text_position, frame):
    """Extracts objection from the given text"""

    # creates the Pattern modularly
    objection_Pattern = (
        start_contributions_opening_bracket_Pattern.format(
            ""  # Nothing to extend, so .format("")
        )
        + base_objection_Pattern
        + start_contributions_closing_bracket_Pattern.format(
            ""  # Nothing to extend, so .format("")
        )
    )

    matches = list(regex.finditer(objection_Pattern, text,))
    for match in matches:
        # replace everything except the delimeters
        text = text.replace(match.group("delete"), " ")
        # Extract the initiators and create entries to the dataframe
        frame, _ = extract_initiators(
            match.group("initiator"),
            electoral_term,
            session,
            identity,
            text_position,
            frame,
            "Widerspruch",
        )

    # Return the frame
    return frame, text


def extract_laughter(text, electoral_term, session, identity, text_position, frame):
    """Extracts laughter from the given text"""

    # creates the Pattern modularly
    laughter_Pattern = (
        start_contributions_opening_bracket_Pattern.format(
            ""  # Nothing to extend, so .format("")
        )
        + base_laughter_Pattern
        + start_contributions_closing_bracket_Pattern.format(
            r"|\sund\sZurufe\)"  # Extending the closing_bracket_Pattern
        )
    )

    matches = list(regex.finditer(laughter_Pattern, text,))
    for match in matches:
        # replace everything except the delimeters
        text = text.replace(match.group("delete"), " ")
        # Extract the initiators and create entries to the dataframe
        frame, _ = extract_initiators(
            match.group("initiator"),
            electoral_term,
            session,
            identity,
            text_position,
            frame,
            "Lachen",
        )

    # Return the frame
    return frame, text


def extract_approval(text, electoral_term, session, identity, text_position, frame):
    """Extracts approval from the given text"""

    # creates the Pattern modularly
    approval_Pattern = (
        start_contributions_opening_bracket_Pattern.format(
            ""  # Nothing to extend, so .format("")
        )
        + base_approval_Pattern
        + start_contributions_closing_bracket_Pattern.format(
            ""  # Nothing to extend, so .format("")
        )
    )

    matches = list(regex.finditer(approval_Pattern, text,))
    for match in matches:
        # replace everything except the delimeters
        text = text.replace(match.group("delete"), " ")
        # Extract the initiators and create entries to the dataframe
        frame, _ = extract_initiators(
            match.group("initiator"),
            electoral_term,
            session,
            identity,
            text_position,
            frame,
            "Zustimmung",
        )

    # Return the frame
    return frame, ""


def extract_interruption(text, electoral_term, session, identity, text_position, frame):
    """Extracts interruptions from the given text"""

    # Creates the Pattern modularly
    interruption_Pattern = (
        start_contributions_opening_bracket_Pattern.format(
            ""  # Nothing to extend, so .format("")
        )
        + base_interruption_Pattern
        + start_contributions_closing_bracket_Pattern.format(
            ""  # Nothing to extend, so .format("")
        )
    )

    # Find matches
    matches = list(regex.finditer(interruption_Pattern, text))

    # Iterate over matches
    for match in matches:
        # replace everything except the delimeters
        text = text.replace(match.group("delete"), " ")
        # Add entry to the frame
        frame = add_entry(
            frame,
            identity,
            "Unterbrechung",
            "",
            "",
            "",
            match.group("delete"),
            text_position,
        )

    return frame, text


def extract_disturbance(text, electoral_term, session, identity, text_position, frame):
    """Extracts disturbance from the given text"""

    # creates the Pattern modularly
    disturbance_Pattern = (
        start_contributions_opening_bracket_Pattern.format(
            ""  # Nothing to extend, so .format("")
        )
        + base_disturbance_Pattern
        + start_contributions_closing_bracket_Pattern.format(
            ""  # Nothing to extend, so .format("")
        )
    )

    matches = list(regex.finditer(disturbance_Pattern, text,))

    for match in matches:
        # replace everything except the delimeters
        text = text.replace(match.group("delete"), " ")
        # Extract the initiators and create entries to the dataframe
        frame, _ = extract_initiators(
            match.group("initiator"),
            electoral_term,
            session,
            identity,
            text_position,
            frame,
            "Unruhe",
        )

    # Return the frame
    return frame, text


def extract(
    speech_text, session, identity, text_position=0, text_position_reversed=True
):
    electoral_term = session // 1000

    # Match all brackets
    brackets = list(
        regex.finditer(r"\(([^(\)]*(\(([^(\)]*)\))*[^(\)]*)\)", speech_text)
    )

    # Create an empty frame for the normal contributions
    frame = {
        "id": [],
        "type": [],
        "name_raw": [],
        "faction": [],
        "constituency": [],
        "content": [],
        "text_position": [],
    }

    contributions_simplified = {"text_position": [], "content": [], "speech_id": []}

    # Iterate over all brackets
    for bracket in reversed(brackets):
        # calculate reversed text_position
        reversed_text_position = len(brackets) - 1 - text_position
        # Make sure to remove all newlines
        speech_text_no_newline = regex.sub(r"\n+", " ", bracket.group())
        speech_text_no_newline = regex.sub(r"\s+", " ", speech_text_no_newline)
        # Save the bracket text
        bracket_text = bracket.group()
        # Save deleted text to DataFrame
        contributions_simplified["text_position"].append(
            reversed_text_position if text_position_reversed else text_position
        )
        contributions_simplified["content"].append(bracket_text)
        contributions_simplified["speech_id"].append(identity)

        deletion_span = bracket.span(1)

        # Remove the bracket text from the speech_text and replace it with the text_position
        speech_text = (
            speech_text[: deletion_span[0]]
            + "{"
            + str(reversed_text_position if text_position_reversed else text_position)
            + "}"
            + speech_text[deletion_span[1] :]
        )

        contribution_methods = [
            extract_applause,
            extract_person_interjection,
            extract_shout,
            extract_cheerfulness,
            extract_objection,
            extract_laughter,
            extract_approval,
            extract_interruption,
            extract_disturbance,
        ]

        for method in contribution_methods:
            frame, speech_text_no_newline = method(
                speech_text_no_newline,
                electoral_term,
                session,
                identity,
                reversed_text_position if text_position_reversed else text_position,
                frame,
            )

        text_position += 1

    return (
        pd.DataFrame(frame),
        speech_text,
        pd.DataFrame(contributions_simplified),
        text_position,
    )


# Method Dictionary:
# Keep in mind to lower the keys
methods = {
    "beifall": extract_applause,
    "zuruf": extract_shout,
    "gegenruf": extract_shout,
    "ruf": extract_shout,
    "heiterkeit": extract_cheerfulness,
    "widerspruch": extract_objection,
    "lachen": extract_laughter,
    "zustimmung": extract_approval,
    "unterbrechung": extract_interruption,
    "unruhe": extract_disturbance,
}
