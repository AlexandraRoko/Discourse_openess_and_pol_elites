'''
------------------------------------------------------------------------
Script name: 02_merge_datasets_together.py
Purpose of script: Generate common key to join Wikipedia data and KL-based measures
Dependencies: ./01_map_names.R
Author: Alexandra Rottenkolber
------------------------------------------------------------------------
'''

import numpy as np
import pandas as pd
import json

# set path
PATH = "./data_analysis/01_data/Plenarprotokolle/"

# import data
with open(PATH + 'processed/id2speech_concat_POSTag.txt', 'r') as infile:
    id2rede_12_19 = json.load(infile)
infile.close()
del id2rede_12_19['0']

with open(PATH + 'processed/key2Party.txt', 'r') as infile:
    key2Party = json.load(infile)
infile.close()

ID_and_names_from_Wiki = pd.read_csv(PATH + 'processed/ID_and_names_from_Wiki.csv') # its from ./01_map_names.R

# start processing data
names_wiki = ID_and_names_from_Wiki["name"]

error = []
politicians_with_noID = []
for speech in id2rede_12_19.keys():
    try:
        if id2rede_12_19[speech]['politicianId'] == "-1":
            name = id2rede_12_19[speech]['firstName'] + " " + id2rede_12_19[speech]['lastName']
            politicians_with_noID.append(name)
    except KeyError:
        error.append(speech)

pol_Ids = []
error = []
for key in id2rede_12_19.keys():
    try:
        pol_Ids.append(id2rede_12_19[key]['politicianId'])
    except KeyError:
        error.append(key)

pol_Ids = pd.Series(pol_Ids).unique()

# assign politician IDs for those where they are missing
flagged_speeches = []
names = []
name2newID = {}
id_counter = 100
PolID2info = {}

for speech in id2rede_12_19.keys():
    temp_dic = {}

    if id2rede_12_19[speech]['politicianId'] != "-1":

        politicianId = id2rede_12_19[speech]['politicianId']
        temp_dic['lastName'] = id2rede_12_19[speech]['lastName']
        temp_dic['firstName'] = id2rede_12_19[speech]['firstName']
        # temp_dic['positionLong'] = id2rede_12_19[speech]['positionLong']
        temp_dic['positionShort'] = id2rede_12_19[speech]['positionShort']

        PolID2info[politicianId] = temp_dic

    elif id2rede_12_19[speech]['politicianId'] == "-1":

        name = id2rede_12_19[speech]['firstName'] + " " + id2rede_12_19[speech]['lastName']

        intermed_dic = {}
        intermed_dic["new_ID"] = id_counter
        intermed_dic["speech"] = speech

        name2newID[name] = intermed_dic
        id_counter += 1

    else:
        flagged_speeches.append(speech)

for fullname in name2newID.keys():
    temp_dic = {}

    politicianId = name2newID[fullname]['new_ID']
    speech = name2newID[fullname]['speech']

    temp_dic['lastName'] = id2rede_12_19[speech]['lastName']
    temp_dic['firstName'] = id2rede_12_19[speech]['firstName']
    temp_dic['positionShort'] = id2rede_12_19[speech]['positionShort']

    PolID2info[politicianId] = temp_dic

for PolID in PolID2info.keys():
    for key in key2Party.keys():
        if (PolID2info[PolID]["lastName"].lower() == key2Party[key]["NACHNAME"].lower()) & (
                PolID2info[PolID]["firstName"].lower() == key2Party[key]["VORNAME"].lower()):
            PolID2info[PolID]["PARTY"] = key2Party[key]['PARTY']
            PolID2info[PolID]["WPs"] = key2Party[key]["WPs"]
            PolID2info[PolID]["BORN"] = key2Party[key]["BORN"]

PolID2info_df = pd.DataFrame.from_dict(PolID2info).T.reset_index().rename(columns={"index": "politicianID"})

for key in id2rede_12_19.keys():

    name = id2rede_12_19[key]['firstName'] + " " + id2rede_12_19[key]['lastName']

    if (name in name2newID.keys()) & (id2rede_12_19[key]['politicianId'] == "-1"):
        id2rede_12_19[key]['politicianId'] = name2newID[name]["new_ID"]

PolID2SpeechID = {}
for key in id2rede_12_19.keys():
    ID = id2rede_12_19[key]['politicianId']
    PolID2SpeechID[str(ID)] = {}
    PolID2SpeechID[str(ID)]["speeches"] = []

for key in id2rede_12_19.keys():
    ID = id2rede_12_19[key]['politicianId']
    PolID2SpeechID[str(ID)]["speeches"].append(key)
    PolID2SpeechID[str(ID)]["firstName"] = id2rede_12_19[key]['firstName']
    PolID2SpeechID[str(ID)]["lastName"] = id2rede_12_19[key]['lastName']

PolID2SpeechID_df = pd.DataFrame.from_dict(PolID2SpeechID).T.reset_index().rename(columns={"index": "politicianID"})
PolID2SpeechID_df["firstName"] = PolID2SpeechID_df["firstName"].map(lambda x: str(x))
PolID2SpeechID_df["surname_lower"] = PolID2SpeechID_df["lastName"].map(lambda x: x.lower())
PolID2SpeechID_df["forename_lower"] = PolID2SpeechID_df["firstName"].map(lambda x: x.lower())
PolID2SpeechID_df["name"] = PolID2SpeechID_df["forename_lower"] + " " + PolID2SpeechID_df["surname_lower"]

# create consistent labels
party_dic = {'B??NDNIS 90/DIE GR??NEN': 'B??NDNIS 90/DIE GR??NEN',
             'PDS': 'PDS',
             'CDU': "CDU/CSU",
             'SPD': 'SPD',
             'FDP': 'FDP',
             'CSU': "CDU/CSU",
             'DIE LINKE.': 'DIE LINKE.',
             'PDS/LL': 'PDS',
             'Plos': 'Parteilos',
             'AfD': 'AfD',
             "CDU/CSU": "CDU/CSU",
             'Parteilos': 'Parteilos'}

# create consistent labels
name_mapping_dic = {'j??rg ganschow': 'J??rg Wolfgang Ganschow',
                    ' gysi': 'Gregor Gysi',
                    'vera lengsfeld': 'Vera Lengsfeld',
                    'j??rgen w m??llemann': 'J??rgen M??llemann',
                    'irmgard schwaetzer': 'Irmgard Adam-Schwaetzer',
                    ' schmidt': np.nan,
                    'andrea gysi': 'Andrea Gysi',
                    'harald b sch??fer': 'Harald B. Sch??fer',
                    'gerhart rudolf baum': 'Gerhart Baum',
                    'margot renesse': 'Margot von Renesse',
                    'christina schenk': np.nan,
                    ' bl??ss': 'Petra Bl??ss',
                    'joachim graf sch??nburg-glauchau': 'Joachim Graf von Sch??nburg-Glauchau',
                    ' wimmer': np.nan,
                    ' sch??fer': np.nan,
                    'peter w reuschenbach': 'Peter Reuschenbach',
                    'de maizi re': 'Thomas de Maiziere',
                    ' heimrich': np.nan,
                    'wolfgang stetten': 'Wolfgang von Stetten',
                    'klein zur wahl des g- -gremiums': np.nan,
                    'ku an sagt': np.nan,
                    'tudjman sagt': np.nan,
                    'teichman logischen': 'Cornelia von Teichman und Logischen',
                    'reinhard schorlemer': 'Reinhard von Schorlemer',
                    'heinrich l kolb': 'Heinrich Leonhard Kolb',
                    'kohl erkl??rt': 'Helmut Kohl',
                    ' stavenhagen': 'Lutz Stavenhagen',
                    'lilo blunck': 'Lieselott Blunck',
                    'susanne jaffke-witt': 'Susanne Jaffke',
                    'kurt j rossmanith': 'Kurt Rossmanith',
                    ' blank': np.nan,
                    'hans with': 'Hans de With',
                    'hans h gattermann': 'Hans H. Gattermann',
                    'joachim poss': np.nan,
                    'helmut kohl gen': 'Helmut Kohl',
                    'gerhard o pfeffermann': 'Gerhard O. Pfeffermann',
                    'michael schmude': 'Michael von Schmude',
                    'uta titze': 'Uta Titze-Stecher',
                    'lutz g stavenhagen': 'Lutz Stavenhagen',
                    'sabine bergmann-pohl parl': 'Sabine Bergmann-Pohl',
                    'ferdi tillmann': 'Ferdinand Tillmann',
                    'gunter weissgerber': 'Gunter Wei??gerber',
                    'peter patema': 'Peter Paterna',
                    'klaus w lippold': 'Klaus Lippold',
                    'heinz werner h??bner': 'Heinz H??bner',
                    'bush sagt sich': np.nan,
                    'dorle marx': 'Dorothea Marx',
                    'bitte sch??n roswitha verh??lsdonk': 'Roswitha Verh??lsdonk',
                    'j??rg essen': 'J??rg van Essen',
                    'herbert meissner': 'Herbert Mei??ner',
                    'erich g fritz': 'Erich G. Fritz',
                    'erika steinbach': 'Erika Steinbach-Hermann',
                    'rita s??ssmuth ??berweisungsvorschlag': 'Rita S??ssmuth',
                    'detlev larcher': 'Detlev von Larcher',
                    'paul k friedhoff': 'Paul Friedhoff',
                    'gisela hilbrecht': np.nan,
                    'rostock-laage hans georg wagner': 'Hans Georg Wagner',
                    'peter h carstensen': 'Peter Harry Carstensen',
                    'werner h skowron': 'Werner Skowron',
                    'christel riemann-hanewinckel': 'Christel Hanewinckel',
                    'alois graf waldburg-zeil': np.nan,
                    'barbara h??ii': 'Barbara H??ll',
                    'werner tegtmeier': np.nan,
                    'herr staatssekret??r werner tegtmeier': np.nan,
                    'karl-hermann haack': 'Karl Hermann Haack',
                    'cornelia teichman': np.nan,
                    'brigitte schulte': np.nan,
                    'lothar maizi re': np.nan,
                    'rita s??ssmuth nennen': 'Rita S??ssmuth',
                    'r werner schuster': 'Werner Schuster',
                    'lutz g stavenhagen': 'Lutz Stavenhagen',
                    'kohl zum milliardenkredit lautet': 'Helmut Kohl',
                    'jetzt erkl??rt walter jens': np.nan,
                    'bitte herr staatsminister helmut sch??fer': 'Helmut Sch??fer',
                    'herr staatsminister helmut sch??fer': 'Helmut Sch??fer',
                    'staatsministerin seiler-albring ursula seiler-albring': 'Ursula Seiler-Albring',
                    'karl h fell': 'Karl H. Fell',
                    'kohl hat k??rzlich ausgef??hrt': 'Helmut Kohl',
                    'ursula schmidt': np.nan,
                    'meine damen herren': np.nan,
                    'hans klein konzernabschlu??befreiungsverordnung': 'Hans Klein',
                    'franz kroppenstedt': np.nan,
                    ' dieter-julius-cronenberg': 'Dieter-Julius Cronenberg',
                    'staatsministerin bitte ursula seiler-albring': 'Ursula Seiler-Albring',
                    'gernot eder': 'Gernot Erler',
                    'helmuth becker berichterstattung': "Helmuth Becker",
                    'bertold reinartz': 'Bertold Mathias Reinartz',
                    ' klaus-dieter-feige': 'Klaus-Dieter Feige',
                    'dieter-julius cronenberg berichterstattung': 'Dieter-Julius Cronenberg',
                    'erich maass': 'Erich Maa??',
                    'bitte staatsministerin ursula seiler-albring': 'Ursula Seiler-Albring',
                    'bitte staatsminister ursula seiler-albring': 'Ursula Seiler-Albring',
                    'helmut kohl tatsache ist': 'Helmut Kohl',
                    'andreas b??low': 'Andreas von B??low',
                    'frerich g??rts': np.nan,
                    'rita s??ssmuth berichterstattung': 'Rita S??ssmuth',
                    ' hans-g??nthertoetemeyer': 'Hans-G??nther Toetemeyer',
                    'hans schuster': 'Hans P. H. Schuster',
                    'carl-detlev hammerstein': 'Carl-Detlev Freiherr von Hammerstein',
                    'reinhard meyer bentrup': 'Reinhard Meyer zu Bentrup',
                    'barbara h??lt': 'Barbara H??ll',
                    'wolfgang geldern': 'Wolfgang von Geldern',
                    'renate schmidt berichterstattung': 'Renate Schmidt',
                    'kohl folgendes versprechen ab': 'Helmut Kohl',
                    'hans a engelhard': 'Hans A. Engelhard',
                    'hans klein berichterstattung': 'Hans Klein',
                    ' kreisky': np.nan,
                    'helmut kohl gestern erneut bekr??ftigte': 'Helmut Kohl',
                    'ulrich inner': 'Ulrich Irmer',
                    'wolfgang gr??bl part': 'Wolfgang Gr??bl',
                    'wolfgang gr??bl parl': 'Wolfgang Gr??bl',
                    'hans klein ??berweisungsvorschlag': 'Hans Klein',
                    'joseph-todor blank': 'Joseph-Theodor Blank',
                    'fujimori gesagt': np.nan,
                    'frerich g??rts': np.nan,
                    'bitte herr staatsminister anton pfeifer': 'Anton Pfeifer',
                    'bitte helmut sch??fer': 'Helmut Sch??fer',
                    'hans klein tagesordnungspunkt': 'Hans Klein',
                    'cristian schmidt': 'Christian Schmidt',
                    'dieter w??rzen': np.nan,
                    'helmut schmidt hat einmal gesagt': np.nan,
                    'johannes v??cking': np.nan,
                    'wilhelm knittel': np.nan,
                    'helmut schmidt': np.nan,
                    'roosevelt hat einmal formuliert': np.nan,
                    'klein darauf hingewiesen': np.nan,
                    'walter priesnitz': np.nan,
                    'heinemann immer wieder mahnend vorhielten': np.nan,
                    'staatsminister ursula seiler-albring': 'Ursula Seiler-Albring',
                    'minister ursula seiler-albring': 'Ursula Seiler-Albring',
                    'staatsministerin ursula seiler-albring': 'Ursula Seiler-Albringi',
                    'bitte herr staatsminister bernd schmidbauer': 'Bernd Schmidbauer',
                    'walesa wie folgt formuliert hat': np.nan,
                    'helmut scholz': np.nan,
                    'helmuth becker ??berweisungsvorschlag': 'Helmuth Becker',
                    'baldur wagner': np.nan,
                    'kohl stellt fest': 'Helmut Kohl',
                    'renate schmidt ??berweisungsvorschlag': 'Renate Schmidt',
                    'peter bl??ss': 'Petra Bl??ss',
                    'reante schmidt': 'Renate Schmidt',
                    's??dkoreas fragte': np.nan,
                    'herr staatsminister bernd schmidbauer': 'Bernd Schmidbauer',
                    'dieter-julius cronenberg ??berweisungsvorschlag': 'Dieter-Julius Cronenberg',
                    'kohl mehr noch': 'Helmut Kohl',
                    'wighard h??rdtl': np.nan,
                    'dietrich vogel': np.nan,
                    'franz-josef feiter': np.nan,
                    'wilhelm knittel': np.nan,
                    'bitte herr staatssekret??r clemens stroetmann': np.nan,
                    'clemens stroetmann': np.nan,
                    'walesa es unumwunden ausgesprochen': np.nan,
                    'franz-josef feiter': np.nan,
                    'herr staatssekret??r clemens stroetmann': np.nan,
                    'clemens stroetmann': np.nan,
                    'wighard h??rdtl': np.nan,
                    'hans klein rhythmus -': 'Hans Klein',
                    'bitte staatsministerire ursula seiler-albring': 'Ursula Seiler-Albring',
                    'gemot eder': 'Gernot Erler',
                    'dieter-julius cronenberg tagesordnungspunkt k': 'Dieter-Julius Cronenberg',
                    'dieter vogel': np.nan,
                    'helmuth becker tagesordnungspunkt e': 'Helmuth Becker',
                    'bitte sehr helmut sch??fer': np.nan,
                    'jelzin in seiner rede sagt': np.nan,
                    'hans klein der text lautet': 'Hans Klein',
                    'roman herzog': np.nan,
                    'roman herzog l??ndern': np.nan,
                    'ursula seiler-aibring': 'Ursula Seiler-Albring',
                    'hans klein tagesordnungspunkt m': 'Hans Klein',
                    'helmut kohl hat daran erinnert': 'Helmut Kohl',
                    'heinrich graf einsiedel': 'Heinrich Graf von Einsiedel',
                    'joseph fischer': np.nan,
                    'marieluise beck': np.nan,
                    'kohl formulierte': 'Helmut Kohl',
                    'herr staatsminister werner hoyer': 'Werner Hoyer',
                    'norbert r??ngen': 'Norbert R??ttgen',
                    ' hans-ulrich': np.nan,
                    'eckart klaeden': 'Eckart von Klaeden',
                    'heidi wright': 'Heidemarie Wright',
                    'antje vollmer berichterstattung': 'Antje Vollmer',
                    'havel zitieren': np.nan,
                    'havel hat folgendes erkl??rt': np.nan,
                    'hans-ulrich klose berichterstattung': 'Hans-Ulrich Klose',
                    'wolfgang llte': 'Wolfgang Ilte',
                    'dagmar g w??hrl': 'Dagmar W??hrl',
                    'frage des abgeordneten wolfgang steiger': np.nan,
                    'bitte franz-josef feiter': np.nan,
                    'manfred overhaus': np.nan,
                    'r manfred overhaus': np.nan,
                    'kohl zugeschickt': 'Helmut Kohl',
                    'hans-ulrich klose tagesordnungspunkt': 'Hans-Ulrich Klose',
                    'reagan gesagt': np.nan,
                    ' carl-ludwig': 'Carl-Ludwig Thiele',
                    'karl jung': np.nan,
                    'chirac premierminister jupp einig': np.nan,
                    'antje vollmer ??berweisungsvorschlag': 'Antje Vollmer',
                    'petra blass': 'Petra Bl??ss',
                    'hans-ulrich klose zusatzpunkt': 'Hans-Ulrich Klose',
                    'adelheid tr??scher': 'Adelheid D. Tr??scher',
                    'heinz dieter essmann': np.nan,
                    'gottfried trager': 'Gottfried Tr??ger',
                    'karl jung': np.nan,
                    'wolfensohn u a': 'Renate Schmidt',
                    'antje vollmer tagesordnungspunkt': 'Antje Vollmer',
                    'j??rgen stark': np.nan,
                    'bitte j??rgen stark': np.nan,
                    'clinton hat recht gesagt': np.nan,
                    'izetbegovic gesagt hat ist richtig': np.nan,
                    'hans-ulrich klose ??berweisungsvorschlag': 'Hans-Ulrich Klose',
                    'bernd klaussner': 'Bernd Klau??ner',
                    'bitte werner hoyer': 'Werner Hoyer',
                    'helmut schmidt abl??sten sagten sie': np.nan,
                    'hans klein tue ich hiermit': np.nan,
                    'bitte herr staatssekret??r helmut sch??fer': 'Helmut Sch??fer',
                    'burkhard hirsch richtigen': 'Burkhard Hirsch',
                    'antje vollmer sagt': 'Antje Vollmer',
                    'hans klein zusatzpunkt': 'Hans Klein',
                    'hans-friedrich ploetz': np.nan,
                    'bitte sch??n bernd schmidbauer': 'Bernd Schmidbauer',
                    'karl a lamers': 'Karl A. Lamers',
                    'hans-ulrich klose gesordnung folgendes mit': 'Hans-Ulrich Klose',
                    'hirsch hat mit recht gesagt': np.nan,
                    'kohl sagt': 'Helmut Kohl',
                    'herzog hingewiesen hat': 'Gustav Herzog',
                    'burkhard hirsch ??berweisungsvorschlag': 'Burkhard Hirsch',
                    'herr staatssekret??r helmut sch??fer': 'Helmut Sch??fer',
                    'bitte sch??n helmut sch??fer': 'Helmut Sch??fer',
                    'peter hausmann': np.nan,
                    'walter hirrlinger fordert': np.nan,
                    'marielulse beck': np.nan,
                    'peter hausmann': np.nan,
                    'hans-ulrich klose w??rtlich gesagt': 'Hans-Ulrich Klose',
                    'kiesinger vor dem hohen hause': np.nan,
                    'milosevi gerichtet': np.nan,
                    'mandela hat gesagt': np.nan,
                    'milo evi': np.nan,
                    'heinz-georg seifert': np.nan,
                    'bitte sch??n werner hoyer': 'Werner Hoyer',
                    'michaela geiger berichterstattung': 'Michaela Geiger',
                    'burkhard hirsch berichterstattung': 'Burkhard Hirsch',
                    'herzog stammt der satz': np.nan,
                    'bitte herr staatssekret??r wilhelm hecker': np.nan,
                    'wilhelm hecker': np.nan,
                    'herr staatssekret??r wilhelm hecker': np.nan,
                    'herr staatsminister bitte helmut sch??fer': 'Helmut Sch??fer',
                    'michaela geiger tagesordnungspunkt a': 'Michaela Geiger',
                    'herzog in stra??burg gesagt hat': np.nan,
                    'michaela geiger zusatzpunkt': 'Michaela Geiger',
                    ' carl-ludwig-thiele': 'Carl-Ludwig Thiele',
                    'hans-ulrich klose gestimmt': 'Hans-Ulrich Klose',
                    'hans-ulrich klose tagesordnungspunkt u': 'Hans-Ulrich Klose',
                    'helmut kohl erstens': 'Helmut Kohl',
                    'herzog gepr??gt hat': 'Gustav Herzog',
                    'hans-ulrich klose abgegebene stimmen': 'Hans-Ulrich Klose',
                    'burkhard hirsch zusatzpunkt': 'Burkhard Hirsch',
                    'jelzin sagte ihm': np.nan,
                    'christa thoben': np.nan,
                    'michaela geiger ??berweisungsvorschlag': 'Michaela Geiger',
                    'milosevic mu?? wissen': np.nan,
                    'herzog gesagt hat': np.nan,
                    'herzog es meinte': np.nan,
                    'willi hausmann': np.nan,
                    'antje vollmer zusatzpunkt': 'Antje Vollmer',
                    'hans-ulrich klose tagesordnungspunkt i': 'Hans-Ulrich Klose',
                    'hans-ulrich klose tagesordnungspunkt q': 'Hans-Ulrich Klose',
                    'hans-ulrich klose tagesordnungspunkt o': 'Hans-Ulrich Klose',
                    'hans-ulrich klose tagesordnungspunkt x': 'Hans-Ulrich Klose',
                    'hans-ulrich klose zusatzpunkt a': 'Hans-Ulrich Klose',
                    'bodo hombach': np.nan,
                    'werner m??ller': "Hans-Werner M??ller",
                    'karl-heinz funke': np.nan,
                    'christine bergmann': np.nan,
                    'willy brandt gesagt': np.nan,
                    'schr??der weiter steht da': np.nan,
                    'klaus wolfgang m??ller': np.nan,
                    'wolfgang gehrcke-reymann': "Wolfgang Gehrcke",
                    'ulla l??tzer': "Ursula L??tzer",
                    'kohl nicht gegeben h??tte': np.nan,
                    'schr??der der bundeskanzler weiter': np.nan,
                    'ernst ulrich weizs??cker': "Ernst Ulrich von Weizs??cker",
                    'axel e fischer': "Axel Fischer",
                    'michael naumann': np.nan,
                    'klima sagt': np.nan,
                    'schr??der ??brigens': np.nan,
                    'ahtisaari die nachricht aus belgrad': np.nan,
                    'karl lamers -karl lamers': "",
                    'schr??der auf': np.nan,
                    'schr??der gesagt': np.nan,
                    'schr??der klar sein': np.nan,
                    'annette fugmann-heesing': np.nan,
                    'roland koch': np.nan,
                    'johannes rau': np.nan,
                    'schr??der sagen': np.nan,
                    'hoover erkl??rte': np.nan,
                    'eckart werthebach': np.nan,
                    'ren r??spel': "Ren?? R??spel",
                    'uwe-karsten heye': np.nan,
                    'reinhard klimmt': np.nan,
                    'vizepr??sidentin petra bl??ss': "Petra Bl??ss",
                    'gespr??che ??ber die frage': np.nan,
                    'schr??der h??tte ihnen gesagt': np.nan,
                    'cajus caesar': "Cajus Julius Caesar",
                    'kollegin - rita streb-hesse': np.nan,
                    'rainer baake': np.nan,
                    'schr??der in dem er mitteilt': np.nan,
                    'hirrlinger sagt dazu': np.nan,
                    ' christaluft': "Christa Luft",
                    'grietje staffelt': np.nan,
                    'christoph st??lzl': np.nan,
                    'willfried maier': np.nan,
                    'hartmuth wrocklage': np.nan,
                    'achim gro??mann parl staatssekret??r beim bundesminister f??r verkehr bau- wohnungswesen': "Achim Gro??mann",
                    'erich stather': np.nan,
                    'peter haupt': np.nan,
                    'johannes rau schlie??en': np.nan,
                    'ihnen gesagt h??tte': np.nan,
                    'schr??der bekr??ftigt hat': np.nan,
                    'schr??der in seiner regierungserkl??rung ausgef??hrt': np.nan,
                    ' santer': np.nan,
                    'erwin anton jordan': np.nan,
                    'schr??der f??hrte weiter aus': np.nan,
                    'julian nida-r??melin': np.nan,
                    'gerhard schr??der gesagt': np.nan,
                    'gunter pleuger': np.nan,
                    'schr??der wort gemeldet hat': np.nan,
                    'herr staatssekret??r erich stather': np.nan,
                    'schr??der knallhart gesagt': np.nan,
                    'peter kurth': np.nan,
                    'schr??der hat recht': np.nan,
                    'mosdorf oder so wie jetzt': np.nan,
                    'rau hat gesagt': np.nan,
                    'bush sagte sie': np.nan,
                    ' hundt': np.nan,
                    'kohl dort hei??t es': np.nan,
                    'ebtekar - deutlich geworden': np.nan,
                    'herr m??llemann ist - steht': np.nan,
                    'schr??der gesagt hat': np.nan,
                    'bush hat dar??ber gesprochen': np.nan,
                    'theodor heuss als er sagte': np.nan,
                    'helmut schmidt hat gesagt': np.nan,
                    'thomas flierl': np.nan,
                    'schr??der gilt': np.nan,
                    'ronald b schill': np.nan,
                    'kohl sagte': np.nan,
                    'manfred stolpe': np.nan,
                    'wolfgang stolpe': np.nan,
                    'schr??der vollmundig aufgestellte forderung': np.nan,
                    'silke stokar neuforn': np.nan,
                    'christina weiss': np.nan,
                    'helmut schmidt hat v??llig recht': np.nan,
                    'cornelia mayer': np.nan,
                    'christian stetten': np.nan,
                    'dorothee b??r': np.nan,
                    'karl-theodor guttenberg': "Karl-Theodor zu Guttenberg",
                    'karzai hat uns gesagt': np.nan,
                    'b la anda': np.nan,
                    'spd-mitglied roland sch??fer': np.nan,
                    'chirac hat immer gesagt': np.nan,
                    'josef philip winkler': "Josef Winkler",
                    'wolfgang thierse berichterstattung': "Wolfgang Thierse",
                    'schr??der unter anderem erkl??rt': np.nan,
                    ' bundeskanzleramtes': np.nan,
                    'hermann otto solms ??berweisungsvorschlag': "Hermann Otto Solms",
                    'klaus werner jonas': "Klaus-Werner Jonas",
                    'b la anda': np.nan,
                    'wolfgang thierse ??berweisungsvorschlag': "Wolfgang Thierse",
                    'gerhard schr??der zentrale aussage war': "Gerhard Schr??der",
                    'schr??der ??u??erte sich dazu w??rtlich': np.nan,
                    'braun hat gestern gesagt': np.nan,
                    'wolfgang b??hmer': np.nan,
                    'schr??der ??brigens seinem vorg??nger': np.nan,
                    'schr??der verk??ndet dazu': np.nan,
                    'roger kusch': np.nan,
                    'hermann otto solms berichterstattung': "Hermann Otto Solms",
                    'professor hans-j??rg jacobsen': np.nan,
                    'ist das richtig kerstin m??ller': "Kerstin M??ller",
                    ' deutschland': np.nan,
                    'hermann otto solms landwirtschaft': "Hermann Otto Solms",
                    'norbert lammert berichterstattung': "Norbert Lammert",
                    'harald wolf': np.nan,
                    'elvira drobinski-weiss': "Elvira Drobinski-Wei??",
                    'dieter althaus': np.nan,
                    'professor horst k??hler': np.nan,
                    'gerhard schr??der erwarten': "Gerhard Schr??der",
                    'recht hat': np.nan,
                    'bush nimmt': np.nan,
                    'kiesinger in einer regierungserkl??rung': np.nan,
                    'herr koch-weser': np.nan,
                    'eisenhower hat gesagt': np.nan,
                    'schr??der dem bosnischen ministerpr??sidenten mitgeteilt': np.nan,
                    'schr??der wortw??rtlich geantwortet': np.nan,
                    'schr??der soll geantwortet haben': np.nan,
                    'sch??ssel erkl??rt mit stolz': np.nan,
                    'staatsministerin christina weiss': np.nan,
                    'k??hler sagen': np.nan,
                    'aus verantwortung f??r unser land': np.nan,
                    'ziercke sagte in demselben interview': np.nan,
                    'thumann hat festgestellt': np.nan,
                    'israel besuchte sagte damals': np.nan,
                    'schr??der nur empfehlen': np.nan,
                    'carl-eduard bismarck': "Carl-Eduard von Bismarck",
                    'sie gefragt hat': np.nan,
                    'k??hler zitieren': np.nan,
                    'ursula der leyen': "Ursula von der Leyen",
                    'thomas maizi re': np.nan,
                    'vizepr??sident wolfgang thierse': "Wolfgang Thierse",
                    'wolfgang neskovic': "Wolfgang Ne??kovi??",
                    'merkel -': "Angela Merkel",
                    'doroth e menzner': "Doroth??e Menzner",
                    'recht geben': np.nan,
                    'sevim da delen': "Sevim Da??delen",
                    'heidrun bluhm-f??rster': "Heidrun Bluhm",
                    'gerhard schr??der hatte gesagt': "Gerhard Schr??der",
                    'hakki keskin': "Hakk?? Keskin",
                    'heuss hat einmal gesagt': np.nan,
                    'angela merkel zitieren': np.nan,
                    'max lehmer': "Maximilian Lehmer",
                    'gerda hasselfeldt ??berweisungsvorschlag': "Gerda Hasselfeldt",
                    'herr l??mmel - andreas g l??mmel': "Andreas L??mmel",
                    'edmund peter geisen': "Edmund Geisen",
                    'schr??der der sagt': np.nan,
                    'philipp missfelder': "Philipp Mi??felder",
                    'bernd neumann die folgende antwort': np.nan,
                    'katrin g??ring-eckardt tagesordnungspunkt b': "Katrin G??ring-Eckardt",
                    'merkel gesagt hat': "Angela Merkel",
                    'schr??der schon fr??her sagte': np.nan,
                    'wolfgang ne kovi': "Wolfgang Ne??kovi??",
                    'klaus b??ger': np.nan,
                    'norbert lammert ??berweisungsvorschlag': "Norbert Lammert",
                    'mbeki sagt': np.nan,
                    'petra pau tagesordnungspunkt': "Petra Pau",
                    'merkel hat gesagt': np.nan,
                    'angela merkel sagen': np.nan,
                    'katrin g??ring-eckardt ??berweisungsvorschlag': "Katrin G??ring-Eckardt",
                    'katrin g??ring-eckardt tagesordnungspunkt f': "Katrin G??ring-Eckardt",
                    'hien auf nachfrage': np.nan,
                    'gerda hasselfeldt tagesordnungspunkt e': "Gerda Hasselfeldt",
                    'angela merkel enthalten': "Angela Merkel",
                    'gerda hasselfeldt tagesordnungspunkt i': "Gerda Hasselfeldt",
                    'inhalt der gesetzes??nderungen ist': np.nan,
                    'katrin lompscher': np.nan,
                    'hans bernhard beus': np.nan,
                    'horst k??hler schlie??en': np.nan,
                    'gerda hasselfeldt tagesordnungspunkt d': "Gerda Hasselfeldt",
                    'gerda hasselfeldt tagesordnungspunkt j': "Gerda Hasselfeldt",
                    'putin gerichtet sage ich': np.nan,
                    'bitte gernot erler': "Gernot Erler",
                    'norbert lammert ??berwiesen': "Norbert Lammert",
                    'h??seyin-kenan aydin bundespr??sident herzog': "H??seyin-Kenan Aydin",
                    'herzog bundeskanzler kohl au??enminister fischer': "Helmut Kohl",
                    'katrin g??ring-eckardt tagesordnungspunkt k': "Katrin G??ring-Eckardt",
                    'katrin g??ring-eckardt zusatzpunkt': "Katrin G??ring-Eckardt",
                    'katrin g??ring-eckardt berichterstattung': "Katrin G??ring-Eckardt",
                    'angela merkel ich sage': "Angela Merkel",
                    'bush in tirana erkl??rt hat': np.nan,
                    'katrin g??ring-eckardt tagesordnungspunkt o': "Katrin G??ring-Eckardt",
                    'merkel sagte am november': np.nan,
                    'gerda hasselfeldt tagesordnungspunkt': "Gerda Hasselfeldt",
                    'gerda hasselfeldt tagesordnungspunkt b': "Gerda Hasselfeldt",
                    'petra pau ??berweisungsvorschlag': "Petra Pau",
                    'heuss formuliert': np.nan,
                    'petra pau tagesordnungspunkt g': "Petra Pau",
                    'gerda hasselfeldt tagesordnungspunkt l': "Gerda Hasselfeldt",
                    'gerda hasselfeldt zusatzpunkt d': "Gerda Hasselfeldt",
                    'horst k??hler sagte vor kurzem': np.nan,
                    ' frauen': np.nan,
                    'petra pau berichterstattung': "Petra Pau",
                    'frieder meyer-krahmer': np.nan,
                    'kabila wurde die devise ausgegeben': np.nan,
                    'gerda hasselfeldt berichterstattung': "Gerda Hasselfeldt",
                    'angela merkel danken': "	Angela Merkel",
                    'petra pau tagesordnungspunkt i': "Petra Pau",
                    'katrin g??ring-eckardt tagesordnungspunkt l': "Katrin G??ring-Eckardt",
                    'bundesregierung vor einigen monaten gesetzt': np.nan,
                    'katrin g??ring-eckardt tagesordnungspunkt': "Katrin G??ring-Eckardt",
                    'petra pau zusatzpunkt': "Petra Pau",
                    'merkel vor kurzem erkl??rt': np.nan,
                    'katrin g??ring-eckardt tagesordnungspunkt d': "Katrin G??ring-Eckardt",
                    'herr staatsminister bitte hermann gr??he': "Hermann Gr??he",
                    'k??hler hat zur finanzkrise gesagt': np.nan,
                    'gerda hasselfeldt tagesordnungspunkt u': "Gerda Hasselfeldt",
                    'gerda hasselfeldt gr??nbuch ten-v': "Gerda Hasselfeldt",
                    'gerda hasselfeldt tagesordnungspunkt mm': "Gerda Hasselfeldt",
                    'gerda hasselfeldt zusatzpunkt m': "Gerda Hasselfeldt",
                    'gerda hasselfeldt zusatzpunkt w': "Gerda Hasselfeldt",
                    'lammert sagte vor wenigen monaten': np.nan,
                    'philipp r??sler': np.nan,
                    'jan aken': "Jan van Aken",
                    'eckart klaeden': "Eckart von Klaeden",
                    'konstantin notz': "Konstantin von Notz",
                    'memet kilic': "Memet K??l????",
                    'matthias w birkwald': "Matthias Birkwald",
                    'hermann e ott': "Henning Otte",
                    'obama hat drei elemente benannt': np.nan,
                    'gerda hasselfeldt kollegen oppermann bekannt': np.nan,
                    'gabriele molitor': "Gabi Molitor",
                    'gerda hasselfeldt zusatzpunkt': "Gerda Hasselfeldt",
                    'petra pau tagesordnungspunkt e': "Petra Pau",
                    'lisa paus': "Elisabeth Paus",
                    'bitte staatsministerin cornelia pieper': "Cornelia Pieper",
                    'viola cramon-taubadel': "Viola von Cramon-Taubadel",
                    'johann david wadephul': "Johann Wadephul",
                    'patrick sensburg': "Patrick Ernst Sensburg",
                    'jean-claude trichet formulierte es so': np.nan,
                    'hans-georg der marwitz': "Hans-Georg von der Marwitz",
                    'aydan ??zo uz': "Aydan ??zo??uz",
                    'angela merkel passiert ist': np.nan,
                    'was haben ministerpr??sidenten': np.nan,
                    'sonja amalie steffen': "Sonja Steffen",
                    'bitte cornelia pieper': "Cornelia Pieper",
                    'merkel gesagt': np.nan,
                    'herr kampeter': "Steffen Kampeter",
                    'jens b??hrnsen': np.nan,
                    'christian wulff': np.nan,
                    'pieper bitte cornelia pieper': "Cornelia Pieper",
                    'merkel die bildungsrepublik ausgerufen frage': np.nan,
                    'schr??der festgestellt': np.nan,
                    'sarkozy drei vorstellungen': np.nan,
                    'abgeordneten fraktionen ist': np.nan,
                    'august hanning': np.nan,
                    'j??rgen z??llner': np.nan,
                    'herr staatssekret??r werner hoyer': "Werner Hoyer",
                    'kohl der gesagt hat': np.nan,
                    'eduard oswald berichterstattung': "Eduard Oswald",
                    'eduard oswald ??berweisungsvorschlag': "Eduard Oswald",
                    'eduard oswald tagesordnungspunkt': "Eduard Oswald",
                    'abschlie??en m??chte ich mit einem zitat des rheinland-pf??lzischen infrastrukturministers roger lewenz': np.nan,
                    'dazu hat der kollege stadler': np.nan,
                    's??dafrikas hat einmal gesagt': np.nan,
                    'eduard oswald halber frage ich': np.nan,
                    'zeit gelassen': np.nan,
                    'obama gesagt hat war': np.nan,
                    'pieper bereit cornelia pieper': np.nan,
                    'eduard oswald tagesordnungspunkt g': np.nan,
                    'merkel sagen': np.nan,
                    'michael link': np.nan,
                    'angela merkel daraus folgt': np.nan,
                    'j??rg polheim': "J??rg von Polheim",
                    'joachim gauck': np.nan,
                    'hans-j??rgen beerfeltz': np.nan,
                    'bundesministerium der finanzen': np.nan,
                    'bundesministerium f??r arbeit soziales': np.nan,
                    'horst k??hler sagte': np.nan,
                    'merkel haben gesagt': np.nan,
                    'merkel hier auf': np.nan,
                    'merkel hier hingestellt gesagt': np.nan,
                    'detlef scheele': np.nan,
                    'johanna wanka': np.nan,
                    'schr??der einmal skizziert': np.nan,
                    'cornelia pr??fer-storcks': np.nan,
                    'ronald reagan': np.nan,
                    'gauck der gesagt hat': np.nan,
                    'dieter hundt hat ausgef??hrt': np.nan,
                    'grillo hat des weiteren gesagt': np.nan,
                    'eduard oswald tagesordnungspunkt s': "Eduard Oswald",
                    'katrin g??ring-eckardt tagesordnungspunkt cc': "Katrin G??ring-Eckardt",
                    'katrin g??ring-eckardt tagesordnungspunkt yy': "Katrin G??ring-Eckardt",
                    'pia zimmermann': "Pia-Beate Zimmermann",
                    'alexander s neu': "Alexander Neu",
                    'johanna wanka': np.nan,
                    'manuela schwesig': np.nan,
                    'ja thomas maizi re': np.nan,
                    'andr hahn': np.nan,
                    'johannes singhammer ??berweisungsvorschlag': "Johannes Singhammer",
                    'angela merkel damals sagte': np.nan,
                    'charles m huber': "Charles M. Huber",
                    'ulle schauws': "Ursula Schauws",
                    'peter hintze tagesordnungspunkt b': "Peter Hintze",
                    'peter hintze tagesordnungspunkt': "Peter Hintze",
                    'michaela engelmeier': "Michaela Engelmeier-Heite",
                    'staatsministerin maria b??hmer': "Maria B??hmer",
                    'philipp graf lerchenfeld': "Philipp Graf von und zu Lerchenfeld",
                    'elisabeth motschmann': "Elisabeth Charlotte Motschmann",
                    'volker ullrich': "Volker Michael Ullrich",
                    ' janukowitsch': np.nan,
                    'gauck geschrieben hat sie schrieb': np.nan,
                    'petra pau tagesordnungspunkt b': "Petra Pau",
                    'edelgard bulmahn ??berweisungsvorschlag': "Edelgard Bulmahn",
                    'sylvia j??rrissen': "Sylvia J??rri??en",
                    'kees vries': "Kees de Vries",
                    'matern marschall': "Matern von Marschall",
                    'alois rainer': "Alois Georg Josef Rainer",
                    'joachim gauck diese zeilen': np.nan,
                    'gauck zitieren': np.nan,
                    'ulla schmidt ??berweisungsvorschlag': "Ulla Schmidt",
                    'bitte herr staatsminister michael roth': "Michael Roth",
                    'andr berghegger': "Andr?? Berghegger",
                    'christina jantz-herrmann': "Christina Jantz",
                    'edelgard bulmahn tagesordnungspunkt': "Edelgard Bulmahn",
                    'joachim gauck als er sagte': np.nan,
                    ' putin': np.nan,
                    'jochim pfeiffer': "Joachim Pfeiffer",
                    ' drobinski-wei??': "Elvira Drobinski-Wei??",
                    'abstimmung ??ber den einzelplan': np.nan,
                    'peter hintze tagesordnungspunkt f': "Peter Hintze",
                    'peter hintze tagesordnungspunkt l': "Peter Hintze",
                    'barsani hat mir gesagt': np.nan,
                    'herr staatssekret??r bitte klaus-dieter fritsche': "",
                    'klaus-dieter fritsche': np.nan,
                    'herr staatsminister bitte michael roth': "Michael Roth",
                    'herr staatsminister michael roth': "Michael Roth",
                    'gauck hat es treffend formuliert': np.nan,
                    'jutta blankau-rosenfeldt': np.nan,
                    'claudia roth ??berweisungsvorschlag': "Claudia Roth",
                    'edelgard bulmahn tagesordnungspunkt iv e': "Edelgard Bulmahn",
                    'edelgard bulmahn drucksache ??berweisungsvorschlag': "Edelgard Bulmahn",
                    'obama unter freunden': np.nan,
                    'putin aussehen muss man sagen': np.nan,
                    'claudia roth schlie??ungsantrag ist abgelehnt': "Claudia Roth",
                    'ulla schmidt tagesordnungspunkt': "Ulla Schmidt",
                    'danke michael roth': "Michael Roth",
                    'peter hintze ??berweisungsvorschlag': "Peter Hintze",
                    'claudia roth tagesordnungspunkt b': "Claudia Roth",
                    'agenda zur??ckdr??ngen': np.nan,
                    'claudia roth zusatzpunkt d': "Claudia Roth",
                    'ulla schmidt drucksache ??berweisungsvorschlag': "Ulla Schmidt",
                    'obama doch einmal eins erkl??ren': np.nan,
                    'ulla schmidt zusatzpunkt': "Ulla Schmidt",
                    'johannes singhammer tagesordnungspunkt i': "Johannes Singhammer",
                    ' entscheidungen': np.nan,
                    'putin war': np.nan,
                    'willy brandt abschlie??en': np.nan,
                    ' herausforderung': np.nan,
                    'erdogan seiner politik gilt': np.nan,
                    'frankreichs hat gesagt': np.nan,
                    'herr dobrindt': np.nan,
                    'putin gegen??ber deutlich gemacht -': np.nan,
                    'ank??ndigungsprogramm bleibt': np.nan,
                    'malu dreyer': np.nan,
                    'lammert ich erinnere mich noch': np.nan,
                    'helmut schmidt formulierte das so': np.nan,
                    'claudia roth https': "Claudia Roth",
                    'merkel die heute sagte': np.nan,
                    'joachim gauck gesagt hat': np.nan,
                    'petra pau zusatzpunkt f': np.nan,
                    'eva-maria schreiber': "Eva Schreiber",
                    'elvan korkmaz-emre': "Elvan Korkmaz",
                    'bettina margarethe wiesmann': "Bettina Wiesmann",
                    'franziska giffey': np.nan,
                    'svenja schulze': np.nan,
                    'helin evrim sommer': np.nan,
                    'thomas opperman': np.nan,
                    'norbert maria altenkamp': "Norbert Altenkamp",
                    'britta katharina dassler': "Britta Dassler",
                    'andreas geisel': np.nan,
                    'nezahat baradari': np.nan,
                    'gerhard zickenheiner': np.nan,
                    'peter heidt': np.nan,
                    'isabel mackensen': np.nan,
                    'elke breitenbach': np.nan,
                    'sandra bubendorfer-licht': np.nan,
                    'joe weingarten': np.nan,
                    'sylvia lehmann': np.nan,
                    'bela bach': np.nan,
                    'matthias n??lke': np.nan,
                    'charlotte schneidewind-hartnagel': np.nan,
                    'dorothee martin': np.nan,
                    'saskia ludwig': np.nan,
                    'janosch dahmen': np.nan,
                    'christian natterer': np.nan,
                    'christopher gohl': np.nan,
                    'maika friemann-jennert': np.nan}


def apply_mapping(mapping_dic, value):
    if value in mapping_dic.keys():
        res = mapping_dic[value]
    else:
        res = np.nan
    return res


PolID2SpeechID_df_copy = PolID2SpeechID_df.copy()
PolID2SpeechID_df["mapped_name"] = PolID2SpeechID_df["name"].map(lambda x: apply_mapping(name_mapping_dic, x))

def get_forename(str_):
    if type(str_) == str:
        res = ' '.join(str_.split(" ")[:-1]).lower()
    elif type(str_) == float:
        res = np.nan

    return res

def get_surname(str_):
    if type(str_) == str:
        res = str_.split(" ")[-1].lower()
    elif type(str_) == float:
        res = np.nan

    return res


PolID2SpeechID_df["forename_lower_mapped"] = PolID2SpeechID_df["mapped_name"].map(lambda x: get_forename(x))
PolID2SpeechID_df["surname_lower_mapped"] = PolID2SpeechID_df["mapped_name"].map(lambda x: get_surname(x))

PolID2SpeechID_df["surname_lower_comb"] = np.where(PolID2SpeechID_df['surname_lower_mapped'].isna() != True,
                                                   PolID2SpeechID_df['surname_lower_mapped'],
                                                   PolID2SpeechID_df['surname_lower'])

PolID2SpeechID_df["forename_lower_comb"] = np.where(PolID2SpeechID_df['forename_lower_mapped'].isna() != True,
                                                    PolID2SpeechID_df['forename_lower_mapped'],
                                                    PolID2SpeechID_df['forename_lower'])

merged_df = pd.merge(PolID2SpeechID_df[
                     ['politicianID', 'speeches', 'mapped_name', 'forename_lower_mapped', 'surname_lower_mapped',
                      'surname_lower_comb', 'forename_lower_comb']],
                 ID_and_names_from_Wiki,
                 how="outer",
                 right_on=["forename_lower", "surname_lower"],
                 left_on=["forename_lower_comb", "surname_lower_comb"])

unique_wikiIDs = [item for item in merged_df["wikidataid"].unique() if item is not np.nan]

speeches_for_df = []
unique_wikiIDs_for_df = []
politicianIDs_for_df = []

for wikiID in unique_wikiIDs:
    length = len(merged_df[merged_df["wikidataid"] == wikiID]["speeches"])

    politicianIDs_for_df.append(max(list(merged_df[merged_df["wikidataid"] == wikiID]["politicianID"])))

    speeches_list = []

    try:
        for i in range(length):

            temp_list = list(merged_df[merged_df["wikidataid"] == wikiID]["speeches"])[i]

            if temp_list is not np.nan:
                speeches_list.append(
                    [int(item) for item in temp_list.replace('[', '').replace(']', '').replace("'", '').split(", ")])

        speeches_list_final = [item for sublist in speeches_list for item in sublist]

        speeches_for_df.append(speeches_list_final)
        unique_wikiIDs_for_df.append(wikiID)

    except (AttributeError, TypeError):
        print(wikiID, speeches_list)

NamesAndSpeeches = pd.DataFrame(list(zip(unique_wikiIDs_for_df, speeches_for_df, politicianIDs_for_df)),
                                columns=['WikiIDs', 'speeches', "politicianID"])


def zero_lengths_to_na(speeches):
    if len(speeches) == 0:
        res = np.nan
    else:
        res = speeches

    return res


NamesAndSpeeches["speeches"] = NamesAndSpeeches["speeches"].map(lambda x: zero_lengths_to_na(x))

NamesAndSpeeches_full = pd.merge(NamesAndSpeeches, test5[['surname_lower_comb', 'forename_lower_comb',
                                                          'pageid', 'wikidataid', 'name']], how="left",
                                 left_on=["WikiIDs"], right_on=["wikidataid"])

NamesAndSpeeches_full["speeches"] = NamesAndSpeeches_full["speeches"].map(lambda x: str(x))

NamesAndSpeeches_full = NamesAndSpeeches_full.drop_duplicates()
NamesAndSpeeches_full = NamesAndSpeeches_full.reset_index().drop(columns=["WikiIDs", "index"])

NamesAndSpeeches_full.to_csv(PATH + "processed/NamesAndSpeeches_full.csv")
