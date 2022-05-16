# Measuring the permeability of parliamentary discourse and the role political elites play in shaping it.


Code used to do the emperical analysis. This repo is structured in the following way: 

* **1_parliamentary_minutes** (Python): Code used to (1) clean the parliamentary minutes which were available in .xml format, (2) clean and pre-process the speeches, and (2) train an LDA topic model, (3) calculate how much speeches deviate from each other (KL-divergence).
* **2_Wikipeida** (R and Python): Code used to (1) scrate Wikipedia and Wikidata, (2) retrieve historical versions of Wikipedia articles, (3) construct graph, (4) as well as a dataset containing the Wikipedia data as well as meta data about MPs pulled from the comparative legislator database, (5) perform a Principal Component Analysis, and (6) a factor analysis. 
* **3_combined_data** (R and Python): Code used to (1) combine Wikipedia data and KL-based measures, (2) analyse combined data, (3) plot figures.


## Script requirements for python scrips


| Package name  |  version |   
|---|---|
|  beautifulsoup4 |  4.9.3 |  
|  datetime | 4.0.1  |   
| dicttoxml  | 1.7.4  |  
| gensim | 3.8.3 |  
| glob2| 0.7   |  
| matplotlib|3.3.2 |  
|  nltk|3.5 |  
| nltk|3.5 |    
| numpy|1.19.2 |  
| json5|0.9.5 | 
| pandas|1.2.3| 
| python|3.8.5 |
| regex|2020.10.15  |
| requests|2.24.0|
| scikit-learn|0.23.2  |
| spacy|2.3.2 |
|xmltodict|0.12.0 |




