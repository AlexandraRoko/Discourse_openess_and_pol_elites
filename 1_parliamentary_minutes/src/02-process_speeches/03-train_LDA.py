from gensim.corpora import Dictionary
from gensim.models import LdaModel
import time
import json
import random

PATH = "/Users/alexandrarottenkolber/Documents/02_Hertie_School/Master thesis/Master_Thesis_Hertie/data_analysis/01_data/Plenarprotokolle/"

with open(PATH + 'processed/data_ready_concat_POSTag_all_sorted_with_AfD.txt', 'r') as infile:
    data_ready_concat = json.load(infile)
infile.close()

print("Data loaded")

random.shuffle(data_ready_concat)

print("Data shuffled")

# Create a dictionary representation of the documents.
dictionary = Dictionary(data_ready_concat)

# Filter out words that occur in less than 20 documents, or more than 50% of the documents.
keep_list = ["moria", "luftfilt", "bubendorfer-licht", "covid-19", "curevac", "coronainfektion", "impfstoffproduktion", "coronapandemi", "coronasituation",
"coronakris", "testkapazitat", "coronapatient", "wirtschaftsstabilisierungsfond", "kontaktbeschrank", "sozialschutz-paket", "maskenpflicht", "lockdown",
"coronazeit", "ffp2-mask", "kontaktnachverfolg", "covid-19-pandemi", "pandemiezeit", "coronamassnahm", "coronahilf", "coronabeding", "offnungsstrategi",
"corona-app", "irini", "unternehmerlohn", "coronapolit", "lockdown-kris", "coronaleugn", "corona-steuerhilfegesetz", "coronaimpfstoff", "corona-warn-app",
"pandemierat", "covid", "biontech", "arbeitsschutzkontrollgesetz", "krankenhauszukunftsgesetz", "tichanowskaja", "lockdown-massnahm", "astrazeneca",
"baulandmobilisierungsgesetz", "inzidenzwert", "impfzentr", "novemberhilf", "dezemberhilf", "lockdown-polit", "covax", "geimpft", "impfstoffbeschaff",
"mutant", "bundesnotbrems"]

dictionary.filter_extremes(no_below=1, no_above=0.6, keep_tokens = keep_list)

# Bag-of-words representation of the documents.
corpus = [dictionary.doc2bow(doc) for doc in data_ready_concat]

print("Dictionary ready. Training model now.")

# Set training parameters.
#num_topics = 100
chunksize = 12000
passes = 25
iterations = 1000
eval_every = None  # Don't evaluate model perplexity, takes too much time.

# Make a index to word dictionary.
temp = dictionary[0]  # This is only to "load" the dictionary.
id2word = dictionary.id2token

start_time = time.time()

#for num_topics in [25, 50, 75, 100, 125, 150]:
for num_topics in [100]:

    lda_model = LdaModel(
        corpus=corpus,
        id2word=id2word,
        chunksize=chunksize,
        alpha='auto',
        eta='auto',
        iterations=iterations,
        num_topics=num_topics,
        passes=passes,
        eval_every=eval_every
    )

    print(f'Time taken : {(time.time() - start_time) / 60:.2f} mins for num_topics = {num_topics}')

    # Save model to disk.
    lda_model.save(PATH + f"LDA_models/models/lda_model_concat_POSTag_self_tuned_all_sorted_shuffle_num_topics_{num_topics}_with_Afd")

    print(f"Model {num_topics} saved. DONE.")
