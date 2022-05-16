'''
------------------------------------------------------------------------
Script name: helper_functions.py
Purpose of script: helper functions
Dependencies: none
Author: Alexandra Rottenkolber
------------------------------------------------------------------------
'''

# packages

import gensim
import logging
from nltk.tokenize import RegexpTokenizer
logging.propagate = False
from __future__ import print_function, unicode_literals
import warnings
warnings.filterwarnings("ignore",category=DeprecationWarning)
import spacy
spacy_de = spacy.load('de_core_news_sm')
from nltk.stem.snowball import GermanStemmer  # (ignore_stopwords=True)

lemmatizer = GermanStemmer(ignore_stopwords=False)

with open('../../../../../01_data/listofnames.txt', 'r') as infile:
    listofnames = json.load(infile)


def tokenise_n_grams_lemma(data):
    tokenizer = RegexpTokenizer(r'\w+')
    for idx in range(len(data)):
        data[idx] = data[idx].lower()  # Convert to lowercase.
        data[idx] = tokenizer.tokenize(data[idx])  # Split into words.

    # Remove numbers, but not words that contain numbers.
    docs_filter1 = [[token for token in doc if not token.isnumeric()] for doc in data]

    # Remove words that are only one character.
    docs_filter2 = [[token for token in doc if len(token) > 1] for doc in docs_filter1]

    return docs_filter2


def tokenise_n_grams_lemma_nltk_POSTag(data):
    for idx in range(len(data)):
        data[idx] = data[idx].lower()  # Convert to lowercase.
        data[idx] = nltk.word_tokenize(data[idx])  # Split into words.

    # Remove numbers, but not words that contain numbers.
    docs_filter1 = [[token for token in doc if not token.isnumeric()] for doc in data]

    # Remove words that are only one character.
    docs_filter2 = [[token for token in doc if len(token) > 1] for doc in docs_filter1]

    docs_filter2_tagged = [nltk.pos_tag(doc) for doc in docs_filter2]

    return docs_filter2_tagged


def tokenise_n_grams_lemma_spacy_POSTag(data):
    for idx in range(len(data)):
        data[idx] = spacy_de(data[idx])

    # POS Tagging
    docs_filter2_tagged = [[(t.orth_, t.tag_) for t in doc] for doc in data]
    # print(docs_filter2_tagged)

    # keep only nouns
    docs_filter2_tagged_nouns = [[tup[0] for tup in doc if tup[1] in ["NN", "NE"]] for doc in docs_filter2_tagged]

    # Remove words that are only one character.
    docs_filter2 = [[token for token in doc if len(token) > 1] for doc in docs_filter2_tagged_nouns]

    return docs_filter2



def stemming(docs_filter2):
    docs_lemm = [[lemmatizer.stem(token) for token in doc if token not in listofnames] for doc in docs_filter2]
    return docs_lemm


def create_bigrams(docs_lemm):
    # Remove stopwords
    # docs_lemm_clean = [[token for token in doc if token not in stop_words] for doc in docs_lemm]
    # docs_lemm_clean = docs_lemm

    # Compute bigrams and trigrams
    from gensim.models import Phrases

    # Build the bigram and trigram models
    bigram = gensim.models.Phrases(docs_lemm, min_count=10, threshold=5)  # higher threshold fewer phrases.
    trigram = gensim.models.Phrases(bigram[docs_lemm], min_count=10, threshold=5)

    # Faster way to get a sentence clubbed as a trigram/bigram
    bigram_mod = gensim.models.phrases.Phraser(bigram)
    trigram_mod = gensim.models.phrases.Phraser(trigram)

    # Add bigrams and trigrams to docs (only ones that appear 20 times or more).
    # bigram = Phrases(docs_lemm, min_count=10)

    for idx in range(len(docs_lemm)):
        # for token in bigram[docs_lemm[idx]]:
        for token in trigram_mod[bigram_mod[docs_lemm[idx]]]:
            if '_' in token:
                # print(token)
                # Token is a bigram, add to document.
                docs_lemm[idx].append(token)

    return docs_lemm