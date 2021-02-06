#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Feb  5 15:50:05 2021

@author: eleanordavies
"""

# =============================================================================
# =============================================================================
# Raw Tweet Pre-Processing & Empath Analysis
# =============================================================================
# =============================================================================


# =============================================================================
# Tweet Data Pre-Proccessing
#  
# - Remove URLs from the tweets
# - Tokenize text
# - Remove emails
# - Remove new lines characters
# - Remove distracting single quotes
# - Remove all punctuation signs
# - Lowercase all text
# - Detokenize text
# - Convert list of texts to Numpy array
# =============================================================================

# Importing relevant libraries 
import numpy as np
import pandas as pd
import re
import nltk
from nltk.tokenize.treebank import TreebankWordDetokenizer
import gensim
from gensim.utils import simple_preprocess
print('Loaded processing packages')

## Load raw tweet data 
tweet_text = pd.read_csv('/Users/eleanordavies/Desktop/Twiiter_data5.csv')

# Creating a function to remove characters 
def depure_data(data):
    
    #Removing URLs with a regular expression
    url_pattern = re.compile(r'https?://\S+|www\.\S+')
    data = url_pattern.sub(r'', data)

    # Remove Emails
    data = re.sub('\S*@\S*\s?', '', data)

    # Remove new line characters
    data = re.sub('\s+', ' ', data)

    # Remove distracting single quotes
    data = re.sub("\'", "", data)
        
    return data


# Splitting twtter data text column to list
data_to_list = tweet_text['text'].values.tolist()

# Applying the above function 
temp = []
for i in range(len(data_to_list)):
    temp.append(depure_data(data_to_list[i]))

tempdf = pd.DataFrame(temp)
tempdf.head()


# Detokenize removes all punctation, emojis and puts text into lowercase 
def sent_to_words(sentences):
    for sentence in sentences:
        yield(gensim.utils.simple_preprocess(str(sentence), deacc=True))  # deacc=True removes punctuations
data_words = list(sent_to_words(temp))
print(data_words[:10])


def detokenize(text):
    return TreebankWordDetokenizer().detokenize(text)


data_detoken = []
for i in range(len(data_words)):
    data_detoken.append(detokenize(data_words[i]))
data_detoken = np.array(data_detoken)
data_detoken_df = pd.DataFrame(data_detoken)
data_detoken_df


# add cleaned tweet text as column 'selected_tweets' to original dataframe 
frames = [data_detoken_df,tweet_text]
demo_tweets_clean = pd.concat(frames, axis=1)
demo_tweets_clean = demo_tweets_clean.rename(columns={0: "selected_tweets"})

## Export clean tweet data 
demo_tweets_clean.to_csv('/Users/eleanordavies/Desktop/demo_tweets_clean.csv', index = False)

print('cleaned tweets!')


# =============================================================================
# Empath Analysis 
#
# - Looking at 20 lexical categories 
# - 10 positive 
# - 10 netagive 
# =============================================================================

# Importing relevant libraries for 'Empath' analysis
import empath
import os
import sys
sys.path.insert(0, os.path.abspath('..'))

from empath import Empath
print('Loaded Empath packages')

# creating the lexicon object 
# list of categories within empath 
lexicon = Empath()
list_cat = ['help', 'office', 'dance', 'money', 'wedding', 'domestic_work', 'sleep', 'medical_emergency', 'cold', 'hate', 'cheerfulness', 'aggression', 'occupation', 'envy', 'anticipation', 'family', 'vacation', 'crime', 'attractive', 'masculine', 'prison', 'health', 'pride', 'dispute', 'nervousness', 'government', 'weakness', 'horror', 'swearing_terms', 'leisure', 'suffering', 'royalty', 'wealthy', 'tourism', 'furniture', 'school', 'magic', 'beach', 'journalism', 'morning', 'banking', 'social_media', 'exercise', 'night', 'kill', 'blue_collar_job', 'art', 'ridicule', 'play', 'computer', 'college', 'optimism', 'stealing', 'real_estate', 'home', 'divine', 'sexual', 'fear', 'irritability', 'superhero', 'business', 'driving', 'pet', 'childish', 'cooking', 'exasperation', 'religion', 'hipster', 'internet', 'surprise', 'reading', 'worship', 'leader', 'independence', 'movement', 'body', 'noise', 'eating', 'medieval', 'zest', 'confusion', 'water', 'sports', 'death', 'healing', 'legend', 'heroic', 'celebration', 'restaurant', 'violence', 'programming', 'dominant_heirarchical', 'military', 'neglect', 'swimming', 'exotic', 'love', 'hiking', 'communication', 'hearing', 'order', 'sympathy', 'hygiene', 'weather', 'anonymity', 'trust', 'ancient', 'deception', 'fabric', 'air_travel', 'fight', 'dominant_personality', 'music', 'vehicle', 'politeness', 'toy', 'farming', 'meeting', 'war', 'speaking', 'listen', 'urban', 'shopping', 'disgust', 'fire', 'tool', 'phone', 'gain', 'sound', 'injury', 'sailing', 'rage', 'science', 'work', 'appearance', 'valuable', 'warmth', 'youth', 'sadness', 'fun', 'emotional', 'joy', 'affection', 'traveling', 'fashion', 'ugliness', 'lust', 'shame', 'torment', 'economics', 'anger', 'politics', 'ship', 'clothing', 'car', 'strength', 'technology', 'breaking', 'shape_and_size', 'power', 'white_collar_job', 'animal', 'party', 'terrorism', 'smell', 'disappointment', 'poor', 'plant', 'pain', 'beauty', 'timidity', 'philosophy', 'negotiate', 'negative_emotion', 'cleaning', 'messaging', 'competing', 'law', 'friends', 'payment', 'achievement', 'alcohol', 'liquid', 'feminine', 'weapon', 'children', 'monster', 'ocean', 'giving', 'contentment', 'writing', 'rural', 'positive_emotion', 'musical', 'colors', 'id', 'injury and death', 'demo', 'what']
pos_cat = ["worship", "masculine", "death", "weakness", "divine", "religion", "sleep", "swearing_terms", "injury", "envy"]
neg_cat = ["gain", "reading", "banking", "independence", "programming", "payment", "technology", "tourism", "air_travel", "negotiate"]
tot_cat = pos_cat + neg_cat 


# defining function to analyse sentences 
# can be altered to add category and normalise values 
# may need to specifiy categories 
def empath_analyse(sentence):
    x = lexicon.analyze(sentence, categories=tot_cat)
    return x

# Load clean tweet data 
tweets = pd.read_csv('/Users/eleanordavies/Desktop/demo_tweets_clean.csv')


# Putting selected tweets into a list for analysis 
data_to_list = tweets['selected_tweets'].values.tolist()
data_to_list = np.array(data_to_list)


# Looping each clean tweet text through the Empath analysis 
data =[]
for i in range(len(data_to_list)):
    data.append(empath_analyse(data_to_list[i])),
data_dict = pd.DataFrame.from_dict(data)

result = pd.concat([tweets, data_dict], axis=1, join="inner")


print('Emapth Complete')

# Aggregating 
# Aggregating by date and location (name is location name)
pivot = pd.pivot_table(result,index=[ "tw_date", "name"], 
                       values=tot_cat,aggfunc=[np.mean])
pivot


# Export aggregated Empath analysis on tweets  
result.to_csv('/Users/eleanordavies/Desktop/result.csv', index = False) 


# Counting total number of tweets per date 
pivot_table = result.pivot_table(
     index='tw_date',
     values='id',
     aggfunc= len).reset_index()

pivot_table.columns = ['tw_date','count_of_tweets']


# Aggregating data by date and location (name) and get mean of lexical categories 
mean_pivot_table = result.pivot_table(
    index=[ "tw_date"], # "name"], 
    values=tot_cat,
    aggfunc=np.mean).reset_index()


mean_pivot_table['count'] = mean_pivot_table.tw_date.map(
   pivot_table.set_index('tw_date').count_of_tweets)

mean_pivot_table.to_csv('/Users/eleanordavies/Desktop/mean_pivot_table.csv', index = False) 





