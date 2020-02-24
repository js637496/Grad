import nltk
import csv
import sys
import string
import datetime
import math
from nltk.util import ngrams
from collections import Counter
from multiprocessing import Process
import multiprocessing
import datetime

maxInt = sys.maxsize

while True:
    # decrease the maxInt value by factor 10 
    # as long as the OverflowError occurs.

    try:
        csv.field_size_limit(maxInt)
        break
    except OverflowError:
        maxInt = int(maxInt/10)

news = open("all-the-news/articles1.csv", "r")
wine = open("wine-reviews/winemag-data_first150k.csv", "r")
#news = open("all-the-news/head1000.csv", "r")
#wine = open("wine-reviews/head1000.csv", "r")
#news = open("all-the-news/head500.csv", "r")
#wine = open("wine-reviews/head500.csv", "r")
#news = open("all-the-news/head50.csv", "r")
#wine = open("wine-reviews/head50.csv", "r")
#news = open("all-the-news/head.csv", "r")
#wine = open("wine-reviews/head.csv", "r")

reviews = []
articles = []
with news as csv_file:
    csv_reader = csv.DictReader(csv_file, delimiter=",")
    for lines in csv_reader:
        articles.append(lines["content"])
with wine as csv_file:
    csv_reader = csv.DictReader(csv_file, delimiter=",")
    for lines in csv_reader:
        reviews.append(lines["description"])


reviewsraw = ""
articlesraw = ""
print("READ REVIEWS")
for review in reviews:
    #review = review.translate(str.maketrans('', '', string.punctuation))
    review = review.lower()
    reviewsraw += review + " "
print("READ REVIEWS COMPLETE")
print("READ NEWS")
for article in articles:
    #article = article.translate(str.maketrans('', '', string.punctuation))
    article = article.lower()
    articlesraw += article + " "
print("READ NEWS COMPLETE")
    
#reviewTokens = nltk.word_tokenize(reviewsraw)
print("SENT TOKE REVIEWS")
now = datetime.datetime.now()
print (now.strftime("%Y-%m-%d %H:%M:%S"))
reviewSents = nltk.sent_tokenize(reviewsraw)
print("SENT TOKE REVIEWS COMPLETE")
now = datetime.datetime.now()
print (now.strftime("%Y-%m-%d %H:%M:%S"))
print("SENT TOKE NEWS")
now = datetime.datetime.now()
print (now.strftime("%Y-%m-%d %H:%M:%S"))
#articleTokens = nltk.word_tokenize(articlesraw)
articleSents = nltk.sent_tokenize(articlesraw)
print("SENT TOKE NEWS COMPLETE")
now = datetime.datetime.now()
print (now.strftime("%Y-%m-%d %H:%M:%S"))


def ngram_function(index, sentences, tokenCountDict, unigramsDict, bigramsDict, trigramsDict, stopwords):
    sentCount = 0
    
    #use temp vars to avoid race conditions
    tmpunigramsDict = Counter()
    tmpbigramsDict = Counter()
    tmptrigramsDict = Counter()
    tmptokenCount = 0
    
    for sent in sentences[index]:
        if sentCount % 1000 == 0:
            now = datetime.datetime.now()
            print("Thread " + str(index) + ": " + str(sentCount) + " of " + str(len(sentences[index])) + " " + now.strftime("%Y-%m-%d %H:%M:%S"))
        sent = sent.translate(str.maketrans('', '', string.punctuation))
        words = nltk.word_tokenize(sent)
        for word in list(words):  
            if word in stopwords:
                words.remove(word)
        tmptokenCount += len(words)
        tmpunigramsDict += Counter(ngrams(words, 1))
        tmpbigramsDict += Counter(ngrams(words, 2))
        tmptrigramsDict += Counter(ngrams(words, 3))
        sentCount += 1
    tokenCountDict[index] = tmptokenCount
    unigramsDict[index] = tmpunigramsDict
    bigramsDict[index] = tmpbigramsDict
    trigramsDict[index] = tmptrigramsDict

def processUnigrams(tokenCount, rawCounter, percentDict, countDict):
    for word in rawCounter:
        firstword = list(word)[0]
        count = rawCounter[(firstword,)]
        if count > 0:
            percent = math.log(count / tokenCount)
            percentDict.update({firstword : percent})        
            countDict.update({firstword : count})
        
def processNGrams(rawCounter, reviewUGCount, percentDict, countDict):
    for ngram in rawCounter:
        count = rawCounter[ngram]
        if count > 0:
            firstword = list(ngram)[0]
            if firstword in reviewUGCount.keys():
                total = reviewUGCount[firstword]
                percent = math.log(count / total)
                percentDict.update({ngram : percent})
                countDict.update({ngram : count})

def nGramDictToCSV(path, fileName, theDict):
    theDict = dict(theDict)
    w = csv.writer(open(path + "/" + fileName + ".csv", "w"))
    for key, val in theDict.items():
        w.writerow([key, val])
        
def sortDict(theDict):
    return dict(sorted(theDict.items(), key=lambda y: y[1], reverse=True))

print("START WINE REVIEWS")
reviewTokenCount = 0

reviewUnigrams = Counter()
reviewBigrams = Counter()
reviewTrigrams = Counter()

#I dont want these in my ngram results
stopwords = ["'", "s", "’", "”", "“", "t"]

print("CREATING NGRAM COUNTS - WINE REIVEWS")
now = datetime.datetime.now()
print (now.strftime("%Y-%m-%d %H:%M:%S"))

#This is taking too long, use all cores on the machine.
#Match to the number of threads on the machine
threadCount = 8

#Prep for multithread
interval = math.ceil(len(reviewSents) / threadCount)

manager = multiprocessing.Manager()
reviewThreadDict = manager.dict()
reviewUnigramsDict = manager.dict()
reviewBigramsDict = manager.dict()
reviewTrigramsDict = manager.dict()
reviewTokenCountDict = manager.dict()

for x in range(threadCount):
    reviewThreadDict.update({x : reviewSents[interval*x:interval*(x+1)]})
    reviewUnigramsDict.update({x : {}})
    reviewBigramsDict.update({x : {}})
    reviewTrigramsDict.update({x : {}})
    reviewTokenCountDict.update({x : 0})

print("Review sentence length: " + str(len(reviewSents)))    
    
if __name__ == "__main__":

    threads = list()
    for index in range(threadCount):
        x = Process(target=ngram_function, args=(index, reviewThreadDict, reviewTokenCountDict, reviewUnigramsDict, reviewBigramsDict, reviewTrigramsDict, stopwords))
        threads.append(x)
        print("Starting thread " + str(index))
        x.start()

    for a in range(threadCount):
        thread = threads[a]       
        thread.join()        
        reviewUnigrams += reviewUnigramsDict[a]
        reviewBigrams += reviewBigramsDict[a]
        reviewTrigrams += reviewTrigramsDict[a]
        reviewTokenCount += reviewTokenCountDict[a]
        print("End thread " + str(a))

print("All threads complete.")

now = datetime.datetime.now()
print (now.strftime("%Y-%m-%d %H:%M:%S"))

print("CREATING COUNTER DICTIONARY - WINE REIVEWS")

reviewNGramCounts = {
    "unigrams" : Counter(reviewUnigrams),
    "bigrams" : Counter(reviewBigrams),
    "trigrams" : Counter(reviewTrigrams)
}

reviewUGPercent = {}
reviewUGCount = {}
reviewBGPercent = {}
reviewBGCount = {}
reviewTGPercent = {}
reviewTGCount = {}

print("PROCCESSING UNIGRAMS PERCENTAGE - WINE REVIEWS")
processUnigrams(reviewTokenCount, reviewNGramCounts["unigrams"], reviewUGPercent, reviewUGCount)
print("PROCCESSING BIGRAM PERCENTAGE - WINE REVIEWS")
processNGrams(reviewNGramCounts["bigrams"], reviewUGCount, reviewBGPercent, reviewBGCount)
print("PROCCESSING TRIGRAMS PERCENTAGE - WINE REVIEWS")
processNGrams(reviewNGramCounts["trigrams"], reviewUGCount, reviewTGPercent, reviewTGCount)

print("SORTING WINE UNIGRAM PERCENT")
reviewUGPercent = sortDict(reviewUGPercent)
print("SORTING WINE UNIGRAM COUNT")
reviewUGCount = sortDict(reviewUGCount)
print("SORTING WINE BIGRAM PERCENT")
reviewBGPercent = sortDict(reviewBGPercent)
print("SORTING WINE BIGRAM COUNT")
reviewBGCount = sortDict(reviewBGCount)
print("SORTING WINE TRIGRAM PERCENT")
reviewTGPercent = sortDict(reviewTGPercent)
print("SORTING WINE TRIGRAM COUNT")
reviewTGCount = sortDict(reviewTGCount)

print("WRITING WINE FILES")
print("WRITING WINE unigramCounts")
nGramDictToCSV("wine-reviews/", "unigramCounts", reviewUGCount)
print("WRITING WINE unigramPercent")
nGramDictToCSV("wine-reviews/", "unigramPercent", reviewUGPercent)
print("WRITING WINE bigramCounts")
nGramDictToCSV("wine-reviews/", "bigramCounts", reviewBGCount)
print("WRITING WINE bigramPercent")
nGramDictToCSV("wine-reviews/", "bigramPercent", reviewBGPercent)
print("WRITING WINE trigramCounts")
nGramDictToCSV("wine-reviews/", "trigramCounts", reviewTGCount)
print("WRITING WINE trigramPercent")
nGramDictToCSV("wine-reviews/", "trigramPercent", reviewTGPercent)

print("END WINE REVIEWS")
now = datetime.datetime.now()
print (now.strftime("%Y-%m-%d %H:%M:%S"))

###############################################################

print("START NEWS")
articleTokenCount = 0    

articleUnigrams = Counter()
articleBigrams = Counter()
articleTrigrams = Counter()

print("CREATING NGRAM COUNTS - NEWS")
now = datetime.datetime.now()
print (now.strftime("%Y-%m-%d %H:%M:%S"))

#Prep for multithread
interval = math.ceil(len(articleSents) / threadCount)

articleThreadDict = manager.dict()
articleUnigramsDict = manager.dict()
articleBigramsDict = manager.dict()
articleTrigramsDict = manager.dict()
articleTokenCountDict = manager.dict()

for x in range(threadCount):
    articleThreadDict.update({x : articleSents[interval*x:interval*(x+1)]})
    articleUnigramsDict.update({x : {}})
    articleBigramsDict.update({x : {}})
    articleTrigramsDict.update({x : {}})
    articleTokenCountDict.update({x : 0})

print("News sentence length: " + str(len(reviewSents)))      
    
if __name__ == "__main__":

    threads = list()
    for index in range(threadCount):
        x = Process(target=ngram_function, args=(index, articleThreadDict, articleTokenCountDict, articleUnigramsDict, articleBigramsDict, articleTrigramsDict, stopwords))
        threads.append(x)
        print("Starting thread " + str(index))
        x.start()

    for a in range(threadCount):
        thread = threads[a]
        thread.join()
        articleUnigrams += articleUnigramsDict[a]
        articleBigrams += articleBigramsDict[a]
        articleTrigrams += articleTrigramsDict[a]
        articleTokenCount += articleTokenCountDict[a]
        print("End thread " + str(a))
        
print("All threads complete")
now = datetime.datetime.now()
print (now.strftime("%Y-%m-%d %H:%M:%S"))

print("CREATING COUNTER DICTIONARY - NEWS")
articleNGramCounts = {
    "unigrams" : Counter(articleUnigrams),
    "bigrams" : Counter(articleBigrams),
    "trigrams" : Counter(articleTrigrams)
}

articleUGPercent = {}
articleUGCount = {}
articleBGPercent = {}
articleBGCount = {}
articleTGPercent = {}
articleTGCount = {}

print("PROCCESSING UNIGRAMS PERCENTAGE - NEWS")
processUnigrams(articleTokenCount, articleNGramCounts["unigrams"], articleUGPercent, articleUGCount)
print("PROCCESSING BIGRAM PERCENTAGE - NEWS")
processNGrams(articleNGramCounts["bigrams"], articleUGCount, articleBGPercent, articleBGCount)
print("PROCCESSING TRIGRAM PERCENTAGE - NEWS")
processNGrams(articleNGramCounts["trigrams"], articleUGCount, articleTGPercent, articleTGCount)

print("SORTING NEWS UNIGRAM PERCENT")
articleUGPercent = sortDict(articleUGPercent)
print("SORTING NEWS UNIGRAM COUNT")
articleUGCount = sortDict(articleUGCount)
print("SORTING NEWS BIGRAM PERCENT")
articleBGPercent = sortDict(articleBGPercent)
print("SORTING NEWS BIGRAM COUNT")
articleBGCount = sortDict(articleBGCount)
print("SORTING NEWS TRIGRAM PERCENT")
articleTGPercent = sortDict(articleTGPercent)
print("SORTING NEWS TRIGRAM COUNT")
articleTGCount = sortDict(articleTGCount)

print("WRITING NEWS FILES")
print("WRITING NEWS unigramCounts")
nGramDictToCSV("all-the-news/", "unigramCounts", articleUGCount)
print("WRITING NEWS unigramPercent")
nGramDictToCSV("all-the-news/", "unigramPercent", articleUGPercent)
print("WRITING NEWS bigramCounts")
nGramDictToCSV("all-the-news/", "bigramCounts", articleBGCount)
print("WRITING NEWS bigramPercent")
nGramDictToCSV("all-the-news/", "bigramPercent", articleBGPercent)
print("WRITING NEWS trigramCounts")
nGramDictToCSV("all-the-news/", "trigramCounts", articleTGCount)
print("WRITING NEWS trigramPercent")
nGramDictToCSV("all-the-news/", "trigramPercent", articleTGPercent)

now = datetime.datetime.now()
print (now.strftime("%Y-%m-%d %H:%M:%S"))

print("END NEWS")

print("DONE")
