import tweepy
import time
import pandas as pd
import numpy as np
from tweepy import OAuthHandler

# Code taken from tutorial: http://www.craigaddyman.com/mining-all-tweets-with-python/
 
# Insert your own keys and tokens below. Easy to request on twitter.
consumer_key = ''
consumer_secret = ''
access_token = ''
access_secret = ''
 
auth = OAuthHandler(consumer_key, consumer_secret)
auth.set_access_token(access_token, access_secret)
 
api = tweepy.API(auth)

dates = []
tweets = []
lis = [830195857530183684] ## most recent tweet id
for i in range(0, 9): ## get last 1800 tweets
    print("ITERATION", i)
    user_timeline = api.user_timeline(screen_name="realDonaldTrump",count=200, include_retweets=False, max_id=lis[-1])
    time.sleep(1) 
    for tweet in user_timeline:
        print tweet.text
        dates.append(tweet.created_at)
        tweets.append(tweet.text.encode('ascii','ignore'))
        lis.append(tweet.id)

dates= np.array(dates)
tweets = np.array(tweets)
data = np.column_stack((dates,tweets))

df = pd.DataFrame(data,xrange(len(tweets)),columns=['date','tweet'])
print(df)
df.to_csv('trump_tweets.csv')
