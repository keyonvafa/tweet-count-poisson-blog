# Predicting Trump's tweet counts

`scrape_tweets.py` scrapes Trump tweets from Twitter, stored at `trump_tweets.csv`.

`tweet_poisson.R` predicts weekly tweet counts using simple Poisson process, detailed in blog post <a href='http://keyonvafa.com/tweet-counts-poisson-processes/'>Tweet Counts as Poisson Processes</a>.

`tweet_autoregressive.R` predicts weekly tweet count using more complicated Poisson GLM model, detailed in blog post <a href='http://keyonvafa.com/tweet-counts-poisson-glm/'>Tweet Counts as Poisson GLMs</a>.
