
tweets = read.csv("trump_tweets.csv",header=TRUE)
date_times = as.POSIXlt(tweets$date)

seconds_in_week = 7*24*60*60

start_date = as.POSIXlt("2016-11-09 12:00:00 EST")
# Since beginning of data: start_date = as.POSIXlt("2016-07-27 12:00:00 EST") 
# Since beginning of presidency: start_date = as.POSIXlt("2017-01-25 12:00:00 EST") 
most_recent_date = date_times[1]

total_weeks = floor(as.integer(most_recent_date-start_date)/7)
tweet_counts = rep(NA,total_weeks)
week_date = rep(NA,total_weeks)
week_start = start_date
for (week in 1:total_weeks) {
  end_date = week_start + seconds_in_week
  tweets_in_week = length(which(date_times > week_start & date_times <= end_date))
  tweet_counts[week] = tweets_in_week
  week_date[week] = as.Date(week_start)
  week_start = end_date
}
x_date = as.Date(week_date,origin = "1970-01-01")
plot(x_date,tweet_counts,xlab="Week",ylab="Number of Tweets",main="Trump Tweet Counts")

current_week_start = start_date + total_weeks * seconds_in_week
tweets_in_current_week = length(which(date_times > current_week_start))
days_left_in_week = as.numeric(current_week_start + seconds_in_week - as.POSIXlt(Sys.time()))

tweet_rate = mean(tweet_counts)/7
lambda = days_left_in_week * tweet_rate

point_estimate = lambda + tweets_in_current_week

t1 = ppois(29 - tweets_in_current_week,lambda = lambda)
t2 = ppois(34 - tweets_in_current_week,lambda = lambda) -
  ppois(29 - tweets_in_current_week,lambda = lambda)
t3 = ppois(39 - tweets_in_current_week,lambda = lambda) -
  ppois(34 - tweets_in_current_week,lambda = lambda)
t4 = ppois(44 - tweets_in_current_week,lambda = lambda) -
  ppois(39 - tweets_in_current_week,lambda = lambda)
t5 = ppois(49 - tweets_in_current_week,lambda = lambda) -
  ppois(44 - tweets_in_current_week,lambda = lambda)
t6 = ppois(54 - tweets_in_current_week,lambda = lambda) -
  ppois(49 - tweets_in_current_week,lambda = lambda)
t7 = ppois(59 - tweets_in_current_week,lambda = lambda) -
  ppois(54 - tweets_in_current_week,lambda = lambda)
t8 = 1 - ppois(59 - tweets_in_current_week,lambda = lambda)

c(t1,t2,t3,t4,t5,t6,t7,t8)