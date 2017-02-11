library(Cairo)
# setwd("Coding/blog_material/trump_tweets")
library(MASS)

tweets = read.csv("trump_tweets.csv",header=TRUE)

date_times = as.POSIXlt(tweets$date) - 5*60*60 ## adjust for GMT vs EST

start_date = as.POSIXlt("2016-08-24 12:00:00 EST")

## 2 steps
## 1) predict number of tweets between current time and nearest noon
## 2) predict number of tweets between nearest noon and end date

seconds_in_day = 24*60*60
current_time= Sys.time()

## get most recent date where we have all data before noon
if (current_time < as.POSIXlt(paste(as.Date(current_time), "12:00:00 EST"))) {
  most_recent_date = as.Date(current_time)-1
} else {
  most_recent_date = as.Date(current_time)
}


## find earliest day with time equal to current time (that is still after first tweet)
time_from_start = as.Date(current_time) - as.Date(start_date)
if (current_time-time_from_start - 60*60> start_date) {
  time_frame_in_question =  current_time-time_from_start - 60*60 #DST
} else {
  time_frame_in_question = current_time-time_from_start + seconds_in_day - 60*60
}

# prepare data frames
total_days = floor(as.integer(most_recent_date-as.Date(start_date)))
tweet_counts = rep(NA,total_days)
tweet_counts_timeframe = rep(NA,total_days)
date = rep(NA,total_days)

# prepare counts
# tweets_in_day counts tweets in a full 24 hour period
# tweets_in_timeframe counts current tweets to next noon
day_in_question = start_date
for (day in 1:total_days) {
  tweets_in_day = length(which(date_times > day_in_question & date_times <=seconds_in_day +  day_in_question))
  tweets_in_timeframe = length(which(date_times > time_frame_in_question & date_times <=seconds_in_day +  day_in_question ))
  day_in_question = day_in_question + seconds_in_day
  time_frame_in_question = time_frame_in_question + seconds_in_day
  
  # adjust for daylight saving time
  if (day_in_question == "2016-11-06 11:00:00 EDT") {
    day_in_question = day_in_question + 60*60
    time_frame_in_question = time_frame_in_question + 60*60
  }
  tweet_counts[day] = tweets_in_day
  tweet_counts_timeframe[day] = tweets_in_timeframe
  date[day] = as.Date(day_in_question)
}
# note that x_date refers to the 24 hour segment that ends at noon

# plot data
x_date = as.Date(date,origin = "1970-01-01")
plot(x_date,tweet_counts,type = "l",xlab= "Date", ylab = "Tweet Counts")

# count number of tweets from beginning of week to previous noon
bet_start = as.POSIXlt("2017-02-08 12:00:00 EDT")
weekly_tweets_so_far = length(which(date_times > bet_start & 
                                      date_times < day_in_question))

# count number of tweets before previous noon and now
tweets_before_timeframe = length(which(date_times > day_in_question & 
                                         date_times < current_time))

# indicator variable for after election (changepoint evident from graph)
since_election = as.integer(x_date - as.Date("2016-11-09",origin = "1970-01-01"))
since_election = as.numeric(since_election > 0)

# indicator variable for after inauguration 
since_inaug = as.integer(x_date - as.Date("2017-01-20",origin = "1970-01-01"))
since_inaug = as.numeric(since_inaug > 0)

# arbitrarily, wed noon to thurs noon is weekday 1
weekday = rep_len((1:7),total_days)

# create data frame
df = data.frame(tweet_counts,tweet_counts_timeframe,x_date,since_election,since_inaug,weekday)
df

# autoregressive steps that count dependence on previous day's tweets
# A3-A5 were not statistically significant, so i have discarded them
A1 = c(rep(NA,1),tweet_counts[1:(total_days-1)])
A2 = c(rep(NA,2),tweet_counts[1:(total_days-2)])
#A3 = c(rep(NA,3),tweet_counts[1:(total_days-3)])
#A4 = c(rep(NA,4),tweet_counts[1:(total_days-4)])
#A5 = c(rep(NA,5),tweet_counts[1:(total_days-5)])

df$A1 = A1
df$A2 = A2
#df$A3 = A3
#df$A4 = A4
#df$A5 = A5

# remove first few days where we don't have autoregressive information
df = na.omit(df)
df

# account for spike due to debates
debate_dates = c("2016-09-27","2016-10-05","2016-10-10","2016-10-20")
df$debate = 0
df[(df$x_date %in% as.Date(debate_dates)),]$debate = 1
df$debate

# previously tried accounting for RNC, but it didn't change model much
#rnc_dates = c("2016-07-18","2016-07-19","2016-07-20","2016-07-21","2016-07-22")
#df$rnc = 0
#df[(df$x_date %in% as.Date(rnc_dates)),]$rnc = 1
#df$rnc

# create model matrix with predictors deemed significant
mat = model.matrix(~ df$since_election + df$since_inaug + as.factor(df$weekday) + df$debate
                   + df$A1 + df$A2)# + df$A3 + df$A4 + df$A5)# + df$rnc)

# run poisson glm on timeframe (aka tweets between now and nearest noon)
glm_pois_timeframe = glm(df$tweet_counts_timeframe ~ mat + 0,family=poisson(link=log))
time_frame_coefs = coef(glm_pois_timeframe)

# run poisson glm on tweet_counts (aka daily tweets, from noon to noon)
glm_pois=glm(df$tweet_counts ~ mat + 0,family=poisson(link=log))
summary(glm_pois)

# check dispersion by running a quasilikelihood poisson glm
# same results as glm poisson, but standard errors are different
glm_quasi=glm(df$tweet_counts ~ mat + 0,family=quasipoisson(link=log))
summary(glm_quasi)$dispersion

# run negative binomial glm just for kicks
glm_nb = glm.nb(df$tweet_counts ~ mat + 0)
summary(glm_nb)

# plot daily tweets along with mean predictions from poisson glm
png(filename="trained_tweet_data.png", type="cairo",units="in", width=6, height=5, 
    pointsize=12, res=250)
plot(df$x_date,df$tweet_counts,type="l",xlab = "Date",ylab="Tweet Count",main = "Poisson GLM")
lines(df$x_date,predict(glm_pois, type="response"),col="red")
legend(max(df$x_date)-60,90,c("Real","Predicted"),lty=c(1,1),col=c("black","red"))
dev.off()

# compare train squared errors for poisson and negative binomial
sum((df$tweet_counts - predict(glm_pois, type="response"))^2)
sum((df$tweet_counts - predict(glm_nb, type="response"))^2)

# analyze weekdays
#mean(df[df$weekday == 1,]$tweet_counts)
#mean(df[df$weekday == 2,]$tweet_counts)
#mean(df[df$weekday == 3,]$tweet_counts)
#mean(df[df$weekday == 4,]$tweet_counts)
#mean(df[df$weekday == 5,]$tweet_counts)
#mean(df[df$weekday == 6,]$tweet_counts)
#mean(df[df$weekday == 7,]$tweet_counts)

# prediction_start is the first day for which we have to make predictions
# it includes the time between current_time and nearest noon, so
# some of the data is partially available, and the rest will be estimated
# using the glm results on timeframe
if (current_time < as.POSIXlt(paste(as.Date(current_time),"12:00:00 EST"))) {
  prediction_start = as.Date(current_time)
} else {
  prediction_start = as.Date(current_time) + 1
}

# count number of days we have to make predictions for
prediction_end = as.Date("2017-02-15")
days_to_predict = as.integer(prediction_end-prediction_start)+1

# create prediction data frame
tweet_counts = rep(NA,days_to_predict)
tweet_counts_timeframe = rep(NA,days_to_predict)
x_date = as.Date(prediction_start:prediction_end,origin = "1970-01-01")
since_election = rep(1,days_to_predict)
since_inaug = rep(1,days_to_predict)
start_weekday = df[nrow(df),]$weekday + as.integer(prediction_start - df[nrow(df),]$x_date)
start_weekday = (start_weekday - 1) %% 7 + 1
weekday = (start_weekday):(start_weekday + days_to_predict -1 )
weekday = (weekday - 1) %% 7 + 1
A1 = c(df[nrow(df),]$tweet_counts,rep(-1,days_to_predict-1))
A2 = c(df[(nrow(df)-1):nrow(df),]$tweet_counts,rep(-1,days_to_predict-2))
debate = rep(0,days_to_predict)

# combine with original to make sure factors are same
# (note this is hacky, and there's definitely a better way)
predict_df = data.frame(tweet_counts,tweet_counts_timeframe,x_date,since_election,since_inaug,weekday,
                        A1,A2,debate)
combined_df = rbind(df,predict_df)
combined_mat = model.matrix(~ combined_df$since_election + combined_df$since_inaug + 
               as.factor(combined_df$weekday) + combined_df$debate + 
                 combined_df$A1 + combined_df$A2 + combined_df$debate)
predict_df = as.data.frame(combined_mat[(nrow(df)+1):nrow(combined_df),])
names(predict_df) = c("(Intercept)","since_election","since_inaug",
                      "as.factor(weekday)2","as.factor(weekday)3",
                      "as.factor(weekday)4","as.factor(weekday)5",
                      "as.factor(weekday)6","as.factor(weekday)7",
                      "debate","A1","A2")

predict_df

# he occasionally deletes tweets (even from before this week)
# and because the rules are weird, we have to account for them
tweets_deleted = 7

# now simulate possibilities 
nsims = 5000
full_results = matrix(NA,nrow=nsims,ncol=days_to_predict)
sim_results = rep(NA,nsims)
for (sim in 1:nsims) {
  predictions = rep(NA,days_to_predict)
  for (day in 1:days_to_predict) {
    if (day == 1) {
      # for the first day, we use our time_frame coefficients for prediction
      # making sure to add tweets between previous noon and now
      lambda_timeframe = exp((as.numeric(predict_df[1,]) %*% time_frame_coefs)[1,1])
      prediction = rpois(1,lambda_timeframe) + tweets_before_timeframe
    } else {
      # for all other days, we make predictions based on tweet_counts glm
      lambda = exp((as.numeric(predict_df[day,]) %*% as.vector(coef(glm_pois)))[1,1])
      prediction = rpois(1,lambda)
    }
    predictions[day] = prediction
    # add autoregressive information
    if (day + 1 <= days_to_predict) {
      predict_df[day + 1,]$A1 = prediction
      if (day + 2 <= days_to_predict) {
        predict_df[day + 2,]$A2 = prediction
      }
    }
  }
  # include full information
  full_results[sim,] = predictions
  # count simulated number of tweets, including weekly tweets so far
  sim_results[sim] = sum(predictions) + weekly_tweets_so_far - tweets_deleted
}
# histogram of data

png(filename="simulated_tweet_hist.png", type="cairo",units="in", width=6, height=5, 
    pointsize=12, res=250)
hist(sim_results,xlab = "Weekly Tweets, 2/8-2/15",main = "Simulated Tweet Counts")
dev.off()

png(filename="simulated_tweet_paths.png", type="cairo",units="in", width=6, height=5, 
    pointsize=12, res=250)
plot(1:days_to_predict,cumsum(full_results[1,]),type = "l",ylim = c(0,60),
     xlab = "Day",ylab = "Total Weekly Tweets",main="Simulation Paths",col="lightgrey",
     xaxt="n")
axis(1, at=1:days_to_predict,labels=c("Sat","Sun","Mon","Tue","Wed"), col.axis="black", las=2)
for (sim in 2:(nsims-5)) {
  lines(1:days_to_predict,cumsum(full_results[sim,]),col="lightgrey")
}
lines(1:days_to_predict,cumsum(full_results[nsims-4,]),col="blue")
lines(1:days_to_predict,cumsum(full_results[nsims-3,]),col="green")
lines(1:days_to_predict,cumsum(full_results[nsims-2,]),col="red")
lines(1:days_to_predict,cumsum(full_results[nsims-1,]),col="yellow")
dev.off()

# calculate probability of each bucket
t1 = length(which(sim_results <= 24))/nsims
t2 = length(which(sim_results >= 25 & sim_results <= 29))/nsims
t3 = length(which(sim_results >= 30 & sim_results <= 34))/nsims
t4 = length(which(sim_results >= 35 & sim_results <= 39))/nsims
t5 = length(which(sim_results >= 40 & sim_results <= 44))/nsims
t6 = length(which(sim_results >= 45 & sim_results <= 49))/nsims
t7 = length(which(sim_results >= 50 & sim_results <= 54))/nsims
t8 = length(which(sim_results >= 55))/nsims

round(c(t1,t2,t3,t4,t5,t6,t7,t8),2)
1-round(c(t1,t2,t3,t4,t5,t6,t7,t8),2)

# tweets up to now
weekly_tweets_so_far + tweets_before_timeframe - tweets_deleted
