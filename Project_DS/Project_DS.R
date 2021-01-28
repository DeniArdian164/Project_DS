library(Rstem)
library(twitteR)
library(NLP)
library(tm)
library(ggplot2)
library(RColorBrewer)
library(SnowballC)
library(sentiment)


reqURL <- "http://api.twitter.com/oath/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
api_key <- "Wa36rHsxpsAJ4wMaoROPt4rMJ" 
api_key_secret <- "Cdl4TcpxLoB94oZiNU0WjMDiTsQ6jGo8JxRacqE9JG3MXeppkR" 
a_token <- "1354299931054415873-OzkNyCqO2ZdpumI2tHNswQw0595tj6" 
a_token_secret <- "cD9ODqOvID9OipM9FdJBoNeZh8KWqUm4c9nGNWY1bd1Wx" 
setup_twitter_oauth(api_key, api_key_secret, a_token, a_token_secret)


lf_tweets <- searchTwitter('Mental Health issue', n=1000, lang="en")
tweets <- sapply(lf_tweets, function(x) x$getText()) 
write.csv(tweets, file = 'F:/Tugas/Tugas Deni/Project_DS/dataTwitter.csv')


tweets = sapply(lf_tweets, function(x) x$getText())
tweets = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets)
tweets = gsub("@\\w+", "", tweets)
tweets = gsub("[[:punct:]]", "", tweets)
tweets = gsub("[[:digit:]]", "", tweets)
tweets = gsub("http\\w+", "", tweets)
tweets = gsub("[ \t]{2,}", "", tweets)
tweets = gsub("^\\s+|\\s+$", "", tweets)
tweets = gsub("note", "", tweets)


try.error = function(x)
{
  
  y = NA
  try_error = tryCatch(tolower(x), error=function(e) e)
  if (!inherits(try_error, "error"))
    y = tolower(x)
  return(y)
}
tweets = sapply(tweets, try.error)
tweets = tweets[!is.na(tweets)]
names(tweets) = NULL

write.csv(tweets, file = 'F:/Tugas/Tugas Deni/Project_DS/dataBersih.csv')


class_emo = classify_emotion(tweets, algorithm="bayes", prior=1.0)
emotion = class_emo[,7]
emotion[is.na(emotion)] = "neutral"
class_pol = classify_polarity(tweets, algorithm="bayes")
polarity = class_pol[,4]


sent_df = data.frame(text=tweets, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
write.csv(sent_df, file = 'F:/Tugas/Tugas Deni/Project_DS/dataSentimen.csv')
View(sent_df)
table(sent_df$emotion)
table(sent_df$polarity)


plotSentiments1 <- function(sent_df, title) 
{
  ggplot(sent_df, aes(x=emotion)) + 
    geom_bar(aes(y=..count.., fill=emotion)) + 
    scale_fill_brewer(palette="Dark2") + 
    ggtitle(title) + 
    theme(legend.position="right") + 
    ylab("Number of Tweets") + 
    xlab("Emotion Categories")
}
plotSentiments1(sent_df, "Tweets of Mental Health issue By Emotion")

plotSentiments2 <- function(sent_df, title)
{
  ggplot(sent_df, aes(x=polarity)) +
    geom_bar(aes(y=..count.., fill=polarity)) +
    scale_fill_brewer(palette="Dark2") +
    ggtitle(title) +
    theme(legend.position="right") +
    ylab("Number of Tweets") +
    xlab("Polarity Categories")
}
plotSentiments2(sent_df, "Tweets of Mental Health issue By Polarity")
