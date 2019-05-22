library("NLP")
library("twitteR")
library("syuzhet")
library("tm")
library("SnowballC")
library("stringi")
library("topicmodels")
library("syuzhet")
library("twitteR")
library("ROAuth")

#Connect your twitter account to R, in order to extract the required tweets.

consumer_key <- '_______________' #'YOUR CONSUMER KEY'
consumer_secret <- '____________' #'YOUR CONSUMER SECRET KEY'
access_token <- '_______________'
access_secret <- '______________'
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)


#Extracting tweets using a particular hashtag:
tweets_v <- searchTwitter("verizon", n=1000,lang = "en")


#Convert this extracted data to a dataframe which makes it more readable and easier to work with.
verizon_tweets <- twListToDF(tweets_v)

#-------------------#
#Data pre-processing#
#-------------------#
verizon_text<- verizon_tweets$text

#convert all text to lower case
verizon_text<- tolower(verizon_text)

# Replace blank space (“rt”)
verizon_text <- gsub("rt", "", verizon_text)

# Replace @UserName
verizon_text <- gsub("@\\w+", "", verizon_text)


# Remove punctuation
verizon_text <- gsub("[[:punct:]]", "", verizon_text)


# Remove links
verizon_text <- gsub("http\\w+", "", verizon_text)

# Remove tabs
verizon_text <- gsub("[ |\t]{2,}", "", verizon_text)


# Remove blank spaces at the beginning
verizon_text <- gsub("^ ", "", verizon_text)

# Remove blank spaces at the end
verizon_text <- gsub(" $", "", verizon_text)

#getting emotions using in-built function
mysentiment_verizon<-get_nrc_sentiment((verizon_text))

#calculationg total score for each sentiment
Sentimentscores_verizon<-data.frame(colSums(mysentiment_verizon[,]))


names(Sentimentscores_verizon)<-"Score"
Sentimentscores_verizon<-cbind("sentiment"=rownames(Sentimentscores_verizon),Sentimentscores_verizon)
rownames(Sentimentscores_verizon)<-NULL

#plotting the sentiments with scores
ggplot(data=Sentimentscores_verizon,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people behind the tweets on verizon")
