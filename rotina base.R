#Livrarias

library(syuzhet)
library(RColorBrewer)
library(wordcloud)
library(tm)
library(twitteR)
library(jsonlite)
library(httr)
library(rtweet)


#Login no Twitter


api_key             = "XXXXX"
api_secret          = "XXXXXX"
access_token        = "XXXXXXX"
access_token_secret = "XXXXXXX"

setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret);

#Baixando os tweets

# Lula --------------------------------------------------------------------


twitter_search_string <- "Lula";
tweets = searchTwitter(twitter_search_string, n=10000, lang="pt", 
                        resultType="mixed",since = "2022-10-02");

#Tornando os tweets em um Data Frame

tweets_df = do.call("rbind", lapply(tweets, as.data.frame));
tweets_df = subset(tweets_df, select = c(text));

#Limpando os dados

#Tweet Cleasing
tweets_df$text = gsub('http.* *', '', tweets_df$text);
tweets_df$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ",  tweets_df$text);
tweets_df$text = gsub(":", "", tweets_df$text);
# remove at people
tweets_df$text = gsub("@\\w+", "", tweets_df$text)
# remove punctuation
tweets_df$text = gsub("[[:punct:]]", "", tweets_df$text)
# remove numbers
tweets_df$text = gsub("[[:digit:]]", "", tweets_df$text)
# remove html links
tweets_df$text = gsub("http\\w+", "", tweets_df$text)
# remove unnecessary spaces
tweets_df$text = gsub("[ \t]{2,}", "", tweets_df$text)
tweets_df$text = gsub("^\\s+|\\s+$", "", tweets_df$text)
# Removing Duplicate tweets
tweets_df["DuplicateFlag"] = duplicated(tweets_df$text)
tweets_df = subset(tweets_df, tweets_df$DuplicateFlag=="FALSE")
tweets_df = subset(tweets_df, select = -c(DuplicateFlag))

#analisando os sentimentos

ateste <- get_tokens(tweets_df)
a_teste <- get_nrc_sentiment(ateste, lang="portuguese")


summary(a_teste)

barplot(
  colSums(prop.table(a_teste[, 1:8])),
  space = 0.2,
  horiz = FALSE,
  las = 1,
  cex.names = 0.7,
  main = "Analise de discurso dos Usuários do Twitter - Lula",
  sub = "Analise realizada por Claudiomar Filho (twitter: FilhoClaudiomar)",
  col = brewer.pal(n = 8, name = "Set3"),
  xlab="emoções", ylab = NULL)



# Bolsonaro ---------------------------------------------------------------

twitter_search_string <- "Bolsonaro";
btweets = searchTwitter(twitter_search_string, n=10000, lang="pt", 
                       resultType="mixed",since = "2022-10-02");

#Tornando os tweets em um Data Frame

btweets_df = do.call("rbind", lapply(btweets, as.data.frame));
btweets_df = subset(btweets_df, select = c(text));

#Limpando os dados

#Tweet Cleasing
btweets_df$text = gsub('http.* *', '', btweets_df$text);
btweets_df$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ",  btweets_df$text);
btweets_df$text = gsub(":", "", btweets_df$text);
# remove at people
btweets_df$text = gsub("@\\w+", "", btweets_df$text)
# remove punctuation
btweets_df$text = gsub("[[:punct:]]", "", btweets_df$text)
# remove numbers
btweets_df$text = gsub("[[:digit:]]", "", btweets_df$text)
# remove html links
btweets_df$text = gsub("http\\w+", "", btweets_df$text)
# remove unnecessary spaces
btweets_df$text = gsub("[ \t]{2,}", "", btweets_df$text)
btweets_df$text = gsub("^\\s+|\\s+$", "", btweets_df$text)
# Removing Duplicate tweets
btweets_df["DuplicateFlag"] = duplicated(btweets_df$text)
btweets_df = subset(btweets_df, btweets_df$DuplicateFlag=="FALSE")
btweets_df = subset(btweets_df, select = -c(DuplicateFlag))

#analisando os sentimentos

bteste <- get_tokens(btweets_df)
b_teste <- get_nrc_sentiment(bteste, lang="portuguese")


summary(b_teste)

barplot(
  colSums(prop.table(b_teste[, 1:8])),
  space = 0.2,
  horiz = FALSE,
  las = 1,
  cex.names = 0.7,
  col = brewer.pal(n = 8, name = "Set3"),
  main = "Analise de discurso dos Usuários do Twitter - Bolsonaro",
  sub = "Analise realizada por Claudiomar Filho (twitter: FilhoClaudiomar)",
  xlab="Emocoes", ylab = NULL)

save (b_teste, file="b_teste.Rdata")
save (a_teste, file="a_teste.Rdata")
