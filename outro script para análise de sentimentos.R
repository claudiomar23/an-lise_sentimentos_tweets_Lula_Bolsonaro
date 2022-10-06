#script disponível em: https://analisemacro.com.br/data-science/teto-de-gastos-analise-de-sentimentos-com-dados-do-twitter/

library(magrittr) # CRAN v2.0.1
library(rtweet) # CRAN v0.7.0
library(lexiconPT) # CRAN v0.1.0
library(dplyr) # CRAN v1.0.7
library(tidytext) # CRAN v0.2.6
library(purrr) # CRAN v0.3.4
library(tidyr) # CRAN v1.1.3
library(ggplot2) # CRAN v3.3.5
library(ggtext) # CRAN v0.1.1
library(scales) # CRAN v1.1.1
library (twitteR)
options(scipen=999)
api_key             = "qVDLsNcJx5ROXkuSXup1rHwNs"
api_secret          = "RZhz5Z2aLJxM3CNQ8KDu4DmyjyF2afDmVpDMq4IYe6GmnnDfHx"
access_token        = "1111096902706216967-EDzLAPEx1W0C4x5SCfwdmgTy8oz3t5"
access_token_secret = "80GU14yzm8IEFpOwX3Zvwqw6xcd9iLlKCSQ2Q5GjUjtX9"

setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret);

df_tweets <- rtweet::search_tweets(
  q = '"lula"', # pesquisar termo exato
  n = 10000, # número de tweets desejados
  include_rts = FALSE, # não incluir retweets
  retryonratelimit = TRUE # para retornar o nº desejado de tweets
)

dplyr::glimpse(df_tweets)

# Léxicos da língua portuguesa para análise de sentimentos
oplex <- lexiconPT::oplexicon_v3.0
sentilex <- lexiconPT::sentiLex_lem_PT02


#O primeiro passo a ser feito é desagregar a informação das mensagens de texto de cada tweet de modo a separar essas mensagens em várias palavras/tokens. Esse processo é conhecido como "tokenização" e sua implementação é bastante simples, conforme abaixo

# Separa coluna "text", que são os tweets, em n tokens/palavras
df_token <- df_tweets %>%
  tidytext::unnest_tokens(output = "term", input = "text")

dplyr::glimpse(df_token)

df_sent <- purrr::reduce(
  .x = list(
    df_token,
    oplex,
    dplyr::select(sentilex, term, lex_polarity = polarity)
  ),
  .f = dplyr::inner_join,
  by = "term"
) %>%
  # Agrupar por identificador único de cada tweet
  dplyr::group_by(id) %>%
  # Obter sentimento total de cada tweet de acordo com polaridade
  # para saber quão negativo/positivo é um tweet
  dplyr::summarise(
    tweet_oplex = sum(polarity),
    tweet_sentilex = sum(lex_polarity)
  ) %>%
  dplyr::ungroup()
dplyr::glimpse(df_sent)


# 2) Usar polaridade por tweet para obter sentimento diário
# Juntar dados de tweets e polaridades
df_sent_by_date <- dplyr::inner_join(df_tweets, df_sent, by = "id") %>%
  # Filtrar somente polaridades positivas/negativas
  dplyr::filter(tweet_oplex != 0) %>%
  # Obter quantidade de tweets com sentimento "negativo/positivo" por dia
  dplyr::count(
    sentiment = dplyr::if_else(tweet_oplex < 0, "negativo", "positivo"),
    date = as.Date(created_at)
  ) %>%
  # Converter para formato wide
  tidyr::pivot_wider(
    id_cols = `date`,
    names_from = `sentiment`,
    values_from = `n`
  ) %>%
  # Obter sentimento diário
  dplyr::mutate(sentimento = positivo - negativo)

dplyr::glimpse(df_sent_by_date)


