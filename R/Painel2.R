
byRetweet = function(tweets) {
  retweeted = length(grep("^RT", tweets$text))
  original = nrow(tweets) - retweeted
  type = c("Retweet", "Tweet Original")
  numberOfTweets = c(retweeted, original)
  df = data.frame(type, numberOfTweets)
  df
}


byTimeOfDay = function(tweets) {
  Sys.setlocale("LC_TIME", "English")
  tweets$created_at2 = strptime(tweets$created_at, "%a %b %d %H:%M:%S %z %Y")
  df = aggregate(tweets$created_at2, list(as.POSIXlt(tweets$created_at2)$hour), FUN=length)
  names(df) = c("Hora", "Tweets")
  df$Hora = as.POSIXlt(paste(df$Hora, ":00", sep=""), format="%H:%M")
  df$Hora = strftime(df$Hora, "%H")
  df
}


byHours = function(tweets, hours) {
  Sys.setlocale("LC_TIME", "English")
  tweets$created_at2 = strptime(tweets$created_at, "%a %b %d %H:%M:%S %z %Y")
  tweets$created_at3 = tweets$created_at2
  tweets$created_at3$hour = unlist(lapply(tweets$created_at2$hour, function(x) {while (x %% hours != 0) { x = x - 1 }; x}))
  tweets$created_at3$min = 0
  tweets$created_at3$sec = 1  
  df = aggregate(tweets$created_at3, by=list(as.character(tweets$created_at3)), FUN=length)
  names(df) = c("Date", "Tweets")
  #df$Date = as.POSIXlt(paste(df$Date, ":00", sep=""), format="%H:%M")
  #df$Date = strftime(df$Date, "%H")
  df
}


unlist(lapply(c(5,2,3,23,22,18,14), function(x) {while (x %% 4 != 0) { x = x - 1 }; x}))



byHashTags = function(tweets) {
  text = tweets$text
  hashtags <- regmatches(text,gregexpr("#(\\d|\\w)+",text))
  hashtags <- unlist(hashtags)
  hashtags <- table(hashtags)
  sorted <- order(hashtags, decreasing=TRUE)
  hashtags <- hashtags[sorted]
  hashtags
  df = data.frame(array(hashtags), names(hashtags))
  if (length(hashtags) == 0) {
    df = data.frame(ocorrencia=NA, hashtag=NA)[numeric(0), ]
  }
  names(df) = c("ocorrencia", "hashtag")
  df
}




byUsuario = function(tweets) {
  source('funcoesTwitter.R')
#   lista = montarListaUsuarios(tweets)
  listaDePara = montarListaDePara(tweets, F)
  tabelas = montarTabelasUsuarios(listaDePara)
  tabelas
}




