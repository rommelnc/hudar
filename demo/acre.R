## http://www.findlatitudeandlongitude.com/?loc=Rio+Branco+-+Acre%2C+Brazil&id=64069

longlat.1 = c(-70.8119953, -9.0237964)
longlat.2 = c(-70.8119953, -9.0237964)
longlat.3 = c(-66.9297517, -9.8742148)
longlat.4 = c(-70.1925286, -4.3723466)
locat=c(longlat.1, longlat.2, longlat.3, longlat.4)

date()
?filterStream
filterStream("../../data/acre-tweets-2013-05-11.json", 
            track = c("acre", "almeida", "perpetua", "perpétua",
                      "política", "câmara", "deputados", "lei",
                      "regulamentação", "rio branco", "pc do b", 
                      "frente popular", "congresso", "senado", 
                      "veto", "dilma", "presidente", "presidenta",
                      "esplanada", "ministério", "PCdoB-AC", "PCdoB",
                      "AC"),
             timeout = 9*24*60*60, 
             oauth = cred)

date()

tweets.df <- parseTweets("../../data/acre-tweets-2013-05-11.json",
                         simplify = TRUE)
tweets.df[tweets.df$lang == "pt",]$text

