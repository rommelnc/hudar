criarTermometroTemas = function(tweets.df) {
  library(rCharts)
  
  tweets.df$date = strptime(tweets.df$created_at, "%a %b %d %H:%M:%S %z %Y")
  
  index = intersect(grep('voto',tweets.df$text,ignore.case=T),grep('(secreto|aberto)',tweets.df$text,ignore.case=T))
  dados = table(cut(tweets.df$date[index], breaks="hour"))
  tamanho = nrow(dados)
  grupos = c(rep('Voto Secreto',tamanho), rep('Ficha Limpa',tamanho), rep('Marco Civil da Internet',tamanho), 
             rep('Quem Tem Boca Vai À Dilma',tamanho), rep('Saúde',tamanho), rep('Educação',tamanho), rep('Corrupção',tamanho))
  resultado = data.frame(linhas=1:(7*tamanho))
  resultado$horario = names(dados)
  resultado$grupo = grupos
  resultado$tweets = dados
  resultado
  
  index = intersect(grep('ficha',tweets.df$text,ignore.case=T),grep('limpa|suja',tweets.df$text,ignore.case=T))
  dados = table(cut(tweets.df$date[index], breaks="hour"))
  i = 1
  inicio = i*tamanho+1
  fim = (i+1)*tamanho
  resultado$tweets[inicio:fim] = dados
  resultado
  
  index = intersect(intersect(grep('marco',tweets.df$text,ignore.case=T),grep('civil',tweets.df$text,ignore.case=T)),grep('internet',tweets.df$text,ignore.case=T))
  dados = table(cut(tweets.df$date[index], breaks="hour"))
  i = i + 1
  inicio = i*tamanho+1
  fim = (i+1)*tamanho
  resultado$tweets[inicio:fim] = dados
  resultado
  
  index = grep('quemtembocavaiadilma',tweets.df$text,ignore.case=T)
  dados = table(cut(tweets.df$date[index], breaks="hour"))
  i = i + 1
  inicio = i*tamanho+1
  fim = (i+1)*tamanho
  resultado$tweets[inicio:fim] = dados
  resultado
  
  index = grep('sa(ú|u)de',tweets.df$text,ignore.case=T)
  dados = table(cut(tweets.df$date[index], breaks="hour"))
  i = i + 1
  inicio = i*tamanho+1
  fim = (i+1)*tamanho
  resultado$tweets[inicio:fim] = dados
  resultado
  
  index = grep('educa(ç|c)(ã|a)o',tweets.df$text,ignore.case=T)
  dados = table(cut(tweets.df$date[index], breaks="hour"))
  i = i + 1
  inicio = i*tamanho+1
  fim = (i+1)*tamanho
  resultado$tweets[inicio:fim] = dados
  resultado
  
  index = grep('corrup(ç|c)(ã|a)o',tweets.df$text,ignore.case=T)
  dados = table(cut(tweets.df$date[index], breaks="hour"))
  i = i + 1
  inicio = i*tamanho+1
  fim = (i+1)*tamanho
  resultado$tweets[inicio:fim] = dados
  resultado
  
  
#   dados
#   
#   votoSecreto = nrow(tweets.df[intersect(grep('voto',tweets.df$text,ignore.case=T),grep('(secreto|aberto)',tweets.df$text,ignore.case=T)),])
#   fichaLimpa = nrow(tweets.df[intersect(grep('ficha',tweets.df$text,ignore.case=T),grep('limpa|suja',tweets.df$text,ignore.case=T)),])
#   marcoCivilInternet = nrow(tweets.df[intersect(intersect(grep('marco',tweets.df$text,ignore.case=T),grep('civil',tweets.df$text,ignore.case=T)),grep('internet',tweets.df$text,ignore.case=T)),])
#   saude = nrow(tweets.df[grep('sa(ú|u)de',tweets.df$text,ignore.case=T),])
#   educacao = nrow(tweets.df[grep('educa(ç|c)(ã|a)o',tweets.df$text,ignore.case=T),])
#   quemTemBoca = nrow(tweets.df[grep('quemtembocavaiadilma',tweets.df$text,ignore.case=T),])
#   corrupcao = nrow(tweets.df[grep('corrup(ç|c)(ã|a)o',tweets.df$text,ignore.case=T),])
# 
#   dados = data.frame(tweets=c(votoSecreto, fichaLimpa, marcoCivilInternet, quemTemBoca, saude, educacao, corrupcao), 
#                      grupo=c('Voto Secreto', 'Ficha Limpa', 'Marco Civil da Internet', 'Quem Tem Boca Vai À Dilma', 'Saúde', 'Educação', 'Corrupção'))
#   print(dados)
  
  
  
  n1 <- nPlot(tweets ~ horario, group = "grupo", data = resultado, type = "stackedAreaChart")
  
  resultado$horario = as.numeric(strptime(resultado$horario, format='%Y-%m-%d %H:%M:%s'))
  n1$xAxis( tickFormat="#!function(d) {return d3.time.format('%x %H:%M')(new Date(d * 1000));}!#" )
  
  resultado$horario = 1:tamanho
  n1 <- nPlot(tweets ~ horario, group = "grupo", data = resultado, type = "stackedAreaChart")
  
  n1
  
  
}

criarGrafo = function(textos, remove = c(), above = .9) {
#   require(ROAuth)
#   require(twitteR)
#   require(XML)
  require(tm)
  require(igraph)
#   require(RColorBrewer)
  
# #   textos = c('política', 'àmanhã', 'eleição')
#   Encoding(textos) <- "UTF-8"
# #   textos
#   textos = gsub("\\`|\\'|\\~|\\^|\"", "", iconv(textos, to="ASCII//TRANSLIT"))
# #   textos
#   textos = iconv(textos, "UTF-8", "UTF-8",sub='')
# #   textos
#   #   xx <- iconv(textos, "UTF-8", "UTF-8",sub='')
#   # xx  
#   #   textos = enc2utf8(textos)
  
  # remove retweet entities
  textos = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", textos)
  # remove at people
  textos = gsub("@\\w+", " ", textos)
  # remove punctuation
  textos = gsub("[[:punct:]]", " ", textos)
  # remove numbers
  textos = gsub("[[:digit:]]", " ", textos)
  # remove html links
  textos = gsub("http\\w+", " ", textos)
  # remove unnecessary spaces
  textos = gsub("[ \t]{2,}", " ", textos)
  textos = gsub("^\\s+|\\s+$", " ", textos)
  
  # lower case using tryTolower with sapply 
  textos = sapply(textos, tryTolower)
  names(textos) = NULL
  
  # remove empty textos (if any)
  textos = textos[textos != ""]
  
  # create corpus
  corpus = Corpus(VectorSource(textos))
  
  # remove stopwords
  # skipwords = c(stopwords("english"))
  skipwords = c(stopwords("portuguese"), remove, c('pra', 'ter', 'diz', 'ver', 'tem', 'pouco', 'ainda', 'tentar', 'deveria', 'sobre', 'quer', 'contra', 'dar', 'cada', 'desse'))
  corpus = tm_map(corpus, removeWords, skipwords)
  
  # term-document matrix
#   tdm = TermDocumentMatrix(corpus)
#   tdm = TermDocumentMatrix(corpus, control=list(minWordLength=2, minDocFreq=.05*length(textos), removeNumbers=T))
  tdm = TermDocumentMatrix(corpus, control=list(minWordLength=2, minDocFreq=.05*length(textos)))
  print(tdm)
  
  tdm = removeSparseTerms(tdm, .999)
  print(tdm)
  
  # convert tdm to matrix
  m = as.matrix(tdm)
  print(nrow(m))
  
  # word counts
  wc = rowSums(m)
  
  # get those words above the 3rd quantile
  lim = quantile(wc, probs=above, na.rm=TRUE)
  good = m[wc > lim,]
  print(nrow(good))
  
  
  # remove columns (docs) with zeroes
  good = good[,colSums(good)!=0]

  # adjacency matrix
  M = good %*% t(good)

  # set zeroes in diagonal
  diag(M) = 0
  
  # graph
  g = graph.adjacency(M, weighted=TRUE, mode="undirected",
                      add.rownames=TRUE)
  
#   # layout
#   glay = layout.fruchterman.reingold(g)
#   
#   # let's superimpose a cluster structure with k-means clustering
#   kmg = kmeans(M, centers=8)
#   gk = kmg$cluster
# 
#   # create nice colors for each cluster
#   gbrew = c("red", brewer.pal(8, "Dark2"))
#   gpal = rgb2hsv(col2rgb(gbrew))
#   gcols = rep("", length(gk))
#   for (k in 1:8) {
#     gcols[gk == k] = hsv(gpal[1,k], gpal[2,k], gpal[3,k], alpha=0.5)
#   }

  # prepare ingredients for plot
  V(g)$size = 10
  V(g)$label = V(g)$name
  V(g)$degree = degree(g)
  V(g)$label.cex = 1.5 * log10(V(g)$degree)
  V(g)$label.color = hsv(0, 0, 0.2, 0.55)
  V(g)$frame.color = NA
#   V(g)$color = gcols
#   E(g)$color = hsv(0, 0, 0.7, 0.3)
  
#   write.graph(g, file="../../data/politics.GML", format="GML")

  g
}

criarGrafoDePara = function(matriz, plot=F) {
  require(igraph)
  require(RColorBrewer)
  
  # graph
  g = graph.adjacency(matriz, weighted=TRUE, mode="directed", add.rownames=TRUE)
  
  # some properties
  V(g)$size = 10
  V(g)$label = V(g)$name
  V(g)$degree = degree(g)
  V(g)$label.cex = 1.5 * log10(V(g)$degree)
  
#   write.graph(g, file="../../data/politics-users.GML", format="GML")
  
  if (plot) {
    # layout
    glay = layout.fruchterman.reingold(g)
    
    # let's superimpose a cluster structure with k-means clustering
    kmg = kmeans(matriz, centers=8)
    gk = kmg$cluster
    
    # create nice colors for each cluster
    gbrew = c("red", brewer.pal(8, "Dark2"))
    gpal = rgb2hsv(col2rgb(gbrew))
    gcols = rep("", length(gk))
    for (k in 1:8) {
      gcols[gk == k] = hsv(gpal[1,k], gpal[2,k], gpal[3,k], alpha=0.5)
    }
    
    # prepare ingredients for plot
    V(g)$label.color = hsv(0, 0, 0.2, 0.55)
    V(g)$frame.color = NA
    V(g)$color = gcols
    E(g)$color = hsv(0, 0, 0.7, 0.3)
    
    # plot
    plot(g, layout=glay)
    title("\nGraph of tweets about elections",
          col.main="gray40", cex.main=1.5, family="serif")
  }
  
  g
}