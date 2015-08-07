recuperarUsuariosRTs = function(tweet) {
  m <- gregexpr("RT @([[:alnum:]]|_)+:", tweet$text)
  usuarios = NA
  if (!is.na(m) & m[[1]][1] > 0) {
    usuarios = regmatches(tweet$text, m)[[1]]
    usuarios = as.vector(sapply(usuarios, function(x) substr(x, 5, nchar(x)-1)))
  } 
  usuarios = usuarios[!is.na(usuarios)]
}

montarListaUsuariosRTs = function(tweets, debug=F) {
  if (debug) {
    print('Montando lista de usuários RTs:')
    # create progress bar
    pb = txtProgressBar(min = 0, max = dim(tweets)[1], style = 3)
  }
  usuarios = recuperarUsuariosRTs(tweets[1,])
  for(i in 2:dim(tweets)[1]) {
    usuarios = c(usuarios, recuperarUsuariosRTs(tweets[i,]))
    if (debug) {
      # update progress bar
      setTxtProgressBar(pb, i)
    }
  }
  usuarios
}

recuperarAutor = function(tweet) {
  autor = tweet$screen_name
  m <- regexpr("RT @([[:alnum:]]|_)+:", tweet$text)
  if (m[1] > 0) {
    autor = regmatches(tweet$text, m)
    autor = substr(autor, 5, nchar(autor)-1)
  } 
  autor
}

# recuperarUsuarios = function(tweet) {
#   m <- gregexpr("@([[:alnum:]]|_)+", tweet$text)
#   usuarios = NA
#   if (!is.na(m) & m[[1]][1] > 0) {
#     usuarios = regmatches(tweet$text, m)[[1]]
#     usuarios = as.vector(sapply(usuarios, function(x) substr(x, 2, nchar(x))))
#   } 
#   usuarios
# }
recuperarUsuarios = function(text) {
  m <- gregexpr("@([[:alnum:]]|_)+", text)
  usuarios = NA
  if (!is.na(m) & m[[1]][1] > 0) {
    usuarios = regmatches(text, m)[[1]]
    usuarios = as.vector(sapply(usuarios, function(x) substr(x, 2, nchar(x))))
  } 
  usuarios
}

recuperarLocalizacoes = function(text) {
  m <- gregexpr("dilma|aecio|eduardo", text)
  localizacao = NA
  if (!is.na(m) & m[[1]][1] > 0) {
    localizacao = regmatches(text, m)[[1]]
  } 
  localizacao
}

montarListaLocalizacoes = function(tweets, debug = F) {
  if (debug) {
    inicio = Sys.time()
  }
  listaLocalizacoes = lapply(tweets$text, FUN = function(x) recuperarLocalizacoes(x))
  
  if (debug) {
    fim = Sys.time()
    print(fim - inicio)
  }
  listaLocalizacoes
}


# montarListaDePara = function(tweets, debug=F) {
#   if (debug) {
#     print('Montando lista de -> para:')
#     # create progress bar
#     pb = txtProgressBar(min = 0, max = dim(tweets)[1], style = 3)
#   }
#   de = tweets[1,]$screen_name
#   para = recuperarUsuarios(tweets[1,])
#   listaDePara = list(de = as.list(de), para = as.list(para))
#   for(i in 2:dim(tweets)[1]) {
#     de = tweets[i,]$screen_name
#     para = recuperarUsuarios(tweets[i,])
#     listaDePara$de = c(listaDePara$de, as.list(de))
#     listaDePara$para = c(listaDePara$para, as.list(para))
#     if (debug) {
#       # update progress bar
#       setTxtProgressBar(pb, i)
#     }
#   }
#   listaDePara
# }
montarListaDePara = function(tweets, debug=F) {
  if (debug) {
    inicio = Sys.time()
  }
  de = as.list(tweets$screen_name)
  para = lapply(tweets$text, FUN = function(x) recuperarUsuarios(x))
  listaDePara = list(de = de, para = para)
  if (debug) {
    fim = Sys.time()
    print(fim - inicio)
  }
  listaDePara
}

montarListaUsuarios = function(tweets, debug=F) {
  listaDePara = montarListaDePara(tweets, debug)
  usuarios = c(unlist(listaDePara$de),unlist(listaDePara$para))
  usuarios = usuarios[!is.na(usuarios)]
  list(usuarios=usuarios, listaDePara=listaDePara)
}

montarMatrizUsuarios = function(tweets, debug=F) {
  require(Matrix)
  lista = montarListaUsuarios(tweets, debug)
  if (debug) {
    print('Montando matriz de -> para:')
    # create progress bar
    pb = txtProgressBar(min = 0, max = length(lista$listaDePara$de), style = 3)
  }
  usuarios = unique(lista$usuarios)
  matriz = Matrix(0, nrow=length(usuarios), ncol=length(usuarios), sparse=TRUE)
  dimnames(matriz)[[1]] = usuarios
  dimnames(matriz)[[2]] = usuarios
  for(i in 1:length(lista$listaDePara$de)) {
    if (!is.na(lista$listaDePara$para[i][[1]])) {
      de = as.character(lista$listaDePara$de[i][[1]])
      para = as.character(lista$listaDePara$para[i][[1]])
      matriz[de,para] = matriz[de,para] + 1
    }
    if (debug) {
      # update progress bar
      setTxtProgressBar(pb, i)
    }
  }
  matriz
}

montarTabelasUsuarios = function(listaDePara) {
  de = unlist(listaDePara$de)
  de = de[!is.na(de)]
  
  para = unlist(listaDePara$para)
  para = para[!is.na(para)]
  
  ocorrencias = c(de,para)
  
  maisFalou = sort(table(de), decreasing=TRUE)
  maisFalado = sort(table(para), decreasing=TRUE)
  maisOcorrencias = sort(table(ocorrencias), decreasing=TRUE)
  
  list(maisFalou = maisFalou, maisFalado = maisFalado, maisOcorrencias = maisOcorrencias)
}

recuperarTweetsEleicoes = function(tweets) {
  usuarios = paste0(c("dilmabr", "AecioNeves", "eduardocampos40", "silva_marina", "Dima Rouseff", "Marina Silva", "Eduardo Campos", "Aecio Neves", "Aécio Neves"), collapse="|")
  eleicao = paste0(c("eleição","eleicao", "eleições", "eleicoes", "eleitoral", "voto", "vata", "vote", "vot", "votação", "votacao"), collapse="|")
  presidente = paste0(c("presidente", "presidência", "presidencia", "presidenta"), collapse="|")
  candidatos = paste0(c("Dilma", "Rousseff", "Lula", "Aécio", "Neves", "Aecio", "Neves", "Eduardo", "Campos", "Marina", "Silva"), collapse="|")
  campanha = paste0(c("campanha", "politica", "política"), collapse="|")
  index = grep(usuarios,tweets$text,ignore.case=T)
  index = union(index,intersect(grep(eleicao,tweets$text,ignore.case=T),grep(presidente,tweets$text,ignore.case=T)))
  index = union(index,intersect(grep(eleicao,tweets$text,ignore.case=T),grep(candidatos,tweets$text,ignore.case=T)))
  index = union(index,intersect(grep(eleicao,tweets$text,ignore.case=T),grep(campanha,tweets$text,ignore.case=T)))
  index = union(index,intersect(grep(presidente,tweets$text,ignore.case=T),grep(candidatos,tweets$text,ignore.case=T)))
  index = union(index,intersect(grep(presidente,tweets$text,ignore.case=T),grep(campanha,tweets$text,ignore.case=T)))
  index = union(index,intersect(grep(candidatos,tweets$text,ignore.case=T),grep(campanha,tweets$text,ignore.case=T)))
  tweets[index,]
}

recuperarTweetsEmArquivos = function(arquivos, filtro, usuarios = NA, debug = FALSE) {
  if (debug) {
    print(paste0('Recuperando os tweets dos arquivos: ', paste0(arquivos, collapse = ', ')))
    # create progress bar
    pb = txtProgressBar(min = 0, max = length(arquivos), style = 3)
  }
  t = data.frame()
  i = 0
  for (arquivo in arquivos) {
    i = i + 1
    load(arquivo)
    t = unique(rbind(tweets[grep(filtro,tweets$text,ignore.case=TRUE),],t))
    if (!is.na(usuarios)) {
      t = unique(rbind(tweets[tweets$screen_name %in% usuarios,],t))
    }
    remove(tweets)
    if (debug) {
      # update progress bar
      setTxtProgressBar(pb, i)
    }
  }
  t
}

salvarParseTweets = function(arquivos, debug = FALSE) {
  library(streamR)
  if (debug) {
    print(paste0('Salvar os tweets dos arquivos: ', paste0(arquivos, collapse = ', ')))
    # create progress bar
    pb = txtProgressBar(min = 0, max = length(arquivos), style = 3)
  }
  i = 0
  for (arquivo in arquivos) {
    i = i + 1
    tweets = parseTweets(arquivo, simplify = FALSE)
    save(tweets, file = sub("\\.json", "\\.Rda", arquivo))
    if (debug) {
      # update progress bar
      setTxtProgressBar(pb, i)
    }
  }
}

salvarTweetsEleicoes = function(arquivo) {
  previous = load(arquivo)
  eleicoes = recuperarTweetsEleicoes(tweets)
  save(list = unique(c(previous, 'eleicoes')), file = arquivo)
  eleicoes
}

joinTweets = function(dia, parse = FALSE, prefix = "../data/politics-tweets-", all = TRUE) {
  require(streamR)
  if (parse) {
    arquivo = paste0(prefix, dia, ".json")
    tweets = parseTweets(arquivo, simplify = FALSE)
  } else {
    arquivo = paste0(prefix, dia, ".Rda")
    load(arquivo)
  }
  t = tweets
  if (all) {
    if (parse) {
      arquivo = paste0(prefix, dia, "-2.json")
      tweets = parseTweets(arquivo, simplify = FALSE)
    } else {
      arquivo = paste0(prefix, dia, "-2.Rda")
      load(arquivo)
    }
    tweets = rbind(tweets,t)
    tweets = unique(tweets)
  } else {
    tweets = t
  }
  arquivo = paste0(prefix, dia, "-all.Rda")
  save(tweets, file=arquivo)
  tweets
}

joinRdas = function(dias, prefix = "../data/politics-tweets-", debug = FALSE) {
  if (debug) {
    print(paste0('Juntando os tweets dos dias: ', dias[1], " a ", dias[length(dias)]))
    # create progress bar
    pb = txtProgressBar(min = 0, max = length(dias), style = 3)
  }
  t = data.frame()
  e = data.frame()
  l = data.frame()
  for (i in 1:length(dias)) {
    arquivo = paste0(prefix, dias[i], "-all.Rda")
    variaveis = load(arquivo)
    ## Juntar os tweets apenas se tiver no arquivo Rda carregado
    if ('tweets' %in% variaveis) {
      t = unique(rbind(tweets,t))
      remove(tweets)
    }
    ## Juntar os tweets das eleicoes apenas se tiver no arquivo Rda carregado
    if ('eleicoes' %in% variaveis) {
      e = unique(rbind(eleicoes,e))
      remove(eleicoes)
    }
    ## Juntar os to links das eleicoes apenas se tiver no arquivo Rda carregado
    if ('topLinksEleicoes' %in% variaveis) {
      ## Ver se tem como já fazer o merge somando os counts aqui...
      l = rbind(topLinksEleicoes,l)
      remove(topLinksEleicoes)
    }
    
    if (debug) {
      # update progress bar
      setTxtProgressBar(pb, i)
    }
  }
  arquivo = paste0(prefix, dias[1], "-to-", dias[length(dias)], "-all.Rda")
  tweets = t
  eleicoes = e
  topLinksEleicoes = l
  remove(t)
  remove(e)
  remove(l)
  save(list = variaveis, file = arquivo)
  list(tweets = tweets, eleicoes = eleicoes, topLinksEleicoes = topLinksEleicoes)
}

joinRdasEleicoes = function(dias, prefix = "../data/politics-tweets-", debug = FALSE) {
  if (debug) {
    print(paste0('Juntando os tweets dos dias: ', dias[1], " a ", dias[length(dias)]))
    # create progress bar
    pb = txtProgressBar(min = 0, max = length(dias), style = 3)
  }
  e = data.frame()
  for (i in 1:length(dias)) {
    arquivo = paste0(prefix, dias[i], "-eleicoes-all.Rda")
    variaveis = load(arquivo)
    ## Juntar os tweets das eleicoes apenas se tiver no arquivo Rda carregado
    if ('eleicoes' %in% variaveis) {
      e = unique(rbind(eleicoes,e))
      remove(eleicoes)
    }
    if (debug) {
      # update progress bar
      setTxtProgressBar(pb, i)
    }
  }
  arquivo = paste0(prefix, dias[1], "-to-", dias[length(dias)], "-eleicoes-all.Rda")
  eleicoes = e
  remove(e)
  save(eleicoes, file = arquivo)
  eleicoes
}

recuperarPalavras = function() {
  usuarios = c("dilmabr", "AecioNeves", "eduardocampos40", "silva_marina", "Dima Rouseff", "Marina Silva", "Eduardo Campos", "Aecio Neves", "Aécio Neves")
  eleicao = c("eleição","eleicao", "eleições", "eleicoes", "eleitoral", "voto", "vata", "vote", "vot", "votação", "votacao")
  presidente = c("presidente", "presidência", "presidencia", "presidenta")
  candidatos = c("Dilma", "Rousseff", "Lula", "Aécio", "Neves", "Aecio", "Neves", "Eduardo", "Campos", "Marina", "Silva")
  campanha = c("campanha", "politica", "política")
  tolower(c(usuarios, eleicao, presidente, candidatos, campanha, 'http'))
}

ajustarDataExcel = function(dados) {
  d = as.POSIXlt(dados$Data.Recuperacao)
  d$year = d$year+4
  dados$Data.Recuperacao = as.Date(format(d))
  unique(dados$Data.Recuperacao)
  d = as.POSIXlt(dados$Data.Ultimo.Tweet)
  d$year = d$year+4
  dados$Data.Ultimo.Tweet = as.Date(format(d))
  unique(dados$Data.Ultimo.Tweet)
  dados
}

referencesByUserByDay = function(tweets, usuarios, datas = NA) {
  if (is.na(datas)) {
    minimo = as.Date(strptime(tweets$created_at[1], "%a %b %d %H:%M:%S %z %Y"))
    maximo = as.Date(strptime(tweets$created_at[nrow(tweets)], "%a %b %d %H:%M:%S %z %Y"))
    datas = seq(minimo, maximo, by = 1)
  }
  require(zoo)
  refs = data.frame()
  for (i in 1:nrow(usuarios)) {
    temp = tweets[tweets$screen_name == usuarios$usuario[i],]
    temp = rbind(temp, tweets[grep(paste0(c(usuarios$usuario[i],usuarios$palavras[i]), collapse="|"), tweets$text, ignore.case=TRUE),])
    ## Se não houver nenhum tweet desse usuário, simplesmente ignorar e analisar o próximo
    if (nrow(temp) == 0) {
      next
    }
    ref = referenceByDay(temp)
    z = zoo(ref[,-1], as.Date(ref$Dia))
    ref = as.data.frame(coredata(na.locf(z, x = as.Date, xout = datas)))
    names(ref)[1] = 'Referencias'
    ref$Dia = datas
    ref$grupo = usuarios$nome[i]
    refs = rbind(refs, ref)
  }
  
  refs
}

referencesByUserByHour = function(tweets, usuarios) {
  refs = data.frame()
  for (i in 1:nrow(usuarios)) {
    temp = tweets[tweets$screen_name == usuarios$usuario[i],]
    temp = rbind(temp, tweets[grep(paste0(c(usuarios$usuario[i],usuarios$palavras[i]), collapse="|"), tweets$text, ignore.case=TRUE),])
    ## Se não houver nenhum tweet desse usuário, simplesmente ignorar e analisar o próximo
    if (nrow(temp) == 0) {
      next
    }
    ref = referenceByHour(temp)
    ref$grupo = usuarios$nome[i]
    refs = rbind(refs, ref)
  }
  
  refs
}

statsByUser = function(tweets, usuarios, modo = 'todos', datas = NA) {
  if (is.na(datas)) {
    minimo = as.Date(strptime(tweets$created_at[1], "%a %b %d %H:%M:%S %z %Y"))
    maximo = as.Date(strptime(tweets$created_at[nrow(tweets)], "%a %b %d %H:%M:%S %z %Y"))
    datas = seq(minimo, maximo, by = 1)
  }
  require(zoo)
  stats = data.frame()
  for (i in 1:nrow(usuarios)) {
    temp = tweets[tweets$screen_name == usuarios$usuario[i],]
    ## Se não houver nenhum tweet desse usuário, simplesmente ignorar e analisar o próximo
    if (nrow(temp) == 0) {
      next
    }
    stat = statsByDay(temp)
    if (modo == 'novos') {
      stat$Seguidores = stat$Seguidores - min(stat$Seguidores)
    } else if (modo == 'diferenca') {
      stat$Seguidores = c(0,diff(stat$Seguidores))
    } else if (modo != 'todos') {
      stop(paste0('Modo ', modo, ' não conhecido. As opções válidas são: novos; diferenca; todos.'))
    }
    z = zoo(stat[,-1], as.Date(stat$Dia))
    stat = as.data.frame(coredata(na.locf(z, x = as.Date, xout = datas)))
    stat$Dia = datas
    stat$grupo = usuarios$nome[i]
    stats = rbind(stats, stat)
  }
  
  stats
}

plotSplineSeguidores = function(dados) {
  require(rCharts)
  grafico <- Highcharts$new()
  grafico$chart(type = "spline")
  grupos = unique(dados$grupo)
  for (i in 1:length(grupos)) {
    grafico$series(data = dados[dados$grupo == grupos[i],]$Seguidores, dashStyle = "longdash", name=paste0(c("Seguidores ",as.character(grupos[i])), collapse=""))
  }
  grafico$xAxis(categories = format(dados$Dia, "%d/%m"), labels = list(rotation=-45))
  #   grafico$set(dom="dilma")
  grafico$legend(align="center", verticalAlign="top", y=40, itemStyle = list(fontSize="16px", fontWeight="bold"))
  grafico$show('inline', include_assets = TRUE, cdn = TRUE)
  grafico
}

plotStats = function(dados) {
  require(rCharts)
  grafico <- nPlot(Seguidores ~ Dia, group="grupo", data = dados, type = 'lineChart')
  grafico$xAxis(tickFormat="#!function(d) {return d3.time.format('%d/%m')(new Date( d * 86400000 ));}!#")
  grafico$yAxis(tickFormat = "#!function(d) {return d3.format('0,.0')(d).replace(',','.')}!#")
  #try new interactive guidelines feature
  grafico$chart(useInteractiveGuideline=TRUE)
  grafico
}

plotReferencesByDay = function(dados) {
  require(rCharts)
  grafico <- nPlot(Referencias ~ Dia, group="grupo", data = dados, type = 'lineChart')
  grafico$xAxis(tickFormat="#!function(d) {return d3.time.format('%d/%m')(new Date( d * 86400000 ));}!#")
  grafico$yAxis(tickFormat = "#!function(d) {return d3.format('0,.0')(d).replace(',','.')}!#")
  #try new interactive guidelines feature
  grafico$chart(useInteractiveGuideline=TRUE)
  grafico
}

plotReferencesByHour = function(dados) {
  require(rCharts)
  dia = format(dados$Dia[1], "%d/%m")
  grafico = nPlot(Referencias ~ Hour, group="grupo", data = dados, type = 'lineChart')
#   grafico$xAxis(tickFormat="#!function(d) {return d3.time.format('%d/%m %H:%M')(new Date( d * 86400000 ));}!#")
  grafico$xAxis(tickFormat=paste0("#!function(d) {return '", dia, "' + ' ' + d + ':00';}!#"), axisLabel = "Hora do Dia")
  grafico$yAxis(tickFormat = "#!function(d) {return d3.format('0,.0')(d).replace(',','.')}!#", axisLabel = "Quantidade de Tweets")
  #try new interactive guidelines feature
  grafico$chart(useInteractiveGuideline=TRUE)
  grafico
}

statsByDay = function(tweets) {
  tweets$created_at2 = strptime(tweets$created_at, "%a %b %d %H:%M:%S %z %Y")
  tweets = tweets[,c('created_at2', 'followers_count', 'favourites_count', 'friends_count')]
  tweets$created_at2 = format(tweets$created_at2, "%m/%d")
  df = aggregate(. ~ created_at2, data = tweets, FUN=max)
  names(df) = c("Dia", "Seguidores", "Favoritos", "Amigos")
  df$Dia = as.POSIXlt(df$Dia, format="%m/%d")
  #   df$Dia = format(df$Dia, "%d/%m")
  df
}

## Lógica está errada!!
retweetsByDay = function(tweets) {
  tweets = tweets[,c('created_at2', 'retweet_count')]
  tweets$created_at2 = format(tweets$created_at2, "%m/%d")
  df = aggregate(. ~ created_at2, data = tweets, FUN=sum)
  names(df) = c("Dia", "Retweets")
  df$Dia = as.POSIXlt(df$Dia, format="%m/%d")
  df$Dia = format(df$Dia, "%d/%m")
  df
}

referenceByDay = function(tweets) {
  tweets$created_at2 = strptime(tweets$created_at, "%a %b %d %H:%M:%S %z %Y")
  tweets$created_at2 = format(tweets$created_at2, "%m/%d")
  df = aggregate(tweets$created_at2, by=list(tweets$created_at2), FUN=length)
  names(df) = c("Dia", "Referencias")
  df$Dia = as.POSIXlt(df$Dia, format="%m/%d")
#   df$Dia = format(df$Dia, "%d/%m")
  df
}

referenceByHour = function(tweets) {
  #Sys.setlocale("LC_TIME", "C") Rodar no windows isso, senão o strptime retorna NA
  tweets$created_at2 = strptime(tweets$created_at, "%a %b %d %H:%M:%S %z %Y")
  tweets$created_at2 = format(tweets$created_at2, "%m/%d %H:00")
  df = aggregate(tweets$created_at2, by=list(tweets$created_at2), FUN=length)
  names(df) = c("Dia", "Referencias")
  df$Hour = as.POSIXlt(df$Dia, format="%d/%m %H:%M")$hour
  #   df$Hour = format(df$Dia, "%H:%M")
  df$Dia = as.POSIXlt(df$Dia, format="%m/%d")
  #   df$Dia = format(df$Dia, "%d/%m %H:%M")
  df
}

loadTweets = function() {
  load('../../data/politics-tweets-2014-05-16.Rda')
  t = tweets
  load('../../data/politics-tweets-2014-05-17.Rda')
  t = rbind(t,tweets)
  load('../../data/politics-tweets-2014-05-18.Rda')
  t = rbind(t,tweets)
  load('../../data/politics-tweets-2014-05-19.Rda')
  t = rbind(t,tweets)
  load('../../data/politics-tweets-2014-05-20.Rda')
  t = rbind(t,tweets)
  load('../../data/politics-tweets-2014-05-21.Rda')
  t = rbind(t,tweets)
  load('../../data/politics-tweets-2014-05-22.Rda')
  t = rbind(t,tweets)
  load('../../data/politics-tweets-2014-05-23.Rda')
  t = rbind(t,tweets)
  load('../../data/politics-tweets-2014-05-24.Rda')
  t = rbind(t,tweets)
  load('../../data/politics-tweets-2014-05-25.Rda')
  t = rbind(t,tweets)
  load('../../data/politics-tweets-2014-05-26.Rda')
  t = rbind(t,tweets)
  load('../../data/politics-tweets-2014-05-27.Rda')
  t = rbind(t,tweets)
  load('../../data/politics-tweets-2014-05-28.Rda')
  t = rbind(t,tweets)
  load('../../data/politics-tweets-2014-05-29.Rda')
  t = rbind(t,tweets)
  load('../../data/politics-tweets-2014-05-30.Rda')
  t = rbind(t,tweets)
  load('../../data/politics-tweets-2014-05-31.Rda')
  t = rbind(t,tweets)
  load('../../data/politics-tweets-2014-06-01.Rda')
  t = rbind(t,tweets)
  load('../../data/politics-tweets-2014-06-02.Rda')
  t = rbind(t,tweets)
  load('../../data/politics-tweets-2014-06-03.Rda')
  t = rbind(t,tweets)
  load('../../data/politics-tweets-2014-06-04.Rda')
  t = rbind(t,tweets)
  load('../../data/politics-tweets-2014-06-05.Rda')
  t = rbind(t,tweets)
  load('../../data/politics-tweets-2014-06-06.Rda')
  t = rbind(t,tweets)
  load('../../data/politics-tweets-2014-06-07.Rda')
  t = rbind(t,tweets)
  load('../../data/politics-tweets-2014-06-08.Rda')
  t = rbind(t,tweets)
  load('../../data/politics-tweets-2014-06-09.Rda')
  t = rbind(t,tweets)
  load('../../data/politics-tweets-2014-06-10.Rda')
  t = rbind(t,tweets)
  load('../../data/politics-tweets-2014-06-11.Rda')
  t = rbind(t,tweets)
  load('../../data/politics-tweets-2014-06-12.Rda')
  t = rbind(t,tweets)
  load('../../data/politics-tweets-2014-06-13.Rda')
  t = rbind(t,tweets)
  load('../../data/politics-tweets-2014-06-14.Rda')
  t = rbind(t,tweets)
  load('../../data/politics-tweets-2014-06-15.Rda')
  t = rbind(t,tweets)
  t
}