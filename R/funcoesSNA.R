analiseEleicoesPorTema = function(arquivo, above = .01) {
  source('R/criarGrafo.R', encoding='UTF-8')
  source('R/funcoesTwitter.R', encoding='UTF-8')
  
  variaveis = load(arquivo)
  
  ## Buscar os tweets das eleicoes apenas se não tiver no arquivo Rda carregado
  if (!('eleicoes' %in% variaveis)) {
    eleicoes = recuperarTweetsEleicoes(tweets)
    save(list = unique(c(variaveis, 'eleicoes')), file = arquivo)
  }
  palavras = recuperarPalavras()
  
  analisePorTema(eleicoes, arquivo, palavras, above)
}

analiseTweetsPorTema = function(arquivo, palavras, above = .01) {
  source('R/criarGrafo.R', encoding='UTF-8')
  source('R/funcoesTwitter.R', encoding='UTF-8')
  
  variaveis = load(arquivo)
  
  ## Se não tiver a variável tweets no arquivo Rda carregado, acusar erro
  if (!('tweets' %in% variaveis)) {
    stop('Deve haver um data frame com nome tweets no arquivo Rda.')
  }
  
  analisePorTema(tweets, arquivo, palavras, above)
}

analisePorTema = function(tweets, arquivo, palavras, above = .01) {
  source('R/criarGrafo.R', encoding='UTF-8')
  source('R/funcoesTwitter.R', encoding='UTF-8')
  source('R/utils.R', encoding='UTF-8')
  
  textos = removerAcento(tweets$text)
  
  ## Remover urls
  textos = gsub('https?://.*[^(\n|\t| |\")][^(\n|\t| |\")]+', ' ',textos)
  
  # Salva grafo de temas do dia
  grafo = criarGrafo(textos, remove = palavras, above)
  require(igraph)
  write.graph(grafo, file=sub("\\.Rda", "-themes\\.GML", arquivo), format="GML")
  
  peso = recuperarPesoAresta(grafo, .97)
  g = delete.edges(grafo, which(E(grafo)$weight < peso))
  
  res = recuperarTemas(g)
  
  temas <- res$temas
  g <- res$grafo
  networkData <- res$networkData
  
  ideg = degree(g, mode = "in", loops = F)
  V(g)$label.cex = (ideg / max(ideg)) + 0.5
  
  ## Simplificar temas, se houver tema com mais de 10 palavras
  temas.simples = temas
  for (i in 1:length(temas)) {
    temas.simples[[i]] = names(sort(ideg[names(ideg) %in% temas[[i]]], decreasing=TRUE))[1:ifelse(length(temas[[i]]) > 10, 10, length(temas[[i]]))]
  }
  
  ## Recuperar tweets por tema simplificado (combinações no máximo das 10 palavras mais frequentes)
  resultado = recuperarTweetsPorTema(temas.simples, tweets, TRUE)
  ## Reordenar temas de acordo com importância (quantidade de usuários envolvidos)
  qtdUsuarios = lapply(resultado$tweets, FUN = function(x) {length(unique(x$screen_name))})
  qtdTweets = lapply(resultado$tweets, FUN = nrow)
  ordem = order(unlist(qtdUsuarios), decreasing=TRUE)
  resultado$temas = resultado$temas[ordem]
  resultado$qtdUsuarios = qtdUsuarios[ordem]
  resultado$qtdTweets = qtdTweets[ordem]
  resultado$tweets = resultado$tweets[ordem]
  resultado$networkData = networkData
  # resultado$imagem = sub("\\.Rda", '\\.png', arquivo)
  #   ## Pensar em mudar a forma de representar o resultado no futuro como se fossem objetos Tema com atributos
  #   for (i in 1:length(resultado)) {
  #     resultado$temas[[i]]$palavras = resultado$temas[[1]]
  #     resultado$temas[[1]]$qtdUsuarios = qtdUsuarios[[1]]
  #     resultado$temas[[1]]$tweets = resultado$tweets[[1]]
  #     ...
  #   }
  save(resultado, file=sub("\\.Rda", '-resultado\\.Rda', arquivo))
  
  ## Gera painel geral com lista de todos os temas e gráficos, etc
  analiseGeralDosTemas(resultado, palavras, tweets, arquivo)
  
  ## Gera painel com grafo de usuários e gráficos de barra de mais twitados, etc
  # analiseDosTemas(resultado, arquivo)
  
  resultado
}

analiseGrafoUsuarios = function(tweets, arquivo) {
  matriz = montarMatrizUsuarios(tweets, T)
  g = criarGrafoDePara(matriz)
  
  ## random walk
  eb <- walktrap.community(g)
  member <- community.to.membership(g, eb$merges, step=nrow(eb$merges)-10)
  sizeCluster = sum(member$csize > 2)
  
  require(RColorBrewer)
  # create nice colors for each cluster
  # gbrew = colorRampPalette(brewer.pal(8, "Dark2"))(max(com$membership)+1)
  gbrew = colorRampPalette(brewer.pal(8, "Dark2"))(sizeCluster+1)
  gpal = rgb2hsv(col2rgb(gbrew))
  k = sizeCluster+1
  corOutros = hsv(gpal[1,k], gpal[2,k], gpal[3,k], alpha=0.5)
  gcols = rep(corOutros, length(member$membership))
  for (k in 0:sizeCluster-1) {
    gcols[member$membership == k] = hsv(gpal[1,k+1], gpal[2,k+1], gpal[3,k+1], alpha=0.5)
  }
  V(g)$color = gcols
  
  ideg = degree(g, mode = "in", loops = F)
  
  V(g)$label.cex = (ideg / max(ideg) * 1.8) + 0.1
  
  V(g)$size = (ideg / max(ideg) * 20) + 5
  
  png(filename = arquivo, width=3840, height=3840, res=300)
  
  result = tryCatch({
    plot.igraph(g, 
                vertex.label = V(g)$name, 
                edge.arrow.size=0.2, 
                edge.curved = F, 
                layout = layout.fruchterman.reingold
    )
  }, error = function(e) {
    print('Deu erro ao tentar plotar o grafo de usuários...')
  })
  dev.off()
  
  g
}

analiseGeralDosTemas = function(resultado, palavras, tweets, arquivo) {
  # Salva grafo de usuários do dia
  #  grafo = analiseGrafoUsuarios(tweets, sub("\\.Rda", '-users\\.png', arquivo))
  #  write.graph(grafo, file=sub("\\.Rda", "-users\\.GML", arquivo), format="GML")
  
  ## TODO: Já tenho que filtrar os temas que vão pra final aqui...
  
  # Salva gráficos de mais tweetados, etc do dia
  listaTemas = resultado$temas
  temas = palavras[palavras != 'http']
  #   fileConn<-file("parametros.R")
  #   save(tweets, file='tweets.Rda')
  #   save(temas, file='temas.Rda')
  #   grafo = g
  #   save(grafo, file='grafo.Rda')
  local = sub("\\.Rda", '-themes.png', arquivo)
  local = sub("../../data/", '', local)
  #   save(local, file='local.Rda')
  #   save(listaTemas, file='listaTemas.Rda')
  #   writeLines(c("load('tweets.Rda')", "load('temas.Rda')", "load('grafo.Rda')", "load('local.Rda')", "index = T"),
  #              fileConn)
  #   close(fileConn)
  
  networkData <- resultado$networkData
  
  index = T
  knitr::knit2html(input='PainelGraficosBarra.Rmd', output=sub("\\.Rda", '-plots\\.html', arquivo))
}

analiseDosTemas = function(resultado, arquivo) {
  source('R/criarGrafo.R', encoding='UTF-8')
  source('R/funcoesTwitter.R', encoding='UTF-8')
  for (i in 1:length(resultado$temas)) {
    # Salva grafo de usuários por tema
    temp = paste0('-theme', i, '-users\\.png')
    grafo = analiseGrafoUsuarios(resultado$tweets[[i]], sub("\\.Rda", temp, arquivo))
    temp = paste0('-theme', i, '-users\\.GML')
    write.graph(grafo, file=sub("\\.Rda", temp, arquivo), format="GML")
    
    # Salva gráficos de mais tweetados, etc por tema
    temp = paste0('-theme', i, '-plots\\.html')
    tweets = resultado$tweets[[i]]
    temas = resultado$temas[[i]]
#     fileConn<-file("parametros.R")
#     save(tweets, file='tweets.Rda')
#     save(temas, file='temas.Rda')
#     save(grafo, file='grafo.Rda')
    local = sub("\\.Rda", paste0('-theme', i, '-users.png'), arquivo)
    local = sub("../../data/", '', local)
#     save(local, file='local.Rda')
#     writeLines(c("load('tweets.Rda')", "load('temas.Rda')", "load('grafo.Rda')", "load('local.Rda')", "index = F"),
#                fileConn)
#     close(fileConn)
    index = F
    
    knitr::knit2html(input='PainelGraficosBarra.Rmd', output=sub("\\.Rda", temp, arquivo))
  }
}



recuperaDataFrameGraficoTemas = function(resultado) {
  temas = c()
  for (i in 1:length(resultado$temas)) {
    temas = c(temas, paste(resultado$temas[[i]], collapse=", "))
  }
  return(data.frame(temas=temas, qtdUsuarios = unlist(resultado$qtdUsuarios), qtdTweets = unlist(resultado$qtdTweets)))
}


recuperarTweetsPorTema = function(temas, tweets, debug = F) {
  tweetsPorTema = replicate(length(temas), c())
  for (i in 1:length(temas)) {
    tweetsPorTema[[i]] = recuperarTweetsPorPalavras(temas[[i]], tweets, debug)
  }
  list(temas = temas, tweets = tweetsPorTema)
}


recuperarTweetsPorPalavras = function(palavras, tweets, debug = F) {
  source('R/utils.R')
  palavras = removerAcento(palavras)
  textos = tweets$text
  textos = removerAcento(textos)
  
  if (length(palavras) == 3) {
    combinacoes = combn(palavras, 2)
  } else {
    combinacoes = combn(palavras, 3)
  }
  
  if (debug) {
    print('Pesquisando tweets por combinações de palavras:')
    print(palavras)
    # create progress bar
    pb = txtProgressBar(min = 0, max = dim(combinacoes)[2], style = 3)
  }
  if (length(palavras) == 3) {
    index = intersect(grep(combinacoes[1,1],textos,ignore.case=T),grep(combinacoes[2,1],textos,ignore.case=T))
  } else {
    index = intersect(grep(combinacoes[3,1],textos,ignore.case=T),intersect(grep(combinacoes[1,1],textos,ignore.case=T),grep(combinacoes[2,1],textos,ignore.case=T)))
  }
  for (i in 2:dim(combinacoes)[2]) {
    #     print(length(index))
    if (length(palavras) == 3) {
      index = union(index,intersect(grep(combinacoes[1,i],textos,ignore.case=T),grep(combinacoes[2,i],textos,ignore.case=T)))
    } else {
      index = union(index,intersect(grep(combinacoes[3,i],textos,ignore.case=T),intersect(grep(combinacoes[1,i],textos,ignore.case=T),grep(combinacoes[2,i],textos,ignore.case=T))))
    }
    if  (debug) {
      # update progress bar
      setTxtProgressBar(pb, i)
    }
  }
  tweets[index,]
}


recuperarTemas = function(g) {
  require(igraph)
  fc <- fastgreedy.community(g)
  table(fc$membership)
  comunidades = as.integer(names(table(fc$membership)[table(fc$membership) > 2]))
  comunidades
  qtdComunidades = length(comunidades)
  temas = replicate(qtdComunidades, c())
  for (i in 1:qtdComunidades) {
    temas[[i]] = fc$names[which(fc$membership == comunidades[i])]
  }
  
  g <- defineTopicsColor(g, fc$membership)
  
  networkData <- createNetworkData(g, temas, fc$membership)
  
  list(temas = temas, grafo = g, networkData = networkData)
}

defineTopicsColor <- function(graph, membership) {
  topics.size <- length(unique(membership))
  require(RColorBrewer)
  # create nice colors for each cluster
  gbrew <- colorRampPalette(brewer.pal(8, "Dark2"))(topics.size+1)
  gpal <- rgb2hsv(col2rgb(gbrew))
  k <- topics.size+1
  color.others<- hsv(gpal[1,k], gpal[2,k], gpal[3,k], alpha=0.5)
  gcols <- rep(color.others, length(membership))
  for (k in 0:topics.size-1) {
    gcols[membership == k] <- hsv(gpal[1,k+1], gpal[2,k+1], gpal[3,k+1], alpha=0.5)
  }
  V(graph)$color <- gcols
  
  graph
}

createNetworkData <- function(graph, topics, memberships, others.label = 'outros') {
  ideg <- degree(graph, mode = "in", loops = F)
  nodes <- data.frame(name = as.factor(V(graph)$name), group = as.vector(memberships), size = (ideg + 2) * 3)
  for (k in 1:length(topics)) {
    nodes$group[nodes$name %in% topics[[k]]] = paste0(topics[[k]], collapse = ' / ')
  }
  nodes$group[!nodes$name %in% unlist(topics)] = others.label
  
  m <- get.data.frame(graph)
  
  source.node <- sapply(m$from, function(x) which(levels(nodes$name) == x))
  target.node <- sapply(m$to, function(x) which(levels(nodes$name) == x))
  
  networkData <- data.frame(source = source.node-1, target = target.node-1, value = scale(m$weight, center = FALSE))
  networkData$nodes <- nodes
  
  networkData
}


recuperarPesoAresta = function(grafo, prob = .8) {
  arestas = sort(E(grafo)$weight, decreasing=T)
  peso = arestas[round( (1-prob) * length(arestas))]
  #   parada = sum(arestas) * prob
  #   soma = 0
  #   peso = 0
  #   for (i in 1:length(arestas)) {
  #     soma = soma + arestas[i]
  #     if (soma > parada) {
  #       peso = arestas[i]
  #       break
  #     }
  #   }
  peso
}
