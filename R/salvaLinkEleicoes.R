decode_short_url <- function(url, parallel = FALSE, cpus = 1, ...) {
  # PACKAGES #
  require(RCurl)
  require(snowfall)
  
  # LOCAL FUNCTIONS #
  decode <- function(u, tries) {
    cat(paste("teste", u, "\n"))
    x <- try( getURL(u, header = TRUE, nobody = TRUE, followlocation = FALSE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")) )
    if(inherits(x, 'try-error') | length(grep(".*[lL]ocation: (\\S+).*", x))<1) {
      return(u)
    } else {
      ret = gsub(".*[lL]ocation: (\\S+).*", "\\1", x)
      if (str_length(ret) < 40 & tries < 4 ) {
        return(decode(ret, tries+1))
      }
      else {
        return(ret)
      }
    }
  }
  
  # MAIN #
  # return decoded URLs
  urls <- c(url, ...)
  l = list()
  if (parallel == FALSE) {
    l <- lapply(urls, decode, tries = 0)
  } else {
    sfInit( parallel=TRUE, cpus=cpus)
    #Rodo a função decode dessa forma local, pois por algum motivo, quando rodava em paralelo chamando decode, ele da erro dizendo que nao encontra a função
    l <- sfLapply(urls, local(decode <- function(u, tries) {  require(RCurl);  require(stringr);  cat(paste("teste", u, "\n"));  x <- try( getURL(u, header = TRUE, nobody = TRUE, followlocation = FALSE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")) );  if(inherits(x, 'try-error') | length(grep(".*[lL]ocation: (\\S+).*", x))<1) {  return(u);  } else {    ret = gsub(".*[lL]ocation: (\\S+).*", "\\1", x);    if (str_length(ret) < 40 & tries < 4 ) {      return(decode(ret, tries+1));    }    else {      return(ret);    }  }}), tries = 0)
    sfStop()
  }
  names(l) <- urls
  return(l)
}


getShortLinks = function(tweets){
  library(stringr)
  links = str_extract_all(tweets$text, "(https?://t.co[^(\n|\t| |\")][^(\n|\t| |\")]+)")
  links = unlist(links)
  links = links[str_length(links) > 20]
  links
}


getDecodedLinks = function(tweets, parallel = FALSE, cpus = 1){
  links = getShortLinks(tweets)
  uniq = unique(links)
  decoded = decode_short_url(uniq, parallel, cpus)
  decodedLinks = links
  m = match(links, names(decoded))
  change <- !is.na(m)
  decodedLinks[change] <- decoded[m[change]]
  names(decodedLinks) <- names(decoded[m[change]])
  decodedLinks
}

getTweetsFromFullUrl = function(url, tweets, decodedLinks){ 
  short_links = names(which(decodedLinks == url))
  tweetsWithUrl = lapply(short_links, function(x) grep(x, tweets$text))
  tweetsWithUrl = unique(unlist(tweetsWithUrl))  
  tweetsWithUrl = tweets[tweetsWithUrl,]
  tweetsWithUrl
}

getUrlsFromTweets = function(tweets, decodedLinks, table = FALSE) {
  short_links = getShortLinks(tweets)
  urls = sapply(short_links, function(x) decodedLinks[x])
  urls = as.character(unlist(urls))
  if (table == TRUE) {
    urls = sort(table(urls), decreasing=TRUE)
  }
  urls
}

###########################################COMO BUSCAR OS LINKS DE UM RDA.##########################################################

#O RDA que for gerado pela função salvaLinkRda vai possuir os tweets e uma lista "links" que relaciona todos os 
#links que aparecem naqueles tweets ao seu link completo (incluindo repetições)
#Ex: u1 -> urlx, u2->urly, u3->urlx, u1-> urlx

#1. Carregar o rda
#2. Selecionar os tweets desejados, por exemplo: eleicoes = recuperarTweetsEleicoes(tweets)

# getUrlsFromTweets(eleicoes, links, table=FALSE)
#   Retorno:
#       [1] "http://twitter.com/BrpadraoFIFA/status/487289675078709248/photo/1"                                             
#       [2] "http://twitter.com/BrpadraoFIFA/status/487289675078709248/photo/1"                                             
#       [3] "http://twitter.com/RomarioOnze/status/488457861971062784/photo/1"                                              
#       [4] "http://twitter.com/marcospeixoto72/status/483792987689074688/photo/1"                                          
#       [5] "http://twitter.com/SINPOFAL/status/488040218399084544/photo/1"          
#       ...


# getUrlsFromTweets(eleicoes, links, table=TRUE)
#   Retorno:
#           http://twitter.com/BrpadraoFIFA/status/487289675078709248/photo/1 
#           50 
#           http://twitter.com/BrpadraoFIFA/status/488323612269871104/photo/1 
#           48 
#           http://twitter.com/RomarioOnze/status/488457861971062784/photo/1 
#           30 
#           http://twitter.com/SINPOFAL/status/488040218399084544/photo/1 
#           22         
#           ...



########################################################################################################################


salvaLinkEleicoesRda <- function(rdaFile, sourceFolder = '') {
  variaveis = load(rdaFile)
  
  ## Buscar os tweets das eleicoes apenas se não tiver no arquivo Rda carregado
  if (!('eleicoes' %in% variaveis)) {
    source(paste0(sourceFolder, 'funcoesTwitter.R'))
    eleicoes = recuperarTweetsEleicoes(tweets)
  }
  
  eleicoes$text = gsub("[^[:graph:]]", " ",eleicoes$text)
  
  linkEleicoes = getDecodedLinks(eleicoes, TRUE, 4)  
  save(list = unique(c(variaveis, 'eleicoes', 'linkEleicoes')), file = rdaFile)
}

salvaLinkRda <- function(rdaFile) {
  load(rdaFile)
  tweets$text = gsub("[^[:graph:]]", " ",tweets$text)
  links = getDecodedLinks(tweets, TRUE, 4)  
  #links = getDecodedLinks(tweets)    
  #topLinks = sort(table(as.character(links)), decreasing=TRUE)
  save(tweets, links, file = rdaFile)
}

# source('salvaLinkEleicoes.R')
# salvaLinkEleicoesRda("../../data/politics-tweets-2014-06-30-2.Rda")
# salvaLinkEleicoesRda("../../data/politics-tweets-2014-06-30.Rda")
# salvaLinkRda("../../data/politics-tweets-2014-07-04-copa-jogo.Rda")
# salvaLinkRda('../../data/politics-tweets-2014-07-04-copa-jogo-simples.Rda')
# salvaLinkRda('../../data/politics-tweets-2014-07-08-copa-jogo-simples.Rda')
# salvaLinkEleicoesRda("../../data/politics-tweets-2014-07-01-all.Rda")
# salvaLinkEleicoesRda("../../data/politics-tweets-2014-07-02-all.Rda")
# salvaLinkEleicoesRda("../../data/politics-tweets-2014-07-03-all.Rda")
# salvaLinkEleicoesRda("../../data/politics-tweets-2014-07-04-all.Rda")
# salvaLinkEleicoesRda("../../data/politics-tweets-2014-07-05-all.Rda")
# salvaLinkEleicoesRda("../../data/politics-tweets-2014-07-08-all.Rda")
# salvaLinkEleicoesRda("../../data/politics-tweets-2014-07-09-all.Rda")
# salvaLinkEleicoesRda('../../data/politics-tweets-2014-07-01-to-2014-07-31-eleicoes.Rda')
