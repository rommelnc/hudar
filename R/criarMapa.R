
library(raster)
library(streamR)






getLocalizacaoEstado = function(tweets) {
  require (stringr)
  
  municipios = read.csv("municipios.csv", header = TRUE, sep=";", fileEncoding="UTF-8")
  municipios$sigla_uf_space = tolower(paste("",municipios$sigla_uf,""))
  estadosNome = c(" acre ", " alagoas ", " amapa ", " amazonas ", " bahia ", " ceara ", " distrito federal ", " espirito santo ", " goias ", " maranhao ", " mato grosso ", " mato grosso do sul " , " minas gerais ", " para ", " paraiba ", " parana ", " pernambuco ", " piaui ", " rio de janeiro ", " rio grande do norte ", " rio grande do sul ", " rondonia ", " roraima ", " santa catarina ", " sao paulo ", " sergipe ", " tocantins ")
  estadosSigla = c(" ac ", " al ", " ap ", " am ", " ba ", " ce ", " df ", " es ", " go ", " ma ", " mt ", " ms ", " mg ", " pa ", " pb ", " pr ", " pe ", " pi ", " rj ", " rn ", " rs ", " ro ", " rr ", " sc ", " sp ", " se ", " to ")
  municipios$municipios_lower = paste("", tolower(iconv(municipios$municipio, to="ASCII//TRANSLIT")), "")
  
  #Porcentagem de location via GPS
  #sum(!is.na(tweets$full_name))/nrow(tweets)
  
  
  #Porcentagem de location via profile != null
  #sum(tweets.df$location != '')/nrow(tweets)


  ######################################Geolocalizacao

  #Separa full_name (geolocaliza??o) com espa?os, lowercase, e remove acentos
  cidadeEstado = strsplit(tweets$full_name, ",") #da o split em "cidade, estado"
  soEstado = unlist(lapply(cidadeEstado, function(x) x[2])) #seleciona a segunda parte do split
  tweets$geo_splitted = paste("", tolower(iconv(soEstado, from="UTF-8", to="ASCII//TRANSLIT")), "") 

  resultadoGeo <- do.call(rbind,lapply(tweets$geo_splitted,function(geolocation) {  
    state = ""
    indx <- sapply(estadosNome, grepl, geolocation,fixed=T) # all matching cities
    indx <- which(indx)                             # use only first match!!
    if (sum(indx) != 0) {
      state = estadosSigla[indx]
    }
    c(state)
  }))
  resultadoGeo = resultadoGeo[,ncol(resultadoGeo)] # casos onde da o match com "mato grosso" e "mato grosso do sul"

  
  ######################################Sigla do estado em Location
  
  
  #Separa location com espa?os, lowercase, e remove acentos
  tweets$location_splitted = paste("", tolower(iconv(gsub("[-/.,;\\]", " ", tweets$location), from="UTF-8", to="ASCII//TRANSLIT")), "")
  
  resultadoEstadoSigla <- do.call(rbind,lapply(tweets$location_splitted,function(location) {  
    state = ""
    indx <- sapply(estadosSigla, grepl, location,fixed=T) # all matching cities
    indx <- which(indx)                             # use only first match!!
    
    if (sum(indx) != 0) {
      state = estadosSigla[indx]
    }
    state
  }))
  resultadoEstadoSigla = resultadoEstadoSigla[,ncol(resultadoEstadoSigla)] #casos de location onde aparece "sp/rj/df/brasil"


  
  ######################################Nome do estado por extenso em Location
  
  tweets$location_splitted = paste("", tolower(iconv(gsub("[-/.,;\\]", " ", tweets$location), from="UTF-8", to="ASCII//TRANSLIT")), "")
  
  resultadoEstadoNome <- do.call(rbind,lapply(tweets$location_splitted,function(location) {  
    state = ""
    indx <- sapply(estadosNome, grepl, location,fixed=T) # all matching cities
    indx <- which(indx)                             # use only first match!!
    
    if (sum(indx) != 0) {
      state = estadosSigla[indx]
    }
    state
  }))
  resultadoEstadoNome = resultadoEstadoNome[,ncol(resultadoEstadoNome)] #casos de location onde aparece "Rio de Janeiro/ Espir?to Santo" ou "Ji-Paran? - Rond?nia - Brasil"

  
  
  ######################################Nome do municipio em Location
  
  
  tweets$location_splitted = paste("", tolower(iconv(gsub("[-/.,;\\]", " ", tweets$location), from="UTF-8", to="ASCII//TRANSLIT")), "")
  
  resultadoMunicipioNome <- do.call(rbind,lapply(tweets$location_splitted,function(location) {  
    state = ""
    indx <- sapply(municipios$municipios_lower, grepl, location,fixed=T) # all matching cities
    indx <- which(indx)                             # use only first match!!
    
    if (sum(indx) != 0) {
      state = paste("",tolower(municipios$sigla_uf[indx]),"")
    }
    state
  }))
  resultadoMunicipioNome = resultadoMunicipioNome[,ncol(resultadoMunicipioNome)] #casos de location onde aparece "Rio de Janeiro/ Espir?to Santo" ou "Ji-Paran? - Rond?nia - Brasil"


  resultados = data.frame(resultadoGeo, resultadoEstadoSigla, resultadoEstadoNome, resultadoMunicipioNome)
  
  resultados$resultadoGeo = as.character(resultados$resultadoGeo)
  resultados$resultadoEstadoSigla = as.character(resultados$resultadoEstadoSigla)
  resultados$resultadoEstadoNome = as.character(resultados$resultadoEstadoNome)
  resultados$resultadoMunicipioNome = as.character(resultados$resultadoMunicipioNome)
  
  
  resultados$final = resultados$resultadoGeo #Procura resultado de geolocalizacao
  
  indices = which(resultados$final == "")
  resultados$final[indices] = resultados$resultadoEstadoSigla[indices] #Se nao tiver geolocalizacao pega pela sigla do estado
  
  indices = which(resultados$final == "")
  resultados$final[indices] = resultados$resultadoEstadoNome[indices] #Se nao tiver sigla pega pelo nome do estado
  
  indices = which(resultados$final == "")
  resultados$final[indices] = resultados$resultadoMunicipioNome[indices] #Se nao tiver nome pega pelo nome do municipio
  
  
  estado.df = as.factor(toupper(str_trim(resultados$final)))
  estado.df

}


load('../data/politics-tweets-2014-05-16.Rda')

tweets.df <- parseTweets("../../data/politics-tweets-2014-05-14.json", simplify = FALSE)

eleicoes_full = tweets[grep("eleicao|eleição|eleicoes|eleições|Dilma Rousseff|Aécio Neves|Aecio Neves|Eduardo Campos|Marina Silva|aecio|dilma|marina|presidencia|presidência|lula|presidenta|pt|psb|psdb", tweets$text, ignore.case=TRUE),]
eleicoes = eleicoes_full[1:500,]
tweets$estado = getLocalizacaoEstado(eleicoes)


#GERA??O DO MAPA
#install.packages("maptools", dep=TRUE)
#install.packages("RColorBrewer",dep=TRUE)
#install.packages("plotrix",dep=TRUE)
#install.packages("classInt",dep=TRUE)

library(maptools)
library(seewave)

png("map-15-16-maio.png", 1920, 1080)
estados = readShapeSpatial("estadosl_2007.shp")
dados = as.vector(summary(tweets$estado)[2:28])
dados = dados/sum(dados)
cores = (rev.heat.colors(length(dados)+10)[rank(dados)])
plot(estados, lwd=.5, axes=FALSE, las=1, col=cores)
text(getSpPPolygonsLabptSlots(estados), as.character(estados@data$SIGLAUF3), cex=.8)
dev.off()
graphics.off()