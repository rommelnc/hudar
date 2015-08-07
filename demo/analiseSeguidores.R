source('funcoesTwitter.R', encoding='UTF-8')
source('funcoesSNA.R', encoding='UTF-8')
## Julho e agosto
arquivos = list.files('../../data/', pattern='2014-07-[[:digit:]]*-all.Rda|2014-08-[[:digit:]]*-all.Rda', full.names=TRUE)
arquivos = arquivos[nchar(arquivos) != max(nchar(arquivos))]
arquivos
## Setembro
arquivos = list.files('../../data/', pattern='2014-09-[[:digit:]]*-all.Rda', full.names=TRUE)
arquivos = arquivos[nchar(arquivos) != max(nchar(arquivos))]
arquivos
## Analisar apenas os últimos X dias
# arquivos = arquivos[(length(arquivos)-13):length(arquivos)]
# Tweets sobre educação da última semana
tweets = recuperarTweetsEmArquivos(arquivos, 'DepPerpetua|DeputadoGladson| Acre | AC |Rio Branco|Deputada Perpétua|Deputada Perpetua|senadora Perpétua|senadora Perpetua|Perpétua Almeida|Perpetua Almeida|Deputado Gladson|senador Gladson|Gladson Cameli', c('DepPerpetua','DeputadoGladson'), TRUE)
# arquivo = '../../data/politics-tweets-2014-07-01-to-2014-08-31-acre-all.Rda'
# save(tweets, file=arquivo)
# palavras = c('DepPerpetua','DeputadoGladson')
## Julho e agosto
arquivo = '../../data/politics-tweets-2014-07-01-to-2014-08-31-acre-completo-all.Rda'
## Setembro
arquivo = '../../data/politics-tweets-2014-09-01-to-2014-09-23-acre-completo-all.Rda'
## Julho a setembro
arquivo = '../../data/politics-tweets-2014-07-01-to-2014-09-23-acre-completo-all.Rda'
save(tweets, file=arquivo)
## Remover tweets indesejados
remover = grep('AC ', tweets$text, ignore.case=TRUE)
deixar = grep('DepPerpetua|DeputadoGladson| Acre | AC |Rio Branco|Deputada Perpétua|Deputada Perpetua|senadora Perpétua|senadora Perpetua|Perpétua Almeida|Perpetua Almeida|Deputado Gladson|senador Gladson|Gladson Cameli', remover, ignore.case=TRUE)
remover = setdiff(remover, deixar)
tweets = tweets[-remover,]
## Adicionar tweets dos deputados

# palavras = c('DepPerpetua','DeputadoGladson')
palavras = c('DepPerpetua', 'DeputadoGladson', 'Acre ', ' AC ', 'Rio Branco', 'Deputada Perpétua', 'Deputada Perpetua', 'senadora Perpétua', 'senadora Perpetua', 'Perpétua Almeida', 'Perpetua Almeida', 'Deputado Gladson', 'senador Gladson', 'Gladson Cameli')
analiseTweetsPorTema(arquivo, palavras)
# load('../../data/politics-tweets-2014-07-01-to-2014-08-31-acre-all-resultado.Rda')
load('../../data/politics-tweets-2014-07-01-to-2014-08-31-acre-completo-all-resultado.Rda')
load(arquivo)
analiseGeralDosTemas(resultado, palavras, tweets, arquivo)
analiseDosTemas(resultado, arquivo)

source('funcoesTwitter.R', encoding='UTF-8')
## Julho a setembro
arquivo = '../../data/politics-tweets-2014-07-01-to-2014-09-23-acre-completo-all.Rda'
load(arquivo)
tweets = tweets[tweets$screen_name %in% c('DepPerpetua', 'DeputadoGladson'),]
usuarios = data.frame(usuario=c('DepPerpetua', 'DeputadoGladson'), nome=c('Perpétua', 'Gladson'))
datas = seq(as.Date('2014-07-01'), as.Date('2014-09-23'), by = 1)
stats = statsByUser(tweets, usuarios, 'diferenca', datas)
stats = statsByUser(tweets, usuarios, 'novos', datas)
stats = statsByUser(tweets, usuarios, 'todos', datas)
plotStats(stats)

## Verificar tweets nos dias que houve mais e menos novos seguidores
## Carrega os tweets dos deputados e calcula valor de novos seguidores (carregado acima)
stats = statsByUser(tweets, usuarios, 'diferenca', datas)
## Carrega todos os tweets relacionado aos deputados para consulta posterior
load(arquivo)
filtroDeputados = 'DepPerpetua|DeputadoGladson|Deputada Perpétua|Deputada Perpetua|senadora Perpétua|senadora Perpetua|Perpétua Almeida|Perpetua Almeida|Deputado Gladson|senador Gladson|Gladson Cameli'
usuarios = c('DepPerpetua', 'DeputadoGladson')
t = tweets[tweets$screen_name %in% usuarios,]
tweets = unique(rbind(tweets[grep(filtroDeputados, tweets$text, ignore.case = TRUE),], t))
## Esse if é para a conversão de data funcionar no Windows - strptime
if (Sys.info()['sysname'] == 'Windows') {
  lct <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "C")  
}
tweets$created_at2 = strptime(tweets$created_at, "%a %b %d %H:%M:%S %z %Y")
tweets$dia = as.Date(tweets$created_at2)
tweets$link = paste0('https://twitter.com/', tweets$screen_name, '/status/', tweets$id_str)

nomeUsuario = 'Perpétua'
usuario = 'DepPerpetua'
nomeUsuario = 'Gladson'
usuario = 'DeputadoGladson'
statsOrdenado = stats[stats$grupo == nomeUsuario,][order(stats[stats$grupo == nomeUsuario,'Seguidores'], decreasing = TRUE),]
statsOrdenado
dia = as.Date(statsOrdenado$Dia[1])
dia = as.Date(statsOrdenado$Dia[nrow(statsOrdenado)])
filtroDias = c(dia - 1, dia)
## Agora que já sei os dias, vou procurar os tweets
tweetsFiltrado = tweets[tweets$screen_name == usuario & tweets$dia %in% filtroDias,]
tweetsFiltrado = tweets[tweets$dia %in% filtroDias,]
tweetsFiltrado = tweetsFiltrado[order(tweetsFiltrado$retweet_count, decreasing = TRUE),]
tweetsFiltrado[,c('link','screen_name','dia','text','retweet_count','favorited')]
nomeSheet = paste(nomeUsuario, dia, statsOrdenado[statsOrdenado$Dia == dia, 'Seguidores'])
library(xlsx)
write.xlsx(tweetsFiltrado[,c('link','screen_name','dia','text','retweet_count','favorited')], '../../data/comparacaoSeguidores.xlsx', nomeSheet, append = TRUE)
