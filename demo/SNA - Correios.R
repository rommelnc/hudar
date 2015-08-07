## Correios

source('funcoesTwitter.R', encoding='UTF-8')
arquivos = list.files('../data', pattern='politics.*2014-1(0-3|1-)[[:digit:]]*.json', full.names=TRUE)
arquivos = list.files('../data', pattern='politics.*2014-11-08-0[[:digit:]]*.json', full.names=TRUE)
arquivos = list.files('../data', pattern='politics.*2014-11-09-0[[:digit:]]*.json', full.names=TRUE)
arquivos = list.files('../data', pattern='politics.*2014-((11-(1|2|3))|12-)[[:digit:]]*.json', full.names=TRUE)
arquivos = arquivos[c(17,18)]
arquivos
salvarParseTweets(arquivos, TRUE)

source('funcoesTwitter.R', encoding='UTF-8')
source('funcoesSNA.R', encoding='UTF-8')
arquivos = list.files('../data', pattern='politics.*2014-1(1|2)-[[:digit:]]*.Rda', full.names=TRUE)
arquivos
## Analisar apenas os últimos X dias
arquivos = arquivos[(length(arquivos)-13):length(arquivos)]



# Tweets sobre os correios
palavras = c('correios','ECT')
tweets = recuperarTweetsEmArquivos(arquivos, palavras)
arquivo = '../datapolitics-tweets-2014-11-01-to-2014-12-12-correios.Rda'
save(tweets, file=arquivo)
analiseTweetsPorTema(arquivo, palavras)
load('../datapolitics-tweets-2014-11-01-to-2014-12-12-correios-resultado.Rda')
load(arquivo)
analiseGeralDosTemas(resultado, palavras, tweets, arquivo)
analiseDosTemas(resultado, arquivo)

# Colocar o servidor para rodar
require(servr)
setwd('../html/')
servr::httd()
