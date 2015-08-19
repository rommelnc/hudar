source('funcoesTwitter.R', encoding='UTF-8')
dias = seq(as.Date('2014-07-16'), as.Date('2014-07-30'), by = 1)
dias = seq(as.Date('2014-07-01'), as.Date('2014-07-30'), by = 1)
tweets = joinRdas(dias, debug = TRUE)
remove(tweets)

source('funcoesSNA.R', encoding='UTF-8')
resultado = analisePorDia('../../data/politics-tweets-2014-05-16.Rda')
resultado = analisePorDia('../../data/politics-tweets-2014-05-17.Rda')
resultado = analisePorDia('../../data/politics-tweets-2014-05-18.Rda')
resultado = analisePorDia('../../data/politics-tweets-2014-05-19.Rda')
resultado = analisePorDia('../../data/politics-tweets-2014-05-20.Rda')
resultado = analisePorDia('../../data/politics-tweets-2014-05-21.Rda')
resultado = analisePorDia('../../data/politics-tweets-2014-05-22.Rda')
resultado = analisePorDia('../../data/politics-tweets-2014-05-23.Rda')
resultado = analisePorDia('../../data/politics-tweets-2014-07-15-all.Rda')
resultado = analisePorDia('../../data/politics-tweets-2014-07-01-to-2014-07-15-all.Rda')
resultado = analisePorDia('../../data/politics-tweets-2014-07-16-to-2014-07-30-all.Rda')
resultado = analisePorDia('../../data/politics-tweets-2014-07-16-to-2014-07-30-eleicoes.Rda')
resultado = analisePorDia('../../data/politics-tweets-2014-07-16-to-2014-07-31-eleicoes.Rda')
resultado = analisePorDia('../../data/politics-tweets-2014-07-01-to-2014-07-30-all.Rda')
resultado = analisePorDia('../../data/politics-tweets-2014-07-01-to-2014-07-31-eleicoes.Rda')

datas = '2014-07-01-to-2014-07-15'
arquivo = paste0('../../data/politics-tweets-', datas, '-all.Rda', collapse='')
load(paste0('../../data/html/politics-tweets-', datas, '-all-resultado.Rda', collpase=''))
analiseDosTemas(resultado, arquivo)

datas = '2014-07-16-to-2014-07-31'
arquivo = paste0('../../data/politics-tweets-', datas, '-eleicoes.Rda', collapse='')
load(paste0('../../data/html/politics-tweets-', datas, '-eleicoes-resultado.Rda', collpase=''))
analiseDosTemas(resultado, arquivo)

datas = '2014-07-01-to-2014-07-31'
arquivo = paste0('../../data/politics-tweets-', datas, '-eleicoes.Rda', collapse='')
load(paste0('../../data/html/politics-tweets-', datas, '-eleicoes-resultado.Rda', collpase=''))
analiseDosTemas(resultado, arquivo)

## Resposta ao pedido de novidade da Eliane Trindade da Folha (pediu às 19:27 do dia 20/08, mas só vi às 22:30)
dia = as.Date('2014-08-01')
for (i in 1:11) {
  arquivo = paste0("../../data/politics-tweets-", dia, "-all.Rda")
  eleicoes = salvarTweetsEleicoes(arquivo)
  arquivo = paste0("../../data/politics-tweets-", dia, "-eleicoes-all.Rda")
  save(eleicoes, file=arquivo)
  dia = dia + 1
}

source('funcoesSNA.R', encoding='UTF-8')
dia = as.Date('2014-08-16')
for (i in 1:15) {
  arquivo = paste0("../../data/politics-tweets-", dia, "-eleicoes-all.Rda")
  analisePorDia(arquivo)
  dia = dia + 1
}

analisePorDia('../../data/politics-tweets-2014-08-20-eleicoes-all.Rda')












source('funcoesTwitter.R', encoding='UTF-8')
dias = seq(as.Date('2014-08-17'), as.Date('2014-08-20'), by = 1)
# tweets = joinRdas(dias, debug = TRUE)
eleicoes = joinRdasEleicoes(dias, debug = TRUE)
remove(eleicoes)
resultado = analisePorDia('../../data/politics-tweets-2014-08-17-to-2014-08-20-eleicoes-all.Rda')


## Dia anterior, dia (acidente foi por volta das 10 horas) e dia posterior a morte de Campos
source('funcoesSNA.R', encoding='UTF-8')
dia = as.Date('2014-08-28')
for (i in 1:3) {
  arquivo = paste0("../../data/politics-tweets-", dia, "-all.Rda")
  eleicoes = salvarTweetsEleicoes(arquivo)
  arquivo = paste0("../../data/politics-tweets-", dia, "-eleicoes-all.Rda")
  save(eleicoes, file=arquivo)
  dia = dia + 1
}

source('funcoesSNA.R', encoding='UTF-8')
dia = as.Date('2014-08-16')
for (i in 1:15) {
  arquivo = paste0("../../data/politics-tweets-", dia, "-eleicoes-all.Rda")
#   analisePorDia(arquivo)
  analiseEleicoesPorTema(arquivo)
  dia = dia + 1
}

source('funcoesTwitter.R', encoding='UTF-8')
dias = seq(as.Date('2014-08-01'), as.Date('2014-08-30'), by = 1)
# tweets = joinRdas(dias, debug = TRUE)
eleicoes = joinRdasEleicoes(dias, debug = TRUE)
remove(eleicoes)
resultado = analisePorDia('../../data/politics-tweets-2014-08-12-to-2014-08-14-eleicoes-all.Rda')


datas = '2014-08-20'
arquivo = paste0('../../data/politics-tweets-', datas, '-eleicoes-all.Rda', collapse='')
load(paste0('../../data/politics-tweets-', datas, '-eleicoes-all-resultado.Rda', collpase=''))
analiseDosTemas(resultado, arquivo)


source('R/funcoesTwitter.R', encoding='UTF-8')
source('R/funcoesSNA.R', encoding='UTF-8')
arquivos = list.files('../../data/', pattern='2014-08-[[:digit:]]*-all.Rda', full.names=TRUE)
## Analisar apenas os últimos X dias
arquivos = arquivos[(length(arquivos)-13):length(arquivos)]

# Tweets sobre as eleições
arquivo = 'data/politics-tweets-2014-08-17-to-2014-08-20-eleicoes-all.Rda'
analiseEleicoesPorTema(arquivo)
load('../../data/politics-tweets-2014-08-17-to-2014-08-20-eleicoes-all-resultado.Rda')
load(arquivo)
analiseGeralDosTemas(resultado, palavras, tweets, arquivo)
analiseDosTemas(resultado, arquivo)

# Tweets sobre educação da última semana
tweets = recuperarTweetsEmArquivos(arquivos, 'educação|educacao|educaçao|educacão')
arquivo = '../../data/politics-tweets-2014-08-17-to-2014-08-20-educacao-all.Rda'
save(tweets, file=arquivo)
palavras = c('educação','educacao','educaçao','educacão')
analiseTweetsPorTema(arquivo, palavras)
load('../../data/politics-tweets-2014-08-17-to-2014-08-20-educacao-all-resultado.Rda')
load(arquivo)
analiseGeralDosTemas(resultado, palavras, tweets, arquivo)
analiseDosTemas(resultado, arquivo)

# Tweets sobre saúde da última semana
tweets = recuperarTweetsEmArquivos(arquivos, 'saude|saúde')
arquivo = '../../data/politics-tweets-2014-08-17-to-2014-08-20-saude-all.Rda'
save(tweets, file=arquivo)
palavras = c('saude','saúde')
analiseTweetsPorTema(arquivo, palavras)
load('../../data/politics-tweets-2014-08-17-to-2014-08-20-saude-all-resultado.Rda')
load(arquivo)
analiseGeralDosTemas(resultado, palavras, tweets, arquivo)
analiseDosTemas(resultado, arquivo)

# Tweets sobre segurança da última semana
tweets = recuperarTweetsEmArquivos(arquivos, 'seguranca|segurança')
arquivo = '../../data/politics-tweets-2014-08-17-to-2014-08-20-seguranca-all.Rda'
save(tweets, file=arquivo)
palavras = c('seguranca','segurança')
analiseTweetsPorTema(arquivo, palavras)
load('../../data/politics-tweets-2014-08-17-to-2014-08-20-seguranca-all-resultado.Rda')
load(arquivo)
analiseGeralDosTemas(resultado, palavras, tweets, arquivo)
analiseDosTemas(resultado, arquivo)

# Tweets sobre corrupção da última semana
tweets = recuperarTweetsEmArquivos(arquivos, 'corrupcao|corrupção|corrupçao|corrupcão')
arquivo = 'data/politics-tweets-2014-08-17-to-2014-08-20-corrupcao-all.Rda'
save(tweets, file=arquivo)
palavras = c('corrupcao','corrupção','corrupçao','corrupcão')
analiseTweetsPorTema(arquivo, palavras)
load('data/politics-tweets-2014-08-17-to-2014-08-20-corrupcao-all-resultado.Rda')
load(arquivo)
analiseGeralDosTemas(resultado, palavras, tweets, arquivo)
analiseDosTemas(resultado, arquivo)

# Tweets sobre a folha de SP da última semana
tweets = recuperarTweetsEmArquivos(arquivos, 'folha')
arquivo = '../../data/politics-tweets-2014-08-17-to-2014-08-20-folha-all.Rda'
save(tweets, file=arquivo)
palavras = c('folha')
analiseTweetsPorTema(arquivo, palavras)
load('../../data/politics-tweets-2014-08-17-to-2014-08-20-folha-all-resultado.Rda')
load(arquivo)
analiseGeralDosTemas(resultado, palavras, tweets, arquivo)
analiseDosTemas(resultado, arquivo)

# Tweets sobre o senado da última semana
tweets = recuperarTweetsEmArquivos(arquivos, 'senado')
arquivo = '../../data/politics-tweets-2014-08-07-to-2014-08-20-senado-all.Rda'
save(tweets, file=arquivo)
palavras = c('senado')
analiseTweetsPorTema(arquivo, palavras)
load('../../data/politics-tweets-2014-08-07-to-2014-08-20-senado-all-resultado.Rda')
load(arquivo)
analiseGeralDosTemas(resultado, palavras, tweets, arquivo)
analiseDosTemas(resultado, arquivo)

source('funcoesTwitter.R', encoding='UTF-8')
source('funcoesSNA.R', encoding='UTF-8')
arquivos = list.files('../../data/', pattern='liberdade-seguranca-tweets-2014-09-[[:digit:]]*-all.Rda', full.names=TRUE)
## Analisar apenas os últimos X dias
# arquivos
# arquivos = arquivos[(length(arquivos)-13):length(arquivos)]

# Tweets sobre liberdade civil
palavras = c("liberdade.*civi", "liberdade.*individua", "aborto", "estatuto.*nascituro", "descriminaliza.*maconha", "guerra.*drogas", 
           "liberdade.*religiosa", "estado.*laico", "ordem.*p(ú|u)blica", "div(ó|o)rcio", "planejamento.*familiar")
filtro = paste0(palavras, collapse='|')
tweets = recuperarTweetsEmArquivos(arquivos, filtro)
arquivo = '../../data/liberdade-tweets-2014-09-06-to-2014-09-08-all.Rda'
save(tweets, file=arquivo)
analiseTweetsPorTema(arquivo, palavras)
load('../../data/liberdade-tweets-2014-09-06-to-2014-09-08-all-resultado.Rda')
load(arquivo)
analiseGeralDosTemas(resultado, palavras, tweets, arquivo)
analiseDosTemas(resultado, arquivo)


# Tweets sobre segurança pública
palavras = c("seguran.*p(ú|u)blica", "valoriza.*pol(í|i)cia", "processua.*penal", "guerra.*drogas", 
             "desmilitariza.*pol(í|i)cia", "maioridade.*penal", "penitenci(á|a)ria", "combate.*organiza.*criminosa", 
             "sistema.*prisiona", "setor.*prisiona", "PEC.*300", "unifica.*pol(í|i)cia", "popula.*carcer(á|a)ria", 
             "pena.*alternativa", "tr(á|a)fico.*droga", "crime.*organizado")
filtro = paste0(palavras, collapse='|')
tweets = recuperarTweetsEmArquivos(arquivos, filtro)
arquivo = '../../data/seguranca-tweets-2014-09-06-to-2014-09-08-all.Rda'
save(tweets, file=arquivo)
analiseTweetsPorTema(arquivo, palavras)
load('../../data/seguranca-tweets-2014-09-06-to-2014-09-08-all-resultado.Rda')
load(arquivo)
analiseGeralDosTemas(resultado, palavras, tweets, arquivo)
analiseDosTemas(resultado, arquivo)

source('salvaLinkEleicoes.R', encoding='UTF-8')
dia = as.Date('2014-08-21')
for (i in 1:1) {
  dia = dia + 1
  joinTweets(dia, TRUE)
  arquivo = paste0("../../data/politics-tweets-", dia, "-all.Rda")
  #   salvarTweetsEleicoes(arquivo)
  salvaLinkEleicoesRda(arquivo)
}

source('salvaLinkEleicoes.R', encoding='UTF-8')
dia = as.Date('2014-07-01')
for (i in 1:30) {
  arquivo = paste0("../../data/politics-tweets-", dia, "-all.Rda")
  salvaLinkEleicoesRda(arquivo)
  dia = dia + 1
}

dia = as.Date('2014-07-04')
for (i in 1:12) {
  arquivo = paste0("../../data/politics-tweets-", dia, "-all.Rda")
  analisePorDia(arquivo)
  dia = dia + 1
}

resultado = analisePorDia('../../data/politics-tweets-2014-07-02-all.Rda')

resultado$temas
resultado$tweets[[2]]$text[1:10]
resultado$imagem



