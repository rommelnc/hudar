library(xlsx)
dados = read.xlsx(file='../../data/Datasus/consolidado.xlsx', sheetIndex=1, stringsAsFactors = FALSE)

dados = read.table(file = '../../data/Datasus/consolidado.csv', fileEncoding = 'ISO-8859-1', header = TRUE, sep = ';', stringsAsFactors = FALSE)
dados = read.csv(file = '../../data/Datasus/consolidado-utf8.csv', header = TRUE, sep = ';', stringsAsFactors = FALSE)
dados = read.csv(file = '../../data/Datasus/consolidado.csv', header = TRUE, sep = ';', stringsAsFactors = FALSE)

codigo = substr(dados$Município, 1, 6)
dados$Código = codigo
str(dados)
head(codigo)
ufs = read.csv(file = '../../data/Datasus/Tabela IBGEUF.csv', header = TRUE, sep = ';', stringsAsFactors = FALSE)
ufs = read.csv(file = '../../data/Datasus/Tabela IBGEUF-utf8.csv', header = TRUE, sep = ';', stringsAsFactors = FALSE)
names(ufs) = c('Código','Estado' ,'Município')
ufs$Município = tolower(ufs$Município)
ufs$Código = substr(as.character(ufs$Código), 1, 6)
str(ufs)
votos = read.csv(file = '../../data/Datasus/Votacao_candidato_municipio_e_zona.csv', header = TRUE, sep = ';', stringsAsFactors = FALSE)
votos = read.csv(file = '../../data/Datasus/Votacao_candidato_municipio_e_zona-utf8.csv', header = TRUE, sep = ';', stringsAsFactors = FALSE)
votos$Município = tolower(votos$Município)
str(votos)
ufs

votos_codigo = merge(ufs, votos, )
str(votos_codigo)

tudo = merge(votos_codigo, dados, by = c('Código'))
str(tudo)

save(tudo, file='../../data/Datasus/tudo_consolidado.Rda')

votacao = tudo[,-c(4,6,8,10,11,20,21,22,25,26,27,28,30)]
str(votacao)

vencedor = ifelse(votacao$Qt.Votos.Nominais.do.Aécio > 1.05*votacao$Qt.Votos.Nominais.da.Dilma, 'Aécio',
                  ifelse(votacao$Qt.Votos.Nominais.da.Dilma > 1.05*votacao$Qt.Votos.Nominais.do.Aécio, 'Dilma', 'Empate'))  
table(vencedor)

votacao$Vencedor = as.factor(vencedor)

str(votacao)

save(votacao, file='../../data/Datasus/votacao_vencedor.Rda')

classificacao = votacao[,-c(1,3,4,5,6)]
classificacao$Estado = as.factor(classificacao$Estado)
classificacao = votacao[,-c(1,2,3,4,5,6)]
str(classificacao)
classificacao$Estado = as.factor(classificacao$Estado)

install.packages('tree')
library(tree)
arvore = tree(formula = Vencedor ~ ., data = classificacao)
arvore = tree(formula = Vencedor ~ PIB_per_capita + Taxa_de_analfabetismo, data = classificacao)
?tree
summary(arvore)
plot(arvore)
text(arvore)

install.packages('rpart')
library(rpart)

fit <- rpart(Vencedor ~ .,
             method="class", data=classificacao)

fit <- rpart(Vencedor ~ PIB_per_capita + Taxa_de_analfabetismo,
             method="class", data=classificacao)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# plot tree 
plot(fit, uniform=TRUE, 
     main="Votação por Município")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

votacao$Estado = as.factor(votacao$Estado)
dilma = votacao[,-c(1,2,3,4)]
str(dilma)

lm = glm(Qt.Votos.Nominais.da.Dilma ~ ., data = dilma)
summary(lm)
plot(lm)

dilma.relevante = dilma[,c(1,2,9,11:15)]
str(dilma.relevante)
lm = glm(Qt.Votos.Nominais.da.Dilma ~ ., data = dilma.relevante)
summary(lm)

plot(dilma$Qt, dilma$Qt.Votos.Nominais.da.Dilma)
plot(dilma$PIB_per_capita, dilma$Qt.Votos.Nominais.da.Dilma)
plot(dilma$X._população_com_renda_._1.2_SM, dilma$Qt.Votos.Nominais.da.Dilma)

hist(dilma$X._população_com_renda_._1.2_SM)
lm

http://www.r-bloggers.com/general-regression-neural-network-with-r/
