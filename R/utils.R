# define "tolower error handling" function 
tryTolower = function(x) {
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}

## Retirado de http://stackoverflow.com/questions/15155814/check-if-r-package-is-installed-then-load-library
carregarPacote <- function(x) {
  if (!require(x,character.only = TRUE)) {
    install.packages(x,dependencies=TRUE, repos='http://star-www.st-andrews.ac.uk/cran/')
    if(!require(x,character.only = TRUE)) stop("Pacote não encontrado!")
  }
}

zeroPad <- function(str, len.out, num.zeros = len.out[1] - nchar(str)){ 
  paste0(paste(rep("0", num.zeros), collapse = ""), str) 
} 

#' Testa se a conexão odbc ainda está aberta.
#'
#' Função responsável por testar se a conexão odbc ainda está aberta.
#'
#' @param con a conexão odbc a ser testada.
isConnectionOpen <- function(con) {
  tryCatch({odbcGetInfo(conn);TRUE},error=function(...)FALSE)
}

removerAcento = function(textos) {
  # gsub("\\`|\\'|\\~|\\^|\"", "", iconv(textos, to="ASCII//TRANSLIT"))
  gsub("`|\\'|~|^\"", "", iconv(textos, to="ASCII//TRANSLIT"))
}

lerArquivo <- function(arquivo) {
  return(readChar(arquivo, file.info(arquivo)$size))
}

executarSql = function(query, dsn, uid, pwd, time = 5) {
  require(RODBC)
  resultado = tryCatch({
    options(stringsAsFactors = FALSE)
    if (exists('ch')) {
      if (!isConnectionOpen(ch)) {
        ## Abrir conexao com o banco
        ch = odbcConnect(dsn = dsn, uid=uid, pwd=pwd)
      }
    } else {
      ## Abrir conexao com o banco
      ch = odbcConnect(dsn = dsn, uid=uid, pwd=pwd)
    }
    
    ##buscar dados
    resultado = sqlQuery(ch, query)
    
    ## Fechar conexao com banco
    close(ch)
    
    return(resultado)
  }, error = function(e) {
    print(paste0("Erro ao executar query: ", query, "\n\t", e))
    ## Tentar executar a consulta novamente depois de time segundos.
    Sys.sleep(time)
    ## Antes de executar a consulta novamente, dobrar o tempo de espera (time*2)
    time = time*2
    resultado = executarSql(query, dsn, uid, pwd, time)
    return(resultado)
  })
  
  resultado
}

resave <- function(..., list = character(), file) {
  previous  <- load(file)
  var.names <- c(list, as.character(substitute(list(...)))[-1L])
  for (var in var.names) assign(var, get(var, envir = parent.frame()))
  save(list = unique(c(previous, var.names)), file = file)
}