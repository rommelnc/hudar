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
  # ver http://stackoverflow.com/questions/20495598/replace-accented-characters-in-r-with-non-accented-counterpart-utf-8-encoding
  unwanted_array = list(    'S'='S', 's'='s', 'Z'='Z', 'z'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                            'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                            'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                            'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                            'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b')
  
  
  require(gsubfn)
  resultado = gsubfn(paste(names(unwanted_array),collapse='|'), unwanted_array, textos)
  resultado
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