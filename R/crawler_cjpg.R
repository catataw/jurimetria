#' Retorna um data.frame com todos os processos retornados pela query.
#' 
#' Cuidado! Os parametros que a funcao recebe (livre, classes, etc) precisam ser exatamente iguais as
#' informacoes que o TJSP precisa para retornar as informacoes.
#' 
#' @export
#' 
#' livre='', classes='', assuntos='', magistrados='', datas=c('',''), varas=''
crawler_cjpg <- function(pag=0, r=NULL, ementa=TRUE, opts=NULL) {
  if(is.null(r)) {
    query <- list(
      dadosConsulta.pesquisaLivre = opts[['livre']],
      classeTreeSelection.values = opts[['classes']],
      assuntoTreeSelection.values = opts[['assuntos']],
      dadosConsulta.dtInicio = opts[['datas']][1],
      dadosConsulta.dtFim = opts[['datas']][2],
      varasTreeSelection.values = opts[['varas']],
      tipoNumero = 'UNIFICADO',
      numeroDigitoAnoUnificado = '',
      foroNumeroUnificado = '',
      dadosConsulta.nuProcesso = '',
      dadosConsulta.nuProcessoAntigo = '',
      classeTreeSelection.text = '',
      assuntoTreeSelection.text = '',
      agenteSelectedEntitiesList = '',
      contadoragente = '0',
      contadorMaioragente = '0',
      cdAgente = opts[['magistrados']],
      nmAgente = '',
      varasTreeSelection.text = '',
      dadosConsulta.ordenacao = 'DESC'
    )
    urlb <- list(scheme='https', hostname='esaj.tjsp.jus.br', path='cjpg/pesquisar.do', query=query)
    class(urlb) <- 'url'
    url <- build_url(urlb)
    url <- gsub('NULL', '', url)
    r <- GET(url, config=list(ssl.verifypeer=F))
  }
  
  url_pag <- sprintf('https://esaj.tjsp.jus.br/cjpg/trocarDePagina.do?pagina=%d',pag)
  # REQUESTS
  cat(sprintf('pagina: %d...', pag))

  cat('fazendo download...')
  r_pag <- GET(url_pag, config=c(ssl.verifypeer=F, set_cookies(unlist(r$cookies))))
  cat('download realizado! ')
  cat('inicializando parser...')
  try ({
    html <- htmlParse(content(r_pag, 'text'), encoding='UTF-8')
    nodes <- getNodeSet(html, "//tr[@class='fundocinza1']//table")  
    cat('rodando parser...')
    df <- ldply(nodes, parse_node)
    df$pag <- pag
    cat('OK!\n')
    return(df)  
  },TRUE)
  cat('BUGOU!!!!!\n')
  return(data.frame())
}

# parsing functions
parse_node_meta <- function(node) {
  val <- str_trim(str_split_fixed(gsub('[\n\r\t]','', xmlValue(node)), ':', 2))
  df <- data.frame(val[1,2], stringsAsFactors=F)
  names(df) <- val[1,1]
  df
}
parse_node <- function(node) {
  children <- xmlChildren(node)
  df <- do.call(cbind, lapply(children[2:(length(children)-1)], parse_node_meta))
  df$n_processo <- gsub('[\n\r\t ]', '', xmlValue(xmlChildren(xmlChildren(children[[1]])$td)$a))
  df$cod_sentenca <- xmlGetAttr(xmlChildren(xmlChildren(children[[1]])$td)$a,'name')
  df$txt <-  gsub('[\r\t]', '',xmlValue(xmlChildren(xmlChildren(children[[length(children)]])$td)[[4]]))
  df
}


#' Retorna um data.frame com todos os processos retornados pela query.
#' 
#' Cuidado! Os parametros que a funcao recebe (livre, classes, etc) precisam ser exatamente iguais as
#' informacoes que o TJSP precisa para retornar as informacoes.
#' 
#' @export
#' 
#' livre='', classes='', assuntos='', magistrados='', datas=c('',''), varas=''
crawler_cjpg2 <- function(opts=NULL, min_pag=1, max_pag=10, ementa=TRUE, salvar=TRUE) {
  query <- list(
    dadosConsulta.pesquisaLivre = opts[['livre']],
    classeTreeSelection.values = opts[['classes']],
    assuntoTreeSelection.values = opts[['assuntos']],
    dadosConsulta.dtInicio = opts[['datas']][1],
    dadosConsulta.dtFim = opts[['datas']][2],
    varasTreeSelection.values = opts[['varas']],
    tipoNumero = 'UNIFICADO',
    numeroDigitoAnoUnificado = '',
    foroNumeroUnificado = '',
    dadosConsulta.nuProcesso = '',
    dadosConsulta.nuProcessoAntigo = '',
    classeTreeSelection.text = '',
    assuntoTreeSelection.text = '',
    agenteSelectedEntitiesList = '',
    contadoragente = '0',
    contadorMaioragente = '0',
    cdAgente = opts[['magistrados']],
    nmAgente = '',
    varasTreeSelection.text = '',
    dadosConsulta.ordenacao = 'DESC'
  )
  urlb <- list(scheme='https', hostname='esaj.tjsp.jus.br', path='cjpg/pesquisar.do', query=query)
  class(urlb) <- 'url'
  url <- build_url(urlb)
  url <- gsub('NULL', '', url)
  r <- GET(url, config=list(ssl.verifypeer=F))
  if(max_pag == 'n_max') {
    h <- htmlParse(content(r, 'text'), encoding='UTF-8')
    val <- xmlValue(getNodeSet(h, "//*[@id='resultados']//td")[[1]])
    num <- as.numeric(str_match(str_trim(val), 'de ([0-9]+)')[1,2])
    max_pag <- ceiling(num/10)
    print(max_pag)
  }
  d <- ldply((min_pag):(max_pag), crawler_cjpg_pag, r=r)
  return(d)
}

crawler_cjpg_pag <- function(pag, r, salvar) {
  url_pag <- sprintf('https://esaj.tjsp.jus.br/cjpg/trocarDePagina.do?pagina=%d', pag)
  # REQUESTS
  cat(sprintf('pagina: %d...', pag))
  cat('fazendo download...')
  r_pag <- GET(url_pag, config=c(ssl.verifypeer=F, set_cookies(unlist(r$cookies))))
  cat('download realizado! ')
  cat('inicializando parser...')
  try ({
    html <- htmlParse(content(r_pag, 'text'), encoding='UTF-8')
    nodes <- getNodeSet(html, "//tr[@class='fundocinza1']//table")  
    cat('rodando parser...')
    df <- ldply(nodes, parse_node)
    df$pag <- pag
    cat('OK!\n')
    saveRDS(df, sprintf('dados_dm3/df_pag_%d.rds', pag))
    return(df)  
  },TRUE)
  cat('BUGOU!!!!!\n')
  return(data.frame())
}
