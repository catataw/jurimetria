crawler_cjsg <- function(pag=0, livre='', ementa='', n_recurso='', n_registro='', relator='', 
                         classe='', assunto='', comarca='', orgao_julgador='', dt_julgamento='', 
                         dt_registro='', tipo_decisao='', ordenar='') { 
  form_data <- list('dados.buscaInteiroTeor'='',
                    'dados.sinonimos'='on',
                    'dados.buscaEmenta'='',
                    'dados.nuProcOrigem'='',
                    'dados.nuRegistro'='',
                    'agenteSelectedEntitiesList'='',
                    'contadoragente'='0',
                    'contadorMaioragente'='0',
                    'agentePK.cdAgente'='',
                    'nmAgente'='',
                    'classesTreeSelection.values'='',
                    'classesTreeSelection.text'='',
                    'assuntosTreeSelection.values'='',
                    'assuntosTreeSelection.text'='',
                    'comarcaSelectedEntitiesList'='',
                    'contadorcomarca'='',
                    'contadorMaiorcomarca'='1',
                    'comarcaPK.cdComarca'=comarca,
                    'dados.comarcas[0].comarcaPK.cdComarca'='583',
                    'nmComarca'='',
                    'secoesTreeSelection.values'='',
                    'secoesTreeSelection.text'='',
                    'dados.dtJulgamentoInicio'='',
                    'dados.dtJulgamentoFim'='',
                    'dados.dtRegistroInicio'='',
                    'dados.dtRegistroFim'='',
                    'tipoDecisaoSelecionados'='A',
                    'dados.ordenacao'='data')
  url <- 'https://esaj.tjsp.jus.br/cjsg/resultadoCompleta.do'
  r <- POST(url, body=form_data, config=list(ssl.verifypeer=F))
  cat(content(r, 'text'), file='/home/julio/mestrado/projeto_mestrado/testes/teste.html')
  
  url_pag <- sprintf('https://esaj.tjsp.jus.br/cjsg/trocaDePagina.do?tipoDeDecisao=A&pagina=%d', pag)  
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

# other functions
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
  df$txt <-  gsub('[\r\t]', '', xmlValue(children[[length(children)]]))
  df
}

# dados <- crawler_cjsg(pag=2, comarca='583')


