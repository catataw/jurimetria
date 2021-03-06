#' Baixa PDF do Justica Aberta e transforma em data.frame
#' 
#' Essa funcao so funciona no linux, e somente se vc instalar o pdfminer (sudo pip install pdfminer==20110515).
#' Essa funcao recebe o codigo da produtividade, o codigo da vara (que e necessario somente para completar
#' a request feita para o servidor do Justica Aberta), o tipo (se e vara:"vara" ou magistrado:"mag") e dois
#' diretorios (um para guardar o pdf, e outro para guardar o arquivo html gerado pelo pdf2txt). Essa funcao
#' e extremamente lenta (demora ~1s por produtividade, e geralmente queremos muitas produtividades).
#' 
#' @export
crawler_prod_ja <- function(cod_prod, cod_vara, tipo='vara', d_pdf=getwd(), d_html=d_pdf, pdf2txt=TRUE) {
  df <- mdply(cbind(cod_prod, cod_vara), crawler_prod_ja_one, tipo, d_pdf, d_html, pdf2txt)
  return(df)
}
rm_accent <- function(x) gsub("`|\\'", "", iconv(x, to = "ASCII//TRANSLIT"))
crawler_prod_ja_one <- function(cod_prod, cod_vara, tipo, d_pdf, d_html, pdf2txt) {
  url <- 'http://www.cnj.jus.br/corregedoria/justica_aberta/?s'
  pdf_file <- download_pdf_ja(url, cod_prod, cod_vara, tipo, d_pdf)
  if(pdf2txt) html_file <- pdf_to_html(pdf_file, d_html)
  dados <- pega_dados(html_file)
  dados$key2 <- gsub(' +', '_', str_trim(gsub('\\(|\\)|\n|\\/','', rm_accent(tolower(dados$key)))))
  metadados <- pega_metadados(html_file)
  dados_melt <- rbind.fill(list(dados, metadados))
  dados_melt$cod_prod <- cod_prod
  dados_melt$cod_vara <- cod_vara
  dados_melt$key2 <- gsub(' +', '_', str_trim(gsub('\\(|\\)|\n|\\/','', rm_accent(tolower(dados_melt$key)))))
  pr <- function(x) return(x[1])
  dados_cast <- dcast(dados_melt, cod_prod + cod_vara ~ key2, fun.aggregate=pr)
  for(x in dados$key2) dados_cast[[x]] <- as.numeric(dados_cast[[x]])
  return(dados_cast)
}
download_pdf_ja <- function(url, cod_prod, cod_vara, tipo, d_pdf) {
  if(tipo == 'vara') {
    post_data <- list(d='relatorios', a='relatorios', f='respostaProdutividadeServentiaPdf', 
                      SEQ_PRODUTIVIDADE_SERVENTIA=cod_prod, SEQ_SERVENTIA_JUDICIAL=cod_vara,
                      security_token='', url='http://www.cnj.jus.br/corregedoria/justica_aberta/')
  } else if(tipo == 'mag') {
    post_data <- list(d='relatorios', a='relatorios', f='respostaProdutividadeMagistradoPdf', 
                      SEQ_PRODUTIVIDADE_MAGISTRADO=cod_prod, SEQ_SERVENTIA_JUDICIAL=cod_vara,
                      security_token='')    
  } else {
    cat(paste0('Este tipo nao e valido: ',tipo,'\n'))
    return()
  }
  pdf_file <- paste0(d_pdf, cod_vara, '_', cod_prod, '.pdf')
  r <- POST(url, body=post_data)
  writeBin(content(r, 'raw'), pdf_file)
  return(pdf_file)
}
pdf_to_html <- function(pdf_file, d_html) {
  if(file.exists(pdf_file)) {
    html_file <- paste0(d_html, str_replace(str_extract(pdf_file, '[^/]*$'), 'pdf', 'html'))
    pdf2txt <- sprintf("pdf2txt -t html %s > %s", pdf_file, html_file)
    system(pdf2txt)
    return(html_file)
  } else {
    cat('nao achei o arquivo!\n')
    return()
  }
}
prod_nums <- function(elem) {
  style <- xmlGetAttr(elem,'style')
  left <- as.numeric(str_split_fixed(str_split_fixed(style,'left:',2)[2],'px;',2)[1])
  width <- as.numeric(str_split_fixed(str_split_fixed(style,'width:',2)[2],'px;',2)[1])
  if(left < 450 | width > 50 | left > 540) {
    return(NULL)
  }
  return(elem)
}
pega_dados <- function(ar) {
  search <- "//div[@style[contains(.,'position:absolute; border: textbox 1px solid; writing-mode:lr-tb;')]]"
  search2 <- "//span[@style[contains(.,'Bold;')]]"
  a <- readChar(ar,file.info(ar)$size)
  html <- htmlParse(a)
  nodes <- getNodeSet(html, search)
  nodes_prod <- unlist(lapply(nodes,prod_nums))
  prods_num <- unlist(str_split(paste0(sapply(nodes_prod, xmlValue),collapse=''),'\n'))
  prods_num <- as.numeric(prods_num[!prods_num %in% c('Total','')])
  if(length(prods_num) > 0) {
    nodes2 <- getNodeSet(html, search2)  
    prods_txt <- str_trim(unlist(str_split(paste0(sapply(nodes2, xmlValue),collapse=''),':')))
    prods_txt <- gsub('Total|QUESTIONÁRIO DE PRODUTIVIDADE DA SERVENTIA\n|QUESTIONÁRIO DE PRODUTIVIDADE DO MAGISTRADO\n?','',prods_txt)
    prods_txt <- prods_txt[prods_txt %in% sapply(prods_txt,toupper)]
    prods_txt <- prods_txt[!prods_txt %in% '']
    if(length(prods_txt) != length(prods_num)) {
      write(ar,paste0('/home2/Projeto_JA/others/bug/',str_sub(ar,str_locate(ar,'/[0-9]+')[1]+1)))
      return(data.frame())
    } else {
      df <- data.frame(arq=ar, key=prods_txt, value=prods_num, tipo='produtividade', stringsAsFactors=F)
      return(df)
    }  
  } else {
    return(data.frame())
  }
}
parse_metadados <- function(linha) {
  if(str_detect(linha,':')) {
    d<-str_trim(str_split_fixed(linha,':',2))
    return(c(metadado=d[1],dado=d[2]))
  } else {
    return(NULL)
  }
}
pega_metadados <- function(arq) { 
  search <- "//div[@style[contains(.,'position:absolute; border: textbox 1px solid; writing-mode:lr-tb;')]]"
  a <- readChar(arq,file.info(arq)$size)
  html <- htmlParse(a)
  nodes <- getNodeSet(html, search)
  node_meta <- nodes[[2]]
  dados <- unlist(str_split(xmlValue(nodes[[2]]), '\n'))
  df <- ldply(dados, parse_metadados)
  names(df) <- c('key','value')
  df$tipo <- 'metadado'
  df$arq <- arq
  return(df)
}
#___________________________________________________________________________________________________





