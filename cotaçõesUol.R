
cotacoes.uol <- function(ativos,
                         inicio = "01/01/2007", fim = "31/12/2016") {
  if( ! "XML" %in% rownames(installed.packages())) install.packages("XML")
  library("XML")
  inicio = strsplit(inicio, "/")[[1]]
  fim = strsplit(fim, "/")[[1]]
  tables <- lapply(ativos, function(share) {
    if( ! grepl(".SA?", share))
      share = paste0(share, ".SA")
    link = 
      paste0("http://cotacoes.economia.uol.com.br/acao/cotacoes-historicas.html?codigo=", share, "&size=99999",
             "&beginDay=", as.numeric(inicio[[1]]), "&beginMonth=", as.numeric(inicio[[2]]), "&beginYear=", as.numeric(inicio[[3]]),
             "&endDay=", as.numeric(fim[[1]]), "&endMonth=", as.numeric(fim[[2]]), "&endYear=", as.numeric(fim[[3]]))
    table <- readHTMLTable(htmlParse(link))$tblInterday
    if(!is.null(table)) {
      table <- data.frame(as.matrix(table[sort(as.numeric(rownames(table)), decreasing = T), ]), stringsAsFactors = F)
      colnames(table) <- c("Data", "Cota��o", "M�nimo", "M�ximo", "Varia��o", "Varia��o %", "Volume")
      table <- table[, c("Data", "Cota��o", "Varia��o %", "Varia��o", "M�nimo", "M�ximo", "Volume")]
    }
    return(table)
  })
  warning(paste("Ativo(s) sem cota��o(�es):",
                paste(ativos[unlist(lapply(tables, function(table)
                  is.null(table)))], collapse = ", ")))
  names(tables) <- ativos
  tables <- tables[ ! unlist(lapply(tables, function(table) is.null(table)))]
  return(tables)
}

salvar.xlsx <- function(tabelas, arquivo,
                        linhas.em.branco = 0, colunas.em.branco = 0) {
  if( ! "xlsx" %in% rownames(installed.packages())) install.packages("xlsx")
  library(xlsx)
  if(file.exists(arquivo)) unlink(arquivo)
  for(i in names(tabelas)) {
    tabela <- tabelas[[i]]
    tabela <- rbind(matrix("", nrow = linhas.em.branco, ncol = ncol(tabela)), colnames(tabela), as.matrix(tabela))
    tabela <- cbind(matrix("", ncol = colunas.em.branco, nrow = nrow(tabela)), rownames(tabela), tabela)
    try(write.xlsx2(tabela, file = arquivo, col.names = F, row.names = F, sheetName = i, append = T))
  }
}

salvar.tabela.unica.xlsx <- function(tabelas, arquivo,
                                     coluna = "Cota��o", completar = "�ltimo", extremidades = "") {
  if( ! "xlsx" %in% rownames(installed.packages())) install.packages("xlsx")
  library(xlsx)
  tables <- lapply(tabelas, function(tabela) tabela[, c("Data", coluna)])
  datas <- unique(unlist(lapply(tables, function(table) table[, 1])))
  datas <- datas[order(as.Date(datas, "%d/%m/%Y"))]
  tabela <- NULL
  for(i in names(tables)) {
    base <- data.frame(matrix(extremidades, nrow = length(datas)), row.names = datas, stringsAsFactors = F)
    colnames(base) <- i
    base[datas %in% tables[[i]][, 1], 1] <- tables[[i]][, 2]
    
    x <- base[, 1] != extremidades
    y <- which( ! x)
    x <- c(min(which(x)), max(which(x)))
    y <- c(y[y > x[[1]] & y < x[[2]]])
    
    if(length(y) != 0)
      if(completar == "�ltimo") {
        y.1 <- list(y[1])
        for(p in y[-1]) {
          if((p - 1) %in% unlist(y.1)) {
            y.1[[length(y.1)]] <- c(y.1[[length(y.1)]], p)
          } else {
            y.1 <- c(y.1, p)
          }
        }
        for(p in y.1) base[p, 1] <- base[p[[1]] - 1, 1]
      } else {
        base[y, ] <- completar
      }
    
    if(is.null(tabela)) {
      tabela <- base
    } else {
      tabela <- cbind(tabela, base)
    }
  }
  
  write.xlsx2(tabela, arquivo)
}



### EXEMPLOS ###

tabelas <- cotacoes.uol(c("SHARE3", "PETR3", "VALE3", "ABEV3", "AMBV3", "WEGE3"),
                        inicio = "01/01/2007", fim = "31/12/2016")
# Percebam que existe uma mensagem de erro neste exemplo. Mesmo com ela, o objeto tabelas � criado.
# O formato de sa�da � uma lista onde cada elemento da lista � um data.frame.
# tabelas <- cotacoes.uol("PETR3") tamb�m functionaria!!!
# Vejam que o nome de cada elemento da lista � o nome do ativo selecionado, da forma que foi escrito.
# Para acessar os dados:
View(tabelas$PETR3)
View(tabelas$VALE3)

# Temos duas formas de salvar os arquivos:
# salvar.xlsx, que salva cada ativo como uma sheet dentro de um mesmo documento excel.
salvar.xlsx(tabelas, arquivo = "cota��es.exemplo.xlsx", linhas.em.branco = 2, colunas.em.branco = 0)
# Como na reuni�o eu vi foi dito que deveriamos fazer as tabelas com 2 linhas anteriores eu deixei a op��o aqui ^^.
# As mensagens de aviso n�o interferem no arquivo, podem verificar.
# Lembrando que o arquivo est� na pasta:
getwd()

# salvar.tabela.unica.xlsx, que salva em uma �nica tabela todos os ativos.
# mas s� com a coluna selecionada. Aqui a coluna padr�o � a Cota��o!
salvar.tabela.unica.xlsx(tabelas, arquivo = "tabela.exemplo.xlsx", coluna = "Cota��o",
                         completar = "�ltimo", extremidades = "")
# O argumento completar se refere aos buracos no meio da s�rie de pre�os, podendo ter como argumento
# tamb�m NA e "". Contudo � bom ressaltar que esta s�rie da UOL usa o mesmo principio do argumento
# completar = "�ltimo", contudo mant�m alguns gaps.
# extremidades: S�o os per�odos antes da primeira negocia��o e a ap�s a �ltima.
# Qualquer valor � aceito aqui, usei "" para completar por ser mais c�modo e ocupar menos espa�o no arquivo.


### Pronto para usar ###
ativos <- c("ativo1", "ativo2", "...")

tabelas <- cotacoes.uol(ativos)
salvar.xlsx(tabelas, arquivo = "Cota��esDetalhado.xlsx", linhas.em.branco = 2)
salvar.tabela.unica.xlsx(tabelas, arquivo = "Cota��esTodos.xlsx")







