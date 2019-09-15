
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
      colnames(table) <- c("Data", "Cotação", "Mínimo", "Máximo", "Variação", "Variação %", "Volume")
      table <- table[, c("Data", "Cotação", "Variação %", "Variação", "Mínimo", "Máximo", "Volume")]
    }
    return(table)
  })
  warning(paste("Ativo(s) sem cotação(ões):",
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
                                     coluna = "Cotação", completar = "último", extremidades = "") {
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
      if(completar == "último") {
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
# Percebam que existe uma mensagem de erro neste exemplo. Mesmo com ela, o objeto tabelas é criado.
# O formato de saída é uma lista onde cada elemento da lista é um data.frame.
# tabelas <- cotacoes.uol("PETR3") também functionaria!!!
# Vejam que o nome de cada elemento da lista é o nome do ativo selecionado, da forma que foi escrito.
# Para acessar os dados:
View(tabelas$PETR3)
View(tabelas$VALE3)

# Temos duas formas de salvar os arquivos:
# salvar.xlsx, que salva cada ativo como uma sheet dentro de um mesmo documento excel.
salvar.xlsx(tabelas, arquivo = "cotações.exemplo.xlsx", linhas.em.branco = 2, colunas.em.branco = 0)
# Como na reunião eu vi foi dito que deveriamos fazer as tabelas com 2 linhas anteriores eu deixei a opção aqui ^^.
# As mensagens de aviso não interferem no arquivo, podem verificar.
# Lembrando que o arquivo está na pasta:
getwd()

# salvar.tabela.unica.xlsx, que salva em uma única tabela todos os ativos.
# mas só com a coluna selecionada. Aqui a coluna padrão é a Cotação!
salvar.tabela.unica.xlsx(tabelas, arquivo = "tabela.exemplo.xlsx", coluna = "Cotação",
                         completar = "último", extremidades = "")
# O argumento completar se refere aos buracos no meio da série de preços, podendo ter como argumento
# também NA e "". Contudo é bom ressaltar que esta série da UOL usa o mesmo principio do argumento
# completar = "último", contudo mantém alguns gaps.
# extremidades: São os períodos antes da primeira negociação e a após a última.
# Qualquer valor é aceito aqui, usei "" para completar por ser mais cômodo e ocupar menos espaço no arquivo.


### Pronto para usar ###
ativos <- c("ativo1", "ativo2", "...")

tabelas <- cotacoes.uol(ativos)
salvar.xlsx(tabelas, arquivo = "CotaçõesDetalhado.xlsx", linhas.em.branco = 2)
salvar.tabela.unica.xlsx(tabelas, arquivo = "CotaçõesTodos.xlsx")







