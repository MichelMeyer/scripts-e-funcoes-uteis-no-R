feriados <- function(anos) {
  anos <- as.numeric(anos)
  if( ! is.numeric(anos) | any(is.na(anos))) stop("A variável anos deve estar em formato numérico")
  
  anos <- sort(anos)
  
  if( ! "rvest" %in% installed.packages()) install.packages("rvest")
  require(rvest)
  
  feriados <- lapply(anos, function(ano) {
    feriados <- read_html(paste0("https://www.anbima.com.br/feriados/fer_nacionais/", ano, ".asp"))
    feriados <- html_table(feriados, fill = T)
    feriados <- feriados[[4]]
    colnames(feriados) <- feriados[1, ]
    feriados <- feriados[-1, ]
    return(feriados)
  })
  
  feriados <- do.call(rbind, feriados)
  
  feriados$Data <- as.Date(feriados$Data, "%d/%m/%y")
  
  return(feriados)
}
