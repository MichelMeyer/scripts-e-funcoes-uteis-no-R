pendrive <- "H:/"
pasta <- "PENDRIVE"
x <- dir(path = pendrive, all.files = T, recursive = T, full.names = T)
if( ! dir.exists(pasta))
  dir.create(pasta)
for(i in 1 : length(x)) {
  y <- x[i]
  if(substr(y, 1, 4) == "H://") {
    y <- gsub("H://", paste0(pasta, "/"), y)
    caminho <- strsplit(y, "/")[[1]]
    caminho <- caminho[ - length(caminho)]
    dir.create(paste(caminho, collapse = "/"), showWarnings = F, recursive = T)
    file.copy(from = x[i], to = paste(caminho, collapse = "/"), copy.date = T)
  } else {
    file.copy(from = x[i], to = paste(pasta))
  }
}
###########################################################################################
## Muita atenção, verifique os arquivos na pasta que foram copiados antes de prosseguir. ##
###########################################################################################
unlink(pendrive, force = T, recursive = T)
file.copy(dir(pasta, all.files = T, full.names = T), pendrive, recursive = T, copy.date = T)
unlink(pasta, force = T, recursive = T)