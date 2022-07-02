# Descargamos la lista de palabras de la RAE
#
tmppath <- tempdir()
tmpfile <- file.path(tmppath,"CREA_total.zip")
url <- "http://corpus.rae.es/frec/CREA_total.zip"
download.file(url, tmpfile, method = "libcurl")
unzip(tmpfile, exdir = tmppath)

RAE_words <- read.table(file= file.path(tmppath,"CREA_total.TXT"), 
                        sep = "\t",
                        quote = "",
                        stringsAsFactors = FALSE,
                        nrows = -1,
                        skip = 1,
                        dec = '.',
                        strip.white = TRUE,
                        fileEncoding = "Latin1",
                        col.names =c("X", "token", "Freq.A", "Freq.N")
)


# Nos quedamos solo con las palabras de 5 letras
library(dplyr)

RAE_words %>% 
  filter(nchar(token) == 5) %>% 
  select(word = token, Freq.N) -> wordle
  
# Usamos expresiones regulares para ir buscando
wordle %>% 
  filter(grepl('^.{5}$', word))
  arrange(-Freq.N)
