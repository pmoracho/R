structure(list(word = c("cali", "con", "prohibición", "y", "todo", 
                        "con", "comunicado", "y", "todo", "en"), 
               upos = c("NOUN", "ADP","NOUN", "CCONJ", "PRON", "ADP", "VERB", "CCONJ", "PRON", "ADP"), 
               feats = c("Gender=Fem|Number=Sing", NA, "Gender=Fem|Number=Sing", NA, "Gender=Masc|Number=Sing|PronType=Tot",
                         NA, "Gender=Masc|Number=Sing|VerbForm=Part",NA, "Gender=Masc|Number=Sing|PronType=Tot", NA)), 
          row.names = c(NA, -10L), class = c("tbl_df", "tbl", "data.frame")) -> corpus_anotado

corpus_anotado


keywords <- c("Gender", "Number", "PronType", "VerbForm")
trows <- nrow(corpus_anotado)
m <- matrix(data=NA_character_, nrow=trows, ncol=length(keywords))
colnames(m) <- keywords

for (i in 1:trow) {
  
  f <- corpus_anotado[i, "feats"]
  if (!is.na(f)) {
    KV <-strsplit(strsplit(f, '\\|')[[1]], '=')
    for (e in KV) {
      m[i, e[1]] <- e[2]  
    } 
  }
} 

library("tidyverse")
corpus_anotado %>% 
  bind_cols(as.data.frame(m))
