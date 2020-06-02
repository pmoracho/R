# 30díasdegráficos con R - día 22 - Nube de palabras
# Tema personalizado: devtools::install_github("pmoracho/ggelegant")
# Gráficos: Ggplo2 + rtweet + ggwordcloud + Algo de dplyr
# Font: Ralleway
# Para #30díasdegráficos y #rstatsES. Día 22: Mi primera nube de palabras, en este caso 
# accedo a mi twitter para ver sobre que escribo. Haho limpieza de stopwords y solo apunto a palabras dentro
# del corpus de 1000 palbaras de la RAE.# 
# Github: https://github.com/pmoracho/R/blob/master/30_diasdegraficosenr_dia22.R
# 
library("tidyverse")
library("rtweet")
library("tidytext")
library("ggwordcloud")

if ("ggelegant" %in% rownames(installed.packages())) {
  library("ggelegant")
} else {
  # devtools::install_github("pmoracho/ggelegant")
  theme_elegante_std <- function(base_family) {}
}

# Twitter API
create_token(
  app = "xx",
  consumer_key = "xx",
  consumer_secret = "xx",
  access_token = "xx",
  access_secret = "xx"
)


RAE_Corpus_1000 <- read.table(file="http://corpus.rae.es/frec/1000_formas.TXT", skip=1, header=FALSE,
                              fileEncoding = "Latin1", 
                              col.names = c("nr", "word", "Frec.absoluta", "Frec.normalizada"),
                              stringsAsFactors = FALSE)

stopwords <- read.csv("https://countwordsfree.com/stopwords/spanish/txt", 
                      col.names="word", header=FALSE, stringsAsFactors = FALSE)

twits <- get_timeline("pmoracho", n = 5000)
twits %>%
  unnest_tokens(word, text) %>% 
  select(word) %>% 
  anti_join(stopwords, by = "word") %>%
  inner_join(RAE_Corpus_1000, by = c("word"="word")) -> my_words
  
my_words %>% 
  count(word, sort = TRUE)  -> my_words_count

my_words %>%
  inner_join(my_words_count %>% head(100), by = "word") %>% 
  # filter(!(word %in% c('https', 'http'))) %>% 
  count(word) %>% 
  ggplot(aes(label = word, size=n, color=n)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 20) +
  theme_elegante_std(base_family = "Ralleway") +
  labs(title = paste("¿De qué habla @pmoracho?"), 
       subtitle = paste0("Nube de palabras de las 100 palabras más usadas de la cuenta de twitter"), 
       caption = "Fuente:@pmoracho",
       y = "",
       x = "")
