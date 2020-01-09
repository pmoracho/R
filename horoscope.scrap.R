signos <- c("aries", "tauro", "geminis", "cancer", "leo", "virgo", "libra", "escorpio", "sagitario", "capricornio", "acuario", "piscis")

get_today_horoscope_from_clarin <- function() {
  
  require("tidyverse")
  require("rvest")
  
  url <- "https://www.clarin.com/horoscopo"
  
  data <- c("data-Aries", "data-Tauro", "data-Géminis", "data-Cáncer", "data-Leo", "data-Virgo", "data-Libra",
            "data-Escorpio", "data-Sagitario", "data-Capricornio", "data-Acuario", "data-Piscis")
  
  read_html(url) %>% 
    html_node('body') %>% 
    xml_find_all("//div[contains(@class, 'description')]") -> items
  
  items %>% 
    html_text() %>% 
    str_remove_all("\nHoróscopo de hoy: |\n") -> textos
  
  items %>% 
    xml_attr("id") -> ids

  ids <- signos[match(ids, data)]
  cbind.data.frame(source="clarin", id=ids, date=Sys.Date(), text=textos, stringsAsFactors=FALSE)
}

get_today_horoscope_from_telam <- function() {
  
  require("tidyverse")
  require("rvest")

  url <- "https://www.lanacion.com.ar/horoscopo"

  ids <- c()
  textos <- c()
  
  for (signo in signos) {
    
    paste0(url, "/", signo) %>% 
      read_html() %>% 
      html_node('body') %>% 
      xml_find_all("//article[contains(@class, 'hoy__descrip')]") %>% 
      html_text() %>% 
      str_remove_all("\r\n|[ \t]{2,}") -> texto
    
    ids[length(ids) + 1] <- signo
    textos[length(textos) + 1] <- texto
    
  }
  cbind.data.frame(source="lanacion", id=ids, date=Sys.Date(), text=textos, stringsAsFactors=FALSE)
  
}

get_today_horoscope_from_telam <- function() {
  
  require("tidyverse")
  require("rvest")
  
  data <- c("1-aries", "2-tauro", "3-gminis", "4-cncer", "5-leo", "6-virgo", "7-libra",
            "8-escorpio", "9-sagitario", "10-capricornio", "11-acuario", "12-piscis")
  
  url <- "http://www.telam.com.ar/horoscopo"
  
  ids <- c()
  textos <- c()
  
  for (d in data) {
    
    paste0(url, "/", d) %>% 
      read_html() %>% 
      html_node('body') %>% 
      xml_find_all("//div [contains(@class, 'three-col-block')]") %>% 
      xml_node("p") %>% 
      `[[`(2) %>% 
      html_text() %>% 
      str_remove_all("\n|[ \t]{2,}") -> texto
    
    ids[length(ids) + 1] <- d
    textos[length(textos) + 1] <- texto
    
  }
  ids <- signos[match(ids, data)]
  cbind.data.frame(source="telam", id=ids, date=Sys.Date(), text=textos, stringsAsFactors=FALSE)
  
}

get_today_horscope <- function() {
  
  rbind(get_today_horoscope_from_lanacion(),
        get_today_horoscope_from_clarin(),
        get_today_horoscope_from_telam()
  )
  
}

# get_today_horscope() %>%  View
# get_today_horoscope_from_clarin() %>%  View
# get_today_horoscope_from_telam() %>%  View