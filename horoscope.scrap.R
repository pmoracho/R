get_today_horoscope_from_clarin <- function() {
  
  require("tidyverse")
  require("rvest")
  
  url <- "https://www.clarin.com/horoscopo"
  
  read_html(url) %>% 
    html_node('body') %>% 
    xml_find_all("//div[contains(@class, 'description')]") -> items
  
  items %>% 
    html_text() %>% 
    str_remove_all("\nHoróscopo de hoy: |\n") %>% 
    as.vector() -> textos
  
  items %>% 
    xml_attr("id") -> ids
  
  cbind.data.frame(id=ids, date=Sys.Date(), text=textos, stringsAsFactors=FALSE)
}