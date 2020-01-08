#' Retorna un texto escrito con un font ASCII
#' Source: https://stackoverflow.com/a/48080240/6836377
#' @param msg   texto
#' @param font  font as usar
#'
#' @return character
#' @export
#'
#' @examples: cat(ascii_text("hola"))
#' 
ascii_text <- function(msg="sample text", font="c_ascii_") {
  
  require("httr")
  
  url <- paste0("http://artii.herokuapp.com/make?text=", msg, "&font=", font)
  ascii_request <- httr::GET(url)
  httr::content(ascii_request,as = "text", encoding = "UTF-8")
  
}


#' Retorna un vector con los fonts disponibles
#'
#' @return character vector
#' @export
#'
#' @examples: ascii_text_fonts()
#' 
ascii_text_fonts <- function() {
  url <- "http://artii.herokuapp.com/fonts_list"
  ascii_request <- httr::GET(url)
  txt <- httr::content(ascii_request,as = "text", encoding = "UTF-8")
  strsplit(txt, "\n")[[1]]
}


#' Retorna un ejemplo con un texto en todos los fonts disponibles
#'
#' @param msg texto
#'
#' @return
#' @export
#'
#' @examples: cat(sample_ascii_text("Prueba"))
sample_ascii_text <- function(msg) {
  
  cat("Generando prueba de letras: ")
  txt <- ""
  for (font in ascii_text_fonts()) {
    cat(paste0(font, ","))
    txt <- paste0(txt, "------------------> Tipo de letra:", font, " <--------\n\n", ascii_text("prueba", font), '\n\n')
  }
  cat("..Fin..\n")
  txt
}



