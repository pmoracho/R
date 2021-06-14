library(shiny)
library(shinythemes)

ui <- navbarPage(title = "Explorador de Emojis",
                 theme = shinytheme("darkly") ,
                 tabPanel("Por emoji", selectInput(inputId = "selector_emoji", 
                                                   label = "Emoji",
                                                   multiple = TRUE,
                                                   choices = letters)),
                 tabPanel("Por País",  selectInput(inputId = "selector_pais", 
                                                   label = "Pais",
                                                   multiple = TRUE,
                                                   choices = letters)),
                 tabPanel("Licencia",  tags$a(href="https://github.com/flor14/latinr_shiny_2020/blob/master/LICENSE.md", "Archivo con licencia MIT"))
                 
  
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)