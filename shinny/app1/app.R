library(shiny)
library(DT)

sql_palabras_busqueda <- function(lang) {
    
    return(c('Palabra1','Palabra2'))
}

sql_metodo <- function(lang) {
    
    return(c('Metodo1','Metodo'))
}

ui <- shinyUI(
    tabPanel("Trend Analysis", value = "tab11",
             br(),
             sidebarLayout(
                 # Sidebar with a slider and selection inputs
                 sidebarPanel(
                     br(), br(), br(),
                     selectInput("clave_busqueda", "Completed searches:",
                                 choices = sql_palabras_busqueda("en"), selected = "text"),
                     hr(),
                     br(),
                     selectInput("metodo_elegido", "Methods:",
                                 sql_metodo("en")),
                     hr(),
                     br(),
                     # Rango de fecha
                     dateRangeInput('dateRange',
                                    label = 'Date range input: ',
                                    start = Sys.Date() - 2, end = Sys.Date(),
                                    max = Sys.Date(), min = '2017-05-01'
                     ),
                     hr(),
                     br()
                     #change the color of action button
                     # actionButton("update_hist", "Run Historical", icon("paper-plane"),
                     #               style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                 ),
                 mainPanel(
                     tabsetPanel(id = "tabhistorical",
                                 # Muestra la trayectoria de la polaridad de los tweets en una fecha determinada y para un metodo elegido
                                 # Total de positivos, negativos o neutros en cada fecha (porcentajes)
                                 tabPanel("Trajectory", value = "tab12",
                                          br(),
                                          h3(textOutput("title_historico")),
                                          br(),
                                          # Horizontal Bar Plot
                                          plotOutput("view_historico_syuzhet"),
                                          br(),hr(),br(),br(),
                                          dataTableOutput("view_hist_syuzhet"),
                                          br(),hr(),br(),br(),
                                          dataTableOutput("view_hist_syuzhet_perc"),
                                          br(),br()
                                 ),
                                 # Muestra la polaridad de los tweets en una fecha determinada y un metodo elegido
                                 tabPanel("Historical", value = "tab17",
                                          br(),
                                          h3(textOutput("title_historico_tweets")),
                                          br(),
                                          # Tabla con los tweets
                                          DT::dataTableOutput("view_hist_tweets"),
                                          br(),br(),
                                          verbatimTextOutput('sel'),
                                          br(),br()
                                 )
                     ) # tabsetPanel - historical
                     
                 ) # mainPanel
             ) #sidebarLayout
    ) #tabPanel - Historical
)


server <- function(input,output){}

runApp(list(ui=ui,server=server))

