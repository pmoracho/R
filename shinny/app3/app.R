library(shiny)
library(shinythemes)

ui <- shinyUI(fluidPage(
    theme = shinytheme("flatly"),
    navbarPage(theme = "flatly","Analisis de Datos", tabPanel("Analisis Exploratorio")),
    sidebarLayout(
        sidebarPanel(
            fileInput("archivo", "Escoger archivo CSV",multiple = TRUE, accept = 
                          c("text/csv","text/comma-separated-values,text/plain",".csv")),
            tabPanel("Histograma",
                     selectInput("columnas", "Seleccionar Columna", choices= NULL))
        ),
        mainPanel(
            uiOutput("tb")
        )
    )
))

server.bak <-function(input, output , session) {
    data <- reactive({
        inFile <- input$archivo
        req(inFile)
        df <- read.csv(input$archivo$datapath,header = TRUE,sep =',',quote = '"')
        #Nombre de las columnas
        variables <- names(df)
        updateSelectInput(session, "columnas", "Seleccionar Columna", choices = variables)
        return(df)          
    })

    output$table <- renderTable({
        if(is.null(data())){return ()}
        data()    
    })
   
    output$tb <- renderUI({
        if(is.null(data()))
            h5("Sin datos")
        else
            tabsetPanel(tabPanel("Datos", tableOutput("table")))
    })    
}

server <-function(input, output , session) {

    observe({
        inFile <- input$archivo
        req(inFile)
        df <- read.csv(input$archivo$datapath,header = TRUE,sep =',',quote = '"')
        #Nombre de las columnas
        variables <- names(df)
        updateSelectInput(session, "columnas", "Seleccionar Columna", choices = variables)
        return(df)          
    })
}

shinyApp(ui = ui, server = server)