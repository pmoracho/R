library(shinyBS)
library(shiny)
library(shinythemes)

ui <- shinyUI(fluidPage(
    titlePanel("Aplikacja testowa nr 6. Praca z plikiem- wybór kolumny"),
    
    sidebarLayout(
        sidebarPanel(
            fileInput("file", "Escoger archivo CSV",multiple = TRUE, accept = 
                          c("text/csv","text/comma-separated-values,text/plain",".csv")),
            selectInput("combobox", label = h3("(Histogram) Wybierz kolumne"),  choices = NULL)
        ),
        
        mainPanel(
            uiOutput("tb")
        )
    )
))

server <- function(input, output, session){
    
    data <- reactive({
        file1 <- input$file
        if(is.null(file1)){return()} 
        dataSet <- read.csv(file=file1$datapath, sep=",", header = TRUE )
        updateSelectInput(session, "combobox", choices = colnames(dataSet))
        dataSet
    })
    
    output$table <- renderTable({
        if(is.null(data())){return ()}
        data()    
    })
    
    output$table2 <- renderTable({
        if(is.null(data()) || is.null(input$choices1)){return ()}
        data()[input$choices1]    
    })
    
    # there is part of file where i make histogram
    output$wykres <- renderPlot({
        x    <- data()[, input$combobox] 
        
        if (!is.numeric(x)) {
            createAlert(session, "alarm", alertId = "niebezpieczenstwo", 
                        title = "Niebezpieczenstwo: ",
                        content = "Histogram przyjmuje tylko wartosci ciagle!", 
                        style = "danger", dismiss = TRUE, append = TRUE)
        }
        if (is.numeric(x)) {
            closeAlert(session, "niebezpieczenstwo")
        }
        
        req(is.numeric(x))
        hist(x , col = 'blue', border = 'white')
    })
    
    
    output$tb <- renderUI({
        if(is.null(data()))
            h5("Wgraj Plik jeśli chcesz cokolwiek zrobić.")
        else
            tabsetPanel(tabPanel("dane", tableOutput("table")),
                        tabPanel("wybrane kolumny",
                                 tableOutput("table2")), 
                        tabPanel("Histogram", 
                                 bsAlert("alarm"),
                                 plotOutput("wykres")))
    })
}

shinyApp(ui, server)