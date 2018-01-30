library(shiny)
rm(ui)
rm(server)

server <- function(input, output) { 
    
    filedata <- reactive({
        infile <- input$datafile
        # require that infile is not NULL (?req)
        # it will prevent propagating errors 
        req(infile) 
        
        # read.csv(infile$datapath, header = TRUE)
        iris
    })
    
    output$toCol <- renderUI({
        df <- filedata()
        
        # as.character: to get names of levels and not a numbers as choices in case of factors
        items <- as.character(df[[1]])
        
        selectInput("species-dropdown", "Species:", items)
    })
    
}

ui <- fluidPage(
    # http://shiny.rstudio.com/gallery/file-upload.html
    sidebarLayout(
        sidebarPanel(
            fileInput('datafile', 'Choose CSV File',
                      accept=c('text/csv', 
                               'text/comma-separated-values,text/plain', 
                               '.csv')),
            tags$hr(),
            checkboxInput('header', 'Header', TRUE),
            radioButtons('sep', 'Separator',
                         c(Comma=',',
                           Semicolon=';',
                           Tab='\t'),
                         ','),
            radioButtons('quote', 'Quote',
                         c(None='',
                           'Double Quote'='"',
                           'Single Quote'="'"),
                         '"')
        ),
        mainPanel(
            uiOutput("toCol")
        )
    )
)

shinyApp(ui, server)