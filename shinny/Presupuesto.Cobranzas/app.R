library(DT)
library(shiny)
library(ggplot2) 

load.raw.data <- function() {
    data.file <- file.path("..", "..", "data", "cobranza.Rda")
    load(data.file)
    cob <- df[, c(1,2,3,4,6,8,9,10,11,12,13)]
    colnames(cob) <- c("P","M","O", "D", "COB", "F1", "F2", "F3", "F4", "F5", "T")
    cob <- cob[order(cob$O,-cob$P),]
    periodos <- unique(cob$P)
    cob$N <- match(cob$P, periodos)
    cob$O <- as.factor(cob$O)
    levels(cob$O) <- c("Loc$","LocUs","Ext")
    return(cob)
}

gen.data <- function(periodo_actual, origen, depto, nperiodos) {
    train.df <- head(cob[cob$T == "R" & cob$P < periodo_actual & cob$O == origen & cob$D == depto,], n=nperiodos*12)
    test.df <- cob[cob$T == "P" & cob$P == periodo_actual & cob$O == origen & cob$D == depto,]
    test.df$CR <- cob[cob$T == "R" & cob$P == periodo_actual & cob$O == origen & cob$D == depto, c(5)]
    test.df$CADM <- cob[cob$T == "P" & cob$P == periodo_actual & cob$O == origen & cob$D == depto, c(5)]
    return(list(train.df=train.df, test.df=test.df))
}

cob <- load.raw.data()
periodos <- unique(cob[cob$T=="R", 1])

# data <- gen.data(periodo_actual=201708, origen="Ext", depto="LEG", nperiodos=1)
# data$test.df

shinyApp(
    ui = tagList(
        # shinythemes::themeSelector(),
        navbarPage(
            theme = "simplex",  # <--- To use a theme, uncomment this
            "Cobranzas",
            tabPanel("Performance de modelos",
                     sidebarPanel(
                         selectInput("periodo_actual", "Periodo a analizar:", periodos),
                         selectInput("depto", "Departamento:", unique(cob$D)),
                         selectInput("origen", "Origen:", unique(cob$O)),
                         selectInput("nperiodos", "Cantidad de ejercicios:", c(1,2,3,4,5)),
                         selectInput("modelo", "Model:", c("lm","rpart2","gbm","treebag")),
                         
                         actionButton("action", "Analizar"),
                         width = 3
                     ),
                     mainPanel(
                         tabsetPanel(
                             id = 'dataset',
                             tabPanel("train", DT::dataTableOutput("train")),
                             tabPanel("test", DT::dataTableOutput("test")),
                             tabPanel("Resultado")
                         )
                         
                         
                     )
            )
            #tabPanel("Navbar 2", "This panel is intentionally left blank"),
            #tabPanel("Navbar 3", "This panel is intentionally left blank")
        )
    ),
    server = function(input, output) {
        # sorted columns are colored now because CSS are attached to them
        output$train <- DT::renderDataTable({
            data <- gen.data(periodo_actual = as.numeric(input$periodo_actual),
                             origen = input$origen,
                             depto = input$depto,
                             nperiodos = as.numeric(input$nperiodos))
            DT::datatable(data$train.df[, -c(3,4)], rownames = FALSE)
        })

        output$test <- DT::renderDataTable({
            data <- gen.data(periodo_actual = as.numeric(input$periodo_actual),
                             origen = input$origen,
                             depto = input$depto,
                             nperiodos = as.numeric(input$nperiodos))
            DT::datatable(data$test.df[, -c(3,4)], rownames = FALSE)
        })
        
    }
)