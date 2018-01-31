library(DT)
library(shiny)
library(ggplot2) 
library(caret)

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
    test.df$N <- nrow(test.df):1
    train.df$N <- nrow(train.df):1
    return(list(train.df=train.df, test.df=test.df))
}

test.model <- function(periodos, origen, depto, nperiodos, formula, modelo) {
    
    res <- data.frame()
    for (p in periodos) {
        data <- gen.data(periodo_actual = p,
                         origen = origen,
                         depto = depto,
                         nperiodos = nperiodos)
        formula <- as.formula(formula)
        model <- train(formula, data=data$train.df, method = modelo)
        cobp <- predict(model, newdata=data$test.df)
        
        res <- rbind(res,c(data$test.df$P, data$test.df$CADM, data$test.df$CR, ValorModelo=cobp, 
                           abs(data$test.df$CR - data$test.df$CADM), abs(data$test.df$CR - cobp)
                        )
                    )
    }    
    rownames(res)<-NULL
    res["Total" ,] <- colSums(res)
    colnames(res) <- c("Periodo", "ValorADM", "Real", "ValorModelo", "DesvioADM", "DesvioModelo")
    
    return(res)
}
cob <- load.raw.data()
periodos <- unique(cob[cob$T=="P", 1])
periodos <- periodos[!is.na(match(periodos,cob[cob$T=="R", 1]))]

variables <- colnames(cob)[!(colnames(cob) %in% c("P","O","D","T","COB"))]
modelos <- c("lm", "rpart", "rpart2", "qrf", "treebag", "gbm")
#data <- gen.data(periodo_actual=201708, origen="Ext", depto="LEG", nperiodos=1)
#data$test.df
#test.model(201708, "Loc$", "LEG", 5, as.formula("COB ~ F1"), "lm")
#gen.data(201708, "Loc$", "LEG", 5)


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
                         actionButton("action", "Analizar"),
                         width = 2
                     ),
                     mainPanel(
                         tabsetPanel(
                             id = 'dataset',
                             tabPanel("Train", div(DT::dataTableOutput("train"), style = "font-size: 75%; width: 75%")),
                             tabPanel("Test", div(DT::dataTableOutput("test"), style = "font-size: 75%; width: 75%")),
                             tabPanel("Correlaciones",div(DT::dataTableOutput("cor"), style = "font-size: 75%; width: 75%")),
                             tabPanel("Dispersion", 
                                      fluidRow(
                                          column(
                                              width = 12, 
                                                 h3('Análisis de la cobranza con relación a las otras variables'),  
                                                 selectInput("variable", "Variable a analizar:", variables)
                                          ),
                                        plotOutput("plot")
                                      )),
                             tabPanel("Modelos", 
                                      h3('Estudio de modelos'),  
                                      fluidRow(
                                          column(
                                              width = 4, 
                                              selectInput("modelo", "Modelo a analizar:", modelos)
                                          ),
                                          column(
                                              offset = 2,
                                              textInput("formula", "Formula", "COB ~ F1"),
                                              width = 4
                                          ),
                                          div(DT::dataTableOutput("modelo"), style = "font-size: 75%; width: 75%")
                                      ))
                         )
                         
                         
                     )
            )
            #tabPanel("Navbar 2", "This panel is intentionally left blank"),
            #tabPanel("Navbar 3", "This panel is intentionally left blank")
        )
    ),
    server = function(input, output) {
        ##########################################################################
        # Train data
        ##########################################################################
        output$train <- DT::renderDataTable({
            data <- gen.data(periodo_actual = as.numeric(input$periodo_actual),
                             origen = input$origen,
                             depto = input$depto,
                             nperiodos = as.numeric(input$nperiodos))
            DT::datatable(data$train.df[, -c(3,11)], 
                          rownames = FALSE,
                          options = list(
                              dom = 't',
                              pageLength = 12*as.numeric(input$nperiodos)
                            ), 
                          callback = JS("table.order([9, 'asc']).draw();")
                          ) %>% formatCurrency(columns=c("COB", "F1", "F2", "F3", "F4", "F5"))
        })

        ##########################################################################
        # Test data
        ##########################################################################
        output$test <- DT::renderDataTable({
            data <- gen.data(periodo_actual = as.numeric(input$periodo_actual),
                             origen = input$origen,
                             depto = input$depto,
                             nperiodos = as.numeric(input$nperiodos))
            DT::datatable(data$test.df[, -c(3,4,11)], 
                          rownames = FALSE,
                          options = list(
                              dom = 't',
                              pageLength = 12*as.numeric(input$nperiodos)
                          ), 
                          callback = JS("table.order([9, 'asc']).draw();")
            ) %>% formatCurrency(columns=c("COB", "F1", "F2", "F3", "F4", "F5", "CR", "CADM"))
        })
        ##########################################################################
        # Tabla de correlaciones
        ##########################################################################
        output$cor <- DT::renderDataTable({
            data <- gen.data(periodo_actual = as.numeric(input$periodo_actual),
                             origen = input$origen,
                             depto = input$depto,
                             nperiodos = as.numeric(input$nperiodos))
            
            DT::datatable(cor(data$train.df[,c(2,5,6,7,8,9,10,12)]), 
                          options = list(
                              dom = 't',
                              pageLength = 12*as.numeric(input$nperiodos)
                          ), 
                          callback = JS("table.order([9, 'asc']).draw();")
            )
        })
        ##########################################################################
        # Plot
        ##########################################################################
        output$plot <- renderPlot({
            data <- gen.data(periodo_actual = as.numeric(input$periodo_actual),
                             origen = input$origen,
                             depto = input$depto,
                             nperiodos = as.numeric(input$nperiodos))
            
            formula <- as.formula(paste("COB", "~", input$variable))
            ggplot(data$train.df, aes_string(x = input$variable, y = "COB")) + 
                geom_point() + 
                geom_smooth(method=lm, color="red") +
                geom_smooth() 
        })    
        ##########################################################################
        # Resultados del modelo
        ##########################################################################
        output$modelo <- DT::renderDataTable({

            res <- test.model(periodos=periodos, 
                              origen = input$origen, 
                              depto = input$depto, 
                              nperiodos = as.numeric(input$nperiodos), 
                              formula = as.formula(input$formula), 
                              modelo = input$modelo) 

            DT::datatable(res, 
                          options = list(
                              rownames = FALSE,
                              dom = 't',
                              pageLength = 12*as.numeric(input$nperiodos)
                          )
            ) %>% formatCurrency(columns=c("ValorADM", "Real", "ValorModelo", "DesvioADM", "DesvioModelo"))
            
        })
        
    }
)