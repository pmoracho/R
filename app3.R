library("leaflet")

ui <- shinyUI(fluidPage(sidebarLayout(
    sidebarPanel(
        checkboxInput("show1", "Mostrar ID=1", value = TRUE),
        checkboxInput("show2", "Mostrar ID=2", value = TRUE),
        checkboxInput("show3", "Mostrar ID=3", value = TRUE)
    ),
    mainPanel(leafletOutput("map"))
)))

df <- data.frame(
    id = as.character(c(1, 2, 3)),
    lat = c(-31.8978827, -32.0499984, -30.9759958),
    lng = c(-64.7816031, -65.0348689, -64.512675)
)

add_a_marker <- function(proxy, id) {
    
    return(addCircleMarkers(
        proxy,
        layerId = df$id[id],
        df$lng[id],
        df$lat[id],
        group = 'marker',
        radius = 10,
        fill = TRUE,
        color = 'red'
    ))
}

server <- shinyServer(function(input, output, session) {
    
    output$map <- renderLeaflet(
        leaflet() %>%
            addTiles() %>% addCircleMarkers(
                layerId = df$id,
                df$lng,
                df$lat,
                group = 'marker',
                radius = 10,
                fill = TRUE,
                color = 'red'
            )
    )
    
    observeEvent(input$show1, {
        proxy <- leafletProxy('map')
        if (!input$show1) {
            removeMarker(proxy, layerId = df$id[1])
        } else {       
            add_a_marker(proxy, 1)
        }
    })

    observeEvent(input$show2, {
        proxy <- leafletProxy('map')
        if (!input$show2) {
            removeMarker(proxy, layerId = df$id[2])
        } else {       
            add_a_marker(proxy, 2)
        }
    })

    observeEvent(input$show3, {
        proxy <- leafletProxy('map')
        if (!input$show3) {
            removeMarker(proxy, layerId = df$id[3])
        } else {       
            add_a_marker(proxy, 3)
        }
    })
    
})

shinyApp(ui, server)