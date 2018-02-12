# Load libraries
library("leaflet")

ui <- shinyUI(fluidPage(sidebarLayout(
    sidebarPanel(
        hr(),
        h4("Remove points"),
        checkboxGroupInput(
            inputId = "removeFromMap",
            label = "",
            choices = c(1:6)
        ),
        p("Cluster1 = 1,2,3"),
        p("Cluster2 = 4,5,6"),
        hr(),
        p("Checked = removed"),
        p("Unchecked = present")
    ),
    mainPanel(leafletOutput("map"))
)))

# ID must be a character
df <- data.frame(
    id = as.character(1:6),
    lng = rnorm(6, -106.1039361, 0.5),
    lat = rnorm(6, 50.543981, 0.5)
)

server <- shinyServer(function(input, output, session) {
    output$map <- renderLeaflet(
        leaflet() %>%
            setView(-106.1039361, 50.543981, zoom = 5) %>%
            addTiles() %>%
            
            addCircleMarkers(
                layerId = df$id[1:3],
                df$lng[1:3],
                df$lat[1:3],
                group = 'marker',
                radius = 3,
                fill = TRUE,
                color = '#EF2929',
                clusterId = "Cluster1",
                clusterOptions = markerClusterOptions()
            ) %>%
            
            addCircleMarkers(
                layerId = df$id[4:6],
                df$lng[4:6],
                df$lat[4:6],
                group = 'marker',
                radius = 3,
                fill = TRUE,
                color = '#729FCF',
                clusterId = "Cluster2",
                clusterOptions = markerClusterOptions()
            )
    )
    
    # Global ID vector
    ids <- df$id
    
    # Remove points
    observeEvent(input$removeFromMap, {
        checkedPoints <- input$removeFromMap
        checkedPoints <- checkedPoints[which(checkedPoints %in% ids)]
        
        proxy <- leafletProxy('map')
        
        if (length(checkedPoints) != 0) {
            if (any(checkedPoints %in% as.character(1:3))) {
                pointsC1 <- checkedPoints[which(checkedPoints %in% ids)]
                proxy %>% removeMarkerFromCluster(layerId = pointsC1, clusterId = "Cluster1")
                ids <<- ids[-which(ids == pointsC1)]
                
            }
            
            if (any(checkedPoints %in% as.character(4:6))) {
                pointsC2 <- checkedPoints[which(checkedPoints %in% ids)]
                proxy %>% removeMarkerFromCluster(layerId = pointsC2, clusterId = "Cluster2")
                ids <<- ids[-which(ids == pointsC2)]
                
            }
        }
        
    })
    
})

shinyApp(ui, server)