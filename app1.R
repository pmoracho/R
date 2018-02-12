library(leaflet)
library(shiny)

markers <- data.frame(Lat = c(54.406486, 54.406486), Lon = c(-2.925284, -1.925284), Hab = c(1,1))

# set basic ui
ui <- fluidPage(
    leafletOutput("map")
)

server <- shinyServer(function(input, output) {
    data <- reactiveValues(clickedMarker=NULL)
    # produce the basic leaflet map with single marker
    output$map <- renderLeaflet(
        leaflet() %>%
            addProviderTiles("CartoDB.Positron") %>%
            addCircleMarkers(lat = markers$Lat, lng = markers$Lon)    
    )
    
    # observe the marker click info and print to console when it is changed.
    observeEvent(input$map_marker_click,{
        data$clickedMarker <- input$map_marker_click
        markers <<- markers[-1,]
        addCircleMarkers(lat = markers$Lat, lng = markers$Lon)    
        print(data$clickedMarker)}
    )
    observeEvent(input$map_click,{
        data$clickedMarker <- NULL
        print(data$clickedMarker)})
})

shinyApp(ui, server)