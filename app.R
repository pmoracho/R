pts <- cbind(11.242828,30.51470)


    # points <- eventReactive(input$recalc, {
    #      pts <<- rbind(pts, cbind(input$long, input$lat))
    #      pts
    # }, ignoreNULL = FALSE)
    # 

library(shiny)
library(leaflet)

latitude <- c(35.94077, 35.83770, 35.84545, 35.81584, 35.79387, 36.05600)
longitude <- c(-78.58010, -78.78084, -78.72444, -78.62568, -78.64262, -78.67600)
radius<-c(15, 12, 12, 12, 12, 15)
ids<-c("a", "b", "c", "d", "e", "f")

selected_marker <- c()

shinyApp(
    ui = fluidPage(
        fluidRow(
            leafletMap(
                "map", "100%", 400,
                initialTileLayer = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                initialTileLayerAttribution = HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>'),
                options=list(
                    center = c(35.94077, -78.58010),
                    zoom = 10,
                    maxBounds = list(list(17, -180), list(59, 180))))
        ),
        fluidRow(
            verbatimTextOutput("Click_text"),
            actionButton("delete", "Delete Selected point")
        )
    ),
    
    server = function(input, output, session){
        
        map = createLeafletMap(session, 'map')
        
        session$onFlushed(once=T, function(){
            
            map$addCircleMarker(lat = latitude, 
                                lng = longitude, 
                                radius = radius, 
                                layerId=ids)
        })        

        observeEvent(input$delete, {
            proxy <- leafletProxy('map')
            if (length(selected_marker)>0) {
                print(selected_marker)
                proxy %>% removeMarker(layerId = "a")
                #removeMarker(map, "a")
            }
        })        

        observe({
            click<-input$map_marker_click
            if(is.null(click))
                return()
            text<-paste("Lattitude ", click$lat, "Longtitude ", click$lng)
            text2<-paste("You've selected point ", click$id)
            selected_marker <<- c(click$lat, click$lng)
            map$clearPopups()
            map$showPopup( click$lat, click$lng, text)
            output$Click_text<-renderText({
                text2
            })
            
        })
        
        
    }
)