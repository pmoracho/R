library(shiny)
library(highcharter)
library(colourpicker)
#ui
ui <- fluidPage(
    
    # Application title
    titlePanel("Problema con highchartr and removeUI"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            colourInput("colormap",h3("Color"),value = "#2c3e50"),
            actionButton("mapa","Mapear",icon('globe'))
        ),
        
        # Main ppanel
        mainPanel(
            #En el tabsetpanel se comentaron los tabpanel con los ui fijos
            #y se incluyeron dos tabpanel para insertar ahi los tab donde ve el 
            #usuario.
            tabsetPanel(id="tabpanel",
                        # tabPanel("Mapa1",div(highchartOutput("mapa1"),id="Mapa1")),
                        # tabPanel("Mapa2",div(highchartOutput("mapa2"),id="Mapa2"))
                        tabPanel("Mapa1",div(id="Mapa1")),
                        tabPanel("Mapa2",div(id="Mapa2")),
                        selected="Mapa2"
            )
        )
    )
)

# server
server <- function(input, output,session) {
    library(shiny)
    library(highcharter)
    library(colourpicker)
    
    nac<-list(type="FeatureCollection",
              features=list(list(type="Feature",
                                 properties=list(state_code=list(0),state_name=list("Nacional")),
                                 geometry=list(type="Polygon",
                                               coordinates=list(list(list(-95,26),
                                                                     list(-91,26),
                                                                     list(-91,22),
                                                                     list(-95,22),
                                                                     list(-95,26)))))))
    
    ui_mapa1<-reactive({tagList(div(
        
        renderHighchart({
            print(paste("1",rv$mapa1))
            
            highchart(type = "map") %>%
                hc_chart(backgroundColor = "#FFFFFF") %>%
                hc_add_series(mapData = nac, showInLegend = FALSE, nullColor = "#424242",
                              borderWidth = 0,data = rv$data_mapa, value = "value",
                              joinBy = c("state_code", "id.edo"), name = "IVP" ,
                              dataLabels = list(enabled = TRUE, format = '{point.properties.state_name}'),
                              borderColor = "#FAFAFA", borderWidth = 0.1,
                              tooltip = list(pointFormat='{point.properties.state_name}: {point.value}<br/>',
                                             valueDecimals = 2),
                              states=list(
                                  hover=list(
                                      color= "#a4edba"
                                  )
                              ))%>%
                hc_colorAxis(tickPixelInterval= 100,
                             minColor= '#E6E7E8',
                             maxColor=isolate(input$colormap))%>%
                hc_title(text ="Indice de Valoracón Predial")%>%
                hc_mapNavigation(enabled= T,
                                 buttonOptions=list(verticalAlign= 'bottom')
                ) %>%
                hc_credits(enabled = TRUE,
                           text = "Fuente: Elaboración propia",
                           href = "") %>% 
                hc_exporting(
                    enabled = TRUE
                )
            
        })      
        
    ))})
    
    ui_mapa2<-reactive({tagList(div(
        
        ##Genera el mapa2
        renderHighchart({
            print(paste("2",rv$mapa2))
            
            highchart(type = "map") %>%
                hc_chart(backgroundColor = "#FFFFFF") %>%
                hc_add_series(mapData = nac, 
                              showInLegend = FALSE, 
                              nullColor = "#424242",
                              borderWidth = 0,
                              data = rv$data_mapa,
                              value = "value",
                              joinBy = c("state_code", "id.edo"), 
                              name = "IVP" ,
                              dataLabels = list(enabled = TRUE, format = '{point.properties.state_name}'),
                              borderColor = "#FAFAFA", 
                              borderWidth = 0.1,
                              tooltip = list(pointFormat='{point.properties.state_name}: {point.value}<br/>',
                                             valueDecimals = 2),
                              states=list(
                                  hover=list(
                                      color= "#a4edba"
                                  )
                              ))%>%
                hc_colorAxis(tickPixelInterval= 100,
                             minColor= '#E6E7E8',
                             maxColor=isolate(input$colormap))%>%
                hc_title(text ="Indice de ValoracIón Predial")%>%
                hc_mapNavigation(enabled= T,
                                 buttonOptions=list(verticalAlign= 'bottom')
                ) %>%
                hc_credits(enabled = TRUE,
                           text = "Fuente: Elaboración propia",
                           href = "") %>% 
                hc_exporting(
                    enabled = TRUE
                )
            
        })
        
    ))})
    
    #Se crean valores reactivos para guardar los tags, unos datos para el mapay
    #y dos valores para controlar la reactividad de los mapas
    rv<-reactiveValues(
        ulttag=NULL,
        acttag="Mapa2",
        data_mapa=data.frame(),
        mapa1=0,
        mapa2=0
    )
    
    #Cuando se cambie de tab corre este codigo
    observeEvent(input$tabpanel,{
        #Guarda en que tag estuvo anteriormente, y en cual esta actaulmente
        rv$ulttag<-rv$acttag; rv$acttag<-input$tabpanel
        
        #Encuentra el ui que debe insertar
        if(rv$acttag=="Mapa1"){
            uires<-ui_mapa1()
            print(rv$acttag)
        }else{
            uires<-ui_mapa2()
            print(rv$acttag)
        }
        
        #Remueve el ui del ultimo tag y inserta el ui del tag actual
        if(!(rv$acttag==rv$ulttag)){
            removeUI(selector = paste("#",rv$ulttag," div",sep=""))}
        
        insertUI(selector = paste("#",rv$acttag,sep=""),ui=uires)
        print("nac")
    })
    
    #Cuando demos clic en el boton mapear carga los datos de cada tab
    #y cambia el valore reactivo
    observeEvent(input$mapa,{
        
        if(rv$acttag=="Mapa1"){
            data<-as.data.frame(cbind(c(0),c(4)))
            names(data)<-c("id.edo","value")
            rv$mapa1<-rv$mapa1+1
            rv$data_mapa<-data }else{#Mapa 2
                data<-as.data.frame(cbind(c(0),c(8)))
                names(data)<-c("id.edo","value")
                rv$data_mapa<-data
                rv$mapa2<-rv$mapa2+1
            }
    })
    
}

# Correr aplicación
shinyApp(ui = ui, server = server)