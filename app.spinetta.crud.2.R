library(shiny)
library(shinyjs)
library(DT)

ui<- tagList(useShinyjs(),
             tags$script(HTML("$(document).on('shiny:sessioninitialized', function(){
  var idz = [];
  var tags = document.getElementsByTagName('a');
 console.log(tags);
for (var i = 0; i < tags.length; i++) {
    idz.push(tags[i].hash);
    console.log(tags[i].hash); //console output for in browser debuggin'
                              }
 console.log(idz); // just checking again..
 Shiny.onInputChange('mydata', idz);
                          })")),
             
             navbarPage(title = "Example",
                        
                        tabPanel("Event List",
                                 sidebarLayout(
                                   sidebarPanel(list(
                                     p("If you click the link, it should go to the edit event panel."),
                                     p("And now it does...")
                                   ), align="left"),
                                   mainPanel(
                                     h3("Event List"),
                                     DT::dataTableOutput('table'),
                                     dataTableOutput('events_table'),
                                     shiny::textOutput("mydata"),
                                     align="center"))),
                        tabPanel("Edit Event", value='edit',
                                 sidebarLayout(
                                   sidebarPanel(
                                     uiOutput("choose_event_id"),
                                     align="center"),
                                   mainPanel()
                                 )),
                        id='top'
             ))




server<- shinyServer(function(input, output, session) {
  my_choices_list<- c("Dog", "Cat", "Fish")
  
  output$choose_event_id  <- renderUI({
    selectizeInput("event_id", "Event", width='100%',
                   choices=my_choices_list, selected=my_choices_list[1])
  })
  output$mydata<- renderPrint({
    tmp<- input$mydata
    tmp<- tmp[2:length(tmp)]
    tmp<- unlist(tmp)
    paste0("HREF value of other tab(s).... ",  tmp, collapse = ", ")
  })
  mylinks<- reactive({
    if(!is.null(input$mydata)){
      tmp<- input$mydata
      tmp<- tmp[2:length(tmp)] # All tabs except the first tab
      tmp
    }
  })
  
  output$table <- DT::renderDataTable({
    if(is.null(mylinks())){
      table<- data.frame(A=1, B=2)
    }
    if(!is.null(mylinks())){
      links_list<- paste0('<a href="', mylinks(),'" data-toggle="tab">test</a>')
      table<- DT::datatable(data.frame(A=my_choices_list, B=rep(links_list, length(my_choices_list))),rownames = FALSE, escape = FALSE,  selection = 'single', options = list(dom = 't'))
    }
    table
    
  })
  table_proxy = dataTableProxy('table')
  
  observeEvent(input$table_rows_selected, {
    cat("The selected Row is...", input$table_rows_selected, "\n")
    updateNavbarPage(session = session, inputId = "top", selected = "edit")
    shiny::updateSelectizeInput(session, inputId = "event_id", selected = my_choices_list[input$table_rows_selected])
    table_proxy %>% selectRows(NULL)
  })
  
})


shinyApp(ui = ui, server=server)