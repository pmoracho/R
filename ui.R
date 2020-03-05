library(shinydashboard)
library(shiny)

sidebar <- dashboardSidebar(
  
  tags$head(tags$style(HTML('.content-wrapper { height: 1500px !important;}'))),
  
  hr(),
  
  sidebarMenu(id="tabs",
              menuItem("a1", tabName="principal", icon=icon("pagelines"), selected=TRUE),
              menuItem("a2", icon=icon("chart-bar"),
                       menuSubItem("b1", tabName = "identificacion", icon = icon("angle-right")),
                       menuSubItem("b2", tabName = "comunicacion", icon = icon("angle-right")),
                       menuSubItem("b3", tabName = "medicamentos", icon = icon("angle-right")),
                       menuSubItem("b4", tabName = "cirugias", icon = icon("angle-right")),
                       menuSubItem("b5", tabName = "infecciones", icon = icon("angle-right")),
                       menuSubItem("b6", tabName = "caidas", icon = icon("angle-right"))
              ),
              menuItem("a3", tabName = "procesos", icon=icon("chart-bar")),
              menuItem("a4", tabName = "tiempos", icon = icon("chart-bar")),
              menuItem("a5", tabName = "manual", icon=icon("mortar-board")),
              menuItem("a6", tabName = "acerca", icon = icon("question"))
  ),width = 285,
  
  hr(),
  
  conditionalPanel("input.tabs=='identificacion'",
                   fluidRow(
                     column(1),
                     column(10,
                            menuItem("c1", tabName="admision_iden", icon=icon("chart-line"), selected=FALSE),
                            menuItem("c8", tabName="uci_iden", icon=icon("chart-line"), selected=FALSE)
                     )
                   )
  ))

body <- dashboardBody(
  
  tabItems(
    
    tabItem(tabName = "principal",withMathJax(),("example of text")),
    tabItem(tabName = "admision_iden", titlePanel("example1"),"example of text 2"),
    tabItem(tabName = "uci_iden", titlePanel("example 2"),"example of text 3")
  )
  
  
)



dashboardPage(
  dashboardHeader(title = "Indic", titleWidth=285),
  sidebar,
  body
)

shiny::runApp(display.mode="showcase")

