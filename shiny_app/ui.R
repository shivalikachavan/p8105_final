library(shiny)
library(shinydashboard)
library(plotly)

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Demographics"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dash", icon = icon("dashboard"))
    ),
    
    radioButtons(
      "demo_choice",
      label = h5("Choose Demographic/Socioeconomic Characteristic"),
      choices = demographics,
      selected = "Age Group"
    )
  ),
  
  dashboardBody(
 
    tabItems(
      tabItem(
        tabName = "dash",
        
        fluidRow(
          box(
            width = 6,
            title = "Distribution of Bad Mental Health Days",
            plotlyOutput("chartA"),
            solidHeader = TRUE
          ),
          box(
            width = 6,
            title = "Distribution of Bad Physical Health Days",
            plotlyOutput("chartB"),
            solidHeader = TRUE
          )
        ),
        
        fluidRow(
          box(
            width = 6,
            title = "Distribution of Having Depressive Disorder",
            plotlyOutput("chartC"),
            solidHeader = TRUE
          ),
          box(
            width = 6,
            title = "Distribution of Binge Drinkers",
            plotlyOutput("chartD"),
            solidHeader = TRUE
          )
        )
      )
    )
  )
)