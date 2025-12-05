library(shiny)
library(plotly)

server <- function(input, output, session) {
  
  # Chart A ---------------------------------------------------------------------
  output$chartA <- renderPlotly({
    demo_var <- input$demo_choice
    
    brfss_data |> 
      count(.data[[demo_var]], mental_health_not_good_days, name = "n") |> 
      group_by(.data[[demo_var]]) |> 
      mutate(prop = n / sum(n)) |> 
      plot_ly(
        x = ~ .data[[demo_var]],
        y = ~ prop,
        color = ~ mental_health_not_good_days,
        type = "bar"
      ) |> 
      layout(
        barmode = "stack",
        xaxis = list(title = demo_var),
        yaxis = list(title = "Proportion")
      )
  })
  
  # Chart B ---------------------------------------------------------------------
  output$chartB <- renderPlotly({
    demo_var <- input$demo_choice
    
    brfss_data |> 
      count(.data[[demo_var]], physical_health_not_good_days, name = "n") |> 
      group_by(.data[[demo_var]])  |> 
      mutate(prop = n / sum(n))  |> 
      plot_ly(
        x = ~ .data[[demo_var]],
        y = ~ prop,
        color = ~ physical_health_not_good_days,
        type = "bar"
      )  |> 
      layout(
        barmode = "stack",
        xaxis = list(title = demo_var),
        yaxis = list(title = "Proportion")
      )
  })
  
  # Chart C ---------------------------------------------------------------------
  output$chartC <- renderPlotly({
    demo_var <- input$demo_choice
    
    brfss_data  |> 
      count(.data[[demo_var]], depressive_disorder, name = "n")  |> 
      group_by(.data[[demo_var]])  |> 
      mutate(prop = n / sum(n))  |> 
      plot_ly(
        x = ~ .data[[demo_var]],
        y = ~ prop,
        color = ~ depressive_disorder,
        type = "bar"
      )  |> 
      layout(
        barmode = "stack",
        xaxis = list(title = demo_var),
        yaxis = list(title = "Proportion")
      )
  })
  
  # Chart D ---------------------------------------------------------------------
  output$chartD <- renderPlotly({
    demo_var <- input$demo_choice
    
    brfss_data  |> 
      count(.data[[demo_var]], binge_drink, name = "n")  |> 
      group_by(.data[[demo_var]])  |> 
      mutate(prop = n / sum(n))  |> 
      plot_ly(
        x = ~ .data[[demo_var]],
        y = ~ prop,
        color = ~ binge_drink,
        type = "bar"
      )  |> 
      layout(
        barmode = "stack",
        xaxis = list(title = demo_var),
        yaxis = list(title = "Proportion")
      )
  })
  
}