library(shiny)
library(shinydashboard)
library(gapminder)
library(rio)
library(tidyverse)
library(kableExtra)

gapminder_data<-gapminder

ui <- dashboardPage(
  dashboardHeader(title="Dashboard1"),
  dashboardSidebar(
    selectInput("country", "Seleccione el pais", choices = gapminder_data |> 
                  select(country) |> 
                  distinct() |> 
                  arrange(country) |> 
                  drop_na())
  ),
  dashboardBody(
    fluidRow(box(plotOutput("esperanza")), box(plotOutput("poblacion"))),
    fluidRow(box(plotOutput("pbi")), box(tableOutput("tabla")))
  )
)

server <- function(input, output) {
  
  output$esperanza<- renderPlot ({
    gapminder_data |> 
      filter(country==input$country) |> 
      ggplot(aes(y=lifeExp, x=year)) +
      geom_line() + 
      geom_point() +
      labs(x = "Años", 
           y = "Esperanza de vida (años)", 
           title = "Evolución de la variable Esperanza de vida",
           subtitle = "Años 1957 - 2007", 
           caption = "Fuente:Gapminder Data")
  })
    
  output$poblacion<- renderPlot({
    gapminder_data |> 
      filter(country==input$country) |> 
      ggplot(aes(y=pop, x=year)) +
      geom_line() + 
      labs(x = "Años", 
           y = "Población", 
           title = "Evolución de la variable Población",
           subtitle = "Años 1957 - 2007", 
           caption = "Fuente:Gapminder Data")
  })
  
  output$pbi<- renderPlot({
    gapminder_data |> 
      filter(country==input$country) |> 
      ggplot(aes(y=gdpPercap, x=year)) +
      geom_col() + 
      labs(x = "Años", 
           y = "PBIpc", 
           title = "Evolución de la variable GDPpercap",
           subtitle = "Años 1957 - 2007", 
           caption = "Fuente:Gapminder Data")
  })
  
  output$tabla<- function(){
    gapminder_data |> 
      filter(country==input$country) |>
      select(year, lifeExp, pop, gdpPercap) |> 
      kable() |> 
      kable_styling() |> 
      scroll_box(height = "50%")
  }
  
}

shinyApp(ui, server)
