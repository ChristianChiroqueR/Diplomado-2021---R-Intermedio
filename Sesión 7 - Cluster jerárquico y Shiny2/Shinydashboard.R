## app.R ##
library(shiny)
library(shinydashboard)
library (rio)
library(factoextra)
library(cluster)
library(ggrepel)
library(tidyverse)
library(maps)
library(kableExtra)

####################PROCESAMIENTO DE DATA#######################################

data(USArrests)
subdata <- data.frame(state=tolower(rownames(USArrests))
                     , USArrests)

#######################ESTRUCTURA DE LA APLICACIÓN##############################

## LEER ESTTE LINK:
# https://rstudio.github.io/shinydashboard/structure.html#layouts

ui <- dashboardPage( 
  dashboardHeader(title = "Datacrimen"),
  dashboardSidebar(
    sidebarMenu(id="IDdelsidebar",
                menuItem("Primera ventana", tabName = "tab1"), 
                menuItem("Segunda ventana", tabName = "tab2"), 
                menuItem("Tercera ventana", tabName = "tab3"),
                menuItem("Cuarta ventana", tabName = "tab4")
  )),
  
  
  dashboardBody(
   tabItems(
   tabItem(tabName = "tab1", plotOutput("cuadro1")),
   tabItem(tabName = "tab2", fluidRow(box(plotOutput("cuadro2")), box(plotOutput("cuadro3")))), 
   tabItem(tabName = "tab3", fluidRow(box(selectInput("input", "Ingresar valor", choices = c("1", "2"))), box(tableOutput("tabla1")))), 
   tabItem(tabName = "tab4", plotOutput("mapa"))
   )
  )
)

server <- function(input, output) {
  output$cuadro1<- renderPlot({
    subdata |> ggplot(aes(x=Murder, y=Assault)) +
      geom_point() + 
      geom_text(aes(label=rownames(subdata)))
  })
  
  output$cuadro2<- renderPlot({
    subdata |> ggplot(aes(x=Murder, y=Rape)) +
      geom_point() + 
      geom_text(aes(label=rownames(subdata)))
  })
  
  output$cuadro3<- renderPlot({
    subdata |> ggplot(aes(x=Rape, y=Assault)) +
      geom_point() + 
      geom_text(aes(label=rownames(subdata)))
  })
  
  output$tabla1<- function(){
    subdata |> 
      kable() |> 
      kable_styling() |> 
      scroll_box(height = "50%")
  }
  
  output$mapa<- renderPlot({
    subdata |> ggplot(aes(map_id=state, fill=Murder))+
      geom_map(map=map_data("state")) + 
      expand_limits(x=map_data("state")$long, y=map_data("state")$lat)
  })
  
  
}

shinyApp(ui, server)