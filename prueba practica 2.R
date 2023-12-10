library(shiny)
library(shinydashboard)
library(dplyr)
library(readr)
library(ggplot2)

datos_libertad <- read_csv("Datos/datos_libertad.csv")

ui <- fluidPage(
  titlePanel("Explorador de Libertad en el Mundo"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("pais", "Seleccionar País", choices = unique(datos_libertad$pais)),
      sliderInput("rango_tiempo", "Seleccionar Rango de Tiempo", min = min(datos_libertad$anio), max = max(datos_libertad$anio), value = c(min(datos_libertad$anio), max(datos_libertad$anio))),
      radioButtons("tipo_datos", "Tipo de Datos", choices = c("Ranking", "Puntaje"), selected = "Ranking"),
      downloadButton("descargar_datos", "Descargar Datos")
    ),
    mainPanel(
      plotOutput("plot_libertad")
    )
  )
)

server <- function(input, output) {
  datos_filtrados <- reactive({
    datos_libertad %>%
      filter(pais == input$pais, anio %in% seq(input$rango_tiempo[1], input$rango_tiempo[2]))
  })
  
  output$plot_libertad <- renderPlot({
    ggplot(datos_filtrados(), aes(x = anio, y = ifelse(input$tipo_datos == "Ranking", libertad_humana_ranking, libertad_humana_puntaje))) +
      geom_line() +
      labs(title = paste("Libertad Humana en", input$pais),
           y = ifelse(input$tipo_datos == "Ranking", "Ranking", "Puntaje"),
           x = "Año")
  })
  
  output$descargar_datos <- downloadHandler(
    filename = function() {
      paste("datos_libertad_", input$pais, "_", input$rango_tiempo[1], "_", input$rango_tiempo[2], ".csv", sep = "")
    },
    content = function(file) {
      write_csv(datos_filtrados(), file)
    }
  )
}

shinyApp(ui, server)
