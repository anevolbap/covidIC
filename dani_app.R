library(shiny)
library(tidyverse)
library(lubridate)
library(ggiraph)

source("src/mapas.R")
source("src/plots.R")
source("src/preprocesar.R")
source("src/utils.R")

## ------------------------------------------------
SIDEBARPANEL_WIDTH = 2
INPUT_FILENAME = "data/Covid19Casos.csv"
OUTPUT_FILENAME = "data/Covid19Casos-procesado.RDS"
## ------------------------------------------------

data = load_processed_data(INPUT_FILENAME, OUTPUT_FILENAME)

ui <- fluidPage(
    titlePanel('Positividad'),
    sidebarLayout(
        sidebarPanel(
            selectInput("distrito", "Distrito", choices = c("AMBA", "PAIS")),
            conditionalPanel(
                condition = "input.tabs == 'Mapa'",
                dateInput("fecha", "Fecha", value = FECHA_ORIGEN, language = "es"),
                h6('Semana: ', textOutput("semana_fecha")),
            ),
            width = SIDEBARPANEL_WIDTH
        ),
        mainPanel(
            tabsetPanel(
                type="tabs",
                id = "tabs",
                tabPanel('Curvas', plotlyOutput("plot_semanas_todas")),
                tabPanel('Mapa',
                         girafeOutput("plot_por_semana"))),
            width = 12 - SIDEBARPANEL_WIDTH)
    )
)

server <- function(input, output, session) {

    los_datos <- reactive({
        test_df(data, distrito_a_seccion(input$distrito))
    })
   
    output$plot_semanas_todas <- renderPlotly({
        plot_curva_positividad(los_datos())
    })

    output$plot_por_semana <- renderGirafe({
        plot_positividad_por_semana(redondear_fecha(input$fecha),
                                    los_datos(),
                                    input$distrito)
    })

    output$semana_fecha <- renderText({
            as.character(redondear_fecha(input$fecha), format = "%Y/%m/%d")
    })
}

shinyApp(ui = ui, server = server)
